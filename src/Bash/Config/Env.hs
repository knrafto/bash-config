{-# LANGUAGE DeriveFunctor, LambdaCase, MultiParamTypeClasses #-}
-- | The Bash execution environment and shell script types.
-- This does not fully represent parts that
-- aren't interpreted, such as redirections or arithmetic expressions.
module Bash.Config.Types
    (
      -- * Values
      Value(..)
    , Array
    , toArray
      -- * Environments
    , Env(..)
    , emptyEnv
      -- ** Execution
    , ExitStatus
    , Status(..)
    , Bash(..)
    , unimplemented
      -- ** Parameters
    , set
    , augment
    , unset
    , value
      -- ** Functions
    , define
    , undefine
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader.Class
import           Control.Monad.State.Class
import           Data.IntMap                (IntMap)
import qualified Data.IntMap                as IntMap
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Language.Bash.Syntax

-------------------------------------------------------------------------------
-- Values
-------------------------------------------------------------------------------

-- | A Bash value
data Value = Value String | Array Array
    deriving (Eq, Ord, Read, Show)

-- | A Bash array.
type Array = IntMap String

-- | Coerce a value to an array.
toArray :: Value -> Array
toArray (Value v) = IntMap.singleton 0 v
toArray (Array a) = a

-------------------------------------------------------------------------------
-- Environments
-------------------------------------------------------------------------------

-- | The execution environment.
data Env = Env
    { -- | Environment parameters or variables.
      parameters :: Map String Value
      -- | Environment functions.
    , functions  :: Map String List
    } deriving (Eq)

-- | The empty environment.
emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty

-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

-- | A command's return code.
type ExitStatus = Maybe Bool

-- | The execution status. If the interpreter cannot fully simulate Bash,
-- the execution status will be set to 'Dirty' and execution will proceed
-- in a safe manner.
data Status
      -- | Unsafe execution
    = Unsafe
      -- | Indeterminate execution.
    | Dirty
      -- | Normal execution.
    | Clean
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | The Bash execution monad.
newtype Bash a = Bash { runBash :: Status -> Env -> Either String (a, Env) }
    deriving (Functor)

instance Applicative Bash where
    pure  = return
    (<*>) = ap

instance Alternative Bash where
    empty = fail "empty"
    (<|>) = mplus

instance Monad Bash where
    return a = Bash $ \_ s -> Right (a, s)
    m >>= k  = Bash $ \r s -> do
                   (a, s') <- runBash m r s
                   runBash (k a) r s'
    fail s   = Bash $ \_ _ -> Left s

instance MonadPlus Bash where
    mzero     = fail "mzero"
    mplus a b = Bash $ \r s -> runBash a r s `mplus` runBash b r s

instance MonadReader Status Bash where
    ask       = Bash $ \r s -> Right (r, s)
    local f m = Bash $ \r s -> runBash m (f r) s
    reader f  = Bash $ \r s -> Right (f r, s)

instance MonadState Env Bash where
    get     = Bash $ \_ s -> Right (s, s)
    put s   = Bash $ \_ _ -> Right ((), s)
    state f = Bash $ \_ s -> Right (f s)

-- | Fail with a message.
unimplemented :: String -> Bash a
unimplemented = fail

-- | Fail with a message if the current execution status is not 'Clean'.
whenClean :: String -> Bash a -> Bash a
whenClean s m = ask >>= \case
    Clean -> m
    _     -> fail s

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Modify the shell parameter map.
modifyParameters
    :: (Map String (Value String) -> Map String (Value String))
    -> Bash ()
modifyParameters f = modify $ \env -> env { parameters = f (parameters env) }

-- | Set a shell parameter. Fails if the current execution status is dirty.
set :: String -> Value String -> Bash ()

-- | Add to a shell parameter.
augment :: String -> Value String -> Bash ()

-- | Unset a shell parameter.
unset :: String -> Bash ()
unset name = modifyParameters (Map.delete name)

-- | Get the value of a binding, if it is known.
value :: String -> Bash (Maybe Value)
value name = gets (Map.lookup name . parameters)

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

-- | Modify the shell function map.
modifyFunctions :: (Map String Function -> Map String Function) -> Bash ()
modifyFunctions f = modify $ \env -> env { functions = f (functions env) }

-- | Define a shell function. Fails if the current execution status is dirty.
define :: String -> Function -> Bash ()
define name body = whenClean name $ modifyFunctions (Map.insert name body)

-- | Undefine a shell function.
undefine :: String -> Bash ()
undefine name = modifyFunctions (Map.delete name)
