{-# LANGUAGE DeriveFunctor, LambdaCase, MultiParamTypeClasses #-}
-- | The Bash execution environment.
module Bash.Config.Env
    ( -- * Environments
      Env(..)
    , Value(..)
    , emptyEnv
      -- * Execution
    , ExitStatus(..)
    , Status(..)
    , Bash(..)
      -- * Parameters
    , set
    , augment
    , unset
    , value
      -- * Functions
    , define
    , undefine
    , call
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader.Class
import           Control.Monad.State.Class
import           Data.Map                   (Map)
import qualified Data.Map                   as Map

-------------------------------------------------------------------------------
-- Environments
-------------------------------------------------------------------------------

-- | The execution environment.
data Env = Env
    { -- | Environment parameters or variables.
      parameters :: Map String Value
      -- | Environment functions.
    , functions  :: Map String (Bash ExitStatus)
    }

-- | A Bash value.
data Value = Value String | Array [String]
    deriving (Eq, Ord, Show)

-- | The empty environment.
emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty

-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

-- | A command's return code.
data ExitStatus
    -- | An unknown return code.
    = Unknown
    -- | A nonzero return code.
    | Failure
    -- | A zero return code.
    | Success
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | The execution status. If the interpreter cannot fully simulate Bash,
-- the execution status will be set to 'Dirty' and execution will proceed
-- in a safe manner.
data Status
    -- | Indeterminate execution.
    = Dirty
    -- | Normal execution.
    | Clean
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | The Bash execution monad.
newtype Bash a = Bash { runBash :: Status -> Env -> Maybe (a, Env) }
    deriving (Functor)

instance Applicative Bash where
    pure  = return
    (<*>) = ap

instance Alternative Bash where
    empty = fail "empty"
    (<|>) = mplus

instance Monad Bash where
    return a = Bash $ \_ s -> Just (a, s)
    m >>= k  = Bash $ \r s -> do
                   (a, s') <- runBash m r s
                   runBash (k a) r s'
    fail _   = Bash $ \_ _ -> Nothing

instance MonadPlus Bash where
    mzero     = fail "mzero"
    mplus a b = Bash $ \r s -> runBash a r s `mplus` runBash b r s

instance MonadReader Status Bash where
    ask       = Bash $ \r s -> Just (r, s)
    local f m = Bash $ \r s -> runBash m (f r) s
    reader f  = Bash $ \r s -> Just (f r, s)

instance MonadState Env Bash where
    get     = Bash $ \_ s -> Just (s, s)
    put s   = Bash $ \_ _ -> Just ((), s)
    state f = Bash $ \_ s -> Just (f s)

-- | Fail if the current execution status is dirty.
whenClean :: Bash a -> Bash a
whenClean m = ask >>= \case
    Dirty -> empty
    Clean -> m

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Modify the shell parameter map.
modifyParameters
    :: (Map String Value -> Map String Value)
    -> Bash ()
modifyParameters f = modify $ \env -> env { parameters = f (parameters env) }

-- | Set a shell parameter. Fails if the current execution status is dirty.
set :: String -> Value -> Bash ()
set name a = whenClean $ modifyParameters (Map.insert name a)

-- | Add to a shell parameter.
augment :: String -> Value -> Bash ()
augment name b = do
    a <- value name
    set name (append a b)
  where
    append (Value x ) (Value y ) = Value (x ++ y)
    append (Value x ) _          = Value x
    append (Array xs) (Array ys) = Array (xs ++ ys)
    append (Array xs) (Value y ) = Value $ case xs of
        []  -> y
        x:_ -> x ++ y

-- | Unset a shell parameter.
unset :: String -> Bash ()
unset name = modifyParameters (Map.delete name)

-- | Get the value of a binding, if it is known.
value :: String -> Bash Value
value name = gets (Map.lookup name . parameters) >>= \case
    Nothing -> empty
    Just v  -> return v

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

-- | Modify the shell function map.
modifyFunctions
    :: (Map String (Bash ExitStatus) -> Map String (Bash ExitStatus))
    -> Bash ()
modifyFunctions f = modify $ \env -> env { functions = f (functions env) }

-- | Define a shell function. Fails if the current execution status is dirty.
define :: String -> Bash ExitStatus -> Bash ()
define name body = whenClean $ modifyFunctions (Map.insert name body)

-- | Undefine a shell function.
undefine :: String -> Bash ()
undefine name = modifyFunctions (Map.delete name)

-- | Call a user-defined function, if it exists.
call :: String -> Bash ExitStatus
call name = gets (Map.lookup name . functions) >>= \case
    Nothing -> empty
    Just f  -> f
