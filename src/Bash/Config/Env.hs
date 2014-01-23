{-# LANGUAGE FlexibleContexts #-}
-- | The Bash execution environment and shell script types.
module Bash.Config.Env
    (
      -- * Values
      Value(..)
    , Array
    , toArray
      -- * Environments
    , Env(..)
    , emptyEnv
    , fromList
      -- ** Execution
    , ExitStatus
    , Bash
      -- ** Parameters
    , set
    , augment
    , unset
    , value
      -- ** Functions
    , define
    , function
    ) where

import           Control.Monad.State
import           Data.IntMap          (IntMap)
import qualified Data.IntMap          as IntMap
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Language.Bash.Syntax (List)

-- | A Bash value
data Value = Value String | Array Array
    deriving (Eq, Ord, Read, Show)

-- | A Bash array.
type Array = IntMap String

-- | Coerce a value to an array.
toArray :: Value -> Array
toArray (Value v) = IntMap.singleton 0 v
toArray (Array a) = a

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

-- | Construct an environment from a list of bindings.
fromList :: [(String, Value)] -> Env
fromList xs = Env (Map.fromList xs) Map.empty

-- | A command's return code.
type ExitStatus = Maybe Bool

-- | The Bash execution monad.
type Bash = State Env

-- | Modify the shell parameter map.
modifyParameters
    :: MonadState Env m => (Map String Value -> Map String Value) -> m ()
modifyParameters f = modify $ \env -> env { parameters = f (parameters env) }

-- | Set a shell parameter. Fails if the current execution status is dirty.
set :: MonadState Env m => String -> Value -> m ()
set k v = modifyParameters $ Map.insert k v

-- | Add to a shell parameter.
augment :: MonadState Env m => String -> Value -> m ()
augment k v = modifyParameters $ Map.alter (Just . flip f v) k
  where
    f Nothing          b         = b
    f (Just (Value a)) (Value b) = Value $ a ++ b
    f (Just a)         (Array b) = Array $ merge (toArray a) b
    f (Just (Array a)) (Value b) = Array $
                                   IntMap.alter (Just . maybe b (++ b)) 0 a

    merge a b = go a 0 (IntMap.elems b)
      where
        go m _ []                 = m
        go m n xs@(x:xs')
            | n `IntMap.member` m = go m (n + 1) xs
            | otherwise           = go (IntMap.insert n x m) (n + 1) xs'

-- | Unset a shell parameter.
unset :: MonadState Env m => String -> m ()
unset k = modifyParameters $ Map.delete k

-- | Get the value of a binding, if it is known.
value :: MonadState Env m => String -> m (Maybe Value)
value k = gets (Map.lookup k . parameters)

-- | Modify the shell function map.
modifyFunctions
    :: MonadState Env m => (Map String List -> Map String List) -> m ()
modifyFunctions f = modify $ \env -> env { functions = f (functions env) }

-- | Define a shell function. Fails if the current execution status is dirty.
define :: MonadState Env m => String -> List -> m ()
define k l = modifyFunctions (Map.insert k l)

-- | Get the value of a function.
function :: MonadState Env m => String -> m (Maybe List)
function k = gets (Map.lookup k . functions)
