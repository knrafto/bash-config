-- | Bash script evaluation.
module Bash.Config.Eval
    ( Eval(..)
    , interpret
    ) where

import Control.Monad.State

import Bash.Config.Env

-- | Interpret a script or function, returning the resulting environment
-- variables and function definitions. Any variables or functions missing
-- are assumed to be unknown.
interpret :: Eval a => a -> Env -> Env
interpret = execState . eval

-- | Executable commands.
class Eval a where
    -- | Execute a command, and return its return value.
    eval :: a -> Bash ExitStatus

instance Eval a => Eval [a] where
    eval = foldr (\e a -> eval e >> a) (return (Just True))
