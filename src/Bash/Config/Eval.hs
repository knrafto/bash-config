{-# LANGUAGE RecordWildCards #-}
-- | Bash script evaluation.
module Bash.Config.Eval
    ( Eval(..)
    , interpret
    ) where

import Control.Applicative
import Control.Monad.State
import Language.Bash.Syntax

import Bash.Config.Env

-- | Interpret a script or function, returning the resulting environment
-- variables and function definitions. Any variables or functions missing
-- are assumed to be unknown.
interpret :: Eval a => a -> Env -> Env
interpret = execState . eval

-- | Evaluate a predicate, and take a branch if possible.
predicate
    :: Eval a
    => a
    -> Bash ExitStatus
    -> Bash ExitStatus
    -> Bash ExitStatus
predicate p t f = do
    s <- eval p
    case s of
        Nothing    -> return Nothing
        Just False -> f
        Just True  -> t

-- | Executable commands.
class Eval a where
    -- | Execute a command, and return its return value.
    eval :: a -> Bash ExitStatus

instance Eval a => Eval [a] where
    eval = foldr (\e a -> eval e >> a) (return (Just True))

instance Eval Command where
    eval (Command c _) = eval c

instance Eval ShellCommand where
    eval = undefined

instance Eval List where
    eval (List ss) = eval ss

instance Eval Statement where
    eval (Statement l Sequential)   = eval l
    eval (Statement _ Asynchronous) = return Nothing

instance Eval AndOr where
    eval (Last p)  = eval p
    eval (And p l) = predicate p (eval l) (return (Just False))
    eval (Or p l)  = predicate p (return (Just True)) (eval l)

instance Eval Pipeline where
    eval Pipeline{..} = (if inverted then fmap not else id) <$>
        case commands of
            []  -> return (Just True)
            [c] -> eval c
            _   -> return Nothing

instance Eval Assign where
    eval = undefined
