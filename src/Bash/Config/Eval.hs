{-# LANGUAGE LambdaCase, RecordWildCards #-}
-- | Bash script evaluation.
module Bash.Config.Eval
    ( Eval(..)
    , interpret
    ) where

import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Language.Bash.Syntax
import           Language.Bash.Word

import           Bash.Config.Env
import           Bash.Config.Expand
import           Bash.Config.Cond

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
predicate p t f = eval p >>= \case
    Just True  -> t
    Just False -> f
    Nothing    -> return Nothing

-- | Evaluate in a subshell.
subshell :: MonadState s m => m a -> m a
subshell a = do
    s <- get
    r <- a
    put s
    return r

-- | Evaluate a simple command.
simpleCommand :: [Assign] -> [Word] -> Bash ExitStatus
simpleCommand as ws = runMaybeT (expandWordList ws) >>= \case
    Nothing       -> return Nothing
    Just []       -> eval as
    Just (c:args) -> command c args

-- | Run a command.
command :: String -> [String] -> Bash ExitStatus
command name args = do
    fs <- gets functions
    let symbols = builtins `Map.union` ((\l _ -> eval l) <$> fs)
    case Map.lookup name symbols of
        Nothing -> return Nothing
        Just f  -> f args

-- | Bash shell builtins.
builtins :: Map String ([String] -> Bash ExitStatus)
builtins = Map.fromList
    [ ("true" , \_ -> return (Just True))
    , ("false", \_ -> return (Just False))
    , ("test" , return . test)
    , ("["    , return . test_)
    ]

-- | Executable commands.
class Eval a where
    -- | Execute a command, and return its return value.
    eval :: a -> Bash ExitStatus

instance Eval a => Eval (Maybe a) where
    eval = maybe (return Nothing) eval

instance Eval a => Eval [a] where
    eval = foldr (\e a -> eval e >> a) (return (Just True))

instance Eval Command where
    eval (Command c _) = eval c

instance Eval ShellCommand where
    eval (SimpleCommand as ss)   = simpleCommand as ss
    eval (FunctionDef name body) = Nothing <$ define name body
    eval (Subshell l)            = subshell (eval l)
    eval (Group l)               = eval l
    eval (Cond expr)             = cond expr
    eval (If p t f)              = predicate p (eval t) (eval f)
    eval _                       = return Nothing

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
            cs  -> subshell (eval cs)

instance Eval Assign where
    eval = undefined
