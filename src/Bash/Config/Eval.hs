{-# LANGUAGE LambdaCase, TemplateHaskell #-}
-- | Bash script evaluation.
module Bash.Config.Eval
    ( Eval(..)
    ) where

import           Control.Applicative
import           Control.Monad.Reader.Class
import           Control.Monad.State.Class
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Monoid                hiding (Last)

import           Bash.Config.Command
import           Bash.Config.Cond
import           Bash.Config.Env
import           Bash.Config.Expand

-- | Evaluate with a dirty status.
dirty :: Eval a => a -> Bash ExitStatus
dirty a = Unknown <$ local (const Dirty) (eval a)

-- | Execute in a subshell. Environment changes during the subshell execution
-- will not affect the outside environment.
subshell :: Eval a => a -> Bash ExitStatus
subshell a = do
    env <- get
    r <- eval a
    put env
    return r

-- | Execute a simple command.
command :: String -> [String] -> Bash ExitStatus
command name args = do
    defined <- gets functions
    let allCommands = builtins <> fmap (const . eval) defined
    case Map.lookup name allCommands of
        Nothing -> return Unknown
        Just f  -> f args

-- | Interpreter builtins. These are commands the the interpreter knows
-- how to execute. Any command not in this map is assumed to be user-defined,
-- or external.
--
-- The implemented builtins are @test@, @[@, @true@, and @false@. Most shell
-- builtins are assumed to have unpredictable effects and will cause the
-- interpreter to fail. However, some shell builtins, such as
-- @break@, @continue@, @pwd@, etc. are assumed to be safe.
builtins :: Map String ([String] -> Bash ExitStatus)
builtins = Map.fromList $
    -- implemented builtins
    [ ("test" , cond )
    , ("["    , cond_)
    , ("true" , \_ -> return Success)
    , ("false", \_ -> return Failure)
    ]
    -- unsafe builtins
    ++ map (\name -> (name, const empty))
        [ ".", "alias", "builtin", "caller", "declare", "enable", "exec"
        , "exit", "export", "let", "local", "logout", "mapfile", "read"
        , "readarray", "readonly", "return", "source", "trap", "typeset"
        , "unset", "unalias"
        ]
  where
    cond_ ws = case unsnoc ws of
        Just (ws', "]") -> cond ws'
        _               -> return Failure

    unsnoc [] = Nothing
    unsnoc xs = Just (init xs, last xs)

-- | Executable commands.
class Eval a where
    -- | Execute a command, and return its return value.
    eval :: a -> Bash ExitStatus

instance Eval a => Eval [a] where
    eval [] = return Success
    eval cs = last <$> mapM eval cs

instance Eval Script where
    eval (Script l) = eval l

instance Eval Command where
    eval (Simple c     ) = eval c
    eval (Shell c      ) = eval c
    eval (FunctionDef f) = eval f
    eval Coproc          = empty

instance Eval List where
    eval (List cs) = eval cs

instance Eval AndOr where
    eval (Last p  ) = eval p
    eval (And p cs) = eval p >>= \case
        Unknown -> dirty cs
        Failure -> return Failure
        Success -> eval cs
    eval (Or  p cs) = eval p >>= \case
        Unknown -> dirty cs
        Failure -> eval cs
        Success -> return Success

instance Eval Pipeline where
    eval (Pipeline b cs) = bang $ case cs of
        []  -> return Success
        [c] -> eval c
        _   -> subshell cs
      where
        bang   = if b then invert else id
        invert = fmap $ \case
            Unknown -> Unknown
            Failure -> Success
            Success -> Failure


instance Eval SimpleCommand where
    eval (SimpleCommand as ws) = optional (expandWords ws) >>= \case
        Nothing       -> return Unknown
        Just []       -> eval as
        Just (c:args) -> command c args

instance Eval Assign where
    eval (Assign name op a) = Success <$ (assign name =<< expandValue a)
                          <|> Unknown <$ unset name
      where
        assign = case op of
            Equals     -> set
            PlusEquals -> augment

instance Eval Function where
    eval (Function name body) = Success <$ define name (Script body)
                            <|> Unknown <$ undefine name

instance Eval ShellCommand where
    eval (Subshell l  ) = subshell l
    eval (Group l     ) = eval l
    eval (Arith _     ) = empty
    eval (Cond ws     ) = cond ws
    eval (For _ _ l   ) = dirty l
    eval (ArithFor _ l) = dirty l
    eval (Select _ _ l) = dirty l
    eval (Case _ cs   ) = eval cs
    eval (If p t f    ) = eval p >>= \case
        Unknown -> dirty t >> dirty f
        Failure -> eval f
        Success -> eval t
    eval (Until p l   ) = dirty p >> dirty l
    eval (While p l   ) = dirty p >> dirty l

instance Eval CaseClause where
    eval (CaseClause _ l _) = dirty l
