{-# LANGUAGE LambdaCase, OverloadedStrings #-}
-- | Bash script evaluation.
module Bash.Config.Eval
    ( Eval(..)
    , interpret
    ) where

import           Control.Applicative
import           Control.Monad.Reader.Class
import           Control.Monad.State.Class
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Monoid                hiding (Last)

import           Bash.Config.Cond
import           Bash.Config.Expand
import           Bash.Config.Types
import           Bash.Config.Word

-- | Interpret a script or function, returning the resulting environment
-- variables and function definitions. Any variables or functions missing
-- are assumed to be unknown.
interpret :: Eval a => a -> Env -> Either String Env
interpret a = fmap snd . runBash (eval a) Clean

-- | Evaluate with a dirty status.
dirty :: Eval a => a -> Bash ExitStatus
dirty a = Nothing <$ local (const Dirty) (eval a)

-- | Execute in a subshell. Environment changes during the subshell execution
-- will not affect the outside environment.
subshell :: Eval a => a -> Bash ExitStatus
subshell a = do
    env <- get
    r <- eval a
    put env
    return r

-- | Execute an assignment builtin.
assignBuiltin :: Word -> [Either Assign Word] -> Bash ExitStatus
assignBuiltin b args = Nothing <$ case Map.lookup b assignBuiltins of
    Nothing -> return ()
    Just f  -> mapM_ f args
  where
    assignBuiltins = Map.fromList $
        [ ("alias"   , \_ -> return ())
        , ("declare" , perform )
        , ("export"  , perform )
        , ("local"   , unassign)
        , ("readonly", unassign)
        , ("typeset" , perform )
        ]

    perform (Left a) = () <$ eval a
    perform _        = return ()

    unassign (Left (Assign n _ _)) = unset n
    unassign (Right w)             = unset (toString w)

-- | Execute a simple command.
command :: String -> [String] -> Bash ExitStatus
command name args = do
    defined <- gets functions
    let allCommands = builtins <> fmap (const . eval) defined
    case Map.lookup name allCommands of
        Nothing -> return Nothing
        Just f  -> f args

-- | Execute a function definition.
functionDef :: Word -> Function -> Bash ExitStatus
functionDef w f = Just True <$ define name f
              <|> Nothing   <$ undefine name
  where
    name = toString w

-- | Interpreter builtins. These are commands the the interpreter knows
-- how to execute. Any command not in this map is assumed to be user-defined,
-- or external.
--
-- The implemented builtins are @test@, @[@, @true@, and @false@. Most shell
-- builtins are assumed to have unpredictable effects and will cause the
-- interpreter to fail. However, some shell builtins, such as
-- @break@, @continue@, @pwd@, etc. are assumed to be safe. Builtins that
-- could take an assignment as a parameter are implemented separately.
builtins :: Map String ([String] -> Bash ExitStatus)
builtins = Map.fromList $
    -- implemented builtins
    [ ("test" , return . test )
    , ("["    , return . test_)
    , ("true" , \_ -> return (Just True) )
    , ("false", \_ -> return (Just False))
    ]
    -- unsafe builtins
    ++ map (\name -> (name, \_ -> unimplemented name))
        [ ".", "builtin", "caller", "enable", "exec", "exit", "let"
        , "logout", "mapfile", "read", "readarray", "return", "source"
        , "trap", "unset", "unalias"
        ]

-- | Executable commands.
class Eval a where
    -- | Execute a command, and return its return value.
    eval :: a -> Bash ExitStatus

instance Eval a => Eval [a] where
    eval [] = return (Just True)
    eval cs = last <$> mapM eval cs

instance Eval Script where
    eval (Script l) = eval l

instance Eval Command where
    eval (Simple c)        = eval c
    eval (Shell c)         = eval c
    eval (FunctionDef w f) = functionDef w f
    eval Coproc            = unimplemented "coproc"

instance Eval List where
    eval (List cs) = eval cs

instance Eval AndOr where
    eval (Last p  ) = eval p
    eval (And p cs) = eval p >>= \case
        Nothing    -> dirty cs
        Just False -> return (Just False)
        Just True  -> eval cs
    eval (Or  p cs) = eval p >>= \case
        Nothing    -> dirty cs
        Just False -> eval cs
        Just True  -> return (Just True)

instance Eval Pipeline where
    eval (Pipeline b cs) = bang $ case cs of
        []  -> return (Just True)
        [c] -> eval c
        _   -> subshell cs
      where
        bang   = if b then invert else id
        invert = fmap (fmap not)

instance Eval SimpleCommand where
    eval (SimpleCommand as ws)  = optional (expandWordList ws) >>= \case
        Nothing       -> return Nothing
        Just []       -> eval as
        Just (c:args) -> command c args
    eval (AssignCommand b args) = assignBuiltin b args

instance Eval Assign where
    eval (Assign name op a) = Just True <$ (assign name =<< expandValue a)
                          <|> Nothing   <$ unset name
      where
        assign = case op of
            Equals     -> set
            PlusEquals -> augment

instance Eval Function where
    eval (Function body) = eval body

instance Eval ShellCommand where
    eval (Subshell l  ) = subshell l
    eval (Group l     ) = eval l
    eval (Arith s     ) = unimplemented $ "((" ++ s ++ "))"
    eval (Cond ws     ) = cond ws
    eval (For _ _ l   ) = dirty l
    eval (ArithFor s _) = unimplemented $ "for ((" ++ s ++ "))"
    eval (Select _ _ l) = dirty l
    eval (Case _ cs   ) = eval cs
    eval (If p t f    ) = eval p >>= \case
        Nothing -> dirty t >> dirty f
        Just r  -> eval $ if r then t else f
    eval (Until p l   ) = dirty p >> dirty l
    eval (While p l   ) = dirty p >> dirty l

instance Eval CaseClause where
    eval (CaseClause _ l _) = dirty l
