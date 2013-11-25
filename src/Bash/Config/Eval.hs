{-# LANGUAGE LambdaCase, TemplateHaskell #-}
-- | Bash script evaluation.
module Bash.Config.Eval
    ( Eval(..)
    ) where

import Control.Applicative
import Control.Lens               hiding (assign, op)
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.Map                   (Map)
import Data.Monoid                hiding (Last)

import Bash.Config.Expand
import Bash.Config.Types

makeLensesFor [ ("envParameters", "parameters")
              , ("envFunctions" , "functions" )
              ]  ''Env

------------------------------------------------------------------------------
-- Bindings
------------------------------------------------------------------------------

-- | Return 'Just' if the current execution status if 'Clean',
-- and 'Nothing' otherwise.
binding :: a -> Bash (Maybe a)
binding a = asks $ \case
    Dirty -> Nothing
    Clean -> Just a

-- | Set a shell parameter.
assign :: String -> Value -> Bash ()
assign name value = do
    mvalue <- binding value
    parameters . at name .= mvalue

-- | Add to a shell parameter.
augment :: String -> Value -> Bash ()
augment name value = do
    a <- use (parameters . at name)
    b <- binding value
    let mvalue = append <$> a <*> b
    parameters . at name .= mvalue

-- | Define a shell function.
define :: String -> Bash ReturnCode -> Bash ()
define name body = do
    mbody <- binding body
    functions . at name .= mbody

------------------------------------------------------------------------------
-- Commands
------------------------------------------------------------------------------

-- | Execute a simple command.
command :: String -> [String] -> Bash ReturnCode
command name args = do
    defined <- use functions
    let allCommands = builtins <> fmap const defined
    case allCommands ^. at name of
        Nothing -> return Unknown
        Just f  -> f args

-- | Execute a conditional.
cond :: [String] -> Bash ReturnCode
cond = undefined

------------------------------------------------------------------------------
-- Execution
------------------------------------------------------------------------------

-- | Evaluate with a 'Dirty' status.
dirty :: Eval a => a -> Bash ReturnCode
dirty a = Unknown <$ local (const Dirty) (eval a)

-- | Execute in a subshell. Environment changes during the subshell execution
-- will not affect the outside environment.
subshell :: Eval a => a -> Bash ReturnCode
subshell a = do
    env <- get
    r <- eval a
    put env
    return r

-- | Executable commands.
class Eval a where
    -- | Execute a command, and return its return value.
    eval :: a -> Bash ReturnCode

instance Eval a => Eval [a] where
    eval [] = return Success
    eval cs = last <$> mapM eval cs

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
    eval (SimpleCommand as ws) = expandWords ws >>= \case
        []     -> eval as
        c:args -> command c args

instance Eval Assign where
    eval (Assign name op value) = do
        value' <- expandValue value
        Success <$ case op of
            Equals     -> assign name value'
            PlusEquals -> augment name value'

instance Eval Function where
    eval (Function name body) = Success <$ define name (eval body)

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

------------------------------------------------------------------------------
-- Builtins
------------------------------------------------------------------------------

-- | Shell builtins.
builtins :: Map String ([String] -> Bash ReturnCode)
builtins = mempty
