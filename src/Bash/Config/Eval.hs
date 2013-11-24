{-# LANGUAGE LambdaCase #-}
-- | Bash script evaluation.
module Bash.Config.Eval
    ( Eval(..)
    ) where

import Control.Applicative
import Control.Monad.Reader.Class
import Control.Monad.State.Class

import Bash.Config.Builtins
import Bash.Config.Test
import Bash.Config.Types

-- | Execute with a 'Dirty' status.
dirty :: Bash a -> Bash a
dirty = local (const Dirty)

-- | Return 'Just' if the current execution status if 'Clean',
-- and 'Nothing' otherwise.
binding :: a -> Bash (Maybe a)
binding a = asks $ \case
    Dirty -> Nothing
    Clean -> Just a

-- | Execute in a subshell. Environment changes during the subshell execution
-- will not affect the outside environment.
subshell :: Bash a -> Bash a
subshell action = do
    env <- get
    r <- action
    put env
    return r

-- | Execute a conditional (@if@) construct.
cond :: Bash Int -> Bash Int -> Bash Int -> Bash Int
cond p t f = optional p >>= \case
    Nothing -> dirty t >> dirty f
    Just 0  -> t
    Just _  -> f

-- | Execute a simple command.
command :: [String] -> Bash Int
command = undefined

-- | Execute a user-defined function.
function :: String -> Bash Int
function c = use (functions . at c) >>= \case
    Nothing -> empty
    Just f  -> f

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
define :: String -> List -> Bash ()
define name body = do
    mbody <- binding body
    functions . at name .= mbody

-- | Executable commands.
class Eval a where
    -- | Execute a command, and return its return value.
    eval :: a -> Bash Int

instance Eval a => Eval [a] where
    eval [] = return 0
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
        0 -> return 0
        _ -> eval cs
    eval (Or  p cs) = eval p >>= \case
        0 -> eval cs
        r -> return r

instance Eval Pipeline where
    eval (Pipeline b cs) = case cs of
        []  -> bang $ return 0
        [c] -> bang $ eval c
        cs  -> bang $ subshell $ eval cs
      where
        bang   = if b then invert else id
        invert = fmap $ \r -> if r == 0 then 1 else 0


instance Eval SimpleCommand where
    eval (SimpleCommand as []) = eval as
    eval (SimpleCommand _  c ) = command c

instance Eval Assign where
    eval (Assign name op value) = 0 <$ case op of
        Equals     -> assign name value
        PlusEquals -> augment name value

instance Eval Function where
    eval (Function name body) = 0 <$ define name (eval body)

instance Eval ShellCommand where
    eval (Subshell l  ) = subshell $ eval l
    eval (Group l     ) = eval l
    eval (Arith _     ) = empty
    eval (Cond ws     ) = test ws
    eval (For _ _ l   ) = dirty $ eval l
    eval (ArithFor _ l) = dirty $ eval l
    eval (Select _ _ l) = dirty $ eval l
    eval (Case _ cs   ) = eval cs
    eval (If p t f    ) = cond (eval p) (eval t) (eval f)
    eval (Until p l   ) = dirty (eval p) >> dirty (eval l)
    eval (While p l   ) = dirty (eval p) >> dirty (eval l)

instance Eval CaseClause where
    eval (CaseClause _ l _) = dirty $ eval l
