{-# LANGUAGE LambdaCase #-}
-- | Bash script evaluation.
module Bash.Config.Eval
    ( Eval(..)
    ) where

import Control.Applicative
import Control.Monad.Reader.Class
import Control.Monad.State.Class

import Bash.Config.Types

-- | Execute with a 'Dirty' status.
dirty :: Bash a -> Bash a
dirty = local (const Dirty)

-- | Execute in a subshell. Environment changes during the subshell execution
-- will not affect the outside environment.
subshell :: Bash a -> Bash a
subshell action = do
    env <- get
    r <- action
    put env
    return r

-- | Invert a command's return value.
invert :: Bash Int -> Bash Int
invert = fmap (\r -> if r == 0 then 1 else 0)

-- | Evaluate a command as a predicate. Returns 'Nothing' if the command could
-- not be executed faithfully.
predicate :: Bash Int -> Bash (Maybe Bool)
predicate m = Just . (== 0) <$> m
          <|> return Nothing

-- | Execute a conditional (@if@) construct.
cond :: Bash Int -> Bash Int -> Bash Int -> Bash Int
cond p t f = predicate p >>= \case
        Nothing    -> dirty t >> dirty f
        Just False -> t
        Just True  -> f

-- | Execute a looping (@while@) construct.
loop :: Bash Int -> Bash Int -> Bash Int
loop p s = go 0
  where
    go r = do
        predicate p >>= \case
            Nothing    -> dirty s
            Just False -> s >>= go
            Just True  -> return r

-- | Evaluate a conditional or @test@ command.
test :: [String] -> Bash Int
test = undefined

-- | Execute a simple command.
command :: [String] -> Bash Int
command = undefined

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
        bang = if b then invert else id

instance Eval SimpleCommand where
    eval (SimpleCommand as []) = eval as
    eval (SimpleCommand _  c ) = command c

instance Eval Assign where
    eval = undefined

instance Eval Function where
    eval (Function name def) = undefined

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
    eval (Until p l   ) = loop (invert $ eval p) (eval l)
    eval (While p l   ) = loop (eval p) (eval l)

instance Eval CaseClause where
    eval (CaseClause _ l _) = dirty $ eval l
