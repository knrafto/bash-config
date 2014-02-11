{-# LANGUAGE LambdaCase #-}
-- | Bash conditional commands.
module Bash.Config.Cond
    ( cond
    , test
    , test_
    ) where

import Control.Applicative
import Control.Arrow
import Control.Monad.Trans.Maybe
import Data.Traversable
import Language.Bash.Cond
import Language.Bash.Word

import Bash.Config.Env
import Bash.Config.Expand

-- | Evaluate a conditional expression.
eval :: CondExpr String -> ExitStatus
eval = \case
    Unary op s      -> lookupOp unaryOps op s
    Binary s1 op s2 -> lookupOp binaryOps op (s1, s2)
    Not e           -> not <$> eval e
    And e1 e2       -> (&&) <$> eval e1 <*> eval e2
    Or e1 e2        -> (||) <$> eval e1 <*> eval e2
  where
    lookupOp ops op = sequenceA (lookup op ops)

    unaryOps =
        [ (ZeroString   , null      )
        , (NonzeroString, not . null)
        ]

    binaryOps = map (second uncurry)
        [ (StrEQ, (==))
        , (StrNE, (/=))
        , (StrLT, (<) )
        , (StrGT, (>) )
        ]

-- | Evaluate a @[[...]]@ expression.
cond :: CondExpr Word -> Bash ExitStatus
cond e = (>>= eval) <$> runMaybeT (traverse expandWord e)

-- | Evaluate the @test@ builtin.
test :: [String] -> ExitStatus
test ss = case parseTestExpr ss of
    Left  _ -> Just False
    Right e -> eval e

-- | Evaluate the @[@ builtin.
test_ :: [String] -> ExitStatus
test_ ss = case unsnoc ss of
    Just (ss', "]") -> test ss'
    _               -> Just False
  where
    unsnoc [] = Nothing
    unsnoc xs = Just (init xs, last xs)
