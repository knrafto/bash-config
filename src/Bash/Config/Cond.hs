{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, LambdaCase #-}
-- | Bash conditional commands.
module Bash.Config.Cond
    ( test
    , test_
    , cond
    ) where

import Control.Applicative
import Data.Foldable
import Data.Functor.Identity
import Data.String
import Data.Traversable
import Text.Parsec.Combinator
import Text.Parsec.Prim       hiding ((<|>), token)
import Text.Parsec.String     ()
import Text.Parsec.Expr

import Bash.Config.Expand
import Bash.Config.Types      hiding (AndOr(..))
import Bash.Config.Word       (Word)

-------------------------------------------------------------------------------
-- Conditional Expressions
-------------------------------------------------------------------------------

-- | Bash conditional expressions.
data Expr a
    = String a
    | Unary String a
    | Binary a String a
    | Not (Expr a)
    | And (Expr a) (Expr a)
    | Or (Expr a) (Expr a)
    deriving (Eq, Functor, Foldable, Traversable)

-- | Evaluate a conditional expression.
eval :: Expr String -> ExitStatus
eval = \case
    String s        -> Just (not $ null s)
    Unary op s      -> lookupOp unaryOps op s
    Binary s1 op s2 -> lookupOp binaryOps op (s1, s2)
    Not e           -> not <$> eval e
    And e1 e2       -> (&&) <$> eval e1 <*> eval e2
    Or e1 e2        -> (||) <$> eval e1 <*> eval e2
  where
    lookupOp ops op = sequenceA (lookup op ops)

    unaryOps =
        [ ("-z", null      )
        , ("-n", not . null)
        ]

    binaryOps = map (\(op, f) -> (op, uncurry f))
        [ ("==", (==))
        , ("=" , (==))
        , ("!=", (/=))
        , ("<" , (<) )
        , (">" , (>) )
        ]

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

-- | A parser over lists of strings.
type Parser t = ParsecT [t] () Identity

-- | Parse a primitive token satisfying a predicate.
token :: Show t => (t -> Maybe a) -> Parser t a
token f = tokenPrim show (\pos _ _ -> pos) f

-- | Parse any word.
anyWord :: Show t => Parser t t
anyWord = token Just

-- | Parse a given word.
word :: (Eq t, Show t, IsString t) => String -> Parser t String
word s = token f
  where
    f t | t == fromString s = Just s
        | otherwise         = Nothing

-- | Parse a word in a list.
oneOf :: (Eq t, Show t, IsString t) => [String] -> Parser t String
oneOf ss = token (\t -> find ((== t) . fromString) ss)

-- | Given a word parser, parse a conditional expression.
condExpr :: (Eq t, Show t, IsString t) => Parser t (Expr t)
condExpr = expr <* eof
  where
    expr = buildExpressionParser opTable term

    term = word "(" *> expr <* word ")"
       <|> Unary <$> unaryOp <*> anyWord
       <|> (anyWord >>= wordTerm)

    wordTerm w = Binary w <$> binaryOp <*> anyWord
             <|> pure (String w)

    opTable =
        [ [Prefix (Not <$ word "!" )          ]
        , [Infix  (And <$ word "-a") AssocLeft]
        , [Infix  (Or  <$ word "-o") AssocLeft]
        ]

    unaryOp = oneOf $ map (\c -> ['-', c]) "abcdefghkprstuwxGLNOSovzn"

    binaryOp = oneOf
        [ "-ef", "-nt", "-ot"
        , "==", "=", "!=", "<", ">"
        , "-eq", "-ne", "-lt", "-le", "-gt", "-ge"
        ]

-------------------------------------------------------------------------------
-- Bash commands
-------------------------------------------------------------------------------

-- | Evaluate the @test@ builtin.
test :: [String] -> ExitStatus
test ss = case parse condExpr "" ss of
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

-- | Evaluate a Bash conditional expression.
cond :: [Word] -> Bash ExitStatus
cond ws = case parse condExpr "" ws of
    Left  _ -> return (Just False)
    Right e -> eval <$> traverse expandWord e
