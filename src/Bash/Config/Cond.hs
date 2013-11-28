{-# LANGUAGE LambdaCase #-}
-- | Bash conditional commands.
module Bash.Config.Cond
    ( test
    , test_
    , cond
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Functor.Identity
import Data.Traversable
import Text.Parsec.Prim      hiding ((<|>), token)
import Text.Parsec.String    ()
import Text.Parsec.Expr

import Bash.Config.Expand
import Bash.Config.Types     hiding (AndOr(..))

-------------------------------------------------------------------------------
-- Conditional Expressions
-------------------------------------------------------------------------------

-- | Bash conditional expressions.
data Expr
    = String String
    | Unary String String
    | Binary String String String
    | Not Expr
    | And Expr Expr
    | Or Expr Expr
    deriving (Eq, Show)

-- | Evaluate a conditional expression.
eval :: Expr -> ExitStatus
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
type ParserT = ParsecT [String] ()

-- | Parse a primitive token satisfying a predicate.
token :: Monad m => (String -> Bool) -> ParserT m String
token p = tokenPrim id (\pos _ _ -> pos) f
  where
    f a | p a       = Just a
        | otherwise = Nothing

-- | Parse any word.
anyWord :: Monad m => ParserT m String
anyWord = token (const True)

-- | Parse a given word.
word :: Monad m => String -> ParserT m String
word w = token (== w)

-- | Parse a word in a list.
oneOf :: Monad m => [String] -> ParserT m String
oneOf ws = token (`elem` ws)

-- | Parse the end of input.
eof :: Monad m => ParserT m ()
eof = try (anyWord *> empty <|> return ())

-- | Given a word parser, parse a conditional expression.
condExpr :: Monad m => ParserT m String -> ParserT m Expr
condExpr p = expr <* eof
  where
    expr = buildExpressionParser opTable term

    term = word "(" *> expr <* word ")"
       <|> Unary <$> unaryOp <*> p
       <|> (p >>= wordTerm)

    wordTerm w = Binary w <$> binaryOp <*> p
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

-- | Evaluate a conditional expression.
condWith :: Monad m => ParserT m String -> [String] -> m ExitStatus
condWith p = liftM evalEither . runParserT (condExpr p) () ""
  where
    evalEither (Left  _) = Just False
    evalEither (Right e) = eval e

-- | Evaluate the @test@ builtin.
test :: [String] -> ExitStatus
test = runIdentity . condWith anyWord

-- | Evaluate the @[@ builtin.
test_ :: [String] -> ExitStatus
test_ ws = case unsnoc ws of
    Just (ws', "]") -> test ws'
    _               -> Just False
  where
    unsnoc [] = Nothing
    unsnoc xs = Just (init xs, last xs)

cond :: [String] -> Bash ExitStatus
cond = condWith (anyWord >>= lift . expandWord)
