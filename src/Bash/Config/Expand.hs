{-# LANGUAGE FlexibleContexts #-}
-- | Shell expansions.
module Bash.Config.Expand
    ( unquote
    , expandWord
    , expandWordList
    , expandValue
    ) where

import           Control.Applicative
import           Data.Monoid
import qualified Text.Parsec.Char    as P
import           Text.Parsec.Prim    hiding ((<|>), many)
import           Text.Parsec.String  ()

import           Bash.Config.Types

-- | A string builder.
newtype Builder = Builder { runBuilder :: String -> String }

instance Monoid Builder where
    mempty        = Builder id
    a `mappend` b = Builder (runBuilder a . runBuilder b)

-- | Construct a 'Builder' from a 'Char'.
fromChar :: Char -> Builder
fromChar c = Builder (c :)

-- | Convert a 'Builder' to a string.
toString :: Builder -> String
toString b = runBuilder b ""

infixl 4 <+>

-- | Sequence two functions and combine their results.
(<+>) :: (Applicative f, Monoid a) => f a -> f a -> f a
(<+>) = liftA2 mappend

-- | Parse a given character.
char :: Stream s m Char => Char -> ParsecT s u m Builder
char c = fromChar c <$ P.char c

-- | Parse any character.
anyChar :: Stream s m Char => ParsecT s u m Builder
anyChar = fromChar <$> P.anyChar

-- | Parse a character that satisfies a predicate.
satisfy :: Stream s m Char => (Char -> Bool) -> ParsecT s u m Builder
satisfy p = fromChar <$> P.satisfy p

-- | Unquote a string.
unquote :: String -> String
unquote s = case parse bare "" s of
    Left  e -> error (show e)
    Right b -> toString b
  where
    bare = char '\'' *> single
       <|> char '\"' *> double
       <|> char '\\' *> escape
       <|> anyChar <+> bare
       <|> pure mempty

    single = mconcat <$> many singleChar <* char '\'' <+> bare
    double = mconcat <$> many doubleChar <* char '\"' <+> bare

    singleChar = satisfy (/= '\'')
    doubleChar = char '\\' *> anyChar <|> satisfy (/= '\"')

    escape = anyChar <+> bare <|> pure mempty

expandWord :: String -> Bash String
expandWord = return  -- TODO

expandWordList :: [String] -> Bash [String]
expandWordList = return  -- TODO

expandValue :: Value -> Bash Value
expandValue = return  -- TODO
