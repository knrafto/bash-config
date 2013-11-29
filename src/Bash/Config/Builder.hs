{-# LANGUAGE FlexibleContexts #-}
-- | String builders and parsers.
module Bash.Config.Builder
    ( -- * Builders
      Builder(..)
    , fromChar
    , fromString
    , toString
      -- * Monoidal parsing
    , (<+>)
    , many
    , char
    , anyChar
    , satisfy
    , string
    , takeWhile
    ) where

import           Prelude             hiding (span, takeWhile)

import           Control.Applicative hiding (many)
import           Data.Monoid
import qualified Text.Parsec.Char    as P
import           Text.Parsec.Prim    hiding ((<|>), many)
import           Text.Parsec.String  ()

-- | A string builder.
newtype Builder = Builder { runBuilder :: String -> String }

instance Monoid Builder where
    mempty        = Builder id
    a `mappend` b = Builder (runBuilder a . runBuilder b)

-- | Construct a 'Builder' from a 'Char'.
fromChar :: Char -> Builder
fromChar c = Builder (c :)

-- | Construct a 'Builder' from a 'String'.
fromString :: String -> Builder
fromString s = Builder (s ++)

-- | Convert a 'Builder' to a string.
toString :: Builder -> String
toString b = runBuilder b ""

infixl 4 <+>

-- | Sequence two functions and combine their results.
(<+>) :: (Applicative f, Monoid a) => f a -> f a -> f a
(<+>) = liftA2 mappend

-- | Sequence a function repeatedly.
many :: (Alternative f, Monoid a) => f a -> f a
many p = go
  where
    go = p <+> go <|> pure mempty

-- | Parse a given character.
char :: Stream s m Char => Char -> ParsecT s u m Builder
char c = fromChar c <$ P.char c

-- | Parse any character.
anyChar :: Stream s m Char => ParsecT s u m Builder
anyChar = fromChar <$> P.anyChar

-- | Parse a character that satisfies a predicate.
satisfy :: Stream s m Char => (Char -> Bool) -> ParsecT s u m Builder
satisfy p = fromChar <$> P.satisfy p

-- | Parse a string of characters.
string :: Stream s m Char => String -> ParsecT s u m Builder
string s = fromString s <$ P.string s

-- | Parse characters while the predicate is satisfied.
takeWhile :: Stream s m Char => (Char -> Bool) -> ParsecT s u m Builder
takeWhile = many . satisfy
