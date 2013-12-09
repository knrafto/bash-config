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
    , (<+)
    , (+>)
    , many
    , many1
    , char
    , anyChar
    , satisfy
    , string
    , takeWhile
    , matchedPair
    , span
    ) where

import           Prelude             hiding (span, takeWhile)

import           Control.Applicative hiding (many)
import           Data.Monoid
import qualified Data.String         as S
import qualified Text.Parsec.Char    as P
import           Text.Parsec.Prim    hiding ((<|>), many)
import           Text.Parsec.String  ()

-- | A string builder.
newtype Builder = Builder { runBuilder :: String -> String }

instance Monoid Builder where
    mempty        = Builder id
    a `mappend` b = Builder (runBuilder a . runBuilder b)

instance S.IsString Builder where
    fromString = fromString

-- | Construct a 'Builder' from a 'Char'.
fromChar :: Char -> Builder
fromChar c = Builder (c :)

-- | Construct a 'Builder' from a 'String'.
fromString :: String -> Builder
fromString s = Builder (s ++)

-- | Convert a 'Builder' to a string.
toString :: Builder -> String
toString b = runBuilder b ""

infixl 4 <+>, <+, +>

-- | Sequence two functions and combine their results.
(<+>) :: (Applicative f, Monoid a) => f a -> f a -> f a
(<+>) = liftA2 mappend

-- | Combine an effectful result with a pure result.
(+>) :: (Applicative f, Monoid a) => a -> f a -> f a
a +> f = (a <>) <$> f

-- | Combine a pure result with an effectful result.
(<+) :: (Applicative f, Monoid a) => f a -> a -> f a
f <+ a = (<> a) <$> f

-- | Sequence a function repeatedly.
many :: (Alternative f, Monoid a) => f a -> f a
many p = go
  where
    go = p <+> go <|> pure mempty

-- | Sequence a function one or more times.
many1 :: (Alternative f, Monoid a) => f a -> f a
many1 p = p <+> many p

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

-- | @matchedPair start end escape@ parses the character @start@, then
-- repeatedly parses characters or @escape@ sequences until character
-- @end@ appears. Returns the contents of the matched pair.
matchedPair
    :: Stream s m Char
    => Char
    -> Char
    -> ParsecT s u m Builder
    -> ParsecT s u m Builder
matchedPair start end escape = P.char start *> many inner <* P.char end
  where
    inner = escape <|> satisfy (/= end)

-- | Parses a matched pair, including the outer characters.
span
    :: Stream s m Char
    => Char
    -> Char
    -> ParsecT s u m Builder
    -> ParsecT s u m Builder
span start end escape =
    fromChar start +> matchedPair start end escape <+ fromChar end
