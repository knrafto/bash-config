{-# LANGUAGE FlexibleInstances, LambdaCase #-}
-- | Manipulating Bash words.
module Bash.Config.Word
    ( -- * Words
      Word
    , Span(..)
      -- * Conversion
    , fromString
    , toString
    , unquote
    ) where

import           Prelude     hiding (span)

import qualified Data.String

-- | A Bash word.
type Word = [Span]

-- | One part of a Bash word.
data Span
    = Char Char
    | Escape Char
    | Single Word
    | Double Word
    | ANSI Word
    | Locale Word
    | Backquote Word
    | Expansion String
    | BraceExpansion Word
    | ArithSubst String
    | CommandSubst Word
    | ProcessSubst Char Word
    | Paren Word
    | Comment String
    deriving (Eq)

instance Data.String.IsString Word where
    fromString = fromString

-- | Convert a string to an unquoted word, without parsing.
fromString :: String -> Word
fromString = map Char

-- | Show a list of values with a function.
showMany :: (a -> ShowS) -> [a] -> ShowS
showMany f = foldr (.) id . map f

-- | Convert a word into a string.
toString :: Word -> String
toString = ($ "") . go
  where
    go = showMany $ \case
        Char c           -> showChar c
        Escape c         -> showChar '\\' . showChar c
        Single w         -> span "\'"  "\'" w
        Double w         -> span "\""  "\"" w
        ANSI w           -> span "$'"  "'"  w
        Locale w         -> span "$\"" "\"" w
        Backquote w      -> span "`"   "`"  w
        Expansion s      -> showChar '$' . showString s
        BraceExpansion w -> span "${" "}"  w
        ArithSubst s     -> showString "$((" . showString s . showString "))"
        CommandSubst w   -> span "$(" ")"  w
        ProcessSubst c w -> showChar c . span "(" ")" w
        Paren w          -> span "("  ")"  w
        Comment s        -> showChar '#' . showString s

    span start end w = showString start . go w . showString end

-- | Remove all quoting from a word. This unquotes quoted or escaped
-- characters, and removes expansions and substitutions.
unquote :: Word -> String
unquote = ($ "") . go
  where
    go = showMany $ \case
        Char c   -> showChar c
        Escape c -> showChar c
        Single w -> go w
        Double w -> go w
        ANSI w   -> go w
        Locale w -> go w
        _        -> id
