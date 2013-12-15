{-# LANGUAGE LambdaCase, OverloadedStrings #-}
-- | Shell expansions.
module Bash.Config.Expand
    ( unquote
    , expandWord
    , expandWordList
    , expandValue
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import Text.Parsec         hiding ((<|>))

import Bash.Config.Types
import Bash.Config.Word

-- | Break a list on an element, if it exists.
breakOn :: Eq a => a -> [a] -> Maybe ([a], [a])
breakOn z = breakBy (== z)

-- | Break a list on an predicate, if it exists.
breakBy :: (a -> Bool) -> [a] -> Maybe ([a], [a])
breakBy p xs = case break p xs of
    (ys, (x:xs')) | p x -> Just (ys, xs')
    _                   -> Nothing

-- | Split a list on an element.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn z = splitBy (== z)

-- | Split a list on a predicate.
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p = go
  where
    go xs = case breakBy p xs of
        Nothing        -> [xs]
        Just (ys, xs') -> ys : go xs'

-- | Map a monadic action over a list and concatenate the results.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f

-- | Get the value of the IFS variable as a string.
ifsValue :: Bash String
ifsValue = valueString <$> value "IFS" <|> pure " \t\n"
  where
    valueString (Value v) = v
    valueString (Array _) = ""

-- | Brace expand a word.
-- TODO: {x..y}
braceExpand :: Word -> [Word]
braceExpand = go
  where
    go w = case brace w of
        Nothing -> [w]
        Just ws -> ws

    brace w = do
        (preamble, w')     <- breakOn (Char '{') w
        (amble, postamble) <- breakOn (Char '}') w'
        let as = expandAmble amble
            bs = braceExpand postamble
        return [preamble ++ a ++ b | a <- as, b <- bs]

    expandAmble = concatAmble . map braceExpand . splitOn (Char ',')

    concatAmble []   = ["{}"]
    concatAmble [xs] = map (\s -> "{" ++ s ++ "}") xs
    concatAmble xss  = concat xss

-- | Fail if a tilde expansion should be performed.
tildeExpand :: Word -> Bash Word
tildeExpand w@(Char '~':_) = unimplemented (toString w)
tildeExpand w              = return w

-- | Perform an expansion.
expansion :: String -> Bash String
expansion s = runParserT (brace <* eof) () s s >>= \case
    Left  _ -> unimplemented s
    Right r -> return r
  where
    brace = char '!' *> lift (unimplemented s)
        <|> char '#' *> lift (unimplemented s)
        <|> parameter

    parameter = do
        name <- many1 (alphaNum <|> char '_')
        fromValue <$> lift (value name)
      where
        fromValue (Value v)     = v
        fromValue (Array (v:_)) = v
        fromValue _             = ""

-- | Perform parameter expansion, arithmetic expansion, command
-- substitution, and process substitution.
expand :: Word -> Bash Word
expand = concatMapM $ \c -> case c of
    Double w         -> return . Double <$> expand w
    Backquote _      -> unimplemented (toString [c])
    Parameter s      -> fromString <$> expansion s
    BraceParameter w -> fromString <$> expansion (toString w)
    ArithSubst _     -> unimplemented (toString [c])
    CommandSubst _   -> unimplemented (toString [c])
    ProcessSubst _ _ -> unimplemented (toString [c])
    _                -> return [c]

-- | Split a word into multiple words.
splitWord :: Word -> Bash [Word]
splitWord w = do
    ifs <- ifsValue
    let isSep (Char c) | c `elem` ifs = True
        isSep _                       = False
    return (splitBy isSep w)

-- | Fail if a filename expansion should be performed.
filenameExpand :: Word -> Bash Word
filenameExpand w
    | any (== Char '*') w = unimplemented (toString w)
    | any (== Char '?') w = unimplemented (toString w)
    | hasCharClass        = unimplemented (toString w)
    | otherwise           = return w
  where
    hasCharClass = isJust $ do
        (_, w') <- breakOn (Char '[') w
        breakOn (Char ']') w'

-- | Expand a single word.
expandWord :: Word -> Bash String
expandWord = tildeExpand
         >=> expand
         >=> return . unquote

-- | Expand a list of words.
expandWordList :: [Word] -> Bash [String]
expandWordList = return . concatMap braceExpand
             >=> mapM tildeExpand
             >=> mapM expand
             >=> concatMapM splitWord
             >=> mapM filenameExpand
             >=> return . filter (not . null)
             >=> return . map unquote

-- | Expand a 'Value'.
expandValue :: (Value Word) -> Bash (Value String)
expandValue (Value v)  = Value <$> expandWord v
expandValue (Array vs) = Array <$> expandWordList vs
