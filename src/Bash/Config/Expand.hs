{-# LANGUAGE LambdaCase #-}
-- | Shell expansions.
module Bash.Config.Expand
    ( unquote
    , expandWord
    , expandWordList
    , expandValue
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import           Text.Parsec.Char
import           Text.Parsec.Prim       hiding ((<|>))
import           Text.Parsec.Combinator
import           Text.Parsec.String

import           Bash.Config.Builder    (Builder, (<+>))
import qualified Bash.Config.Builder    as B
import           Bash.Config.Types

-- | Kliesli compose a list of monadic actions.
concatKliesli :: Monad m => [a -> m a] -> a -> m a
concatKliesli = foldr (>=>) return

-- | Call 'error' with a function name if a parse fails.
parseUnsafe :: String -> Parser a -> String -> a
parseUnsafe name p s = case parse p "" s of
    Left  e -> error $ "Bash.Config.Expand." ++ name ++ ": " ++ show e
    Right r -> r

-- | Return whether the given characters appear unquoted in a word.
containsUnquoted :: String -> String -> Bool
containsUnquoted w cs = case parse (word cs <* eof) "" w of
    Left  _ -> True
    Right _ -> False

-- | Get the value of the IFS variable.
ifsValue :: Bash String
ifsValue = ifs <|> pure " \t\n"
  where
    ifs = value "IFS" >>= \case
        Value v -> return v
        _       -> empty

-- | Unquote a string.
unquote :: String -> String
unquote = parseUnsafe "unquote" (B.toString <$> B.many naked)
  where
    naked  = escape
         <|> single
         <|> double
         <|> B.anyChar

    escape = char '\\' *> (B.anyChar <|> return mempty)
    single = B.matchedPair '\'' '\'' empty
    double = B.matchedPair '\"' '\"' escape

-- | Parse a word, delimited by an unquoted occurence of one of the given
-- characters.
word :: String -> Parser Builder
word delims = B.many $ escape
                   <|> single
                   <|> double
                   <|> B.satisfy (`notElem` delims)
  where
    escape = B.char '\\' <+> (B.anyChar <|> return mempty)
    single = B.span '\'' '\'' empty
    double = B.span '\"' '\"' escape

-- | Brace expand a word.
braceExpand :: String -> [String]
braceExpand = parseUnsafe "braceExpand" (map B.toString <$> go False)
  where
    go inner = try (brace inner)
           <|> return <$> word (if inner then ",}" else "")

    brace inner = do
        a <- word "{"
        _ <- char '{'
        b <- concatBrace <$> go True `sepBy` char ','
        _ <- char '}'
        c <- go inner
        return (pure a <+> b <+> c)

    concatBrace []   = [B.fromString "{}"]
    concatBrace [xs] = map (\x -> B.fromChar '{' <> x <> B.fromChar '}') xs
    concatBrace xss  = concat xss

-- | Fail if a tilde expansion should be performed.
tildeExpand :: String -> Bash String
tildeExpand ('~':_) = empty
tildeExpand s       = return s

-- | Fail if a process substitution should be performed.
processSubst :: String -> Bash String
processSubst s
    | s `containsUnquoted` "<>" = empty
    | otherwise                 = return s

dollarExpand :: String -> Bash String
dollarExpand = return  -- TODO

-- | Split a word into multiple split.
splitWord :: String -> Bash [String]
splitWord s = do
    ifs <- ifsValue
    return $ parseUnsafe "splitWord" (map B.toString <$> split ifs) s
  where
    split ifs = word ifs `sepBy` many1 (satisfy (`elem` ifs))

-- | Fail if a filename expansion should be performed.
filenameExpand :: String -> Bash String
filenameExpand s
    | s `containsUnquoted` "*?[" = empty
    | otherwise                  = return s

expandWord :: String -> Bash String
expandWord = concatKliesli
    [ tildeExpand
    , mapM processSubst
    , dollarExpand
    , return . unquote
    ]

expandWordList :: [String] -> Bash [String]
expandWordList = concatKliesli
    [ return . concatMap braceExpand
    , mapM tildeExpand
    , mapM processSubst
    , mapM dollarExpand
    , return . filter (not . null)
    , fmap concat . mapM splitWord
    , mapM filenameExpand
    , return . map unquote
    ]

expandValue :: Value -> Bash Value
expandValue = return  -- TODO
