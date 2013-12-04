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
import           Control.Monad.Trans
import           Data.Char
import           Data.Functor.Identity
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
parseUnsafeT :: Monad m => String -> ParsecT String () m a -> String -> m a
parseUnsafeT name p = liftM fromRight . runParserT p () ""
  where
    fromRight (Left  e) = error $ "Bash.Config.Expand." ++ name ++ ": " ++
                                  show e
    fromRight (Right r) = r

-- | Call 'error' with a function name if a parse fails.
parseUnsafe :: String -> Parser a -> String -> a
parseUnsafe name p = runIdentity . parseUnsafeT name p

-- | Return whether the given characters appear unquoted in a wordPart.
containsUnquoted :: String -> String -> Bool
containsUnquoted w cs = parseUnsafe "containsUnquoted" contains w
  where
    contains = not <$ wordPart cs <*> isEof
    isEof    = True <$ eof <|> pure False

-- | Get the value of the IFS variable as a string.
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

-- | Parse a wordPart, delimited by an unquoted occurence of one of the given
-- characters.
wordPart :: Monad m => String -> ParsecT String u m Builder
wordPart delims = B.many $ escape
                       <|> single
                       <|> double
                       <|> B.satisfy (`notElem` delims)
  where
    escape = B.char '\\' <+> (B.anyChar <|> return mempty)
    single = B.span '\'' '\'' empty
    double = B.span '\"' '\"' escape

-- | Parse and perform a parameter substitution.
parameterSubst :: ParsecT String () Bash Builder
parameterSubst = char '{' *> name <* char '}'
             <|> digit *> lift empty
             <|> name
             <|> pure (B.fromChar '$')
  where
    name = do
        n <- many1 (satisfy isNameLetter)
        B.fromString <$> lift (stringValue n)
      where
        isNameLetter c = isAlphaNum c || c == '_'

    stringValue = fmap asString . value
      where
        asString (Value v )    = v
        asString (Array [])    = ""
        asString (Array (v:_)) = v

-- | Brace expand a wordPart.
braceExpand :: String -> [String]
braceExpand = parseUnsafe "braceExpand" (map B.toString <$> go False)
  where
    go inner = try (brace inner)
           <|> return <$> wordPart (if inner then ",}" else "")

    brace inner = do
        a <- wordPart "{"
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

-- | Perform parameter expansion, arithmetic expansion, and command
-- substitution.
dollarExpand :: String -> Bash String
dollarExpand = parseUnsafeT "dollarExpand" (B.toString <$> go)
  where
    go     = B.many (try dollar <|> wordPart "$")
    dollar = char '$' *> (paren <|> parameterSubst)
    paren  = char '(' *> lift empty

-- | Split a wordPart into multiple split.
splitWord :: String -> Bash [String]
splitWord s = do
    ifs <- ifsValue
    return $ parseUnsafe "splitWord" (map B.toString <$> parts ifs) s
  where
    parts ifs = wordPart ifs `sepBy` many1 (satisfy (`elem` ifs))

-- | Fail if a filename expansion should be performed.
filenameExpand :: String -> Bash String
filenameExpand s
    | s `containsUnquoted` "*?[" = empty
    | otherwise                  = return s

expandWord :: String -> Bash String
expandWord = concatKliesli
    [ tildeExpand
    , processSubst
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
