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
import           Text.Parsec.Prim       hiding ((<|>), many)
import           Text.Parsec.Combinator hiding (optional)
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

-- | Get the value of the IFS variable as a string.
ifsValue :: Bash String
ifsValue = toString <$> value "IFS" <|> pure " \t\n"
  where
    toString (Value v) = v
    toString (Array _) = ""

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

-- | Parse part of a word.
wordPart :: Monad m => String -> ParsecT String u m Builder
wordPart delims = escape
              <|> single
              <|> double
              <|> B.satisfy (`notElem` delims)
  where
    escape = B.char '\\' <+> (B.anyChar <|> return mempty)
    single = B.span '\'' '\'' empty
    double = B.span '\"' '\"' escape

-- | Parse a word, delimited by an unquoted occurence of one of the given
-- characters.
word :: Monad m => String -> ParsecT String u m Builder
word = B.many . wordPart

-- | Parse a nonempty word, delimited by an unquoted occurence of one of the given
-- characters.
word1 :: Monad m => String -> ParsecT String u m Builder
word1 = B.many1 . wordPart

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

-- | Perform parameter expansion, arithmetic expansion, command
-- substitution, and process substitution.
expand :: String -> Bash String
expand = parseUnsafeT "expand" (B.toString <$> go)
  where
    go     = B.many $ angle
                  <|> dollar
                  <|> word1 "$<>"

    angle  = oneOf "<>" *> lift empty
    dollar = char '$' *> (paren <|> parameterSubst)
    paren  = char '(' *> lift empty

-- | Split a word into multiple split.
splitWord :: String -> Bash [String]
splitWord s = do
    ifs <- ifsValue
    return $ parseUnsafe "splitWord" (map B.toString <$> parts ifs) s
  where
    parts ifs = sep ifs *> many (word1 ifs <* sep ifs)
    sep   ifs = skipMany (oneOf ifs)

-- | Fail if a filename expansion should be performed.
filenameExpand :: String -> Bash String
filenameExpand s = parseUnsafeT "filenameExpand" (s <$ part) s
  where
    delims = "*?["
    part   = word delims *> optional (oneOf delims *> lift empty)

-- | Expand a single word.
expandWord :: String -> Bash String
expandWord = concatKliesli
    [ tildeExpand
    , expand
    , return . unquote
    ]

-- | Expand a list of words.
expandWordList :: [String] -> Bash [String]
expandWordList = concatKliesli
    [ return . concatMap braceExpand
    , mapM tildeExpand
    , mapM expand
    , return . filter (not . null)
    , fmap concat . mapM splitWord
    , mapM filenameExpand
    , return . map unquote
    ]

-- | Expand a 'Value'.
expandValue :: Value -> Bash Value
expandValue (Value v)  = Value <$> expandWord v
expandValue (Array vs) = Array <$> expandWordList vs
