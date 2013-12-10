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
    single = char '\'' *> B.many (B.noneOf "\'") <* char '\''
    double = char '\"' *> B.many (escape <|> B.noneOf "\"") <* char '\"'

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

-- | Brace expand a word. Expansions are either of the form @{a,b,c}@, or
-- @{x..y[..incr]}@ where @x@ and @y@ are either integers or single
-- characters, and @incr@ is an integer increment.
braceExpand :: String -> [String]
braceExpand = parseUnsafe "braceExpand" (map B.toString <$> go False)
  where
    go inner = try (brace inner)
           <|> return <$> glob (if inner then ",}" else "")

    brace inner = do
        a <- glob "{"
        _ <- char '{'
        b <- try sequenceExp <|> commaExp
        _ <- char '}'
        c <- go inner
        return (pure a <+> b <+> c)

    glob = word  -- TODO

    sequenceExp = do
        a   <- seqPart
        b   <- sep *> seqPart
        inc <- optional (sep *> number)
        map B.fromString <$> (charExp a b inc <|> arithExp a b inc)
      where
        seqPart = many1 (noneOf ".{,}")
        sep     = string ".."
        number  = seqPart >>= toNumber

    toChar [c] = pure c
    toChar _   = empty

    toNumber :: Alternative f => String -> f Int
    toNumber s = case reads s of
        [(n,"")] -> pure n
        _        -> empty

    enum :: (Ord a, Enum a) => a -> a -> Maybe Int -> [a]
    enum a b c = map toEnum [fromEnum a, fromEnum a + inc .. fromEnum b]
      where
        inc = case c of
            Just i              -> i
            Nothing | a > b     -> -1
                    | otherwise -> 1

    charExp a b inc = do
        start <- toChar a
        end   <- toChar b
        let chars = enum start end inc
        return $ map return chars

    arithExp a b inc = do
        start <- toNumber a
        end   <- toNumber b
        let pad     = leadingZero a || leadingZero b
            width   = max (length a) (length b)
            numbers = enum start end inc
        return $ map (if pad then showWidth width else show) numbers
      where
        leadingZero ('0':_:_)     = True
        leadingZero ('-':'0':_:_) = True
        leadingZero _             = False

        showWidth w n
            | n < 0     = '-' : showWidth (w - 1) (negate n)
            | otherwise = replicate (w - length s) '0' ++ s
          where
            s = show n

    commaExp = concatBrace <$> go True `sepBy` char ','
      where
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
