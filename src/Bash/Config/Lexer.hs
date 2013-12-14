{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  #-}
-- | The Bash lexer.
module Bash.Config.Lexer
    ( -- * Tokens
      Token(..)
    , TokenMode(..)
    , showToken
      -- * Special tokens
    , reservedWords
    , redirOps
    , heredocOps
    , controlOps
    , normalOps
      -- * Parsers
    , name
    , word
      -- * Tokens
    , Tokens
    , nextToken
    , makeTokens
    , sourcePos
    , setTokenMode
    , queueHeredoc
    ) where

import Prelude                hiding (span)

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Functor.Identity
import Data.List              hiding (span)
import Data.Maybe
import Text.Parsec.Char       hiding (newline)
import Text.Parsec.Combinator hiding (optional)
import Text.Parsec.Prim       hiding ((<|>), many)
import Text.Parsec.Pos
import Text.Parsec.String     ()

import Bash.Config.Types
import Bash.Config.Word

-------------------------------------------------------------------------------
-- Tokens
-------------------------------------------------------------------------------

-- | Bash tokens.
data Token
      -- | A generic word.
    = TWord Word
      -- | A shell operator.
    | TOperator String
      -- | An IO redirection number.
    | TNumber Int
      -- | A shell variable assignment.
    | TAssign Assign
      -- | An arithmetic expression.
    | TArith String
    deriving (Eq)

-- | Lexer token modes.
data TokenMode
      -- | Lex normal tokens, operators, and redirection numbers.
    = NormalMode
      -- | Lex shell variable assignments.
    | AssignMode
      -- | Lex arithmetic expressions.
    | ArithMode
    deriving (Eq, Ord, Enum, Bounded)

-- | Convert a token to the string it represents.
showToken :: Token -> String
showToken = \case
    TWord s     -> toString s
    TOperator s -> s
    TNumber i   -> show i
    TAssign a   -> showAssign a
    TArith s    -> s
  where
    showAssign (Assign lhs op rhs) = lhs ++ showAssignOp op ++ showValue rhs

    showAssignOp Equals     = "="
    showAssignOp PlusEquals = "+="

    showValue v = case fmap toString v of
        Value s -> s
        Array a -> "(" ++ intercalate " " a ++ ")"

-------------------------------------------------------------------------------
-- Special tokens
-------------------------------------------------------------------------------

-- | Shell metacharacters, which delimit tokens.
metachars :: String
metachars = " \t\n;|&()<>"

-- | Shell reserved words.
reservedWords :: [Word]
reservedWords =
    [ "!", "[[", "]]", "{", "}"
    , "if", "then", "else", "elif", "fi"
    , "case", "esac", "for", "select", "while", "until"
    , "in", "do", "done", "time", "function"
    ]

-- | Redirection operators, not including heredoc operators.
redirOps :: [String]
redirOps = [">", "<", ">>", ">|", "<>", "<<<", "<&", ">&", "&>", "&>>"]

-- | Heredoc operators.
heredocOps :: [String]
heredocOps = ["<<", "<<-"]

-- | Shell control operators.
controlOps :: [String]
controlOps =
    [ "(", ")", "((", "))", ";;", ";&", ";;&"
    , "|", "|&", "||", "&&", ";", "&", "\n"
    ]

-- | All normal mode operators.
normalOps :: [String]
normalOps = redirOps ++ heredocOps ++ controlOps

-------------------------------------------------------------------------------
-- Lexer type
-------------------------------------------------------------------------------

-- | The lexer state.
newtype LexerState = LexerState
    { -- | A list of heredoc delimiters, in reverse order.
      heredocs :: [String]
    }

-- | Parsec's state.
type ParsecState = State String LexerState

-- | Construct a new lexer state.
initialState :: LexerState
initialState = LexerState { heredocs = [] }

-- | Push a new heredoc delimiter.
addHeredoc :: String -> LexerState -> LexerState
addHeredoc h s = s { heredocs = h : heredocs s }

-- | The lexer monad.
type Lexer = Parsec String LexerState

-- | Run the lexer.
runLexer :: Lexer a -> ParsecState -> Maybe (a, ParsecState)
runLexer m s = case reply of
    Ok a s' _ -> Just (a, s')
    _         -> Nothing
  where
    consumed = runIdentity $ runParsecT m s
    reply    = runIdentity $ case consumed of
        Consumed a -> a
        Empty a    -> a

-------------------------------------------------------------------------------
-- Basic Bash lexers
-------------------------------------------------------------------------------

-- | A flipped version of `(<$>)`.
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

-- | Ensure that a lexer's output is nonempty.
nonempty :: MonadPlus m => m [a] -> m [a]
nonempty m = m >>= \xs -> if null xs then mzero else return xs

-- | @upTo n p@ parses zero to @n@ occurences of @p@.
upTo :: Alternative f => Int -> f a -> f [a]
upTo m p = go m
  where
    go n | n <= 0    = pure []
         | otherwise = (:) <$> p <*> go (n - 1) <|> pure []

-- | @upTo1 n p@ parses one to @n@ occurences of @p@.
upTo1 :: Alternative f => Int -> f a -> f [a]
upTo1 n p = (:) <$> p <*> upTo (n - 1) p

-- | Take a line of characters from the source, stripping the trailing
-- newline (if any). Fails if there is no input left.
takeLine :: Stream s m Char => ParsecT s u m String
takeLine = lookAhead anyChar
        *> many (satisfy (/= '\n')) <* optional (char '\n')

-- | Skip spaces, tabs, and comments.
skipSpace :: Stream s m Char => ParsecT s u m ()
skipSpace = () <$ skipMany spaceChar <* optional comment
  where
    spaceChar = '\n' <$ try (string "\\\n") <|> oneOf " \t"
    comment   = char '#' *> skipMany (satisfy (/= '\n'))

-- | Lex a newline, and skip any following heredocs.
newline :: Lexer String
newline = do
    _ <- char '\n'
    hs <- heredocs <$> getState
    modifyState $ \s -> s { heredocs = [] }
    mapM_ skipHeredoc (reverse hs)
    return "\n"

-- | Skip a heredoc delimited by a string.
skipHeredoc :: Stream s m Char => String -> ParsecT s u m ()
skipHeredoc h = go
  where
    go = optional takeLine >>= \case
        Just l | l /= h -> go
        _               -> return ()

-- | Parse an arithmetic expression.
arith :: Stream s m Char => ParsecT s u m String
arith = toString <$> go
  where
    go = many inner

    inner = Paren <$ char '(' <*> go <* char ')'
        <|> Char <$> satisfy (/= ')')

-- | Parse the longest available operator from a list.
operator :: Stream s m Char => [String] -> ParsecT s u m String
operator = go
  where
    go ops
        | null ops      = empty
        | "" `elem` ops = try (continue ops) <|> pure ""
        | otherwise     = continue ops

    continue ops = do
        c <- anyChar
        (c :) <$> go (prefix c ops)

    prefix c = map tail . filter (\x -> not (null x) && head x == c)

-- | Parse a name.
name :: Stream s m Char => ParsecT s u m String
name = (:) <$> nameStart <*> many nameLetter
  where
    nameStart  = letter   <|> char '_'
    nameLetter = alphaNum <|> char '_'

-- | @span start end escape@ parses a span starting with @start@ and ending
-- with @end@, with possible @escape@ sequences inside.
span
    :: Stream s m Char
    => Char -> Char
    -> ParsecT s u m Span
    -> ParsecT s u m Word
span start end escape = char start *> many inner <* char end
  where
    inner = escape
        <|> Char <$> satisfy (/= end)

-- | Parse an ANSI C string in single quotes.
ansiString :: Stream s m Char => ParsecT s u m Word
ansiString = span '\'' '\'' (try escape)
  where
    escape = Escape <$ char '\\' <*> escapeCode

    escapeCode = charCodes
             <|> char 'x' *> hex 2
             <|> char 'u' *> hex 4
             <|> char 'U' *> hex 8
             <|> oct 3
             <|> char 'c' *> ctrlCodes

    charCodes = codes "abeEfnrtv\\\'\"" "\a\b\ESC\ESC\f\n\r\t\v\\\'\""

    ctrlCodes = '\FS' <$ try (string "\\\\")
            <|> codes "@ABCDEFGHIJKLMOPQRSTUVWXYZ[]^_?"
                      ("\NUL\SOH\STX\ETX\EOT\ENQ\ACK\BEL\BS\HT\LF\VT\FF" ++
                       "\CR\SO\SI\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM" ++
                       "\SUB\ESC\GS\RS\US\DEL")

    codes chars replacements = do
        c <- anyChar
        case lookup c table of
            Nothing -> unexpected [c]
            Just c' -> return c'
      where
        table = zip chars replacements

    oct n = number n 8 octDigit
    hex n = number n 16 hexDigit

    number maxDigits base baseDigit = do
        digits <- map digitToInt <$> upTo1 maxDigits baseDigit
        let n = foldl' (\x d -> base*x + d) 0 digits
        return $ if n > ord maxBound then '\0' else chr n  -- arbitrary

-- | Parse a word.
word :: Stream s m Char => ParsecT s u m Word
word = many bare
  where
    bare = try (string "\\\n" *> bare)
       <|> escape
       <|> single
       <|> double
       <|> backquote
       <|> try specialQuote
       <|> dollar
       <|> processSubst
       <|> Char <$> noneOf metachars

    escape = Escape <$ char '\\' <*> anyChar

    single = Single <$> span '\'' '\'' empty

    double = Double <$> span '\"' '\"' (escape <|> backquote <|> dollar)

    backquote = Backquote <$> span '`'  '`'  escape

    specialQuote = char '$' *> rest
      where
        rest = Single <$> ansiString
           <|> double

    dollar = char '$' *> rest
      where
        rest = expansion
           <|> braceExpansion
           <|> try arithSubst
           <|> commandSubst
           <|> return (Char '$')

    expansion = Expansion . return <$> digit
            <|> Expansion <$> name

    braceExpansion = BraceExpansion <$> span '{' '}' inner
      where
        inner = escape
            <|> single
            <|> double
            <|> backquote
            <|> dollar

    arithSubst   = ArithSubst <$ string "((" <*> arith <* string "))"
    commandSubst = CommandSubst <$> paren
    processSubst = ProcessSubst <$> oneOf "<>" <*> paren

    paren = char '(' *> many inner <* char ')'
      where
        inner = escape
            <|> single
            <|> double
            <|> backquote
            <|> dollar
            <|> Paren <$> paren
            <|> Comment <$ char '#' <*> many (satisfy (/= '\n'))
            <|> Char <$> satisfy (/= ')')

-------------------------------------------------------------------------------
-- Bash token lexers
-------------------------------------------------------------------------------

-- | Lex a bash token using a given lexical analysis mode.
lexBash :: TokenMode -> Lexer Token
lexBash NormalMode = lexNormal
lexBash AssignMode = lexAssign
lexBash ArithMode  = lexArith

-- | Lex a token in normal mode.
lexNormal :: Lexer Token
lexNormal = normalWord
        <|> TOperator <$> newline
        <|> TOperator <$> operator normalOps
  where
    normalWord = do
        w <- nonempty word
        optional (lookAhead anyChar) <&> \case
            Just c | isAngle c && all isDigitChar w
                -> TNumber (read (toString w))
            _   -> TWord w

    isAngle c = c == '<' || c == '>'

    isDigitChar (Char c) | isDigit c = True
    isDigitChar _                    = False

-- | Lex a token in assignment mode.
lexAssign :: Lexer Token
lexAssign = TAssign <$> assign
  where
    assign = Assign <$> lhs <*> assignOp <*> rhs

    lhs = (++) <$> name <*> option "" subscript

    subscript = wrap <$ char '[' <*> many (satisfy (/= ']')) <* char ']'
      where
        wrap s = "[" ++ s ++ "]"

    assignOp = operator ["=", "+="] <&> \case
        "="  -> Equals
        "+=" -> PlusEquals
        _    -> error "Bash.Lexer.lexAssign: unknown operator"

    rhs = Array <$  char '(' <*> arrayElems <* char ')'
      <|> Value <$> word

    arrayElems = skipArraySpace >> many (nonempty word <* skipArraySpace)

    skipArraySpace = skipSpace >> (char '\n' *> skipArraySpace <|> return ())

-- | Lex a token in arithmetic mode.
lexArith :: Lexer Token
lexArith = TArith <$> arith

-------------------------------------------------------------------------------
-- Parsec token stream
-------------------------------------------------------------------------------

-- | A Parsec token stream, keeping track of token modes. The stream caches
-- lexed tokens for the current token mode. Re-reading lexed tokens is fast,
-- but switching token modes can be expensive.
data Tokens = Tokens
    { -- | The current lexer state.
      tokenState :: ParsecState
      -- | The current token mode.
    , tokenMode  :: TokenMode
      -- | Get the next token, if possible.
    , nextToken  :: Maybe (Token, Tokens)
    }

instance Monad m => Stream Tokens m Token where
    uncons = return . nextToken

-- | Construct a token stream from a lexer state.
toTokens :: ParsecState -> TokenMode -> Tokens
toTokens s mode = Tokens
    { tokenState = s'
    , tokenMode  = mode
    , nextToken  = next
    }
  where
    s'   = fromMaybe s . fmap snd $ runLexer skipSpace s
    next = case runLexer (lexBash mode) s' of
        Just (t, s'') -> Just (t, toTokens s'' mode)
        _             -> Nothing

-- | Construct a named token stream.
makeTokens :: TokenMode -> SourceName -> String -> Tokens
makeTokens mode source s = toTokens initialParsecState mode
  where
    initialParsecState = State
        { stateInput = s
        , statePos   = initialPos source
        , stateUser  = initialState
        }

-- | Get the input source position.
sourcePos :: Tokens -> SourcePos
sourcePos = statePos . tokenState

-- | Set the lexer token mode.
setTokenMode :: TokenMode -> Tokens -> Tokens
setTokenMode mode i = toTokens (tokenState i) mode

-- | Notify the lexer that a heredoc delimited by a word is needed.
queueHeredoc :: String -> Tokens -> Tokens
queueHeredoc h i = toTokens (queue $ tokenState i) (tokenMode i)
  where
    queue s = s { stateUser = addHeredoc h (stateUser s) }
