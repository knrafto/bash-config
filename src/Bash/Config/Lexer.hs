{-# LANGUAGE
    BangPatterns
  , DeriveFunctor
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  #-}
-- | The Bash lexer.
--
-- The lexer uses a custom Lexer monad to parse individual tokens. The lexer
-- supports one-character lookahead and arbitrary backtracking when needed.
-- The parser communicates with the lexer by specifying lexical analysis modes.
module Bash.Config.Lexer
    ( -- * Tokens
      Token(..)
    , Located(..)
    , TokenMode(..)
    , showToken
    , untag
      -- * Special tokens
    , reservedWords
    , redirOps
    , heredocOps
    , controlOps
    , normalOps
      -- * Tokens
    , Tokens
    , nextToken
    , makeTokens
    , setTokenMode
    , queueHeredoc
    ) where

import           Prelude                   hiding (span)

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State.Class
import           Data.Char
import           Data.List                 hiding (span)
import           Data.Monoid
import           Text.Parsec               (Stream(..))
import           Text.Parsec.Pos           (SourceName, SourcePos)

import           Bash.Config.Source        (Source)
import qualified Bash.Config.Source        as S
import           Bash.Config.Types

-------------------------------------------------------------------------------
-- Tokens
-------------------------------------------------------------------------------

-- | Bash tokens.
data Token
      -- | A generic word.
    = TWord String
      -- | A shell operator.
    | TOperator String
      -- | An IO redirection number.
    | TNumber Int
      -- | A shell variable assignment.
    | TAssign Assign
      -- | An arithmetic expression.
    | TArith String
    deriving (Eq)

-- | A token with a position.
data Located = Located !SourcePos Token
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
    TWord s     -> s
    TOperator s -> s
    TNumber i   -> show i
    TAssign a   -> showAssign a
    TArith s    -> s
  where
    showAssign (Assign lhs op rhs) = lhs ++ showAssignOp op ++ showValue rhs

    showAssignOp Equals     = "="
    showAssignOp PlusEquals = "+="

    showValue (Value s) = s
    showValue (Array a) = "(" ++ intercalate " " a ++ ")"

-- | Untag a token.
untag :: Located -> Token
untag (Located _ t) = t

-------------------------------------------------------------------------------
-- Special tokens
-------------------------------------------------------------------------------

-- | Shell metacharacters, which delimit tokens.
metachars :: String
metachars = " \t\n;|&()<>"

-- | Shell reserved words.
reservedWords :: [String]
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
-- Lexer state
-------------------------------------------------------------------------------

-- | The lexer state.
data LexerState = LexerState
    { -- | The underlying source.
      source   :: Source
      -- | A string buffer, used to hold the current token.
    , buffer   :: Endo String
      -- | A list of needed heredoc delimiters, in reverse order.
    , heredocs :: [String]
    }

-- | Construct a new lexer state from a 'Source'.
makeLexerState :: Source -> LexerState
makeLexerState s = LexerState
    { source   = s
    , buffer   = mempty
    , heredocs = []
    }

-------------------------------------------------------------------------------
-- Lexer monad
-------------------------------------------------------------------------------

-- | A backtracking lexer monad.
newtype Lexer a = Lexer { runLexer :: LexerState -> Maybe (a, LexerState) }
    deriving (Functor)

instance Applicative Lexer where
    pure  = return
    (<*>) = ap

instance Alternative Lexer where
    empty = mzero
    (<|>) = mplus

instance Monad Lexer where
    return a = Lexer $ \s -> Just (a, s)
    m >>= k  = Lexer $ \s -> do
        (a, s') <- runLexer m s
        runLexer (k a) s'
    fail _   = mzero

instance MonadPlus Lexer where
    mzero     = Lexer $ \_ -> Nothing
    mplus a b = Lexer $ \s -> runLexer a s `mplus` runLexer b s

instance MonadState LexerState Lexer where
    get     = Lexer $ \s -> Just (s, s)
    put s   = Lexer $ \_ -> Just ((), s)
    state f = Lexer $ Just . f

-------------------------------------------------------------------------------
-- Lexer primitives
-------------------------------------------------------------------------------

-- | A flipped version of `(<$>)`.
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

-- | Use the lexer's internal buffer to lex a string, and returns the
-- buffer's. Clears the buffer both before and after running the lexer.
withBuffer :: Lexer () -> Lexer String
withBuffer action = do
    modify $ \s -> s { buffer = mempty }
    action
    b <- gets buffer
    modify $ \s -> s { buffer = mempty }
    return (appEndo b "")

-- | Return the next character.
peekChar :: Lexer (Maybe Char)
peekChar = gets (S.peekChar . source)

-- | Take the next character from the source.
takeChar :: Lexer Char
takeChar = gets (S.takeChar . source) >>= \case
    Nothing      -> empty
    Just (c, s') -> do modify $ \s -> s { source = s' }
                       return c

-- | Take a line of characters from the source, stripping the trailing
-- newline (if any).
takeLine :: Lexer String
takeLine = takeChar >>= \case
    '\n' -> return []
    c    -> (c :) <$> takeLine

-- | Add a character to the buffer.
addChar :: Char -> Lexer ()
addChar c = modify $ \s -> s { buffer = buffer s <> Endo (c:) }

-- | Move a character from the source to the buffer.
moveChar :: Lexer ()
moveChar = takeChar >>= addChar

-- | Move a character from the source to the buffer if the predicate is
-- satisfied, and fails otherwise.
satisfy :: (Char -> Bool) -> Lexer ()
satisfy p = peekChar >>= \case
    Just c | p c -> moveChar
    _            -> empty

-- | Continue skipping characters until the predicate is satisfied.
skipWhile :: (Char -> Bool) -> Lexer ()
skipWhile p = go
  where
    go = peekChar >>= \case
        Just c | p c -> takeChar >> go
        _            -> return ()

-- | Continue moving characters from the source to the buffer while the
-- predicate is satisfied. This lexer does not fail.
moveWhile :: (Char -> Bool) -> Lexer ()
moveWhile p = go
  where
    go = peekChar >>= \case
        Just c | p c -> moveChar >> go
        _            -> return ()

-- | Ensure that a lexer's output is nonempty.
nonempty :: Lexer [a] -> Lexer [a]
nonempty l = l >>= \xs -> if null xs then empty else return xs

-- | @span end f@ moves a string character-by-character until the
-- character @end@ is found and moved. For each character, the lexer
-- @f@ is run, which may handle escape sequences, comments, or other
-- special sequences.
span :: Char -> (Char -> Lexer ()) -> Lexer ()
span end f = go
  where
    go = peekChar >>= \case
        Nothing -> empty
        Just c  -> do moveChar
                      unless (c == end) $ f c >> go

-- | Tag a lexed token with its location.
locate :: Lexer Token -> Lexer Located
locate l = Located <$> gets (S.sourcePos . source) <*> l

-------------------------------------------------------------------------------
-- Basic Bash lexers
-------------------------------------------------------------------------------

-- | Skip spaces, tabs, and comments.
skipSpace :: Lexer ()
skipSpace = go
  where
    go = peekChar >>= \case
        Just ' '  -> takeChar >> go
        Just '\t' -> takeChar >> go
        Just '\\' -> (escape >> go) <|> return ()
        Just '#'  -> skipWhile (/= '\n')
        _         -> return ()

    escape = do
        _ <- takeChar
        n <- takeChar
        guard (n == '\n')

-- | Lex a newline, and skip any following heredocs.
newline :: Lexer String
newline = do
    c <- takeChar
    guard (c == '\n')
    hs <- gets heredocs
    modify $ \s -> s { heredocs = [] }
    mapM_ skipHeredoc (reverse hs)
    return "\n"

-- | Skip a heredoc delimited by a string.
skipHeredoc :: String -> Lexer ()
skipHeredoc h = go
  where
    go = do
        l <- takeLine
        unless (l == h) go

-- | Move an arithmetic expression into the buffer.
arith :: Lexer ()
arith = go
  where
    go = peekChar >>= \case
        Nothing  -> return ()
        Just ')' -> return ()
        Just '(' -> moveChar >> paren_ >> go
        _        -> moveChar >> go

    paren_ = span ')' $ \case
        '(' -> paren_
        _   -> return ()

-- | Lex the longest available operator from a list.
operator :: [String] -> Lexer String
operator = withBuffer . go
  where
    go ops
        | null ops      = empty
        | "" `elem` ops = continue ops <|> return ()
        | otherwise     = continue ops

    continue ops = do
        c <- takeChar
        addChar c
        go (prefix c ops)

    prefix c = map tail . filter (\x -> not (null x) && head x == c)

-- | Lex a word, obeying and preserving quoting rules.
word :: Lexer String
word = withBuffer go
  where
    go = peekChar >>= \case
        Nothing -> return ()
        Just c  -> char c

    char = \case
        '\\'                   -> escape >> go
        '\''                   -> continue singleQuote_
        '\"'                   -> continue doubleQuote_
        '`'                    -> continue backquote_
        '$'                    -> continue dollar_
        '<'                    -> continue angle_ <|> return ()
        '>'                    -> continue angle_ <|> return ()
        c | c `elem` metachars -> return ()
          | otherwise          -> moveChar >> go

    continue l = moveChar >> l >> go

    escape = takeChar >> peekChar >>= \case
        Just '\n' -> void takeChar
        _         -> addChar '\\' >> moveChar

    angle_ = peekChar >>= \case
        Just '(' -> moveChar >> paren_
        _        -> empty

    singleQuote_ = span '\'' $ \_ -> return ()

    doubleQuote_ = span '\"' $ \case
        '\\' -> moveChar
        '`'  -> backquote_
        '$'  -> dollar_
        _    -> return ()

    ansiQuote_ = span '\'' $ \case
        '\\' -> moveChar
        _    -> return ()

    backquote_ = span '`' $ \case
        '\\' -> moveChar
        '$'  -> dollar_
        _    -> return ()

    parameter_ = span '}' $ \case
        '\\' -> moveChar
        '\'' -> singleQuote_
        '\"' -> doubleQuote_
        '`'  -> backquote_
        '$'  -> dollar_
        _    -> return ()

    dollar_ = peekChar >>= \case
        Just '\'' -> moveChar >> ansiQuote_
        Just '\"' -> moveChar >> doubleQuote_
        Just '('  -> moveChar >> dollarParen_
        Just '{'  -> moveChar >> parameter_
        _         -> return ()

    dollarParen_ = peekChar >>= \case
        Just '(' -> moveChar >> arith_
        _        -> paren_

    -- command substitutions, subshells, function parentheses, etc.
    paren_ = span ')' $ \case
        '\\' -> moveChar
        '\'' -> singleQuote_
        '\"' -> doubleQuote_
        '`'  -> backquote_
        '('  -> paren_
        '$'  -> dollar_
        '#'  -> moveWhile (/= '\n')  -- comment
        _    -> return ()

    arith_ = arith <* satisfy (== ')') <* satisfy (== ')')

-------------------------------------------------------------------------------
-- Bash token lexers
-------------------------------------------------------------------------------

-- | Lex a bash token using a given lexical analysis mode.
lexBash :: TokenMode -> Lexer Located
lexBash NormalMode = lexNormal
lexBash AssignMode = lexAssign
lexBash ArithMode  = lexArith

-- | Lex a token in normal mode.
lexNormal :: Lexer Located
lexNormal = skipSpace *> locate token
  where
    token = normalWord
        <|> TOperator <$> newline
        <|> TOperator <$> operator normalOps

    normalWord = do
        w <- nonempty word
        peekChar <&> \case
            Just c | isAngle c && all isDigit w -> TNumber (read w)
            _                                   -> TWord w

    isAngle c = c == '<' || c == '>'

-- | Lex a token in assignment mode.
lexAssign :: Lexer Located
lexAssign = skipSpace *> locate (TAssign <$> assign)
  where
    assign = Assign <$> lhs <*> assignOp <*> rhs

    lhs = withBuffer $ do
        satisfy isNameStart
        _ <- many (satisfy isNameLetter)
        peekChar >>= \case
            Just '[' -> moveWhile (/= ']') >> satisfy (== ']')
            _        -> return ()
      where
        isNameStart  c = isAlpha c || c == '_'
        isNameLetter c = isNameStart c || isDigit c

    assignOp = operator ["=", "+="] <&> \case
        "="  -> Equals
        "+=" -> PlusEquals
        _    -> error "Bash.Lexer.assign: unknown operator"

    rhs = peekChar >>= \case
        Just '(' -> takeChar >> Array <$> arrayElems
        _        -> Value <$> word

    arrayElems = skipArraySpace >> peekChar >>= \case
        Just ')' -> [] <$ takeChar
        _        -> (:) <$> nonempty word <*> arrayElems

    skipArraySpace = skipSpace >> peekChar >>= \case
        Just '\n' -> takeChar >> skipArraySpace
        _         -> return ()

-- | Lex a token in arithmetic mode.
lexArith :: Lexer Located
lexArith = locate $ TArith <$> withBuffer arith

-------------------------------------------------------------------------------
-- Parsec token stream
-------------------------------------------------------------------------------

-- | A Parsec token stream, keeping track of token modes. The stream caches
-- lexed tokens for the current token mode. Re-reading lexed tokens is fast,
-- but switching token modes can be expensive.
data Tokens = Tokens
    { -- | The current lexer state.
      lexerState :: LexerState
      -- | The current token mode.
    , tokenMode  :: TokenMode
      -- | Get the next token, if possible.
    , nextToken  :: Maybe (Located, Tokens)
    }

instance Monad m => Stream Tokens m Located where
    uncons = return . nextToken

-- | Construct a token stream from a lexer state.
toTokens :: LexerState -> TokenMode -> Tokens
toTokens s mode = Tokens
    { lexerState = s
    , tokenMode  = mode
    , nextToken  = next
    }
  where
    next = case runLexer (lexBash mode) s of
        Just (t, s') -> Just (t, toTokens s' mode)
        _            -> Nothing

-- | Construct a named token stream.
makeTokens :: TokenMode -> SourceName -> String -> Tokens
makeTokens mode name s = toTokens (makeLexerState $ S.source name s) mode

-- | Set the lexer token mode.
setTokenMode :: TokenMode -> Tokens -> Tokens
setTokenMode mode i = toTokens (lexerState i) mode

-- | Notify the lexer that a heredoc delimited by a word is needed.
queueHeredoc :: String -> Tokens -> Tokens
queueHeredoc h i = toTokens (addHeredoc $ lexerState i) (tokenMode i)
  where
    addHeredoc s = s { heredocs = h : heredocs s }
