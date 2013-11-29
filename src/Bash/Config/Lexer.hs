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
    , TokenMode(..)
    , showToken
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
    , sourcePos
    , setTokenMode
    , queueHeredoc
    ) where

import           Prelude                   hiding (span)

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Functor.Identity
import           Data.List                 hiding (span)
import           Data.Maybe
import           Data.Monoid
import           Text.Parsec.Char          hiding (newline)
import           Text.Parsec.Prim          hiding ((<|>), many)
import           Text.Parsec.Pos

import           Bash.Config.Builder       (Builder, (<+>))
import qualified Bash.Config.Builder       as B
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

-- | Parse a span between two characters.
span :: Char -> Char -> Lexer Builder -> Lexer Builder
span start end p = B.char start <+> rest
  where
    rest = B.char end <|> p <+> rest

-- | Take a line of characters from the source, stripping the trailing
-- newline (if any). Fails if there is no input left.
takeLine :: Lexer String
takeLine = B.toString
       <$  lookAhead anyChar
       <*> B.many (B.satisfy (/= '\n'))
       <*  optional (char '\n')

-- | Skip spaces, tabs, and comments.
skipSpace :: Lexer ()
skipSpace = () <$ many spaceChar <* optional comment
  where
    spaceChar = try escape <|> satisfy (`elem` " \t")
    escape    = char '\\' *> char '\n'
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
skipHeredoc :: String -> Lexer ()
skipHeredoc h = go
  where
    go = optional takeLine >>= \case
        Just l | l /= h -> go
        _               -> return ()

-- | Parse an arithmetic expression.
arith :: Lexer Builder
arith = B.many (parens <|> B.satisfy (/= ')'))
  where
    parens = span '(' ')' (parens <|> B.anyChar)

-- | Lex the longest available operator from a list.
operator :: [String] -> Lexer String
operator = fmap B.toString . go
  where
    go ops
        | null ops      = empty
        | "" `elem` ops = try (continue ops) <|> pure mempty
        | otherwise     = continue ops

    continue ops = do
        c <- anyChar
        pure (B.fromChar c) <+> go (prefix c ops)

    prefix c = map tail . filter (\x -> not (null x) && head x == c)

-- | Lex a word, obeying and preserving quoting rules.
word :: Lexer String
word = B.toString <$> go
  where
    go = B.many $ try newlineEscape
              <|> escape
              <|> singleQuote
              <|> doubleQuote
              <|> try ansiQuote
              <|> try localeQuote
              <|> dollar
              <|> try angle
              <|> B.satisfy (`notElem` metachars)

    newlineEscape = mempty <$ string "\\\n"

    escape = B.char '\\' <+> B.anyChar

    singleQuote = span '\'' '\'' B.anyChar

    doubleQuote = span '\"' '\"' $ escape
                               <|> backquote
                               <|> dollar
                               <|> B.anyChar

    ansiQuote = char '$' *> ansiSpan

    ansiSpan  = span '\'' '\'' $ try singleEscape
                             <|> escape
                             <|> B.anyChar

    -- backslash escape a single quote outside the rest of the string
    singleEscape = B.fromString "'\\''" <$ string "\\'"

    localeQuote = char '$' *> doubleQuote

    angle = B.satisfy (`elem` "<>") <+> paren

    dollar = char '$' *> (parameter <|> try arith_ <|> paren)

    backquote = span '`' '`' $ escape
                           <|> dollar
                           <|> B.anyChar

    parameter = span '{' '}' $ escape
                           <|> singleQuote
                           <|> doubleQuote
                           <|> backquote
                           <|> dollar
                           <|> B.anyChar

    -- command substitutions, subshells, function parentheses, etc.
    paren = span '(' ')' $ escape
                       <|> singleQuote
                       <|> doubleQuote
                       <|> backquote
                       <|> paren
                       <|> dollar
                       <|> comment
                       <|> B.anyChar

    comment = B.char '#' <+> B.takeWhile (/= '\n')

    arith_ = B.string "((" <+> arith <+> B.string "))"

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
            Just c | isAngle c && all isDigit w -> TNumber (read w)
            _                                   -> TWord w

    isAngle c = c == '<' || c == '>'

-- | Lex a token in assignment mode.
lexAssign :: Lexer Token
lexAssign = TAssign <$> assign
  where
    assign = Assign <$> lhs <*> assignOp <*> rhs

    lhs = B.toString <$> (name <+> subscript)

    name = B.satisfy isNameStart
       <+> B.takeWhile isNameLetter

    isNameStart  c = isAlpha c || c == '_'
    isNameLetter c = isNameStart c || isDigit c

    subscript = B.char '[' <+> B.takeWhile (/= ']') <+> B.char ']'
            <|> pure mempty

    assignOp = operator ["=", "+="] <&> \case
        "="  -> Equals
        "+=" -> PlusEquals
        _    -> error "Bash.Lexer.assign: unknown operator"

    rhs = Array <$  char '(' <*> arrayElems
      <|> Value <$> word

    arrayElems  = skipArraySpace >> arrayElems_
    arrayElems_ = []  <$  char ')'
              <|> (:) <$> nonempty word <*> arrayElems

    skipArraySpace = skipSpace >> (char '\n' *> skipArraySpace <|> return ())

-- | Lex a token in arithmetic mode.
lexArith :: Lexer Token
lexArith = TArith . B.toString <$> arith

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
makeTokens mode name s = toTokens initialParsecState mode
  where
    initialParsecState = State
        { stateInput = s
        , statePos   = initialPos name
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
