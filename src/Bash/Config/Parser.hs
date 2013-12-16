{-# LANGUAGE LambdaCase, OverloadedStrings #-}
-- | The Bash parser.
--
-- The parser is built using Parsec over a custom token stream for
-- context-sensitive lexical analysis. The parser communicates with the
-- lexer by specifying lexical analysis modes.
module Bash.Config.Parser
    ( parse
    ) where

import           Control.Applicative    hiding (optional, many)
import           Control.Monad.Identity
import           Data.Char
import           Data.Maybe
import           Text.Parsec.Combinator hiding (anyToken, eof, notFollowedBy)
import           Text.Parsec.Error      (ParseError)
import           Text.Parsec.Pos
import           Text.Parsec.Prim       (ParsecT, skipMany, many, try)
import qualified Text.Parsec.Prim       as P

import           Bash.Config.Expand
import           Bash.Config.Lexer      hiding (word)
import           Bash.Config.Types
import           Bash.Config.Word

-------------------------------------------------------------------------------
-- Parser type
-------------------------------------------------------------------------------

-- | A Bash parser.
type Parser = ParsecT Tokens () Identity

-- | Run the parser with a source name and a 'String' input.
parse :: SourceName -> String -> Either ParseError Script
parse source s = P.parse script source (makeTokens NormalMode source s)

-------------------------------------------------------------------------------
-- Parser primitives
-------------------------------------------------------------------------------

-- | Modify a parser's input.
modifyInput :: Monad m => (s -> s) -> ParsecT s u m ()
modifyInput f = P.setInput . f =<< P.getInput

-- | Parse with a token mode, resetting to normal mode after the parser runs.
withMode :: TokenMode -> Parser a -> Parser a
withMode mode p = tokenMode mode *> p <* tokenMode NormalMode
  where
    tokenMode = modifyInput . setTokenMode

-- | A primitive token parser.
token :: (Token -> Maybe a) -> Parser a
token = P.tokenPrim showToken updatePos
  where
    updatePos _ _ = sourcePos

-- | Parse any token.
anyToken :: Parser Token
anyToken = token Just

-- | Parse a given word token.
word :: Word -> Parser Word
word w = token $ \case
    TWord w' | w' == w -> Just w'
    _                  -> Nothing

-- | Parse any word token.
anyWord :: Parser Word
anyWord  = token $ \case
    TWord w -> Just w
    _       -> Nothing

-- | Parse a reserved word.
reservedWord :: Parser Word
reservedWord = token $ \case
    TWord w | w `elem` reservedWords -> Just w
    _                                -> Nothing

-- | Parse a word that is not a reserved word.
unreservedWord :: Parser Word
unreservedWord = token $ \case
    TWord w | w `notElem` reservedWords -> Just w
    _                                   -> Nothing

-- | Parse a given operator.
operator :: String -> Parser String
operator op = token $ \case
    TOperator op' | op == op' -> Just op
    _                         -> Nothing
-- | Parse any operator.
anyOperator :: Parser String
anyOperator = token $ \case
    TOperator op -> Just op
    _            -> Nothing

-- | Parse an IO redirection number.
number :: Parser Int
number = token $ \case
    TNumber i -> Just i
    _         -> Nothing

-- | Parse an assignment word.
assign :: Parser Assign
assign = withMode AssignMode $ token $ \case
    TAssign s -> Just s
    _         -> Nothing

-- | Parse a word representing a flag.
flag :: Parser Word
flag = token $ \case
    TWord w | isFlag (toString w) -> Just w
    _                             -> Nothing
  where
    isFlag s = not (null s)
            && head s == '-'
            && all (\c -> isAlpha c || c == '-') s

-- | Parse a list of flags.
flags :: Parser [Word]
flags = many flag

-- | Parse an arithmetic expression, including the trailing @))@.
arith :: Parser String
arith = withMode ArithMode $ token $ \case
    TArith s -> Just s
    _        -> Nothing

-- | Notify the lexer that the following lines contain a heredoc.
needHeredoc :: String -> Parser ()
needHeredoc = modifyInput . queueHeredoc

-- | @notFollowedBy p@ succeeds if parser @p@ fails.
notFollowedBy :: Parser Token -> Parser ()
notFollowedBy p = try (more <|> return ())
  where
    more = do
        t <- p
        P.unexpected (showToken t)

-- | Parse the end of a file.
eof :: Parser ()
eof = notFollowedBy anyToken

-------------------------------------------------------------------------------
-- Basic parsers
-------------------------------------------------------------------------------

-- | Skip a list terminator.
listTerm :: Parser ()
listTerm = term *> newlineList
  where
    term = operator "\n"
       <|> operator ";"
       <|> operator "&"

-- | Skip zero or more newlines.
newlineList :: Parser ()
newlineList = skipMany (operator "\n")

-------------------------------------------------------------------------------
-- Simple commands
-------------------------------------------------------------------------------

-- | Skip a redirection.
redir :: Parser ()
redir = optional number
        *> choice (map redirect redirOps ++ map heredoc heredocOps)
  where
    redirect op = () <$ operator op <* anyWord
    heredoc  op = operator op *> (needHeredoc . unquote =<< anyWord)

-- | Skip a list of redirections.
redirList :: Parser ()
redirList = skipMany redir

-- | Parse part of a command.
commandParts :: Parser a -> Parser [a]
commandParts p = catMaybes <$> many part
  where
    part = Just    <$> p
       <|> Nothing <$  redir

-- | Parse an assignment command.
assignCommand :: Parser SimpleCommand
assignCommand = AssignCommand <$> assignBuiltin <* flags <*> commandParts arg
  where
    arg = Left  <$> assign
      <|> Right <$> anyWord

    assignBuiltin = try $ do
        w <- anyWord
        guard (w `elem` assignBuiltins)
        return w

    assignBuiltins = [ "alias", "declare", "export"
                     , "local", "readonly", "typeset"
                     ]

-- | Parse a simple command.
simpleCommand :: Parser SimpleCommand
simpleCommand = do
    notFollowedBy (TWord <$> reservedWord)
    as <- commandParts assign
    assignCommand <|> simple as
  where
    simple as = do
        ws <- commandParts anyWord
        guard (not $ null as && null ws)
        return (SimpleCommand as ws)

-------------------------------------------------------------------------------
-- Lists
-------------------------------------------------------------------------------

-- | Parse a pipeline.
pipelineCommand :: Parser Pipeline
pipelineCommand = time *> (bang <|> pipeline0)
              <|> bang
              <|> pipeline1
  where
    bang = invert <$ word "!" <*> pipeline0

    pipeline0 = Pipeline False <$> command `sepBy`  pipelineSep
    pipeline1 = Pipeline False <$> command `sepBy1` pipelineSep

    pipelineSep = (operator "|" <|> operator "|&") <* newlineList

    time = word "time" *> flags

    invert (Pipeline b cs) = Pipeline (not b) cs

-- | Parse a compound list of commands.
compoundList :: Parser List
compoundList = List <$ newlineList <*> list
  where
    list = andOr `sepEndBy1` listTerm

    andOr = do
        p <- pipelineCommand
        let rest = And p <$ operator "&&" <* newlineList <*> andOr
               <|> Or  p <$ operator "||" <* newlineList <*> andOr
        rest <|> pure (Last p)

-- | Parse a possible empty compound list of commands.
inputList :: Parser List
inputList = newlineList *> option (List []) compoundList

-- | Parse a command group, wrapped either in braces or in a @do...done@ block.
doGroup :: Parser List
doGroup = word "do" *> compoundList <* word "done"
      <|> word "{"  *> compoundList <* word "}"

-------------------------------------------------------------------------------
-- Compound commands
-------------------------------------------------------------------------------

-- | Parse a compound command.
shellCommand :: Parser ShellCommand
shellCommand = forCommand
           <|> caseCommand
           <|> whileCommand
           <|> untilCommand
           <|> selectCommand
           <|> ifCommand
           <|> subshell
           <|> group
           <|> arithCommand
           <|> condCommand

-- | Parse a @case@ command.
caseCommand :: Parser ShellCommand
caseCommand = Case <$ word "case"
          <*> anyWord <* newlineList
          <*  word "in" <* newlineList
          <*> clauses
  where
    clauses = [] <$ word "esac"
          <|> do p <- pattern
                 c <- inputList
                 nextClause (CaseClause p c)

    nextClause f = (:) <$> (f <$> clauseTerm) <* newlineList <*> clauses
               <|> [f Break] <$ newlineList <* word "esac"

    pattern = optional (operator "(")
           *> anyWord `sepBy` operator "|"
           <* operator ")"

    clauseTerm = Break       <$ operator ";;"
             <|> FallThrough <$ operator ";&"
             <|> Continue    <$ operator ";;&"

-- | Parse a @while@ command.
whileCommand :: Parser ShellCommand
whileCommand = While <$ word "while"
           <*> compoundList
           <*  word "do" <*> compoundList <* word "done"

-- | Parse an @until@ command.
untilCommand :: Parser ShellCommand
untilCommand = Until <$ word "until"
           <*> compoundList
           <*  word "do" <*> compoundList <* word "done"

-- | Parse a list of words for a @for@ or @select@ command.
wordList :: Parser [Word]
wordList = [] <$ operator ";" <* newlineList
       <|> newlineList *> inList
  where
    inList = word "in" *> many anyWord <* listTerm
         <|> return []

-- | Parse a @for@ command.
forCommand :: Parser ShellCommand
forCommand = word "for" *> (arithFor_ <|> for_)
  where
    arithFor_ = ArithFor
            <$  operator "((" <*> arith
            <*  optional listTerm
            <*> doGroup

    for_ = For <$> anyWord <*> wordList <*> doGroup

-- | Parse a @select@ command.
selectCommand :: Parser ShellCommand
selectCommand = Select <$ word "select" <*> anyWord <*> wordList <*> doGroup

-- | Parse an @if@ command.
ifCommand :: Parser ShellCommand
ifCommand = word "if" *> if_
  where
    if_ = If <$> compoundList <* word "then" <*> compoundList <*> alternative

    alternative = singleton . Shell <$ word "elif" <*> if_
              <|> word "else" *> compoundList <* word "fi"
              <|> List [] <$ word "fi"

    singleton c = List [Last (Pipeline False [c])]

-- | Parse a subshell command.
subshell :: Parser ShellCommand
subshell = Subshell <$ operator "(" <*> compoundList <* operator ")"

-- | Parse a command group.
group :: Parser ShellCommand
group = Group <$ word "{" <*> compoundList <* word "}"

-- | Parse an arithmetic command.
arithCommand :: Parser ShellCommand
arithCommand = Arith <$ operator "((" <*> arith

-- | Parse a conditional command.
condCommand :: Parser ShellCommand
condCommand = Cond <$ word "[[" <*> go
  where
    go = []  <$  word "]]"
     <|> (:) <$> condPart <*> go

    condPart = anyWord
           <|> fromString <$> anyOperator

-------------------------------------------------------------------------------
-- Coprocesses
-------------------------------------------------------------------------------

-- | Parse a coprocess command.
coproc :: Parser Command
coproc = word "coproc" *> coprocCommand
  where
    coprocCommand = try namedCommand
                <|> Shell <$> shellCommand
                <|> Simple <$> simpleCommand

    namedCommand = Shell <$ unreservedWord <*> shellCommand

-------------------------------------------------------------------------------
-- Function definitions
-------------------------------------------------------------------------------

-- | Parse a function definition.
functionDef :: Parser Command
functionDef = functionDef1
          <|> try functionDef2
  where
    functionDef1 = FunctionDef <$ word "function" <*> anyWord
                <* optional functionParens <*> functionBody

    functionDef2 = FunctionDef <$> unreservedWord
                <* functionParens <*> functionBody

    functionParens = () <$ operator "(" <* operator ")"

    functionBody = Function <$ newlineList <*> shellCommand <* redirList

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

-- | Parse a single command.
command :: Parser Command
command = Shell  <$> shellCommand <* redirList
      <|> Coproc <$  coproc       <* redirList
      <|> functionDef             <* redirList
      <|> Simple <$> simpleCommand

-- | Parse an entire script (e.g. a file) as a list of commands.
script :: Parser Script
script = Script <$> inputList <* eof
