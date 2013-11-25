{-# LANGUAGE LambdaCase #-}
-- | The Bash parser.
--
-- The parser is built using Parsec over a custom token stream for
-- context-sensitive lexical analysis. The parser communicates with the
-- lexer by specifying lexical analysis modes.
module Bash.Config.Parse
    ( parse
    ) where

import           Control.Applicative    hiding (optional, many)
import           Control.Monad.Identity
import           Text.Parsec.Combinator hiding (anyToken, eof)
import           Text.Parsec.Error      (ParseError)
import           Text.Parsec.Pos
import           Text.Parsec.Prim       (ParsecT, skipMany, many)
import qualified Text.Parsec.Prim       as P

import           Bash.Config.Expand
import           Bash.Config.Lexer
import           Bash.Config.Types

-------------------------------------------------------------------------------
-- Parser type
-------------------------------------------------------------------------------

-- | A Bash parser.
type Parser = ParsecT Tokens () Identity

-- | Run the parser with a source name and a 'String' input.
parse :: SourceName -> String -> Either ParseError Script
parse name s = P.parse script name (makeTokens NormalMode name s)

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
token f = P.tokenPrim (showToken . untag) updatePos (f . untag)
  where
    updatePos _ (Located pos _) _ = pos

-- | Parse any token.
anyToken :: Parser Token
anyToken = token Just

-- | Parse a given word token.
word :: String -> Parser String
word s = token $ \case
    TWord s' | s == s' -> Just s
    _                  -> Nothing

-- | Parse any word token.
anyWord :: Parser String
anyWord  = token $ \case
    TWord s -> Just s
    _       -> Nothing

-- | Parse a word that is not a reserved word.
unreservedWord :: Parser String
unreservedWord = token $ \case
    TWord s | s `notElem` reservedWords -> Just s
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

-- | Parse an arithmetic expression.
arith :: Parser String
arith = withMode ArithMode $ token $ \case
    TArith s -> Just s
    _        -> Nothing

-- | Notify the lexer that the following lines contain a heredoc.
needHeredoc :: String -> Parser ()
needHeredoc = modifyInput . queueHeredoc

-- | Parse the end of a file.
eof :: Parser ()
eof = P.try (moreTokens <|> return ())
  where
    moreTokens = do
        t <- anyToken
        P.unexpected (showToken t)

-------------------------------------------------------------------------------
-- Basic parsers
-------------------------------------------------------------------------------

-- | Skip a list terminator.
listTerm :: Parser ()
listTerm = term <* newlineList
  where
    term = () <$ operator "\n"
       <|> () <$ operator ";"
       <|> eof

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

-- | Parse a simple command part.
commandPart :: Parser a -> Parser [a]
commandPart p = go
  where
    go = (:) <$> p <*> go
     <|> redir *> go
     <|> return []

-- | Parse a simple command beginning with the given word.
simpleCommand :: String -> Parser SimpleCommand
simpleCommand w = SimpleCommand [] . (w :) <$> commandPart anyWord

-- | Parse a simple command that begins with an assignment.
assignCommand :: Parser SimpleCommand
assignCommand =
    SimpleCommand <$> commandPart1 assign <*> commandPart anyWord
  where
    commandPart1 p = (:) <$> p <*> commandPart p
                 <|> redir *> commandPart1 p

-------------------------------------------------------------------------------
-- Lists
-------------------------------------------------------------------------------

-- | Parse a pipeline.
pipelineCommand :: Parser Pipeline
pipelineCommand = word "time" *> optional timeArgs *> (bang <|> pipeline0)
              <|> bang
              <|> pipeline1
  where
    bang = invert <$ word "!" <*> pipeline0

    pipeline0 = Pipeline False <$> command `sepBy`  pipelineSep
    pipeline1 = Pipeline False <$> command `sepBy1` pipelineSep

    pipelineSep = (operator "|" <|> operator "|&") <* newlineList

    timeArgs = word "-p" *> optional (word "--")

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
                 c <- clauseCommand
                 nextClause (CaseClause p c)

    nextClause f = (:) <$> (f <$> clauseTerm) <* newlineList <*> clauses
                 <|> [f Break] <$ newlineList <* word "esac"

    pattern = optional (operator "(")
           *> sepBy anyWord (operator "|")
           <* operator ")"

    clauseCommand = compoundList
                <|> List [] <$ newlineList

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
wordList :: Parser [String]
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
            <$  operator "((" <*> arith <* operator "))"
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
arithCommand = Arith <$ operator "((" <*> arith <* operator "))"

-- | Parse a conditional command.
condCommand :: Parser ShellCommand
condCommand = Cond <$ word "[[" <*> go
  where
    go = []  <$  word "]]"
     <|> (:) <$> anyWord     <*> go
     <|> (:) <$> anyOperator <*> go

-------------------------------------------------------------------------------
-- Coprocesses
-------------------------------------------------------------------------------

-- | Parse a coprocess command.
coproc :: Parser Command
coproc = word "coproc" *> coprocCommand
  where
    coprocCommand = Simple <$> assignCommand
                <|> Shell <$> shellCommand
                <|> namedCommand

    namedCommand = do
        w <- unreservedWord
        Shell <$> shellCommand <|> Simple <$> simpleCommand w

-------------------------------------------------------------------------------
-- Function definitions
-------------------------------------------------------------------------------

-- | Parse a pair of function parentheses @()@.
functionParens :: Parser ()
functionParens = () <$ operator "(" <* operator ")"

-- | Parse a function body.
functionBody :: Parser Function
functionBody = Function <$ newlineList <*> shellCommand <* redirList

-- | Parse a function definition beginning with the keyword @function@.
functionDef1 :: Parser Command
functionDef1 = FunctionDef
           <$  word "function"
           <*> anyWord
           <*  optional functionParens
           <*> functionBody

-- | Parse a function definition beginning with the given name.
functionDef2 :: String -> Parser Command
functionDef2 w = FunctionDef w <$ functionParens <*> functionBody

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

-- | Parse a single command.
command :: Parser Command
command = baseCommand <* redirList
  where
    baseCommand = Simple <$> assignCommand
              <|> Coproc <$  coproc
              <|> Shell  <$> shellCommand <* redirList
              <|> functionDef1
              <|> namedCommand

    namedCommand = do
        w <- unreservedWord
        functionDef2 w <|> Simple <$> simpleCommand w

-- | Parse an entire script (e.g. a file) as a list of commands.
script :: Parser Script
script = Script <$> compoundList <* eof
