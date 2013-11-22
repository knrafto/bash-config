-- | Bash shell script types. This does not fully represent parts that
-- aren't interpreted, such as redirections or arithmetic expressions.
module Bash.Config.Types
    ( -- * Scripts
      -- ** Commands
      Command(..)
      -- ** Lists
    , List(..)
    , AndOr(..)
    , Pipeline(..)
      -- ** Simple commands
    , SimpleCommand(..)
    , Assign(..)
    , AssignOp(..)
    , Value(..)
      -- ** Functions
    , Function(..)
      -- ** Shell commands
    , ShellCommand(..)
    , CaseClause(..)
    , CaseTerm(..)
      -- * Environments
    , Env(..)
    , emptyEnv
    , unquote
    ) where

import Data.Map (Map)
import Data.Monoid

-- | A Bash command.
data Command
    = Simple SimpleCommand
    | Shell ShellCommand
    | FunctionDef Function
    | Coproc
    deriving (Eq, Show)

-- | A compound list of statements, terminated by @&@ or @;@.
newtype List = List [AndOr]
    deriving (Eq, Show)

-- | A list of pipelines separated by @&&@ and @||@.
data AndOr
    = Last Pipeline
    | And Pipeline AndOr
    | Or Pipeline AndOr
    deriving (Eq, Show)

-- | A (possibly inverted) pipeline, linked with @|@ or @|&@.
data Pipeline = Pipeline Bool [Command]
    deriving (Eq, Show)

-- | A simple command.
data SimpleCommand = SimpleCommand [Assign] [String]
    deriving (Eq, Show)

-- | An assignment word.
data Assign = Assign String AssignOp Value
    deriving (Eq, Show)

-- | An assignment operator (@=@ or @+=@).
data AssignOp = Equals | PlusEquals
    deriving (Eq, Show)

-- | The assigned value.
data Value = Value String | Array [String]
    deriving (Eq, Show)

-- | A function definition.
data Function = Function String ShellCommand
    deriving (Eq, Show)

-- | A compound command.
data ShellCommand
    = Subshell List
    | Group List
    | Arith String
    | Cond [String]
    | For String [String] List
    | ArithFor String List
    | Select String [String] List
    | Case String [CaseClause]
    | If List List List
    | Until List List
    | While List List
    deriving (Eq, Show)

-- | A single case clause.
data CaseClause = CaseClause [String] List CaseTerm
    deriving (Eq, Show)

-- | A case clause terminator. A clause can either 'Break' out of the case
-- statement with @;;@, 'FallThrough' to the next clause with @;&@, or
-- 'Continue' by testing the pattern for the next clause with @;;&@.
data CaseTerm
    = Break
    | FallThrough
    | Continue
    deriving (Eq, Show)

data Env = Env
    { envParameters :: Map String Value
    , envFunctions  :: Map String ShellCommand
    }

-- | The empty environment.
emptyEnv :: Env
emptyEnv = Env mempty mempty

-- | Remove all quoting from a word.
unquote :: String -> String
unquote = undefined  -- TODO
