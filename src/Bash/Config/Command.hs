-- | Bash shell script types. This does not fully represent parts that
-- aren't interpreted, such as redirections or arithmetic expressions.
module Bash.Config.Command
    ( Script(..)
    , Command(..)
      -- ** Lists
    , List(..)
    , AndOr(..)
    , Pipeline(..)
      -- ** Simple commands
    , SimpleCommand(..)
    , Assign(..)
    , AssignOp(..)
      -- ** Functions
    , Function(..)
      -- ** Shell commands
    , ShellCommand(..)
    , CaseClause(..)
    , CaseTerm(..)
    ) where

import Bash.Config.Env (Value)

-- | A Bash script.
newtype Script = Script List
    deriving (Eq, Show)

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
