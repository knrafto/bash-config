{-# LANGUAGE DeriveFunctor, MultiParamTypeClasses #-}
-- | Bash shell script types. This does not fully represent parts that
-- aren't interpreted, such as redirections or arithmetic expressions.
module Bash.Config.Types
    ( -- * Execution
      Env(..)
    , emptyEnv
    , ReturnCode(..)
    , Status(..)
    , Bash(..)
      -- * Commands
    , Command(..)
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
      -- * Miscellaneous
    , unquote
    , append
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.Map
import Data.Monoid

------------------------------------------------------------------------------
-- Execution
------------------------------------------------------------------------------

-- | The execution environment.
data Env = Env
    { -- | Environment parameters or variables.
      envParameters :: Map String Value
      -- | Environment functions.
    , envFunctions  :: Map String (Bash ReturnCode)
    }

-- | The empty environment.
emptyEnv :: Env
emptyEnv = Env mempty mempty

-- | A command's return code.
data ReturnCode
    -- | An unknown return code.
    = Unknown
    -- | A nonzero return code.
    | Failure
    -- | A zero return code.
    | Success
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | The execution status. If the interpreter cannot fully simulate Bash,
-- the execution status will be set to 'Dirty' and execution will proceed
-- in a safe manner.
data Status
    -- | Indeterminate execution.
    = Dirty
    -- | Normal execution.
    | Clean
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | The Bash execution monad.
newtype Bash a = Bash { runBash :: Status -> Env -> Maybe (a, Env) }
    deriving (Functor)

instance Applicative Bash where
    pure  = return
    (<*>) = ap

instance Alternative Bash where
    empty = fail "empty"
    (<|>) = mplus

instance Monad Bash where
    return a = Bash $ \_ s -> Just (a, s)
    m >>= k  = Bash $ \r s -> do
                   (a, s') <- runBash m r s
                   runBash (k a) r s'
    fail _   = Bash $ \_ _ -> Nothing

instance MonadPlus Bash where
    mzero     = fail "mzero"
    mplus a b = Bash $ \r s -> runBash a r s `mplus` runBash b r s

instance MonadReader Status Bash where
    ask       = Bash $ \r s -> Just (r, s)
    local f m = Bash $ \r s -> runBash m (f r) s
    reader f  = Bash $ \r s -> Just (f r, s)

instance MonadState Env Bash where
    get     = Bash $ \_ s -> Just (s, s)
    put s   = Bash $ \_ _ -> Just ((), s)
    state f = Bash $ \_ s -> Just (f s)

------------------------------------------------------------------------------
-- Commands
------------------------------------------------------------------------------

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

-- | Remove all quoting from a word.
unquote :: String -> String
unquote = undefined  -- TODO

-- | Append two values.
append :: Value -> Value -> Value
append = undefined  -- TODO
