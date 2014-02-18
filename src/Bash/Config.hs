-- | Parse an interpret Bash configuration files.
module Bash.Config
    ( -- * Types
      Env(..)
    , emptyEnv 
    , Value(..)
      -- * Parsing
    , parse
      -- * Interpretation
    , Eval
    , interpret
    ) where

import Language.Bash.Parse

import Bash.Config.Eval
import Bash.Config.Env
