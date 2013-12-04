-- | Parse an interpret Bash configuration files.
module Bash.Config
    ( -- * Types
      Env(..)
    , emptyEnv 
    , Value(..)
    , Script
    , Function
      -- * Parsing
    , parse
      -- * Interpretation
    , Eval
    , interpret
    ) where

import Bash.Config.Eval
import Bash.Config.Parser
import Bash.Config.Types
