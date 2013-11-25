-- | Parse an interpret Bash configuration files.
module Bash.Config
    ( -- * Types
      Env(..)
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
import Bash.Config.Parse
import Bash.Config.Types
