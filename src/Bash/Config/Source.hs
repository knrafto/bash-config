-- | A source that keeps track of its current position.
module Bash.Config.Source
    ( -- * Source type
      Source
      -- * Construction
    , source
      -- * Operations
    , sourcePos
    , peekChar
    , takeChar
    ) where

import Data.Maybe
import Text.Parsec.Pos

-- | A string source.
data Source = Source String !SourcePos

-- | Construct a new named 'Source' from a 'String'.
source :: SourceName -> String -> Source
source name s = Source s (initialPos name)

-- | Get the current source position.
sourcePos :: Source -> SourcePos
sourcePos (Source _ p) = p

-- | Return the next character, if there is one.
peekChar :: Source -> Maybe Char
peekChar (Source s _) = listToMaybe s

-- | Return the next character and an updated source, of 'Nothing' if the
-- end of the source was reached.
takeChar :: Source -> Maybe (Char, Source)
takeChar (Source s p) = case s of
    c:s' -> Just (c, Source s' (updatePosChar p c))
    _    -> Nothing
