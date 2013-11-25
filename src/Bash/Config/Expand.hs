-- | Shell expansions.
module Bash.Config.Expand
    ( unquote
    , expandWords
    , expandValue
    ) where

import Bash.Config.Types

unquote :: String -> String
unquote = undefined

expandWords :: [String] -> Bash [String]
expandWords = undefined

expandValue :: Value -> Bash Value
expandValue = undefined
