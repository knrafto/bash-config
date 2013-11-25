-- | Shell expansions and other value manipulation.
module Bash.Config.Expand
    ( unquote
    , append
    , expandWords
    , expandValue
    ) where

import Bash.Config.Types

unquote :: String -> String
unquote = undefined

append :: Value -> Value -> Value
append = undefined

expandWords :: [String] -> Bash [String]
expandWords = undefined

expandValue :: Value -> Bash Value
expandValue = undefined
