-- | Shell expansions.
module Bash.Config.Expand
    ( unquote
    , expandWords
    , expandValue
    ) where

import Bash.Config.Env

unquote :: String -> String
unquote = id  -- TODO

expandWords :: [String] -> Bash [String]
expandWords = return  -- TODO

expandValue :: Value -> Bash Value
expandValue = return  -- TODO
