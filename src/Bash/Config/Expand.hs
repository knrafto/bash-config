-- | Shell expansions.
module Bash.Config.Expand
    ( unquote
    , expandWord
    , expandWordList
    , expandValue
    ) where

import Bash.Config.Types

unquote :: String -> String
unquote = id  -- TODO

expandWord :: String -> Bash String
expandWord = return  -- TODO

expandWordList :: [String] -> Bash [String]
expandWordList = return  -- TODO

expandValue :: Value -> Bash Value
expandValue = return  -- TODO
