-- | Shell expansions.
module Bash.Config.Expand
    ( unquote
    , expandWord
    , expandWordList
    , expandValue
    ) where

import           Control.Applicative
import           Text.Parsec.Char
import           Text.Parsec.Prim    (parse)

import qualified Bash.Config.Builder as B
import           Bash.Config.Types

-- | Unquote a string.
unquote :: String -> String
unquote s = case parse naked "" s of
    Left  e -> error (show e)
    Right b -> B.toString b
  where
    naked      = B.many $ single
                      <|> double
                      <|> escape
                      <|> B.anyChar

    escape     = char '\\' *> B.anyChar
    single     = char '\'' *> B.takeWhile (/= '\'') <* char '\''
    double     = char '\"' *> B.many doubleChar <* char '\"'
    doubleChar = escape <|> B.satisfy (/= '\"')

expandWord :: String -> Bash String
expandWord = return  -- TODO

expandWordList :: [String] -> Bash [String]
expandWordList = return  -- TODO

expandValue :: Value -> Bash Value
expandValue = return  -- TODO
