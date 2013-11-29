-- | Shell expansions.
module Bash.Config.Expand
    ( unquote
    , expandWord
    , expandWordList
    , expandValue
    ) where

import           Control.Applicative
import           Data.Monoid
import           Text.Parsec.Prim    (parse)

import           Bash.Config.Builder
import           Bash.Config.Types

-- | Unquote a string.
unquote :: String -> String
unquote s = case parse bare "" s of
    Left  e -> error (show e)
    Right b -> toString b
  where
    bare = char '\'' *> single
       <|> char '\"' *> double
       <|> char '\\' *> escape
       <|> anyChar <+> bare
       <|> pure mempty

    single = mconcat <$> many singleChar <* char '\'' <+> bare
    double = mconcat <$> many doubleChar <* char '\"' <+> bare

    singleChar = satisfy (/= '\'')
    doubleChar = char '\\' *> anyChar <|> satisfy (/= '\"')

    escape = anyChar <+> bare <|> pure mempty

expandWord :: String -> Bash String
expandWord = return  -- TODO

expandWordList :: [String] -> Bash [String]
expandWordList = return  -- TODO

expandValue :: Value -> Bash Value
expandValue = return  -- TODO
