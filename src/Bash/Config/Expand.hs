{-# LANGUAGE LambdaCase #-}
-- | Shell expansions.
module Bash.Config.Expand
    ( unquote
    , expandWord
    , expandWordList
    , expandValue
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Text.Parsec         hiding ((<|>))

import Bash.Config.Types
import Bash.Config.Word

-- | Split a list on a predicate.
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p = go
  where
    go xs = case dropWhile p xs of
        []  -> []
        xs' -> let (w, xs'') = break p xs' in  w : go xs''

-- | Map a monadic action over a list and concatenate the results.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f

-- | Get the value of the IFS variable as a string.
ifsValue :: Bash String
ifsValue = valueString <$> value "IFS" <|> pure " \t\n"
  where
    valueString (Value v) = v
    valueString (Array _) = ""

-- | Brace expand a word. Parameters are either of the form @{a,b,c}@, or
-- @{x..y[..incr]}@ where @x@ and @y@ are either integers or single
-- characters, and @incr@ is an integer increment.
braceExpand :: Word -> [Word]
braceExpand = undefined

-- | Fail if a tilde expansion should be performed.
tildeExpand :: Word -> Bash Word
tildeExpand (Char '~':_) = empty
tildeExpand s            = return s

-- | Perform an expansion.
expansion :: String -> Bash String
expansion s = runParserT (brace <* eof) () s s >>= \case
    Left  _ -> empty
    Right r -> return r
  where
    brace = char '!' *> lift empty
        <|> char '#' *> lift empty
        <|> parameter

    parameter = do
        name <- many1 (alphaNum <|> char '_')
        fromValue <$> lift (value name)
      where
        fromValue (Value v)     = v
        fromValue (Array (v:_)) = v
        fromValue _             = ""

-- | Perform parameter expansion, arithmetic expansion, command
-- substitution, and process substitution.
expand :: Word -> Bash Word
expand = concatMapM $ \case
    Double w         -> expand w
    Backquote _      -> empty
    Parameter s      -> fromString <$> expansion s
    BraceParameter w -> fromString <$> expansion (toString w)
    ArithSubst _     -> empty
    CommandSubst _   -> empty
    ProcessSubst _ _ -> empty
    s                -> return [s]

-- | Split a word into multiple words.
splitWord :: Word -> Bash [Word]
splitWord w = do
    ifs <- ifsValue
    let isIFS (Char c) | c `elem` ifs = True
        isIFS _                       = False
    return (splitBy isIFS w)

-- | Fail if a filename expansion should be performed.
filenameExpand :: Word -> Bash Word
filenameExpand s
    | any isGlob s = empty
    | otherwise    = return s
  where
    isGlob (Char c) | c `elem` "*?[" = True
    isGlob _                         = False

-- | Expand a single word.
expandWord :: Word -> Bash String
expandWord = tildeExpand
         >=> expand
         >=> return . unquote

-- | Expand a list of words.
expandWordList :: [Word] -> Bash [String]
expandWordList = return . concatMap braceExpand
             >=> mapM tildeExpand
             >=> mapM expand
             >=> concatMapM splitWord
             >=> mapM filenameExpand
             >=> return . filter (not . null)
             >=> return . map unquote

-- | Expand a 'Value'.
expandValue :: (Value Word) -> Bash (Value String)
expandValue (Value v)  = Value <$> expandWord v
expandValue (Array vs) = Array <$> expandWordList vs
