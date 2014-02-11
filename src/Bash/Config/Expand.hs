-- | Shell expansions.
module Bash.Config.Expand
    ( expandWordList
    , expandWord
    , expandRValue
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Maybe
import qualified Data.IntMap               as IntMap
import           Data.List
import           Language.Bash.Expand
import           Language.Bash.Syntax
import           Language.Bash.Word

import           Bash.Config.Env

-- | Read an index.
readIx :: MonadPlus m => String -> m Int
readIx s = case reads s of
    [(n,"")] | n >= 0 -> return n
    _                 -> mzero

-- | Expand a list of words.
expandWordList :: [Word] -> MaybeT Bash [String]
expandWordList = undefined

-- | Expand a word.
expandWord :: Word -> MaybeT Bash String
expandWord = undefined

-- | Expand an array.
expandArray :: [(Maybe Word, Word)] -> MaybeT Bash Array
expandArray ws = IntMap.fromList . assemble . concat <$> mapM expandElem ws
  where
    expandElem (Nothing, w) = do
        s <- expandWord w
        return [(Nothing, s)]

    expandElem (Just sub, w) = do
        i  <- readIx =<< expandWord sub
        ss <- expandWordList [w]
        return $ map (\s -> (Just i, s)) ss

    assemble = snd . mapAccumL go 0
      where
        go i (Nothing, s) = (i + 1, (i, s))
        go _ (Just i, s)  = (i + 1, (i, s))

-- | Expand an rvalue.
expandRValue :: RValue -> MaybeT Bash Value
expandRValue (RValue v) = Value <$> expandWord v
expandRValue (RArray a) = Array <$> expandArray a
