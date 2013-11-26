{-# LANGUAGE DeriveDataTypeable #-}
-- | Bash interpreter testing.
--
-- Tests are specified as @tests/*.sh@ files. Each file consists of two Bash
-- scripts, separated by @\"###\"@ on a line by itself. Both scripts
-- are run and their resulting parameters are compared.
module Main ( main ) where

import Control.Exception
import Control.Monad
import Data.Functor
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable
import System.Directory
import System.FilePath
import System.Exit
import System.IO
import Test.Tasty
import Test.Tasty.Providers

import Bash.Config

newtype TestFile = TestFile FilePath deriving (Typeable)

instance IsTest TestFile where
    run _ f _   = runTest f
    testOptions = return []

newtype TestFailed = TestFailed String deriving (Show, Typeable)

instance Exception TestFailed

testFile :: FilePath -> TestTree
testFile file = singleTest (takeBaseName file) (TestFile file)

runTest :: TestFile -> IO Result
runTest (TestFile file) = catchResult $ do
    (lines1, lines2) <- break (== "###") . lines <$> readFile file
    let script1 = unlines lines1
        script2 = unlines $ replicate (length lines1) "" ++ lines2
    env1 <- runScript file script1
    env2 <- runScript file script2
    let message = diff (parameters env1) (parameters env2)
    unless (null message) $ throwIO (TestFailed message)
  where
    catchResult m = toResult <$> try m

    toResult (Left (TestFailed msg)) = Result False msg
    toResult _                       = Result True ""

runScript :: FilePath -> String -> IO Env
runScript name s = do
    script <- case parse name s of
        Left  e -> throwIO (TestFailed $ show e)
        Right r -> return r
    case interpret script emptyEnv of
        Nothing  -> throwIO (TestFailed "execution failed")
        Just env -> return env

diff :: Map String Value -> Map String Value -> String
diff a b = concatMap report $ Map.keys (a `Map.union` b)
  where
    report k
        | v1 == v2  = ""
        | otherwise = "expected: " ++ showValue v2 ++ "\n" ++
                      " but got: " ++ showValue v1 ++ "\n"
      where
        v1 = Map.lookup k a
        v2 = Map.lookup k b

        showValue Nothing           = "unknown " ++ k
        showValue (Just (Value v )) = k ++ "=" ++ show v
        showValue (Just (Array vs)) = k ++ "=(" ++ intercalate " " vs ++ ")"

prepareTests :: IO TestTree
prepareTests = do
    setCurrentDirectory "tests"
    cases <- filter (\f -> takeExtension f == ".sh") <$>
             getDirectoryContents "."
    when (null cases) $ do
        hPutStrLn stderr "error: no test cases found"
        exitFailure
    return $ testGroup "tests" (map testFile cases)

main :: IO ()
main = do
    tests <- prepareTests
    defaultMain tests
