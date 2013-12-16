{-# LANGUAGE DeriveDataTypeable, LambdaCase #-}
-- | Bash interpreter testing.
--
-- Tests are specified as @tests/*.sh@ files. Each file consists of a
-- series of pairs of scripts, separated by the line beginning with @###@.
-- Each pair of scripts should result in the same environment.
module Main ( main ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char
import           Data.List
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Typeable
import           System.Directory
import           System.FilePath
import           System.Exit
import           System.IO
import           Test.Tasty
import           Test.Tasty.Providers
import           Text.Parsec.Combinator hiding (optional)
import           Text.Parsec.Error
import           Text.Parsec.Pos
import           Text.Parsec.Prim       hiding ((<|>), parseTest, try)

import           Bash.Config            hiding (parse, interpret)
import qualified Bash.Config            as Bash (parse, interpret)

data TestHeading = TestHeading String Line Line

data TestPair = TestPair TestHeading Script Script
    deriving (Typeable)

instance IsTest TestPair where
    run _ p _   = runTest p
    testOptions = return []

newtype FailedTest = FailedTest String
    deriving (Show, Typeable)

instance IsTest FailedTest where
    run _ (FailedTest msg) _ = return (Result False msg)
    testOptions              = return []

instance Exception FailedTest

failTest :: MonadIO m => String -> m a
failTest = liftIO . throwIO . FailedTest

parseTest :: FilePath -> IO [TestPair]
parseTest file = do
    ls <- lines <$> readFile file
    runParserT (many1 pair <* eof) () file ls >>= \case
        Left  e  -> failTest (show e)
        Right ps -> return ps
  where
    satisfy p = tokenPrim id updatePos test
      where
        updatePos pos _ _  = incSourceLine pos 1
        test c | p c       = Just c
               | otherwise = Nothing

    testLine  = satisfy (not . isTestSep)
    testSep   = dropWhile isSpace . drop 3 <$> satisfy isTestSep
    isTestSep = ("###" `isPrefixOf`)

    lineNumber = sourceLine <$> getPosition

    pair = do
        l1 <- lineNumber
        (name, s1) <- script
        (_   , s2) <- script
        l2 <- subtract 1 <$> lineNumber
        return $ TestPair (TestHeading name l1 l2) s1 s2

    script = do
        name <- testSep
        offset <- subtract 1 <$> lineNumber
        ls <- many1 testLine
        case Bash.parse file (unlines ls) of
            Left  e -> failTest . show $ incParseErrorLine offset e
            Right s -> return (name, s)

    incParseErrorLine offset e =
        setErrorPos (incSourceLine (errorPos e) offset) e

testPair :: TestPair -> TestTree
testPair p@(TestPair heading _ _) = singleTest (showHeading heading) p
  where
    showHeading (TestHeading name l1 l2) =
        "(" ++ show l1 ++ "-" ++ show l2 ++ ")" ++
        if null name then "" else (" " ++ name)

testFile :: FilePath -> IO TestTree
testFile file = do
    eps <- try (parseTest file)
    return $ case eps of
        Left e   -> singleTest file (e :: FailedTest)
        Right ps -> testGroup file (map testPair ps)

runTest :: TestPair -> IO Result
runTest (TestPair _ s1 s2) = do
    [e1, e2] <- mapM execute [s1, s2]
    let msg = diff (parameters e1) (parameters e2)
    return $ Result (null msg) msg
  where
    execute s = case Bash.interpret s emptyEnv of
        Left  e -> fail $ "execution failed: " ++ e
        Right e -> return e

diff :: Map String (Value String) -> Map String (Value String) -> String
diff a b = concatMap report $ Map.keys (a `Map.union` b)
  where
    report k
        | v1 == v2  = ""
        | otherwise = "expected: " ++ showElem v2 ++ "\n" ++
                      " but got: " ++ showElem v1 ++ "\n"
      where
        v1 = Map.lookup k a
        v2 = Map.lookup k b

        showElem Nothing  = "unknown " ++ k
        showElem (Just v) = k ++ "=" ++ showValue v

        showValue v = case fmap show v of
            Value s  -> s
            Array ss -> "(" ++ intercalate " " ss ++ ")"

prepareTests :: IO TestTree
prepareTests = do
    dir <- getCurrentDirectory
    cases <- filter (\f -> takeExtension f == ".sh") <$>
             getDirectoryContents dir
    when (null cases) $ do
        hPutStrLn stderr "error: no test cases found"
        exitFailure
    tests <- mapM testFile cases
    return $ testGroup "tests" tests

main :: IO ()
main = do
    setCurrentDirectory "tests"
    tests <- prepareTests
    defaultMain tests
