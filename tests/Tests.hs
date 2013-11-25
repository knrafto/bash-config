module Main
    ( main
    ) where

import Control.Monad
import Data.List
import Test.HUnit
import Test.Tasty
import Test.Tasty.HUnit

import Bash.Config.Command
import Bash.Config.Env
import Bash.Config.Lexer

token :: TokenMode -> String -> Token -> Assertion
token m s expected = case nextToken (makeTokens m "" s) of
    Nothing     -> assertFailure "testAssign failed"
    Just (t, _) -> untag t @?= expected

tokens :: TokenMode -> String -> [Token] -> Assertion
tokens m s expected = map untag ts @?= expected
  where
    ts = unfoldr nextToken (makeTokens m "" s)

tests :: TestTree
tests = testGroup "tester"
    [ testGroup "normal mode" $
        let testTokens = tokens NormalMode
            testWord w = testTokens w [TWord w]
        in
        [ testCase "whitespace" $
            testTokens "  # aa \n bb cc \\\n dd # ee\n ff# "
                [ TOperator "\n"
                , TWord "bb"
                , TWord "cc"
                , TWord "dd"
                , TOperator "\n"
                , TWord "ff#"
                ]
        , testCase "word" $ do
            testWord "["
            testWord "!@#$%^*~?/+=0[]"
        , testCase "operator" $ do
            forM_ normalOps $ \op -> testTokens op [TOperator op]
            testTokens "()" [TOperator "(", TOperator ")"]
        , testCase "redirection number" $ do
            testTokens "1>2"  [TNumber 1, TOperator ">", TWord "2"]
            testTokens "15<<" [TNumber 15, TOperator "<<"]
            testTokens "1 >"  [TWord "1", TOperator ">"]
        , testCase "escaping" $ do
            testTokens "a\\\nb" [TWord "ab"]
            testTokens "\\$( )" [TWord "\\$", TOperator "(", TOperator ")"]
            testWord "a\\\"b"
            testWord "a\\ b"
        , testCase "single quoting" $ do
            testWord "'abc'"
            testWord "'${ <`\\'}"
            testWord "a''b"
        , testCase "double quoting" $ do
            testWord "\"abc\""
            testWord "\" \\\"<('\""
            testWord "\" `\"` ${\" \"} \""
            testWord "a\"\"b"
        , testCase "ANSI C quoting" $ do
            testWord "$'abc'"
            testWord "$'${ <\\'('"
            testWord "a$''b"
        , testCase "locale quoting" $ do
            testWord "$\"abc\""
            testWord "$\" \\\"<('\""
            testWord "a$\"\"b"
        , testCase "backquoting" $ do
            testWord "`abc`"
            testWord "` \\`'<(\"{`"
            testWord "` ${` `} $(` `) `"
            testWord "a``b"
        , testCase "parameter expansion" $ do
            testWord "${abc}"
            testWord "\"${abc}\""
            testWord "${\\} \"}\" '}' `}` $(}) }"
            testWord "a${}b"
        , testCase "command substitution" $ do
            testWord "$(abc)"
            testWord "\"$(abc)\""
            testWord "$( \\) \")\" ')' (\" ) \") # ) \n ${)} `}` )"
            testWord "a$()b"
        , testCase "arithmetic expansion" $ do
            testWord "$(())"
            testWord "$((a + b))"
            testWord "$(((z = (a + b) * (c + (d + e) * (f + g)))))"
            testWord "$((c = 8#123+++++0xbe))"
            testWord "$(( ( # ) \"'`${ \\))"
            testWord "a$(())b"
        , testCase "process substitution" $ do
            testWord "<(abc)"
            testWord "<( \\) \")\" ')' (\" ) \") # ) \n ${)} `}` )"
            testWord "a<(b)c"
        ]
    , testCase "arithmetic mode" $ do
        let testArith w = token ArithMode w (TArith w)
        testArith ""
        testArith "a + b"
        testArith "(z = (a + b) * (c + (d + e) * (f + g)))"
        testArith "c = 8#123+++++0xbe"
    , testCase "assignment mode" $ do
        let testAssign s = token AssignMode s . TAssign
        testAssign "a=b" $
            Assign "a" Equals (Value "b")
        testAssign "a+=b" $
            Assign "a" PlusEquals (Value "b")
        testAssign "a=" $
            Assign "a" Equals (Value "")
        testAssign "_0z_='a'" $
            Assign "_0z_" Equals (Value "'a'")
        testAssign "a[`b`]=c" $
            Assign "a[`b`]" Equals (Value "c")
        testAssign "a=()" $
            Assign "a" Equals (Array [])
        testAssign "a=(b c d)" $
            Assign "a" Equals (Array ["b","c","d"])
        testAssign "a=(b \n c # ) \n d)" $
            Assign "a" Equals (Array ["b","c","d"])
        testAssign "a=('b' `c` $(d))" $
            Assign "a" Equals (Array ["'b'","`c`","$(d)"])
    ]

main :: IO ()
main = defaultMain tests
