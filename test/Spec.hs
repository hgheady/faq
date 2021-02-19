{-# LANGUAGE OverloadedStrings   #-}

import Lib

import Test.HUnit (assertEqual, assertFailure)
import Test.Framework (defaultMain, Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Data.Maybe (fromJust)
import Data.Monoid
import Control.Monad

testGroups :: [Test]
testGroups = [ testRead
             ]

testRead :: Test
testRead = testGroup "Read inputs"
    [
      testCase "Input is split as expected" $ do
        let r = splitInput "foo(bar,baz,Q)"
        case r of
          ["foo","bar","baz","Q"] -> return ()
          _ -> assertFailure $ show r
    , testCase "Input is split as expected (spaces)" $ do
        let r = splitInput "foo (bar, baz, Q)"
        case r of
          ["foo","bar","baz","Q"] -> return ()
          _ -> assertFailure $ show r
    , testCase "Fact parses" $ do
        let r = parse "INPUT foo (bar)"
        case r of
          Nothing -> assertFailure "INPUT parsed as Nothing"
          Just (Query _ _) -> assertFailure "INPUT parsed as Query"
          Just (Fact _ _) -> return ()
    , testCase "Query parses" $ do
        let r = parse "QUERY foo (bar,baz,Q)"
        case r of
          Nothing -> assertFailure "QUERY failed to parse"
          Just (Fact _ _) -> assertFailure "QUERY parsed as Fact"
          Just (Query "foo" [Literal "bar", Literal "baz", Variable "Q"]) -> return ()
          Just (Query _ _) -> assertFailure $ "QUERY parsed unexpectedly: " ++ show r
    , testCase "Fact parses regardless of spaces" $ do
        let r = parse "INPUTfoo(bar,baz)"
        case r of
          Nothing -> assertFailure "INPUT failed to parse"
          Just (Fact "foo" ["bar","baz"]) -> return ()
          Just _ -> assertFailure $ show r
    , testCase "Query parses regardless of spaces" $ do
        let r = parse "QUERYfoo(bar,baz)"
        case r of
          Nothing -> assertFailure "INPUT failed to parse"
          Just (Query "foo" [Literal "bar", Literal "baz"]) -> return ()
          Just _ -> assertFailure $ show r
    ]

main :: IO ()
main = defaultMain testGroups
