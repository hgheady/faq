{-# LANGUAGE OverloadedStrings   #-}

import Run
import Lib

import Test.HUnit (assertEqual, assertFailure)
import Test.Framework (defaultMain, Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Text (Text)
import Control.Monad

testGroups :: [Test]
testGroups = [ testRead
             , testAddFact
             , testRun
             ]

testRead :: Test
testRead = testGroup "Read"
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

testAddFact :: Test
testAddFact = testGroup "Add facts"
    [
      testCase "Add singleton" $ do
        let r = addFact Map.empty (Fact "is_villain" ["Penguin"])
        case Map.toList r of
          [("is_villain", (Node "Penguin" Nothing Nothing))] -> return ()
          _  -> assertFailure $ "Add unexpected output: " ++ show r
    , testCase "Add singletons" $ do
        let r = foldl addFact Map.empty
                [ (Fact "is_villain" ["Penguin"])
                , (Fact "is_villain" ["Dollmaker"])
                ]
        case Map.toList r of
          [ ("is_villain", (Node "Penguin" (Just (Node "Dollmaker" Nothing Nothing)) Nothing))] -> return ()
          _  -> assertFailure $ "Add unexpected output: " ++ show r
    , testCase "Add pairs" $ do
        let r = foldl addFact Map.empty
                [ (Fact "work_together" ["Gordon", "Bullock"])
                , (Fact "work_together" ["Gordon", "Batman"])
                ]
        case Map.toList r of
          [ ("work_together", (Node "Gordon" Nothing
                                (Just (Node "Bullock"
                                        (Just (Node "Batman" Nothing Nothing)) Nothing))))] -> return ()
          _  -> assertFailure $ "Add unexpected output: " ++ show r

    ]

testRun :: Test
testRun = testGroup "Run"
    [
      testCase "Running on empty is vacuous" $ do
        let r = run [] [] Map.empty Nothing
        case r of
          ["true"] -> return ()
          _  -> assertFailure $ "Run unexpected output: " ++ show r
    , testCase "Running an assertion works" $ do
        let r = run [Literal "Penguin"] []  Map.empty (Just testNode)
        case r of
          ["true"] -> return ()
          _  -> assertFailure $ "Run unexpected output: " ++ show r
    ]

testTrie :: Map Text Node
testTrie = Map.fromList [("is_villain", testNode)]

testNode :: Node
testNode = Node "Penguin" Nothing Nothing

main :: IO ()
main = defaultMain testGroups
