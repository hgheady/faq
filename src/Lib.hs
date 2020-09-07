{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( fePrint
  , feFact
  , feQuery
  , fePrintFact
  , fePrintQuery
  , fePrintTrie
  , fePrintChildren
  , fePrintNexts
  , Fact (..)
  , Query (..)
  , Node (..)
  ) where

import Control.Monad
import Data.Char
import Data.Maybe
import Data.Text (Text, pack, unpack, splitOn)
import qualified Data.Text as T


fePrint :: Text -> IO ()
fePrint msg = putStrLn $ "---\n" <> unpack msg

fePrintFact :: (Text, [Text]) -> IO [()]
fePrintFact (p,ts) = mapM (putStr . show) $ p : ts

fePrintQuery :: Query -> IO [()]
fePrintQuery q = do
  mapM (putStrLn . show) $ (qPred q) : []
  mapM (putStrLn . show) $ qEls q

fePrintChildren = fePrintList nChild "; child: "

fePrintNexts = fePrintList nNext "; next: "

fePrintList :: (Node -> Maybe Node) -> Text -> Node -> Text
fePrintList f t n =  T.intercalate t (map nTerm $ listNodes n)
  where
    listNodes :: Node -> [Node]
    listNodes n = n : case f n of
      Just c  -> listNodes c
      Nothing -> []

fePrintTrie :: Node -> IO [()]
fePrintTrie n = mapM (putStrLn . show) $ children n
  where
    children :: Node -> [Node]
    children n = n : case nChild n of
      Just c  -> children c
      Nothing -> []

-- fePrintTrie :: Node -> Text
-- fePrintTrie n = T.intercalate "<PT>" $ map nc $ children n
--   where
--     nc :: Node -> Text
--     nc n = T.intercalate "<nc>" [fePrintNexts n, fePrintChildren n]
--     children :: Node -> [Node]
--     children n = n : case nChild n of
--       Just c  -> children c
--       Nothing -> []

-- data Fact = Fact
--   { fPredicate :: Text
--   , fTerms     :: [Text]
--   }

-- feFact :: Text -> Maybe Fact
-- feFact t = case T.splitOn " " t of
--   (pred : terms) -> Just $ Fact pred terms
--   _      -> Nothing

feFact :: Text -> Maybe (Text, [Text])
feFact t = case T.splitOn " " t of
  (pred : terms) -> Just (pred, terms)
  _              -> Nothing

data Node = Node
  { nTerm  :: Text
  , nNext  :: Maybe Node
  , nChild :: Maybe Node
  } deriving (Show)

feQuery :: Text -> Maybe Query
feQuery t = case T.splitOn " " t of
  (pred : texts) -> case makeEls texts of
    Just es -> Just Query { qPred = pred
                          , qEls  = es
                          }
    Nothing -> Nothing
  where
    makeEls :: [Text] -> Maybe [QueryElement]
    makeEls ts = let qs = catMaybes $ map makeEl ts
                 in case length ts == length qs of
                      True  -> Just qs
                      False -> Nothing

    makeEl :: Text -> Maybe QueryElement
    makeEl t = case T.length t of
                 1 -> if isUpper (T.head t)
                      then Just $ Variable t
                      else Just $ Literal t
                 _ -> Just $ Literal t

      -- Just (t, ($ case (length t) of
      --                       1 -> if isUpper t then Variable else Literal
      --                       _ -> Literal ))

data Fact = Fact
  { fPred  :: Text
  , fTerms :: [Text]
  } deriving (Show)

data Query = Query
  { qPred :: Text
  , qEls  :: [QueryElement]
  } deriving (Show)

data QueryElement = Literal Text | Variable Text deriving (Show)
