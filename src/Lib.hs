{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( fePrint
  , feFact
  , feQuery
  , iPred
  , addNode
  , Input (..)
  , QueryElement (..)
  , Node (..)
  ) where

import Control.Monad
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text, pack, unpack, splitOn)
import qualified Data.Text as T


fePrint :: Text -> IO ()
fePrint msg = putStrLn $ "---\n" <> unpack msg

feFact :: Text -> Maybe Input
feFact t = case T.splitOn " " t of
  (pred : terms) -> Just Fact { fPred = pred
                              , fTerms = terms
                              }
  _              -> Nothing

feQuery :: Text -> Maybe Input
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

iPred :: Input -> Text
iPred (Fact  p _) = p
iPred (Query p _) = p

data Input = Fact  { fPred  :: Text, fTerms :: [Text] }
           | Query { qPred :: Text,  qEls  :: [QueryElement] } deriving (Show)

data QueryElement = Literal Text | Variable Text deriving (Show)

data Node = Node
  { nTerm  :: Text
  , nNext  :: Maybe Node
  , nChild :: Maybe Node
  } deriving (Show)

addNode :: (Map Text Node) -> Input -> (Map Text Node)
addNode m (Fact p ts) = case addNode' (Map.lookup p m) ts of
  Just n  -> Map.insert p n m
  Nothing -> m
addNode m _           = m

addNode' :: Maybe Node -> [Text] -> Maybe Node
addNode' mn (t:ts) =
  case mn of
  Nothing -> Just Node { nTerm  = t
                       , nNext  = Nothing
                       , nChild = makeChildren ts
                       }
  Just n  ->
    let child = nChild n
        next  = nNext  n
    in case (nTerm n == t) of
      True  -> Just n { nChild = addNode' child ts }
      False -> Just n { nNext  = addNode' next  [t]
                      , nChild = addNode' child ts
                      }
addNode' _ [] = Nothing

makeChildren :: [Text] -> Maybe Node
makeChildren (t:ts) = Just Node { nTerm  = t
                                , nNext  = Nothing
                                , nChild = makeChildren ts
                                }
makeChildren [] = Nothing
