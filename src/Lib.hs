{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( feFact
  , feQuery
  , iPred
  , addFact
  , Input (..)
  , QueryElement (..)
  , Node (..)
  ) where

import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T


data Input = Fact  { fPred  :: Text, fTerms :: [Text] }
           | Query { qPred  :: Text
                   , qEls   :: [QueryElement]
                   } deriving (Show)

data QueryElement = Literal Text | Variable Text deriving (Show, Eq)

feFact :: Text -> Maybe Input
feFact t = case T.splitOn " " t of
  (pred : terms) -> Just Fact { fPred = pred
                              , fTerms = terms
                              }
  _              -> Nothing

feQuery :: Text -> Maybe Input
feQuery t = case T.splitOn " " t of
  (pred : texts) -> case makeEls texts of
    Just es -> Just Query { qPred  = pred
                          , qEls   = es
                          }
    Nothing -> Nothing
  where
    makeEls :: [Text] -> Maybe [QueryElement]
    makeEls ts = let qs = catMaybes $ map makeEl ts
                 in case length ts == length qs of
                      True  -> Just qs
                      False -> Nothing

    makeEl :: Text -> Maybe QueryElement
    makeEl "" = Nothing
    makeEl t  = if isUpper (T.head t)
                then Just $ Variable t
                else Just $ Literal t

iPred :: Input -> Text
iPred (Fact  p _) = p
iPred (Query p _) = p


data Node = Node
  { nTerm     :: Text
  , nNext     :: Maybe Node
  , nChild    :: Maybe Node
  } deriving (Show)

addFact :: (Map Text Node) -> Input -> (Map Text Node)
addFact m (Fact p ts) = case addNode' (Map.lookup p m) ts of
  Just n  -> Map.insert p n m
  Nothing -> m
addFact m _           = m

addNode' :: Maybe Node -> [Text] -> Maybe Node
addNode' mn ts@(th:tt) =
  case mn of
  Nothing -> Just Node { nTerm  = th
                       , nNext  = Nothing
                       , nChild = addNode' Nothing tt
                       }
  Just n  ->
    let chld = nChild n
        next = nNext  n
    in case (nTerm n == th) of
      True  -> Just n { nChild = addNode' chld tt }
      False -> Just n { nNext  = addNode' next ts }
addNode' _ [] = Nothing
