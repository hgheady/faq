{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( faqFact
  , faqQuery
  , iPred
  , addFact
  , parse
  , putT
  , splitInput
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

parse :: Text -> Maybe Input
parse t = case T.splitAt 5 t of
  ("INPUT", t') -> faqFact t'
  ("QUERY", t') -> faqQuery t'
  (_      , _) -> Nothing

putT :: Text -> IO ()
putT = (putStrLn . T.unpack)

data Input = Fact  { fPred  :: Text, fTerms :: [Text] }
           | Query { qPred  :: Text
                   , qEls   :: [QueryElement]
                   } deriving (Show)

data QueryElement = Literal Text | Variable Text deriving (Show, Eq)

faqFact = faqFact' . splitInput
faqQuery = faqQuery' . splitInput

faqFact' :: [Text] -> Maybe Input
faqFact' t = case t of
  [] -> Nothing
  (_:[]) -> Nothing
  (predicate : terms) -> Just Fact { fPred = predicate
                                   , fTerms = terms
                                   }

faqQuery' :: [Text] -> Maybe Input
faqQuery' t = case t of
  (predicate : texts) -> case makeEls texts of
    Just es -> Just Query { qPred  = predicate
                          , qEls   = es
                          }
    Nothing -> Nothing
  _ -> Nothing
  where
    makeEls :: [Text] -> Maybe [QueryElement]
    makeEls ts = let qs = catMaybes $ map makeEl ts
                 in case length ts == length qs of
                      True  -> Just qs
                      False -> Nothing

    makeEl :: Text -> Maybe QueryElement
    makeEl "" = Nothing
    makeEl t' | isUpper (T.head t') && 1 == (T.length t') = Just $ Variable t'
    makeEl t' = Just $ Literal t'


-- TODO: should fold
splitInput :: Text -> [Text]
splitInput t = case T.split (`elem` ("()"::[Char])) $ T.filter (/= ' ') t of
  [predicate, terms, _] -> predicate : T.splitOn "," terms
  _ -> []

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
addFact m _  = m

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
