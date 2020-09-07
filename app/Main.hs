{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Data.List
import Data.Maybe
import Data.Text (Text, splitOn, pack)
import qualified Data.Text as T
import Lib

main :: IO ()
main = do
  ingested <- getContents
  let input = lines ingested
  -- let input' = map parse input
  let facts = map scrub $ filter isFact input
  let queries = map scrub $ filter isQuery input
  -- mapM print $ facts
  -- mapM print $ queries
  let feFacts   = map feFact facts
  let feQueries = map feQuery queries
  -- mapM fePrintFact $ map fromJust $ filter isJust feFacts
  -- mapM fePrintQuery $ map fromJust $ filter isJust feQueries
  mapM (putStrLn . show) $ map fromJust $ filter isJust feFacts
  mapM (putStrLn . show) $ map fromJust $ filter isJust feQueries
  let factTries = makeTries $ map fromJust $ filter isJust feFacts
  case Map.lookup "are_friends" factTries of
    -- Just n  -> (putStr . show) $ fePrintTrie n
    Just n  -> fePrintTrie n
    Nothing -> return [()]
  fePrint "true"

-- parse :: String -> Maybe Input
-- parse s = case splitAt 6 s of
--   ("INPUT ": s) -> 

-- data Input = Fact | Query

isFact :: String -> Bool
isFact t = "INPUT " `isPrefixOf` t

isQuery :: String -> Bool
isQuery t = "QUERY " `isPrefixOf` t

scrub :: String -> Text
scrub s = pack $ filter syntax $ drop 6 s
  where
    syntax :: Char -> Bool
    syntax c = (not . (`elem` ("()," :: [Char]))) c

makeTries :: [(Text, [Text])] -> (Map Text Node)
makeTries facts = foldl addNode Map.empty facts
  where
    addNode :: (Map Text Node) -> (Text, [Text]) -> (Map Text Node)
    addNode m (p, ts) = case addNode' (Map.lookup p m) ts of
      Just n  -> Map.insert p n m
      Nothing -> m

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
