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
  let input' =  map fromJust $ filter isJust $ map (parse . scrub') input
  -- mapM (putStrLn . show) input'

  -- let facts = map scrub $ filter isFact input
  -- let queries = map scrub $ filter isQuery input
  -- let feFacts   = map feFact' facts
  -- let feQueries = map feQuery queries
  -- mapM (putStrLn . show) $ map fromJust $ filter isJust feFacts
  -- mapM (putStrLn . show) $ map fromJust $ filter isJust feQueries
  -- let factTries = makeTries $ map fromJust $ filter isJust feFacts

  factEngine Map.empty input'
  
  -- case Map.lookup "are_friends" factTries of
  --   Just n  -> fePrintTrie n
  --   Nothing -> return [()]
  fePrint "Fin"

factEngine :: (Map Text Node) -> [Input] -> IO [()]
factEngine m (i:is) = do
    m' <- engine m (Map.lookup (iPred i) m) i
    factEngine m' is
  where
    engine :: (Map Text Node) -> Maybe Node -> Input -> IO (Map Text Node)
    engine m _ f@(Fact  _ _) = addFact  m f
    engine m mn q@(Query _ _) = runQuery m mn q
factEngine _ [] = do return [()]

-- factEngine :: (Map Text Node) -> [Input] -> IO [()]
-- factEngine m (i:is) = case Map.lookup (iPred i) m of
--   Nothing -> case i of
--     q@(Query _ _) -> engine m Nothing q >> factEngine m is
--     f@(Fact  _ _) -> factEngine m is
--   mn@(Just n)  -> do
--     m' <- engine m mn i
--     factEngine m' is
--   where
--     engine :: (Map Text Node) -> Maybe Node -> Input -> IO (Map Text Node)
--     engine m _ f@(Fact  _ _) = addFact  m f
--     engine m mn q@(Query _ _) = runQuery m mn q
-- factEngine _ [] = do return [()]

addFact :: (Map Text Node) -> Input -> IO (Map Text Node)
addFact m f = do
  return $ addNode' m f

runQuery :: (Map Text Node) -> Maybe Node -> Input -> IO (Map Text Node)
runQuery m mn (Query p els) = do
  putStrLn "---"
  case els of
    []     -> return ()
    (e:[]) -> case e of
      (Literal  l) -> singleL mn l
      (Variable v) -> case mn of
        n@(Just _) -> singleV n v
        Nothing    -> putStrLn "none"
    (e:es) -> return ()
  return m
  where
    singleL :: Maybe Node -> Text -> IO ()
    singleL mn l = case mn of
      Just n  -> case (nTerm n == l) of
        True  -> putStrLn "true"
        False -> singleL (nNext n) l
      Nothing -> putStrLn "false"

    singleV :: Maybe Node -> Text -> IO ()
    singleV (Just n) v = do
      putT (v <> ": " <> nTerm n)
      singleV (nNext n) v
    singleV Nothing _  = do return ()

putT :: Text -> IO ()
putT = (putStrLn . T.unpack)

parse :: Text -> Maybe Input
parse t = case T.splitAt 6 t of
  ("INPUT ", t) -> feFact  t
  ("QUERY ", t) -> feQuery t
  (_       ,"") -> Nothing

scrub' :: String -> Text
scrub' s = pack $ filter syntax s
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

isFact :: String -> Bool
isFact t = "INPUT " `isPrefixOf` t

isQuery :: String -> Bool
isQuery t = "QUERY " `isPrefixOf` t

scrub :: String -> Text
scrub s = pack $ filter syntax $ drop 6 s
  where
    syntax :: Char -> Bool
    syntax c = (not . (`elem` ("()," :: [Char]))) c
