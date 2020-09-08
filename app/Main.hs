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

main :: IO [()]
main = do
  ingested <- getContents
  let input = lines ingested
  let input' =  map fromJust $ filter isJust $ map (parse . scrub) input
  -- mapM (putStrLn . show) input'
  factEngine Map.empty input'

factEngine :: (Map Text Node) -> [Input] -> IO [()]
factEngine m (i:is) = do
    m' <- engine m (Map.lookup (iPred i) m) i
    factEngine m' is
  where
    engine :: (Map Text Node) -> Maybe Node -> Input -> IO (Map Text Node)
    engine m _  f@(Fact  _ _) = addFact  m f
    engine m mn q@(Query _ _) = runQuery m mn q
factEngine _ [] = return [()]

addFact :: (Map Text Node) -> Input -> IO (Map Text Node)
addFact m f = do return $ addNode m f

runQuery :: (Map Text Node) -> Maybe Node -> Input -> IO (Map Text Node)
runQuery m mn q@(Query p els) = do
  putStrLn "---"
  case els of
    []     -> return ()
    (e:[]) -> case e of
      (Literal  l) -> putMT $ singleL mn l
      (Variable v) -> case mn of
        n@(Just _) -> putMT $ singleV n v
        Nothing    -> putStrLn "none"
    (e:es) -> putMT $ compound mn els Nothing
  return m
  where
    compound :: Maybe Node -> [QueryElement] -> Maybe Text -> Maybe Text
    compound mn ex@(e:es) mt = case e of
      (Literal  l) -> case mn of
        Just n  -> case nTerm n == l of
          True  -> compound (nChild n) es mt
          False -> compound (nNext  n) ex mt
        Nothing -> Just "none"
      (Variable v) -> case mn of
        Just n  -> compound (nChild n) es $ case mt of
          Just t  -> Just $ t <> ((v <> ": " <> nTerm n <> "\n"))
          Nothing -> Just ((v <> ": " <> nTerm n <> "\n"))
        Nothing -> mt
    compound Nothing []  mt = mt

    singleL :: Maybe Node -> Text -> Maybe Text
    singleL mn l = case mn of
      Just n  -> case (nTerm n == l) of
        True  -> Just "true"
        False -> singleL (nNext n) l
      Nothing -> Just "false"

    singleV :: Maybe Node -> Text -> Maybe Text
    singleV (Just n) v = Just ((v <> ": " <> nTerm n <> "\n")
      <> (fromMaybe "" $ singleV (nNext n) v))
    singleV Nothing _  = Nothing

putMT :: Maybe Text -> IO ()
putMT (Just t) = putT t
putMT Nothing  = return ()

putT :: Text -> IO ()
putT = (putStrLn . T.unpack)

parse :: Text -> Maybe Input
parse t = case T.splitAt 6 t of
  ("INPUT ", t) -> feFact  t
  ("QUERY ", t) -> feQuery t
  (_       ,"") -> Nothing

scrub :: String -> Text
scrub s = pack $ filter syntax s
  where
    syntax :: Char -> Bool
    syntax c = (not . (`elem` ("()," :: [Char]))) c
