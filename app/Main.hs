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
  factEngine Map.empty input'
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

addFact :: (Map Text Node) -> Input -> IO (Map Text Node)
addFact m f = do
  return $ addNode m f

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



isFact :: String -> Bool
isFact t = "INPUT " `isPrefixOf` t

isQuery :: String -> Bool
isQuery t = "QUERY " `isPrefixOf` t

scrub :: String -> Text
scrub s = pack $ filter syntax $ drop 6 s
  where
    syntax :: Char -> Bool
    syntax c = (not . (`elem` ("()," :: [Char]))) c
