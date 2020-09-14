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
  factEngine input' Map.empty

factEngine :: [Input] -> (Map Text Node) -> IO [()]
factEngine (i@(Fact  _ _    ):is) m = factEngine is $ addFact m i
factEngine (i@(Query _ _ _ _):is) m =
  runQuery (Map.lookup (iPred i) m) i >> factEngine is m
factEngine [] _ = return [()]

addFact :: (Map Text Node) -> Input -> Map Text Node
addFact m f = addNode m f

                        --v Query predicate elements match bnd
runQuery :: Maybe Node -> Input -> IO [()]
runQuery mn q@(Query prd els mch bnd) =
  mapM putT $ ["---"] <> case run els [] mn of
                           [] -> ["false"]
                           ts -> ts
  where
    run :: [QueryElement] -> [Text] -> Maybe Node -> [Text]
    run els@(e:es) ts (Just n) = case e of
      Literal l  -> if l == nTerm n
                    then run es  ts (nChild n)
                    else run els ts (nNext  n)
      Variable v -> case run es (v <> ": " <> (nTerm n)  : ts) (nChild n) of
        []  -> run els ts (nNext n)
        ts' -> run els ts' (nNext n)
    run ((Variable v):es) ts Nothing = ts
    run ((Literal  l):es) ts Nothing = []
    run [] [] Nothing  = ["true"]
    run [] ts Nothing  = ts
    run [] ts (Just _) = []

-- return with no bindings, not failed = 'true'

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
