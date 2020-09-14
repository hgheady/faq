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
  factEngine input' Map.empty

factEngine :: [Input] -> (Map Text Node) -> IO [()]
factEngine (i@(Fact  _ _):is) m = factEngine is $ addFact m i
factEngine (i@(Query _ _):is) m =
  runQuery (Map.lookup (iPred i) m) i >> factEngine is m
factEngine [] _ = return [()]

addFact :: (Map Text Node) -> Input -> Map Text Node
addFact m f = addNode m f

                        --v Query predicate elements match bnd
runQuery :: Maybe Node -> Input -> IO [()]
runQuery mn q@(Query prd els) = mapM putT $ ["---"] <> reverse
  (case run els [] Map.empty mn of
     [] -> ["false"]
     ts -> ts)
  where
    run :: [QueryElement] -> [Text] -> Map Text Text -> Maybe Node -> [Text]
    run els@(e:es) ts bnd (Just n) = case e of
      Literal l  -> if l == nTerm n
                    then run es  ts bnd (nChild n)
                    else run els ts bnd (nNext  n)
                         
      Variable v -> case Map.lookup v bnd of
                      Just bound -> if bound /= nTerm n
                                    then case nNext n of
                                           n'@(Just _) -> run els ts bnd n'
                                           Nothing     -> []
                                    else case run es ts bnd (nChild n) of
                                           []  -> run els ts bnd (nNext n)
                                           ts' -> run els ts' bnd (nNext n)
                      Nothing    -> let bnd' = Map.insert v (nTerm n) bnd
                                        ts'  = (v <> ": " <> nTerm n) : ts
                                    in case run es ts' bnd' (nChild n) of
                                           []  -> run els ts  bnd (nNext n)
                                           ts' -> run els ts' bnd (nNext n)
    run ((Variable v):es) ts _ Nothing  = ts
    run ((Literal  l):es) ts _ Nothing  = []
    run []                [] _ Nothing  = ["true"]
    run []                ts _ Nothing  = ts
    run []                ts _ (Just _) = []

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
