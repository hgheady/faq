{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Lib

main :: IO ()
main = do
  ingested <- getContents
  let input = map fromJust $ filter isJust $ map (parse . T.pack) $ lines ingested
  factEngine Map.empty input

factEngine :: (Map Text Node) -> [Input] -> IO ()
factEngine m (i@(Fact  _ _):is) = factEngine (addFact m i) is
factEngine m (i@(Query _ _):is) = runQuery (Map.lookup (iPred i) m) i
                                  >> factEngine m is
factEngine _ []                 = return ()

runQuery :: Maybe Node -> Input -> IO ()
runQuery mn q@(Query prd els) = mapM_ putT $ ["---"]
  <> (case run els [] Map.empty mn of [] -> ["false"]
                                      ts -> ts)
  where
    run :: [QueryElement] -> [Text] -> Map Text Text -> Maybe Node -> [Text]
    run els@(e:es) ts bound (Just n) = case e of
      Literal l  -> if l == nTerm n
                    then run es  ts bound (nChild n)
                    else run els ts bound (nNext  n)
                         
      Variable v -> case Map.lookup v bound of
        Just boundVal -> if boundVal == nTerm n
                         then run els (run es ts bound (nChild n)) bound (nNext n)
                         else if isJust $ nNext n
                              then run els ts bound $ nNext n
                              else []
        Nothing       -> let bound' = Map.insert v (nTerm n) bound
                         in case (valBound (nTerm n) bound) of
                              Just (v', _) -> if v == v'
                                              then case run es ts bound' (nChild n) of
                                                     []  -> run els ts  bound (nNext n)
                                                     ts' -> run els ts' bound (nNext n)
                                              else []
                              Nothing      -> case run es ts bound' (nChild n) of
                                                []  -> run els ts  bound (nNext n)
                                                ts' -> run els ts' bound (nNext n)

    run (Literal  _:_) ts bound Nothing  = []
    run (Variable _:_) ts bound Nothing  = ts
    run []             ts bound Nothing  = let ts' = (map out (Map.toList bound))
                                           in if (length ts' >= 1) then (ts' <> ts) else ["true"]

    valBound :: Text -> (Map Text Text) -> Maybe (Text, Text)
    valBound val map = find isVal (Map.toList map)
      where isVal (_, txt) = txt == val

    out :: (Text, Text) -> Text
    out (v, t) = v <> ": " <> t
