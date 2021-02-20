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
import Run

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
