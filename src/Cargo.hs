module Cargo
where

import System.Random
import Text.Printf
import Control.Monad

import qualified Data.Edison.Assoc.StandardMap as M

import Statistics

type Cargo = M.FM String Int
type Market = [(String, (Int, Int))]

cargonames = ["Grain", "Fruit", "Gem stones", "Firearms"]

randomCargo :: IO String
randomCargo = chooseIO cargonames

showCargo :: Cargo -> String
showCargo c | M.null c  = "No cargo\n"
            | otherwise = concatMap (\(k, v) -> printf "%-20s-%4d\n" k v) (M.toSeq c) 

showMarket :: Market -> String
showMarket m = title ++ infos
  where title = printf "%-16s%8s%8s\n" "Good" "Quantity" "Price"
        infos = concatMap (\(n, (q, p)) -> printf "%-20s%10d%10d\n" n q p) m

numCargoItems = length cargonames

randomMarket :: IO Market
randomMarket = do
  let names = cargonames
  prices <- replicateM numCargoItems $ randomRIO (5, 15)
  quantities <- replicateM numCargoItems $ randomRIO (0, 30)
  return $ zip names (zip quantities prices)

fitCargo :: Cargo -> [(String, Int)]
fitCargo c = map (\s -> (s, M.lookupWithDefault 0 s c)) cargonames

showMarketAndCargo :: Market -> Cargo -> String
showMarketAndCargo m c = title ++ infos
  where title = printf "%-14s%9s%6s%6s\n" "Good" "Quantity" "Price" "Cargo"
        infos = concatMap (\((n, (q, p)), (_, q')) -> printf "%-14s%9d%6d%6d\n" n q p q') (zip m (fitCargo c))
