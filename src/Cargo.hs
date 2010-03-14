module Cargo
where

import Text.Printf

import qualified Data.Edison.Assoc.StandardMap as M

import Statistics

type Cargo = M.FM String Int

cargonames = ["Grain", "Fruit", "Gem stones", "Firearms"]

randomCargo :: IO String
randomCargo = chooseIO cargonames

showCargo :: Cargo -> String
showCargo c | M.null c  = "No cargo\n"
            | otherwise = concatMap (\(k, v) -> printf "%-20s-%4d\n" k v) (M.toSeq c) 

