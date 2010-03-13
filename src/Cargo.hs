module Cargo
where

import qualified Data.Edison.Assoc.StandardMap as M

import Statistics

type Cargo = M.FM String Int

cargonames = ["Grain", "Fruit", "Gem stones", "Firearms"]

randomCargo :: IO String
randomCargo = chooseIO cargonames

