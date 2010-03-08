module Sector(Sector, getPart, getPartsR)
where

import Data.List
import Data.Char
import System.Random
import Control.Monad.State
import Data.Bits
import Data.Ord

import Utils

type Sector = (Int, Int)

getPart :: RndS a -> Sector -> a
getPart r s = 
  evalState r (sectorToRnd s)

getPartsR :: (Int, Int) -> RndS a -> Sector -> [a]
getPartsR n r s =
  evalState (do
     num <- randomThingR n
     replicateM num r)
     (sectorToRnd s)

sectorToSeed :: Sector -> Int
sectorToSeed (sx, sy) = sx `shiftL` (bitSize sx `div` 2) + sy

sectorToRnd :: Sector -> Rnd
sectorToRnd s = mkStdGen . sectorToSeed $ s


