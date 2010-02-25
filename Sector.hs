module Sector
where

import Data.List
import Data.Char
import System.Random
import Control.Monad.State
import Data.Bits
import Data.Ord

type Sector = (Int, Int)

type Rnd = StdGen

type RndS = State Rnd

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

randomThingR :: (Random t, RandomGen s, MonadState s m) => (t, t) -> m t
randomThingR range = do
  r <- get
  let (a, g) = randomR range r
  put g
  return a

randomPair :: (Random t, RandomGen s, MonadState s m) => (t, t) -> m (t, t)
randomPair range = do
  a <- randomThingR range
  a' <- randomThingR range
  return (a, a')

{-
getPart :: RndS a -> Sector -> a
getPart r s = 
  evalState r (sectorToRnd s)
-}


