module StarSystem(getGalaxySector)
where

import Data.List
import Data.Char
import Data.Tree
import System.Random
import Control.Monad.State
import Data.Bits
import Data.Ord
import System.IO
import Text.Printf

import Stars
import Sector
import Utils
import Console
import Statistics

type Point = (Int, Int)

data StarSystem = StarSystem {
    getName        :: String
  , getCoordinates :: Point
  , getStars       :: [StellarBody]
  }

instance Displayable StarSystem where
  display s = printf "\t%s (%d, %d)\n%s\n" (getName s) (fst c) (snd c) (concatMap display ss)
    where c  = getCoordinates s
          ss = getStars s

pointRange :: Point
pointRange = (0, 100)

getGalaxySector :: Sector -> [StarSystem]
getGalaxySector = getPartsR (2, 5) randomStarSystem

randomStarSystem :: RndS StarSystem
randomStarSystem = do
    point <- randomPoint
    name <- randomName
    stars <- randomStars
    return $ StarSystem name point stars

randomName :: RndS String
randomName = do
  n <- randomThingR (2, 8)
  s <- replicateM n (randomThingR ('a', 'z'))
  return (capitalize s)

randomPoint :: RndS Point
randomPoint = randomPair pointRange


