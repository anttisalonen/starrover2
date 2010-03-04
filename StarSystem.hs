module StarSystem(getGalaxySector, StarSystem(..), findBody, findSystem, Sector, Point)
where

import Data.List
import Data.Char
import Data.Tree
import Data.Maybe
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


-- Usage: e.g.
-- putStrLn $ concatMap display $ getGalaxySector (0, 0)

type Point = (Int, Int)

data StarSystem = StarSystem {
    getSSName      :: String
  , getCoordinates :: Point
  , getStars       :: [StellarBody]
  }

instance Displayable StarSystem where
  display s = printf "\t%s (%d, %d)\n%s\n" (getSSName s) (fst c) (snd c) (concatMap (displayStellar ss) ss)
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
    stars <- randomStars name
    return $ StarSystem name point stars

randomName :: RndS String
randomName = do
  n <- randomThingR (2, 8)
  s <- replicateM n (randomThingR ('a', 'z'))
  return (capitalize s)

randomPoint :: RndS Point
randomPoint = randomPair pointRange

findBody :: String -> Sector -> Maybe StellarBody
findBody n sec = 
  let allbodies = concatMap allBodies $ concatMap getStars $ getGalaxySector sec
  in listToMaybe $ filter (\b -> getName b == n) allbodies

allBodies :: StellarBody -> [StellarBody]
allBodies s = s : concatMap allBodies (getSatellites s)

findSystem :: String -> Sector -> Maybe StarSystem
findSystem n sec =
  listToMaybe $ filter (\b -> getSSName b == n) (getGalaxySector sec)

