module StarSystem(getGalaxySector, StarSystem(..), findBody, findBody', findSystem, findSystem', Sector, Point)
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
  display s = printf "\t%s (%d, %d)\n%s\n" (getSSName s) (fst c) (snd c) (concatMap (displayStellar True ss) ss)
    where c  = getCoordinates s
          ss = getStars s
  displayShort s = printf "\t%s (%d, %d) - %d stars\n" (getSSName s) (fst c) (snd c) (length (getStars s))
    where c  = getCoordinates s

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

findBody :: (Sector -> [StarSystem]) -> Sector -> [String] -> Maybe StellarBody
findBody f sec ns = findBody' (f sec) ns

findBody' :: [StarSystem] -> [String] -> Maybe StellarBody
findBody' _ [] = Nothing
findBody' systems (n:ns) = 
  case findSystem' systems n of
    Nothing  -> Nothing
    Just sys -> findBody'' (getStars sys) ns

findBody'' :: [StellarBody] -> [String] -> Maybe StellarBody
findBody'' _      []     = Nothing
findBody'' bodies (n:ns) =
  case listToMaybe (filter (\s -> getName s == n) bodies) of
    Nothing -> Nothing
    Just pl -> if null ns
                 then Just pl
                 else findBody'' (getSatellites pl) ns

allBodies :: StellarBody -> [StellarBody]
allBodies s = s : concatMap allBodies (getSatellites s)

findSystem' :: [StarSystem] -> String -> Maybe StarSystem
findSystem' syss n =
  listToMaybe $ filter (\b -> getSSName b == n) syss

findSystem :: (Sector -> [StarSystem]) -> Sector -> String -> Maybe StarSystem
findSystem f sec n = findSystem' (f sec) n

