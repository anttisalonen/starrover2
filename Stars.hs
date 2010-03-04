module Stars(randomStars, StellarBody(..),
    BodyType(..), displayStellar)
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

import Utils
import Orbit
import Planets
import Console
import Statistics

instance Displayable StellarBody where
  display s =
      printf "%s%s" 
                (displayShort s)
                (concatMap display (getSatellites s))
  displayShort s = displayGen s ++ "\n"

displayGen :: StellarBody -> String
displayGen (StellarBody n temp _ rad typ mass _) =
    printf "%s: %s (Mass: %2.3f %s, %sorbit radius: %2.3f AU)" 
              n
              descr 
              mass 
              massdescr 
              (if temp /= 0 then printf "%d degrees K, " temp else "") 
              rad 
       where descr     = display typ
             massdescr = if typ == Star then "solar masses" else "Earth masses"

displayStellar :: [StellarBody] -> StellarBody -> String
displayStellar stars s = displayGen s ++ tempstring
  where tempstring = if getBodyType s == Star
                       then ""
                       else printf "temperatures: %d - %d degrees C" mint maxt
        mint = minTemperature stars (getOrbit s)
        maxt = maxTemperature stars (getOrbit s)

instance Displayable BodyType where
  display Star        = "Star"
  display GasGiant    = "Gas giant"
  display RockyPlanet = "Rocky planet"

randStarTemp :: RndS Int
randStarTemp = do
    big <- chance 1 2
    if big
      then randomRM (10000, 20000)
      else randomRM (3000, 10000)

tempToMaxOrbit :: Int -> Float
tempToMaxOrbit t = (fromIntegral t) * 0.008

tempToMinOrbit :: Int -> Float
tempToMinOrbit t = (fromIntegral t) * 0.00003

createPlanet :: String -> Orbit -> Float -> RndS StellarBody
createPlanet n toporb thisrad = do
  orbvel <- (+ 5 * (10 / thisrad)) `fmap` randomRM (1.0, 2.0)
  let thisorb = combineOrbits toporb (circleVel orbvel thisrad)
  isGasGiant <- chance 1 2
  if isGasGiant
    then do
      mass <- randomRM (15, 400)
      return $ StellarBody n 0 thisorb thisrad GasGiant mass [] -- TODO: add moons
    else do
      mass <- randomRM (0.001, 10)
      return $ StellarBody n 0 thisorb thisrad RockyPlanet mass [] -- TODO: temperature

createPlanets :: [String] -> Orbit -> Float -> Float -> RndS [StellarBody]
createPlanets ns toporb minrad maxrad 
  | minrad >= maxrad = return []
  | otherwise        = do
     thisrad <- randomRM (minrad, minrad * 2)
     if thisrad >= maxrad
       then return []
       else do
         pl <- createPlanet (head ns) toporb thisrad
         pls <- createPlanets (tail ns) toporb (thisrad * 2) maxrad
         return (pl:pls)

extendName :: String -> [String]
extendName n = 
  if isAlpha (last n)
    then zipWith (++) (repeat (n ++ " ")) (map show [1..])
    else zipWith (++) (repeat (n ++ " ")) (map (:[]) ['A'..'Z'])

randomStars :: String -> RndS [StellarBody]
randomStars n = do
  -- http://www.cfa.harvard.edu/news/2006/pr200611.html
  -- "Most milky way stars are single"
  split <- chance 1 2
  let childNames = extendName n
  if not split
    then do
      temp <- randStarTemp
      let orb = (\_ -> (0, 0))
      let minorb = tempToMinOrbit temp
      let maxorb = tempToMaxOrbit temp
      ps <- createPlanets childNames orb minorb maxorb
      return $ [StellarBody n temp orb 0 Star 0 ps] -- TODO: mass
    else do
      vel <- randomRM (0.1, 100)
      r1 <- randomRM (1, 1000)
      r2 <- randomRM (0.1, r1)
      t1 <- randStarTemp
      -- TODO: t2 dependent of ratio of radii
      t2 <- randStarTemp
      -- TODO: recurse split for multiple star systems
      -- the max orbit determination is probably not that good...
      let orb1 = circleVel vel r1
      let orb2 = circleVel vel r2
      let minorb1 = tempToMinOrbit t1
      let minorb2 = tempToMinOrbit t2
      let maxorb1 = (min (tempToMaxOrbit t1) ((max r1 r2) * 0.5))
      let maxorb2 = (min (tempToMaxOrbit t2) ((max r1 r2) * 0.5))
      let n1 = childNames !! 0
      let n2 = childNames !! 1
      p1s <- createPlanets (extendName n1) orb1 minorb1 maxorb1
      p2s <- createPlanets (extendName n2) orb2 minorb2 maxorb2
      let s1 = StellarBody n1 t1 orb1 r1 Star 0 p1s -- TODO: mass
      let s2 = StellarBody n2 t2 orb2 r2 Star 0 p2s -- TODO: mass 
      -- TODO: attach planets
      return [s1, s2]


