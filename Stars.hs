{-# LANGUAGE TypeSynonymInstances #-}
module Stars(randomStars, Body(..), StellarBody,
    getBody, mkBody, getSatellites,
    BodyType(..),
    Orbit,
    PointF)
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
import Console
import Statistics

type PointF = (Float, Float)

type Orbit = Float -> PointF

data Body = Body {
    getName        :: String
  , getTemperature :: Int
  , getOrbit       :: Orbit
  , getOrbitRadius :: Float
  , getBodyType    :: BodyType
  , getMass        :: Float
  }

type StellarBody = Tree Body

mkBody :: String
       -> Int
       -> Orbit
       -> Float
       -> BodyType
       -> Float
       -> [StellarBody]
       -> StellarBody
mkBody n t o r b m pls = Node (Body n t o r b m) pls

getBody :: StellarBody -> Body
getBody = rootLabel

getSatellites :: StellarBody -> [StellarBody]
getSatellites = subForest

instance Displayable StellarBody where
  display body =
    let b = getBody body
        pls = getSatellites body
    in printf "%s%s" 
         (display b)
         (concatMap display pls)

instance Displayable Body where
  display b =
    let descr     = display (getBodyType b)
        massdescr = if (getBodyType b) == Star then "solar masses" else "Earth masses"
    in printf "%s: %s (Mass: %2.3f %s, %sorbit radius: %2.3f AU)\n" 
                (getName b)
                descr 
                (getMass b)
                massdescr 
                (if (getTemperature b) /= 0 then printf "%d degrees K, " (getTemperature b) else "") 
                (getOrbitRadius b) 

data BodyType = Star
              | GasGiant
              | RockyPlanet
  deriving (Eq)

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
      return $ mkBody n 0 thisorb thisrad GasGiant mass [] -- TODO: add moons
    else do
      mass <- randomRM (0.001, 10)
      return $ mkBody n 0 thisorb thisrad RockyPlanet mass [] -- TODO: temperature

combineOrbits :: Orbit -> Orbit -> Orbit
combineOrbits f1 f2 = \a ->
  let (x0, y0) = f1 a
      (x1, y1) = f2 a
  in (x0 + x1, y0 + y1)

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
      return $ [mkBody n temp orb 0 Star 0 ps] -- TODO: mass
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
      let s1 = mkBody n1 t1 orb1 r1 Star 0 p1s -- TODO: mass
      let s2 = mkBody n2 t2 orb2 r2 Star 0 p2s -- TODO: mass 
      -- TODO: attach planets
      return [s1, s2]

unitCircle :: Float -> PointF
unitCircle a = (cos a, sin a)

circleR :: Float -> Float -> PointF
circleR r a = 
  let (x, y) = unitCircle a
  in (r * x, r * y)

circleVel :: Float -> Float -> Float -> PointF
circleVel v r a = circleR r (a * v * pi * 2)

ellipse :: Float -> Float -> Float -> PointF
ellipse a b t = (a * cos t, b * sin t)

ellipseVel :: Float -> Float -> Float -> Float -> PointF
ellipseVel v a b t = (a * cos (v * t), b * sin (v * t))

shiftedEllipseVel :: Float -> Float -> Float -> Float -> Float -> Float -> PointF
shiftedEllipseVel x0 y0 v a b t = 
  let (x, y) = ellipseVel v a b t
  in (x + x0, y + y0)

