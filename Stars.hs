module Stars(randomStars, StellarBody(..),
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

data StellarBody = StellarBody {
    getTemperature :: Int
  , getOrbit       :: Orbit
  , getOrbitRadius :: Float
  , getBodyType    :: BodyType
  , getMass        :: Float
  , getSatellites  :: [StellarBody]
  }

instance Displayable StellarBody where
  display (StellarBody temp _ rad typ mass pls) =
      printf "%s (Mass: %2.3f %s, %sorbit radius: %2.3f AU)\n%s" 
                descr 
                mass 
                massdescr 
                (if temp /= 0 then printf "%d degrees K, " temp else "") 
                rad 
                (concatMap display pls)
         where descr     = display typ
               massdescr = if typ == Star then "solar masses" else "Earth masses"

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

createPlanet :: Orbit -> Float -> RndS StellarBody
createPlanet toporb thisrad = do
  orbvel <- (+ 5 * (10 / thisrad)) `fmap` randomRM (1.0, 2.0)
  let thisorb = combineOrbits toporb (circleVel orbvel thisrad)
  isGasGiant <- chance 1 2
  if isGasGiant
    then do
      mass <- randomRM (15, 400)
      return $ StellarBody 0 thisorb thisrad GasGiant mass [] -- TODO: add moons
    else do
      mass <- randomRM (0.001, 10)
      return $ StellarBody 0 thisorb thisrad RockyPlanet mass [] -- TODO: temperature

combineOrbits :: Orbit -> Orbit -> Orbit
combineOrbits f1 f2 = \a ->
  let (x0, y0) = f1 a
      (x1, y1) = f2 a
  in (x0 + x1, y0 + y1)

createPlanets :: Orbit -> Float -> Float -> RndS [StellarBody]
createPlanets toporb minrad maxrad 
  | minrad >= maxrad = return []
  | otherwise        = do
     thisrad <- randomRM (minrad, minrad * 2)
     if thisrad >= maxrad
       then return []
       else do
         pl <- createPlanet toporb thisrad
         pls <- createPlanets toporb (thisrad * 2) maxrad
         return (pl:pls)

randomStars :: RndS [StellarBody]
randomStars = do
  -- http://www.cfa.harvard.edu/news/2006/pr200611.html
  -- "Most milky way stars are single"
  split <- chance 1 2
  if not split
    then do
      temp <- randStarTemp
      let orb = (\_ -> (0, 0))
      let minorb = tempToMinOrbit temp
      let maxorb = tempToMaxOrbit temp
      ps <- createPlanets orb minorb maxorb
      return $ [StellarBody temp orb 0 Star 0 ps] -- TODO: mass
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
      p1s <- createPlanets orb1 minorb1 maxorb1
      p2s <- createPlanets orb2 minorb2 maxorb2
      let s1 = StellarBody t1 orb1 r1 Star 0 p1s -- TODO: mass
      let s2 = StellarBody t2 orb2 r2 Star 0 p2s -- TODO: mass 
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

