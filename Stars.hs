module Stars(getGalaxySector)
where

import Data.List
import Data.Char
import System.Random
import Control.Monad.State
import Data.Bits
import Data.Ord
import System.IO
import Text.Printf

import Sector
import Utils
import Console
import Statistics

type Point = (Int, Int)

data StarSystem = StarSystem {
    getName        :: String
  , getCoordinates :: Point
  }

instance Displayable StarSystem where
  display s = printf "%s (%d, %d)" (getName s) (fst c) (snd c)
    where c = getCoordinates s

pointRange :: Point
pointRange = (0, 100)

getGalaxySector :: Sector -> [StarSystem]
getGalaxySector = getPartsR (2, 5) randomStarSystem

randomName :: RndS String
randomName = do
  n <- randomThingR (2, 8)
  s <- replicateM n (randomThingR ('a', 'z'))
  return (capitalize s)

randomPoint :: RndS Point
randomPoint = randomPair pointRange

randomStarSystem :: RndS StarSystem
randomStarSystem = do
    point <- randomPoint
    name <- randomName
    stars <- randomStars
    return $ StarSystem name point

type PointF = (Float, Float)

type Orbit = Float -> PointF

data Star = Star {
    getStarTemperature         :: Int
  , getStarOrbit               :: Orbit
  , getPlanets                 :: [Planet]
  }

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

data PlanetType = GasGiant { getPlanetMass :: Float }
                | Rocky    { getPlanetMass :: Float }

type Temperature = Float -> Float

data Planet = Planet {
    getPlanetOrbit       :: Orbit
  , getPlanetType        :: PlanetType
  , getPlanetOrbitRadius :: Float
  }

instance Displayable Planet where
  display (Planet _ (GasGiant m) r) = printf "Gas giant (Mass: %2.3f Earth masses, radius: %2.3f AU)\n" m r
  display (Planet _ (Rocky m) r)    = printf "Rocky planet (Mass: %2.3f Earth masses, radius: %2.3f AU)\n" m r

instance Displayable Star where
  display (Star temp _ pls) = printf "Star with temperature of %d K\n%s" temp (concatMap display pls)

createPlanet :: Orbit -> Float -> RndS Planet
createPlanet toporb thisrad = do
  orbvel <- (+ 5 * (10 / thisrad)) `fmap` randomRM (1.0, 2.0)
  let thisorb = combineOrbits toporb (circleVel orbvel thisrad)
  isGasGiant <- chance 1 2
  if isGasGiant
    then do
      mass <- randomRM (15, 400)
      return $ Planet thisorb (GasGiant mass) thisrad
    else do
      mass <- randomRM (0.001, 10)
      return $ Planet thisorb (Rocky mass) thisrad

combineOrbits :: Orbit -> Orbit -> Orbit
combineOrbits f1 f2 = \a ->
  let (x0, y0) = f1 a
      (x1, y1) = f2 a
  in (x0 + x1, y0 + y1)

createPlanets :: Orbit -> Float -> Float -> RndS [Planet]
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

randomStars :: RndS [Star]
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
      return $ [Star temp orb ps]
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
      let s1 = Star t1 orb1 p1s
      let s2 = Star t2 orb2 p2s
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

