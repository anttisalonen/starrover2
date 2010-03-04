module Planets
where

import Orbit

data StellarBody = StellarBody {
    getName        :: String
  , getTemperature :: Int
  , getOrbit       :: Orbit
  , getOrbitRadius :: Float
  , getBodyType    :: BodyType
  , getMass        :: Float
  , getSatellites  :: [StellarBody]
  }

data BodyType = Star
              | GasGiant
              | RockyPlanet
  deriving (Eq)

temperatureByOrbit :: [StellarBody] -> Float -> Orbit -> Int
temperatureByOrbit stars a orb = temperatureByPoint (stars ++ concatMap getSatellites stars) a (orb a)

temperatureByPoint :: [StellarBody] -> Float -> PointF -> Int
temperatureByPoint stars a p = floor . sum $ map (temperatureByPoint' p a) (stars ++ concatMap getSatellites stars)

-- TODO: this probably doesn't cover the full circle...does it?
minTemperature :: [StellarBody] -> Orbit -> Int
minTemperature stars orb = 
  minimum 
     (map (\a -> temperatureByPoint (stars ++ concatMap getSatellites stars) a (orb a)) [0.0, 0.1..1.0])

maxTemperature :: [StellarBody] -> Orbit -> Int
maxTemperature stars orb = 
  maximum 
     (map (\a -> temperatureByPoint (stars ++ concatMap getSatellites stars) a (orb a)) [0.0, 0.1..1.0])

temperatureByPoint' :: PointF -> Float -> StellarBody -> Float
temperatureByPoint' (x, y) a s 
  | getBodyType s /= Star = 0
  | otherwise             =
      let (x0, y0) = getOrbit s a
          diff     = sqrt $ (x - x0) ^ 2 + (y - y0) ^ 2
      in (0.5 / (1 + 2 * diff)) * (fromIntegral $ getTemperature s)

maxPop :: StellarBody -> Int
maxPop s = case getBodyType s of
             RockyPlanet -> floor $ 1000 * getMass s
             _           -> 0

