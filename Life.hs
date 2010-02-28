module Life
where

import Data.List(foldl')

import Stars
import Planets

data Country = Country {
    getPop :: Int
  , getTL  :: Int
  , getLoc :: String
  }

createLife :: [StellarBody] -> [Country]
createLife ss = foldl' go [] ss
  where go acc x = 
          case getBodyType x of
            RockyPlanet ->
              if getMass x > 0.1 && 
                 minTemperature ss (getOrbit x) > 230 &&
                 maxTemperature ss (getOrbit x) < 360
                 then newCountry "" : foldl' go acc (getSatellites x)
                 else foldl' go acc (getSatellites x)
            _ ->      foldl' go acc (getSatellites x)

newCountry :: String -> Country
newCountry l = Country 100 1 l

