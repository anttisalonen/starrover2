module Life
where

import Data.List(foldl')
import Text.Printf

import Sector
import StarSystem
import Stars
import Planets
import Console

data Country = Country {
    getCountryName :: String
  , getPop :: Int
  , getTL  :: Int
  , getLoc :: (Sector, StellarBody)
  }

instance Displayable Country where
  display c = printf "%s - Population: %d - TL: %d - Location %s:\n%s"
                (getCountryName c)
                (getPop c)
                (getTL c)
                (show . fst $ getLoc c)
                (display . snd $ getLoc c)

createLife :: Sector -> [Country]
createLife sec = foldl' go [] ss
  where ss = concatMap getStars $ getGalaxySector sec
        go acc x = 
          case getBodyType x of
            RockyPlanet ->
              if getMass x > 0.1 && 
                 minTemperature ss (getOrbit x) > 230 &&
                 maxTemperature ss (getOrbit x) < 360
                 then newCountry sec x : foldl' go acc (getSatellites x)
                 else foldl' go acc (getSatellites x)
            _ ->      foldl' go acc (getSatellites x)

newCountry :: Sector -> StellarBody -> Country
newCountry sec s = Country (getName s) 100 1 (sec, s)

stepDevelopment :: Country -> Country
stepDevelopment c = 
  let mp = maxPop (snd . getLoc $ c)
  in c{getPop = floor (fromIntegral (mp - (getPop c)) * 0.1) + (getPop c)}

-- TODO: also take distance to the planet into account
ratePlanet :: [StellarBody] -> StellarBody -> Float
ratePlanet stars s | getBodyType s /= RockyPlanet = 0
                   | otherwise                    =
  let tempdiff = fromIntegral $ max (abs (maxTemperature stars (getOrbit s) - 300))
                                    (abs (minTemperature stars (getOrbit s) - 300))
      temppoints = max 0 (100 - tempdiff)
      sizepoints = getMass s * 10
  in min temppoints sizepoints

rateSystem :: StarSystem -> [(String, Float)]
rateSystem sys = 
  filter (\(_, v) -> v > 0.0) $
  zip (map getName (concatMap getSatellites (getStars sys)))
      (map 
         (\s -> ratePlanet (getStars sys) s)
         (concatMap getSatellites (getStars sys)))

rateSector :: Sector -> [(String, Float)]
rateSector sec = concatMap rateSystem (getGalaxySector sec)

