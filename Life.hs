module Life
where

import Data.List(foldl', intercalate, inits)
import Text.Printf
import Data.Maybe

import Sector
import StarSystem
import Stars
import Planets
import Console

data Country = Country {
    getCountryName :: String
  , getPop         :: Int
  , getTL          :: Int
  , getSector      :: Sector
  , getLoc         :: [String]
  }

instance Displayable Country where
  display c = printf "%s - Population: %d - TL: %d - Location %s - %s\n"
                (getCountryName c)
                (getPop c)
                (getTL c)
                (show $ getSector c)
                (intercalate " - " (getLoc c))

displayCountry :: (Sector -> [StarSystem]) -> Country -> String
displayCountry f c = display c ++ rest
  where rest = concat (sysinfo:plinfos)
        (sysinfo, pldispfunc) = case findSystem f (getSector c) (head (getLoc c)) of
                    Just s  -> (displayShort s, displayStellar False (getStars s))
                    Nothing -> ("", displayShort)
        plinfos = map pldispfunc $ catMaybes $ map (findBody f (getSector c)) (tail $ inits (getLoc c))

createLife :: (Sector -> [StarSystem]) -> Sector -> [Country]
createLife f sec = concatMap (createLife' sec) (f sec)

createLife' :: Sector -> StarSystem -> [Country]
createLife' sec sys = foldl' (go [getSSName sys]) [] stars
  where stars = getStars sys
        go parents acc x = 
          let tname = getName x
          in if ratePlanet stars x > 0
               then newCountry sec (tname:parents) : foldl' (go (tname:parents)) acc (getSatellites x)
               else foldl' (go (tname:parents)) acc (getSatellites x)

newCountry :: Sector -> [String] -> Country
newCountry sec parents = Country (head parents) 100 1 sec (reverse parents)

stepDevelopment :: (Sector -> [StarSystem]) -> Country -> Country
stepDevelopment f c = stepDevelopment' (f (getSector c)) c

stepDevelopment' :: [StarSystem] -> Country -> Country
stepDevelopment' sys c = 
  let mplanet = findBody' sys (getLoc c)
  in case mplanet of
       Nothing -> c
       Just pl ->
         let mp = maxPop pl
         in c{getPop = floor (fromIntegral (mp - (getPop c)) * 0.1) + (getPop c)}

-- TODO: write another function which also takes the distance to the planet into account
ratePlanet :: [StellarBody] -> StellarBody -> Float
ratePlanet stars s | getBodyType s /= RockyPlanet = 0
                   | otherwise                    =
  let tempdiff = fromIntegral $ max (abs (maxTemperature stars (getOrbit s) - 295))
                                    (abs (minTemperature stars (getOrbit s) - 295))
      temppoints = max 0 (70 - tempdiff)
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

