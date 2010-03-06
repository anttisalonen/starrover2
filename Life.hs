module Life
where

import Data.List(foldl', intercalate, inits)
import Text.Printf
import Data.Maybe
import qualified Data.Edison.Assoc.StandardMap as Map

import Sector
import StarSystem
import Stars
import Planets
import qualified Market as Market
import MarketData
import Console

data Country = Country {
    getCountryName :: String
  , getProductionSource :: Market.ProductionSource GoodProduction
  , getSector      :: Sector
  , getLoc         :: [String]
  }

getPop :: Country -> Int
getPop = Market.getPopulation . getProductionSource

setPop :: Country -> Int -> Country
setPop c n = 
  let p  = getProductionSource c
      p' = p{Market.getPopulation = n}
  in c{getProductionSource = p'}

getTL :: Country -> Int
getTL = Market.getTechlevel . getProductionSource

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
createLife' sec sys = createLifeExclude' [] sec sys

createLifeExclude :: [[String]] -> (Sector -> [StarSystem]) -> Sector -> [Country]
createLifeExclude excs f sec = concatMap (createLifeExclude' excs sec) (f sec)

createLifeExclude' :: [[String]] -> Sector -> StarSystem -> [Country]
createLifeExclude' excs sec sys = foldl' (go [getSSName sys]) [] stars
  where stars = getStars sys
        go parents acc x = 
          let tname = getName x
          in if ratePlanet stars x > 0 && (reverse (tname:parents)) `notElem` excs
               then newCountry sec (tname:parents) : foldl' (go (tname:parents)) acc (getSatellites x)
               else foldl' (go (tname:parents)) acc (getSatellites x)

getCountriesInSector :: Map.FM Sector [Country] -> Sector -> [Country]
getCountriesInSector m s = Map.lookupWithDefault [] s m

countryMap :: [Country] -> Map.FM Sector [Country]
countryMap = foldl' go Map.empty
  where go acc x = Map.insertWith (++) (getSector x) [x] acc

spread :: (Sector -> [StarSystem]) -> (Sector -> [Country]) -> Country -> Maybe Country
spread fsys fcountries c = spread' fsys (map getLoc (fcountries (getSector c))) c

spread' :: (Sector -> [StarSystem]) -> [[String]] -> Country -> Maybe Country
spread' fsys excs c = 
  listToMaybe $ createLifeExclude excs fsys (getSector c)

newCountry :: Sector -> [String] -> Country
newCountry sec parents = Country (head parents) newPS sec (reverse parents)

newPS :: Market.ProductionSource GoodProduction
newPS = Market.ProductionSource 100 1 Map.empty

stepDevelopment :: (Sector -> [StarSystem]) -> Country -> Country
stepDevelopment f c = stepDevelopment' (f (getSector c)) c

stepDevelopment' :: [StarSystem] -> Country -> Country
stepDevelopment' sys c = 
  let mplanet = findBody' sys (getLoc c)
  in case mplanet of
       Nothing -> c
       Just pl ->
         let mp = maxPop pl
         in setPop c (floor (fromIntegral (mp - (getPop c)) * 0.1) + (getPop c))

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

