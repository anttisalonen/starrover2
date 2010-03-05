module LifeTest()
where

import Data.Maybe

import qualified Data.Edison.Assoc.StandardMap as Map

import Life
import Sector
import StarSystem
import Stars
import Planets
import Console

sec :: Sector
sec = (5, 3)

cs = createLife getGalaxySector sec

developed = head $ drop 30 $ iterate (map (stepDevelopment getGalaxySector)) $ cs

firstCountry = head cs
secondCountry = cs !! 1
initialCountries = countryMap [firstCountry, secondCountry]
firstCountryWithColonies = spread getGalaxySector (getCountriesInSector initialCountries) firstCountry
countriesAfterFirstSpread = countryMap [firstCountryWithColonies, secondCountry]
secondCountryWithColonies = spread getGalaxySector (getCountriesInSector countriesAfterFirstSpread) secondCountry
countriesAfterTwoSpreads = countryMap [firstCountryWithColonies, secondCountryWithColonies]
countryListAfterTwoSpreads = concat $ Map.elements countriesAfterTwoSpreads


