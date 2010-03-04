module LifeTest
where

import Data.Maybe

import Life
import Sector
import StarSystem
import Stars
import Planets
import Console

sec :: Sector
sec = (3, 4)

cs = createLife getGalaxySector sec

c = head cs

developed = head $ drop 30 $ iterate (map (stepDevelopment getGalaxySector)) $ createLife getGalaxySector (3,4)

