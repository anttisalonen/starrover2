module LifeTest
where

import Data.Maybe

import Life
import Sector
import StarSystem
import Stars
import Planets
import Console

sec = (3, 4)

cs = createLife sec

c = head cs

