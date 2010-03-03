module Galactic
where

import StarSystem
import Stars

distPoints :: (Float, Float) -> (Float, Float) -> Float
distPoints (x0, y0) (x1, y1) = sqrt $ (x0 - x1) ^ 2 + (y0 - y1) ^ 2

distPoints' :: (Int, Int) -> (Int, Int) -> Float
distPoints' (x0, y0) (x1, y1) = 
  distPoints (fromIntegral x0, fromIntegral y0) (fromIntegral x1, fromIntegral y1)

sectorDist :: Sector -> Sector -> Float
sectorDist = distPoints'

pointDist :: Point -> Point -> Float
pointDist = distPoints'

bodyDist :: Float -> Body -> Body -> Float
bodyDist a b1 b2 = distPoints (getOrbit b1 a) (getOrbit b2 a)

