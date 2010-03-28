module Universe
where

import Graphics.Rendering.OpenGL as OpenGL

import AObject
import Politics
import Statistics

aobjs =
  [ AObject "Star"       0   (Color4 0.9 0.0 0.0 1.0) 6.0 0   Nothing
  , AObject "Murphy's"   10  (Color4 0.5 0.5 1.0 1.0) 2.0 28  (Just "Murphy")
  , AObject "Loki"       250 (Color4 0.0 0.4 0.5 1.0) 4.0 55  (Just "Harju")
  , AObject "Harju"      30  (Color4 0.6 0.6 0.6 1.0) 9.0 115 (Just "Harju")
  , AObject "Riesenland" 80  (Color4 0.1 0.8 0.8 1.0) 2.0 230 (Just "Riesenland")
  , AObject "Natail"     180 (Color4 0.2 0.2 0.9 1.0) 1.5 480 (Just "Natail")
  ]

relations = mkRelationshipMap
  [(("Murphy",     "Harju"),      (Peace, -5)),
   (("Murphy",     "Riesenland"), (Peace, 1)),
   (("Murphy",     "Natail"),     (Peace, -3)),
   (("Harju",      "Riesenland"), (Peace, -1)),
   (("Harju",      "Natail"),     (Peace, 2)),
   (("Riesenland", "Natail"),     (Peace, 0)),
   (("Murphy",     "Murphy"),     (Peace, 10)),
   (("Harju",      "Harju"),      (Peace, 10)),
   (("Riesenland", "Riesenland"), (Peace, 10)),
   (("Natail",     "Natail"),     (Peace, 10))]

randomAllegiance :: IO String
randomAllegiance = chooseIO allegiances

allegiances = ["Murphy", "Harju", "Riesenland", "Natail"]

plalleg :: String
plalleg = ""

initialAttitudes :: AttitudeMap
initialAttitudes = nullAttitudes allegiances


