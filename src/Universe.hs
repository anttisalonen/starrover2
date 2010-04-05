module Universe
where

import Graphics.Rendering.OpenGL as OpenGL

import AObject
import Politics
import Statistics
import Tree
import OpenGLUtils

aobjs :: AObjTree
aobjs = Node (0.0, 0.0)
  [ Leaf $ AObject "Star"       0   (Color4 0.9 0.0 0.0 1.0) 6.0 1.0 0   glVector3Null Nothing
  , Leaf $ AObject "Murphy's"   10  (Color4 0.5 0.5 1.0 1.0) 2.0 1.0 28  glVector3Null (Just "Murphy")
  , Leaf $ AObject "Loki"       250 (Color4 0.0 0.4 0.5 1.0) 4.0 1.0 55  glVector3Null (Just "Harju")
  , Node (30, 115) $ 
       [Leaf $ AObject "Harju"         30  (Color4 0.6 0.6 0.6 1.0) 9.0 1.0 0  glVector3Null (Just "Harju")
      , Leaf $ AObject "Harju's Moon"  30  (Color4 0.2 0.9 0.6 1.0) 0.8 1.0 25 glVector3Null (Just "Harju")]
  , Leaf $ AObject "Riesenland" 80  (Color4 0.1 0.8 0.8 1.0) 2.0 1.0 230 glVector3Null (Just "Riesenland")
  , Leaf $ AObject "Riesenland" 80  (Color4 0.1 0.8 0.8 1.0) 2.0 1.0 230 glVector3Null (Just "Riesenland")
  , Node (180, 480) $ 
       [Leaf $ AObject "Natail"     180 (Color4 0.2 0.2 0.9 1.0) 1.0 1.5 60 glVector3Null (Just "Natail")
      , Leaf $ AObject "Mammoth"    0   (Color4 0.3 0.0 0.6 1.0) 1.5 1.0 40 glVector3Null (Just "Natail")]
  ]

relations = mkRelationshipMap relationsList

relationsList =
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


