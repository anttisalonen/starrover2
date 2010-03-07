module AObject
where

import Graphics.Rendering.OpenGL as OpenGL

import OpenGLUtils
import Entity

data AObject = AObject {
    initialAngle :: GLdouble
  , color        :: Color4 GLfloat
  , size         :: GLdouble
  , orbitRadius  :: GLdouble
  }

aobjToEntities :: AObject -> [Entity]
aobjToEntities a = [e, o]
  where e = newEntity 
               (sin (initialAngle a) * (orbitRadius a),
                cos (initialAngle a) * (orbitRadius a),
                0)
               (AObject.color a)
               Polygon
               (circlePoints 16)
               (glVector3AllUnit *** (size a))
        o = newEntity
               glVector3Null
               (Color4 0.5 0.5 0.5 0.1)
               LineLoop
               (circlePoints 128)
               (glVector3AllUnit *** (orbitRadius a))


