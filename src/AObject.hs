module AObject
where

import Graphics.Rendering.OpenGL as OpenGL

import OpenGLUtils
import Entity

data AObject = AObject {
    aobjName     :: String
  , angle        :: GLdouble
  , color        :: Color4 GLfloat
  , size         :: GLdouble
  , orbitRadius  :: GLdouble
  }

modifyAngle :: (GLdouble -> GLdouble) -> AObject -> AObject
modifyAngle f t = t{angle = f (angle t)}

aobjToEntities :: AObject -> (Entity, Entity)
aobjToEntities a = (e, o)
  where e = newEntity 
               (sin (degToRad $ angle a) * (orbitRadius a),
                cos (degToRad $ angle a) * (orbitRadius a),
                0)
               0
               (AObject.color a)
               Polygon
               (circlePoints 16)
               (glVector3AllUnit *** (size a))
        o = newEntity
               glVector3Null
               0
               (Color4 0.5 0.5 0.5 0.1)
               LineLoop
               (circlePoints 128)
               (glVector3AllUnit *** (orbitRadius a))

aobjPoints = circlePoints 16
aorbitPoints = circlePoints 128
aorbitColor = Color4 0.5 0.5 0.5 (1 :: GLfloat)

getPosition :: AObject -> GLvector3
getPosition aobj = 
  let r = orbitRadius aobj
      a = degToRad $ angle aobj
      objcoordx = r * cos a
      objcoordy = r * sin a
  in (objcoordx, objcoordy, 0)
