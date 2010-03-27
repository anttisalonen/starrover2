module AObject
where

import Data.Maybe

import Graphics.Rendering.OpenGL as OpenGL

import OpenGLUtils
import Entity
import Collision
import Utils

data AObject = AObject {
    aobjName     :: String
  , angle        :: GLdouble
  , color        :: Color4 GLfloat
  , size         :: GLdouble
  , orbitRadius  :: GLdouble
  , colonyOwner  :: Maybe String
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

findCollisions :: ((GLdouble, GLdouble), (GLdouble, GLdouble)) -> [AObject] -> Maybe AObject
findCollisions plbox aobs = 
  listToMaybe . catMaybes $ map colliding aobs
    where colliding aobj =
            if collides2d plbox abox
              then Just aobj
              else Nothing
            where (objcoordx, objcoordy, _) = AObject.getPosition aobj
                  abox = boxArea (objcoordx, objcoordy) (size aobj)

getAObj :: String -> [AObject] -> Maybe AObject
getAObj _ []     = Nothing
getAObj n (a:as) = if aobjName a == n then Just a else getAObj n as 


