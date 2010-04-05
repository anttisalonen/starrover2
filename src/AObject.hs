module AObject
where

import Data.Maybe
import Data.Foldable

import Graphics.Rendering.OpenGL as OpenGL

import OpenGLUtils
import Entity
import Collision
import Utils
import Tree

data AObject = AObject {
    aobjName          :: String
  , angle             :: GLdouble
  , color             :: Color4 GLfloat
  , size              :: GLdouble
  , orbitalSpeedcoeff :: GLdouble
  , orbitRadius       :: GLdouble
  , barycenter        :: GLvector3
  , colonyOwner       :: Maybe String
  }

modifyAngle :: (GLdouble -> GLdouble) -> AObject -> AObject
modifyAngle f t = t{angle = f (angle t)}

modifyBarycenter :: (GLvector3 -> GLvector3) -> AObject -> AObject
modifyBarycenter f t = t{barycenter = f (barycenter t)}

nullBarycenters :: AObjTree -> AObjTree
nullBarycenters (Leaf a)    = Leaf (modifyBarycenter (const glVector3Null) a)
nullBarycenters (Node n ts) = Node n (map nullBarycenters ts)

setupBarycenters :: AObjTree -> AObjTree
setupBarycenters = updateBarycenters . nullBarycenters

updateBarycenters :: AObjTree -> AObjTree
updateBarycenters = go glVector3Null
  where go disp (Leaf a)             = 
          Leaf (modifyBarycenter (*+* disp) a)
        go disp (Node (ang, rad) ts) = 
          Node (ang, rad) 
               (map (go ((getPosition' ang rad) *+* disp)) ts)

type Orbit = (GLdouble, GLdouble) -- angle, orbitRadius

type AObjTree = Tree Orbit AObject

nullAObjTree :: AObjTree
nullAObjTree = Node (0.0, 0.0) []

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

aobjPoints = circlePoints 32
aorbitPoints = circlePoints 128
aorbitColor = Color4 0.5 0.5 0.5 (1 :: GLfloat)

getPosition :: AObject -> GLvector3
getPosition aobj = 
  (getPosition' (angle aobj) (orbitRadius aobj)) *+* 
    (barycenter aobj)

getPosition' :: GLdouble -> GLdouble -> GLvector3
getPosition' a' r = 
  let a = degToRad a'
  in (r * cos a, r * sin a, 0)

findCollisions :: (Foldable f) => ((GLdouble, GLdouble), (GLdouble, GLdouble)) -> f AObject -> Maybe AObject
findCollisions plbox aobs = 
  listToMaybe . catMaybes $ map colliding (toList aobs)
    where colliding aobj =
            if collides2d plbox abox
              then Just aobj
              else Nothing
            where (objcoordx, objcoordy, _) = AObject.getPosition aobj
                  abox = boxArea (objcoordx, objcoordy) (size aobj)

planetNameToAllegiance :: AObjTree -> String -> String
planetNameToAllegiance aobs planetname =
  fromMaybe "Unknown" (fmap aobjName (find (\a -> aobjName a == planetname) aobs))

getAllegiance :: AObject -> String
getAllegiance a = fromMaybe "Unknown" (colonyOwner a)

hasOwner :: String -> AObject -> Bool
hasOwner alleg a = case colonyOwner a of
                     Nothing -> False
                     Just n  -> n == alleg

hasSomeOwner :: AObject -> Bool
hasSomeOwner = isJust . colonyOwner
