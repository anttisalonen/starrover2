module Entity
where

import Control.Monad.State

import Graphics.Rendering.OpenGL as OpenGL

import OpenGLUtils

-- Entity stuff
data Entity = Entity {
    position     :: GLvector3
  , velocity     :: GLvector3
  , acceleration :: GLvector3
  , rotation     :: GLdouble
  , angVelocity  :: GLdouble
  , angAccel     :: GLdouble
  , color        :: Color4 GLfloat
  , primitive    :: PrimitiveMode
  , vertices     :: [GLvector3]
  , scale        :: GLvector3
  }

newEntity :: GLvector3 -> Color4 GLfloat -> PrimitiveMode -> [GLvector3] -> GLvector3 -> Entity
newEntity p c pr vrt scl = Entity p glVector3Null glVector3Null 0 0 0 c pr vrt scl

-- TODO: generate mod-functions using TH
modifyPosition :: (GLvector3 -> GLvector3) -> Entity -> Entity
modifyPosition f t = t{Entity.position = f (Entity.position t)}

modifyVelocity :: (GLvector3 -> GLvector3) -> Entity -> Entity
modifyVelocity f t = t{velocity = f (velocity t)}

modifyAcceleration :: (GLvector3 -> GLvector3) -> Entity -> Entity
modifyAcceleration f t = t{acceleration = f (acceleration t)}

modifyRotation :: (GLdouble -> GLdouble) -> Entity -> Entity
modifyRotation f t = t{rotation = f (rotation t)}

modifyAngVelocity :: (GLdouble -> GLdouble) -> Entity -> Entity
modifyAngVelocity f t = t{angVelocity = f (angVelocity t)}

modifyAngAccel :: (GLdouble -> GLdouble) -> Entity -> Entity
modifyAngAccel f t = t{angAccel = f (angAccel t)}

modifyColor :: (Color4 GLfloat -> Color4 GLfloat) -> Entity -> Entity
modifyColor f t = t{Entity.color = f (Entity.color t)}

modifyPrimitive :: (PrimitiveMode -> PrimitiveMode) -> Entity -> Entity
modifyPrimitive f t = t{primitive = f (primitive t)}

modifyScale :: (GLvector3 -> GLvector3) -> Entity -> Entity
modifyScale f t = t{Entity.scale = f (Entity.scale t)}

degToRad :: (Floating a) => a -> a
degToRad d = d * pi / 180

wrap :: (Num a, Ord a) => a -> a -> a -> a
wrap mn mx v =
  if v < mn 
    then wrap mn mx (v + diff)
    else if v > mx
           then wrap mn mx (v - diff)
           else v
    where diff = mx - mn

wrapDegrees :: (Num a, Ord a) => a -> a
wrapDegrees = wrap (-180) 180

updateEntity :: GLdouble -> Entity -> Entity
updateEntity delta ent = flip execState ent $ do
  let (accx, accy, accz) = acceleration ent
  let rr = degToRad $ rotation ent
  let accVector = 
        (accx * cos rr - accy * sin rr,
         accx * sin rr + accy * cos rr,
         accz)
  modify $ modifyVelocity (*+* (accVector *** delta))
  modify $ modifyPosition (*+* ((velocity ent) *** delta))
  modify $ modifyAngVelocity (+ (angAccel ent) * delta)
  modify $ modifyRotation (wrapDegrees . (+ (angVelocity ent) * delta))

resetAcceleration :: Entity -> Entity
resetAcceleration = modifyAcceleration (const glVector3Null)


