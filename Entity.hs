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
  , rotation     :: GLfloat
  , angVelocity  :: GLfloat
  , angAccel     :: GLfloat
  , color        :: Color4 GLfloat
  , primitive    :: PrimitiveMode
  }

newEntity :: GLvector3 -> Color4 GLfloat -> PrimitiveMode -> Entity
newEntity p c pr = Entity p glVector3Null glVector3Null 0 0 0 c pr

-- TODO: generate mod-functions using TH
modifyPosition :: (GLvector3 -> GLvector3) -> Entity -> Entity
modifyPosition f t = t{Entity.position = f (Entity.position t)}

modifyVelocity :: (GLvector3 -> GLvector3) -> Entity -> Entity
modifyVelocity f t = t{velocity = f (velocity t)}

modifyAcceleration :: (GLvector3 -> GLvector3) -> Entity -> Entity
modifyAcceleration f t = t{acceleration = f (acceleration t)}

modifyRotation :: (GLfloat -> GLfloat) -> Entity -> Entity
modifyRotation f t = t{rotation = f (rotation t)}

modifyAngVelocity :: (GLfloat -> GLfloat) -> Entity -> Entity
modifyAngVelocity f t = t{angVelocity = f (angVelocity t)}

modifyAngAccel :: (GLfloat -> GLfloat) -> Entity -> Entity
modifyAngAccel f t = t{angAccel = f (angAccel t)}

modifyColor :: (Color4 GLfloat -> Color4 GLfloat) -> Entity -> Entity
modifyColor f t = t{Entity.color = f (Entity.color t)}

modifyPrimitive :: (PrimitiveMode -> PrimitiveMode) -> Entity -> Entity
modifyPrimitive f t = t{primitive = f (primitive t)}

degToRad :: (Floating a) => a -> a
degToRad d = d * pi / 180

updateEntity :: Float -> Entity -> Entity
updateEntity delta ent = flip execState ent $ do
  let (accx, accy, accz) = acceleration ent
  let rr = degToRad $ rotation ent
  let accVector = 
        (accx * cos rr - accy * sin rr,
         accx * sin rr + accy * cos rr,
         accz)
  -- TODO: wrap rotation
  modify $ modifyVelocity (*+* (accVector *** delta))
  modify $ modifyPosition (*+* ((velocity ent) *** delta))
  modify $ modifyAngVelocity (+ (angAccel ent) * delta)
  modify $ modifyRotation (+ (angVelocity ent) * delta)

resetAcceleration :: Entity -> Entity
resetAcceleration = modifyAcceleration (const glVector3Null)

