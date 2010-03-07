module Entity
where

import Control.Monad.State

import Graphics.Rendering.OpenGL as OpenGL

import OpenGLUtils

-- Entity stuff
data Entity = Entity {
    position     :: GLvector3
  , rotation     :: GLfloat
  , velocity     :: GLvector3
  , acceleration :: GLvector3
  , color        :: Color4 GLfloat
  , primitive    :: PrimitiveMode
  }

newEntity :: GLvector3 -> Color4 GLfloat -> PrimitiveMode -> Entity
newEntity p c pr = Entity p 0 glVector3Null glVector3Null c pr

-- TODO: generate mod-functions using TH
modifyPosition :: (GLvector3 -> GLvector3) -> Entity -> Entity
modifyPosition f t = t{Entity.position = f (Entity.position t)}

modifyRotation :: (GLfloat -> GLfloat) -> Entity -> Entity
modifyRotation f t = t{rotation = f (rotation t)}

modifyVelocity :: (GLvector3 -> GLvector3) -> Entity -> Entity
modifyVelocity f t = t{velocity = f (velocity t)}

modifyAcceleration :: (GLvector3 -> GLvector3) -> Entity -> Entity
modifyAcceleration f t = t{acceleration = f (acceleration t)}

modifyColor :: (Color4 GLfloat -> Color4 GLfloat) -> Entity -> Entity
modifyColor f t = t{Entity.color = f (Entity.color t)}

modifyPrimitive :: (PrimitiveMode -> PrimitiveMode) -> Entity -> Entity
modifyPrimitive f t = t{primitive = f (primitive t)}

updateEntity :: Float -> Entity -> Entity
updateEntity delta ent = flip execState ent $ do
  modify $ modifyVelocity (*+* ((acceleration ent) *** delta))
  modify $ modifyPosition (*+* ((velocity ent) *** delta))
