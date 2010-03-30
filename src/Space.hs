module Space(newStdShipEntity,
  playerShipColor,
  enemyShipColor,
  width,
  height,
  drawGLScreen,
  drawEntity,
  collides2d,
  getShipBox,
  angleFromTo,
  angleFromToRad,
  randPos
  )
where

import Control.Monad
import System.Random

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL

import OpenGLUtils
import Entity
import AObject
import Utils
import Collision

newStdShipEntity :: GLvector3 -> Color4 GLfloat -> GLdouble -> Entity
newStdShipEntity pos c rot = newEntity pos rot c TriangleFan trianglePoints glVector3AllUnit

playerShipColor, enemyShipColor :: Color4 GLfloat
playerShipColor = Color4 0.0 0.5 0.0 1.0
enemyShipColor  = Color4 0.5 0.0 0.0 1.0

width :: (Num a) => a
width = 800

height :: (Num a) => a
height = 600

drawEntity :: Entity -> IO ()
drawEntity ent = do
    loadIdentity
    translate $ (\(x,y,z) -> Vector3 x y z) (Entity.position ent)
    rotate (Entity.rotation ent) $ Vector3 0 0 (1 :: GLdouble)
    (\(x,y,z) -> OpenGL.scale x y z) (Entity.scale ent)
    currentColor $= (Entity.color ent)
    renderPrimitive (primitive ent) $ forM_ (vertices ent) $ \(x,y,z) -> do
      vertex $ Vertex3 x y z

drawGLScreen :: [Entity] -> [AObject] -> IO ()
drawGLScreen ents objs = do
  clear [ColorBuffer,DepthBuffer]
  lineWidth $= 5

  forM_ ents drawEntity
  
  lineWidth $= 1
  forM_ objs $ \aobj -> do
    loadIdentity
    rotate (angle aobj) $ Vector3 0 0 (1 :: GLdouble)
    translate $ Vector3 (orbitRadius aobj) 0 0
    uniformScale (size aobj)
    currentColor $= (AObject.color aobj)
    renderPrimitive Polygon $ forM_ aobjPoints $ \(x,y,z) -> do
      vertex $ Vertex3 x y z

    loadIdentity
    uniformScale (orbitRadius aobj)
    currentColor $= aorbitColor
    renderPrimitive LineLoop $ forM_ aorbitPoints $ \(x,y,z) -> do
      vertex $ Vertex3 x y z

  glSwapBuffers

angleFromTo :: (RealFloat t) => (t, t, t)
            -> (t, t, t)
            -> t
angleFromTo a b = radToDeg $ angleFromToRad a b

angleFromToRad :: (RealFloat t) => (t, t, t)
            -> (t, t, t)
            -> t
angleFromToRad (ax, ay, _) (bx, by, _) =
  atan2 (by - ay) (bx - ax)

randPos :: ((Int, Int), (Int, Int)) -> IO GLvector3
randPos ((minx, miny), (maxx, maxy)) = do
  x <- fromIntegral `fmap` randomRIO (minx, maxx)
  y <- fromIntegral `fmap` randomRIO (miny, maxy)
  return (x, y, 0)

getShipBox
  :: Entity -> ((GLdouble, GLdouble), (GLdouble, GLdouble))
getShipBox e = 
  let (x, y, _) = Entity.position e
  in boxArea (x, y) 1

trianglePoints :: [GLvector3]
trianglePoints =
   [ (0,    1,   0)
    ,(0.9, -1,   0)
    ,(0.0, -0.7, 0)
    ,(-0.9,-1,   0)
   ]


