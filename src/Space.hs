{-# LANGUAGE NoMonomorphismRestriction #-}
module Space
where

import Control.Monad

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL

import OpenGLUtils
import Entity
import AObject

-- generic stuff
pollAllSDLEvents :: IO [SDL.Event]
pollAllSDLEvents = go []
    where go l = do
                   e <- SDL.pollEvent
                   if e == SDL.NoEvent
                     then return l
                     else do
                       es <- pollAllSDLEvents
                       return (e:es)

hasEvent :: (SDL.Event -> Bool) -> [SDL.Event] -> Bool
hasEvent fun evts = or $ map fun evts

newStdShip :: GLvector3 -> Color4 GLfloat -> Entity
newStdShip pos c = newEntity pos c TriangleFan trianglePoints glVector3AllUnit

playerShipColor, enemyShipColor :: Color4 GLfloat
playerShipColor = Color4 0.0 0.5 0.0 1.0
enemyShipColor  = Color4 0.5 0.0 0.0 1.0

boxArea (x, y) r = ((x - r, x + r), (y - r, y + r))

width = 800
height = 600

drawGLScreen :: [Entity] -> [AObject] -> IO ()
drawGLScreen ents objs = do
  clear [ColorBuffer,DepthBuffer]
  lineWidth $= 5

  forM_ ents $ \ent -> do
    loadIdentity
    translate $ (\(x,y,z) -> Vector3 x y z) (Entity.position ent)
    rotate (Entity.rotation ent) $ Vector3 0 0 (1 :: GLdouble)
    (\(x,y,z) -> OpenGL.scale x y z) (Entity.scale ent)
    currentColor $= (Entity.color ent)
    renderPrimitive (primitive ent) $ forM_ (vertices ent) $ \(x,y,z) -> do
      vertex $ Vertex3 x y z
  
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

processEvent :: (Monad m) => [(SDLKey, (m (), m ()))] -> Event -> m ()
processEvent n evt =
  let mk = case evt of
             KeyDown (Keysym k _ _) -> Just (True, k)
             KeyUp   (Keysym k _ _) -> Just (False, k) 
             _                      -> Nothing
  in case mk of
       Nothing     -> return ()
       Just (e, k) -> case lookup k n of
                        Nothing       -> return ()
                        Just (a1, a2) -> if e then a1 else a2

processEvents
  :: (Monad m) => [(SDLKey, (m (), m ()))] -> [Event] -> m ()
processEvents n = mapM_ (processEvent n)

isQuit :: [SDL.Event] -> Bool
isQuit = hasEvent isq
  where isq Quit = True
        isq (KeyDown (Keysym SDLK_q _ _)) = True
        isq _ = False

collides1d (a, b) (c, d) =
  (a < c && c < b) || (a < d && d < b) ||
  (c < a && a < d) || (c < b && b < d)

collides2d (x1, y1) (x2, y2) =
  collides1d x1 x2 && collides1d y1 y2

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
