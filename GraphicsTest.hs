{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import System.Exit
import System.Environment
import Control.Monad
import Control.Monad.State as State

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL

import OpenGLUtils
import Entity

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

-- test scenario
step = (0.05, 0.0, 0.0)
width = 800
height = 600

data TestState = TestState {
    tri       :: Entity
  , quad      :: Entity
  , stopped   :: Bool
  }

-- TODO: generate mod-functions using TH
modTri :: (Entity -> Entity) -> TestState -> TestState
modTri f t = t{tri = f (tri t)}

modQuad :: (Entity -> Entity) -> TestState -> TestState
modQuad f t = t{quad = f (quad t)}

modStopped :: (Bool -> Bool) -> TestState -> TestState
modStopped f t = t{stopped = f (stopped t)}

main = withInit [InitVideo] $ do
  blendEquation $= FuncAdd
  blendFunc $= (OpenGL.SrcAlpha, OneMinusSrcAlpha)
  createAWindow 

initState :: TestState
initState = TestState 
    (newEntity glVector3Null (Color4 0.0 0.5 0.0 1.0) Triangles)
    (newEntity glVector3Null (Color4 0.5 0.5 1.0 1.0) Quads) 
    False

createAWindow = do
  setVideoMode width height 0 [OpenGL]
  depthFunc $= Just Less
  clearColor $= Color4 0 0 0 1
  viewport $= (Position 0 0, Size width height)
  matrixMode $= Projection
  loadIdentity
  ortho ((-1) * width * 0.01) (1 * width * 0.01) ((-1) * height * 0.01) (1 * height * 0.01) (-10) 10
  matrixMode $= Modelview 0
  evalStateT loop initState

-- TODO: figure out how to make this a State TestState ()
processEvent :: SDL.Event -> StateT TestState IO ()
processEvent (KeyDown (Keysym SDLK_SPACE _ _)) = modify $ modStopped $ const True
processEvent (KeyUp   (Keysym SDLK_SPACE _ _)) = modify $ modStopped $ const False
processEvent (KeyDown (Keysym SDLK_w     _ _)) = modify $ modTri $ modifyAcceleration (*+* (0.0,   0.002,  0.0))
processEvent (KeyUp   (Keysym SDLK_w     _ _)) = modify $ modTri $ modifyAcceleration (*+* (0.0, (-0.002), 0.0))
processEvent (KeyDown (Keysym SDLK_s     _ _)) = modify $ modTri $ modifyAcceleration (*+* (0.0, (-0.002), 0.0))
processEvent (KeyUp   (Keysym SDLK_s     _ _)) = modify $ modTri $ modifyAcceleration (*+* (0.0,   0.002,  0.0))
processEvent (KeyDown (Keysym SDLK_a     _ _)) = modify $ modTri $ modifyAngVelocity (+1.5)
processEvent (KeyUp   (Keysym SDLK_a     _ _)) = modify $ modTri $ modifyAngVelocity (subtract 1.5)
processEvent (KeyDown (Keysym SDLK_d     _ _)) = modify $ modTri $ modifyAngVelocity (subtract 1.5)
processEvent (KeyUp   (Keysym SDLK_d     _ _)) = modify $ modTri $ modifyAngVelocity (+1.5)
processEvent _                                 = return ()

processEvents :: [SDL.Event] -> StateT TestState IO ()
processEvents = mapM_ processEvent

isQuit :: [SDL.Event] -> Bool
isQuit = hasEvent isq
  where isq Quit = True
        isq (KeyDown (Keysym SDLK_q _ _)) = True
        isq _ = False

loop :: StateT TestState IO ()
loop = do 
  liftIO $ delay 10
  state <- State.get
  liftIO $ drawGLScreen [tri state, quad state]
  when (not (stopped state)) $ do
    modify $ modTri (updateEntity 1)
    modify $ modQuad (updateEntity 1)
  events <- liftIO $ pollAllSDLEvents
  let quit = isQuit events
  processEvents events
  when (not quit) loop

drawGLScreen :: [Entity] -> IO ()
drawGLScreen entities = do
  clear [ColorBuffer,DepthBuffer]

  forM_ entities $ \ent -> do
    loadIdentity
    translate $ (\(x,y,z) -> Vector3 x y z) (Entity.position ent)
    rotate (Entity.rotation ent) $ Vector3 0 0 (1 :: GLfloat)
    currentColor $= (Entity.color ent)
    let points = case primitive ent of
                   Quads -> quadsPoints
                   _     -> polygonPoints
    renderPrimitive (primitive ent) $ forM_ points $ \(x,y,z) -> do
      vertex $ Vertex3 x y z
  
  glSwapBuffers

polygonPoints :: [GLvector3]
polygonPoints =
   [ (0,1, 0)
    ,(-1,-1, 0)
    ,(1,-1, 0)]

quadsPoints :: [GLvector3]
quadsPoints =
   [(1,1,1)  ,(-1,1,1)  ,(-1,-1,1) ,(1,-1,1) ]
