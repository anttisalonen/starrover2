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
import Camera
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

-- test scenario
width = 800
height = 600

data TestState = TestState {
    tri          :: Entity
  , aobjects     :: [AObject]
  , camera       :: Camera
  , camzoom      :: GLdouble
  , camzoomdelta :: GLdouble
  , stopped      :: Bool
  }

-- TODO: generate mod-functions using TH
modTri :: (Entity -> Entity) -> TestState -> TestState
modTri f t = t{tri = f (tri t)}

modAObjects :: ([AObject] -> [AObject]) -> TestState -> TestState
modAObjects f t = t{aobjects = f (aobjects t)}

modCamera :: (Camera -> Camera) -> TestState -> TestState
modCamera f t = t{camera = f (camera t)}

modCamZoom :: (GLdouble -> GLdouble) -> TestState -> TestState
modCamZoom f t = t{camzoom = f (camzoom t)}

modCamZoomDelta :: (GLdouble -> GLdouble) -> TestState -> TestState
modCamZoomDelta f t = t{camzoomdelta = f (camzoomdelta t)}

modStopped :: (Bool -> Bool) -> TestState -> TestState
modStopped f t = t{stopped = f (stopped t)}

main = withInit [InitVideo] $ do
  blendEquation $= FuncAdd
  blendFunc $= (OpenGL.SrcAlpha, OneMinusSrcAlpha)
  createAWindow 

aobjs =
  [ AObject 0   (Color4 0.9 0.0 0.0 1.0) 6.0 0
  , AObject 10  (Color4 0.5 0.5 1.0 1.0) 2.0 28
  , AObject 250 (Color4 0.0 0.4 0.5 1.0) 4.0 80
  , AObject 30  (Color4 0.6 0.6 0.6 1.0) 3.0 100
  , AObject 80  (Color4 0.6 0.6 0.6 1.0) 2.0 130
  ]

aobjsAndOrbits = unzip $ map aobjToEntities aobjs

initState :: TestState
initState = TestState 
    (newEntity (50.0, 30.0, 0.0) (Color4 0.0 0.5 0.0 1.0) TriangleFan trianglePoints glVector3AllUnit)
    aobjs
    ((-0.01 * width, -0.01 * height), (0.02 * width, 0.02 * height))
    100
    0
    False

createAWindow = do
  setVideoMode width height 0 [OpenGL]
  depthFunc $= Just Less
  clearColor $= Color4 0 0 0 1
  viewport $= (Position 0 0, Size width height)
  setCamera (camera initState)
  evalStateT loop initState

zoomChangeFactor :: (Floating a) => a
zoomChangeFactor = 1.0

-- TODO: figure out how to make this a State TestState ()
processEvent :: SDL.Event -> StateT TestState IO ()
processEvent (KeyDown (Keysym SDLK_SPACE _ _)) = modify $ modStopped not
processEvent (KeyDown (Keysym SDLK_w     _ _)) = modify $ modTri $ modifyAcceleration (*+* (0.0,   0.002,  0.0))
processEvent (KeyUp   (Keysym SDLK_w     _ _)) = modify $ modTri $ modifyAcceleration (*+* (0.0, (-0.002), 0.0))
processEvent (KeyDown (Keysym SDLK_s     _ _)) = modify $ modTri $ modifyAcceleration (*+* (0.0, (-0.002), 0.0))
processEvent (KeyUp   (Keysym SDLK_s     _ _)) = modify $ modTri $ modifyAcceleration (*+* (0.0,   0.002,  0.0))
processEvent (KeyDown (Keysym SDLK_a     _ _)) = modify $ modTri $ modifyAngVelocity (+1.5)
processEvent (KeyUp   (Keysym SDLK_a     _ _)) = modify $ modTri $ modifyAngVelocity (subtract 1.5)
processEvent (KeyDown (Keysym SDLK_d     _ _)) = modify $ modTri $ modifyAngVelocity (subtract 1.5)
processEvent (KeyUp   (Keysym SDLK_d     _ _)) = modify $ modTri $ modifyAngVelocity (+1.5)
processEvent (KeyDown (Keysym SDLK_MINUS _ _)) = modify $ modCamZoomDelta $ (+ zoomChangeFactor)
processEvent (KeyUp   (Keysym SDLK_MINUS _ _)) = modify $ modCamZoomDelta $ (subtract zoomChangeFactor)
processEvent (KeyDown (Keysym SDLK_PLUS  _ _)) = modify $ modCamZoomDelta $ (subtract zoomChangeFactor)
processEvent (KeyUp   (Keysym SDLK_PLUS  _ _)) = modify $ modCamZoomDelta $ (+ zoomChangeFactor)
processEvent (KeyDown (Keysym SDLK_i     _ _)) = do
  s <- State.get
  liftIO . putStrLn $ "Zoom: " ++ show (camzoom s)
processEvent _                                 = return ()

processEvents :: [SDL.Event] -> StateT TestState IO ()
processEvents = mapM_ processEvent

isQuit :: [SDL.Event] -> Bool
isQuit = hasEvent isq
  where isq Quit = True
        isq (KeyDown (Keysym SDLK_q _ _)) = True
        isq _ = False

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx n = if mn > n then mn else if mx < n then mx else n

loop :: StateT TestState IO ()
loop = do 
  liftIO $ delay 10
  state <- State.get
  modify $ modCamZoom $ (+ (camzoomdelta state))
  modify $ modCamera $ setZoom $ clamp 1.0 250 $ (camzoom state) + (400 * (length2 $ velocity (tri state)))
  modify $ modCamera $ setCentre $ Entity.position (tri state)
  liftIO $ setCamera (camera state)
  liftIO $ drawGLScreen (tri state) (aobjects state)
  when (not (stopped state)) $ do
    modify $ modTri (updateEntity 1)
    modify $ modAObjects $ map (\a -> if orbitRadius a == 0 then a else modifyAngle (+ (10 * recip (orbitRadius a))) a)
  events <- liftIO $ pollAllSDLEvents
  let quit = isQuit events
  processEvents events
  when (not quit) loop

drawGLScreen :: Entity -> [AObject] -> IO ()
drawGLScreen ent aobjs = do
  clear [ColorBuffer,DepthBuffer]

  loadIdentity
  translate $ (\(x,y,z) -> Vector3 x y z) (Entity.position ent)
  rotate (Entity.rotation ent) $ Vector3 0 0 (1 :: GLdouble)
  (\(x,y,z) -> OpenGL.scale x y z) (Entity.scale ent)
  currentColor $= (Entity.color ent)
  renderPrimitive (primitive ent) $ forM_ (vertices ent) $ \(x,y,z) -> do
    vertex $ Vertex3 x y z
  
  forM_ aobjs $ \aobj -> do
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

trianglePoints :: [GLvector3]
trianglePoints =
   [ (0,    1,   0)
    ,(0.9, -1,   0)
    ,(0.0, -0.7, 0)
    ,(-0.9,-1,   0)
   ]
