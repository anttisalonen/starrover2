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
  , aobjects     :: [Entity]
  , camera       :: Camera
  , camzoom      :: GLdouble
  , camzoomdelta :: GLdouble
  , stopped      :: Bool
  }

-- TODO: generate mod-functions using TH
modTri :: (Entity -> Entity) -> TestState -> TestState
modTri f t = t{tri = f (tri t)}

modAObjects :: ([Entity] -> [Entity]) -> TestState -> TestState
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

initState :: TestState
initState = TestState 
    (newEntity glVector3Null (Color4 0.0 0.5 0.0 1.0) Triangles trianglePoints glVector3AllUnit)
    [(modifyPosition (*+* (0.0, 14.0, 0.0)) (newEntity glVector3Null (Color4 0.5 0.5 1.0 1.0) Polygon (circlePoints 16) (glVector3AllUnit *** 3.0)))
    , (modifyPosition (*+* (-12.0, -34.0, 0.0)) (newEntity glVector3Null (Color4 0.9 0.0 0.0 1.0) Polygon (circlePoints 16) (glVector3AllUnit *** 3.0)))
    , (modifyPosition (*+* (32.0,  90.0, 0.0)) (newEntity glVector3Null (Color4 0.6 0.6 0.6 1.0) Polygon (circlePoints 16) (glVector3AllUnit *** 3.0)))
    , (modifyPosition (*+* ( 4.0, -78.0, 0.0)) (newEntity glVector3Null (Color4 0.0 0.4 0.5 1.0) Polygon (circlePoints 16) (glVector3AllUnit *** 3.0)))
    ]
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
processEvent (KeyDown (Keysym SDLK_MINUS _ _)) = modify $ modCamZoomDelta $ (+ zoomChangeFactor)
processEvent (KeyUp   (Keysym SDLK_MINUS _ _)) = modify $ modCamZoomDelta $ (subtract zoomChangeFactor)
processEvent (KeyDown (Keysym SDLK_PLUS  _ _)) = modify $ modCamZoomDelta $ (subtract zoomChangeFactor)
processEvent (KeyUp   (Keysym SDLK_PLUS  _ _)) = modify $ modCamZoomDelta $ (+ zoomChangeFactor)
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
  modify $ modCamZoom $ (max 1.0) . (+ (camzoomdelta state))
  modify $ modCamera $ setZoom $ (camzoom state) + (100 * (length2 $ velocity (tri state)))
  modify $ modCamera $ setCentre $ Entity.position (tri state)
  liftIO $ setCamera (camera state)
  liftIO $ drawGLScreen (tri state : aobjects state)
  when (not (stopped state)) $ do
    modify $ modTri (updateEntity 1)
    modify $ modAObjects (map $ updateEntity 1)
  when (stopped state) $ do
    liftIO $ putStrLn $ "cam zoom: " ++ show (camzoom state)
    liftIO $ putStrLn $ "cam: " ++ show (camera state)
    liftIO $ putStrLn $ "pos: " ++ show (Entity.position (tri state))
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
    rotate (Entity.rotation ent) $ Vector3 0 0 (1 :: GLdouble)
    (\(x,y,z) -> OpenGL.scale x y z) (Entity.scale ent)
    currentColor $= (Entity.color ent)
    renderPrimitive (primitive ent) $ forM_ (vertices ent) $ \(x,y,z) -> do
      vertex $ Vertex3 x y z
  
  glSwapBuffers

trianglePoints :: [GLvector3]
trianglePoints =
   [ (0,1, 0)
    ,(-1,-1, 0)
    ,(1,-1, 0)]

circlePoints :: Int -> [GLvector3]
circlePoints n = 
  let xs = map (sin . (2 * pi *) . (/(fromIntegral n)) . fromIntegral) [0..(n - 1)]
      ys = map (cos . (2 * pi *) . (/(fromIntegral n)) . fromIntegral) [0..(n - 1)]
      zs = repeat 0
  in zip3 xs ys zs

