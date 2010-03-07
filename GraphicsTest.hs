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

type Camera = ((GLdouble, GLdouble), (GLdouble, GLdouble))

data TestState = TestState {
    tri         :: Entity
  , quad        :: Entity
  , camera      :: Camera
  , camzoom     :: GLdouble
  , stopped     :: Bool
  }

-- TODO: generate mod-functions using TH
modTri :: (Entity -> Entity) -> TestState -> TestState
modTri f t = t{tri = f (tri t)}

modQuad :: (Entity -> Entity) -> TestState -> TestState
modQuad f t = t{quad = f (quad t)}

modCamera :: (Camera -> Camera) -> TestState -> TestState
modCamera f t = t{camera = f (camera t)}

modCamZoom :: (GLdouble -> GLdouble) -> TestState -> TestState
modCamZoom f t = t{camzoom = f (camzoom t)}

modStopped :: (Bool -> Bool) -> TestState -> TestState
modStopped f t = t{stopped = f (stopped t)}

main = withInit [InitVideo] $ do
  blendEquation $= FuncAdd
  blendFunc $= (OpenGL.SrcAlpha, OneMinusSrcAlpha)
  createAWindow 

initState :: TestState
initState = TestState 
    (newEntity glVector3Null (Color4 0.0 0.5 0.0 1.0) Triangles)
    (modifyPosition (*+* (0.0, 4.0, 0.0)) (newEntity glVector3Null (Color4 0.5 0.5 1.0 1.0) Quads))
    ((-0.01 * width, -0.01 * height), (0.02 * width, 0.02 * height))
    1
    False

setCamera :: Camera -> IO ()
setCamera ((minx, miny), (diffx, diffy)) = do
  matrixMode $= Projection
  loadIdentity
  ortho minx (minx + diffx) miny (miny + diffy) (-10) 10
  matrixMode $= Modelview 0

changeZoom :: GLdouble -> Camera -> Camera
changeZoom z ((minx, miny), (diffx, diffy)) =
  let ndiffx = diffx * z
      ndiffy = diffy * z
      ocent = (minx + diffx / 2, miny + diffy / 2, 0)
  in setCentre ocent ((0, 0), (ndiffx, ndiffy))

setCentre :: GLvector3 -> Camera -> Camera
setCentre (nx, ny, _) ((_, _), (diffx, diffy)) =
  ((nx - diffx / 2, ny - diffy / 2), (diffx, diffy))

createAWindow = do
  setVideoMode width height 0 [OpenGL]
  depthFunc $= Just Less
  clearColor $= Color4 0 0 0 1
  viewport $= (Position 0 0, Size width height)
  setCamera (camera initState)
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
processEvent (KeyDown (Keysym SDLK_PLUS  _ _)) = modify $ modCamZoom $ (+ 0.015)
processEvent (KeyUp   (Keysym SDLK_PLUS  _ _)) = modify $ modCamZoom $ (subtract 0.015)
processEvent (KeyDown (Keysym SDLK_MINUS _ _)) = modify $ modCamZoom $ (subtract 0.015)
processEvent (KeyUp   (Keysym SDLK_MINUS _ _)) = modify $ modCamZoom $ (+ 0.015)
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
  modify $ modCamera $ changeZoom (camzoom state)
  modify $ modCamera $ setCentre $ Entity.position (tri state)
  liftIO $ setCamera (camera state)
  liftIO $ drawGLScreen [tri state, quad state]
  when (not (stopped state)) $ do
    modify $ modTri (updateEntity 1)
    modify $ modQuad (updateEntity 1)
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
