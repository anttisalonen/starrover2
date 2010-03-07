{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import System.Exit
import System.Environment
import Control.Monad
import Control.Monad.State as State

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL

-- generic stuff
(*+*) :: GLvector3 -> GLvector3 -> GLvector3
(*+*) (x0, y0, z0) (x1, y1, z1) = (x0 + x1, y0 + y1, z0 + z1)

(*-*) :: GLvector3 -> GLvector3 -> GLvector3
(*-*) (x0, y0, z0) (x1, y1, z1) = (x0 - x1, y0 - y1, z0 - z1)

glVector3Null :: GLvector3
glVector3Null = (0, 0, 0)

type GLvector3 = (GLfloat, GLfloat, GLfloat)

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
    rtri    :: GLvector3
  , rquad   :: GLvector3
  , stopped :: Bool
  }

main = withInit [InitVideo] $ do
  blendEquation $= FuncAdd
  blendFunc $= (OpenGL.SrcAlpha, OneMinusSrcAlpha)
  createAWindow 

initState :: TestState
initState = TestState glVector3Null glVector3Null False

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

-- TODO: generate mod-functions using TH
modRtri :: (GLvector3 -> GLvector3) -> TestState -> TestState
modRtri f t = t{rtri = f (rtri t)}

modRquad :: (GLvector3 -> GLvector3) -> TestState -> TestState
modRquad f t = t{rquad = f (rquad t)}

modStopped :: (Bool -> Bool) -> TestState -> TestState
modStopped f t = t{stopped = f (stopped t)}

-- TODO: figure out how to make this a State TestState ()
processEvent :: SDL.Event -> StateT TestState IO ()
processEvent (KeyDown (Keysym SDLK_SPACE _ _)) = modify (modStopped $ const True)
processEvent (KeyUp   (Keysym SDLK_SPACE _ _)) = modify (modStopped $ const False)
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
  liftIO $ drawGLScreen (rtri state) (rquad state)
  let rtri' = if (stopped state)
                then (rtri state)
                else (rtri state) *+* step
  let rquad' = if (stopped state)
                then (rquad state)
                else (rquad state) *-* step
  events <- liftIO $ pollAllSDLEvents
  let quit = isQuit events
  processEvents events
  modify (\t -> t{rtri=rtri', rquad=rquad'})
  when (not quit) loop

drawGLScreen :: GLvector3 -> GLvector3 -> IO ()
drawGLScreen rtri rquad = do
  clear [ColorBuffer,DepthBuffer]

  loadIdentity
  translate $ (\(x,y,z) -> Vector3 x y z) rtri
  currentColor $= Color4 0.0 0.5 0.0 1.0
  renderPrimitive Triangles $ forM_ polygonPoints $ \(x,y,z) -> do
    vertex $ Vertex3 x y z
  
  loadIdentity
  translate $ (\(x,y,z) -> Vector3 x y z) rquad
  currentColor $= Color4 0.5 0.5 1 1.0
  renderPrimitive Quads $ forM quadsPoints $ \(x,y,z) -> do
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
