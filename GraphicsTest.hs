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
  event <- liftIO $ pollEvent
  quit <- case event of
            Quit                        -> return True
            KeyDown (Keysym SDLK_q _ _) -> return True
            _                           -> return False
  let stopped' = case event of
            KeyDown (Keysym SDLK_SPACE _ _) -> True
            KeyUp (Keysym SDLK_SPACE _ _)   -> False
            _                               -> (stopped state)
  put (TestState rtri' rquad' stopped')
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
