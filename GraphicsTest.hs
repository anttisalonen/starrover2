{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import System.Exit
import System.Environment
import Control.Monad

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL

step = (0.05, 0.0, 0.0)
width = 800
height = 600

(*+*) :: GLvector3 -> GLvector3 -> GLvector3
(*+*) (x0, y0, z0) (x1, y1, z1) = (x0 + x1, y0 + y1, z0 + z1)

(*-*) :: GLvector3 -> GLvector3 -> GLvector3
(*-*) (x0, y0, z0) (x1, y1, z1) = (x0 - x1, y0 - y1, z0 - z1)

glVector3Null :: GLvector3
glVector3Null = (0, 0, 0)

main = withInit [InitVideo] $ do
  progName <- getProgName
  blendEquation $= FuncAdd
  blendFunc $= (OpenGL.SrcAlpha, OneMinusSrcAlpha)
  createAWindow progName

createAWindow name = do
  setVideoMode width height 0 [OpenGL]
  depthFunc $= Just Less
  clearColor $= Color4 1 1 1 1
  viewport $= (Position 0 0, Size width height)
  matrixMode $= Projection
  loadIdentity
  ortho ((-1) * width * 0.01) (1 * width * 0.01) ((-1) * height * 0.01) (1 * height * 0.01) (-10) 10
  matrixMode $= Modelview 0
  loop glVector3Null glVector3Null False

loop rtri rquad stopped = do 
  delay 10
  drawGLScreen rtri rquad
  let rtri' = if stopped
                then rtri
                else rtri *+* step
  let rquad' = if stopped
                then rquad
                else rquad *-* step
  event <- pollEvent
  quit <- case event of
            Quit                        -> return True
            KeyDown (Keysym SDLK_q _ _) -> return True
            _                           -> return False
  let stopped' = case event of
            KeyDown (Keysym SDLK_SPACE _ _) -> True
            KeyUp (Keysym SDLK_SPACE _ _)   -> False
            _                               -> stopped
  when (not quit) (loop rtri' rquad' stopped')

type GLvector3 = (GLfloat, GLfloat, GLfloat)

drawGLScreen :: GLvector3 -> GLvector3 -> IO ()
drawGLScreen rtri rquad = do
  clear [ColorBuffer,DepthBuffer]

  loadIdentity
  translate $ (\(x,y,z) -> Vector3 x y z) rtri
  -- rotate rtri $ Vector3 0 0 (1::GLfloat)
  forM_ polygonPoints $ \l -> do
    renderPrimitive Polygon $ mapM_ (\((r,g,b),(x,y,z)) -> do
      currentColor $= Color4 r g b 1
      vertex$Vertex3 x y z) l
  
  loadIdentity
  translate $ (\(x,y,z) -> Vector3 x y z) rquad
  -- rotate rquad $ Vector3 0 0 (1::GLfloat)
  currentColor $= Color4 0.5 0.5 1 0.5
  renderPrimitive Quads $ forM_ quadsPoints $ \((r,g,b),l) -> do
    currentColor $= Color4 r g b 0.5
    mapM_ (\(x,y,z) -> vertex$Vertex3 x y z) l
  
  glSwapBuffers

polygonTrans :: (GLfloat,GLfloat,GLfloat)
polygonTrans = (-1.5,0,-9)

polygonPoints :: [[((GLfloat,GLfloat,GLfloat),(GLfloat,GLfloat,GLfloat))]]
polygonPoints =
  [ 
   [ ((0,0.5,0.5),(0,1, 0))
    ,((0,1.0,0.0),(-1,-1, 0))
    ,((0,1.0,0.0),(1,-1, 0))]
  ]

quadsTrans :: (GLfloat,GLfloat,GLfloat)
quadsTrans = (1.5,0,-6)

quadsPoints :: [((GLfloat,GLfloat,GLfloat),[(GLfloat,GLfloat,GLfloat)])]
quadsPoints =
  [
  ((1,0,0),  [(1,1,1)  ,(-1,1,1)  ,(-1,-1,1) ,(1,-1,1) ])
  ]
