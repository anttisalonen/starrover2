{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import System.Exit
import System.Environment
import Control.Monad

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL

step = 0.5

main = withInit [InitVideo] $ do
  progName <- getProgName
  blendEquation $= FuncAdd
  blendFunc $= (OpenGL.SrcAlpha, OneMinusSrcAlpha)
  createAWindow progName

createAWindow name
    = do setVideoMode width height 0 [OpenGL]
         depthFunc $= Just Less
         clearColor $= Color4 1 1 1 1
         viewport $= (Position 0 0, Size width height)
         matrixMode $= Projection
         loadIdentity
         ortho ((-1) * w * 0.01) (1 * w * 0.01) ((-1) * h * 0.01) (1 * h * 0.01) (-10) 10
         matrixMode $= Modelview 0
         let loop rtri rquad stopped = do 
                       delay 10
                       (rtri', rquad') <- drawGLScreen rtri rquad stopped
                       event <- pollEvent
                       quit <- case event of
                                 Quit -> return True
                                 KeyDown (Keysym SDLK_q _ _) -> return True
                                 _    -> return False
                       let stopped' = case event of
                                 KeyDown (Keysym SDLK_SPACE _ _) -> True
                                 KeyUp (Keysym SDLK_SPACE _ _)   -> False
--                                  VideoResize w h -> resizeGLScene w h >> return False
                                 _ -> stopped
                       when (not quit) (loop rtri' rquad' stopped')
         loop 0 0 False
      where width = 800
            height = 600
            (w,h) = (fromIntegral width, fromIntegral height)

resizeGLScene w h = do
  setVideoMode w h 0 [OpenGL,Resizable]
  viewport $= (Position 0 0,Size (fromIntegral w) (fromIntegral h))
  matrixMode $= Projection
  loadIdentity
  let radio = (fromIntegral w)/(fromIntegral h)
  perspective 45 radio 0.1 100
  matrixMode $= Modelview 0

drawGLScreen :: GLfloat -> GLfloat -> Bool -> IO (GLfloat, GLfloat)
drawGLScreen rtri rquad stopped = do
  clear [ColorBuffer,DepthBuffer]
  loadIdentity

  translate $ (\(x,y,z) -> Vector3 x y z) polygonTrans
  rotate rtri $ Vector3 0 0 (1::GLfloat)
  let rtri' = if stopped
                then rtri
                else rtri + step
  forM_ polygonPoints $ \l -> do
    renderPrimitive Polygon $ mapM_ (\((r,g,b),(x,y,z)) -> do
      currentColor $= Color4 r g b 1
      vertex$Vertex3 x y z) l
  
  loadIdentity
  translate $ (\(x,y,z) -> Vector3 x y z) quadsTrans
  rotate rquad $ Vector3 0 0 (1::GLfloat)
  let rquad' = if stopped
                 then rquad
                 else rquad - step
  currentColor $= Color4 0.5 0.5 1 0.5
  renderPrimitive Quads $ forM_ quadsPoints $ \((r,g,b),l) -> do
    currentColor $= Color4 r g b 0.5
    mapM_ (\(x,y,z) -> vertex$Vertex3 x y z) l
  
  glSwapBuffers
  return (rtri', rquad')

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