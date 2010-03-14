module TextScreen
where

import Control.Monad
import Control.Monad.State as State
import Prelude hiding (catch)

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import Graphics.Rendering.FTGL as FTGL

import Camera
import Space
import Utils

makeTextScreen :: [(Font, Color4 GLfloat, String)] -> IO () -> IO ()
makeTextScreen instructions additional = do
  clear [ColorBuffer,DepthBuffer]
  loadIdentity
  setCamera ((0, 0), (width, height))
  translate (Vector3 100 400 (0 :: GLdouble))
  forM_ instructions $ \(f, c, s) -> do
    currentColor $= c
    forM_ (lines s) $ \str -> do
      renderFont f str FTGL.Front
      translate (Vector3 0 (-50) (0 :: GLdouble))
  additional
  glSwapBuffers

loopTextScreen :: (MonadIO m) => m () -> m (Maybe a) -> m a
loopTextScreen drawScreenFunc handleEventsFunc = untilDoneR $ do
  liftIO $ delay 10
  drawScreenFunc
  handleEventsFunc

drawExitButton :: Font -> IO ()
drawExitButton f = do
  loadIdentity
  translate $ Vector3 100 (100) (0 :: GLdouble)
  currentColor $= Color4 1.0 1.0 1.0 1.0
  renderPrimitive LineLoop $
    mapM_ vertex [Vertex3 (0 :: GLdouble) 0 0, Vertex3 0 30 0, Vertex3 100 30 0, Vertex3 100 0 0]
  translate $ Vector3 10 10 (0 :: GLdouble)
  renderFont f "Exit" FTGL.Front


