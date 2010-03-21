module TextScreen(loopTextScreen,
  makeTextScreen,
  drawButton,
  menu)
where

import Control.Monad
import Control.Monad.State as State
import Prelude hiding (catch)

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import Graphics.Rendering.FTGL as FTGL

import Camera
import Space
import SDLUtils
import Utils

writeLine :: (GLdouble, GLdouble) -> (Font, Color4 GLfloat, String) -> IO ()
writeLine (xstart, ystart) (f, c, s) = do
  loadIdentity
  translate (Vector3 xstart ystart (0 :: GLdouble))
  currentColor $= c
  forM_ (lines s) $ \str -> do
    renderFont f str FTGL.Front
    translate (Vector3 0 (-50) (0 :: GLdouble))

makeTextScreen :: (GLdouble, GLdouble) -> [(Font, Color4 GLfloat, String)] -> IO () -> IO ()
makeTextScreen (xstart, ystart) instructions additional = do
  clear [ColorBuffer,DepthBuffer]
  loadIdentity
  setCamera ((0, 0), (width, height))
  translate (Vector3 xstart ystart (0 :: GLdouble))
  forM_ instructions $ \(f, c, s) -> do
    currentColor $= c
    forM_ (lines s) $ \str -> do
      renderFont f str FTGL.Front
      translate (Vector3 0 (-50) (0 :: GLdouble))
  additional
  glSwapBuffers

-- TODO: write another version of this based on FPS
-- and another one with m Bool as event function
loopTextScreen :: (MonadIO m) => m () -> m (Maybe a) -> m a
loopTextScreen drawScreenFunc handleEventsFunc = untilDoneR $ do
  liftIO $ delay 10
  drawScreenFunc
  handleEventsFunc

drawButton :: String -> Font -> ((GLdouble, GLdouble), (GLdouble, GLdouble)) -> IO ()
drawButton str f ((tlx, tly), (diffx, diffy)) = do
  loadIdentity
  translate $ Vector3 tlx (tly + 2) (0 :: GLdouble)
  currentColor $= Color4 1.0 1.0 1.0 1.0
  renderPrimitive LineLoop $
    mapM_ vertex [Vertex3 (0 :: GLdouble) 0 0, Vertex3 0 diffy 0, Vertex3 diffx diffy 0, Vertex3 diffx 0 0]
  translate $ Vector3 10 8 (0 :: GLdouble)
  renderFont f str FTGL.Front

menu :: (Font, Color4 GLfloat, String) -> [(Font, Color4 GLfloat, String)] -> (Font, Color4 GLfloat, String) -> IO Int
menu (titlef, titlec, titlestr) options cursor = do
  let numitems = length options
  let title = (titlef, titlec, titlestr ++ "\n\n\n")
  if numitems == 0
    then return 0
    else do
      n <- Prelude.flip evalStateT (1 :: Int) $ do
        let drawfunc = do n <- State.get 
                          liftIO $ makeTextScreen (200, 500)
                            (title:options)
                            (liftIO $ writeLine (150, 400 - 50 * fromIntegral n) cursor)
        let getInput = do
              evts <- liftIO $ pollAllSDLEvents
              when (keyWasPressed SDLK_DOWN evts) $
                modify (\p -> min numitems $ p + 1)
              when (keyWasPressed SDLK_UP evts) $
                modify (\p -> max 1 $ p - 1)
              if oneofKeyWasPressed [SDLK_RETURN, SDLK_SPACE] evts
                then State.get >>= return . Just 
                else return Nothing
        loopTextScreen drawfunc getInput
      return n

