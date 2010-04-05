module TextScreen(loopTextScreen,
  makeTextScreen,
  drawButton,
  pressOneOfScreen,
  pressKeyScreen,
  pressAnyKeyScreen,
  menu)
where

import Data.List
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

drawButton :: Maybe (String, Font) -> ((GLdouble, GLdouble), (GLdouble, GLdouble)) -> IO ()
drawButton ms ((tlx, tly), (diffx, diffy)) = do
  loadIdentity
  translate $ Vector3 tlx (tly + 2) (0 :: GLdouble)
  currentColor $= Color4 1.0 1.0 1.0 1.0
  renderPrimitive LineLoop $
    mapM_ vertex [Vertex3 (0 :: GLdouble) 0 0, Vertex3 0 diffy 0, Vertex3 diffx diffy 0, Vertex3 diffx 0 0]
  case ms of
    Nothing -> return ()
    Just (str, f) -> do
      translate $ Vector3 10 8 (0 :: GLdouble)
      renderFont f str FTGL.Front

menu :: Int -> (Font, Color4 GLfloat, String) -> [(Font, Color4 GLfloat, String)] -> (Font, Color4 GLfloat, String) -> IO Int
menu defval (titlef, titlec, titlestr) options cursor = do
  let numitems = length options
  let title = (titlef, titlec, titlestr ++ replicate (4 - length (lines titlestr)) '\n')
  let drawCursor n = liftIO $ writeLine (130, 400 - 50 * fromIntegral n) cursor
  let buttoncoords :: (Num a) => [((a, a), (a, a))]
      buttoncoords = [((180, 385 - 50 * fromIntegral i), (300, 40)) | i <- [1..numitems]]
  let drawButtons = forM_ buttoncoords $ \b -> do
      drawButton Nothing b
  if numitems == 0
    then return 0
    else do
      n <- Prelude.flip evalStateT (clamp 1 numitems defval) $ do
        let drawfunc = do n <- State.get 
                          liftIO $ makeTextScreen (200, 500)
                            (title:options)
                            (drawButtons >> drawCursor n)
        let getInput = do
              evts <- liftIO $ pollAllSDLEvents
              when (keyWasPressed SDLK_DOWN evts) $
                modify (\p -> min numitems $ p + 1)
              when (keyWasPressed SDLK_UP evts) $
                modify (\p -> max 1 $ p - 1)
              if oneofKeyWasPressed [SDLK_RETURN, SDLK_SPACE] evts
                then State.get >>= return . Just 
                else do
                  let mbutton = mouseClickInAny height [ButtonLeft] buttoncoords evts
                  return $ mbutton >>= (fmap . fmap) (+1) (Prelude.flip elemIndex buttoncoords)
        loopTextScreen drawfunc getInput
      return n

pressOneOfScreen :: (MonadIO m) => m () -> [SDLKey] -> m SDLKey
pressOneOfScreen scr keys = 
  loopTextScreen scr
     (liftIO $ pollAllSDLEvents >>= return . specificKeyPressed keys)

pressKeyScreen :: (MonadIO m) => m () -> SDLKey -> m ()
pressKeyScreen scr k =
  loopTextScreen scr
     (liftIO $ pollAllSDLEvents >>= return . boolToMaybe . keyWasPressed k)

pressAnyKeyScreen :: (MonadIO m) => m () -> m ()
pressAnyKeyScreen scr =
  loopTextScreen scr
     (liftIO $ pollAllSDLEvents >>= return . boolToMaybe . anyKeyOrMouseWasPressed)

