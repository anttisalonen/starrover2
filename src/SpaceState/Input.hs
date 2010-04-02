module SpaceState.Input(handleEvents)
where

import Data.List hiding (maximum)
import Data.Foldable
import Control.Monad hiding (forM_)
import Control.Monad.State as State hiding (forM_)
import Prelude hiding (catch, until, maximum)

import Graphics.UI.SDL as SDL

import Utils
import Space
import OpenGLUtils
import Entity
import Camera
import AObject
import SDLUtils
import SpaceState.Common
import SpaceState.Game

inputMapping
  :: [(SDLKey,
       (StateT SpaceState IO (),
        StateT SpaceState IO ()))]
inputMapping = 
  [ (SDLK_w,     (accelerate 0.002,    accelerate 0))
  , (SDLK_s,     (accelerate (-0.002), accelerate 0))
  , (SDLK_a,     (turn 1.5, setTurn 0))
  , (SDLK_d,     (turn (-1.5), setTurn 0))
  , (SDLK_UP,    (accelerate 0.002, accelerate 0))
  , (SDLK_DOWN,  (accelerate (-0.002), accelerate 0))
  , (SDLK_LEFT,  (turn 1.5, setTurn 0))
  , (SDLK_RIGHT, (turn (-1.5), setTurn 0))
  , (SDLK_MINUS, (changeZoom zoomChangeFactor, setZoomDelta 0))
  , (SDLK_PLUS,  (changeZoom (-zoomChangeFactor), setZoomDelta 0))
  , (SDLK_i,     (showInfo, return ()))
  , (SDLK_p,     (modify $ modStopped not, return ()))
  , (SDLK_m,     (showMap, return ()))
  ]

showMap :: StateT SpaceState IO ()
showMap = do
  state <- State.get
  let maxrad = maximum $ fmap orbitRadius $ aobjects state
  let ((minx', maxx'), (miny', maxy')) = boxThatIncludes (-maxrad, maxrad) (-maxrad, maxrad) 10 10 width height
  let objrad = 0.01 * (min (maxx' - minx') (maxy' - miny'))
  liftIO $ until anyKeyOrMouseWasPressedIO $ 
    inOrthoBoxDraw minx' maxx' miny' maxy' (-10) 10 $ do
      delay 10
      drawGLScreen (Just objrad) [tri state] (aobjects state)

showInfo = do
  s <- State.get
  liftIO . putStrLn $ "Zoom: " ++ show (camzoom $ camstate s)
  liftIO . putStrLn $ "Player position: " ++ show (Entity.position $ tri s)
  forM_ (aobjects s) $ \aobj -> do
    liftIO . putStrLn $ "Astronomical body position: " ++ show (AObject.getPosition aobj)
  liftIO . putStrLn $ show $ allegattitudes s

handleEvents :: StateT SpaceState IO Bool
handleEvents = do
  events <- liftIO $ pollAllSDLEvents
  processEvents inputMapping events
  return $ isQuit events


