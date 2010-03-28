module SpaceState.Common
where

import Data.List
import Control.Monad.State as State
import Prelude hiding (catch)

import Entity
import Camera
import SpaceState.Game

releaseKeys :: StateT SpaceState IO ()
releaseKeys = do
  setTurn 0
  accelerate 0 -- prevent involuntary actions
  setZoomDelta 0

recoveryText :: String
recoveryText = 
  intercalate "\n" 
    ["After ejecting from your space ship, you drifted in space",
     "until a friendly alien picked you up and took you with him",
     "to a nearby trading post, where you recovered some of your",
     "strength.",
     "",
     "After a long search, you manage to find a used ",
     "space ship, and embark on a new adventure..."]

-- accelerate :: (MonadState SpaceState m) => GLdouble -> m ()
accelerate a = modify $ modTri $ modifyAcceleration (const (0.0,  a, 0.0))

-- turn :: (MonadState SpaceState m) => GLdouble -> m ()
turn a = modify $ modTri $ modifyAngVelocity (+a)
setTurn a = modify $ modTri $ modifyAngVelocity (const a)

changeZoom a = modify $ modCameraState $ modCamZoomDelta (+a)
setZoomDelta a = modify $ modCameraState $ modCamZoomDelta (const a)


