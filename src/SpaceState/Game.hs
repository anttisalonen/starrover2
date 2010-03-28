module SpaceState.Game
where

import Data.List
import Control.Monad
import Control.Monad.State as State
import Prelude hiding (catch)

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import Graphics.Rendering.FTGL as FTGL
import qualified Data.Edison.Assoc.StandardMap as M

import OpenGLUtils
import Entity
import Camera
import AObject
import Combat
import Space
import Cargo
import Utils
import TextScreen
import Politics
import Universe
import Mission
import SpaceState.Difficulty

startState :: String -> Difficulty -> Font -> Font -> SpaceState
startState plname d f f2 = SpaceState 
    (newStdShipEntity (50.0, 30.0, 0.0) playerShipColor 0)
    aobjs
    stdCamera
    False
    f
    f2
    M.empty
    maxHold
    startCash
    ("", M.empty)
    0
    3
    startPlHealth
    d
    initialAttitudes
    emptyMissionMap
    plname
    Nothing

data SpaceState = SpaceState {
    tri            :: Entity
  , aobjects       :: [AObject]
  , camstate       :: CameraState
  , stopped        :: Bool
  , gamefont       :: Font
  , monofont       :: Font
  , plcargo        :: Cargo
  , plholdspace    :: Int
  , plcash         :: Int
  , lastmarket     :: (String, Market)
  , points         :: Int
  , lives          :: Int
  , plhealth       :: Int
  , difficulty     :: Difficulty
  , allegattitudes :: AttitudeMap
  , plmissions     :: MissionMap
  , playername     :: String
  , availmission   :: Maybe Mission
  }

-- TODO: generate mod-functions using TH
modTri :: (Entity -> Entity) -> SpaceState -> SpaceState
modTri f t = t{tri = f (tri t)}

modAObjects :: ([AObject] -> [AObject]) -> SpaceState -> SpaceState
modAObjects f t = t{aobjects = f (aobjects t)}

modCameraState :: (CameraState -> CameraState) -> SpaceState -> SpaceState
modCameraState f t = t{camstate = f (camstate t)}

modStopped :: (Bool -> Bool) -> SpaceState -> SpaceState
modStopped f t = t{stopped = f (stopped t)}

modPlCargo :: (Cargo -> Cargo) -> SpaceState -> SpaceState
modPlCargo f t = t{plcargo = f (plcargo t)}

modPlCash :: (Int -> Int) -> SpaceState -> SpaceState
modPlCash f t = t{plcash = f (plcash t)}

modPlHoldspace :: (Int -> Int) -> SpaceState -> SpaceState
modPlHoldspace f t = t{plholdspace = f (plholdspace t)}

modMarket :: ((String, Market) -> (String, Market)) -> SpaceState -> SpaceState
modMarket f t = t{lastmarket = f (lastmarket t)}

modPoints :: (Int -> Int) -> SpaceState -> SpaceState
modPoints f t = t{points = f (points t)}

modLives :: (Int -> Int) -> SpaceState -> SpaceState
modLives f t = t{lives = f (lives t)}

modPlHealth :: (Int -> Int) -> SpaceState -> SpaceState
modPlHealth f t = t{plhealth = f (plhealth t)}

modAllegAttitudes :: (AttitudeMap -> AttitudeMap) -> SpaceState -> SpaceState
modAllegAttitudes f t = t{allegattitudes = f (allegattitudes t)}

modPlMissions :: (MissionMap -> MissionMap) -> SpaceState -> SpaceState
modPlMissions f t = t{plmissions = f (plmissions t)}

modAvailMission :: (Maybe Mission -> Maybe Mission) -> SpaceState -> SpaceState
modAvailMission f t = t{availmission = f (availmission t)}

stdCamera :: CameraState
stdCamera = CameraState 
      ((-0.01 * width, -0.01 * height), (0.02 * width, 0.02 * height))
      100
      0

zoomChangeFactor :: (Floating a) => a
zoomChangeFactor = 1.0

drawSpace :: StateT SpaceState IO ()
drawSpace = do
  state <- State.get
  modify $ modCameraState $ modCamZoom $ (+ (camzoomdelta $ camstate state))
  modify $ modCameraState $ modCamera $ setZoom $ clamp 30 250 $ (camzoom $ camstate state) + (400 * (length2 $ velocity (tri state)))
  modify $ modCameraState $ modCamera $ setCentre $ Entity.position (tri state)
  liftIO $ setCamera (camera $ camstate state)
  liftIO $ drawGLScreen [tri state] (aobjects state)

gameoverText :: String
gameoverText = 
  intercalate "\n" 
    ["After escaping with your emergency capsule and",
     "returning to civilization, you realize your",
     "adventurous days are over."]

gameOver :: String -> String -> StateT SpaceState IO Bool
gameOver s s2 = do
  state <- State.get
  pressKeyScreen 
      (liftIO $ makeTextScreen (100, 500) 
          [(gamefont state, Color4 1.0 0.2 0.2 1.0, s ++ "\n\n" ++ s2 ++ "\n" ++ "\n"),
           (gamefont state, Color4 1.0 1.0 1.0 1.0, "Total points: " ++ show (finalPoints state)),
           (gamefont state, Color4 1.0 0.2 0.2 1.0, "Press ENTER to continue")]
          (return ())) SDLK_RETURN
  return True

maxHold = holdspace intermediate
startPlHealth = maxhealth intermediate
startCash = 10

finalPoints :: SpaceState -> Int
finalPoints s = points s + plcash s + (lives s * 50)

allegAttitude :: String -> SpaceState -> Int
allegAttitude planetname state = 
  attitude (planetNameToAllegiance (aobjects state) planetname) $ 
     allegattitudes state

