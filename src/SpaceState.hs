module SpaceState(runGame)
where

import System.Random
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State as State
import Prelude hiding (catch)

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import Graphics.Rendering.FTGL as FTGL
import qualified Data.Edison.Assoc.StandardMap as M

import OpenGLUtils
import Statistics
import Entity
import Camera
import AObject
import Combat
import Space
import Cargo
import Utils
import TextScreen
import SDLUtils

-- test scenario
data TestState = TestState {
    tri          :: Entity
  , aobjects     :: [AObject]
  , camstate     :: CameraState
  , stopped      :: Bool
  , gamefont     :: Font
  , monofont     :: Font
  , plcargo      :: Cargo
  , plholdspace  :: Int
  , plcash       :: Int
  , lastmarket   :: (String, Market)
  , points       :: Int
  , lives        :: Int
  }

-- TODO: generate mod-functions using TH
modTri :: (Entity -> Entity) -> TestState -> TestState
modTri f t = t{tri = f (tri t)}

modAObjects :: ([AObject] -> [AObject]) -> TestState -> TestState
modAObjects f t = t{aobjects = f (aobjects t)}

modCameraState :: (CameraState -> CameraState) -> TestState -> TestState
modCameraState f t = t{camstate = f (camstate t)}

modStopped :: (Bool -> Bool) -> TestState -> TestState
modStopped f t = t{stopped = f (stopped t)}

modPlCargo :: (Cargo -> Cargo) -> TestState -> TestState
modPlCargo f t = t{plcargo = f (plcargo t)}

modPlCash :: (Int -> Int) -> TestState -> TestState
modPlCash f t = t{plcash = f (plcash t)}

modPlHoldspace :: (Int -> Int) -> TestState -> TestState
modPlHoldspace f t = t{plholdspace = f (plholdspace t)}

modMarket :: ((String, Market) -> (String, Market)) -> TestState -> TestState
modMarket f t = t{lastmarket = f (lastmarket t)}

modPoints :: (Int -> Int) -> TestState -> TestState
modPoints f t = t{points = f (points t)}

modLives :: (Int -> Int) -> TestState -> TestState
modLives f t = t{lives = f (lives t)}

maxHold = holdspace intermediate
startCash = 10

aobjs =
  [ AObject "Star"       0   (Color4 0.9 0.0 0.0 1.0) 6.0 0
  , AObject "Murphy's"   10  (Color4 0.5 0.5 1.0 1.0) 2.0 28
  , AObject "Loki"       250 (Color4 0.0 0.4 0.5 1.0) 4.0 80
  , AObject "Harju"      30  (Color4 0.6 0.6 0.6 1.0) 3.0 100
  , AObject "Riesenland" 80  (Color4 0.6 0.6 0.6 1.0) 2.0 130
  ]

stdCamera :: CameraState
stdCamera = CameraState 
      ((-0.01 * width, -0.01 * height), (0.02 * width, 0.02 * height))
      100
      0

startState :: Font -> Font -> TestState
startState f f2 = TestState 
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

zoomChangeFactor :: (Floating a) => a
zoomChangeFactor = 1.0

-- accelerate :: (MonadState TestState m) => GLdouble -> m ()
accelerate a = modify $ modTri $ modifyAcceleration (const (0.0,  a, 0.0))

-- turn :: (MonadState TestState m) => GLdouble -> m ()
turn a = modify $ modTri $ modifyAngVelocity (+a)
setTurn a = modify $ modTri $ modifyAngVelocity (const a)

changeZoom a = modify $ modCameraState $ modCamZoomDelta (+a)
setZoomDelta a = modify $ modCameraState $ modCamZoomDelta (const a)

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
  ]

showInfo = do
  s <- State.get
  liftIO . putStrLn $ "Zoom: " ++ show (camzoom $ camstate s)
  liftIO . putStrLn $ "Player position: " ++ show (Entity.position $ tri s)
  forM_ (aobjects s) $ \aobj -> do
    liftIO . putStrLn $ "Astronomical body position: " ++ show (AObject.getPosition aobj)

initState :: StateT TestState IO ()
initState = do
  lc <- getRandomPlanet
  modify $ modTri $ modifyPosition (const $ (getPosition lc *+* (glVector3UnitX *** (AObject.size lc))))
  modify $ modPlCash $ const startCash
  modify $ modPlHoldspace $ const maxHold
  modify $ modPlCargo $ const M.empty
  gotoCity (aobjName lc)
  catapult (AObject.getPosition lc)
  releaseKeys

runGame :: Font -> Font -> IO Int
runGame f f2 = do
  let is = startState f f2
  setCamera (camera $ camstate is)
  evalStateT (do
    initState
    loop)
    is

loop :: StateT TestState IO Int
loop = untilDoneR $ do 
  liftIO $ delay 10
  state <- State.get
  drawSpace
  dead <- if stopped state
            then return False
            else updateSpaceState
  if not dead
    then do
      quits <- handleEvents
      if quits
        then gameOver "You decided to retire." "" >> die
        else return Nothing
    else die

finalPoints :: TestState -> Int
finalPoints s = points s + plcash s + (lives s * 50)

die :: StateT TestState IO (Maybe Int)
die = do
  state <- State.get
  return $ Just $ finalPoints state

handleEvents :: StateT TestState IO Bool
handleEvents = do
  events <- liftIO $ pollAllSDLEvents
  processEvents inputMapping events
  return $ isQuit events

gotoCity :: String -> StateT TestState IO ()
gotoCity planetname = do
  state <- State.get
  nmarket <- if planetname == fst (lastmarket state)
               then return $ lastmarket state
               else do
                 m <- liftIO $ randomMarket
                 return (planetname, m)
  let market = snd nmarket
  modify $ modMarket $ const nmarket
  (m', cargo', cash', hold') <- liftIO $ execStateT 
                                  (tradeScreen ("Landed on " ++ planetname) 
                                      (gamefont state) (monofont state)) 
                                  (market, plcargo state, plcash state, plholdspace state)
  modify $ modMarket $ modSnd $ const m'
  modify $ modPlCargo $ const cargo'
  modify $ modPlCash $ const cash'
  modify $ modPlHoldspace $ const hold'

catapult :: GLvector3 -> StateT TestState IO ()
catapult vec = do
  state <- State.get
  let plloc = Entity.position (tri state)
  let (dx, dy, _) = (plloc *-* vec)
  let newvel = OpenGLUtils.normalize (dx, dy, 0) *** 0.2
  modify $ modTri $ modifyPosition $ (*+* (newvel *** 5))
  modify $ modTri $ modifyVelocity $ const newvel
  modify $ modTri $ resetAcceleration
  modify $ modTri $ modifyRotation $ (+180)

updateSpaceState :: StateT TestState IO Bool
updateSpaceState = do
  state <- State.get
  modify $ modTri (updateEntity 1)
  modify $ modAObjects $ map (\a -> if orbitRadius a == 0 then a else modifyAngle (+ (10 * recip (orbitRadius a))) a)
  let mlanded = findCollisions (getShipBox $ tri state) (aobjects state)
  case mlanded of
    Nothing -> do
      val <- liftIO $ randomRIO (0, 500 :: Int)
      if (val == 0) 
        then startCombat
        else return False
    Just lc -> do
      if aobjName lc == "Star"
        then lostLife "You flew too close to the star!" recoveryText
        else do
          gotoCity (aobjName lc)
          catapult (AObject.getPosition lc)
          releaseKeys
          return False

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

gameoverText :: String
gameoverText = 
  intercalate "\n" 
    ["After escaping with your emergency capsule and",
     "returning to civilization, you realize your",
     "adventurous days are over."]

releaseKeys :: StateT TestState IO ()
releaseKeys = do
  setTurn 0
  accelerate 0 -- prevent involuntary actions
  setZoomDelta 0

getRandomPlanet :: StateT TestState IO AObject
getRandomPlanet = do
  state <- State.get
  n <- liftIO $ chooseIO (aobjects state)
  if (aobjName n == "Star")
    then getRandomPlanet
    else return n

lostLife :: String -> String -> StateT TestState IO Bool
lostLife s1 s2 = do
  modify $ modLives pred
  modify $ modPlCash $ const 0
  modify $ modPlHoldspace $ const maxHold
  modify $ modPlCargo $ const M.empty
  state <- State.get
  if lives state <= 0
    then do
      gameOver s1 gameoverText
    else do
      loopTextScreen (liftIO $ makeTextScreen (30, 550) [(gamefont state,
                         Color4 1.0 0.2 0.2 1.0, s1 ++ "\n\n" ++ s2 ++ "\n\nPress ENTER to continue")] 
                         (return ()))
                     (liftIO $ pollAllSDLEvents >>= return . boolToMaybe . keyWasPressed SDLK_RETURN)
      initState
      return False

gameOver :: String -> String -> StateT TestState IO Bool
gameOver s s2 = do
  state <- State.get
  loopTextScreen 
      (liftIO $ makeTextScreen (100, 500) 
          [(gamefont state, Color4 1.0 0.2 0.2 1.0, s ++ "\n\n" ++ s2 ++ "\n" ++ "\n"),
           (gamefont state, Color4 1.0 1.0 1.0 1.0, "Total points: " ++ show (finalPoints state)),
           (gamefont state, Color4 1.0 0.2 0.2 1.0, "Press ENTER to continue")]
          (return ()))
      (liftIO $ pollAllSDLEvents >>= return . boolToMaybe . keyWasPressed SDLK_RETURN)
  return True

startCombat :: StateT TestState IO Bool
startCombat = do
  state <- State.get
  en <- liftIO $ randomEnemy
  c <- loopTextScreen (liftIO $ makeTextScreen (100, 400) 
                         [(gamefont state, Color4 1.0 1.0 1.0 1.0, 
                           concat ["You spot another ship traveling nearby.\n",
                                   "It seems to be a " ++ (describeEnemy en) ++ ".\n",
                                   "Press ENTER to start a battle against the foreign ship\n",
                                   "or ESCAPE to escape"])]
                          (return ()))
                      (liftIO $ pollAllSDLEvents >>= return . specificKeyPressed [SDLK_RETURN, SDLK_ESCAPE])
  if c == SDLK_RETURN
    then do
      enemyrot <- liftIO $ fromIntegral `fmap` randomRIO (-180, 180 :: Int)
      plpos <- liftIO $ randPos ((0, 0), (50, 100))
      enpos <- liftIO $ randPos ((100, 0), (150, 100))
      let plrot = angleFromTo plpos enpos - 90
      mnewcargo <- liftIO $ evalStateT combatLoop (newCombat intermediate plpos enpos plrot enemyrot en)
      case mnewcargo of
        Just newcargo -> do
          if M.null newcargo
            then do
              liftIO $ makeTextScreen (100, 400) [(gamefont state, Color4 1.0 1.0 1.0 1.0, "You survived - Current cargo status:"),
                                       (monofont state, Color4 1.0 1.0 0.0 1.0, showCargo newcargo),
                                       (gamefont state, Color4 1.0 1.0 1.0 1.0, "Press ENTER to continue")] (return ())
              liftIO $ getSpecificSDLChar SDLK_RETURN
            else do
              modify $ modPoints (+100)
              (_, cargo', _, hold') <- liftIO $ execStateT 
                             (takeScreen ("Captured cargo") 
                                 (gamefont state) (monofont state)) 
                             (newcargo, plcargo state, plcash state, plholdspace state)
              modify $ modPlCargo (const cargo')
              modify $ modPlHoldspace (const hold')
          releaseKeys
          return False
        Nothing -> do
          releaseKeys
          lostLife "You fought bravely, but your ship was blown to pieces." recoveryText
    else do
      releaseKeys
      return False

drawSpace :: StateT TestState IO ()
drawSpace = do
  state <- State.get
  modify $ modCameraState $ modCamZoom $ (+ (camzoomdelta $ camstate state))
  modify $ modCameraState $ modCamera $ setZoom $ clamp 30 250 $ (camzoom $ camstate state) + (400 * (length2 $ velocity (tri state)))
  modify $ modCameraState $ modCamera $ setCentre $ Entity.position (tri state)
  liftIO $ setCamera (camera $ camstate state)
  liftIO $ drawGLScreen [tri state] (aobjects state)


