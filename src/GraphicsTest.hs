module Main()
where

import System.Random
import System.Directory
import System.IO (hPutStrLn, stderr)
import Data.Maybe
import Control.Monad
import Control.Monad.State as State
import System.IO.Error (mkIOError, doesNotExistErrorType)
import Control.Exception (throwIO, catch, IOException)
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

import Paths_starrover2

-- test scenario
data TestState = TestState {
    tri          :: Entity
  , aobjects     :: [AObject]
  , camstate     :: CameraState
  , stopped      :: Bool
  , gamefont     :: Font
  , monofont     :: Font
  , cargo        :: Cargo
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

modCargo :: (Cargo -> Cargo) -> TestState -> TestState
modCargo f t = t{cargo = f (cargo t)}

main = catch (withInit [InitVideo] $ do
  -- blendEquation $= FuncAdd
  -- blendFunc $= (OpenGL.SrcAlpha, OneMinusSrcAlpha)
  createAWindow)
  (\e -> hPutStrLn stderr $ "Exception: " ++ show (e :: IOException)) 

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

initState :: Font -> Font -> TestState
initState f f2 = TestState 
    (newStdShip (50.0, 30.0, 0.0) playerShipColor 0)
    aobjs
    stdCamera
    False
    f
    f2
    M.empty

loadDataFont :: FilePath -> IO Font
loadDataFont fp = do
  fn <- getDataFileName fp
  exists <- doesFileExist fn
  when (not exists) $ do
    throwIO $ mkIOError doesNotExistErrorType "loading data font during initialization" Nothing (Just fn)
  f <- createTextureFont fn
  _ <- setFontFaceSize f 24 72
  return f

createAWindow = do
  _ <- setVideoMode width height 0 [OpenGL]
  depthFunc $= Just Less
  clearColor $= Color4 0 0 0 1
  viewport $= (Position 0 0, Size width height)
  f <- loadDataFont "share/DejaVuSans.ttf"
  f2 <- loadDataFont "share/DejaVuSansMono.ttf"
  let is = initState f f2
  setCamera (camera $ camstate is)
  evalStateT loop is

zoomChangeFactor :: (Floating a) => a
zoomChangeFactor = 1.0

-- accelerate :: (MonadState TestState m) => GLdouble -> m ()
accelerate a = modify $ modTri $ modifyAcceleration (const (0.0,  a, 0.0))

-- turn :: (MonadState TestState m) => GLdouble -> m ()
turn a = modify $ modTri $ modifyAngVelocity (+a)
setTurn a = modify $ modTri $ modifyAngVelocity (const a)

changeZoom a = modify $ modCameraState $ modCamZoomDelta (+a)

inputMapping = 
  [ (SDLK_w,     (accelerate 0.002,    accelerate 0))
  , (SDLK_s,     (accelerate (-0.002), accelerate 0))
  , (SDLK_a,     (turn 1.5, setTurn 0))
  , (SDLK_d,     (turn (-1.5), setTurn 0))
  , (SDLK_UP,    (accelerate 0.002, accelerate 0))
  , (SDLK_DOWN,  (accelerate (-0.002), accelerate 0))
  , (SDLK_LEFT,  (turn 1.5, setTurn 0))
  , (SDLK_RIGHT, (turn (-1.5), setTurn 0))
  , (SDLK_MINUS, (changeZoom zoomChangeFactor, changeZoom (-zoomChangeFactor)))
  , (SDLK_PLUS,  (changeZoom (-zoomChangeFactor), changeZoom zoomChangeFactor))
  , (SDLK_i,     (showInfo, return ()))
  , (SDLK_p,     (modify $ modStopped not, return ()))
  ]

showInfo = do
  s <- State.get
  liftIO . putStrLn $ "Zoom: " ++ show (camzoom $ camstate s)
  liftIO . putStrLn $ "Player position: " ++ show (Entity.position $ tri s)
  forM_ (aobjects s) $ \aobj -> do
    liftIO . putStrLn $ "Astronomical body position: " ++ show (AObject.getPosition aobj)

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx n = if mn > n then mn else if mx < n then mx else n

handleCollisions :: ((GLdouble, GLdouble), (GLdouble, GLdouble)) -> [AObject] -> Maybe AObject
handleCollisions plbox aobs = 
  listToMaybe . catMaybes $ map colliding aobs
    where colliding aobj =
            if collides2d plbox abox
              then Just aobj
              else Nothing
            where (objcoordx, objcoordy, _) = AObject.getPosition aobj
                  abox = boxArea (objcoordx, objcoordy) (size aobj)

loop :: StateT TestState IO ()
loop = untilDone $ do 
  liftIO $ delay 10
  state <- State.get
  drawSpace
  when (not (stopped state)) updateSpaceState
  handleEvents

handleEvents :: StateT TestState IO Bool
handleEvents = do
  events <- liftIO $ pollAllSDLEvents
  processEvents inputMapping events
  return $ isQuit events

drawExitButton :: Font -> IO ()
drawExitButton f = do
  loadIdentity
  translate $ Vector3 100 (100) (0 :: GLdouble)
  currentColor $= Color4 1.0 1.0 1.0 1.0
  renderPrimitive LineLoop $
    mapM_ vertex [Vertex3 (0 :: GLdouble) 0 0, Vertex3 0 30 0, Vertex3 100 30 0, Vertex3 100 0 0]
  translate $ Vector3 10 10 (0 :: GLdouble)
  renderFont f "Exit" FTGL.Front

loopTextScreen :: (MonadIO m) => m () -> m Bool -> m ()
loopTextScreen drawScreenFunc handleEventsFunc = untilDone $ do
  liftIO $ delay 10
  drawScreenFunc
  handleEventsFunc

gotoCity :: String -> StateT TestState IO ()
gotoCity n = do
  state <- State.get
  liftIO $ makeTextScreen [(gamefont state, Color4 1.0 1.0 1.0 1.0, "Landed on " ++ n),
                           (gamefont state, Color4 1.0 1.0 1.0 1.0, "Current cargo status:"),
                           (monofont state, Color4 1.0 1.0 0.0 1.0, showCargo (cargo state))]
                          (drawExitButton (gamefont state))
  liftIO $ getSpecificSDLChar SDLK_RETURN
  return ()

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

updateSpaceState :: StateT TestState IO ()
updateSpaceState = do
  state <- State.get
  modify $ modTri (updateEntity 1)
  modify $ modAObjects $ map (\a -> if orbitRadius a == 0 then a else modifyAngle (+ (10 * recip (orbitRadius a))) a)
  let mlanded = handleCollisions (getShipBox $ tri state) (aobjects state)
  case mlanded of
    Nothing -> do
      val <- liftIO $ randomRIO (0, 500 :: Int)
      when (val == 0) startCombat
    Just lc -> do
      if aobjName lc == "Star"
        then gameOver "You burn to death!"
        else do
          gotoCity (aobjName lc)
          catapult (AObject.getPosition lc)

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

gameOver :: String -> StateT TestState IO ()
gameOver s = do
  state <- State.get
  loopTextScreen (liftIO $ makeTextScreen [(gamefont state, Color4 1.0 0.2 0.2 1.0, s ++ "\nPress ENTER to continue")] (return ()))
                 (liftIO $ pollAllSDLEvents >>= return . keyWasPressed SDLK_RETURN)
  let is = initState (gamefont state) (monofont state)
  modify $ const is

startCombat :: StateT TestState IO ()
startCombat = do
  state <- State.get
  liftIO $ makeTextScreen [(gamefont state, Color4 1.0 1.0 1.0 1.0, "Combat beginning - press ENTER to start\nor ESCAPE to escape")] (return ())
  c <- liftIO $ getSpecificSDLChars [SDLK_RETURN, SDLK_ESCAPE]
  when (c == SDLK_RETURN) $ do
    mnewcargo <- liftIO $ evalStateT combatLoop (newCombat (cargo state))
    case mnewcargo of
      Just newcargo -> do
        liftIO $ makeTextScreen [(gamefont state, Color4 1.0 1.0 1.0 1.0, "You survived - Current cargo status:"),
                                 (monofont state, Color4 1.0 1.0 0.0 1.0, showCargo newcargo),
                                 (gamefont state, Color4 1.0 1.0 1.0 1.0, "Press ENTER to continue")] (return ())
        liftIO $ getSpecificSDLChar SDLK_RETURN
        modify $ modCargo (const newcargo)
        return ()
      Nothing -> do
        gameOver "You've been exterminated . . ."
  setTurn 0
  accelerate 0 -- prevent involuntary actions

drawSpace :: StateT TestState IO ()
drawSpace = do
  state <- State.get
  modify $ modCameraState $ modCamZoom $ (+ (camzoomdelta $ camstate state))
  modify $ modCameraState $ modCamera $ setZoom $ clamp 30 250 $ (camzoom $ camstate state) + (400 * (length2 $ velocity (tri state)))
  modify $ modCameraState $ modCamera $ setCentre $ Entity.position (tri state)
  liftIO $ setCamera (camera $ camstate state)
  liftIO $ drawGLScreen [tri state] (aobjects state)


