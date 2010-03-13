{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Data.Either
import Control.Monad
import Control.Monad.State as State

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import qualified Data.Edison.Seq.SimpleQueue as S

import OpenGLUtils
import Entity
import Camera
import AObject

-- generic stuff
pollAllSDLEvents :: IO [SDL.Event]
pollAllSDLEvents = go []
    where go l = do
                   e <- SDL.pollEvent
                   if e == SDL.NoEvent
                     then return l
                     else do
                       es <- pollAllSDLEvents
                       return (e:es)

hasEvent :: (SDL.Event -> Bool) -> [SDL.Event] -> Bool
hasEvent fun evts = or $ map fun evts

-- test scenario
width = 800
height = 600

data TestState = TestState {
    tri          :: Entity
  , aobjects     :: [AObject]
  , camstate     :: CameraState
  , stopped      :: Bool
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

data Combat = Combat {
    ship1          :: Entity
  , ship2          :: Entity
  , ship1health    :: Int
  , ship2health    :: Int
  , lasers         :: S.Seq Entity
  , combatPaused   :: Bool
  }

newCombat :: Combat
newCombat = Combat (newStdShip (0, 0, 0) playerShipColor)
                   (newStdShip (30, 20, 0) enemyShipColor)
                   3
                   3
                   S.empty
                   False

modShip1 :: (Entity -> Entity) -> Combat -> Combat
modShip1 f t = t{ship1 = f (ship1 t)}

modShip2 :: (Entity -> Entity) -> Combat -> Combat
modShip2 f t = t{ship2 = f (ship2 t)}

modShip1Health :: (Int -> Int) -> Combat -> Combat
modShip1Health f t = t{ship1health = f (ship1health t)}

modShip2Health :: (Int -> Int) -> Combat -> Combat
modShip2Health f t = t{ship2health = f (ship2health t)}

modLasers :: (S.Seq Entity -> S.Seq Entity) -> Combat -> Combat
modLasers f t = t{lasers = f (lasers t)}

modCombatPaused :: (Bool -> Bool) -> Combat -> Combat
modCombatPaused f t = t{combatPaused = f (combatPaused t)}

main = withInit [InitVideo] $ do
  -- blendEquation $= FuncAdd
  -- blendFunc $= (OpenGL.SrcAlpha, OneMinusSrcAlpha)
  createAWindow 

aobjs =
  [ AObject 0   (Color4 0.9 0.0 0.0 1.0) 6.0 0
  , AObject 10  (Color4 0.5 0.5 1.0 1.0) 2.0 28
  , AObject 250 (Color4 0.0 0.4 0.5 1.0) 4.0 80
  , AObject 30  (Color4 0.6 0.6 0.6 1.0) 3.0 100
  , AObject 80  (Color4 0.6 0.6 0.6 1.0) 2.0 130
  ]

aobjsAndOrbits = unzip $ map aobjToEntities aobjs

newStdShip :: GLvector3 -> Color4 GLfloat -> Entity
newStdShip pos c = newEntity pos c TriangleFan trianglePoints glVector3AllUnit

playerShipColor, enemyShipColor :: Color4 GLfloat
playerShipColor = Color4 0.0 0.5 0.0 1.0
enemyShipColor  = Color4 0.5 0.0 0.0 1.0

stdCamera :: CameraState
stdCamera = CameraState 
      ((-0.01 * width, -0.01 * height), (0.02 * width, 0.02 * height))
      100
      0

initState :: TestState
initState = TestState 
    (newStdShip (50.0, 30.0, 0.0) playerShipColor)
    aobjs
    stdCamera
    False

createAWindow = do
  _ <- setVideoMode width height 0 [OpenGL]
  depthFunc $= Just Less
  clearColor $= Color4 0 0 0 1
  viewport $= (Position 0 0, Size width height)
  setCamera (camera $ camstate initState)
  evalStateT loop initState

zoomChangeFactor :: (Floating a) => a
zoomChangeFactor = 1.0

-- accelerate :: (MonadState TestState m) => GLdouble -> m ()
accelerate a = modify $ modTri $ modifyAcceleration (const (0.0,  a, 0.0))

-- turn :: (MonadState TestState m) => GLdouble -> m ()
turn a = modify $ modTri $ modifyAngVelocity (+a)

changeZoom a = modify $ modCameraState $ modCamZoomDelta (+a)

inputMapping = 
  [ (SDLK_w,     (accelerate 0.002,    accelerate 0))
  , (SDLK_s,     (accelerate (-0.002), accelerate 0))
  , (SDLK_a,     (turn 1.5, turn (-1.5)))
  , (SDLK_d,     (turn (-1.5), turn 1.5))
  , (SDLK_UP,    (accelerate 0.002, accelerate 0))
  , (SDLK_DOWN,  (accelerate (-0.002), accelerate 0))
  , (SDLK_LEFT,  (turn 1.5, turn (-1.5)))
  , (SDLK_RIGHT, (turn (-1.5), turn 1.5))
  , (SDLK_MINUS, (changeZoom zoomChangeFactor, changeZoom (-zoomChangeFactor)))
  , (SDLK_PLUS,  (changeZoom (-zoomChangeFactor), changeZoom zoomChangeFactor))
  , (SDLK_i,     (showInfo, return ()))
  , (SDLK_p,     (modify $ modStopped not, return ()))
  ]

accelerateCombat a = modify $ modShip1 $ modifyAcceleration (const (0.0,  a, 0.0))
turnCombat a = modify $ modShip1 $ modifyAngVelocity (+a)

laserLength = 1

ship1Shoot :: StateT Combat IO ()
ship1Shoot = do
  state <- State.get
  let en = ship1 state
      shippos = Entity.position en
      shipvel = Entity.velocity en
      shiprot = Entity.rotation en + 90
      lookVector = (cos (degToRad shiprot), sin (degToRad shiprot), 0)
      laserpos = shippos *+* (lookVector *** 3)
      laservel = shipvel *+* (lookVector *** 1)
      laserrot = shiprot
  let nent = Entity laserpos laservel glVector3Null laserrot 0 0 (Color4 1.0 0.0 0.0 1.0) Lines [(1.0, 0.0, 0.0), (-1.0, 0.0, 0.0)] (glVector3AllUnit *** laserLength)
  modify $ modLasers $ S.rcons nent

combatMapping = 
  [ (SDLK_w,     (accelerateCombat 0.002,    accelerateCombat 0))
  , (SDLK_s,     (accelerateCombat (-0.002), accelerateCombat 0))
  , (SDLK_a,     (turnCombat 1.5, turnCombat (-1.5)))
  , (SDLK_d,     (turnCombat (-1.5), turnCombat 1.5))
  , (SDLK_UP,    (accelerateCombat 0.002, accelerateCombat 0))
  , (SDLK_DOWN,  (accelerateCombat (-0.002), accelerateCombat 0))
  , (SDLK_LEFT,  (turnCombat 1.5, turnCombat (-1.5)))
  , (SDLK_RIGHT, (turnCombat (-1.5), turnCombat 1.5))
  , (SDLK_p,     (modify $ modCombatPaused not, return ()))
  , (SDLK_SPACE, (ship1Shoot, return ()))
  ]

processEvent :: (Monad m) => [(SDLKey, (m (), m ()))] -> Event -> m ()
processEvent n evt =
  let mk = case evt of
             KeyDown (Keysym k _ _) -> Just (True, k)
             KeyUp   (Keysym k _ _) -> Just (False, k) 
             _                      -> Nothing
  in case mk of
       Nothing     -> return ()
       Just (e, k) -> case lookup k n of
                        Nothing       -> return ()
                        Just (a1, a2) -> if e then a1 else a2

showInfo = do
  s <- State.get
  liftIO . putStrLn $ "Zoom: " ++ show (camzoom $ camstate s)
  liftIO . putStrLn $ "Player position: " ++ show (Entity.position $ tri s)
  forM_ (aobjects s) $ \aobj -> do
    liftIO . putStrLn $ "Astronomical body position: " ++ show (AObject.getPosition aobj)

processEvents
  :: (Monad m) => [(SDLKey, (m (), m ()))] -> [Event] -> m ()
processEvents n = mapM_ (processEvent n)

isQuit :: [SDL.Event] -> Bool
isQuit = hasEvent isq
  where isq Quit = True
        isq (KeyDown (Keysym SDLK_q _ _)) = True
        isq _ = False

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx n = if mn > n then mn else if mx < n then mx else n

collides1d (a, b) (c, d) =
  (a < c && c < b) || (a < d && d < b) ||
  (c < a && a < d) || (c < b && b < d)

collides2d (x1, y1) (x2, y2) =
  collides1d x1 x2 && collides1d y1 y2

boxArea (x, y) r = ((x - r, x + r), (y - r, y + r))

getShipBox e = 
  let (x, y, _) = Entity.position e
  in boxArea (x, y) 1

handleCollisions :: StateT TestState IO ()
handleCollisions = do
  state <- State.get
  let plbox = getShipBox (tri state)
  forM_ (aobjects state) $ \aobj -> do
    let (objcoordx, objcoordy, _) = AObject.getPosition aobj
        abox = boxArea (objcoordx, objcoordy) (size aobj)
    when (collides2d plbox abox) $ do
      liftIO $ putStrLn "inside planet!"

loop :: StateT TestState IO ()
loop = do 
  liftIO $ delay 10
  state <- State.get
  drawSpace
  quitsState <- if not (stopped state)
                  then updateSpaceState
                  else return False
  quits <- handleEvents
  when (not (or [quitsState, quits])) loop

combatLoop :: StateT Combat IO Bool
combatLoop = do
  liftIO $ delay 10
  state <- State.get
  drawCombat
  oneDead <- if combatPaused state
               then return 0
               else updateCombatState
  handleCombatAI
  quits <- handleCombatEvents
  if quits
    then return quits
    else if oneDead /= 0
      then return $ oneDead == 1
      else combatLoop

handleCombatAI :: StateT Combat IO ()
handleCombatAI = return ()

handleEvents :: StateT TestState IO Bool
handleEvents = do
  events <- liftIO $ pollAllSDLEvents
  processEvents inputMapping events
  return $ isQuit events

updateSpaceState :: StateT TestState IO Bool
updateSpaceState = do
  state <- State.get
  modify $ modTri (updateEntity 1)
  modify $ modAObjects $ map (\a -> if orbitRadius a == 0 then a else modifyAngle (+ (10 * recip (orbitRadius a))) a)
  handleCollisions
  if collides2d ((10, 20), (10, 20)) (getShipBox (tri state))
    then do
      quits <- liftIO $ evalStateT combatLoop newCombat
      return quits
    else do
      return False

drawSpace :: StateT TestState IO ()
drawSpace = do
  state <- State.get
  modify $ modCameraState $ modCamZoom $ (+ (camzoomdelta $ camstate state))
  modify $ modCameraState $ modCamera $ setZoom $ clamp 30 250 $ (camzoom $ camstate state) + (400 * (length2 $ velocity (tri state)))
  modify $ modCameraState $ modCamera $ setCentre $ Entity.position (tri state)
  liftIO $ setCamera (camera $ camstate state)
  liftIO $ drawGLScreen [tri state] (aobjects state)

drawCombat :: StateT Combat IO ()
drawCombat = do
  state <- State.get
  let (x1, y1, _) = Entity.position (ship1 state)
      (x2, y2, _) = Entity.position (ship2 state)
      ((minx1, maxx1), (miny1, maxy1)) = boxArea (x1, y1) 10
      ((minx2, maxx2), (miny2, maxy2)) = boxArea (x2, y2) 10
      minx = min minx1 minx2
      maxx = max maxx1 maxx2
      miny = min miny1 miny2
      maxy = max maxy1 maxy2
      ratio = width / height
      (midx, midy) = ((maxx + minx) / 2, (maxy + miny) / 2)
      dx = midx - minx
      dy = midy - miny
      dx' = max dx (dy * ratio)
      dy' = max dy (dx * (1/ratio))
      minx' = midx - dx'
      miny' = midy - dy'
      maxx' = midx + dx'
      maxy' = midy + dy'
  liftIO $ matrixMode $= Projection
  liftIO $ loadIdentity
  liftIO $ ortho minx' maxx' miny' maxy' (-10) 10
  liftIO $ matrixMode $= Modelview 0
  liftIO $ drawGLScreen ([ship1 state, ship2 state] ++ (S.toList (lasers state))) []

updateCombatState :: StateT Combat IO Int
updateCombatState = do
  modify $ modLasers $ S.map (updateEntity 1)
  modify $ modShip1 (updateEntity 1)
  modify $ modShip2 (updateEntity 1)
  handleCombatCollisions
  state <- State.get
  if (ship1health state == 0)
    then return 1
    else if (ship2health state == 0)
           then return 2
           else return 0

handleCombatEvents :: StateT Combat IO Bool
handleCombatEvents = do
  events <- liftIO $ pollAllSDLEvents
  processEvents combatMapping events
  return $ isQuit events

checkCollision ::
     ((GLdouble, GLdouble), (GLdouble, GLdouble))
     -> ((GLdouble, GLdouble), (GLdouble, GLdouble))
     -> Entity
     -> Either Int Entity
checkCollision plbox1 plbox2 las =
  if collides2d plbox1 ((minx, maxx), (miny, maxy))
    then Left 1
    else if collides2d plbox2 ((minx, maxx), (miny, maxy))
           then Left 2
           else Right las
   where (lposx, lposy, _) = Entity.position las
         lrot = degToRad $ Entity.rotation las
         lx1 = lposx - laserLength * cos lrot
         lx2 = lposx + laserLength * cos lrot
         ly1 = lposy - laserLength * sin lrot
         ly2 = lposy + laserLength * sin lrot
         minx = min lx1 lx2
         maxx = max lx1 lx2
         miny = min ly1 ly2
         maxy = max ly1 ly2

handleCombatCollisions :: StateT Combat IO ()
handleCombatCollisions = do
  state <- State.get
  let plbox1 = getShipBox (ship1 state)
  let plbox2 = getShipBox (ship2 state)
  let colls = map (checkCollision plbox1 plbox2) (S.toList $ lasers state)
  let (hits, newlasers) = partitionEithers colls
  let numhits1 = Prelude.length $ filter (==1) hits
  let numhits2 = Prelude.length $ filter (==2) hits
  when (numhits1 > 0) $ liftIO $ putStrLn "Ship 1 hit!"
  when (numhits2 > 0) $ liftIO $ putStrLn "Ship 2 hit!"
  modify $ modLasers $ const (S.fromList newlasers)
  modify $ modShip1Health (subtract numhits1)
  modify $ modShip2Health (subtract numhits2)

drawGLScreen :: [Entity] -> [AObject] -> IO ()
drawGLScreen ents objs = do
  clear [ColorBuffer,DepthBuffer]
  lineWidth $= 5

  forM_ ents $ \ent -> do
    loadIdentity
    translate $ (\(x,y,z) -> Vector3 x y z) (Entity.position ent)
    rotate (Entity.rotation ent) $ Vector3 0 0 (1 :: GLdouble)
    (\(x,y,z) -> OpenGL.scale x y z) (Entity.scale ent)
    currentColor $= (Entity.color ent)
    renderPrimitive (primitive ent) $ forM_ (vertices ent) $ \(x,y,z) -> do
      vertex $ Vertex3 x y z
  
  lineWidth $= 1
  forM_ objs $ \aobj -> do
    loadIdentity
    rotate (angle aobj) $ Vector3 0 0 (1 :: GLdouble)
    translate $ Vector3 (orbitRadius aobj) 0 0
    uniformScale (size aobj)
    currentColor $= (AObject.color aobj)
    renderPrimitive Polygon $ forM_ aobjPoints $ \(x,y,z) -> do
      vertex $ Vertex3 x y z

    loadIdentity
    uniformScale (orbitRadius aobj)
    currentColor $= aorbitColor
    renderPrimitive LineLoop $ forM_ aorbitPoints $ \(x,y,z) -> do
      vertex $ Vertex3 x y z

  glSwapBuffers

trianglePoints :: [GLvector3]
trianglePoints =
   [ (0,    1,   0)
    ,(0.9, -1,   0)
    ,(0.0, -0.7, 0)
    ,(-0.9,-1,   0)
   ]
