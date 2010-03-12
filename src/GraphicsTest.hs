{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

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
  , camera       :: Camera
  , camzoom      :: GLdouble
  , camzoomdelta :: GLdouble
  , stopped      :: Bool
  , combatState  :: Maybe Combat
  }

data Combat = Combat {
    ship1  :: Entity
  , ship2  :: Entity
  , lasers :: S.Seq Entity
  }

newCombat :: Combat
newCombat = Combat (newStdShip (0, 0, 0) playerShipColor)
                   (newStdShip (30, 20, 0) enemyShipColor)
                   S.empty

-- TODO: generate mod-functions using TH
modTri :: (Entity -> Entity) -> TestState -> TestState
modTri f t = t{tri = f (tri t)}

modAObjects :: ([AObject] -> [AObject]) -> TestState -> TestState
modAObjects f t = t{aobjects = f (aobjects t)}

modCamera :: (Camera -> Camera) -> TestState -> TestState
modCamera f t = t{camera = f (camera t)}

modCamZoom :: (GLdouble -> GLdouble) -> TestState -> TestState
modCamZoom f t = t{camzoom = f (camzoom t)}

modCamZoomDelta :: (GLdouble -> GLdouble) -> TestState -> TestState
modCamZoomDelta f t = t{camzoomdelta = f (camzoomdelta t)}

modStopped :: (Bool -> Bool) -> TestState -> TestState
modStopped f t = t{stopped = f (stopped t)}

modCombatState :: (Maybe Combat -> Maybe Combat) -> TestState -> TestState
modCombatState f t = t{combatState = f (combatState t)}

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

initState :: TestState
initState = TestState 
    (newStdShip (50.0, 30.0, 0.0) playerShipColor)
    aobjs
    ((-0.01 * width, -0.01 * height), (0.02 * width, 0.02 * height))
    100
    0
    False
    Nothing

createAWindow = do
  _ <- setVideoMode width height 0 [OpenGL]
  depthFunc $= Just Less
  clearColor $= Color4 0 0 0 1
  viewport $= (Position 0 0, Size width height)
  setCamera (camera initState)
  evalStateT loop initState

zoomChangeFactor :: (Floating a) => a
zoomChangeFactor = 1.0

-- accelerate :: (MonadState TestState m) => GLdouble -> m ()
accelerate a = modify $ modTri $ modifyAcceleration (*+* (0.0,  a, 0.0))

-- turn :: (MonadState TestState m) => GLdouble -> m ()
turn a = modify $ modTri $ modifyAngVelocity (+a)

changeZoom a = modify $ modCamZoomDelta $ (+a)

-- TODO: figure out how to make this a State TestState ()
processEvent :: SDL.Event -> StateT TestState IO ()
processEvent (KeyDown (Keysym SDLK_SPACE _ _)) = modify $ modStopped not
processEvent (KeyDown (Keysym SDLK_w     _ _)) = accelerate 0.002
processEvent (KeyUp   (Keysym SDLK_w     _ _)) = accelerate (-0.002)
processEvent (KeyDown (Keysym SDLK_s     _ _)) = accelerate (-0.002)
processEvent (KeyUp   (Keysym SDLK_s     _ _)) = accelerate 0.002
processEvent (KeyDown (Keysym SDLK_a     _ _)) = turn 1.5
processEvent (KeyUp   (Keysym SDLK_a     _ _)) = turn (-1.5)
processEvent (KeyDown (Keysym SDLK_d     _ _)) = turn (-1.5)
processEvent (KeyUp   (Keysym SDLK_d     _ _)) = turn 1.5
processEvent (KeyDown (Keysym SDLK_UP    _ _)) = accelerate 0.002
processEvent (KeyUp   (Keysym SDLK_UP    _ _)) = accelerate (-0.002)
processEvent (KeyDown (Keysym SDLK_DOWN  _ _)) = accelerate (-0.002)
processEvent (KeyUp   (Keysym SDLK_DOWN  _ _)) = accelerate 0.002
processEvent (KeyDown (Keysym SDLK_LEFT  _ _)) = turn 1.5
processEvent (KeyUp   (Keysym SDLK_LEFT  _ _)) = turn (-1.5)
processEvent (KeyDown (Keysym SDLK_RIGHT _ _)) = turn (-1.5)
processEvent (KeyUp   (Keysym SDLK_RIGHT _ _)) = turn 1.5
processEvent (KeyDown (Keysym SDLK_MINUS _ _)) = changeZoom zoomChangeFactor 
processEvent (KeyUp   (Keysym SDLK_MINUS _ _)) = changeZoom (-zoomChangeFactor)
processEvent (KeyDown (Keysym SDLK_PLUS  _ _)) = changeZoom (-zoomChangeFactor)
processEvent (KeyUp   (Keysym SDLK_PLUS  _ _)) = changeZoom zoomChangeFactor 
processEvent (KeyDown (Keysym SDLK_i     _ _)) = do
  s <- State.get
  liftIO . putStrLn $ "Zoom: " ++ show (camzoom s)
  liftIO . putStrLn $ "Player position: " ++ show (Entity.position $ tri s)
  forM_ (aobjects s) $ \aobj -> do
    liftIO . putStrLn $ "Astronomical body position: " ++ show (AObject.getPosition aobj)
processEvent _                                 = return ()

processEvents :: [SDL.Event] -> StateT TestState IO ()
processEvents = mapM_ processEvent

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
  case combatState state of
    Nothing -> do
      drawSpace
      when (not (stopped state)) $ do
        updateSpaceState
      quits <- handleEvents
      when (not quits) loop
    Just c -> do
      quits <- handleEvents
      when (not quits) loop

handleEvents :: StateT TestState IO Bool
handleEvents = do
  events <- liftIO $ pollAllSDLEvents
  processEvents events
  return $ isQuit events

updateSpaceState :: StateT TestState IO ()
updateSpaceState = do
  state <- State.get
  modify $ modTri (updateEntity 1)
  modify $ modAObjects $ map (\a -> if orbitRadius a == 0 then a else modifyAngle (+ (10 * recip (orbitRadius a))) a)
  handleCollisions
  when (collides2d ((10, 20), (10, 20)) (getShipBox (tri state))) $ do
    modify $ modCombatState $ const $ Just newCombat

drawSpace :: StateT TestState IO ()
drawSpace = do
  state <- State.get
  modify $ modCamZoom $ (+ (camzoomdelta state))
  modify $ modCamera $ setZoom $ clamp 30 250 $ (camzoom state) + (400 * (length2 $ velocity (tri state)))
  modify $ modCamera $ setCentre $ Entity.position (tri state)
  liftIO $ setCamera (camera state)
  liftIO $ drawGLScreen (tri state) (aobjects state)

drawGLScreen :: Entity -> [AObject] -> IO ()
drawGLScreen ent objs = do
  clear [ColorBuffer,DepthBuffer]

  loadIdentity
  translate $ (\(x,y,z) -> Vector3 x y z) (Entity.position ent)
  rotate (Entity.rotation ent) $ Vector3 0 0 (1 :: GLdouble)
  (\(x,y,z) -> OpenGL.scale x y z) (Entity.scale ent)
  currentColor $= (Entity.color ent)
  renderPrimitive (primitive ent) $ forM_ (vertices ent) $ \(x,y,z) -> do
    vertex $ Vertex3 x y z
  
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
