module Combat(combatLoop, newCombat)
where

import System.Random
import Data.Either
import Control.Monad
import Control.Monad.State as State

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import qualified Data.Edison.Seq.SimpleQueue as S
import qualified Data.Edison.Assoc.StandardMap as M

import OpenGLUtils
import Entity
import Space
import Cargo

data Combat = Combat {
    ship1          :: Entity
  , ship2          :: Entity
  , ship1health    :: Int
  , ship2health    :: Int
  , lasers         :: S.Seq Entity
  , cargo          :: Cargo
  , combatPaused   :: Bool
  }

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

modShipN :: Int -> (Entity -> Entity) -> Combat -> Combat
modShipN 1 f = modShip1 f
modShipN 2 f = modShip2 f
modShipN _ _ = id

newCombat :: Cargo -> Combat
newCombat c = Combat (newStdShip (0, 0, 0) playerShipColor 0)
                   (newStdShip (90, 60, 0) enemyShipColor 180)
                   3
                   3
                   S.empty
                   c
                   False

accelerateCombat n a = modify $ modShipN n $ modifyAcceleration (const (0.0,  a, 0.0))
turnCombat n a = modify $ modShipN n $ modifyAngVelocity (+a)
setTurnCombat n a = modify $ modShipN n $ modifyAngVelocity (const a)

laserLength :: GLdouble
laserLength = 1

shipNShoot :: Int -> StateT Combat IO ()
shipNShoot n = do
  state <- State.get
  let men = case n of
              1 -> Just $ ship1 state
              2 -> Just $ ship2 state
              _ -> Nothing
  case men of
    Nothing -> return ()
    Just en -> do
      let shippos = Entity.position en
          shipvel = Entity.velocity en
          shiprot = Entity.rotation en + 90
          lookVector = (cos (degToRad shiprot), sin (degToRad shiprot), 0)
          laserpos = shippos *+* (lookVector *** 3)
          laservel = shipvel *+* (lookVector *** 1)
          laserrot = shiprot
      let nent = Entity laserpos laservel glVector3Null laserrot 0 0 (Color4 1.0 0.0 0.0 1.0) Lines [(1.0, 0.0, 0.0), (-1.0, 0.0, 0.0)] (glVector3AllUnit *** laserLength)
      modify $ modLasers $ S.rcons nent

accelForce :: GLdouble
accelForce = 0.002

turnRate :: GLdouble
turnRate = 1.5

combatMapping = 
  [ (SDLK_w,     (accelerateCombat 1 accelForce,    accelerateCombat 1 0))
  , (SDLK_s,     (accelerateCombat 1 (-accelForce), accelerateCombat 1 0))
  , (SDLK_a,     (turnCombat 1 turnRate, turnCombat 1 (-turnRate)))
  , (SDLK_d,     (turnCombat 1 (-turnRate), turnCombat 1 turnRate))
  , (SDLK_UP,    (accelerateCombat 1 accelForce, accelerateCombat 1 0))
  , (SDLK_DOWN,  (accelerateCombat 1 (-accelForce), accelerateCombat 1 0))
  , (SDLK_LEFT,  (turnCombat 1 turnRate, turnCombat 1 (-turnRate)))
  , (SDLK_RIGHT, (turnCombat 1 (-turnRate), turnCombat 1 turnRate))
  , (SDLK_p,     (modify $ modCombatPaused not, return ()))
  , (SDLK_SPACE, (shipNShoot 1, return ()))
  ]

combatLoop :: StateT Combat IO (Maybe Cargo)
combatLoop = do
  liftIO $ delay 10
  state <- State.get
  drawCombat
  oneDead <- if combatPaused state
               then return 0
               else updateCombatState
  handleCombatAI
  quits <- handleCombatEvents
  if quits || oneDead == 1
    then return Nothing
    else if oneDead == 2
      then (fmap . fmap) Just liftIO $ arrangeCargo (cargo state)
      else combatLoop

createRandomCargo :: Int -> IO Cargo
createRandomCargo i = do
  cnames <- replicateM i randomCargo
  cargoquantities <- replicateM i (randomRIO (1, 20 :: Int)) 
  return $ M.fromSeqWith (+) (zip cnames cargoquantities)

arrangeCargo :: Cargo -> IO Cargo
arrangeCargo c = do
  difftypes <- randomRIO (1, 3 :: Int)
  c' <- createRandomCargo difftypes
  return $ M.unionWith (+) c c'

handleCombatAI :: StateT Combat IO ()
handleCombatAI = do -- accelerateCombat 2 (accelForce * 0.5)
  state <- State.get
  let (myposx, myposy, _) = Entity.position (ship2 state)
  let (enemyposx, enemyposy, _) = Entity.position (ship1 state)
  let angleToEnemy = atan2 (enemyposy - myposy) (enemyposx - myposx)
  let myangle = degToRad $ wrapDegrees $ Entity.rotation (ship2 state) + 90
  let epsilon = 0.001
  if myangle + epsilon < angleToEnemy
    then setTurnCombat 2 turnRate
    else if myangle - epsilon > angleToEnemy
           then setTurnCombat 2 (-turnRate)
           else setTurnCombat 2 0
  accelerateCombat 2 accelForce
  val <- liftIO $ randomRIO (0, 20 :: Int)
  when (val == 0) $ shipNShoot 2

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


