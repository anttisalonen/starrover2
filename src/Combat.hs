module Combat(combatLoop, newCombat, randomAI, AIMode(..))
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
import Utils
import SDLUtils

data AIMode = Human
            | Dummy
            | PoorAim
            | BetterAim
  deriving (Enum, Bounded, Show)

randomAI :: IO AIMode
randomAI = do
  n <- randomRIO (1, 2 :: Int)
  case n of
    1 -> return PoorAim
    _ -> return BetterAim

data Ship = Ship {
    shipentity :: Entity
  , health     :: Int
  , aimode     :: AIMode
  }

modShipEntity :: (Entity -> Entity) -> Ship -> Ship
modShipEntity f t = t{shipentity = f (shipentity t)}

modHealth :: (Int -> Int) -> Ship -> Ship
modHealth f t = t{health = f (health t)}

data Combat = Combat {
    ship1          :: Ship
  , ship2          :: Ship
  , lasers         :: S.Seq Entity
  , cargo          :: Cargo
  , combatPaused   :: Bool
  }

modShip1 :: (Ship -> Ship) -> Combat -> Combat
modShip1 f t = t{ship1 = f (ship1 t)}

modShip2 :: (Ship -> Ship) -> Combat -> Combat
modShip2 f t = t{ship2 = f (ship2 t)}

modLasers :: (S.Seq Entity -> S.Seq Entity) -> Combat -> Combat
modLasers f t = t{lasers = f (lasers t)}

modCombatPaused :: (Bool -> Bool) -> Combat -> Combat
modCombatPaused f t = t{combatPaused = f (combatPaused t)}

modShipN :: Int -> (Ship -> Ship) -> Combat -> Combat
modShipN 1 f = modShip1 f
modShipN 2 f = modShip2 f
modShipN _ _ = id

newStdShip :: GLvector3 -> Color4 GLfloat -> GLdouble -> AIMode -> Ship
newStdShip pos c rot mode = 
  Ship (newStdShipEntity pos c rot)
       3 mode

newCombat :: GLvector3 -> GLvector3 -> GLdouble -> GLdouble -> AIMode -> Cargo -> Combat
newCombat plpos enpos rot rot2 mode c = 
  Combat (newStdShip plpos playerShipColor rot Human)
   (newStdShip enpos enemyShipColor rot2 mode)
   S.empty
   c
   False

accelerateCombat n a = modify $ modShipN n $ modShipEntity $ modifyAcceleration (const (0.0,  a, 0.0))
turnCombat n a = modify $ modShipN n $ modShipEntity $ modifyAngVelocity (+a)
setTurnCombat n a = modify $ modShipN n $ modShipEntity $ modifyAngVelocity (const a)

laserLength :: GLdouble
laserLength = 1

laserSpeed :: GLdouble
laserSpeed = 1

shipNShoot :: Int -> StateT Combat IO ()
shipNShoot n = do
  state <- State.get
  let men = case n of
              1 -> Just $ shipentity $ ship1 state
              2 -> Just $ shipentity $ ship2 state
              _ -> Nothing
  case men of
    Nothing -> return ()
    Just en -> do
      let shippos = Entity.position en
          shipvel = Entity.velocity en
          shiprot = Entity.rotation en + 90
          lookVector = (cos (degToRad shiprot), sin (degToRad shiprot), 0)
          laserpos = shippos *+* (lookVector *** 3)
          laservel = shipvel *+* (lookVector *** laserSpeed)
          laserrot = shiprot
          lasercol = if n == 1 then Color4 0.0 1.0 0.0 1.0 else Color4 1.0 0.0 0.0 1.0
      let nent = Entity laserpos laservel glVector3Null laserrot 0 0 lasercol Lines [(1.0, 0.0, 0.0), (-1.0, 0.0, 0.0)] (glVector3AllUnit *** laserLength)
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
  , (SDLK_i,     (showCombatInfo, return ()))
  ]

showCombatInfo :: StateT Combat IO ()
showCombatInfo = do
  state <- State.get
  let mpos = Entity.position (shipentity $ ship2 state)
  let epos = Entity.position (shipentity $ ship1 state)
  let evel = Entity.velocity (shipentity $ ship1 state)
  let mvel = Entity.velocity (shipentity $ ship2 state)
  let mtgtpos = findHitpoint (epos *-* mpos) (evel *-* mvel) laserSpeed
  liftIO $ putStrLn $ "Pos diff: " ++ (show (epos *-* mpos))
  liftIO $ putStrLn $ "Player Velocity: " ++ (show evel)
  liftIO $ putStrLn $ "Hit point: " ++ (show mtgtpos)

handleTooFar :: StateT Combat IO Bool
handleTooFar = do
  state <- State.get
  let mpos = Entity.position (shipentity $ ship2 state)
  let epos = Entity.position (shipentity $ ship1 state)
  return $ OpenGLUtils.length (mpos *-* epos) > 200.0

combatLoop :: StateT Combat IO (Maybe Cargo)
combatLoop = do
  liftIO $ delay 10
  state <- State.get
  drawCombat
  oneDead <- if combatPaused state
               then return 0
               else updateCombatState
  handleCombatAI
  toofar <- handleTooFar
  quits <- handleCombatEvents
  if quits || oneDead == 1
    then return Nothing
    else if oneDead == 2
      then (fmap . fmap) Just liftIO $ arrangeCargo (cargo state)
      else if toofar
             then return (Just (cargo state))
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
handleCombatAI = do
  state <- State.get
  case aimode (ship2 state) of
    Human     -> return ()
    Dummy     -> accelerateCombat 2 (accelForce * 0.5)
    PoorAim   -> doPoorAim
    BetterAim -> doBetterAim

findHitpoint :: GLvector3 -- ^ target position relative to (0, 0, _)
  -> GLvector3 -- ^ target velocity
  -> GLdouble  -- ^ expanding rate of circle radius from own position
  -> Maybe GLvector3 -- ^ meeting point of target and radius
findHitpoint (a, q, _) (d, e, _) c 
  | c == 0 && d == 0 && e == 0 = Nothing
  | otherwise                  =
    let (t1, t2) = tps a q d e c
        tf = filter (>= 0) [t1, t2]
        tf' = minimum tf
    in if null tf
         then Nothing
         else Just (a + d * tf', q + e * tf', 0)

tps x y p q c = ((term1 - term2) / term3, (term1 + term2) / term3)
  where term1 = p * x + q * y
        term2 = sqrt (c*c*x*x - q*q*x*x + 2 * p * q * x * y + c*c*y*y - p*p*y*y)
        term3 = c*c - p*p - q*q

chargeTarget :: GLdouble -> StateT Combat IO ()
chargeTarget angleToTarget = do
  state <- State.get
  let myangle = degToRad $ wrapDegrees $ Entity.rotation (shipentity $ ship2 state) + 90
  let epsilon = 0.01
  if myangle + epsilon < angleToTarget
    then setTurnCombat 2 turnRate
    else if myangle - epsilon > angleToTarget
           then setTurnCombat 2 (-turnRate)
           else setTurnCombat 2 0
  when (abs (myangle - angleToTarget) < 0.3) $ do
    val <- liftIO $ randomRIO (0, 10 :: Int)
    when (val == 0) $ shipNShoot 2
  accelerateCombat 2 accelForce

doBetterAim :: StateT Combat IO ()
doBetterAim = do
  state <- State.get
  let mpos = Entity.position (shipentity $ ship2 state)
  let epos = Entity.position (shipentity $ ship1 state)
  let evel = Entity.velocity (shipentity $ ship1 state)
  let mvel = Entity.velocity (shipentity $ ship2 state)
  let mtgtpos = findHitpoint (epos *-* mpos) (evel *-* mvel) laserSpeed
  let angleToEnemy = case mtgtpos of
                       Nothing     -> angleFromToRad mpos epos
                       Just tgtpos -> angleFromToRad glVector3Null tgtpos 
  chargeTarget angleToEnemy

doPoorAim :: StateT Combat IO ()
doPoorAim = do
  state <- State.get
  let mpos = Entity.position (shipentity $ ship2 state)
  let epos = Entity.position (shipentity $ ship1 state)
  chargeTarget (angleFromToRad mpos epos)

drawCombat :: StateT Combat IO ()
drawCombat = do
  state <- State.get
  let (x1, y1, _) = Entity.position (shipentity $ ship1 state)
      (x2, y2, _) = Entity.position (shipentity $ ship2 state)
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
  liftIO $ drawGLScreen ([shipentity (ship1 state), shipentity (ship2 state)] ++ (S.toList (lasers state))) []

updateCombatState :: StateT Combat IO Int
updateCombatState = do
  modify $ modLasers $ S.map (updateEntity 1)
  modify $ modShip1 $ modShipEntity (updateEntity 1)
  modify $ modShip2 $ modShipEntity (updateEntity 1)
  handleCombatCollisions
  state <- State.get
  if (health (ship1 state) == 0)
    then return 1
    else if (health (ship2 state) == 0)
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
  let plbox1 = getShipBox (shipentity $ ship1 state)
  let plbox2 = getShipBox (shipentity $ ship2 state)
  let colls = map (checkCollision plbox1 plbox2) (S.toList $ lasers state)
  let (hits, newlasers) = partitionEithers colls
  let numhits1 = Prelude.length $ filter (==1) hits
  let numhits2 = Prelude.length $ filter (==2) hits
  when (numhits1 > 0) $ liftIO $ putStrLn "Ship 1 hit!"
  when (numhits2 > 0) $ liftIO $ putStrLn "Ship 2 hit!"
  modify $ modLasers $ const (S.fromList newlasers)
  modify $ modShipN 1 $ modHealth (subtract numhits1)
  modify $ modShipN 2 $ modHealth (subtract numhits2)


