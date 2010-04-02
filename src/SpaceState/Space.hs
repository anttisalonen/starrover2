module SpaceState.Space(runGame, Difficulty(..))
where

import System.Random
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State as State
import Prelude hiding (catch)

import Graphics.UI.SDL as SDL
import Graphics.Rendering.FTGL as FTGL

import Entity
import Camera
import AObject
import Combat
import Space
import Utils
import Tree

import SpaceState.Common
import SpaceState.Input
import SpaceState.Game
import SpaceState.City
import SpaceState.Init
import SpaceState.Difficulty
import SpaceState.Combat

runGame :: String -> Difficulty -> Font -> Font -> IO Int
runGame plname d f f2 = do
  let is = startState plname d f f2
  setCamera (camera $ camstate is)
  evalStateT (do
    initState
    loop)
    is

loop :: StateT SpaceState IO Int
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

updateSpaceState :: StateT SpaceState IO Bool
updateSpaceState = do
  state <- State.get
  modify $ modTri (updateEntity 1)
  modify $ modAObjects $
     mapInner (\(ang, rad) -> if ang == 0 
                   then (ang, rad)
                   else (ang + 10 * recip rad, rad))
  modify $ modAObjects $ 
     fmap (\a -> if orbitRadius a == 0 
                   then a 
                   else modifyAngle (+ (10 * recip (orbitRadius a))) a)
  modify $ modAObjects $ setupBarycenters
  let mlanded = findCollisions (getShipBox $ tri state) (aobjects state)
  case mlanded of
    Nothing -> do
      val <- liftIO $ randomRIO (0, 500 :: Int)
      if (val == 0) 
        then startCombat Nothing >>= return . fst
        else return False
    Just lc -> do
      if aobjName lc == "Star"
        then lostLife "You flew too close to the star!" recoveryText
        else do
          diedInCity <- enteringCity lc
          releaseKeys
          return diedInCity

-- returns: nothing -> no police contact
-- or Just (gameover?, combatwon?)
survivedPolice :: AObject -> StateT SpaceState IO (Maybe (Bool, Bool))
survivedPolice lc = do
  let planetname = aobjName lc
  state <- State.get
  let attid = allegAttitude planetname state
  if attid >= (-1)
    then return Nothing
    else do
      let s = concat ["There's police! What to do?\n",
                 "Press ENTER to fight your way to the starport\n",
                 "or ESCAPE to escape"]
      pship <- liftIO $ randomPolice $ difficultyAIshift $ difficulty state
      startCombat (Just (s, pship, getAllegiance lc)) >>= return . Just

enteringCity :: AObject -> StateT SpaceState IO Bool
enteringCity lc = do
  n <- survivedPolice lc
  case n of
    Nothing                     -> gotoCity lc >> return False
    Just (gameover, combatwon)  -> do
      if combatwon
        then gotoCity lc
        else catapult (AObject.getPosition lc)
      return gameover


