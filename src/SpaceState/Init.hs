module SpaceState.Init(lostLife, initState, die)
where

import Data.List
import Control.Monad
import Control.Monad.State as State
import Prelude hiding (catch)

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import qualified Data.Edison.Assoc.StandardMap as M

import OpenGLUtils
import Statistics
import Entity
import AObject
import TextScreen
import Mission

import SpaceState.Game
import SpaceState.City
import SpaceState.Common

lostLife :: String -> String -> StateT SpaceState IO Bool
lostLife s1 s2 = do
  modify $ modLives pred
  modify $ modPlCash $ const 0 -- so that no points are given for cash
  state <- State.get
  if lives state <= 0
    then do
      gameOver s1 gameoverText
    else do
      pressKeyScreen (liftIO $ makeTextScreen (30, 550) [(gamefont state,
                         Color4 1.0 0.2 0.2 1.0, s1 ++ "\n\n" ++ s2 ++ "\n\nPress ENTER to continue")] 
                         (return ())) SDLK_RETURN
      initState
      return False

initState :: StateT SpaceState IO ()
initState = do
  lc <- getRandomPlanet
  modify $ modAObjects $ setupBarycenters
  modify $ modTri $ modifyPosition (const $ (getPosition lc *+* (glVector3UnitX *** (AObject.size lc))))
  modify $ modPlCash $ const startCash
  modify $ modPlHoldspace $ const maxHold
  modify $ modPlCargo $ const M.empty
  modify $ modPlHealth $ const startPlHealth
  modify $ modPlMissions $ const emptyMissionMap
  modify $ modAvailMission $ const Nothing
  gotoCity lc
  releaseKeys

getRandomPlanet :: StateT SpaceState IO AObject
getRandomPlanet = do
  state <- State.get
  n <- liftIO $ chooseIO (aobjects state)
  if (aobjName n == "Star")
    then getRandomPlanet
    else return n

die :: StateT SpaceState IO (Maybe Int)
die = do
  state <- State.get
  return $ Just $ finalPoints state


