module SpaceState.City(gotoCity, catapult)
where

import Data.List
import Control.Monad
import Control.Monad.State as State
import Prelude hiding (catch)

import Graphics.Rendering.OpenGL as OpenGL

import OpenGLUtils
import Entity
import AObject
import Cargo
import Utils
import TextScreen
import SpaceState.Game

gotoCity :: AObject -> StateT SpaceState IO ()
gotoCity lc = do
  let planetname = aobjName lc
  state <- State.get
  nmarket <- if planetname == fst (lastmarket state)
               then return $ lastmarket state
               else do
                 m <- liftIO $ randomMarket
                 return (planetname, m)
  modify $ modMarket $ const nmarket
  cityLoop planetname
  catapult (AObject.getPosition lc)

cityLoop :: String -> StateT SpaceState IO ()
cityLoop planetname = do
  state <- State.get
  let f = gamefont state
  let alleg = planetNameToAllegiance (aobjects state) planetname
  n <- liftIO $ menu (f, Color4 1.0 1.0 1.0 1.0, 
                  concat ["Starport on " ++ planetname,
                          if alleg == planetname then "" else "\nThis planet belongs to the country of " ++ alleg ++ "."])
            [(f, Color4 1.0 1.0 0.0 1.0, "Market"),
             (f, Color4 1.0 1.0 0.0 1.0, "Shipyard"),
             (f, Color4 1.0 1.0 0.0 1.0, "Leave " ++ planetname)]
            (f, Color4 1.0 1.0 0.0 1.0, "=>")
  case n of
    1 -> gotoMarket planetname >> cityLoop planetname
    2 -> gotoShipyard >> cityLoop planetname
    _ -> return ()

gotoShipyard :: StateT SpaceState IO ()
gotoShipyard = do
  state <- State.get
  let f = gamefont state
  let damages = startPlHealth - plhealth state
  let cost = damages * 20
  let repairtext =
        if damages == 0
          then "Repair (no damages)"
          else "Repair ship (Cost: " ++ show cost ++ " credits)"
  n <- liftIO $ menu (f, Color4 1.0 1.0 1.0 1.0, "Shipyard")
            [(f, Color4 1.0 1.0 0.0 1.0, repairtext),
             (f, Color4 1.0 1.0 0.0 1.0, "Exit")]
            (f, Color4 1.0 1.0 0.0 1.0, "=>")
  case n of
    1 -> do
           when (cost > 0 && plcash state >= cost) $ do
             modify $ modPlCash $ (subtract cost)
             modify $ modPlHealth $ const startPlHealth
           gotoShipyard
    _ -> return ()

gotoMarket :: String -> StateT SpaceState IO ()
gotoMarket planetname = do
  state <- State.get
  let market = snd . lastmarket $ state
  (m', cargo', cash', hold') <- liftIO $ execStateT 
                                  (tradeScreen ("Market on " ++ planetname) 
                                      (gamefont state) (monofont state)) 
                                  (market, plcargo state, plcash state, plholdspace state)
  modify $ modMarket $ modSnd $ const m'
  modify $ modPlCargo $ const cargo'
  modify $ modPlCash $ const cash'
  modify $ modPlHoldspace $ const hold'

catapult :: GLvector3 -> StateT SpaceState IO ()
catapult vec = do
  state <- State.get
  let plloc = Entity.position (tri state)
  let (dx, dy, _) = (plloc *-* vec)
  let newvel = OpenGLUtils.normalize (dx, dy, 0) *** 0.2
  modify $ modTri $ modifyPosition $ (*+* (newvel *** 5))
  modify $ modTri $ modifyVelocity $ const newvel
  modify $ modTri $ resetAcceleration
  modify $ modTri $ modifyRotation $ (+180)


