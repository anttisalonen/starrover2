module SpaceState.City(gotoCity, catapult)
where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State as State
import Prelude hiding (catch)

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL

import Statistics
import OpenGLUtils
import Entity
import AObject
import Cargo
import Utils
import TextScreen
import Mission
import SpaceState.Game

-- TODO: MaybeT?
updateAvailableMission :: AObject -> StateT SpaceState IO ()
updateAvailableMission lc = do
  let planetname = aobjName lc
  state <- State.get
  let malleg = colonyOwner lc
  case malleg of
    Nothing -> return ()
    Just alleg -> do
      case possibleMissionType (allegAttitude planetname state) of
        Nothing        -> return ()
        Just Messenger -> do
          let otherplanets = map aobjName $ filter (hasOwner alleg) (aobjects state)
          if null otherplanets
            then return ()
            else do
              n <- liftIO $ chooseIO otherplanets
              if n == planetname
                then return ()
                else modify $ modAvailMission $ const $ Just $ MessengerMission n

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
  updateAvailableMission lc
  cityLoop lc
  catapult (AObject.getPosition lc)

cityLoop :: AObject -> StateT SpaceState IO ()
cityLoop lc = do
  let planetname = aobjName lc
  state <- State.get
  let f = gamefont state
      alleg = getAllegiance lc
      leave = "Leave " ++ planetname
      cangovernor = isJust $ possibleMissionType (allegAttitude planetname state)
      options = "Market" : "Shipyard" : (if cangovernor then "Governor" : [leave] else [leave])
  n <- liftIO $ menu (f, Color4 1.0 1.0 1.0 1.0, 
                  concat ["Starport on " ++ planetname,
                          if alleg == planetname then "" else "\nThis planet belongs to the country of " ++ alleg ++ "."])
            (map (\s -> (f, Color4 1.0 1.0 0.0 1.0, s)) options)
            (f, Color4 1.0 1.0 0.0 1.0, "=>")
  case n of
    1 -> gotoMarket planetname >> cityLoop lc
    2 -> gotoShipyard >> cityLoop lc
    3 -> if cangovernor then gotoGovernor lc >> cityLoop lc else return ()
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

gotoGovernor :: AObject -> StateT SpaceState IO ()
gotoGovernor lc = do
  let planetname = aobjName lc
  state <- State.get
  let alleg = getAllegiance lc
      plname = playername state
  let noMissionsScreen = 
          pressKeyScreen 
            (liftIO $ makeTextScreen (100, 500) 
                [(gamefont state, Color4 1.0 0.2 0.2 1.0, 
                   intercalate "\n" ["\"Dear " ++ plname ++ ", we currently have",
                                     "no tasks for you.\"",
                                     "",
                                     "",
                                     "Press Enter to continue"])]
                (return ())) SDLK_RETURN
  case missionFor alleg (plmissions state) of
    Nothing -> do
      case availmission state of
        Nothing -> noMissionsScreen
        Just mis@(MessengerMission tgt) -> do
          let txt = intercalate "\n" ["\"Dear " ++ plname ++ ", I welcome you to my",
                                      "residence. I have an important mission that",
                                      "needs to be taken care of by a trustworthy",
                                      "adventurer like yourself.", 
                                      "",
                                      "The mission requires you to deliver an",
                                      "important message from here to the planet",
                                      "of " ++ tgt ++ ".",
                                      "Will you accept?\" (y/n)"]
          c <- pressOneOfScreen 
                     (liftIO $ makeTextScreen (100, 500) 
                                 [(gamefont state, Color4 1.0 1.0 1.0 1.0, txt)]
                                  (return ()))
                     [SDLK_y, SDLK_n, SDLK_ESCAPE]
          modify $ modAvailMission (const Nothing)
          case c of
            SDLK_y -> modify $ modPlMissions $ setMission alleg mis
            _      -> return ()
    Just m  -> do
      case m of
        MessengerMission tgt -> 
          if (tgt == planetname)
            then do
              let txt = intercalate "\n" ["\"Thank you, " ++ plname ++ ", for serving our great",
                                          "country of " ++ alleg ++ " by delivering this important",
                                          "delivering this important message to us.\"",
                                          "",
                                          "Mission accomplished!",
                                          "",
                                          "",
                                          "Press Enter to continue"]
              modify $ modAvailMission (const Nothing)
              modify $ modPlMissions $ removeMission alleg
              pressKeyScreen 
                (liftIO $ makeTextScreen (100, 500) 
                    [(gamefont state, Color4 1.0 0.2 0.2 1.0, txt)]
                    (return ())) SDLK_RETURN
            else noMissionsScreen

