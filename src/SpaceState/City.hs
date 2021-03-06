module SpaceState.City(gotoCity, catapult)
where

import Data.List hiding (concat)
import Data.Maybe
import Data.Foldable
import Control.Monad hiding (mapM_)
import Control.Monad.State as State hiding (mapM_)
import Prelude hiding (catch, concat, mapM_)
import Text.Printf

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.Rendering.FTGL as FTGL
import Graphics.UI.SDL as SDL hiding (flip)

import Statistics
import OpenGLUtils
import Politics
import Entity
import AObject
import Cargo
import Utils
import TextScreen
import Mission
import SpaceState.Game

updateAvailableMission :: AObject -> StateT SpaceState IO ()
updateAvailableMission lc = do
  state <- State.get
  let malleg = colonyOwner lc
  case malleg of
    Nothing -> return ()
    Just alleg -> do
      case possibleMissionType (allegAttitude alleg state) of
        Nothing        -> return ()
        Just m         -> createMission m alleg lc

createMission :: MissionCategory -> String -> AObject -> StateT SpaceState IO ()
createMission Messenger alleg lc = do
  let planetname = aobjName lc
  state <- State.get
  let otherplanets = fmap aobjName $ filter (hasOwner alleg) (toList $ aobjects state)
  when (not (null otherplanets)) $ do
    n <- liftIO $ chooseIO otherplanets
    when (n /= planetname) $ 
      modify $ modAvailMission $ const $ Just $ MessengerMission n

createMission SecretMessage alleg lc = do
  let planetname = aobjName lc
  state <- State.get
  let otherplanets = map aobjName $ filter (\a ->  hasSomeOwner a && (not . hasOwner alleg) a) (toList $ aobjects state)
  when (not (null otherplanets)) $ do
    n <- liftIO $ chooseIO otherplanets
    when (n /= planetname) $ 
      modify $ modAvailMission $ const $ Just $ SecretMessageMission n

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
  handleArrival lc
  updateAvailableMission lc
  cityLoop lc
  catapult lc

cityLoop :: AObject -> StateT SpaceState IO ()
cityLoop lc = do
  let planetname = aobjName lc
  state <- State.get
  let f = gamefont state
      alleg = getAllegiance lc
      leave = "Leave " ++ planetname
      cangovernor = isJust $ possibleMissionType (allegAttitude alleg state)
      options = "Market" : "Shipyard" : "Foreign affairs" : (if cangovernor then "Governor" : [leave] else [leave])
  n <- liftIO $ menu 1 (f, Color4 1.0 1.0 1.0 1.0, 
                  concat ["Starport on " ++ planetname,
                          if alleg == planetname then "" else "\nThis planet belongs to the country of " ++ alleg ++ "."])
            (map (\s -> (f, Color4 1.0 1.0 0.0 1.0, s)) options)
            (f, Color4 1.0 1.0 0.0 1.0, "=>")
  case n of
    1 -> gotoMarket planetname >> cityLoop lc
    2 -> gotoShipyard >> cityLoop lc
    3 -> liftIO (showForeignAffairs (colonyOwner lc) (gamefont state)) >> cityLoop lc
    4 -> if cangovernor then gotoGovernor lc >> cityLoop lc else return ()
    _ -> return ()

showForeignAffairs :: Maybe String -> Font -> IO ()
showForeignAffairs mst f =
  let strs =
        case mst of
          Nothing -> ["This colony is in anarchy!"]
          Just st -> map relToStr rs
            where rs = relationshipsOf st
                  relToStr (l, v) | v <= (-4) = printf "We despise the state of %s." l
                  relToStr (l, v) | v <= (-1) = printf "We are not very fond of %s." l
                  relToStr (l, v) | v <= 1 = printf "Our relationship with the state of %s is neutral." l
                  relToStr (l, v) | v <= 4 = printf "%s is a friend of ours." l
                  relToStr (l, _) | otherwise = printf "We are allied with the state of %s." l
      drawfunc = makeTextScreen (100, 420) 
                                (map (\s -> (f, Color4 1.0 1.0 1.0 1.0, s)) 
                                     (strs ++ ["\n\nPress any key to continue"])) 
                                (return ())
  in pressAnyKeyScreen drawfunc

gotoShipyard :: StateT SpaceState IO ()
gotoShipyard = do
  state <- State.get
  let f = gamefont state
  let damages = startPlHealth - plhealth state
  let cost = damages * 20
  let repairtext =
        if damages == 0
          then "Repair (no damages)"
          else "Repair ship (Cost: " ++ show cost ++ ")"
  n <- liftIO $ menu 1 (f, Color4 1.0 1.0 1.0 1.0, "Shipyard")
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

catapult :: AObject -> StateT SpaceState IO ()
catapult lc = do
  state <- State.get
  let objpos = AObject.getPosition lc
      dist = AObject.size lc
      plloc = Entity.position (tri state)
      pldir = OpenGLUtils.normalize (plloc *-* objpos)
  modify $ modTri $ modifyPosition $ const $ objpos *+* (pldir *** (dist + 5))
  modify $ modTri $ modifyVelocity $ const $ pldir *** 0.2
  modify $ modTri $ resetAcceleration
  modify $ modTri $ modifyRotation $ (+180)

gotoGovernor :: AObject -> StateT SpaceState IO ()
gotoGovernor lc = do
  state <- State.get
  let alleg = getAllegiance lc
  case missionFor alleg (plmissions state) of
    Nothing          -> 
      case availmission state of
        Nothing  -> noMissionsScreen
        Just mis -> offerMission mis lc
    Just currmission -> missionDisplay (gamefont state) alleg currmission

missionDisplay f alleg (MessengerMission tgt) = do
  missionDisplayGeneric f alleg ["You have an important message that you need to",
                                 "bring to the planet of " ++ tgt ++ "."]
missionDisplay f alleg (SecretMessageMission tgt) = do
  missionDisplayGeneric f alleg ["There's a spy of ours currently residing",
                                 "on the foreign planet of " ++ tgt ++ ".",
                                 "You must bring him a secret message."]

missionDisplayGeneric f alleg msg = 
  pressAnyKeyScreen 
    (liftIO $ makeTextScreen (100, 500) 
        [(f, Color4 1.0 0.2 0.2 1.0, 
           intercalate "\n" (["Your current mission for " ++ alleg ++ ":", ""]
                             ++ msg ++ ["", "Press any key to continue"]))]
        (return ()))

handleArrival :: AObject -> StateT SpaceState IO ()
handleArrival lc = do
  state <- State.get
  mapM_ (flip handleArrival' lc) (missions (plmissions state))

noMissionsScreen :: StateT SpaceState IO ()
noMissionsScreen = do
  state <- State.get
  let plname = playername state
  pressAnyKeyScreen 
    (liftIO $ makeTextScreen (100, 500) 
        [(gamefont state, Color4 1.0 0.2 0.2 1.0, 
           intercalate "\n" ["\"Dear " ++ plname ++ ", we currently have",
                             "no tasks for you.\"",
                             "",
                             "",
                             "Press any key to continue"])]
        (return ()))

offerMission :: Mission -> AObject -> StateT SpaceState IO ()
offerMission mis@(MessengerMission tgt) lc = do
  state <- State.get
  let plname = playername state
  let txt = ["\"Dear " ++ plname ++ ", I welcome you to my",
            "residence. I have an important mission that",
            "needs to be taken care of by a trustworthy",
            "adventurer like yourself.", 
            "",
            "The mission requires you to deliver an",
            "important message from here to the planet",
            "of " ++ tgt ++ ".",
            "Will you accept?\" (y/n)"]
  offerMissionGeneric mis lc txt

offerMission mis@(SecretMessageMission tgt) lc = do
  state <- State.get
  let plname = playername state
  let txt = ["\"Dear " ++ plname ++ ", I welcome you to my",
            "residence. I have a very important mission that",
            "needs to be taken care of by a competent",
            "adventurer like yourself.", 
            "",
            "The mission requires you to deliver a message",
            "to a spy of ours hidden on the foreign planet",
            "of " ++ tgt ++ ".",
            "Will you accept?\" (y/n)"]
  offerMissionGeneric mis lc txt

offerMissionGeneric :: Mission -> AObject -> [String] -> StateT SpaceState IO ()
offerMissionGeneric mis lc msg = do
  state <- State.get
  let alleg = getAllegiance lc
  let txt = intercalate "\n" msg
  c <- pressOneOfScreen 
             (liftIO $ makeTextScreen (100, 500) 
                         [(gamefont state, Color4 1.0 1.0 1.0 1.0, txt)]
                          (return ()))
             [SDLK_y, SDLK_n, SDLK_ESCAPE]
  modify $ modAvailMission (const Nothing)
  case c of
    SDLK_y -> modify $ modPlMissions $ setMission alleg mis
    _      -> return ()

handleArrival' :: (String, Mission) -> AObject -> StateT SpaceState IO ()
handleArrival' (alleg, MessengerMission tgt) lc = do
  state <- State.get
  let plname = playername state
      planetname = aobjName lc
  checkArrived tgt planetname alleg pointsForMessengerMission
    ["\"Thank you, " ++ plname ++ ", for serving our great",
     "country of " ++ alleg ++ " by delivering this important",
     "message to us.\"",
     "",
     "Mission accomplished!",
     "",
     "",
     "Press Enter to continue"]

handleArrival' (alleg, SecretMessageMission tgt) lc = do
  state <- State.get
  let plname = playername state
      planetname = aobjName lc
  checkArrived tgt planetname alleg pointsForSecretMessage
    ["\"Thank you, " ++ plname ++ ", for serving our great",
     "country of " ++ alleg ++ " by delivering this important",
     "message to us here on " ++ planetname ++ ".\"",
     "",
     "Mission accomplished!",
     "",
     "",
     "Press Enter to continue"]

onActualArrival :: String -> Int -> [String] -> StateT SpaceState IO ()
onActualArrival alleg pts str = do
  state <- State.get
  let txt = intercalate "\n" str
  modify $ modAvailMission (const Nothing)
  modify $ modAllegAttitudes $ modAttitude (+pts) alleg
  modify $ modPlMissions $ removeMission alleg
  pressKeyScreen 
    (liftIO $ makeTextScreen (100, 500) 
        [(gamefont state, Color4 1.0 0.2 0.2 1.0, txt)]
        (return ())) SDLK_RETURN

checkArrived :: String -> String -> String -> Int -> [String] -> StateT SpaceState IO ()
checkArrived tgt planetname alleg pts str = 
  when (tgt == planetname) (onActualArrival alleg pts str)

