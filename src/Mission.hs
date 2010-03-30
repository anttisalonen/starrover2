module Mission
where

import Control.Monad (liftM)
import Data.Maybe (isJust)
import Data.List (find)

import qualified Data.Edison.Assoc.AssocList as M

type MissionMap = M.FM String Mission

data MissionCategory = Messenger
                     | SecretMessage

data Mission = MessengerMission String
             | SecretMessageMission String

emptyMissionMap :: MissionMap
emptyMissionMap = M.empty

missionFor :: String -> MissionMap -> Maybe Mission
missionFor = M.lookupM

hasMissionFor :: String -> MissionMap -> Bool
hasMissionFor s m = isJust $ missionFor s m

setMission :: String -> Mission -> MissionMap -> MissionMap
setMission = M.insert

removeMission :: String -> MissionMap -> MissionMap
removeMission = M.delete

minRepForMission = 2

missionRepNeeded :: [(Int, MissionCategory)]
missionRepNeeded = [(6, SecretMessage), (minRepForMission, Messenger)]

possibleMissionType :: Int -> Maybe MissionCategory
possibleMissionType thr = 
  liftM snd (find (\n -> fst n <= thr) missionRepNeeded)

missions :: MissionMap -> [(String, Mission)]
missions = M.toSeq

