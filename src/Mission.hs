module Mission
where

import Control.Monad (liftM)
import Data.Maybe (isJust)
import Data.List (find)

import qualified Data.Edison.Assoc.AssocList as M

type MissionMap = M.FM String Mission

data MissionCategory = Messenger

data Mission = MessengerMission String

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
missionRepNeeded = [(minRepForMission, Messenger)]

possibleMissionType :: Int -> Maybe MissionCategory
possibleMissionType thr = 
  liftM snd (find (\n -> fst n <= thr) missionRepNeeded)

