module Types
where

data Game = Game {
    gameSetup     :: Setup
  , player        :: Player
  , gameMode      :: GameMode
  , inMenu        :: Bool
  , gameHandlers  :: [Handler]
  }

data Player = Player {
    playerName :: String
  }

type Point = (Float, Float)

data GameMode = Docked DockedMode
              | Flying
              | Hyperspace
              | Combat
  deriving (Eq, Read, Show)

data DockedMode = DockedMenu
                | Market
                | BulletinBoard
                | Governor
  deriving (Eq, Read, Show)

type Handler = (Game -> Bool, Game -> IO Game)

data Setup = Setup { getDifficulty :: Difficulty }

data Difficulty = Easy | Medium | Hard
  deriving (Eq, Read, Show, Enum, Bounded)


