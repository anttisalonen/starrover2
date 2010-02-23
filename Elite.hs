module Main
where

import Data.List
import Text.Printf
import System.IO
import System.Exit

import qualified Data.Map as M
import Control.Monad

import MainMenu

main :: IO ()
main = do
  mainMenu menu mainMenuActions emptySetup

mainMenuActions :: ActionMap Setup
mainMenuActions = M.fromList [("1", startGame), ("2", options), ("3", quit)]

gameTitle = "The Game"

startGame, options, quit :: Action Setup

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

startGame setup = do
  game <- intro setup
  mainLoop game >> return setup

intro :: Setup -> IO Game
intro s = do
  putStrLn "Hello! What is your name?"
  n <- getLine
  if null n
    then intro s
    else do
      putStrLn $ "Welcome, " ++ n ++ "!"
      return $ Game s (Player n) (Docked DockedMenu) False handlers

handlers = 
 [
    (inMenu, handleGameMenu)
  , (hasMode (Docked DockedMenu), handleDockedMenu)
  , (hasMode Flying, handleFlying)
  , (hasMode Hyperspace, handleHyperspace)
  , (hasMode Combat, handleCombat)
 ]

hasMode :: GameMode -> Game -> Bool
hasMode m g = gameMode g == m

mainLoop :: Game -> IO ()
mainLoop g = do
  case pick (gameHandlers g) g of
    Nothing -> putStrLn "No case - game over!" >> return ()
    Just f  -> f g >>= mainLoop
    
pick :: [(a -> Bool, b)] -> a -> Maybe b
pick []          _ = Nothing
pick ((f, s):ns) x = if f x
                       then Just s 
                       else pick ns x

handleGameMenu :: Game -> IO Game
handleGameMenu g = do
  n <- putStrGetDigit $ 
       "Game menu\n" ++
       "Difficulty: " ++ (show . getDifficulty . gameSetup $ g) ++ "\n" ++
       "1. Play\n" ++
       "2. Quit"
  case n of
    1 -> return (g{inMenu = False})
    2 -> exitWith ExitSuccess
    _ -> handleGameMenu g

dockedMenuMenu g = "Docked menu"

dockedMenuOptions = "Press m to go to menu"

getOneChar :: IO Char
getOneChar = do
  b <- hGetBuffering stdin
  hSetBuffering stdin NoBuffering
  c <- getChar
  putStrLn ""
  hSetBuffering stdin b
  return c

getDockedMenuInput :: IO (Game -> Game)
getDockedMenuInput = do
  c <- getOneChar
  case c of
    'm' -> return gotoMenu
    _   -> return id

gotoMenu g = g{inMenu = True}

handleDockedMenu :: Game -> IO Game
handleDockedMenu g = do
  putStrLn $ dockedMenuMenu g
  putStrLn $ dockedMenuOptions
  m <- getDockedMenuInput
  return (m g)

handleFlying g = error "flying undefined"
handleHyperspace g = error "hyperspace undefined"
handleCombat g = error "combat undefined"

type Handler = (Game -> Bool, Game -> IO Game)

readInt :: String -> Maybe Int
readInt s = case reads s of
              [(n, _)] -> Just n
              _        -> Nothing

putStrGetDigit :: String -> IO Int
putStrGetDigit s = do
  when (not (null s)) $ putStrLn s
  n <- getOneChar
  case readInt [n] of
    Just p  -> return p
    Nothing -> putStrGetDigit s

getDigit :: IO Int
getDigit = putStrGetDigit ""

putStrGetNumber :: String -> IO Int
putStrGetNumber s = do
  when (not (null s)) $ putStrLn s
  n <- getLine
  if null n
    then putStrGetNumber s
    else case readInt n of
           Just p  -> return p
           Nothing -> putStrGetNumber s

getNumber :: IO Int
getNumber = putStrGetNumber ""

options (Setup oldDiff) = do
  putStrLn $ "Current difficulty: " ++ show oldDiff
  let diffs = zip (map show [1..]) ([minBound :: Difficulty .. maxBound])
  forM diffs (\(n, d) -> printf "%s. %s\n" n (show d))
  n <- getOneChar
  case getFromTable [n] diffs of
    Nothing -> options (Setup oldDiff)
    Just d  -> return (Setup d)

getFromTable :: String -> [(String, a)] -> Maybe a
getFromTable _ []               = Nothing
getFromTable c ((d, h):ds) = if c `isPrefixOf` d then Just h else getFromTable c ds

quit _ = exitWith ExitSuccess

emptySetup :: Setup
emptySetup = Setup Medium

menu :: IO String
menu = uncurry menuFrame menuDef

menuDef :: (String, M.Map String String)
menuDef = ("Welcome to " ++ gameTitle ++ "!\n" ++ 
           "1. start new game\n" ++
           "2. set options\n"     ++
           "3. quit\n",
           M.fromList [("1", "1"), ("2", "2"), ("3", "3")])

menuFrame :: String -> M.Map String a -> IO a
menuFrame msg actions = do
  putStr msg
  n <- getOneChar
  case getFromTable [n] (M.toList actions) of
    Nothing -> menuFrame msg actions
    Just a  -> return a

data Setup = Setup { getDifficulty :: Difficulty }

data Difficulty = Easy | Medium | Hard
  deriving (Eq, Read, Show, Enum, Bounded)


