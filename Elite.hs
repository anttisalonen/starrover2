module Elite
where

import Data.List
import Text.Printf
import System.Exit

import qualified Data.Map as M
import Control.Monad

import Utils
import Types

gameTitle = "The Game"

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


