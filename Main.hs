module Main
where

import Data.List
import Text.Printf
import System.Exit

import qualified Data.Map as M
import Control.Monad

import MainMenu
import Utils
import Elite
import Types

main :: IO ()
main = do
  mainMenu menu mainMenuActions emptySetup

mainMenuActions :: ActionMap Setup
mainMenuActions = M.fromList [("1", startGame), ("2", options), ("3", quit)]

startGame, options, quit :: Action Setup

startGame setup = do
  game <- intro setup
  mainLoop game >> return setup

options (Setup oldDiff) = do
  putStrLn $ "Current difficulty: " ++ show oldDiff
  let diffs = zip (map show [1..]) ([minBound :: Difficulty .. maxBound])
  forM diffs (\(n, d) -> printf "%s. %s\n" n (show d))
  n <- getOneChar
  case getFromTable [n] diffs of
    Nothing -> options (Setup oldDiff)
    Just d  -> return (Setup d)

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


