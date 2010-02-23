module MainMenu(Action, ActionMap, mainMenu)
where

import Data.List(isPrefixOf)

import qualified Data.Map as M

type Action a = a -> IO a
type ActionMap a = M.Map String (Action a)

mainMenu :: IO String -> ActionMap a -> a -> IO ()
mainMenu menu actions setup = do
  chosenActionName <- menu
  case M.lookup chosenActionName actions of
    Nothing -> return ()
    Just a  -> a setup >>= mainMenu menu actions


