module SpaceState.Combat(startCombat)
where

import System.Random
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State as State
import Prelude hiding (catch)

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import qualified Data.Edison.Assoc.StandardMap as M

import Combat
import Space
import Cargo
import TextScreen
import Politics
import SDLUtils
import Universe

import SpaceState.Common
import SpaceState.Game
import SpaceState.Init
import SpaceState.Difficulty

startCombat :: Maybe (String, Enemy, String) -- ^ If Nothing, use random enemy
                                             -- and standard message. Otherwise
                                             -- use given (msg, enemy, enemy allegiance).
            -> StateT SpaceState IO (Bool, Bool) -- ^ (gameOver?, combatWon?)
startCombat n = do
  state <- State.get
  (s, en, enalleg) <- case n of
                        Nothing -> do
                          en' <- liftIO $ randomEnemy $ difficultyAIshift $ difficulty state
                          enalleg' <- liftIO $ randomAllegiance
                          s' <- return $
                           concat ["You spot another ship traveling nearby.\n",
                                   "It seems to be a " ++ (describeEnemy en') ++ ".\n",
                                   "The ship is part of the country of " ++ enalleg' ++ ".\n",
                                   "Press ENTER to start a battle against the foreign ship\n",
                                   "or ESCAPE to escape"]
                          return (s', en', enalleg')
                        Just m -> return m
  c <- loopTextScreen (liftIO $ makeTextScreen (100, 400) 
                         [(gamefont state, Color4 1.0 1.0 1.0 1.0, s)]
                          (return ()))
                      (liftIO $ pollAllSDLEvents >>= return . specificKeyPressed [SDLK_RETURN, SDLK_ESCAPE])
  if c == SDLK_RETURN
    then do
      enemyrot <- liftIO $ fromIntegral `fmap` randomRIO (-180, 180 :: Int)
      plpos <- liftIO $ randPos ((0, 0), (50, 100))
      enpos <- liftIO $ randPos ((100, 0), (150, 100))
      let plrot = angleFromTo plpos enpos - 90
      (newhealth, newpoints, mnewcargo) <- liftIO $ evalStateT combatLoop 
                                           (newCombat plalleg enalleg intermediate (plhealth state) plpos enpos plrot enemyrot en)
      if newhealth == 0
        then do
          releaseKeys
          gameover <- lostLife "You fought bravely, but your ship was blown to pieces." recoveryText
          return (gameover, False)
        else do
          modify $ modPlHealth $ const newhealth
          case mnewcargo of
            Nothing -> do
              liftIO $ makeTextScreen (100, 400) [(gamefont state, Color4 1.0 1.0 1.0 1.0, "The enemy is out of your sight.\n\n"),
                                       (gamefont state, Color4 1.0 1.0 1.0 1.0, "Press ENTER to continue")] (return ())
              liftIO $ getSpecificSDLChar SDLK_RETURN
              releaseKeys
              return (False, False)
            Just newcargo -> do
              when (not (M.null newcargo)) $ do
                modify $ modPoints (+(newpoints * (diffcoeff $ difficulty state)))
                (_, cargo', _, hold') <- liftIO $ execStateT 
                               (takeScreen ("Captured cargo") 
                                   (gamefont state) (monofont state)) 
                               (newcargo, plcargo state, plcash state, plholdspace state)
                modify $ modPlCargo (const cargo')
                modify $ modPlHoldspace (const hold')
              killed enalleg
              releaseKeys
              return (False, True)
    else do
      releaseKeys
      return (False, False)

internationalAction :: String -> Int -> StateT SpaceState IO ()
internationalAction s f = modify $ modAllegAttitudes $ consequences f s allegiances relations

killed :: String -> StateT SpaceState IO ()
killed enalleg = internationalAction enalleg (-1)


