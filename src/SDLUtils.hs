module SDLUtils
where

import Control.Exception (throwIO)
import Data.Maybe
import Data.Char
import Data.List

import Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Utilities as SDLU
import qualified Data.Edison.Assoc.StandardMap as M

-- generic stuff
pollAllSDLEvents :: IO [SDL.Event]
pollAllSDLEvents = pollAllSDLEvents'' True

pollAllSDLEvents' :: IO [SDL.Event]
pollAllSDLEvents' = pollAllSDLEvents'' False

pollAllSDLEvents'' :: Bool -> IO [SDL.Event]
pollAllSDLEvents'' throwOnQuit = go []
    where go l = do
            e <- SDL.pollEvent
            if e == SDL.NoEvent
              then return l
              else if throwOnQuit && e == SDL.Quit
                     then (throwIO $ userError "User wants to quit")
                     else do
                       es <- pollAllSDLEvents
                       return (e:es)

hasEvent :: (SDL.Event -> Bool) -> [SDL.Event] -> Bool
hasEvent fun evts = or $ map fun evts

getSDLChar :: IO SDLKey
getSDLChar = do
  e <- waitEvent
  case e of
    KeyDown (Keysym n _ _) -> return n
    _                      -> getSDLChar

getSpecificSDLChar :: SDLKey -> IO ()
getSpecificSDLChar c = do
  d <- getSDLChar
  if c == d
    then return ()
    else getSpecificSDLChar c

getSpecificSDLChars :: [SDLKey] -> IO SDLKey
getSpecificSDLChars cs = do
  d <- getSDLChar
  if d `elem` cs
    then return d
    else getSpecificSDLChars cs

keyDowns :: [SDL.Event] -> [SDLKey]
keyDowns = foldl' (\acc e -> case e of KeyDown (Keysym n _ _) -> (n:acc); _ -> acc) []

specificKeyPressed :: [SDLKey] -> [SDL.Event] -> Maybe SDLKey
specificKeyPressed ks evts = listToMaybe $ intersect ks (keyDowns evts)

processEvent :: (Monad m) => [(SDLKey, (m (), m ()))] -> Event -> m ()
processEvent n evt =
  let mk = case evt of
             KeyDown (Keysym k _ _) -> Just (True, k)
             KeyUp   (Keysym k _ _) -> Just (False, k) 
             _                      -> Nothing
  in case mk of
       Nothing     -> return ()
       Just (e, k) -> case lookup k n of
                        Nothing       -> return ()
                        Just (a1, a2) -> if e then a1 else a2

processEvents
  :: (Monad m) => [(SDLKey, (m (), m ()))] -> [Event] -> m ()
processEvents n = mapM_ (processEvent n)

isQuit :: [SDL.Event] -> Bool
isQuit = hasEvent isq
  where isq Quit = True
        isq (KeyDown (Keysym SDLK_q _ _)) = True
        isq _ = False

keyWasPressed :: SDLKey -> [SDL.Event] -> Bool
keyWasPressed j es = j `elem` keyDowns es

oneofKeyWasPressed :: [SDLKey] -> [SDL.Event] -> Bool
oneofKeyWasPressed j es = not . null $ j `intersect` keyDowns es

anyKeyOrMouseWasPressed :: [SDL.Event] -> Bool
anyKeyOrMouseWasPressed = hasEvent isk
  where isk (KeyDown _)             = True
        isk (MouseButtonDown _ _ _) = True
        isk _                       = False

anyKeyOrMouseWasPressedIO :: IO Bool
anyKeyOrMouseWasPressedIO = pollAllSDLEvents >>= return . anyKeyOrMouseWasPressed

inputLine :: Bool -> [SDL.Event] -> String -> (String, Bool)
inputLine shift es sd = (sd', entered)
  where sd'      = osd ++ newInput
        osd      = reverse . drop backspaces $ reverse sd
        backspaces = Data.List.length $ filter (== SDLK_BACKSPACE) keys
        keys     = keyDowns es
        entered  = keyWasPressed SDLK_RETURN es
        newInput = map up $ catMaybes $ map sdlkKeyToChar keys
        up       = if shift then toUpper else id

shiftDown :: IO Bool
shiftDown = do
  mods <- getModState
  return (KeyModLeftShift `elem` mods || KeyModRightShift `elem` mods || KeyModShift `elem` mods)

-- TODO: add special characters
sdlkKeyToChar :: SDLKey -> Maybe Char
sdlkKeyToChar = Prelude.flip M.lookupM (M.fromSeq 
    (zip (SDLU.enumFromTo SDLK_a SDLK_z ++ SDLU.enumFromTo SDLK_0 SDLK_9 ++ [SDLK_SPACE]) 
         (['a'..'z'] ++ ['0'..'9'] ++ " ")))

mouseClickIn :: Int -> [SDL.MouseButton] -> ((Int, Int), (Int, Int)) -> [SDL.Event] -> Bool
mouseClickIn height buttons ((minx, miny), (diffx, diffy)) =
  hasEvent f
    where f (MouseButtonDown x y b) = 
            let x' = fromIntegral x
                y' = height - fromIntegral y
            in b `elem` buttons && 
               x' >= minx && 
               y' >= miny && 
               x' <= minx + diffx && 
               y' <= miny + diffy
          f _ = False

mouseClickInAny :: Int -> [SDL.MouseButton] -> [((Int, Int), (Int, Int))] -> [SDL.Event] -> Maybe ((Int, Int), (Int, Int))
mouseClickInAny height bs areas events = 
  foldr (\x acc -> if mouseClickIn height bs x events then Just x else acc) 
        Nothing areas


