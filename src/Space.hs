module Space(newStdShipEntity,
  playerShipColor,
  enemyShipColor,
  boxArea,
  width,
  height,
  drawGLScreen,
  drawEntity,
  pollAllSDLEvents,
  pollAllSDLEvents',
  processEvents,
  isQuit,
  keyWasPressed,
  anyKeyOrMouseWasPressed,
  collides2d,
  getShipBox,
  getSDLChar,
  getSpecificSDLChar,
  getSpecificSDLChars,
  inputLine,
  shiftDown,
  specificKeyPressed,
  mouseClickIn,
  mouseClickInAny,
  angleFromTo,
  angleFromToRad
  )
where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Control.Exception (throwIO)

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Utilities as SDLU
import qualified Data.Edison.Assoc.StandardMap as M

import OpenGLUtils
import Entity
import AObject
import Utils
import Collision

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

newStdShipEntity :: GLvector3 -> Color4 GLfloat -> GLdouble -> Entity
newStdShipEntity pos c rot = newEntity pos rot c TriangleFan trianglePoints glVector3AllUnit

playerShipColor, enemyShipColor :: Color4 GLfloat
playerShipColor = Color4 0.0 0.5 0.0 1.0
enemyShipColor  = Color4 0.5 0.0 0.0 1.0

width :: (Num a) => a
width = 800

height :: (Num a) => a
height = 600

drawEntity :: Entity -> IO ()
drawEntity ent = do
    loadIdentity
    translate $ (\(x,y,z) -> Vector3 x y z) (Entity.position ent)
    rotate (Entity.rotation ent) $ Vector3 0 0 (1 :: GLdouble)
    (\(x,y,z) -> OpenGL.scale x y z) (Entity.scale ent)
    currentColor $= (Entity.color ent)
    renderPrimitive (primitive ent) $ forM_ (vertices ent) $ \(x,y,z) -> do
      vertex $ Vertex3 x y z

drawGLScreen :: [Entity] -> [AObject] -> IO ()
drawGLScreen ents objs = do
  clear [ColorBuffer,DepthBuffer]
  lineWidth $= 5

  forM_ ents drawEntity
  
  lineWidth $= 1
  forM_ objs $ \aobj -> do
    loadIdentity
    rotate (angle aobj) $ Vector3 0 0 (1 :: GLdouble)
    translate $ Vector3 (orbitRadius aobj) 0 0
    uniformScale (size aobj)
    currentColor $= (AObject.color aobj)
    renderPrimitive Polygon $ forM_ aobjPoints $ \(x,y,z) -> do
      vertex $ Vertex3 x y z

    loadIdentity
    uniformScale (orbitRadius aobj)
    currentColor $= aorbitColor
    renderPrimitive LineLoop $ forM_ aorbitPoints $ \(x,y,z) -> do
      vertex $ Vertex3 x y z

  glSwapBuffers

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
keyWasPressed j = hasEvent isk
  where isk (KeyDown (Keysym x _ _)) | x == j = True
        isk _                                 = False

anyKeyOrMouseWasPressed :: [SDL.Event] -> Bool
anyKeyOrMouseWasPressed = hasEvent isk
  where isk (KeyDown _)             = True
        isk (MouseButtonDown _ _ _) = True
        isk _                       = False

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

mouseClickIn :: [SDL.MouseButton] -> ((Int, Int), (Int, Int)) -> [SDL.Event] -> Bool
mouseClickIn buttons ((minx, miny), (diffx, diffy)) =
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

mouseClickInAny :: [SDL.MouseButton] -> [((Int, Int), (Int, Int))] -> [SDL.Event] -> Maybe ((Int, Int), (Int, Int))
mouseClickInAny bs areas events = foldr (\x acc -> if mouseClickIn bs x events then Just x else acc) Nothing areas

angleFromTo :: (RealFloat t) => (t, t, t)
            -> (t, t, t)
            -> t
angleFromTo a b = radToDeg $ angleFromToRad a b

angleFromToRad :: (RealFloat t) => (t, t, t)
            -> (t, t, t)
            -> t
angleFromToRad (ax, ay, _) (bx, by, _) =
  atan2 (by - ay) (bx - ax)

getShipBox
  :: Entity -> ((GLdouble, GLdouble), (GLdouble, GLdouble))
getShipBox e = 
  let (x, y, _) = Entity.position e
  in boxArea (x, y) 1

trianglePoints :: [GLvector3]
trianglePoints =
   [ (0,    1,   0)
    ,(0.9, -1,   0)
    ,(0.0, -0.7, 0)
    ,(-0.9,-1,   0)
   ]


