module Main(main)
where

import System.Directory
import System.IO (hPutStrLn, stderr)
import Text.Printf
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State as State
import System.IO.Error (mkIOError, doesNotExistErrorType)
import Control.Exception (throwIO, catch, IOException)
import Prelude hiding (catch)
import qualified Data.ByteString.Char8 as Str

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import Graphics.Rendering.FTGL as FTGL

import Space
import SpaceState.Space
import Utils
import TextScreen
import SDLUtils

import Paths_starrover2

main = catch (withInit [InitVideo] $ do
  -- blendEquation $= FuncAdd
  -- blendFunc $= (OpenGL.SrcAlpha, OneMinusSrcAlpha)
  initAll)
  (\e -> hPutStrLn stderr $ "Exception: " ++ show (e :: IOException)) 

loadDataFont :: FilePath -> IO Font
loadDataFont fp = do
  fn <- getDataFileName fp
  exists <- doesFileExist fn
  when (not exists) $ do
    throwIO $ mkIOError doesNotExistErrorType "loading data font during initialization" Nothing (Just fn)
  f <- createTextureFont fn
  _ <- setFontFaceSize f 24 72
  return f

initAll = do
  _ <- setVideoMode width height 0 [OpenGL]
  depthFunc $= Just Less
  clearColor $= Color4 0 0 0 1
  viewport $= (Position 0 0, Size width height)
  f <- loadDataFont "share/DejaVuSans.ttf"
  f2 <- loadDataFont "share/DejaVuSansMono.ttf"
  mainMenu f f2

mainMenu f f2 = do
  n <- menu 1 (f, Color4 1.0 1.0 1.0 1.0, "Star Rover 2")
            [(f, Color4 1.0 1.0 1.0 1.0, "Start a new game"),
             (f, Color4 1.0 1.0 1.0 1.0, "High scores"),
             (f, Color4 1.0 1.0 1.0 1.0, "Help"),
             (f, Color4 1.0 1.0 1.0 1.0, "Quit")]
            (f, Color4 0.1 0.1 1.0 1.0, "=>")
  case n of
    1 -> startGame f f2
    2 -> do
           appdir <- getAppUserDataDirectory "starrover2"
           highscore <- loadHighscore appdir hiscorefilename
           showHighscore f f2 "High scores" highscore
           mainMenu f f2
    3 -> helpScreen f >> mainMenu f f2
    _ -> return ()

helpScreen f = do
  let drawfunc = makeTextScreen (100, 520)
                  [(f, Color4 1.0 1.0 1.0 1.0, "Controls in space:"),
                   (f, Color4 1.0 1.0 1.0 1.0, "Arrows/W/A/S/D - control your ship"),
                   (f, Color4 1.0 1.0 1.0 1.0, "Space - shoot (in combat)"),
                   (f, Color4 1.0 1.0 1.0 1.0, "+ and - - zoom/unzoom"),
                   (f, Color4 1.0 1.0 1.0 1.0, "M - star system map"),
                   (f, Color4 1.0 1.0 1.0 1.0, "Q - retire"),
                   (f, Color4 1.0 1.0 1.0 1.0, "\n"),
                   (f, Color4 1.0 1.0 1.0 1.0, "Controls in menus:"),
                   (f, Color4 1.0 1.0 1.0 1.0, "Arrows - move around in menus"),
                   (f, Color4 1.0 1.0 1.0 1.0, "Enter or space - choose action")]
                  (return ())
  pressAnyKeyScreen drawfunc

startGame f f2 = do
  (n, d) <- initGame f
  pts <- runGame n d f f2
  doHighscore f f2 pts n d
  mainMenu f f2

introText = intercalate "\n"
  ["Welcome, Adventurer!",
   "What is your name?"]

initGame :: Font -> IO (String, Difficulty)
initGame f = do
  n <- getNameInput introText f
  d <- menu 2 (f, Color4 1.0 1.0 1.0 1.0, "Choose your difficulty, " ++ capitalize n)
            (map (\s -> (f, Color4 1.0 1.0 1.0 1.0, show s)) (allEnums :: [Difficulty]))
            (f, Color4 0.1 0.1 1.0 1.0, "=>")
  return (capitalize n, toEnum (d - 1))

type Highscore a = [(Int, String, a)]

loadHighscore :: (Read a) => FilePath -> FilePath -> IO (Highscore a)
loadHighscore dir fn = do
  createDirectoryIfMissing False dir
  let fpath = dir ++ "/" ++ fn
  exists <- doesFileExist fpath
  if exists
    then do
      contents <- liftM Str.unpack $ Str.readFile fpath
      case safeRead contents of
        Nothing -> do
          hPutStrLn stderr $ "Corrupt high score file (" ++ fpath ++ ") - deleting"
          removeFile fpath
          return []
        Just h -> return h
    else return []

saveHighscore :: (Show a) => FilePath -> FilePath -> Highscore a -> IO ()
saveHighscore dir fn hs = do
  createDirectoryIfMissing False dir
  let fpath = dir ++ "/" ++ fn
  writeFile fpath (show hs)

displayHighscore :: (Show a) => Highscore a -> String
displayHighscore = concatMap (\(pts, n, v) -> printf "%-16s %-8s %8d\n" n (show v) pts)

getNameInput :: String -> Font -> IO String
getNameInput greet f = do
  Prelude.flip evalStateT "" $ do
    let drawfunc = do n <- State.get 
                      liftIO $ makeTextScreen (100, 500)
                        [(f, Color4 1.0 1.0 1.0 1.0, greet ++ "\n"),
                         (f, Color4 1.0 1.0 1.0 1.0, n)] (return ())
    let getInput = do
          evts <- liftIO $ pollAllSDLEvents
          shift <- liftIO $ shiftDown
          s <- State.get
          let (s', fin) = inputLine shift evts s
          if fin && not (null s')
            then return (Just s')
            else put s' >> return Nothing
    loopTextScreen drawfunc getInput

hiscorefilename = "hiscore"

doHighscore :: Font -> Font -> Int -> String -> Difficulty -> IO ()
doHighscore f f2 pts n diff = do
  let numentries = 7
  appdir <- getAppUserDataDirectory "starrover2"
  highscore <- loadHighscore appdir hiscorefilename
  let madeit = if pts > 0 && length highscore < numentries
                 then True
                 else let (p, _, _) = last highscore in p < pts
  highscore' <- if madeit
                  then do
                    return $ take numentries $ insertRev (pts, n, diff) highscore
                  else return highscore
  let inittext = if not madeit
                   then "Unfortunately you didn't make it to the high score list."
                   else "You made it to the high score list!\nPoints: " ++ show pts
  when (madeit) $ saveHighscore appdir hiscorefilename highscore'
  showHighscore f f2 inittext highscore'

showHighscore :: Font -> Font -> String -> Highscore Difficulty -> IO ()
showHighscore f f2 inittext hs = do
  let drawfunc = makeTextScreen (100, 520)
                  [(f,  Color4 1.0 1.0 1.0 1.0, inittext ++ "\n"),
                   (f2, Color4 1.0 1.0 1.0 1.0, displayHighscore hs),
                   (f,  Color4 1.0 1.0 1.0 1.0, "\n\nPress any key")] (return ())
  pressAnyKeyScreen drawfunc

