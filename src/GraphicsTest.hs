module Main()
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

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import Graphics.Rendering.FTGL as FTGL

import Camera
import Space
import SpaceState
import Utils
import TextScreen

import Paths_starrover2

main = catch (withInit [InitVideo] $ do
  -- blendEquation $= FuncAdd
  -- blendFunc $= (OpenGL.SrcAlpha, OneMinusSrcAlpha)
  createAWindow)
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

createAWindow = do
  _ <- setVideoMode width height 0 [OpenGL]
  depthFunc $= Just Less
  clearColor $= Color4 0 0 0 1
  viewport $= (Position 0 0, Size width height)
  f <- loadDataFont "share/DejaVuSans.ttf"
  f2 <- loadDataFont "share/DejaVuSansMono.ttf"
  let is = initState f f2
  setCamera (camera $ camstate is)
  pts <- evalStateT loop is
  doHighscore f f2 pts

type Highscore a = [(Int, String, a)]

loadHighscore :: (Read a) => FilePath -> FilePath -> IO (Highscore a)
loadHighscore dir fn = do
  createDirectoryIfMissing False dir
  let fpath = dir ++ "/" ++ fn
  exists <- doesFileExist fpath
  if exists
    then do
      contents <- readFile fpath
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

displayHighscore :: Highscore a -> String
displayHighscore = concatMap (\(pts, n, _) -> printf "%-16s %8d\n" n pts)

getName :: Font -> IO String
getName f = do
  Prelude.flip evalStateT "" $ do
    let drawfunc = do n <- State.get 
                      liftIO $ makeTextScreen (100, 500)
                        [(f, Color4 1.0 1.0 1.0 1.0, "Please enter your name:\n"),
                         (f, Color4 1.0 1.0 1.0 1.0, n)] (return ())
    let getInput = do
          evts <- liftIO $ pollAllSDLEvents
          s <- State.get
          let (s', fin) = inputLine evts s
          when (not (null evts)) $ liftIO $ putStrLn $ show evts
          when (not (null evts)) $ liftIO $ putStrLn $ s'
          when (not (null evts)) $ liftIO $ putStrLn $ show fin
          if fin && not (null s')
            then do liftIO (putStrLn "returning"); return (Just s')
            else put s' >> return Nothing
    loopTextScreen drawfunc getInput

doHighscore :: Font -> Font -> Int -> IO ()
doHighscore f f2 pts = do
  let numentries = 7
  appdir <- getAppUserDataDirectory "starrover2"
  let hiscorefilename = "hiscore"
  highscore <- loadHighscore appdir hiscorefilename
  let madeit = if length highscore < numentries
                 then True
                 else let (p, _, _) = (highscore !! (numentries - 1)) in p < pts
  highscore' <- if madeit
                  then do
                    n <- getName f
                    return $ take numentries $ insert (pts, n, ()) highscore
                  else return highscore
  let drawfunc = makeTextScreen (100, 500)
                  [(f,  Color4 1.0 1.0 1.0 1.0, "High scores"),
                   (f2, Color4 1.0 1.0 1.0 1.0, displayHighscore highscore'),
                   (f,  Color4 1.0 1.0 1.0 1.0, "\n\nPress any key to exit")] (return ())
  loopTextScreen drawfunc 
                 (pollAllSDLEvents >>= return . boolToMaybe . anyKeyOrMouseWasPressed)
  when (madeit) $ saveHighscore appdir hiscorefilename highscore'

