module Main()
where

import System.Directory
import System.IO (hPutStrLn, stderr)
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
  evalStateT loop is

