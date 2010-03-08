module Utils
where

import System.IO
import Data.List
import Data.Char
import Control.Monad
import System.Random
import Control.Monad.State

getOneChar :: IO Char
getOneChar = do
  b <- hGetBuffering stdin
  hSetBuffering stdin NoBuffering
  c <- getChar
  putStrLn ""
  hSetBuffering stdin b
  return c

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

pick :: [(a -> Bool, b)] -> a -> Maybe b
pick []          _ = Nothing
pick ((f, s):ns) x = if f x
                       then Just s 
                       else pick ns x

getFromTable :: String -> [(String, a)] -> Maybe a
getFromTable _ []               = Nothing
getFromTable c ((d, h):ds) = if c `isPrefixOf` d then Just h else getFromTable c ds

type RndS = State Rnd

type Rnd = StdGen

randomThingR :: (Random t, RandomGen s, MonadState s m) => (t, t) -> m t
randomThingR range = do
  r <- get
  let (a, g) = randomR range r
  put g
  return a

randomPair :: (Random t, RandomGen s, MonadState s m) => (t, t) -> m (t, t)
randomPair range = do
  a <- randomThingR range
  a' <- randomThingR range
  return (a, a')

capitalize []     = []
capitalize (h:hs) = toUpper h : hs

{-
randomThing :: (Random t, RandomGen s, MonadState s m) => m t
randomThing = do
  r <- get
  let (a, g) = random r
  put g
  return a
-}


