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

untilDoneR :: (Monad m) => m (Maybe a) -> m a
untilDoneR f = do
  done <- f
  case done of
    Nothing -> untilDoneR f
    Just x  -> return x

boolToMaybe :: Bool -> Maybe ()
boolToMaybe True  = Just ()
boolToMaybe False = Nothing

untilDone :: (Monad m) => m Bool -> m ()
untilDone f = untilDoneR (f >>= return . boolToMaybe)

degToRad :: (Floating a) => a -> a
degToRad d = d * pi / 180

wrap :: (Num a, Ord a) => a -> a -> a -> a
wrap mn mx v =
  if v < mn 
    then wrap mn mx (v + diff)
    else if v > mx
           then wrap mn mx (v - diff)
           else v
    where diff = mx - mn

wrapDegrees :: (Num a, Ord a) => a -> a
wrapDegrees = wrap (-180) 180

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx n = if mn > n then mn else if mx < n then mx else n

{-
randomThing :: (Random t, RandomGen s, MonadState s m) => m t
randomThing = do
  r <- get
  let (a, g) = random r
  put g
  return a
-}


