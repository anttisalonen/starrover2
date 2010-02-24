module Main
where

import Data.List
import Data.Char
import qualified Data.Edison.Assoc.StandardMap as M
import System.Random
import Control.Monad.State

type Key = (Sector, Point)

type Sector = (Int, Int)

type Point = (Int, Int)

type Value = String

values = cycle ['a'..'z']

type Rnd = StdGen

pointRange :: Point
pointRange = (0, 100)

type Database = M.FM Key Value

type RndS = State Rnd

randomSector :: Sector -> RndS Database
randomSector s = do
  num <- randomThingR (2, 5)
  points <- replicateM num randomPoint
  values <- replicateM num randomValue
  return $ M.fromSeq $ zip (zip (repeat s) points) values

randomArea :: Int -> RndS Database
randomArea n = do
  let sectornums = concat . take n $ expand2
  sectors <- mapM randomSector sectornums
  return $ foldl' M.union M.empty sectors

randomValue :: RndS String
randomValue = do
  n <- randomThingR (2, 8)
  s <- replicateM n (randomThingR ('a', 'z'))
  return (capitalize s)

capitalize []     = []
capitalize (h:hs) = toUpper h : hs

randomPoint :: RndS Point
randomPoint = randomPair pointRange

randomThing :: (Random t, RandomGen s, MonadState s m) => m t
randomThing = do
  r <- get
  let (a, g) = random r
  put g
  return a

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

expand1 :: (Ord a, Num a) => [a]
expand1 = iterate (\x -> if x > 0 then -x else -x + 1) 0

block :: (Num a, Enum a) => a -> [(a, a)]
block x = [(a, b) | a <- [-x..x], b <- [-x..x], abs a == x || abs b == x]

expand2 :: (Ord a, Num a, Enum a) => [[(a, a)]]
expand2 = unfoldr (\x -> Just (block x, x + 1)) 0

universe :: Int -> Database
universe i = evalState (randomArea i) (mkStdGen 0)

universe' = universe 50

inSector :: Sector -> Key -> Value -> Bool
inSector (sx1, sy1) ((sx, sy), _) _ = sx1 == sx && sy1 == sy

sector1, sector2, sector3 :: Database
sector1 = getSector (18, -4) universe'
sector2 = getSector (19, -4) universe'
sector3 = getSector (18, -3) universe'
sector4 = getSector (19, -3) universe'

getSector :: Sector -> Database -> Database
getSector s = M.filterWithKey (inSector s)

main = do
  putStrLn "Universe created"
  -- putStrLn "Press enter to start..."
  -- getLine
  putStrLn "First sector (18, -4):"
  print sector1
  putStrLn "Second sector (19, -4):"
  print sector2
  putStrLn "Third sector (18, -3):"
  print sector3
  putStrLn "Fourth sector (19, -3):"
  print sector4
  putStrLn "Fifth sector (-18, 13):"
  print $ getSector (-18, 13) universe'
  putStrLn "Sixth sector (-19, 13):"
  print $ getSector (-19, 13) universe'
  putStrLn "Press enter to quit..."
  getLine
