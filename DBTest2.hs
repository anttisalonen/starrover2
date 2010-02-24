module Main
where

import Data.List
import Data.Char
import System.Random
import Control.Monad.State
import Data.Bits
import Data.Ord
import System.IO

type Key = (Sector, Point)

type Sector = (Int, Int)

type Point = (Int, Int)

type Value = String

type Rnd = StdGen

pointRange :: Point
pointRange = (0, 100)

type RndS = State Rnd

getSector :: Sector -> [(Key, Value)]
getSector s@(sx, sy) = flip evalState (mkStdGen $ sx `shiftL` (bitSize sx `div` 2) + sy) $ do
  num <- randomThingR (2, 5)
  points <- replicateM num randomPoint
  values <- replicateM num randomValue
  return $ zip (zip (repeat s) points) values

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

main = do
  putStrLn "Universe created"
  -- putStrLn "Press enter to start..."
  -- getLine
  putStrLn "First sector (18, -4):"
  print $ getSector (18, -4)
  putStrLn "Second sector (19, -4):"
  print $ getSector (19, -4)
  putStrLn "Third sector (18, -3):"
  print $ getSector (18, -3)
  putStrLn "Fourth sector (19, -3):"
  print $ getSector (19, -3)
  putStrLn "Fifth sector (-18, 13):"
  print $ getSector (-18, 13)
  putStrLn "Sixth sector (-19, 13):"
  print $ getSector (-19, 13)
  {-
  let sectors = concat [getSector (x, y) | x <- [0..80], y <- [0..80]]
  let names = map snd sectors
  let namegroups = group . sort $ names
  let lengths :: [Int]
      lengths = map length namegroups
  let lennamepairs = zip lengths namegroups
  let worst = head . reverse $ sortBy (comparing fst) lennamepairs
  -- let dupes = head . reverse . sort . map length . group . sort $ names
  -- print dupes
  -- print . head . group . sort $ names
  print $ worst
  putStrLn "Press enter to quit..."
  getLine
  -}
  hSetBuffering stdin NoBuffering
  runZipper (0, 0)

runZipper :: (Int, Int) -> IO ()
runZipper s@(x, y) = do
  putStrLn ""
  print s
  print $ getSector s
  c <- getChar
  case c of
    'a' -> runZipper (x - 1, y)
    's' -> runZipper (x, y - 1)
    'w' -> runZipper (x, y + 1)
    'd' -> runZipper (x + 1, y)
    _   -> return ()

