module Main
where

import Data.List
import Data.Char
import System.Random
import Control.Monad.State
import Data.Bits
import Data.Ord
import System.IO

import Sector
import Utils

type Key = (Sector, Point)

type Point = (Int, Int)

type Value = String

pointRange :: Point
pointRange = (0, 100)

getKeyValues :: Sector -> [(Key, Value)]
getKeyValues s = getPartsR (2, 5) (do
    point <- randomPoint
    value <- randomValue
    return ((s, point), value)
  ) s

getSector :: Sector -> [Value]
getSector s = map snd (getKeyValues s)

randomValue :: RndS String
randomValue = do
  n <- randomThingR (2, 8)
  s <- replicateM n (randomThingR ('a', 'z'))
  return (capitalize s)

capitalize []     = []
capitalize (h:hs) = toUpper h : hs

randomPoint :: RndS Point
randomPoint = randomPair pointRange

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

