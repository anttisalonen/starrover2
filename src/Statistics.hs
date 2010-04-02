{-# LANGUAGE Rank2Types #-} 
module Statistics
where

import Data.Foldable
import Control.Monad.State
import System.Random
import Data.List

type Rnd a = (RandomGen g) => State g a

randomRM :: (Random a) => (a, a) -> Rnd a
randomRM v = do
  g <- get
  (x, g') <- return $ randomR v g
  put g'
  return x

choose :: [a] -> Rnd a
choose l = do
  let n = length l
  i <- randomRM (0, n - 1)
  return (l !! i)

chooseIO :: (Foldable f) => f a -> IO a
chooseIO l = do
  let l' = toList l
      n = length l'
  i <- randomRIO (0, n - 1)
  return (l' !! i)

stdNormal :: (Random a, Ord a, Floating a) => Rnd a
stdNormal = do
  u1 <- randomRM (-1, 1)
  u2 <- randomRM (-1, 1)
  let m = stdNormalMarsaglia u1 u2
  case m of
    Nothing      -> stdNormal
    Just (z1, _) -> return z1

stdNormalMarsaglia :: (Ord a, Floating a) => a -> a -> Maybe (a, a)
stdNormalMarsaglia y1 y2 = 
  if q > 1 then Nothing else Just (z1, z2)
  where z1 = y1 * p
        z2 = y2 * p
        q = y1 * y1 + y2 * y2
        p = sqrt ((-2) * log q / q)

normal :: (Random a, Ord a, Floating a) => a -> a -> Rnd a
normal mu sigma = do
  n <- stdNormal
  return $ mu + n * sigma

normalR :: (Random a, Ord a, Floating a) => (a, a) -> a -> a -> Rnd a
normalR (mn, mx) mu sigma = do
  n <- normal mu sigma
  if n < mn 
    then return mn 
    else if n > mx
           then return mx else return n

normalIO :: (Random a, Ord a, Floating a) => a -> a -> IO a
normalIO mu sigma = newStdGen >>= return . evalState (normal mu sigma)

normalRIO :: (Random a, Ord a, Floating a) => (a, a) -> a -> a -> IO a
normalRIO limits mu sigma = newStdGen >>= return . evalState (normalR limits mu sigma)

average :: (Fractional a) => [a] -> a
average l = go 0 0 l
  where go acc len []     = acc / len
        go acc len (x:xs) = go (acc + x) (len + 1) xs

averageInt :: [Int] -> Int
averageInt l = go 0 0 l
  where go acc len []     = acc `div` len
        go acc len (x:xs) = go (acc + x) (len + 1) xs

median :: (Ord a, Num a) => [a] -> a
median [] = error "Median on empty list"
median l = (sort l) !! (length l `div` 2)

-- chance of a to b
chance :: Int -> Int -> Rnd Bool
chance a b = do
  v <- randomRM (1, b)
  if a <= v - 1
   then return True
   else return False

-- shuffle :: [a] -> Rnd [a]
shuffle l = shuffle' l [] -- >>= reverse >>= flip shuffle' []
  where shuffle' [] bs = return bs
        shuffle' as bs = do
          el <- choose as
          shuffle' (delete el as) (el : bs)

