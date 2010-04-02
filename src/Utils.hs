module Utils
where

import System.IO
import Data.List hiding (foldl')
import Data.Foldable
import Data.Char
import Control.Monad
import System.Random
import Control.Monad.State

import Collision

getOneChar :: IO Char
getOneChar = do
  b <- hGetBuffering stdin
  hSetBuffering stdin NoBuffering
  c <- getChar
  putStrLn ""
  hSetBuffering stdin b
  return c

readInt :: String -> Maybe Int
readInt = safeRead

safeRead :: (Read a) => String -> Maybe a
safeRead s = case reads s of
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

while :: (Monad m) => m Bool -> m () -> m ()
while n m = do
  r <- n
  if r
    then m >> while n m
    else return ()

until :: (Monad m) => m Bool -> m () -> m ()
until n m = while (liftM not n) m

radToDeg :: (Floating a) => a -> a
radToDeg r = r * 180 / pi

degToRad :: (Floating a) => a -> a
degToRad d = d * pi / 180

lengthF :: (Foldable f) => f a -> Int
lengthF = foldl' (\acc _ -> acc + 1) 0

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

insertRev :: (Ord a) => a -> [a] -> [a]
insertRev x xs = insertBy f x xs
  where f a b = case compare a b of
                  LT -> GT
                  EQ -> EQ
                  GT -> LT

modFst :: (a -> a) -> (a, b) -> (a, b)
modFst f (a, b) = (f a, b)

modSnd :: (b -> b) -> (a, b) -> (a, b)
modSnd f (a, b) = (a, f b)

safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:xs) n | n < 0     = Nothing
                   | n == 0    = Just x
                   | otherwise = safeIndex xs (n - 1)

allEnums :: (Enum a, Bounded a) => [a]
allEnums = [minBound..maxBound]

boxThatIncludes :: (Ord a, Floating a) => 
     (a, a) -- ^ coords of first pos
  -> (a, a) -- ^ coords of second pos
  -> a      -- ^ size of first box
  -> a      -- ^ size of second box
  -> a      -- ^ width
  -> a      -- ^ height
  -> ((a, a), (a, a)) -- ^ box that includes both first and second box
boxThatIncludes (x1, x2) (y1, y2) inc1 inc2 w h =
  let ((minx1, maxx1), (miny1, maxy1)) = boxArea (x1, y1) inc1
      ((minx2, maxx2), (miny2, maxy2)) = boxArea (x2, y2) inc2
      minx = min minx1 minx2
      maxx = max maxx1 maxx2
      miny = min miny1 miny2
      maxy = max maxy1 maxy2
      ratio = w / h
      (midx, midy) = ((maxx + minx) / 2, (maxy + miny) / 2)
      dx = midx - minx
      dy = midy - miny
      dx' = max dx (dy * ratio)
      dy' = max dy (dx * (1/ratio))
      minx' = midx - dx'
      miny' = midy - dy'
      maxx' = midx + dx'
      maxy' = midy + dy'
  in ((minx', maxx'), (miny', maxy'))

{-
randomThing :: (Random t, RandomGen s, MonadState s m) => m t
randomThing = do
  r <- get
  let (a, g) = random r
  put g
  return a
-}


