{-# LANGUAGE NoMonomorphismRestriction #-}
module Cargo
where

import System.Random
import Text.Printf
import Data.List
import Control.Monad
import Control.Monad.State as State

import qualified Data.Edison.Assoc.StandardMap as M
import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import Graphics.Rendering.FTGL as FTGL

import Statistics
import TextScreen
import Space
import SDLUtils

type Cargo = M.FM String Int
type Market = M.FM String (Int, Int)

cargonames = sort ["Grain", "Fruit", "Gem stones", "Firearms"]

randomCargo :: IO String
randomCargo = chooseIO cargonames

showCargo :: Cargo -> String
showCargo c | M.null c  = "No cargo\n"
            | otherwise = concatMap (\(k, v) -> printf "%-20s-%4d\n" k v) (M.toOrdSeq c) 

showMarket :: Market -> String
showMarket m = title ++ infos
  where title = printf "%-16s%8s%8s\n" "Good" "Quantity" "Price"
        infos = concatMap (\(n, (q, p)) -> printf "%-20s%10d%10d\n" n q p) (M.toOrdSeq m)

numCargoItems = length cargonames

randomMarket :: IO Market
randomMarket = do
  let names = cargonames
  prices <- replicateM numCargoItems $ randomRIO (5, 15)
  quantities <- replicateM numCargoItems $ randomRIO (0, 30)
  return $ M.fromSeq $ zip names (zip quantities prices)

fitCargo :: Cargo -> [(String, Int)]
fitCargo c = map (\s -> (s, M.lookupWithDefault 0 s c)) cargonames

fromMarket :: Int -> String -> Market -> Market
fromMarket q = toMarket (-q)

fromCargo :: Int -> String -> Cargo -> Cargo
fromCargo q = toCargo (-q)

toMarket :: Int -> String -> Market -> Market
toMarket q = M.adjust (\(q', p) -> (q' + q, p))

toCargo :: Int -> String -> Cargo -> Cargo
toCargo q n = M.insertWith (+) n q

showMarketAndCargo :: Market -> Cargo -> String
showMarketAndCargo m c = title ++ infos
  where title = printf "%-14s%9s%6s%6s\n" "Good" "Quantity" "Price" "Cargo"
        infos = concatMap (\((n, (q, p)), (_, q')) -> printf "%-14s%9d%6d%6d\n" n q p q') (zip (M.toOrdSeq m) (fitCargo c))

type TradeState = (Market, Cargo, Int)

buy :: Int -> String -> StateT TradeState IO ()
buy q n = do
  (market, cargo, cash) <- State.get
  let mval = M.lookupM n market
  case mval of
    Nothing      -> return ()
    Just (q', p) -> do
      let totalq = min (max 0 q') q
      let totalp = totalq * p
      if totalp > cash || totalq == 0
        then return ()
        else do
          State.put (fromMarket totalq n market,
               toCargo totalq n cargo,
               subtract totalp cash)

sell :: Int -> String -> StateT TradeState IO ()
sell q n = do
  (_, cargo, _) <- State.get
  let mval = M.lookupM n cargo
  case mval of
    Nothing -> return ()
    Just q' -> buy (negate (min q' q)) n

tradeScreen :: String -> Font -> Font -> StateT TradeState IO ()
tradeScreen str f1 f2 = do
  (market, _, _) <- State.get
  let exitb = ((100, 100), (100, 30))
      buybuttons  = map (\i -> ((550, 440 - 50 * fromIntegral i), (100, 30))) [1..numCargoItems]
      sellbuttons = map (\i -> ((680, 440 - 50 * fromIntegral i), (100, 30))) [1..numCargoItems]
      buyactions  = map (\(n, (_, _)) -> buy  1 n >> return Nothing) (M.toOrdSeq market)
      sellactions = map (\(n, (_, _)) -> sell 1 n >> return Nothing) (M.toOrdSeq market)
      allbuttons  = exitb : (buybuttons ++ sellbuttons)
      allactions  = return (Just ()) : (buyactions ++ sellactions)
      bttoaction  = zip allbuttons allactions
  let handleInput = do
        events <- liftIO $ pollAllSDLEvents
        let mbutton = mouseClickInAny height [ButtonLeft] allbuttons events
        case mbutton of
          Nothing -> return Nothing
          Just n  -> case lookup n bttoaction of
                       Just act -> act
                       Nothing  -> return Nothing
  loopTextScreen (do (market', cargo, cash) <- State.get
                     liftIO $ makeTextScreen (10, 500) 
                               [(f1, Color4 1.0 1.0 1.0 1.0, str),
                                (f2, Color4 1.0 1.0 0.0 1.0, showMarketAndCargo market' cargo),
                                (f1, Color4 1.0 1.0 1.0 1.0, "Cash: " ++ show cash)]
                               (drawButton "Exit" f1 exitb >>
                                mapM_ (drawButton "Buy" f1) buybuttons >>
                                mapM_ (drawButton "Sell" f1) sellbuttons))
                 handleInput

