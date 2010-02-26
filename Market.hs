module Market(pricesAfterTrade, 
   Good(..),
   getPrice,
   getQuantity,
   getPricesAfterImports,
   Production(..),
   TradeGraph,
   ProductionSource(..),
   Environment(..),
   tradeGraphToTradeList,
   getPriceList,
   getMarketList)
where

import Data.List
import Data.Maybe (catMaybes)
import Data.Function (on)

import qualified Data.Edison.Assoc.StandardMap as M

import Data.Graph
import Data.Graph.Inductive

data Good a = Good {
    getGoodName    :: String
  , getBasePrice   :: Float
  , getProdCoeff   :: Float
  , getEnvironment :: a
  , getNeededTL    :: Float
  }

getGoodPrice :: Float -> Float -> Float -> Float
getGoodPrice produced prodcoeff baseprice = 
  let marketprice = baseprice - produced * prodcoeff
  in max (baseprice / 2) marketprice

producedQuantityCoefficient = 0.01

getProducedQuantity'' :: Float -> Float -> Float -> Float -> Float
getProducedQuantity'' environmentpotential population techlevel neededtl =
  if neededtl > techlevel
    then 0
    else environmentpotential * population * techlevel * producedQuantityCoefficient

data Environment a = Environment {
    productionPotentials :: M.FM a Float
  }

data ProductionSource a = ProductionSource {
    getPopulation  :: Float
  , getTechlevel   :: Float
  , getAllocations :: M.FM a Float
  }

data Production a = Production {
    getProductionSource      :: ProductionSource a
  , getProductionEnvironment :: Environment a
  }

emptyProductionSource = ProductionSource 0 0 M.empty

emptyProductionEnvironment = Environment M.empty

getGoodEnvironmentPotential :: (Ord a) => Environment a -> Good a -> Float
getGoodEnvironmentPotential e g = 
  M.lookupWithDefault 0 (getEnvironment g) (productionPotentials e)

getProducedQuantity' :: (Ord a) => ProductionSource a -> Environment a -> Good a -> Float
getProducedQuantity' p e g =
  getProducedQuantity'' epo allocpop tl (getNeededTL g)
    where epo      = getGoodEnvironmentPotential e g
          allocpop = M.lookupWithDefault 0 (getEnvironment g) (getAllocations p)
          tl       = getTechlevel p

getQuantity :: (Ord a) => Good a -> Production a -> Float
getQuantity g p = getProducedQuantity' (getProductionSource p) (getProductionEnvironment p) g

getPrice :: (Ord a) => Good a -> ProductionSource a -> Environment a -> Float
getPrice g ps e =
  getGoodPrice (getProducedQuantity' ps e g) (getProdCoeff g) (getBasePrice g)

getPrice'' :: (Ord a) => Good a -> Production a -> Float
getPrice'' g p = getPrice g (getProductionSource p) (getProductionEnvironment p)

getPricesAfterImports :: Float -> (Float, Float) -> (Float, Float)
getPricesAfterImports importCoeff (price1, price2) =
  let p1 = min price1 price2
      p2 = max price1 price2
      importerPrice = p1 + (1 - importCoeff) * (p2 - p1)
      exporterPrice = p1
      doSwap = price1 > price2
  in if doSwap then (importerPrice, exporterPrice) else (exporterPrice, importerPrice)

type TradeGraph a = Gr (String, Production a) Float

mkTradeGraph :: [(Int, String, Production a)] -> [(Int, Int, Float)] -> TradeGraph a
mkTradeGraph nds = 
  let split (a, b, c) = (a, (b, c))
  in mkGraph (map split nds)

pricesAfterTrade :: [(Float, (String, Float), (String, Float))] -> M.FM String Float
pricesAfterTrade xs = foldl' go M.empty (sort xs)
  where go acc (impcoeff, (s1, p1), (s2, p2)) =
           M.unionWith min res acc
              where res = M.fromSeq [(s1, p1'), (s2, p2')]
                       where p1n        = M.lookupWithDefault p1 s1 acc
                             p2n        = M.lookupWithDefault p2 s2 acc
                             (p1', p2') = getPricesAfterImports impcoeff (p1n, p2n)

tradeGraphToTradeList :: (Ord a) => Good a -> TradeGraph a -> [(Float, (String, Float), (String, Float))]
tradeGraphToTradeList gd gr = ufold go [] gr
  where go (to, me, (myname, pr), from) acc =
          let myprice = getPrice'' gd pr
              they = to ++ from
              idCoeffToNamePriceCoeff (c, i) = 
                 case lab gr i of
                   Nothing       -> Nothing
                   Just (n, pro) -> Just (n, getPrice'' gd pro, c)
              inlist :: [(String, Float, Float)]
              inlist = catMaybes $ map idCoeffToNamePriceCoeff they
              completelist = map (\(n2, p2, c) -> (c, (myname, myprice), (n2, p2))) inlist
          in completelist ++ acc

getPriceList :: (Ord a) =>
     String -> [Good a] -> TradeGraph a -> [(String, Float)]
getPriceList whose gds tgraph = zip 
      (map getGoodName gds)
      (getPrices whose gds tgraph) 

getPrices whose gds tgraph = 
      (map snd $ 
          concatMap M.toSeq $ 
          map (M.filterWithKey (\k _ -> k == whose)) 
             (map (\g -> pricesAfterTrade $ tradeGraphToTradeList g tgraph) gds))

getMarketList :: (Ord a) =>
     String -> [Good a] -> TradeGraph a -> [(String, Float, Float)]
getMarketList whose gds tgraph = zip3
      (map getGoodName gds)
      (getPrices whose gds tgraph) 
      (getQuantities whose gds tgraph)

nfilter :: (Data.Graph.Inductive.Graph gr) => (a -> Bool) -> gr a b -> [a]
nfilter f = ufold go []
  where go ctxt acc = if f (lab' ctxt) then (lab' ctxt):acc else acc

getQuantities whose gds tgraph = 
      map (flip getQuantity prod) gds
    where prod = case nfilter (\(n, _) -> n == whose) tgraph of
            [n] -> snd n
            _   -> Production emptyProductionSource emptyProductionEnvironment

