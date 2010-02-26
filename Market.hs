module Market
where

import Data.List
import Data.Maybe

import qualified Data.Edison.Assoc.StandardMap as M

import Data.Graph
import Data.Graph.Inductive

data Good = Good {
    getGoodName    :: String
  , getBasePrice   :: Float
  , getProdCoeff   :: Float
  , getEnvironment :: GoodProduction
  , getNeededTL    :: Float
  }

grain         = Good "Grain"          5.0  0.2 Agriculture 0
syntheticMeat = Good "Synthetic meat" 50.0 0.1 Factory     90
coal          = Good "Coal"           10.0 0.2 Mining      10

goods =
 [
   grain
 , syntheticMeat
 , coal
 ]

getPrice :: Float -> Float -> Float -> Float
getPrice produced prodcoeff baseprice = 
  let marketprice = baseprice - produced * prodcoeff
  in max (baseprice / 2) marketprice

producedQuantityCoefficient = 0.01

getProducedQuantity :: Float -> Float -> Float -> Float -> Float
getProducedQuantity environmentpotential population techlevel neededtl =
  if neededtl > techlevel
    then 0
    else environmentpotential * population * techlevel * producedQuantityCoefficient

data GoodProduction = Agriculture
                    | Mining
                    | Factory
  deriving (Ord, Eq)

data Environment = Environment {
    productionPotentials :: M.FM GoodProduction Float
  }

moonEnvironment  = Environment (M.fromSeq [(Agriculture, 0),   (Mining, 0),   (Factory, 10)])
earthEnvironment = Environment (M.fromSeq [(Agriculture, 100), (Mining, 100), (Factory, 100)])
marsEnvironment  = Environment (M.fromSeq [(Agriculture, 10),  (Mining, 200), (Factory, 50)])

data ProductionSource = ProductionSource {
    getPopulation  :: Float
  , getTechlevel   :: Float
  , getAllocations :: M.FM GoodProduction Float
  }

data Production = Production {
    getProductionSource      :: ProductionSource
  , getProductionEnvironment :: Environment
  }

moonProductionSource  = ProductionSource 100000  90  (M.fromSeq [(Agriculture, 0.0), (Mining, 0.0), (Factory, 1.0)])
earthProductionSource = ProductionSource 9000000 100 (M.fromSeq [(Agriculture, 0.4), (Mining, 0.2), (Factory, 0.4)])
marsProductionSource  = ProductionSource 400000  60  (M.fromSeq [(Agriculture, 0.1), (Mining, 0.7), (Factory, 0.2)])

moonProduction  = Production moonProductionSource  moonEnvironment
earthProduction = Production earthProductionSource earthEnvironment
marsProduction  = Production marsProductionSource marsEnvironment

getGoodEnvironmentPotential :: Environment -> Good -> Float
getGoodEnvironmentPotential e g = 
  M.lookupWithDefault 0 (getEnvironment g) (productionPotentials e)

getProducedQuantity' :: ProductionSource -> Environment -> Good -> Float
getProducedQuantity' p e g =
  getProducedQuantity epo allocpop tl (getNeededTL g)
    where epo      = getGoodEnvironmentPotential e g
          allocpop = M.lookupWithDefault 0 (getEnvironment g) (getAllocations p)
          tl       = getTechlevel p

getPrice' :: Good -> ProductionSource -> Environment -> Float
getPrice' g ps e =
  getPrice (getProducedQuantity' ps e g) (getProdCoeff g) (getBasePrice g)

getPrice'' :: Good -> Production -> Float
getPrice'' g p = getPrice' g (getProductionSource p) (getProductionEnvironment p)

getPricesAfterImports :: Float -> (Float, Float) -> (Float, Float)
getPricesAfterImports importCoeff (price1, price2) =
  let p1 = min price1 price2
      p2 = max price1 price2
      importerPrice = p1 + (1 - importCoeff) * (p2 - p1)
      exporterPrice = p1
      doSwap = price1 > price2
  in if doSwap then (importerPrice, exporterPrice) else (exporterPrice, importerPrice)

moongrainprice = getPrice' grain moonProductionSource moonEnvironment 
earthgrainprice = getPrice' grain earthProductionSource earthEnvironment 
marsgrainprice = getPrice' grain marsProductionSource marsEnvironment 
moonsyntheticmeatprice = getPrice' syntheticMeat moonProductionSource moonEnvironment 
earthsyntheticmeatprice = getPrice' syntheticMeat earthProductionSource earthEnvironment 
marssyntheticmeatprice = getPrice' syntheticMeat marsProductionSource marsEnvironment 
mooncoalprice = getPrice' coal moonProductionSource moonEnvironment 
earthcoalprice = getPrice' coal earthProductionSource earthEnvironment 
marscoalprice = getPrice' coal marsProductionSource marsEnvironment 

tradeEarthMars = 0.60 :: Float
tradeEarthMoon = 0.95 :: Float
tradeMoonMars  = 0.30 :: Float

(marsCoal1, earthCoal1) = getPricesAfterImports tradeEarthMars (marscoalprice , earthcoalprice )
(marsCoal2, moonCoal1)  = getPricesAfterImports tradeMoonMars (minimum [marscoalprice, marsCoal1] , mooncoalprice )
(earthCoal2, moonCoal2) = getPricesAfterImports tradeEarthMoon  (minimum [earthCoal1, earthcoalprice] , minimum [moonCoal1, mooncoalprice] )

earthCoalPrice = minimum [earthCoal1, earthCoal2]
moonCoalPrice = minimum [moonCoal1, moonCoal2]
marsCoalPrice = minimum [marsCoal1, marsCoal2]

earthMoonMarsTriangle :: TradeGraph
earthMoonMarsTriangle = mkGraph [
  (1, ("Earth", earthProduction)),
  (2, ("Moon",  moonProduction)),
  (3, ("Mars",  marsProduction))] 
    [(1, 2, 0.95), (1, 3, 0.60), (2, 3, 0.30)]

type TradeGraph = Gr (String, Production) Float

mkTradeGraph :: [(Int, String, Production)] -> [(Int, Int, Float)] -> TradeGraph
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

tradeGraphToTradeList :: Good -> TradeGraph -> [(Float, (String, Float), (String, Float))]
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

