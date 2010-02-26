module MarketTestData
where

import qualified Data.Edison.Assoc.StandardMap as M
import Data.Graph.Inductive

import Market

data GoodProduction = Agriculture
                    | Mining
                    | Factory
  deriving (Ord, Eq)

grain         = Good "Grain"          5.0  0.2 Agriculture 0
syntheticMeat = Good "Synthetic meat" 50.0 0.1 Factory     90
coal          = Good "Coal"           10.0 0.2 Mining      10

goods =
 [
   grain
 , syntheticMeat
 , coal
 ]

moonEnvironment  = Environment (M.fromSeq [(Agriculture, 0),   (Mining, 0),   (Factory, 10)])
earthEnvironment = Environment (M.fromSeq [(Agriculture, 100), (Mining, 100), (Factory, 100)])
marsEnvironment  = Environment (M.fromSeq [(Agriculture, 10),  (Mining, 200), (Factory, 50)])

moonProductionSource  = ProductionSource 100000  90  (M.fromSeq [(Agriculture, 0.0), (Mining, 0.0), (Factory, 1.0)])
earthProductionSource = ProductionSource 9000000 100 (M.fromSeq [(Agriculture, 0.4), (Mining, 0.2), (Factory, 0.4)])
marsProductionSource  = ProductionSource 400000  60  (M.fromSeq [(Agriculture, 0.1), (Mining, 0.7), (Factory, 0.2)])

moonProduction  = Production moonProductionSource  moonEnvironment
earthProduction = Production earthProductionSource earthEnvironment
marsProduction  = Production marsProductionSource marsEnvironment

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

earthMoonMarsTriangle :: TradeGraph GoodProduction
earthMoonMarsTriangle = mkGraph [
  (1, ("Earth", earthProduction)),
  (2, ("Moon",  moonProduction)),
  (3, ("Mars",  marsProduction))] 
    [(1, 2, 0.95), (1, 3, 0.60), (2, 3, 0.30)]


