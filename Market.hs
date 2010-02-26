module Market
where

import qualified Data.Edison.Assoc.StandardMap as M

{-
data Market = Market {
    techlevel  :: Int
  , government :: Government
  , population :: Int
  , resources  :: [(Good, Int)]
  , imports    :: [(Good, Int)]
  }
-}

data Good = Good {
    getGoodName    :: String
  , getBasePrice   :: Float
  , getPriceCoeff  :: Float
  , getProdCoeff   :: Float
  , getEnvironment :: GoodProduction
  , getNeededTL    :: Float
  }

grain         = Good "Grain"          5.0  0.02 0.2 Agriculture 0
syntheticMeat = Good "Synthetic meat" 50.0 0.02 0.2 Factory     90

goods =
 [
   grain
 , syntheticMeat
 ]

getPrice :: Float -> Float -> Float -> Float -> Float
getPrice imported produced prodcoeff baseprice = 
  let total       = imported + produced
      marketprice = baseprice - total * prodcoeff
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

moonEnvironment  = Environment (M.fromSeq [(Agriculture, 0), (Mining, 0), (Factory, 10)])
earthEnvironment = Environment (M.fromSeq [(Agriculture, 100), (Mining, 100), (Factory, 100)])

data ProductionSource = ProductionSource {
    getPopulation  :: Float
  , getTechlevel   :: Float
  , getAllocations :: M.FM GoodProduction Float
  }

moonProduction  = ProductionSource 100000  90  (M.fromSeq [(Agriculture, 0.0), (Mining, 0.0), (Factory, 1.0)])
earthProduction = ProductionSource 9000000 100 (M.fromSeq [(Agriculture, 0.4), (Mining, 0.2), (Factory, 0.4)])

getGoodEnvironmentPotential :: Environment -> Good -> Float
getGoodEnvironmentPotential e g = 
  M.lookupWithDefault 0 (getEnvironment g) (productionPotentials e)

getProducedQuantity' :: ProductionSource -> Environment -> Good -> Float
getProducedQuantity' p e g =
  getProducedQuantity epo allocpop tl (getNeededTL g)
    where epo      = getGoodEnvironmentPotential e g
          allocpop = M.lookupWithDefault 0 (getEnvironment g) (getAllocations p)
          tl       = getTechlevel p

getPrice' :: Float -> ProductionSource -> Environment -> Good -> Float
getPrice' imported ps e g =
  getPrice imported (getProducedQuantity' ps e g) (getProdCoeff g) (getBasePrice g)
