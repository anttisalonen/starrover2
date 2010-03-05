module MarketData
where

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


