module SpaceState.Difficulty
where

import Graphics.Rendering.OpenGL as OpenGL

data Difficulty = Easy
                | Medium
                | Hard
  deriving (Enum, Bounded, Show, Read, Ord, Eq)

diffcoeff :: Difficulty -> Int
diffcoeff Easy   = 1
diffcoeff Medium = 2
diffcoeff Hard   = 3

difficultyAIshift :: Difficulty -> GLdouble
difficultyAIshift Easy = (-0.3)
difficultyAIshift Medium = 0
difficultyAIshift Hard = 0.3


