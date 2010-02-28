module Console
where

class Displayable a where
  display :: a -> String

