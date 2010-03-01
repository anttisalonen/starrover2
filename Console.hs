module Console
where

class Displayable a where
  display :: a -> String

instance (Displayable a) => Displayable [a] where
  display = concatMap display
