module Console
where

class Displayable a where
  display :: a -> String
  displayShort :: a -> String
  displayShort = display

instance (Displayable a) => Displayable [a] where
  display = concatMap display
  displayShort = concatMap displayShort

display_ :: (Displayable a) => a -> IO ()
display_ = putStr . display

