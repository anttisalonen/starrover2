module Tree(Tree(..), mapInner)
where

import Data.Foldable

data Tree a b = Leaf b | Node a [Tree a b]
  deriving (Eq, Read, Show)

mapTree :: (b -> c) -> Tree a b -> Tree a c
mapTree f (Leaf b)    = Leaf (f b)
mapTree f (Node a ts) = Node a (map (mapTree f) ts)

mapInner :: (a -> c) -> Tree a b -> Tree c b
mapInner _ (Leaf b)    = Leaf b
mapInner f (Node a ts) = Node (f a) (map (mapInner f) ts)

instance Functor (Tree a) where
  fmap = mapTree

foldTreer :: (a -> b -> b) -> b -> Tree c a -> b
foldTreer f v (Leaf a)        = f a v
foldTreer _ v (Node _ [])     = v
foldTreer f v (Node c (t:ts)) = foldTreer f (foldTreer f v t) (Node c ts)

instance Foldable (Tree a) where
  foldr = foldTreer

