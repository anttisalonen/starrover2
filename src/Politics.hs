module Politics
where

import Control.Monad.State

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import qualified Data.Edison.Assoc.StandardMap as Map

import Life

data Relation = Unknown
              | ColonyOf
              | Peace
              | War
  deriving (Show)

type RelationGraph = Gr String Relation

emptyRelationGraph :: RelationGraph
emptyRelationGraph = empty

emptyGraphState :: GraphState
emptyGraphState = (empty, [])

type GraphState = (RelationGraph, [(String, Node)])

addNodeToMap :: String -> State GraphState Int
addNodeToMap str = do
  (gr, nodemap) <- get
  let maxnode = if null nodemap
                  then 1
                  else 1 + (snd $ head nodemap)
  put (gr, (str, maxnode):nodemap)
  return maxnode

getNodeInMap :: String -> State GraphState Node
getNodeInMap str = do
  (gr, nodemap) <- get
  let mnode = lookup str nodemap 
  case mnode of
    Nothing -> addNodeToMap str
    Just n  -> return n

-- modifyFst :: (MonadState (f, s) m) => (f -> f) -> m ()
modifyFst fun = do
  (f, s) <- get
  let f' = fun f
  put (f', s)

-- modifySnd :: (MonadState (f, s) m) => (s -> s) -> m ()
modifySnd fun = do
  (f, s) <- get
  let s' = fun s
  put (f, s')

addRelation :: String -> Relation -> String -> State GraphState ()
addRelation c1 r c2 = do
  n1 <- getNodeInMap c1
  n2 <- getNodeInMap c2
  modifyFst (insEdge (n1, n2, r))

