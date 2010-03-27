module Politics(Relation(..), mkRelationshipMap, Friendliness,
  AttitudeMap,
  nullAttitudes
  )
where

import Data.List

import qualified Data.Edison.Assoc.StandardMap as M

data Relation = Unknown
              | Peace
              | War
              | ColonyOf
              | Allied
  deriving (Show)

type Friendliness = Int
type RelationshipMap = M.FM (String, String) Relationship
type Relationship = (Relation, Friendliness)

emptyRelationshipMap :: RelationshipMap
emptyRelationshipMap = M.empty

setRelationship :: Relationship -> String -> String -> RelationshipMap -> RelationshipMap
setRelationship r s1 s2 = M.insert (s1, s2) r

setRelationship2 :: Relationship -> String -> String -> RelationshipMap -> RelationshipMap
setRelationship2 r s1 s2 = setRelationship r s1 s2 . setRelationship r s2 s1

modRelationship :: (Relationship -> Relationship) -> String -> String -> RelationshipMap -> RelationshipMap
modRelationship f s1 s2 = M.adjustOrInsert f (f (Unknown, 0)) (s1, s2)

modRelationship2 :: (Relationship -> Relationship) -> String -> String -> RelationshipMap -> RelationshipMap
modRelationship2 f s1 s2 = modRelationship f s1 s2 . modRelationship f s2 s1

mkRelationshipMap :: [((String, String), Relationship)] -> RelationshipMap
mkRelationshipMap = foldl' go emptyRelationshipMap
  where go acc ((s1, s2), r) = setRelationship2 r s1 s2 acc

type AttitudeMap = M.FM String Friendliness

nullAttitudes :: [String] -> AttitudeMap
nullAttitudes as = M.fromSeq $ zip as (repeat 0)

