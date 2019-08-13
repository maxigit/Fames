{-# LANGUAGE PatternSynonyms #-}
module WarehousePlanner.SimilarBy
( SimilarBy
, unSimilar
, similarKey
, pattern SimilarBy
, groupSimilar
)
where
import Prelude
import Data.List(groupBy)
import Data.Function(on)

data SimilarBy k a = SimilarByPrivate k a [a]


-- pattern SimilarP :: SimilarBy k a ->  
pattern SimilarBy k x xs <- SimilarByPrivate k x xs

unSimilar (SimilarByPrivate _ x xs) = x:xs
similarKey (SimilarByPrivate k _ _ ) = k

-- | Similar to prelude `group`, ie doesn't sort
groupSimilar :: (Eq k) => (a -> k) -> [a] ->  [SimilarBy k a]
groupSimilar k  xs = let
  groups = groupBy ((==) `on` k) xs
  mkSimilar (x:xs) = SimilarByPrivate (k x) x xs
  mkSimilar [] = error "Should not happend because groupBy doesn't create empty group"
  in map mkSimilar groups
