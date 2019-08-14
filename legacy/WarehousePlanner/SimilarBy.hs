{-# LANGUAGE PatternSynonyms #-}
module WarehousePlanner.SimilarBy
( SimilarBy
, unSimilar
, similarKey
, pattern SimilarBy
, groupSimilar
, sortSimilarOn
, dropSimilar
)
where
import Prelude
import Data.List(groupBy, sortOn)
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

sortSimilarOn :: (Ord b) => (a -> b) -> SimilarBy k a -> SimilarBy k a
sortSimilarOn fn  (SimilarByPrivate k x xs) = case sortOn fn (x:xs) of
  [] -> error "Shouldn't happend: initial ilst is not empty"
  (x:xs) -> SimilarByPrivate k x xs

dropSimilar :: Int -> SimilarBy k a -> Maybe (SimilarBy k a)
dropSimilar n (SimilarByPrivate k x xs) = case drop n (x:xs) of
  [] -> Nothing
  (y:ys) -> Just $ SimilarByPrivate k y ys
