{-# LANGUAGE DeriveTraversable #-}
module WarehousePlanner.Slices where 
import ClassyPrelude hiding (uncons, stripPrefix, unzip)
import WarehousePlanner.Type (BoxBreak(..))
-- | An ordered list. Modifying it using fmap doesn't reorder it.
-- It is so that we can work with infinite list.
-- Therefore fmap should be used with caution and make sure
-- the order is kept.
newtype OrderedList a = OrderedList [a] deriving (Eq, Show, Foldable, Functor)

newtype Slot k a = Slot (OrderedList (k, a))
  deriving (Eq, Show, Functor, Foldable)
newtype Slice k a = Slice (OrderedList (k, Slot k a))
  deriving (Eq, Show, Functor, Foldable)
-- | Everything within slices are supposed to be sorted according
-- the k. When using fmap (etc) only monotone function should be used.
newtype Slices k a = Slices (OrderedList (k, Slice k a))
  deriving (Eq, Show, Functor, Foldable)

{-# COMPLETE SlotO #-}
pattern SlotO xs = Slot (OrderedList xs)
{-# COMPLETE SliceO #-}
pattern SliceO xs  = Slice (OrderedList xs)
{-# COMPLETE SlicesO #-}
pattern SlicesO xs = Slices (OrderedList xs)
  
mergeWith :: Ord k => (a -> a -> [a]) -> OrderedList (k, a) -> OrderedList (k, a) -> OrderedList (k, a)
mergeWith combine (OrderedList xs) (OrderedList ys) = OrderedList (go xs ys) where
    go [] ys = ys
    go xs [] = xs
    go (x:xs) (y:ys) = 
      case compare (fst x) (fst y) of
        LT -> x : go xs (y:ys)
        EQ -> (map (\v -> (fst x,v)) ( snd x `combine` snd y)) <> go xs ys
        GT -> y : go (x:xs) ys


instance Ord k => Semigroup (Slot k a) where
  Slot xs <> Slot ys = Slot (mergeWith (\a b -> [a,b]) xs ys)
  
instance Ord k => Monoid (Slot k a) where
  mempty = Slot (OrderedList [])
  
instance Ord k => Semigroup (Slice k a) where
  Slice xs <> Slice ys = Slice (mergeWith (\a b -> [a <> b]) xs ys)
  
instance Ord k => Monoid (Slice k a) where
  mempty = Slice (OrderedList [])
  
instance Ord k => Semigroup (Slices k a) where
  Slices xs <> Slices ys = Slices (mergeWith (\a b -> [a <> b]) xs ys)
  
instance Ord k => Monoid (Slices k a) where
  mempty = Slices (OrderedList [])
  
instance Bifunctor Slot where
  bimap l r (Slot key'poss) = Slot $ fmap (bimap l r) key'poss
  
instance Bifunctor Slice where
  bimap l r (Slice key'slots) = Slice $ fmap (bimap l (bimap l r)) key'slots
  
instance Bifunctor Slices where
  bimap l r (Slices key'slices) = Slices $ fmap (bimap l (bimap l r)) key'slices
  
-------------------------------------------------- {{{ 1
-- * Utilities         
--------------------------------------------------
buildSlices :: Int -> Int -> Int -> (Int -> k) -> (Int -> Int -> k) ->  (Int -> Int -> Int -> (k, a)) -> Slices k a
buildSlices nbOfSlices nbOfSlots nbPerSlot mkSliceIndex mkSlotIndex mkPos = let
  mkSlice sliceIndex = 
    (mkSliceIndex sliceIndex, Slice . OrderedList $ map (mkSlot sliceIndex) [0..nbOfSlots - 1])
  mkSlot sliceIndex slotIndex =
    (mkSlotIndex sliceIndex slotIndex, Slot . OrderedList $ map (mkPos sliceIndex slotIndex ) [0..nbPerSlot - 1])
  in Slices . OrderedList $ map mkSlice [0..nbOfSlices - 1]

numSlices :: (Num n, Enum n) => Slices k a -> Slices (n, k) a
numSlices (SlicesO slices) = SlicesO $ [bimap (i,) numSlice slice | (slice, i) <- zip slices [0..]]

numSlice :: (Num n, Enum n) => Slice k a -> Slice (n, k) a
numSlice (SliceO slots) = SliceO $ [bimap (i,) numSlot slot | (slot, i) <- zip slots [0..]]

numSlot :: (Num n, Enum n) => Slot k a -> Slot (n, k) a
numSlot (SlotO poss) = SlotO $ [bimap (i,) id pos | (pos, i) <- zip poss [0..]]
unconsSlot :: Slot k a -> Maybe (a,k, Slot k a)
unconsSlot (Slot (OrderedList key'poss)) =
  case key'poss of
    [] -> Nothing
    ((key,pos):kps) -> Just (pos,key, Slot (OrderedList kps))
    
unconsSlice :: Slice k a -> Maybe (a,(k,k), Slice k a)
unconsSlice (Slice (OrderedList key'slots)) =
  case key'slots of
    [] -> Nothing
    ((key, slot0):kss) -> 
      case unconsSlot slot0 of
        Nothing -> unconsSlice (Slice $ OrderedList kss)
        Just (pos, k1, slot) -> Just (pos, (key, k1), Slice (OrderedList $ (key, slot):kss))
    
unconsSlices :: Slices k a -> Maybe (a, (k,k,k), Slices k a)
unconsSlices (Slices (OrderedList key'slices)) =
  case key'slices of
    [] -> Nothing
    ((key, slice0):kss) ->
      case unconsSlice slice0 of
        Nothing -> unconsSlices (Slices $ OrderedList kss)
        Just (pos, (k1,k2), slice) -> Just (pos, (key,k1,k2), Slices (OrderedList $ (key, slice):kss))


  

unconsSlicesTo :: Eq shelf => (a -> shelf) -> Maybe shelf -> Maybe BoxBreak -> Slices (Int, k) a -> Maybe (a, ((Int, k),(Int, k),(Int, k)), Slices (Int, k) a)
unconsSlicesTo _ _ Nothing slices = unconsSlices slices
unconsSlicesTo _ _ _ (Slices (OrderedList [])) = Nothing
unconsSlicesTo getShelf  Nothing (Just StartNewShelf) slices = unconsSlicesTo getShelf Nothing (Just StartNewSlice) slices
unconsSlicesTo getShelf js@(Just prevShelf) (Just StartNewShelf) slices = 
  -- try next slice until different shelf
  case unconsSlicesTo getShelf Nothing (Just StartNewSlice) slices of
    Nothing -> Nothing
    Just new@(a, _, _) | getShelf a /= prevShelf -> Just new
    Just (_, _, newSlices) -> 
      unconsSlicesTo getShelf js (Just StartNewShelf) newSlices
unconsSlicesTo _ _ (Just StartNewSlice) slices =
  case unconsSlices slices of
    Nothing -> Nothing
    Just new@(_, (_,(0,_),_), _) -> Just new
    --               ^
    --               +-- new slices mean slot number = 0
    _ -> unconsSlices (dropTillSlice slices)
unconsSlicesTo _ _ (Just StartNewSlot) slices =
  case unconsSlices slices of
    Nothing -> Nothing
    Just new@(_, (_,_,(0,_)), _) -> Just new
    _ -> unconsSlices (dropTillSlot slices)

dropTillSlice, dropTillSlot :: Slices k a -> Slices k a
dropTillSlice (SlicesO slices) = cleanSlices $ SlicesO (drop 1 slices)
dropTillSlot (SlicesO ((i, SliceO (_:slots)):slices)) = cleanSlices $ SlicesO (slice:slices) where
  slice = (i, SliceO slots)
dropTillSlot _ = SlicesO []

-- | Remove empty sublists
cleanSlices :: Slices k a -> Slices k a
cleanSlices (SlicesO ((_, SliceO []):slices)) = cleanSlices $ SlicesO (map (second cleanSlice) slices)
cleanSlices slices = slices

cleanSlice :: Slice k a -> Slice k a
cleanSlice (SliceO ((_, SlotO []):slots)) = cleanSlice $ SliceO slots
cleanSlice slice = slice

  

filterSlices :: (a -> Bool)  -> Slices k a -> Slices k a
filterSlices keep = filterSlicesWithKey (\_ a ->  keep a)
filterSlicesWithKey :: (k -> a -> Bool)  -> Slices k a -> Slices k a
filterSlicesWithKey keep (SlicesO xs) = cleanSlices $ SlicesO $ map (second $ uncleanFilterSliceWithKey keep) xs
-- 
uncleanFilterSliceWithKey :: (k -> a -> Bool) -> Slice k a -> Slice k a
uncleanFilterSliceWithKey keep (SliceO xs) = SliceO (map (second $ uncleanFilterSlotWithKey keep) xs)

uncleanFilterSlotWithKey :: (k  -> a -> Bool) -> Slot k a -> Slot k a
uncleanFilterSlotWithKey keep (SlotO xs)  = SlotO $ filter (uncurry keep) xs
