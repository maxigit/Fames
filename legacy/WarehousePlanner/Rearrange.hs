{-# LANGUAGE NamedFieldPuns #-}
{-# Language CPP #-}
module WarehousePlanner.Rearrange
where 

import ClassyPrelude;
import  WarehousePlanner.Base
import Data.Text(splitOn, split)
-- import qualified Data.Map as Map
import Control.Monad (zipWithM)
import Control.Monad.State (modify)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

-- * Type {{{1
data ShiftStrategy = ShiftAll | StayInPlace deriving (Show, Eq)

data ForUnused = DeleteUnused | KeepUnused 
     deriving (Show, Eq)
 
data ForGrouping = Don'tGroup | GroupByContent
     deriving (Show, Eq)

-- * Export {{1
rearrangeBoxesByContent ::  ForUnused -> ForGrouping -> [Tag'Operation] -> (Box s -> Bool) -> BoxSelector s -> [(Box s -> Maybe (Shelf s) -> Bool, ShiftStrategy)] -> WH [Box s] s
rearrangeBoxesByContent deleteUnused groupByContent tagOps isUsed boxsel actions = do
  boxes <- findBoxByNameAndShelfNames boxsel >>= mapM findBox
  -- group boxes by style and content
  let onContent = (`on` \b -> (boxStyle b, boxContent b))
      boxByContent = case groupByContent of
                     GroupByContent ->  groupBy (onContent (==)) $ sortBy (onContent compare) boxes
                     Don'tGroup ->  [boxes]
  newss <- mapM (\boxes ->  shiftUsedBoxes isUsed boxes actions) boxByContent
  let news = concat newss
      untagOps = negateTagOperations tagOps
      newSet = Set.fromList news
      unMoved = Set.fromList boxes \\ newSet
  toUntag <- case deleteUnused of
             DeleteUnused -> do
                let (toDelete, toKeep) = partition isUsed $ toList unMoved
                deleteBoxes toDelete
                return toKeep
             KeepUnused ->  
                  return $ toList unMoved

  void $ zipWithM (updateBoxTags untagOps) toUntag [1..]
  zipWithM (updateBoxTags tagOps) news [1..]
            

        
  
shiftUsedBoxes :: forall s . (Box s -> Bool) -> [Box s] -> [(Box s -> Maybe (Shelf s) -> Bool, ShiftStrategy)] -> WH [Box s] s
shiftUsedBoxes isUsed boxes inBucket'strategies = do
  box'shelves <- mapM (\b -> (return $ boxShelf b) >>= mapM findShelf >>= \s -> return (b, s)) boxes
  let swaps = shiftUsedInBucketsWithStrategy isUsed (map mkBucket inBucket'strategies)
      mkBucket (inBucket, strategy) = (map fst $ filter (uncurry inBucket) box'shelves, strategy)
  newms <- mapM doSwap swaps
  return $ catMaybes newms
  where doSwap :: (Box s, Box s) -> WH (Maybe (Box s)) s
        doSwap (source, dest) | source == dest = return Nothing
        doSwap (source, dest) = do
                 new <- updateBox (\s -> s { boxOffset = boxOffset  dest
                                  , boxBoxOrientations = boxBoxOrientations dest
                                  , orientation = orientation dest
                                  , boxBreak = boxBreak dest
                                  }
                                  ) source
                                  >>= assignShelf (boxShelf dest)

                 return $ Just new

-- * Parse {{1
-- Actions are of the shape
-- [/][#] selector1 >[!] selector2.1 [|] selector2.2 > [!]selector3
-- 
parseActions :: Text -> (ForUnused, ForGrouping, [(Box s -> Maybe (Shelf s) -> Bool, ShiftStrategy)])
parseActions s0 = let
  (flags, s1) = span (`elem` t "/-%") s0
  asLocation = '/' `elem` flags
  ss = splitOn ">" s1
  forUnused = if '-' `elem` flags
              then DeleteUnused
              else KeepUnused
  forGrouping = if '%' `elem` flags
                then Don'tGroup
                else GroupByContent
  mkFn ss' = let
    (c, ss) = span (`elem` t "!_ ") ss'
    strat = if '!' `elem` c
            then StayInPlace
            else ShiftAll
    selectors = parseSelectors asLocation ss
    in (\box shelfm -> orTrue [ applyNameSelector  (nameSelector boxSel) boxStyle box
                            && applyTagSelectors (tagSelectors boxSel) boxTags box
                            && case shelfm of
                                 Nothing | SelectAnything <- shelfSel  -> True
                                 Nothing -> False
                                 Just shelf -> applyNameSelector (nameSelector shelfSel) shelfName shelf
                                               && applyTagSelectors (tagSelectors shelfSel) shelfTag shelf
                          | BoxSelector boxSel shelfSel _ <- selectors

                          ]
       , strat
       )
  in (forUnused, forGrouping, reverse (map mkFn ss))
  
-- | 
parseSelectors :: Bool -> Text -> [BoxSelector s]
parseSelectors isLocation s = let
  -- split by | and |
  ss = split (`elem` t " |") s
  parse selector | null selector = Nothing
  parse selector = Just $ if isLocation
                   then BoxSelector SelectAnything (parseSelector selector) (BoxNumberSelector Nothing Nothing Nothing)
                   else parseBoxSelector selector
  r =  mapMaybe parse ss
  in  r
  
  
  
  
-- | Force Text
t :: Text -> Text
t s = s
       
       
-- * Internal {{1
-- Move "used" box the right
-- and shift boxes to the left to fill gabs
-- 
-- Example (Uppercase box are "used")
--     a b C d -> a b d C
shiftUsed :: (a -> Bool) -> [a] -> [(a,a)]
shiftUsed isUsed xs = let
  (used, unused) = partition isUsed xs
  in zip (used ++ unused) xs

  
-- | shift used boxes but leave boxes staying in the same bucket in place or not
-- according to the bucket strategy.
shiftUsedInBucketsWithStrategy :: Show a => (a -> Bool) -> [([a], ShiftStrategy)] -> [(a, a)]
shiftUsedInBucketsWithStrategy isUsed bucket'strategies =  let
  ----------------------------------------------------------
  -- We run the algorythm twice. The first time to see which
  -- boxes have changed buckets and the second time,
  -- with the boxes which should stay in place
  ----------------------------------------------------------
  -- first we need to shiftUsed all the boxes regardless of their bucket.
  -- Assign a number to a bucket and link it to the box
  box'buckets = [(box, (bucketNumber, strategy))
          | ((boxes, strategy), bucketNumber) <- zip bucket'strategies [1..]
          , box <- zip boxes [1..]
          -- ^ in order to make a `Set a ` later we without `Ord a` we need ,
          -- to assign each box with local id
          ]
  rawShifts = shiftUsed (isUsed . fst . fst) box'buckets
  -- then we for each bucket, remove boxes which haven't moved
  stayInPlace (_,(source, StayInPlace)) (_,(dest, _)) | source == dest = True
  stayInPlace  _ _ = False
  -- However, shiftUsed needs tho box in the original order
  -- which is not guaranted by {shiftUsed}. 
  inPlaceSet = Set.fromList $ map (snd . fst . fst) $ filter (not . uncurry stayInPlace) rawShifts
  finalBoxes = [ box
               | ((box, index), _) <- box'buckets
               , index `Set.member` inPlaceSet
               ]
  in shiftUsed isUsed finalBoxes
  

  

-- | Modify the intrinsic order (in Warehouse) of boxes
-- for given boxes in the given order
freezeOrder :: [BoxId s] -> WH () s
freezeOrder boxesInOrder =  do
  let inOrderSet = Set.fromList boxesInOrder
      go [] bs = bs
      go _ [] = []
      go (o:os) (b:bs) = 
                -- replace the current box with the first in boxesInOrder  if current
                -- box is part of the boxesInOrder
                if b `member` inOrderSet
                then o : go os bs
                else b : go os bs
  modify \warehouse -> warehouse {boxes = Seq.fromList $ go boxesInOrder (toList $ boxes warehouse) }
            
  

