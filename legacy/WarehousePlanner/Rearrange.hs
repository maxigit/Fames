{-# LANGUAGE NamedFieldPuns, ViewPatterns #-}
{-# Language CPP #-}
module WarehousePlanner.Rearrange
where 

import ClassyPrelude;
import qualified Prelude
import  WarehousePlanner.Base
import  WarehousePlanner.Expr
import Data.Text(splitOn, split)
import qualified Data.Text as Text
-- import qualified Data.Map as Map
import Control.Monad (zipWithM)
import Control.Monad.State (modify)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import GHC.Utils.Monad (mapAccumLM)
import Data.List (unfoldr)

-- * Type {{{1
data ShiftStrategy = ShiftAll | StayInPlace deriving (Show, Eq)

data ForUnused = DeleteUnused | KeepUnused 
     deriving (Show, Eq)
 
data ForGrouping = Don'tGroup | GroupByContent
     deriving (Show, Eq)

-- * Export {{1
rearrangeBoxesByContent ::  ForUnused -> ForGrouping -> [Tag'Operation] -> (Box s -> Bool) -> (Text -> Bool) -> BoxSelector s -> [(Box s -> Maybe (Shelf s) -> Bool, ShiftStrategy)] -> WH [Box s] s
rearrangeBoxesByContent deleteUnused groupByContent tagOps isUsed isSticky boxsel actions = do
  boxes <- findBoxByNameAndShelfNames boxsel >>= mapM findBox
  -- group boxes by style and content
  let onContent = (`on` \b -> (boxStyle b, boxContent b))
      boxByContent = case groupByContent of
                     GroupByContent ->  groupBy (onContent (==)) $ sortBy (onContent compare) boxes
                     Don'tGroup ->  [boxes]
  newss <- mapM (\boxes ->  shiftUsedBoxes isUsed isSticky boxes actions) boxByContent
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
            

        
  
shiftUsedBoxes :: forall s . (Box s -> Bool) -> (Text -> Bool) -> [Box s] -> [(Box s -> Maybe (Shelf s) -> Bool, ShiftStrategy)] -> WH [Box s] s
shiftUsedBoxes isUsed isSticky boxes inBucket'strategies = do
  box'shelves <- mapM (\b -> (return $ boxShelf b) >>= mapM findShelf >>= \s -> return (b, s)) boxes
  let swaps = shiftUsedInBucketsWithStrategy isUsed (map mkBucket inBucket'strategies)
      mkBucket (inBucket, strategy) = (map fst $ filter (uncurry inBucket) box'shelves, strategy)
  newms <- mapM doSwap swaps
  return $ catMaybes newms
  where doSwap :: (Box s, Box s) -> WH (Maybe (Box s)) s
        doSwap (source, dest) | source == dest = return Nothing
        doSwap (source, dest) = do
                 let (sticky, nonSticky) = Map.partitionWithKey (\k _ -> isSticky k) (boxTags source)
                 when (not $ null sticky) do
                   void $ updateBox (\b -> b { boxTags = nonSticky} ) source
                      
                 new <- updateBox (\s -> s { boxOffset = boxOffset  dest
                                  , boxBoxOrientations = boxBoxOrientations dest
                                  , orientation = orientation dest
                                  , boxBreak = boxBreak dest
                                  , boxTags = boxTags dest <> sticky
                                  }
                                  ) source
                                  >>= assignShelf (boxShelf dest)
                 -- keep sticky tags

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
  box'bucketsWithoutN = [(box, (bucketNumber, strategy))
                | ((boxes, strategy), bucketNumber) <- zip bucket'strategies [1..]
                , box <- boxes
          -- ^ in order to make a `Set a ` later we without `Ord a` we need ,
          -- to assign each box with local id
                ]
  box'buckets = zipWith (\(b, bucket's) i -> ((b, i), bucket's))
                        box'bucketsWithoutN
                        [1..]
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
            
  

-- * Filling shelve
data MakeDimension = MakeDimension 
                     { mdShelfBoxToDim :: Dimension -> Dimension -> Dimension -> Dimension -- ^ shelf box dimension to update
                     , mdDescription :: Text
                     } 
instance Show MakeDimension where show =  unpack .  mdDescription
mkDim0 :: MakeDimension
mkDim0 = MakeDimension (\_ _ _ -> mempty) "0" 

-- | Misc command to fill a shelf
data FillCommand s = FCBox (Box s) (Maybe Orientation)
                 | FCBoxWithPosition (Box s) Position
                 | FCSkip
                 | FCNewDepth
                 | FCNextColumn
                 | FCSetOrientation Orientation
                 | FCResetOrientation
                 | FCSetIgnoreDimension Bool
                 | FCSetOrientationStrategy PartitionMode OrientationStrategy MakeDimension
                 | FCClearNextPositions
                 | FCSetLastBox MakeDimension
                 | FCSetOffset MakeDimension
                 | FCUseCurrentBox -- marker to insert current box dimension
     deriving (Show)
     
-- | Internal state used to keep track of the filling state process
data FillState = FillState
               { fOffset :: Dimension
               , fLastBox_ :: Dimension -- ^ last orientated box
               , fMaxCorner :: Dimension
               , fLastOrientation :: Maybe Orientation
               -- , fNextPositions :: [Position] -- positions to use next in case of complex orientation strategy
               , fIgnoreDimension :: Bool -- ^ If true reuse the dimension of the lats box instead of the current one
               -- Usefull to force slightly bigger box or fit smaller box without altering the overall layout
               , fNextPositions :: Slices Double Position 
               , fLastOrientationStrategy :: Maybe (PartitionMode, OrientationStrategy, Dimension) -- ^ with initial offset
               }
               deriving Show

emptyFillState :: FillState
emptyFillState = FillState { fOffset = Dimension 0 0 0
                           , fLastBox_  = Dimension 0 0 0
                           , fMaxCorner = Dimension 0 0 0
                           , fLastOrientation = Nothing
                           , fIgnoreDimension = False
                           , fNextPositions = mempty
                           , fLastOrientationStrategy = Nothing
                           }
-- | Fill shelf with box in the given order. regardless if boxes fit or not.
-- Checking can be done later if needed.
-- This is used force boxes in a shelf when we know (from barcode scanning
-- that the box is there.
fillShelf:: [FillCommand s] -> Shelf s -> WH [Box s] s
fillShelf commands shelf = do
  -- empty the shelf first
  existingBoxes <- findBoxByShelf shelf
  s0 <- defaultShelf
  mapM (assignShelf $ Just s0) existingBoxes
  fmap (catMaybes . snd)  $ mapAccumLM (executeFillCommand shelf) emptyFillState commands 

executeFillCommand :: Shelf s -> FillState -> FillCommand s -> WH (FillState, Maybe (Box s)) s
executeFillCommand shelf state@FillState{..} = \case
           FCBox box orm -> 
                 case unconsSlices fNextPositions of
                  Nothing -> let or = fromMaybe (orientation box) $ orm <|> fLastOrientation
                             in executeFillCommand shelf state $ FCBoxWithPosition box (Position fOffset or)
                  Just (next,_, nexts) -> doBox state {fNextPositions = nexts} box next

           FCBoxWithPosition box position -> doBox state { fNextPositions = dropTillSlot  fNextPositions } box position
           FCSkip -> let
              (Position offset or, nexts) = 
                  case unconsSlices fNextPositions of
                    Nothing -> (Position fOffset (fromMaybe up fLastOrientation), mempty)
                    Just (next,_,nexts) -> (next, nexts)
              dim = rotate or fLastBox_
              offset' = offset <> Dimension 0 0 (dHeight dim)
              in return ( state { fOffset = offset'
                                , fMaxCorner = maxDimension [ fMaxCorner, offset' <> dim ]
                                , fNextPositions = nexts
                                }
                        , Nothing
                        )
           FCNextColumn -> do
                 let droped = dropTillSlice fNextPositions
                     (Position _offset or, nexts) = 
                         case unconsSlices droped of
                           Nothing -> (Position fOffset (fromMaybe up fLastOrientation), mempty)
                           Just (next,_,_) -> (next, droped)
                     dim = rotate or fLastBox_
                    -- try to pop one slice
                 return ( case nexts of
                            _ | nexts == mempty -> let Dimension ol ow _oh = fOffset
                                                       l = dLength fMaxCorner
                                                       newOffset = if l <= dLength fOffset
                                                                   then Dimension (ol +dLength dim) ow 0
                                                                   else Dimension l ow 0
                                                       in  FillState{ fOffset = newOffset
                                                                    , fMaxCorner =  maxDimension [fMaxCorner, newOffset]
                                                                    , fNextPositions = mempty
                                                                    , ..}
                            _ -> FillState {fNextPositions = nexts, ..}
                        , Nothing
                        )
           FCNewDepth -> do
                 let newOffset = Dimension 0 (dWidth fMaxCorner) 0 
                 case fLastOrientationStrategy of
                  Nothing -> return ( FillState{ fOffset = newOffset
                                               , fNextPositions = mempty
                                               , fMaxCorner = newOffset
                                               , ..}
                                    , Nothing
                                    )
                  Just (partitionMode, strategy, initialOffset) -> doStrategy partitionMode strategy (initialOffset <> Dimension 0 (dWidth $ fLastBox) 0)
           FCSetOrientation o -> 
                 return ( FillState { fLastOrientation = Just o
                                    , fNextPositions = mempty
                                    , fMaxCorner = if fMaxCorner == mempty
                                                   then rotate o fLastBox_
                                                   else fMaxCorner
                                    , ..}
                        , Nothing
                        )
           FCResetOrientation  -> 
                 return ( FillState { fLastOrientation = Nothing
                                    , fNextPositions = mempty
                                    , ..}
                        , Nothing
                        )
           FCSetIgnoreDimension ignore ->
                 return ( FillState {fIgnoreDimension = ignore,..}
                        , Nothing
                        )
           FCSetOrientationStrategy partitionMode strategy mkOffset  ->  
            doStrategy partitionMode strategy (mdShelfBoxToDim mkOffset (maxDim shelf) (rotate (osOrientations strategy) fLastBox_) fMaxCorner)
           FCClearNextPositions -> 
                 return ( FillState {fNextPositions = mempty, ..}
                        , Nothing
                        )
           FCSetLastBox mkDim ->
                 let dim = (mdShelfBoxToDim mkDim) (maxDim shelf) fLastBox_  fLastBox_
                 in return ( FillState {fLastBox_ = dim
                                       , ..
                                       }

                           , Nothing
                           )
           FCSetOffset mkDim ->
                 let offset = (mdShelfBoxToDim mkDim) (maxDim shelf) fLastBox fOffset
                 in return ( FillState {fOffset = offset
                                       , ..
                                       }

                           , Nothing
                           )
           FCUseCurrentBox -> return (state, Nothing)

    where doBox state box (Position offset or) = do
                 let dim = dimensionFor box (Just or) state
                     offset' = offset <> Dimension 0 0 (dHeight dim)
                     -- tag box if dimension is different
                     tagOps = if dimensionSame dim (_boxDim box)
                            then []
                            else [("@size-forced", SetTag)]
                 box' <- updateBox (\b -> b { orientation = or
                                            , boxOffset = offset 
                                            }
                                   ) (updateBoxTags' tagOps box)
                         >>= assignShelf (Just shelf)
                 -- assignShelf (Just shelf) box'
                 return ( state { fOffset=offset'
                                , fLastBox_ = _boxDim box 
                                , fMaxCorner = maxDimension [fMaxCorner, boxCorner box']
                                , fLastOrientation = Just or
                                }
                        , Just box'
                        )
          doStrategy partitionMode strategy offset = do
              -- find the next positions 
              used <- case partitionMode of
                        POverlap -> map boxAffDimension <$> findBoxByShelf shelf
                        _ -> return [AffDimension mempty fMaxCorner]
              let positions = bestPositions' partitionMode [strategy] shelf offset used fLastBox_
              if  Prelude.length positions == 0
              then error . unpack $ "Strategy "  <> tshow partitionMode <> " " <> tshow strategy <> " doesn't allow any boxes.\nCheck if the shelf is deep enough: "
                           <>  (shelfName shelf)
                           <> " " <> printDim (maxDim shelf) <> "/" <> printDim offset
                           <> " " <> printDim fLastBox_

              else return ( FillState { fNextPositions = positions
                                      , fLastOrientationStrategy = Just (partitionMode, strategy, offset)
                                      , fMaxCorner = offset
                                      , .. }
                          , Nothing
                          )
          fLastBox = rotate (fromMaybe up fLastOrientation) fLastBox_

           

-- | get the box dimension unless overriden 
dimensionFor :: Box s -> Maybe Orientation -> FillState -> Dimension
dimensionFor box forcedOrientation FillState{..} = 
    case (fIgnoreDimension, fLastBox_) of
         (_, Dimension 0 0 0) -> boxDim box'
         (False, _) -> boxDim box'
         (True, last) -> last
    where box' =  box {orientation = fromMaybe (orientation box) (forcedOrientation <|> fLastOrientation) }
   
parseFillCommand :: [Text] -> Maybe (FillCommand s, [Text])
parseFillCommand = \case 
  ("" : coms) -> parseFillCommand coms
  --
  ("skip" : coms) -> Just (FCSkip, coms)
  ("sk" : coms) -> Just (FCSkip, coms)
  --
  ("next": "column" : coms)  -> Just (FCNextColumn, coms)
  ("nc" : coms) -> Just (FCNextColumn, coms)
  --
  ("new": "depth" : coms) -> Just (FCNewDepth, coms)
  ("nd" : coms) -> Just (FCNewDepth, coms)
  --
  ("reset" :  "orientation" : coms) -> Just (FCResetOrientation, coms)
  ("ro" : coms)  -> Just (FCResetOrientation, coms)
  --
  ("ignore" : "dimension" : coms)  -> Just (FCSetIgnoreDimension True, coms)
  ("id" : coms)  -> Just (FCSetIgnoreDimension True, coms)
  --
  ("use" : "dimension" : coms)  -> Just (FCSetIgnoreDimension False, coms)
  ("ud" : coms)  -> Just (FCSetIgnoreDimension False, coms)
  --
  ("set" : "strategy": "with": l : w : h :coms ) | strat@(Just _) <- parseStrategyCommand l w h coms -> strat
  ("ssw" : l : w : h :coms ) | strat@(Just _) <- parseStrategyCommand l w h coms -> strat
  --
  ("set" : "strategy": coms ) | strat@(Just _) <- parseStrategyCommand "0" "0" "0" coms -> strat
  ("ss" : coms ) | strat@(Just _) <- parseStrategyCommand "0" "0" "0" coms -> strat
  --
  ("clear" : "position" : coms) -> Just (FCClearNextPositions, coms)
  ("cp" : coms) -> Just (FCClearNextPositions, coms)
  --
  ("set": "box" : l : w : h : coms) -> Just (FCSetLastBox $ parseMakeDimension l w h, coms)
  ("sb": l : w : h : coms) -> Just (FCSetLastBox $ parseMakeDimension l w h, coms)
  --
  ("set": "offset" : l : w : h : coms) -> Just (FCSetOffset (parseMakeDimension l w h), coms)
  ("so": l : w : h : coms) -> Just (FCSetOffset (parseMakeDimension l w h), coms)
  --
  (com : coms) | Just (c, "") <- uncons com, Just o <- readOrientationMaybe c -> Just (FCSetOrientation o, coms)
  --
  ("comment" : _ : coms) -> parseFillCommand coms
  ("cc" : _ : coms) -> parseFillCommand coms
  --
  (";": coms) -> Just (FCUseCurrentBox, coms)
  ("use": "current": "box": coms) -> Just (FCUseCurrentBox, coms)
  ("ucb": coms) -> Just (FCUseCurrentBox, coms)
  --
  [] -> Nothing
  coms -> error $ "Cant' parse " ++ unpack (unwords coms)

parseStrategyCommand :: Text -> Text -> Text -> [Text] -> Maybe (FillCommand s, [Text])
parseStrategyCommand _ _ _ [] = Nothing
parseStrategyCommand l w h (com:coms) = do -- maybe
   let (orientations, (_,partitionMode, _, _)) = extractModes com 
   case parseOrientationRule [] orientations of
    [strategy] -> Just (FCSetOrientationStrategy partitionMode strategy (parseMakeDimension l w h), coms)
    _ -> Nothing
    

parseFillCommands :: Text -> [FillCommand s]
parseFillCommands command = let
  commands = splitOn " " $ toLower command
  in unfoldr parseFillCommand commands
  

parseMakeDimension :: Text -> Text -> Text -> MakeDimension
parseMakeDimension l w h = MakeDimension{..} where
  mdShelfBoxToDim shelf box current = Dimension (parseEvaluator l shelf box $ dLength current)
                                                (parseEvaluator w shelf box $ dWidth current)
                                                (parseEvaluator h shelf box $ dHeight current)
  mdDescription = Text.unwords [l, w, h]

parseEvaluator :: Text -> (Dimension -> Dimension -> Double -> Double)
parseEvaluator "" = \_ _ x -> x
parseEvaluator s =
  case parseExprE s of
    Left _ -> \_ _ x -> x
    Right expr -> \shelf box def ->  evalExpr (fmap (evalWith shelf box def) expr)
    
  where evalWith :: Dimension -> Dimension -> Double -> Text -> Double
        evalWith shelf box def varname = let
          vars :: Map Text Double
          vars = mapFromList [("l", dLength shelf)
                         ,("w", dWidth shelf)
                         ,("h", dHeight shelf)
                         ,("lb", dLength box)
                         ,("wb", dWidth box)
                         ,("hb", dHeight box)
                         ,("%", def)
                         ]

          in findWithDefault (error $ unpack varname ++ " is no't a valid key") varname vars
          

