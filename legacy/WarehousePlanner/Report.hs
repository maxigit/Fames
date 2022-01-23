-- | Miscelaneous reporting functions
module WarehousePlanner.Report
( reportAll
, report
, summary
, generateMoves
, generateMOPLocations
, generateGenericReport
, bestBoxesFor
, bestShelvesFor
, bestAvailableShelvesFor
, bestHeightForShelf
, shelvesReport
, shelfTagsReport
, boxesReport
, groupShelvesReport
, groupShelvesReport'
, groupBoxesReport
, reportPairs
, boxStyleWithTags
) where

import WarehousePlanner.Base
import WarehousePlanner.Optimum
import ClassyPrelude hiding(or)
import Control.Monad.ST.Unsafe(unsafeSTToIO)
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad.State(get, gets, evalStateT)
import Text.Printf(printf)
import qualified Data.Map.Strict as Map'
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.List as List
import Data.Text(breakOn)
import Text.Tabular as Tabul
import Data.Text(replace)

-- pattern (:<) :: Text -> Maybe (Char, Text)
pattern x :< xs <- (uncons -> Just (x, xs))
reportAll :: WH [Text] s
reportAll = do
    sb <- shelfBoxes
    let groups =  Map'.fromListWith (+) (map (\(s,b) -> ((shelfName s, boxDim b, orientation b, boxStyle b), (1::Int))) sb)
    return $ map toString (Map'.toList groups)
    where toString ((name, __dim, o, style), count) =
            name
                <> ", " <> style
                <> ", " <> tshow count  <> ", " <> showOrientation o

-- | Find the box which fit a given shelf  the best
bestBoxesFor :: Text -> WH [Text] s
bestBoxesFor shelf = do
    boxIds <- toList <$> gets boxes
    boxes' <- mapM findBox boxIds
    shelves <- findShelfBySelector (Selector (matchName shelf) []) >>= mapM findShelf
    getOr <- gets boxOrientations

    let groups  = Map'.fromList (map (\b -> ((boxDim b, boxStyle b), b)) boxes')
        boxes = Map'.elems groups
    let bors = map (\b -> (b, getOr b)) boxes
        boxHeight = dHeight . boxDim
        shelfHeight = dHeight . minDim

    let s  = headEx shelves
        tries = [ (-((fromIntegral (n*k*m))*(boxVolume box / boxHeight box / shelfVolume s * shelfHeight s)), box)
            | (box, ors) <- bors
            , let (_,_,n,k,m,_) = bestArrangement (ors s) [(maxDim s, s)] (_boxDim box)
            ]

        bests = sortBy (compare `on` fst) tries

    mapM (report s) (map snd bests)

-- | Find best boxes ignoring the shelve height
bestHeightForShelf :: Text -> WH (IO ()) s
bestHeightForShelf shelf = do
    boxIds <- toList <$> gets boxes
    boxes' <- mapM findBox boxIds
    shelves <- findShelfBySelector (Selector (matchName shelf) []) >>= mapM findShelf
    getOr <- gets boxOrientations

    let groups  = Map'.fromList (map (\b -> ((boxDim b, boxStyle b), b)) boxes')
        boxes = Map'.elems groups
    let bors = map (\b -> (b, getOr b)) boxes

    let s  = headEx shelves
        tries = [ (-((fromIntegral (n*k*m))*boxVolume box / shelfVolume s), (box, or, diag, (n,k,m)))
            | (box, ors) <- bors
            , let (or,diag,n,k,m,_) = bestArrangement (ors s) [(maxDim s, s)] (_boxDim box)
            ]

        bests = sortBy (compare `on` fst) tries
        reportHeight shelf_  (box, or, diag, (n,k,__m)) = do
          similarBoxes <- findBoxByNameSelector (matchName $ boxStyle box)
          let box' = box { orientation = or }
          let lengthRatio = ( dLength (boxDim box') * (fromIntegral n)
                            / dLength (minDim shelf_)
                            )
              widthRatio = ( dWidth (boxDim box') * (fromIntegral k)
                           / dWidth (minDim shelf_)
                           )
              height = dHeight (boxDim box')
              recommendedHeight = height * (fromIntegral ((ceiling (130/height)) :: Int))
              numberOfBoxes = length similarBoxes
              shelvesNeeded = fromIntegral numberOfBoxes * recommendedHeight / height / fromIntegral n

          return $ putStrLn $ "box: " <> boxStyle box
            <> ", height: " <> (pack $ printf " %0.2f (%0.2f)" recommendedHeight height)
            <> " " <> showOrientationWithDiag or diag
            <> (pack $ printf " %dx%dx%d" n k (floor (recommendedHeight/height) :: Int))
            <> (pack $ printf " H:%0.1f%%" (100*lengthRatio))
            <> (pack $ printf " (%0.1f%%)" (100*lengthRatio*widthRatio))
            <> (pack $ printf " : %d b -> %0.1f s " numberOfBoxes (shelvesNeeded :: Double))

    ios <- mapM (reportHeight s) (map snd bests)
    return $ sequence_ ios

report :: Shelf s -> Box s -> WH Text s
report shelf box = do
    getOr <- gets boxOrientations
    similarBoxes <- findBoxByNameSelector (matchName $ boxStyle box)
    let (or,diag,n,k,m,_) = bestArrangement (getOr box shelf) [(maxDim shelf, shelf)] (_boxDim box)
        ratio = boxVolume box * fromIntegral (n * k * m) / shelfVolume shelf
        numberOfBoxes = length similarBoxes
        shelvesNeeded = fromIntegral numberOfBoxes / fromIntegral (n*m*k)
    return $ "box: " <> boxStyle box <> ", shelf: " <> shelfName shelf
                        <> " " <> showOrientationWithDiag or diag
                        <> (pack $ printf " %0.1f%%" (ratio*100))
                        <> (pack $ printf " %dx%dx%d" n k m)
                        <> (pack $ printf " (%d)" (n*m*k))
                        <> (pack $ printf " : %d b -> %0.1f s " numberOfBoxes (shelvesNeeded :: Double))
  
                        -- <> tshow (_boxDim box) <> tshow (maxDim shelf)



-- | find the best shelf for a given style
-- Doesn't take into account boxes already there.
bestShelvesFor :: Text -> WH [Text]s
bestShelvesFor style'shelf = do
    (boxes, shelves) <- boxAndShelvesFor style'shelf
    or <- gets boxOrientations

    let box = headEx boxes
        bests = bestShelves box (or box) shelves

    mapM (flip report $ box) bests

boxAndShelvesFor :: Text -> WH ([Box s], [Shelf s]) s
boxAndShelvesFor style'shelf = do
  let (style, shelfSelector) = case (breakOn "," style'shelf) of
                                  (s,"") -> (s, "")
                                  (box, shelf) -> (box, drop 1 shelf)
                                  
  boxes <- findBoxByNameSelector (matchName $ style) >>= mapM findBox
  shelves <- findShelfBySelector (parseSelector shelfSelector) >>= mapM findShelf
  return (boxes, shelves)

-- | find the best shelf for a given style
-- depends on what's already there.
bestAvailableShelvesFor :: Text -> WH [Text] s
bestAvailableShelvesFor style'shelf = do
    (boxes, shelves) <- boxAndShelvesFor style'shelf
    or <- gets boxOrientations
    let  box = headEx boxes
    -- sort is stable so by passing shelves in
    -- the best order we get at shelves sorted by
    -- n and then best order
    let bests = bestShelves box (or box) shelves
    let   getInfo shelf = do
            boxesLeft <- moveBoxes ExitLeft boxes [shelf]
            return (shelf, length boxes - length boxesLeft)
    shelfInfos <- mapM getInfo bests
    let go (shelf, n) = do
            r <- report shelf  (headEx boxes)
            return $  (pack $ printf "%d/%d => " n (length boxes)) <> r
    mapM go ( sortBy (comparing (Down . snd))
                     (filter ((/=0).snd) shelfInfos)
                   )




-- * Summary 
-- Display total volume shelf volume
-- with a breakdown per shelf tags

data SummaryInfo = SummaryInfo
  { siFloor :: Maybe Double --  ^ m^2
  , siTotalVolume :: Maybe Double -- ^ m^3
  , siUsedVolume :: Double -- ^ m3
  } deriving (Show)

siFree :: SummaryInfo -> Maybe Double
siFree si = do
  total <- siTotalVolume si
  return $ total - siUsedVolume si

siUsedPercent :: SummaryInfo -> Maybe Double
siUsedPercent si = do
  total <- siTotalVolume si
  return $ (siUsedVolume si / total) * 100
instance Semigroup SummaryInfo where
  (SummaryInfo f t u) <> (SummaryInfo f' t' u') =
    SummaryInfo (f `mPlus` f') (t `mPlus` t') (u+u')
instance Monoid SummaryInfo where
  mempty = SummaryInfo Nothing Nothing 0

mPlus :: Maybe Double -> Maybe Double -> Maybe Double
mPlus Nothing x = x
mPlus (Just x) (Just y) = Just (x+y)
mPlus x Nothing =  x

summarizeShelves :: [Shelf s] -> WH (SummaryInfo) s
summarizeShelves shelves = do
    let totalVolume = (/1000000) . sum $ map shelfVolume shelves
        totalFloor = (/10000) . sum $ map (floorSpace.minDim) shelves
    used <- (/1000000) <$> sum `fmap` mapM occupiedVolume shelves
    return $ SummaryInfo (Just totalFloor) (Just totalVolume) used

summary :: WH ([[Text]], [Text])  s
summary = do
    ss <- toList <$> gets shelves >>= mapM findShelf
    -- group shelves using summary property
    -- summary=no mean don't display
    let groups = Map'.fromListWith (<>) [ (summaryGroup, [s])
                                        | s <- filter (applyTagSelectors (maybeToList $ parseTagSelector "!sep")  shelfTag) ss
                                        , let summaryGroup = flattenTagValues <$>lookup "summary" (shelfTag s)
                                        ]
    infos' <- traverse summarizeShelves groups
    let infos = Map'.mapWithKey adjust infos'
        adjust tag si = case tag of
          (Just ('_':<_)) -> si {siTotalVolume = Nothing, siFloor = Nothing }
          _ -> si
    let total = mconcat (Map'.elems infos)

        f = tshow . (\x -> x :: Int) . round :: Double -> Text
        renderSI si = let values = [ f . siUsedVolume
                                   , (maybe "" f) .  siTotalVolume
                                   , (maybe "" f) . siFree
                                   , (maybe "" f) . siUsedPercent
                                   , (maybe "" f) . siFloor
                                   ] <*> [si]
                      in values

        _unused_table = Table (Group NoLine (Header . fst <$> Map'.toList infos))
                      (Group SingleLine (map Header (words "used total free %used floor" :: [Text])))
                      (map renderSI $ Map'.elems infos)
                +----+ row (Just "Total") (renderSI total)
    -- return $ (TAscii.render (fromMaybe "<main>") id id table)
    return  ( zipWith (:) ("":  (map (fromMaybe "<main>") $ Map'.keys infos))
                          ( (words "used total free %used floor")
                          : (map renderSI $ Map'.elems infos)
                          )
            , (renderSI total)
            )



_unused_isShelfEmpty s = null `fmap` findBoxByShelf s
-- @todo optimize
_unused_seqFilterM :: (Functor m, Monad m) => (a -> m Bool) -> Seq a -> m (Seq a)
_unused_seqFilterM test s = fmap Seq.fromList (filterM test (toList s))

occupiedVolume :: Shelf' shelf => shelf s -> WH Double s
occupiedVolume s = do
    boxes <- findBoxByShelf s
    return $ sum $ map  (volume ._boxDim) boxes



-- * Shelve report 
-- | Display shelf information including, depth really used
--
shelvesReport :: WH [Text] s
shelvesReport = do
  ss <- toList <$> gets shelves >>= mapM findShelf
  ls <- mapM  report_ ss

  return $ ("name,comment,length,width,height,depthLeft,usedRatio,bottom") : ls

  where report_ :: Shelf s -> WH Text s
        report_ shelf = do
          let (Dimension l w h) = minDim shelf
          (name, depth) <- usedDepth shelf
          let left = w - depth

          -- find max depth
          return $ pack $ printf "%s,%s,%0.2f,%0.2f,%0.2f,%0.2f,%.0f%%,%0.2f\n"
                    (shelfNameTag shelf)
                    (pack $ printf "%s (%0.2f)" name depth :: Text)
                    l w h
                    left
                    (depth/w*100)
                    (bottomOffset shelf)


-- returns the lis
listShelfTags :: WH ([Text]) s
listShelfTags = do
  ss <- toList <$> gets shelves >>= mapM findShelf
  return . List.nub . sort $ concat (map (flattenTags . shelfTag) ss)

shelfTagsReport :: WH (IO ()) s
shelfTagsReport = do
  tags <- listShelfTags
  return $ (mapM_ putStrLn tags)

-- * box report 
-- | Display list of all individual boxes and their location
boxesReport :: WH (IO ()) s
boxesReport = do
  sbS <- shelfBoxes
  ios <- mapM report_ sbS

  return $ sequence_ ((putStrLn "style,content,location"):ios)

  where -- report_ :: Box s -> WH (IO ()) s
        report_ (shelf, box) = do
          let ((nl, nw, nh), diag) = howManyWithDiagonal (maxDim shelf) (boxDim box)
          return $ printf "%s,%s,%s,%s,%dx%dx%d\n" (boxStyle box) (boxContent box) (shelfName shelf) (showOrientationWithDiag (orientation box) diag) nl nw nh
  
-- * Compatible with warehouse planner input 
-- | Generates moves from actual styles positions, ie find all shelves
-- for  a given styles

groupNames :: [Text] -> [Text]
groupNames names = let
  groups = Map'.fromListWith (<>) [(initEx name, [lastEx name]) | name <- names]

  toName (prefix, "") = prefix
  toName (prefix, x :< "") = prefix <> singleton x
  toName (prefix, xs) = pack $ printf "%s[%s]" prefix (sort xs)

  in map toName (Map'.toList groups)

boxStyleWithTags :: Box s -> Text
boxStyleWithTags b = let
  isVirtual ('\'':<_) = True
  isVirtual _ = False
  tags = filter (not . isVirtual) (getTagList b)
  in intercalate "#" (boxStyleAndContent b : tags)
 

shelvesToNames :: [Shelf s ] -> [Text]
shelvesToNames = List.nub . sort . map shelfName

generateMoves :: (Box s -> Text) -> WH [Text] s
generateMoves boxName0 = generateMoves' (Just "stock_id,location") (Just . boxName0) printGroup where
     printGroup  boxName' _ shelves = boxName' <> "," <> intercalate "|" (groupNames $ shelvesToNames shelves)
-- generateMoves' :: (Box s -> Text) -> (Box s -> [Text]) -> WH [Text] s
generateMoves' :: (Ord k, Eq k)
               => Maybe Text --  ^ Header
               -> (Box s -> Maybe k) --  ^ box key
               ->  (k -> [Box s] -> [Shelf s]  -> Text) --  ^ string from key, boxes and unique shelfnames
               -> WH [Text] s
generateMoves' header boxKey0 printGroup = do
 s'bS <- shelfBoxes
 generateMovesFor header boxKey0 printGroup s'bS 
generateMovesFor header boxKey0 printGroup box'shelfs = do
 let groups = Map'.fromListWith (<>) [ (key, [(b, s)])
                                     | (s,b) <- box'shelfs
                                     , Just key <- [boxKey0 b]
                                     -- , "mop-exclude" `notElem` boxTags b
                                     ]
     printGroup' (key, box'shelves) = printGroup key boxes shelves where (boxes, shelves) = unzip box'shelves
 return $ maybe id   (:) header $  map printGroup' (Map'.toList groups)
  
-- | Generates files compatible with MOP
generateMOPLocations :: WH [Text] s
generateMOPLocations = generateMoves' (Just "stock_id,location") boxName printGroup where
  -- use box style unless the box is tagged as exception
  boxName box = let comment = getTagValuem box "mop-comment"
                    hasTag = tagIsPresent box
                in case (hasTag "mop-exclude", hasTag "mop-exception") of
                   (True, _) -> Nothing -- skipped
                   (False, True) -> Just $ (boxStyleAndContent box, comment)
                   (False, False) -> Just $ (boxStyle box, comment)
 
  -- display shelf we pick from first. This is needed
  -- because the first displayed shelf is used to sort style by location
  sortShelves = List.nub
              . map shelfName
              . sortOn ((,) <$> (Down . flip tagIsPresent  "mop-priority") <*> shelfName)
              . filter (not . flip tagIsPresent "mop-exclude")
  -- truncate 
  groupNames2 name names = case groupNames names of
   (x:y:_:_) -> intercalate "|" [name, x,y] <> " ..."
   xs -> intercalate "|" (name:xs)
  -- add comment from tag
  printGroup (boxName_, boxComment) _ shelves = boxName_ <> "," <>
    case (boxComment, sortShelves shelves) of
           (Nothing, name:names) -> groupNames2 name names 
           (Just comment, [name]) -> name <> " " <> comment
           (Just comment, name:names) -> (groupNames2 name names)  <> " " <> comment
           ( commentM, []) -> fromMaybe "" commentM
                                        
-- | Generate a generic report using tags prefixed by the report param
generateGenericReport :: Day -> Text -> WH [Text] s
generateGenericReport today prefix = do
  s'bS <- shelfBoxes
  -- group by group if exists
  let groupKey box = getTagValuem box (prefix <> "-group")
      groups0 = Map'.fromListWith (<>) [ (gkey, [s'b])
                                    | s'b <- s'bS
                                    , let gkey = groupKey (snd s'b)
                                    ]
      groups = case Map'.keys groups0 of
        [Nothing] -> groups0
        _ -> Map'.delete Nothing groups0

  rs <- forM (Map'.toList groups) $ \(gkey, s'bs) -> do
    -- display the group only if it's not null
    let withTitle items = case gkey of
                            Just s | not (null s) -> expandReportValue today boxes (shelvesToNames shelves) s : items
                            _ -> items
        (shelves, boxes) = unzip s'bs
    items <- generateGenericReport' today prefix s'bs
    return (withTitle items)
  return $ concat rs
        
  
generateGenericReport':: Day -> Text -> [(Shelf s, Box s)] -> WH [Text] s
generateGenericReport' today prefix s'bs = generateMovesFor Nothing boxKey0 printGroup s'bs where
  boxKey0 box = getTagValuem box (prefix <> "-key")
  printGroup boxKey_ [] __shelfNames = boxKey_
  printGroup boxKey_ boxes@(box:_) shelfNames = let
    value0 = getTagValuem box (prefix <> "-value")
    value = maybe boxKey_ (expandReportValue today boxes $ shelvesToNames shelfNames ) value0
    in value


-- | Expands group related properties, which can't be processed on a indivial box
-- e.g. boxes counts, shelves list etc ...
expandReportValue :: Day -> [Box s] -> [Text] -> Text -> Text
expandReportValue today boxes shelves s = let
  updates = [ replace "${count}" (tshow $ length boxes)
            , replace "${locations}" (intercalate "|" $ groupNames shelves)
            , replace "${shelves}" (intercalate "|" shelves)
            , replace "${total-volume}" $ pack $ printf "%0.1f" ((sum $ map boxVolume boxes) * 1E-6)
            , replace "${style-count}" (tshow $ length styles)
            , replace "${styles}" $ (intercalate "|" styles)
            , replace "${content-count}" (tshow $ length contents)
            , replace "${contents}" $ (intercalate "|" contents)
            , replace "${sku-count}" (tshow $ length contents)
            , replace "${skus}" $ (intercalate "|" skus)
            , replace "${shelf-count}" $ (tshow $ length shelves)
            , replace "${dimensions-count}" (tshow  $ lengthBy' boxes _boxDim)
            , replace "${orientations}" orientations
            , replace "${orientation-count}" (tshow  $ length orientations)
            , replace "${hash}" "#"
            , replace "${comma}" ","
            , replace "${dollar}" "$"
            , replace "${divide}" "/"
            , replace "${today}" (pack $ formatTime defaultTimeLocale "%Y-%m-%d" today)
            ]
  orientations = concatMap showOrientation . List.nub . sort $ map orientation boxes
  styles = List.nub . sort $ map boxStyle boxes
  contents = List.nub . sort $ map boxContent boxes
  skus = List.nub. sort $ map boxStyleAndContent boxes
  in foldl' (flip ($)) s updates


lengthBy' :: (Ord k, Eq k) => [a] -> (a -> k) -> Int
lengthBy' boxes f = length . List.nub . sort $ (map f boxes)

-- replace :: Text -> Text -> Text -> Text
-- replace needle value "" = "
-- replace needle value s@(c:<cs) = case stripPrefix needle s of
--   Nothing -> c `cons` replace needle value cs
--   Just s' -> value <> replace needle value s'

  
  

  
-- * Optimizer 
-- find optimal way to rearrange the warehouse.
-- The first things is to separate what needs to go on the "top" from
-- normal normal shelves
-- next we need to group shelves by similarity

-- then we need get for each style is best shelf and the remaining bits


-- shelfKey :: Shelf s -> (Dimension, Dimension, BoxOrientator, FillingStrategy)
shelfKey = (,,)
         <$> (tweakWidth <$> maxDim)
         <*> shelfBoxOrientator
         <*> shelfFillingStrategy
  where tweakWidth s = s -- { dWidth = 100 }


-- | group identical shelves, keep the first one as example
groupShelves :: (Shelf s -> Bool) -> WH [(Shelf s, [Text])] s
groupShelves exclude = do
  ss <- toList <$> gets shelves >>= mapM findShelf
  let groups = Map'.fromListWith (<>) [ (shelfKey s, [s])
                                      | s <- filter (applyTagSelectors (mapMaybe parseTagSelector ["!sep", "!error"])
                                                                       shelfTag) ss
                                      , exclude s == False
                                      ]
      regroup ss_ = let sorted = sortBy (comparing shelfName) ss_
                   in (headEx sorted, map shelfName sorted)

  return $ map (regroup) (Map'.elems groups)


-- | Displays shelf groups summary
groupShelvesReport :: WH [Text] s
groupShelvesReport = do
  groups <- groupShelves (const False)
  let sorted = sortBy (comparing ( headEx . snd  )) groups
  forM sorted $ \(s, names) -> do
      let d = maxDim s
      return $ tshow (length names) <> " x " <> (headEx names) <> " -- " <> (lastEx names)
        <> " " <> printDim d


-- Displays shelf group Map (ie what belong to  which group)
groupShelvesReport' :: WH (IO ()) s
groupShelvesReport' = do
  groups <- groupShelves (const False)
  return $ do
    forM_ (sortBy (comparing (shelfName . fst)) groups) $ \(_, names) -> do
      forM_ names $ \name -> putStrLn $ name <> " => " <> (headEx names)

type BoxKey = (Dimension, Text, [Orientation])
boxKeyForGroup :: Box s -> BoxKey
boxKeyForGroup = (,,) <$> _boxDim <*> boxStyle <*> boxBoxOrientations

-- | Group boxes by style and dimension
-- keep the first one as an example
groupBoxes :: (Box s -> Bool) -> WH [(Box s, Int)] s
groupBoxes exclude = do
    boxIds <- toList <$> gets boxes
    boxes <- mapM findBox boxIds
    groupBoxes' (filter (not . exclude) boxes)

groupBoxes' :: [Box s] -> WH [(Box s, Int)] s
groupBoxes' boxes =  do
    let groups = Map'.fromListWith (<>) [ (boxKeyForGroup b, [b])
                                        | b <- boxes
                                        ]
        regroup bs = (lastEx bs, length bs)
    return $ map regroup (Map'.elems groups)

groupBoxesReport :: [Box s] -> WH [Text] s
groupBoxesReport boxes = do
  groups <- groupBoxes' boxes
  let withVolumes = [(volume (boxDim b) * (fromIntegral q), b'q) | b'q@(b,q) <- groups ]
  let sorted = map snd $ sortBy (comparing (\(rank, (volume_, _)) -> (rank, Down volume_))) (zip [1..] withVolumes)
  forM sorted $ \(v, (box, qty)) -> do
      let d = _boxDim box
      return $ (boxStyle box ) <> (concatMap showOrientation (boxBoxOrientations box )) <>   " x "  <> tshow qty
               <> " " <> f (dLength d) <> "," <> f(dWidth d) <> "," <> f (dHeight d)
               <> " " <> f (v/1000000 ) <> "m^3"
      where f = pack . printf ("%0.1f")

-- | Temporary data to stock "residuals"
-- i.e. how  many boxes can fit in the given shelw
-- and how many are left
data Residual s = Residual
  { rBox :: Box s
  , rShelf :: Shelf s
  , rNumberOfShelves :: Int -- ^ number of available shelves
  , rBoxPerShelf :: !Int -- ^
  , rUsedShelves :: !Int -- ^ number of shelves fully used
  , rBoxLeft :: !Int
  , rOrientation :: !Orientation
  , rVolumeLeftForFull :: !Double -- ^ Volume left for a full shelf
  , rVolumeUsedForLO :: !Double -- ^ Volume used for a left over shelf
  , rVolumeLeftForLO :: !Double -- ^ Volume left for left over shelf
  , rPercentageUsedForFull :: !Int -- ^ Percentage of volume used for a full shelf. Only if full shelf needed
  } deriving (Eq, Show)

rNumberOfBox r = rUsedShelves r * rBoxPerShelf r + rBoxLeft r
rVolumeLeftForAllFull r = rVolumeLeftForFull r * (fromIntegral $ rUsedShelves r)
-- | Define Ord for residuals so that "best" residuals are first
-- best minimize the wasted space

findResidual :: [OrientationStrategy] -> Shelf s -> Box s -> Int -> Int -> Maybe (Residual s)
findResidual orientations shelf box qty nbOfShelf = let
  sdim = maxDim shelf
  bdim = _boxDim box
  (or,_, nl, nw, nh, ()) = bestArrangement orientations [(sdim, ())] bdim
  in case nl*nw*nh of
    0 -> Nothing
    nbPerShelf -> let
      (used, left) = qty `divMod` nbPerShelf
      volLeftForFull = if used == 0
                  then 0
                  else volume sdim - (fromIntegral nbPerShelf) * (volume bdim)
      volUsedLO = (fromIntegral left) * (volume bdim)
      volLeftOver = if left  == 0
                      then 0
                      else volume sdim - volUsedLO
      perc = if volLeftForFull == 0 then 100 else round $ (1- volLeftForFull / volume sdim) * 100
      in Just $ Residual box shelf nbOfShelf nbPerShelf used left or (volLeftForFull / 1000000) (volUsedLO / 1000000) (volLeftOver / 1000000) perc
                         

-- | Try each each pair of boxes and each shelves to find the best arrange
-- a pair of style is good if it minimize the occupied ration

data MixedResult = LeftOnly Int  | RightOnly Int | Both Int Int deriving (Eq, Read, Show, Ord)
data Pair s  = Pair
  { pRes1 :: Residual s
  , pRes2 :: Maybe (Residual s)
  , pWastedVolume :: Double
  , pN1 :: Int -- ^ number of full shelves for box 1
  , pN2 :: Int -- ^ number of full shelves for box 2
  , pMixed :: MixedResult
  , pMixedLeft :: Double -- ^ total volume left in the mixed area if any
  } deriving (Eq, Show)

newPair :: Warehouse s -> Residual s -> Maybe (Residual s) -> Maybe (Pair s)
newPair _ res Nothing  =  let
  n1 = rUsedShelves res
  wastedVolume = rVolumeLeftForLO res + rVolumeLeftForAllFull res
  in Just $ Pair res Nothing wastedVolume n1 0 (LeftOnly (rBoxLeft res)) (rVolumeLeftForLO res)

newPair wh res (Just res') =
  if rVolumeLeftForLO res < rVolumeUsedForLO res'
  then Nothing
  else
    -- check if both residuals can actually fit
    let addShelf shelf = do
          newShelf (shelfName shelf)
            (Just $ intercalate "#" $ flattenTags $  shelfTag shelf)
            (maxDim shelf)
            (maxDim shelf)
            0
            (shelfBoxOrientator shelf)
            (shelfFillingStrategy shelf)
        addBoxes res_ shelf = do
          let box = rBox res_
          forM [1..rBoxLeft res_] $ \_ -> newBox (boxStyle box)
                                             (boxContent box)
                                             (_boxDim box)
                                             (rOrientation res_) -- current orientation
                                             shelf
                                             (boxBoxOrientations box)
                                             (getTagList box)

        tryW = do
          -- traceShowM ("NEW PAIR", res, res')
          def <- addShelf (rShelf res)
          shelf <- addShelf (rShelf res)
          boxes <- addBoxes res def
          boxes' <- addBoxes res' def
          left <- moveBoxes ExitOnTop boxes [shelf]
          left' <- moveBoxes ExitOnTop boxes' [shelf]
          -- shelfR <- findShelf shelf

          -- traceShowM ("RES", length boxes, length left, length boxes', length left')
          return (length boxes, length left, length boxes', length left')


        ors = boxOrientations wh
        wh' = (emptyWarehouse (whDay wh)) {boxOrientations=ors}
        (toFit, notFit, toFit', notFit' ) = unsafePerformIO $ unsafeSTToIO (evalStateT tryW wh')

      in case (notFit, notFit') of
          (0,0) -> -- the two partial shelves we need to extract the contribution
                  -- of the left boxes. For that we calculate the volume left in
                  -- in the mixed shelf and share it using the pro-rata of the overall volume used
                  let used = rVolumeUsedForLO res
                      used' = rVolumeUsedForLO res'
                      totalWasted = (shelfVolume (rShelf res) / 1000000 - used - used')
                      wastedVolume = totalWasted*used/(used+used')
                  in Just $ Pair res (Just res') (rVolumeLeftForAllFull res + wastedVolume)
                        (rUsedShelves res) (rUsedShelves res') (Both toFit toFit')
                        (totalWasted)
          -- does not mix
          (0, _) -> Nothing
          (_, _) -> error "Bug"

pTotalShelves :: Pair s -> Int
pTotalShelves = go <$> pN1 <*> pN2 <*> pMixed where
  go p1 p2 mix = p1 + p2 + case mix of
    LeftOnly 0 -> 0
    RightOnly 0 -> 0
    _  -> 1




findBestPairings :: Warehouse s ->  Map'.Map BoxKey [Residual s] -> [Box s] -> Box s -> [Pair s]
findBestPairings _ _ [] _ = []
findBestPairings wh residualMap boxes box =
  let residuals = lookupResidual residualMap box
      monopairs = catMaybes $ [ newPair wh r Nothing | r <- residuals]

  -- create a temporary map shelf -> to residual for current box
      resMap = Map'.fromList [(shelfKey (rShelf r), r) | r <- residuals ]
      pairs = do
          box' <- boxes -- each other box

          res' <- lookupResidual residualMap box' -- each shelf via residuals
          let shelf = rShelf res'
          res <- maybeToList $ Map'.lookup (shelfKey shelf) resMap
          maybeToList $ newPair wh res (Just res')

  in sortBy (comparing pWastedVolume) (pairs <> monopairs)


lookupResidual :: Map'.Map BoxKey [Residual s] -> Box s -> [Residual s]
lookupResidual residualMap box = fromMaybe [] $ Map'.lookup (boxKeyForGroup box) residualMap



-- | List all pairs of style and sorted efficiency
-- Residual (shelf,box) with insufficient ration of volume occupation
-- can be filtered.
reportPairs :: Int-> WH (IO ()) s
reportPairs percThreshold = do
  -- removes every boxes which are already in and the shelf itself
  ss <- mapM findShelf =<< toList <$> gets shelves

  shelfBoxes_ <- catMaybes <$> forM (filter (applyTagSelectors (mapMaybe parseTagSelector ["!sep", "!error"]) shelfTag) ss) ( \shelf -> do
    boxes <- findBoxByShelf shelf
    return $ if null boxes then Nothing else Just (shelfName shelf, map boxKey boxes)
    )


  let shelvesToSkip = Set.fromList (map fst shelfBoxes_ )
      boxesToSkip = Set.fromList (concatMap snd shelfBoxes_)
      isTop = maybeToList (parseTagSelector "top")
    
  boxGroups <- groupBoxes (\b -> boxKey b `Set.member` boxesToSkip)
  shelfGroups <- groupShelves (\s -> shelfName s `Set.member` shelvesToSkip || applyTagSelectors isTop shelfTag s)

  boxo <- gets boxOrientations

  wh <- get

  let residualMap = Map'.fromList
        [ (boxKeyForGroup box, residuals)
        | (box, qty) <- boxGroups
        , let residuals = [ residual
                          | (shelf, shelves) <- shelfGroups
                          , residual <- maybeToList $ findResidual (boxo box shelf) shelf box qty (length shelves)
                          , rPercentageUsedForFull residual > percThreshold
                          ]
        ]

  let boxGroupWeight (box, qty) = Down $ (fromIntegral qty) * boxVolume box
  let sortedBoxGroups = sortBy (comparing boxGroupWeight) boxGroups
  let boxes = map fst sortedBoxGroups

  let pairsS = map (findBestPairings wh residualMap boxes) boxes

  return $ mapM_ (mapM_  (putStrLn . showPair) {-. take 500-})  pairsS

showPair pair = let res = pRes1 pair
                    shelf = rShelf res
                    nbOfShelves = rNumberOfShelves res
  in case (pRes2 pair, pMixed pair) of
    (Just res', Both q q') ->  (boxStyle . rBox $ res) <>
                  (pack $ printf "x%02d" (rNumberOfBox res)) <> (showOrientation $ rOrientation res) <> " <=> "
                  <> (pack $ printf "%02dx" $ rNumberOfBox res')
                  <> (boxStyle $ rBox res' ) <> (showOrientation $ rOrientation res')
                  <> " [[ " <> (shelfNameTag shelf )
                  <> (pack $ printf" (%d|%d) ]] " nbOfShelves (nbOfShelves - pTotalShelves pair))
                  <> tshow (pN1 pair) <> ":"<> (tshow $ rPercentageUsedForFull res) <> "% | " <> tshow q <> "<>" <> tshow q' <> " | "
                  <> tshow (pN2 pair) <> ":" <> (tshow $ rPercentageUsedForFull res') <> (pack $ printf "mixLeft;%0.1f " (pMixedLeft pair)) <> (pack $ printf "%% wasted:%0.2f  " (pWastedVolume pair))
                  <> printDim (boxDim $ rBox res) <> (pack $ printf "(%d)" $ rBoxPerShelf res) <> " || " <> printDim (boxDim $ rBox res') <> (pack $ printf "(%d)" $ rBoxPerShelf res')

    (_, LeftOnly q ) -> (boxStyle . rBox $ res)
                  <> (pack $ printf "x%02d" (rNumberOfBox res)) <>" <=   0 "
                  <> " [[ " <> (shelfNameTag shelf) <> (pack $ printf " (%d|%d) ]] "
                                                                   nbOfShelves
                                                                   (nbOfShelves - pTotalShelves pair))
                  <> tshow (pN1 pair) <> ":" <> (tshow $ rPercentageUsedForFull res) <> "%  | " <> tshow q <> " _ | "
                  <> (pack $ printf " wasted:%0.2f  " (pWastedVolume pair))
                  <> printDim (boxDim $ rBox res) <> (pack $ printf "(%d)" $ rBoxPerShelf res)
    _ -> error "Bug"

