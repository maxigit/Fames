{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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

-- import Control.Monad.ST (runST, stToIO, RealWorld)
import WarehousePlanner.Base
import WarehousePlanner.Optimum
import Prelude hiding(printf)
import Control.Monad.ST.Unsafe(unsafeSTToIO)
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad.State
import Text.Printf(printf)
import qualified Data.Map.Strict as Map'
import qualified Data.Set as Set
-- import qualified Data.Vector as Vec
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Function(on)
import Data.List (sortBy, partition, intercalate, nub, sort, tails, stripPrefix)
import Data.Ord (comparing, Down(..))
import Control.Applicative
import Data.Monoid
import Data.Foldable (toList)
import qualified Data.Foldable as F
import qualified Data.Traversable  as T
import Data.Maybe
import Data.Time(Day, formatTime, defaultTimeLocale)

import Text.Tabular as Tabul
import qualified Text.Tabular.AsciiArt as TAscii
import qualified Text.Tabular.Html as THtml
import qualified Text.Tabular.Csv as TCsv

import Debug.Trace

reportAll :: WH [String] s
reportAll = do
    sb <- shelfBoxes
    let groups =  Map'.fromListWith (+) (map (\(s,b) -> ((shelfName s, boxDim b, orientation b, boxStyle b), (1::Int))) sb)
    return $ map toString (Map'.toList groups)
    where toString ((name, dim, o, style), count) =
            name
                ++ ", " ++ style
                ++ ", " ++ show count  ++ ", " ++ showOrientation o

-- | Find the box which fit a given shelf  the best
bestBoxesFor :: String -> WH [String] s
bestBoxesFor shelf = do
    boxIds <- toList <$> gets boxes
    boxes' <- mapM findBox boxIds
    shelves <- findShelfByName shelf >>= mapM findShelf
    getOr <- gets boxOrientations

    let groups  = Map'.fromList (map (\b -> ((boxDim b, boxStyle b), b)) boxes')
        boxes = Map'.elems groups
    let bors = map (\b -> (b, getOr b)) boxes
        boxHeight = dHeight . boxDim
        shelfHeight = dHeight . minDim

    let s  = head shelves
        tries = [ (-((fromIntegral (n*k*m))*(boxVolume box / boxHeight box / shelfVolume s * shelfHeight s)), box)
            | (box, ors) <- bors
            , let (_,n,k,m,_) = bestArrangement (ors s) [(maxDim s, s)] (_boxDim box)
            ]

        bests = sortBy (compare `on` fst) tries

    mapM (report s) (map snd bests)

-- | Find best boxes ignoring the shelve height
bestHeightForShelf :: String -> WH (IO ()) s
bestHeightForShelf shelf = do
    boxIds <- toList <$> gets boxes
    boxes' <- mapM findBox boxIds
    shelves <- findShelfByName shelf >>= mapM findShelf
    getOr <- gets boxOrientations

    let groups  = Map'.fromList (map (\b -> ((boxDim b, boxStyle b), b)) boxes')
        boxes = Map'.elems groups
    let bors = map (\b -> (b, getOr b)) boxes

    let s  = head shelves
        tries = [ (-((fromIntegral (n*k*m))*boxVolume box / shelfVolume s), (box, or, (n,k,m)))
            | (box, ors) <- bors
            , let (or,n,k,m,_) = bestArrangement' (ors s) [(maxDim s, s)] (_boxDim box)
            ]

        bests = sortBy (compare `on` fst) tries
        reportHeight shelf  (box, or, (n,k,m)) = do
          similarBoxes <- findBoxByStyle (boxStyle box)
          let box' = box { orientation = or }
          let lengthRatio = ( dLength (boxDim box') * (fromIntegral n)
                            / dLength (minDim shelf)
                            )
              widthRatio = ( dWidth (boxDim box') * (fromIntegral k)
                           / dWidth (minDim shelf)
                           )
              height = dHeight (boxDim box')
              recommendedHeight = height * (fromIntegral ((ceiling (130/height)) :: Int))
              numberOfBoxes = length similarBoxes
              shelvesNeeded = fromIntegral numberOfBoxes * recommendedHeight / height / fromIntegral n

          return $ putStrLn $ "box: " ++ boxStyle box
            ++ ", height: " ++ printf " %0.2f (%0.2f)" recommendedHeight height
            ++ " " ++ showOrientation or
            ++ printf " %dx%dx%d" n k (floor (recommendedHeight/height) :: Int)
            ++ printf " H:%0.1f%%" (100*lengthRatio)
            ++ printf " (%0.1f%%)" (100*lengthRatio*widthRatio)
            ++ printf " : %d b -> %0.1f s " numberOfBoxes (shelvesNeeded :: Double)

    ios <- mapM (reportHeight s) (map snd bests)
    return $ sequence_ ios

-- | Like best arrangement but only take the
bestArrangement' :: Show a => [(Orientation, Int, Int)] -> [(Dimension, a)] -> Dimension -> (Orientation, Int, Int, Int, a)
bestArrangement' orientations shelves box = let
    options = [ (o, extra, (nl, max minW (min nw maxW), 1), sl*sw*sh)
              | (o, minW, maxW) <-   orientations
              , (shelf, extra) <- shelves
              , let Dimension sl sw sh =  shelf
              , let (nl, nw, _nh) = howMany shelf (rotate o box)
              ]

    bests = sortBy (compare `on` fst)
                [ ( ( -nl*nh*nw
                    , bh*bh*fromIntegral(nh*nh)+bl*bl*fromIntegral(nl*nl)
                    , vol
                    )
                  , (ori, nl, nw, nh, extra)

                  )
                 | (ori, extra, (nl, nw, nh), vol ) <- options
                 , let Dimension bl bh _bw = rotate ori box
                 ]
    in
        -- trace ({-show shelves ++ show box ++-}  show bests) $
        (snd . head) bests


report :: Shelf s -> Box s -> WH String s
report shelf box = do
    getOr <- gets boxOrientations
    similarBoxes <- findBoxByStyle (boxStyle box)
    let (or,n,k,m,_) = bestArrangement (getOr box shelf) [(maxDim shelf, shelf)] (_boxDim box)
        ratio = boxVolume box * fromIntegral (n * k * m) / shelfVolume shelf
        numberOfBoxes = length similarBoxes
        shelvesNeeded = fromIntegral numberOfBoxes / fromIntegral (n*m*k)
    return $ "box: " ++ boxStyle box ++ ", shelf: " ++ shelfName shelf
                        ++ " " ++ showOrientation or
                        ++  printf " %0.1f%%" (ratio*100)
                        ++ printf " %dx%dx%d" n k m
                        ++ printf " (%d)" (n*m*k)
                        ++ printf " : %d b -> %0.1f s " numberOfBoxes (shelvesNeeded :: Double)

                        -- ++ show (_boxDim box) ++ show (maxDim shelf)



-- | find the best shelf for a given style
-- Doesn't take into account boxes already there.
bestShelvesFor :: String -> WH [String]s
bestShelvesFor style = do
    boxes <- findBoxByStyle  style >>= mapM findBox
    shelves <- toList <$> gets shelves >>= mapM findShelf
    or <- gets boxOrientations

    let box = head boxes
        bests = bestShelves box (or box) shelves

    mapM (flip report $ box) bests

-- | find the best shelf for a given style
-- depends on what's already there.
bestAvailableShelvesFor :: String -> WH [String] s
bestAvailableShelvesFor style = do
    boxes <- findBoxByStyle  style >>= mapM findBox
    or <- gets boxOrientations
    let  box = head boxes
    shelves <- toList <$> gets shelves >>= mapM findShelf
    -- sort is stable so by passing shelves in
    -- the best order we get at shelves sorted by
    -- n and then best order
    let bests = bestShelves box (or box) shelves
    let   getInfo shelf = do
            boxesLeft <- moveBoxes ExitLeft boxes [shelf]
            return (shelf, length boxes - length boxesLeft)
    shelfInfos <- mapM getInfo bests
    let go (shelf, n) = do
            r <- report shelf  (head boxes)
            return $  printf "%d/%d => " n (length boxes) ++ r
    mapM go ( sortBy (comparing (Down . snd))
                     (filter ((/=0).snd) shelfInfos)
                   )




-- * Summary
-- Display total volume shelf volume
-- with a breakdown per shelf tags

data SummaryInfo = SummaryInfo
  { siFloor :: Maybe Double -- ^ m^2
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
instance Monoid SummaryInfo where
  (SummaryInfo f t u) `mappend` (SummaryInfo f' t' u') =
    SummaryInfo (f `mPlus` f') (t `mPlus` t') (u+u')
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

summary :: WH ([[String]], [String])  s
summary = do
    ss <- toList <$> gets shelves >>= mapM findShelf
    let groups = Map'.fromListWith (<>) [ (shelfTag s, [s])
                                        | s <- filter ((/= Just "sep") . shelfTag ) ss
                                        ]
    infos' <- traverse summarizeShelves groups
    let infos = Map'.mapWithKey adjust infos'
        adjust tag si = case tag of
          (Just ('_':_)) -> si {siTotalVolume = Nothing, siFloor = Nothing }
          _ -> si
    let total = mconcat (Map'.elems infos)

        f = show . (\x -> x :: Int) . round :: Double -> String
        renderSI si = let values = [ f . siUsedVolume
                                   , (maybe "" f) .  siTotalVolume
                                   , (maybe "" f) . siFree
                                   , (maybe "" f) . siUsedPercent
                                   , (maybe "" f) . siFloor
                                   ] <*> [si]
                      in values

        _unused_table = Table (Group NoLine (Header . fst <$> Map'.toList infos))
                      (Group SingleLine (map Header (words "used total free %used floor")))
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
shelvesReport :: WH [String] s
shelvesReport = do
  ss <- toList <$> gets shelves >>= mapM findShelf
  ls <- mapM  report ss

  return $ ("name,comment,length,width,height,depthLeft,usedRatio") : ls

  where report :: Shelf s -> WH String s
        report shelf = do
          let (Dimension l w h) = minDim shelf
          (name, depth) <- usedDepth shelf
          let left = w - depth

          -- find max depth
          return $ printf "%s%s,%s,%0.2f,%0.2f,%0.2f,%0.2f,%.0f%%\n"
                    (shelfName shelf) (maybe "" ('#':) (shelfTag shelf))
                    (printf "%s (%0.2f)" name depth :: String)
                    l w h
                    left
                    (depth/w*100)


-- returns the lis
listShelfTags :: WH ([String]) s
listShelfTags = do
  ss <- toList <$> gets shelves >>= mapM findShelf
  return . nub . sort $ catMaybes (map shelfTag ss)

shelfTagsReport :: WH (IO ()) s
shelfTagsReport = do
  tags <- listShelfTags
  return $ (mapM_ putStrLn tags)

-- * box report
-- | Display list of all individual boxes and their location
boxesReport :: WH (IO ()) s
boxesReport = do
  sbS <- shelfBoxes
  ios <- mapM report sbS

  return $ sequence_ ((putStrLn "style,content,location"):ios)

  where -- report :: Box s -> WH (IO ()) s
        report (shelf, box) = do
          let (nl, nw, nh) = howMany (maxDim shelf) (boxDim box)
          return $ printf "%s,%s,%s,%s,%dx%dx%d\n" (boxStyle box) (boxContent box) (shelfName shelf) (showOrientation (orientation box)) nl nw nh
  
-- * Compatible with warehouse planner input
-- | Generates moves from actual styles positions, ie find all shelves
-- for  a given styles

groupNames :: [String] -> [String]
groupNames names = let
  groups = Map'.fromListWith (++) [(init name, [last name]) | name <- names]

  toName (prefix, []) = prefix
  toName (prefix, [x]) = prefix ++ [x]
  toName (prefix, xs) = printf "%s[%s]" prefix (sort xs)

  in map toName (Map'.toList groups)

boxStyleWithTags b = let
  isVirtual ('\'':_) = True
  isVirtual _ = False
  tags = filter (not . isVirtual) (boxTagList b)
  in intercalate "#" (boxStyleAndContent b : tags)
 
generateMoves :: (Box s -> String) -> WH [String] s
generateMoves boxName = generateMoves' "stock_id,location" (Just . boxName) printGroup where
     printGroup boxName _ shelves = boxName ++ "," ++ intercalate "|" (groupNames shelves)
-- generateMoves' :: (Box s -> String) -> (Box s -> [String]) -> WH [String] s
generateMoves' :: (Ord k, Eq k)
               => String -- ^ Header
               -> (Box s -> Maybe k) -- ^ box key
               ->  (k -> [Box s] -> [String]  -> String) -- ^ string from key, boxes and unique shelfnames
               -> WH [String] s
generateMoves' header boxKey printGroup = do
 s'bS <- shelfBoxes
 let groups = Map'.fromListWith (<>) [ (key, [(b, shelfName s)])
                                     | (s,b) <- s'bS
                                     , Just key <- [boxKey b]
                                     -- , "mop-exclude" `notElem` boxTags b
                                     ]
     printGroup' (key, box'shelves) = printGroup key boxes (nub $ sort shelves) where (boxes, shelves) = unzip box'shelves
 return (header
        : map printGroup' (Map'.toList groups)
        )
-- | Generates files compatible with MOP
generateMOPLocations :: WH [String] s
generateMOPLocations = generateMoves' "stock_id,location" boxName printGroup where
  -- use box style unless the box is tagged as exception
  boxName box = let tags = boxTags box
                    comment = F.asum $ map (stripPrefix "mop-comment=") (boxTagList box)
                in case ("mop-exclude" `elem` tags , "mop-exception" `elem` tags) of
                   (True, _) -> Nothing -- skipped
                   (False, True) -> Just $ (boxStyleAndContent box, comment)
                   (False, False) -> Just $ (boxStyle box, comment)
 -- add comment from tag
  printGroup (boxName, boxComment) _ shelves = boxName ++ "," ++
    case (boxComment, shelves) of
           (Nothing, name:names) -> intercalate "|" (name : groupNames names) 
           (Just comment, [name]) -> intercalate "|" [name , comment]
           (Just comment, name:names) -> intercalate "|" (name : groupNames names)  ++ " " ++ comment
                                        
-- | Generate a generic report using tags prefixed by the report param
generateGenericReport today prefix = generateMoves' "" boxKey printGroup where
  boxKey box = let tags = boxTags box
               in F.asum $ map (stripPrefix $ prefix ++ "key=") (Set.toList tags)
  printGroup boxKey [] shelfNames = boxKey
  printGroup boxKey boxes@(box:_) shelfNames = let
    value0 = F.asum $ map (stripPrefix (prefix ++ "value=")) (boxTagList box)
    value = maybe "" (expandReportValue today boxes shelfNames ) value0
    in value


-- | Expands group related properties, which can't be processed on a indivial box
-- e.g. boxes counts, shelves list etc ...
expandReportValue :: Day -> [Box s] -> [String] -> String -> String
expandReportValue today boxes shelves s = let
  updates = [ replace "${count}" (show $ length boxes)
            , replace "${locations}" (intercalate "|" $ groupNames shelves)
            , replace "${shelves}" (intercalate "|" shelves)
            , replace "${total-volume}" $ printf "%0.1f" ((sum $ map boxVolume boxes) * 1E-6)
            , replace "${style-count}" (show $ length styles)
            , replace "${styles}" $ (intercalate "|" styles)
            , replace "${content-count}" (show $ length contents)
            , replace "${contents}" $ (intercalate "|" contents)
            , replace "${sku-count}" (show $ length contents)
            , replace "${skus}" $ (intercalate "|" skus)
            , replace "${shelf-count}" $ (show $ length shelves)
            , replace "${dimensions-count}" (show  $ lengthBy' boxes _boxDim)
            , replace "${orientations}" orientations
            , replace "${orientation-count}" (show  $ length orientations)
            , replace "${hash}" "#"
            , replace "${comma}" ","
            , replace "${dollar}" "$"
            , replace "${divide}" "/"
            , replace "${today}" (formatTime defaultTimeLocale "%Y-%m-%d" today)
            ]
  orientations = concatMap showOrientation . nub . sort $ map orientation boxes
  styles = nub . sort $ map boxStyle boxes
  contents = nub . sort $ map boxContent boxes
  skus = nub. sort $ map boxStyleAndContent boxes
  in F.foldl' (flip ($)) s updates


lengthBy' :: (Ord k, Eq k) => [a] -> (a -> k) -> Int
lengthBy' boxes f = length . nub . sort $ (map f boxes)

replace :: String -> String -> String -> String
replace needle value [] = []
replace needle value s@(c:cs) = case stripPrefix needle s of
  Nothing -> c : replace needle value cs
  Just s' -> value ++ replace needle value s'

  
  

  
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
groupShelves :: (Shelf s -> Bool) -> WH [(Shelf s, [String])] s
groupShelves exclude = do
  ss <- toList <$> gets shelves >>= mapM findShelf
  let groups = Map'.fromListWith (<>) [ (shelfKey s, [s])
                                      | s <- filter (keepTag . shelfTag) ss
                                      , exclude s == False
                                      ]
      keepTag (Just "sep") = False
      keepTag (Just ('e':_)) = False
      keepTag _ = True
                               
      regroup ss = let sorted = sortBy (comparing shelfName) ss
                   in (head sorted, map shelfNameTag sorted)

  return $ map (regroup) (Map'.elems groups)


-- | Displays shelf groups summary
groupShelvesReport :: WH [String] s
groupShelvesReport = do
  groups <- groupShelves (const False)
  let sorted = sortBy (comparing ( head . snd  )) groups
  forM sorted $ \(s, names) -> do
      let d = maxDim s
      return $ show (length names) ++ " x " ++ (head names) ++ " -- " ++ (last names)
        ++ " " ++ printDim d


-- Displays shelf group Map (ie what belong to  which group)
groupShelvesReport' :: WH (IO ()) s
groupShelvesReport' = do
  groups <- groupShelves (const False)
  return $ do
    forM_ (sortBy (comparing (shelfName . fst)) groups) $ \(s, names) -> do
      forM names $ \name -> putStrLn $ name ++ " => " ++ (head names)

type BoxKey = (Dimension, String, [Orientation])
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
        regroup bs = (last bs, length bs)
    return $ map regroup (Map'.elems groups)

groupBoxesReport :: [Box s] -> WH [String] s
groupBoxesReport boxes = do
  groups <- groupBoxes' boxes
  let withVolumes = [(volume (boxDim b) * (fromIntegral q), b'q) | b'q@(b,q) <- groups ]
  let sorted = map snd $ sortBy (comparing (\(rank, (volume, _)) -> (rank, Down volume))) (zip [1..] withVolumes)
  forM sorted $ \(v, (box, qty)) -> do
      let d = _boxDim box
      return $ (boxStyle box ) ++ (concatMap showOrientation (boxBoxOrientations box )) ++   " x "  ++ show qty
               ++ " " ++ f (dLength d) ++ "," ++ f(dWidth d) ++ "," ++ f (dHeight d)
               ++ " " ++ f (v/1000000 ) ++ "m^3"
      where f = printf ("%0.1f")

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

findResidual :: [(Orientation, Int, Int)] -> Shelf s -> Box s -> Int -> Int -> Maybe (Residual s)
findResidual orientations shelf box qty nbOfShelf = let
  sdim = maxDim shelf
  bdim = _boxDim box
  (or, nl, nw, nh, ()) = bestArrangement orientations [(sdim, ())] bdim
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
newPair wh res Nothing  =  let
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
            (shelfTag shelf)
            (maxDim shelf)
            (maxDim shelf)
            (shelfBoxOrientator shelf)
            (shelfFillingStrategy shelf)
        addBoxes res shelf = do
          let box = rBox res
          forM [1..rBoxLeft res] $ \i -> newBox (boxStyle box)
                                             (boxContent box)
                                             (_boxDim box)
                                             (rOrientation res) -- current orientation
                                             shelf
                                             (boxBoxOrientations box)
                                             (boxTagList box)

        tryW = do
          -- traceShowM ("NEW PAIR", res, res')
          def <- addShelf (rShelf res)
          shelf <- addShelf (rShelf res)
          boxes <- addBoxes res def
          boxes' <- addBoxes res' def
          left <- moveBoxes ExitOnTop boxes [shelf]
          left' <- moveBoxes ExitOnTop boxes' [shelf]
          shelfR <- findShelf shelf

          -- traceShowM ("RES", length boxes, length left, length boxes', length left')
          return (length boxes, length left, length boxes', length left')


        ors = boxOrientations wh
        wh' = emptyWarehouse {boxOrientations=ors}
        (toFit, notFit, toFit', notFit' ) = unsafePerformIO $ unsafeSTToIO (evalStateT tryW wh')

      in case (notFit, notFit') of
          (0,0) -> -- the two partial shelves we need to extract the contribution
                  -- of the left boxes. For that we calculate the volume left in
                  -- in the mixed shelf and share it using the pro-rata of the overall volume used
                  let used = rVolumeUsedForLO res
                      used' = rVolumeUsedForLO res'
                      totalWasted = (shelfVolume (rShelf res) / 1000000 - used - used')
                      wastedVolume = traceShow (used, used', totalWasted, res, res')totalWasted*used/(used+used')
                  in Just $ Pair res (Just res') (rVolumeLeftForAllFull res + wastedVolume)
                        (rUsedShelves res) (rUsedShelves res') (Both toFit toFit')
                        (totalWasted)
          -- does not mix
          (0, _) -> Nothing

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

  in sortBy (comparing pWastedVolume) (pairs ++ monopairs)


lookupResidual :: Map'.Map BoxKey [Residual s] -> Box s -> [Residual s]
lookupResidual residualMap box = fromMaybe [] $ Map'.lookup (boxKeyForGroup box) residualMap



-- | List all pairs of style and sorted efficiency
-- Residual (shelf,box) with insufficient ration of volume occupation
-- can be filtered.
reportPairs :: Int-> WH (IO ()) s
reportPairs percThreshold = do
  -- removes every boxes which are already in and the shelf itself
  ss <- mapM findShelf =<< toList <$> gets shelves


  let keepTag (Just "sep") = False
      keepTag (Just ('e':_)) = False
      -- keepTag (Just ("top")) = False
      keepTag _ = True

  shelfBoxes <- catMaybes <$> forM (filter (keepTag . shelfTag) ss) ( \shelf -> do
    boxes <- findBoxByShelf shelf
    return $ if null boxes then Nothing else Just (shelfName shelf, map boxKey boxes)
    )

  let shelvesToSkip = Set.fromList (map fst shelfBoxes )
      boxesToSkip = Set.fromList (concatMap snd shelfBoxes)
    
  boxGroups <- groupBoxes (\b -> boxKey b `Set.member` boxesToSkip)
  shelfGroups <- groupShelves (\s -> shelfName s `Set.member` shelvesToSkip || shelfTag s == Just "top" )

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
    (Just res', Both q q') ->  (boxStyle . rBox $ res) ++
                  (printf "x%02d" (rNumberOfBox res)) ++ (showOrientation $ rOrientation res) ++ " <=> "
                  ++ (printf "%02dx" $ rNumberOfBox res')
                  ++ (boxStyle $ rBox res' ) ++ (showOrientation $ rOrientation res')
                  ++ " [[ " ++ (shelfNameTag shelf )
                  ++ (printf" (%d|%d) ]] " nbOfShelves (nbOfShelves - pTotalShelves pair))
                  ++ show (pN1 pair) ++ ":"++ (show $ rPercentageUsedForFull res) ++ "% | " ++ show q ++ "+" ++ show q' ++ " | "
                  ++ show (pN2 pair) ++ ":" ++ (show $ rPercentageUsedForFull res') ++ (printf "mixLeft;%0.1f " (pMixedLeft pair)) ++ (printf "%% wasted:%0.2f  " (pWastedVolume pair))
                  ++ printDim (boxDim $ rBox res) ++ (printf "(%d)" $ rBoxPerShelf res) ++ " || " ++ printDim (boxDim $ rBox res') ++ (printf "(%d)" $ rBoxPerShelf res')

    (_, LeftOnly q ) -> (boxStyle . rBox $ res)
                  ++ (printf "x%02d" (rNumberOfBox res)) ++" <=   0 "
                  ++ " [[ " ++ (shelfNameTag shelf) ++ (printf " (%d|%d) ]] "
                                                                   nbOfShelves
                                                                   (nbOfShelves - pTotalShelves pair))
                  ++ show (pN1 pair) ++ ":" ++ (show $ rPercentageUsedForFull res) ++ "%  | " ++ show q ++ " _ | "
                  ++ (printf " wasted:%0.2f  " (pWastedVolume pair))
                  ++ printDim (boxDim $ rBox res) ++ (printf "(%d)" $ rBoxPerShelf res)

