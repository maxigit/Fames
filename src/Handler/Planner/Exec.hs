{-# OPTIONS_GHC -Wno-unused-top-binds #-} -- TODO remove
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-} -- TODO remove
{-# OPTIONS_GHC -Wno-name-shadowing #-} -- TODO remove
{-# OPTIONS_GHC -Wno-unused-do-bind #-} -- TODO remove
{-# OPTIONS_GHC -Wno-orphans #-} -- TODO remove
module Handler.Planner.Exec where

import Import hiding(get, toLower)
import Planner.Types
import Planner.Internal
import Planner.Colour(readFromPalette)
import WarehousePlanner.Base
import WarehousePlanner.Csv
import WarehousePlanner.Display
import Util.Cache
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.State (put,get)
import Data.Colour (Colour,affineCombo)
import Data.Colour.Names (readColourName,black,lightgray,white, wheat, darkorange, lightsteelblue, royalblue)
import Data.Colour.SRGB(sRGB24read)
import Data.Char(isHexDigit,toLower)

-- * Type
-- | Typeable version of Warehouse. Needed to be cached.

instance Show (Warehouse s) where
  show wh = "Warehouse : " ++ "\nBOXES:" ++ show (length $ boxes wh)
                           ++ "\nSHELVES:\n" ++ show (length $ shelves wh)
                           -- ++ "\nGROUPS\n" ++ show (length $ shelfGroup wh)
data WarehouseCache = WarehouseCache (Warehouse ()) deriving (Show, Typeable)
-- * Caching


cacheWarehouseIn :: DocumentHash -> Warehouse s -> Handler (Maybe WarehouseCache)
cacheWarehouseIn (DocumentHash key) warehouse = do
  cache0 False (cacheHour 1) ("warehouse", key) (return . Just $ freeze warehouse)

cacheWarehouseOut :: DocumentHash -> Handler (Maybe WarehouseCache)
cacheWarehouseOut (DocumentHash key) = do
  wcache <- cache0 False (cacheHour 1) ("warehouse", key) (return Nothing)
  -- This hack creates a key if the warehouse doesn't exists
  case wcache of
    Nothing -> do
      cache <- getsYesod appCache
      purgeKey cache ("warehouse", key) -- can occur in concurrency probleme FIXME
      return ()
    _ -> return ()
  return $ wcache

freeze :: Warehouse s -> WarehouseCache
freeze = WarehouseCache . unsafeCoerce

unfreeze :: WarehouseCache -> Warehouse s
unfreeze (WarehouseCache warehouse)= unsafeCoerce warehouse

cacheScenarioIn :: Scenario -> Handler (Text, Int)
cacheScenarioIn sc = do
  let (DocumentHash key) = scenarioKey sc
  layoutSize <- scenarioLayoutSize sc
  cache0 False (cacheHour 1) ("scenario", key) (return $ Just (sc, layoutSize))
  return (key, layoutSize)

cacheScenarioOut :: Text -> Handler (Maybe (Scenario, Int))
cacheScenarioOut key = do
  cache0 False (cacheHour 1) ("scenario", key) (return Nothing)

-- * Deep copy
copyWarehouse :: WH (WH (Warehouse s) s) t
copyWarehouse = do
  wh0 <- get
  -- buildGroup <- copyShelfGroup (shelfGroup wh0)
  buildGroup <- mapM copyShelf (shelves wh0)
  return $ do
    put (emptyWarehouse $ whDay wh0) { boxStyling = unsafeCoerce $ boxStyling wh0
                                    , shelfStyling = unsafeCoerce $ shelfStyling wh0
                                    , boxOrientations = unsafeCoerce $ boxOrientations wh0
                                    }
    sequence buildGroup
    get


copyShelfGroup :: ShelfGroup t -> WH (WH (ShelfGroup s) s) t
copyShelfGroup (ShelfProxy sId) = do
  sIdM <- copyShelf sId
  return $ do
    sId' <- sIdM
    return $ ShelfProxy sId'
copyShelfGroup (ShelfGroup gIds dir) = do
  gIdM <- mapM copyShelfGroup gIds
  return $ do
    gIds' <- sequence gIdM
    return $ ShelfGroup gIds' dir

copyShelf :: ShelfId t -> WH (WH (ShelfId s) s) t
copyShelf sId = do
  Shelf{..} <- findShelf sId
  boxes <- findBoxByShelf sId
  buildBoxFnsM <- mapM copyBox boxes
  return $ do
    nshelf <- (newShelf shelfName (Just $ intercalate "#" $ flattenTags $ shelfTag) minDim maxDim shelfBoxOrientator shelfFillingStrategy)
    let nId = shelfId nshelf
    let _n = map ($ nId) buildBoxFnsM
    sequence _n
    updateShelf (\s ->  s {flow = flow} ) nshelf
    return nId
  
copyBox :: Box t -> WH (ShelfId s -> WH (Box s) s) t
copyBox box@Box{..} = return $ \shelf -> do
  newBox <- newBox boxStyle boxContent _boxDim orientation shelf boxBoxOrientations (getTagList box)
  updateBox (\b -> b { boxOffset = boxOffset, boxTags = boxTags}) newBox

-- * Exec
-- underscores are stripped before looking for the color name
-- this allow the same colours to be used more that once
-- example, navy#_navy#white,  will use navy twice
colorFromTag :: HasTags tagged => tagged -> Text -> Maybe (Colour Double)
colorFromTag box tag = let
  colors = mapMaybe valueToColour (getTagValues box tag)
  in case colors of
  [] -> Nothing
  [col] -> Just col
  (col:cols) -> let w = 1/fromIntegral (length colors) -- ALL colors
                in Just $ affineCombo (map (w,) cols) col

-- | Extract styling information from tag as properties
-- use fg= foregroung
stylingFromTags :: Box s -> BoxStyling
stylingFromTags box = let
  foreground = black `fromMaybe` colorFromTag box "fg"
  background = wheat `fromMaybe` colorFromTag box "bg"
  background2 = colorFromTag box "circle"
  border = colorFromTag box "border"
  title = getTagValues box "title"
  barTitle= case getTagValues box "bar-title" of
               [] -> Nothing
               vs -> Just $ intercalate "\n" vs
  displayBarGauge = not (tagIsPresent box "no-bar-gauge")
  offsetBarGaugeX = getTagValuem box "bar-gauge-x" >>= readMay
  offsetBarGaugeY = getTagValuem box "bar-gauge-y" >>= readMay
  in BoxStyling{..}

shelfStylingFromTags :: Shelf s -> ShelfStyling
shelfStylingFromTags shelf = let
  isSeparator = tagIsPresent shelf "sep"
  foreground = black `fromMaybe` colorFromTag shelf "fg"
  background = (if isSeparator then white else lightsteelblue) `fromMaybe` colorFromTag shelf "bg"
  border = royalblue `fromMaybe` colorFromTag shelf "border"
  barBackground = (if isSeparator then lightgray else darkorange) `fromMaybe` colorFromTag shelf "bar-bg"
  barForeground = black `fromMaybe` colorFromTag shelf "bar-fg"
  title = getTagValues shelf "title"
  barTitle= getTagValuem shelf "bar-title"
  displayBarGauge = not (tagIsPresent shelf "no-bar-gauge") && not isSeparator
  in ShelfStyling{..}
  
-- | Transform tag value to colours
-- Try to read standard name or use hexadecimal
valueToColour :: Text -> Maybe (Colour Double)
valueToColour t = case dropWhile (=='_') s of
  [] -> Nothing
  cs | all isHexDigit cs && length cs == 3 -> Just $ sRGB24read (s >>= (replicate 2))
  cs | all isHexDigit cs && length cs == 6 -> Just $ sRGB24read s
  dropped -> readFromPalette (pack dropped) <|> readColourName (map Data.Char.toLower dropped)
  where s = unpack t

-- | blend all colours equaly.
-- folding using normal blend would not work as
-- the weight of the last colour would count for half of everything



defOrs :: [Orientation]
defOrs = [ tiltedForward, tiltedFR ]                       

-- | Execute a scenario, read and write cache if necessary.
-- The execution of each steps is cached, so that
-- when modifying a file in the middle results, all the steps
-- at the beginning which haven't changed don't need to be recalculated.
-- To do so, we create and execute a chain of scenario with one step
-- and the initial step corresponding to the previous one.
execScenario :: Scenario -> Handler (Warehouse RealWorld)
execScenario sc@Scenario{..} = do
  today <- todayH
  initialM <- join <$> cacheWarehouseOut `mapM` sInitialState
  let warehouse0 = maybe (emptyWarehouse today) unfreeze initialM
      go :: Warehouse RealWorld -> [Step] -> [Step] -> Handler (Warehouse RealWorld)
      go w _ [] = return w
      go warehouse (previous) (steps0) = do
        -- in order to only save at saving points
        -- we need to execute all steps between saving points
        -- as a group
        let (toExecutes, steps') = break (== SavingPoint) steps0
            steps = drop 1 steps' -- drop SavingPoint if step non empty
            allPreviousSteps = previous <> toExecutes
        (wCopyM,_) <- runWH warehouse copyWarehouse
        let subKey = warehouseScenarioKey $ Scenario Nothing allPreviousSteps  Nothing
        wM <- cacheWarehouseOut subKey
        w <- case wM of
          Nothing -> do
            execM <- liftIO $ mapM executeStep toExecutes
            (_, w') <- runWH (emptyWarehouse today)  $ do
              wCopy <- wCopyM
              put wCopy { boxStyling = stylingFromTags, shelfStyling = shelfStylingFromTags }
              sequence_ execM
            -- traceShowM ("Scenario step => execute", subKey)
            cacheWarehouseIn subKey w'
            return w'
          Just w' -> {-traceShowM ("Scenario Step => use cache", subKey) >>-} (return $ unfreeze w')
        -- carry on with the remaing steps
        go w (allPreviousSteps) steps
  go warehouse0 [] (sSortedSteps sc) where
  

execWithCache :: Scenario -> Handler (Warehouse RealWorld)
-- execWithCache = execScenario
execWithCache sc = do
  let key = warehouseScenarioKey sc
  wM <- cacheWarehouseOut key
  case wM of
    Nothing -> execScenario sc
    Just wh -> return  $ unfreeze wh


renderScenario :: Scenario -> Maybe DocumentHash
               -> Handler (Either String [_Diagram])
renderScenario sc layoutM = do
  case layoutM <|> (sLayout sc) of
    Nothing -> return $ Left "No layout provided"
    Just layout -> do
        wh0 <- execWithCache sc
        groupW <- liftIO $ readWarehouse (contentPath layout)
        diags <- execWH wh0 ( do
                                group <- groupW
                                renderSlices group
                           )
        return (Right diags)

renderReport sc report = do
  wh0 <- execWithCache sc
  execWH wh0 report



