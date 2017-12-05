module Handler.Planner.Exec where

import Import
import Planner.Types
import Planner.Internal
import WarehousePlanner.Base
import WarehousePlanner.Csv
import WarehousePlanner.Display
import Util.Cache
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.State (put)
import Data.Colour (Colour,blend,over, affineCombo)
import Data.Colour.Names (readColourName,wheat)

-- * Type
-- | Typeable version of Warehouse. Needed to be cached.

instance Show (Warehouse s) where
  show wh = "Warehouse : " ++ "\nBOXES:" ++ show (length $ boxes wh)
                           ++ "\nSHELVES:\n" ++ show (length $ shelves wh)
                           -- ++ "\nGROUPS\n" ++ show (length $ shelfGroup wh)
data WarehouseCache = WarehouseCache (Warehouse ()) deriving (Show, Typeable)
-- * Caching


cacheWarehouseIn (DocumentHash key) warehouse = do
  cache0 False (cacheDay 1) ("warehouse", key) (return . Just $ freeze warehouse)

cacheWarehouseOut :: DocumentHash -> Handler (Maybe WarehouseCache)
cacheWarehouseOut (DocumentHash key) = do
  wcache <- cache0 False (cacheDay 1) ("warehouse", key) (return Nothing)
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

cacheScenarioIn sc = do
  let (DocumentHash key) = scenarioKey sc
  layoutSize <- scenarioLayoutSize sc
  cache0 False (cacheDay 1) ("scenario", key) (return $ Just (sc, layoutSize))
  return (key, layoutSize)

cacheScenarioOut key = do
  cache0 False (cacheDay 1) ("scenario", key) (return Nothing)

-- * Exec
-- underscores are stripped before looking for the color name
-- this allow the same colours to be used more that once
-- example, navy#_navy#white,  will use navy twice
colorFromTag :: Box s -> Colour Double
colorFromTag box = let
  colors = mapMaybe readColourName (map (dropWhile (=='_')) (boxTags box))
  in case colors of
  [] -> wheat
  [col] -> col
  (col:cols) -> let w = 1/fromIntegral (length colors) -- ALL colors
                in affineCombo (map (w,) cols) col

-- | blend all colours equaly.
-- folding using normal blend would not work as
-- the weight of the last colour would count for half of everything



defOrs = [ tiltedForward, tiltedFR ]                       

                            
-- some steps need to be done before other to make sense
-- shelves first, then orientations rules, then in inital order
sSortedSteps Scenario{..} = let
  steps = zipWith key sSteps  [1..]
  key step@(Step header hash _) i = ((priority header, i), step)
  priority header = case header of
                         ShelvesH  -> 1
                         OrientationsH -> 2
                         _ -> 3
               

  sorted = sortBy (comparing fst) steps
  in  map snd sorted
  
execScenario sc@Scenario{..} = do
  initialM <- join <$> cacheWarehouseOut `mapM` sInitialState
  stepsW <- lift $ mapM executeStep (sSortedSteps sc)
        -- put (fromMaybe emptyWarehouse (unsafeCoerce initialM))
  -- execute and store the resulting warehouse
  (_, warehouse) <- runWH (maybe emptyWarehouse unfreeze initialM) { colors = colorFromTag}
                          (sequence stepsW >> return ())
  let key = warehouseScenarioKey sc
  cacheWarehouseIn key warehouse
  return warehouse

execWithCache sc = do
  let key = warehouseScenarioKey sc
  wM <- cacheWarehouseOut key
  case wM of
    Nothing -> execScenario sc
    Just wh -> return  $ unfreeze wh


renderScenario sc layoutM = do
  case layoutM <|> (sLayout sc) of
    Nothing -> return $ Left "No layout provided"
    Just layout -> do
        wh0 <- execWithCache sc
        groupW <- lift $ readWarehouse (contentPath layout)
        diags <- execWH wh0 ( do
                                group <- groupW
                                renderSlices group
                           )
        return (Right diags)


renderReport sc report = do
  wh0 <- execWithCache sc
  execWH wh0 report



