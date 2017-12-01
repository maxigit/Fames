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
import Data.Colour (Colour,blend)
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
  traceShowM ("CACHE WAREHOUSE IN", key, freeze warehouse)

  cache0 False (cacheDay 1) ("warehouse", key) (return . Just $ freeze warehouse)

cacheWarehouseOut :: DocumentHash -> Handler (Maybe WarehouseCache)
cacheWarehouseOut (DocumentHash key) = do
  traceShowM ("CACHE WAREHOUSE OUT", key)
  wcache <- cache0 False (cacheDay 1) ("warehouse", key) (traceShowM "wh not found" >> return Nothing)
  traceShowM wcache
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
  traceShowM ("CACHE Scenario IN", key)
  cache0 False (cacheDay 1) ("scenario", key) (return $ Just (sc, layoutSize))
  return (key, layoutSize)

cacheScenarioOut key = do
  traceShowM ("CACHE Scenario OUT", key)
  cache0 False (cacheDay 1) ("scenario", key) (return Nothing)

-- * Exec
colorFromTag :: Box s -> Colour Double
colorFromTag box = let
  colors = mapMaybe readColourName (boxTags box)
  in case colors of
  [] -> wheat
  (col:_) -> blend 0.2 wheat col

execScenario sc@Scenario{..} = do
  initialM <- join <$> cacheWarehouseOut `mapM` sInitialState
  traceShowM ("EXEC" ,
          case initialM of
            Nothing -> "no initial state found"
            _  -> "INITIAL FOUND" 
    )
  stepsW <- lift $ mapM executeStep sSteps
        -- put (fromMaybe emptyWarehouse (unsafeCoerce initialM))
  -- execute and store the resulting warehouse
  (_, warehouse) <- runWH (maybe emptyWarehouse unfreeze initialM) {colors = colorFromTag} (sequence stepsW >> return ())
  let key = warehouseScenarioKey sc
  cacheWarehouseIn key warehouse
  return warehouse

execWithCache sc = do
  let key = warehouseScenarioKey sc
  traceShowM ("Scenario KEY:", key)
  wM <- cacheWarehouseOut key
  case wM of
    Nothing -> traceShow "Exec with cache new" execScenario sc
    Just wh -> traceShow "USE cached" $ return  $ unfreeze wh


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




