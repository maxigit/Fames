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

-- * Type
-- | Typeable version of Warehouse. Needed to be cached.
data WarehouseCache = WarehouseCache (Warehouse ()) deriving Typeable
-- * Caching

        

cacheWarehouseIn (DocumentHash key) warehouse =
  cache0 False (cacheDay 1) ("warehouse", key) (return . Just $ freeze warehouse)
cacheWarehouseOut (DocumentHash key) = do
  wcache <- cache0 False (cacheDay 1) ("warehouse", key) (return Nothing)
  return $ unfreeze <$> wcache

freeze :: Warehouse s -> WarehouseCache
freeze = WarehouseCache . unsafeCoerce

unfreeze :: WarehouseCache -> Warehouse s
unfreeze (WarehouseCache warehouse)= unsafeCoerce warehouse

cacheScenarioIn sc = do
  let (DocumentHash key) = scenarioKey sc
  cache0 False (cacheDay 1) ("scenario", key) (return . Just $ sc)
  return key

cacheScenarioOut key = do
  cache0 False (cacheDay 1) ("scenario", key) (return Nothing)
  

-- * Exec

execScenario sc@Scenario{..} = do
  initialM <- traverse cacheWarehouseOut sInitialState
  stepsW <- lift $ mapM executeStep sSteps
        -- put (fromMaybe emptyWarehouse (unsafeCoerce initialM))
  -- execute and store the resulting warehouse
  (_, warehouse) <- runWH (fromMaybe emptyWarehouse (unsafeCoerce initialM)) (sequence stepsW >> return ())
  let key = warehouseScenarioKey sc
  cacheWarehouseIn key warehouse
  return warehouse

execWithCache sc = do
  let key = warehouseScenarioKey sc
  wM <- cacheWarehouseOut key
  case wM of
    Nothing -> execScenario sc
    Just wh -> return wh


renderScenario sc layoutM = do
  case layoutM <|> (sLayout sc) of
    Nothing -> return $ Left "No layout provided"
    Just layout -> do
        wh0 <- execWithCache sc
        groupW <- lift $ readWarehouse (contentPath layout)
        diag <- execWH wh0 ( do
                                group <- groupW
                                renderGroup group
                           )
        return (Right diag)




