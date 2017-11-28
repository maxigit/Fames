module Planner.Internal where

import ClassyPrelude.Yesod
import WarehousePlanner.Display
import WarehousePlanner.Base
import Control.Monad.ST (runST, stToIO, RealWorld)
import Control.Monad.State (evalStateT,runStateT)

import Unsafe.Coerce (unsafeCoerce)


-- * Example
warehouseExamble  = do
  let dim0 = Dimension 270 80 145
  let dim1 = Dimension 31 34 72
  shelves <- mapM (  \i -> newShelf ("A1" <> show i) Nothing dim0  dim0 DefaultOrientation ColumnFirst ) [1..50]
  let shelfid = shelfId (headEx shelves)
  boxes <- mapM (\i -> newBox "style" (show i) dim1 up shelfid [up] ) [1..300]
  moveBoxes boxes shelves
  -- rearrangeShelves [shelf, shelf2]
  
  return $ ShelfGroup (map (ShelfProxy .shelfId) shelves) Vertical

-- * Parsing
-- warehouseFromOrg :: WH (ShelfGroup s)
warehouseFromOrg text = warehouseExamble


-- * Rendering

warehouseToDiagram warehouse = do
  let exec = do
        group <- warehouse
        renderGroup group
  diag <- execWH0 warehouse exec
  return diag

execWH0 wh = execWH emptyWarehouse
execWH warehouse0 wh = lift $ stToIO $ evalStateT wh warehouse0

runWH wh warehouse0= lift . stToIO $ runStateT wh warehouse0


freeze :: Warehouse s -> Warehouse ()
freeze = unsafeCoerce

unfreeze :: Warehouse () -> Warehouse s
unfreeze = unsafeCoerce
