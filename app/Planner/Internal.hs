module Planner.Internal where

import ClassyPrelude.Yesod
import WarehousePlanner.Display
import WarehousePlanner.Base
import Control.Monad.ST (runST, stToIO, RealWorld)
import Control.Monad.State (evalStateT)


-- * Example
warehouseExamble  = do
  let dim0 = Dimension 270 80 145
  let dim1 = Dimension 31 34 72
  shelf <- newShelf "A1" Nothing dim0  dim0 DefaultOrientation ColumnFirst
  shelf2 <- newShelf "A2" Nothing dim0  dim0 DefaultOrientation ColumnFirst
  let shelfid = shelfId shelf
  boxes <- mapM (\i -> newBox "style" (show i) dim1 up shelf [up] ) [1..30]
  moveBoxes boxes [shelf2]
  rearrangeShelves [shelf, shelf2]
  
  return (ShelfGroup [ShelfProxy shelfid, ShelfProxy (shelfId shelf2)]
                                   Vertical
                       )

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
