-- Warning ! This code has been generated !
-- Handler
module Handler.FAX.Def where
import Import
import FAX


{-# NOINLINE getFAXItemRequestsR #-}
getFAXItemRequestsR :: Handler Html 
getFAXItemRequestsR = entityTableHandler (FAX'R FAXItemRequestsR) ([] :: [Filter FAX.ItemRequest]) 

{-# NOINLINE getFAXAccesslogR #-}
getFAXAccesslogR :: Handler Html 
getFAXAccesslogR = entityTableHandler (FAX'R FAXAccesslogR) ([] :: [Filter FAX.Accesslog]) 

{-# NOINLINE getFAXActionsR #-}
getFAXActionsR :: Handler Html 
getFAXActionsR = entityTableHandler (FAX'R FAXActionsR) ([] :: [Filter FAX.Action]) 

{-# NOINLINE getFAXAdvancedHelpIndexR #-}
getFAXAdvancedHelpIndexR :: Handler Html 
getFAXAdvancedHelpIndexR = entityTableHandler (FAX'R FAXAdvancedHelpIndexR) ([] :: [Filter FAX.AdvancedHelpIndex]) 

{-# NOINLINE getFAXBoxDimensionsR #-}
getFAXBoxDimensionsR :: Handler Html 
getFAXBoxDimensionsR = entityTableHandler (FAX'R FAXBoxDimensionsR) ([] :: [Filter FAX.BoxDimension]) 

{-# NOINLINE getFAXDispatchedR #-}
getFAXDispatchedR :: Handler Html 
getFAXDispatchedR = entityTableHandler (FAX'R FAXDispatchedR) ([] :: [Filter FAX.Dispatched]) 

{-# NOINLINE getFAXMyBigOrderR #-}
getFAXMyBigOrderR :: Handler Html 
getFAXMyBigOrderR = entityTableHandler (FAX'R FAXMyBigOrderR) ([] :: [Filter FAX.MyBigOrder]) 

{-# NOINLINE getFAXMyPickupR #-}
getFAXMyPickupR :: Handler Html 
getFAXMyPickupR = entityTableHandler (FAX'R FAXMyPickupR) ([] :: [Filter FAX.MyPickup]) 

{-# NOINLINE getFAXMyRatesR #-}
getFAXMyRatesR :: Handler Html 
getFAXMyRatesR = entityTableHandler (FAX'R FAXMyRatesR) ([] :: [Filter FAX.MyRate]) 

{-# NOINLINE getFAXMyVatR #-}
getFAXMyVatR :: Handler Html 
getFAXMyVatR = entityTableHandler (FAX'R FAXMyVatR) ([] :: [Filter FAX.MyVat]) 

{-# NOINLINE getFAXStocktakePriorityR #-}
getFAXStocktakePriorityR :: Handler Html 
getFAXStocktakePriorityR = entityTableHandler (FAX'R FAXStocktakePriorityR) ([] :: [Filter FAX.StocktakePriority]) 

{-# NOINLINE getFAXStockAuditR #-}
getFAXStockAuditR :: Handler Html 
getFAXStockAuditR = entityTableHandler (FAX'R FAXStockAuditR) ([] :: [Filter FAX.StockAudit]) 

{-# NOINLINE getFAXStockTakeR #-}
getFAXStockTakeR :: Handler Html 
getFAXStockTakeR = entityTableHandler (FAX'R FAXStockTakeR) ([] :: [Filter FAX.StockTake]) 

{-# NOINLINE getFAXVolumesR #-}
getFAXVolumesR :: Handler Html 
getFAXVolumesR = entityTableHandler (FAX'R FAXVolumesR) ([] :: [Filter FAX.Volume]) 

