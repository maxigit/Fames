-- Warning ! This code has been generated !
-- Handler
module Handler.FAX.Def where
import Import
import FAX


getFAXItemRequestsR :: Handler Html 
getFAXItemRequestsR = entityTableHandler (FAX'R FAXItemRequestsR) ([] :: [Filter FAX.ItemRequest]) 

getFAXBoxDimensionsR :: Handler Html 
getFAXBoxDimensionsR = entityTableHandler (FAX'R FAXBoxDimensionsR) ([] :: [Filter FAX.BoxDimension]) 

getFAXDispatchedR :: Handler Html 
getFAXDispatchedR = entityTableHandler (FAX'R FAXDispatchedR) ([] :: [Filter FAX.Dispatched]) 

getFAXMyBigOrderR :: Handler Html 
getFAXMyBigOrderR = entityTableHandler (FAX'R FAXMyBigOrderR) ([] :: [Filter FAX.MyBigOrder]) 

getFAXMyPickupR :: Handler Html 
getFAXMyPickupR = entityTableHandler (FAX'R FAXMyPickupR) ([] :: [Filter FAX.MyPickup]) 

getFAXMyRatesR :: Handler Html 
getFAXMyRatesR = entityTableHandler (FAX'R FAXMyRatesR) ([] :: [Filter FAX.MyRate]) 

getFAXMyVatR :: Handler Html 
getFAXMyVatR = entityTableHandler (FAX'R FAXMyVatR) ([] :: [Filter FAX.MyVat]) 

getFAXStocktakePriorityR :: Handler Html 
getFAXStocktakePriorityR = entityTableHandler (FAX'R FAXStocktakePriorityR) ([] :: [Filter FAX.StocktakePriority]) 

getFAXStockAuditR :: Handler Html 
getFAXStockAuditR = entityTableHandler (FAX'R FAXStockAuditR) ([] :: [Filter FAX.StockAudit]) 

getFAXStockTakeR :: Handler Html 
getFAXStockTakeR = entityTableHandler (FAX'R FAXStockTakeR) ([] :: [Filter FAX.StockTake]) 

getFAXVolumesR :: Handler Html 
getFAXVolumesR = entityTableHandler (FAX'R FAXVolumesR) ([] :: [Filter FAX.Volume]) 

