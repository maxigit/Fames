-- Warning ! This code has been generated !
-- Handler
module Handler.FAMES.Def where
import Import
import FAMES


getFAMESFamesBarcodeSeedR :: Handler Html 
getFAMESFamesBarcodeSeedR = entityTableHandler (FAMES'R FAMESFamesBarcodeSeedR) ([] :: [Filter FAMES.FamesBarcodeSeed]) 

getFAMESFamesBoxtakeR :: Handler Html 
getFAMESFamesBoxtakeR = entityTableHandler (FAMES'R FAMESFamesBoxtakeR) ([] :: [Filter FAMES.FamesBoxtake]) 

getFAMESFamesCommentR :: Handler Html 
getFAMESFamesCommentR = entityTableHandler (FAMES'R FAMESFamesCommentR) ([] :: [Filter FAMES.FamesComment]) 

getFAMESFamesDocumentKeyR :: Handler Html 
getFAMESFamesDocumentKeyR = entityTableHandler (FAMES'R FAMESFamesDocumentKeyR) ([] :: [Filter FAMES.FamesDocumentKey]) 

getFAMESFamesEmailR :: Handler Html 
getFAMESFamesEmailR = entityTableHandler (FAMES'R FAMESFamesEmailR) ([] :: [Filter FAMES.FamesEmail]) 

getFAMESFamesOperatorR :: Handler Html 
getFAMESFamesOperatorR = entityTableHandler (FAMES'R FAMESFamesOperatorR) ([] :: [Filter FAMES.FamesOperator]) 

getFAMESFamesPackinglistR :: Handler Html 
getFAMESFamesPackinglistR = entityTableHandler (FAMES'R FAMESFamesPackinglistR) ([] :: [Filter FAMES.FamesPackinglist]) 

getFAMESFamesPackinglistDetailR :: Handler Html 
getFAMESFamesPackinglistDetailR = entityTableHandler (FAMES'R FAMESFamesPackinglistDetailR) ([] :: [Filter FAMES.FamesPackinglistDetail]) 

getFAMESFamesStocktakeR :: Handler Html 
getFAMESFamesStocktakeR = entityTableHandler (FAMES'R FAMESFamesStocktakeR) ([] :: [Filter FAMES.FamesStocktake]) 

getFAMESFamesStockAdjustmentR :: Handler Html 
getFAMESFamesStockAdjustmentR = entityTableHandler (FAMES'R FAMESFamesStockAdjustmentR) ([] :: [Filter FAMES.FamesStockAdjustment]) 

getFAMESFamesStockAdjDetailR :: Handler Html 
getFAMESFamesStockAdjDetailR = entityTableHandler (FAMES'R FAMESFamesStockAdjDetailR) ([] :: [Filter FAMES.FamesStockAdjDetail]) 

getFAMESFamesTransactionMapR :: Handler Html 
getFAMESFamesTransactionMapR = entityTableHandler (FAMES'R FAMESFamesTransactionMapR) ([] :: [Filter FAMES.FamesTransactionMap]) 

getFAMESFamesUserR :: Handler Html 
getFAMESFamesUserR = entityTableHandler (FAMES'R FAMESFamesUserR) ([] :: [Filter FAMES.FamesUser]) 

