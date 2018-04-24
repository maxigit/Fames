module Handler.Items.Reports.Common where

import Import
import Handler.Table
import Items.Types
import Handler.Items.Common
import FA
import Data.Time(addDays)
import qualified Data.Map as Map

-- * DB
loadItemTransactions = do
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  today <- utctDay <$> liftIO getCurrentTime
  let to = today
      from = addDays (-365) to
  moves <- runDB $ selectList ( [ FA.StockMoveTranDate >=. from
                                , FA.StockMoveTranDate <=. to
                                , FA.StockMoveType ==. fromEnum ST_CUSTDELIVERY
                                ]
                                <> filterE id FA.StockMoveStockId (Just . LikeFilter $ stockLike)
                              )
                              [] -- LimitTo 100]
  let info = map (moveToTransInfo . entityVal) moves
      grouped = Map.fromListWith(<>) [(iiStyle i, iiInfo i ) | i <- info ]

  return $ Map.toList grouped


-- * Converter
moveToTransInfo FA.StockMove{..} = ItemInfo style var info where
  (style, var) = skuToStyleVar stockMoveStockId
  info = qprice stockMoveQty (stockMovePrice*(1-stockMoveDiscountPercent/100))
  

-- * Reports
-- Display sales and purchase of an item
itemReport = do
  trans <- loadItemTransactions
  return [whamlet|
    $forall tran <- trans
      <p>#{tshow tran}
                 |]
