{-# LANGUAGE OverloadedLabels, OverloadedRecordDot, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
module Handler.Items.Reports.Sources
where 

import Import hiding(on, (==.), selectSource)
-- import qualified Database.Persist as P
import Database.Esqueleto.Experimental
-- import Handler.Util
import FA

type EntityX a = SqlExpr (Entity a)


itemSalesSource = select $ itemSalesQuery  "M%" "" ""
itemSalesQuery :: Text -> Text -> _StockFilter -> SqlQuery (EntityX DebtorTran :& EntityX DebtorTransDetail)
itemSalesQuery stockLike _defaultLocation  _stockFilter = 
  -- let (stockJoin, stockWhere, stockParams) = stockFilterToSql  stockFilter
  let query = do 
            (trans :& detail) <- from $ debtorTransAndDetailsTable
                                             `innerJoinIf` ( Just $ 
                                                           (table @StockMove) `on` (\( trans :& detail :& move)  -> detail.stockId ==. move.stockId
                                                                                                          &&. detail.debtorTransNo ==. just move.transNo
                                                                                                          &&. detail.debtorTransType ==. just (move ^. #type)
                                                                                                          &&. trans.tranDate ==. move.tranDate
                                                                                 )
                                                           )
            where_ (detail.stockId `like` val (stockLike :: Text) )
            pure (trans :& detail)

  in query

  
t `innerJoinIf` (Just b) = From do
     (a :& _, fn) <- unFrom $ t `innerJoin` b
     return $ (a, fn)
t `innerJoinIf` Nothing = t
                
-- debtorTransAndDetailsTable :: From (SqlExpr (Entity DebtorTran) :& SqlExpr (Entity DebtorTransDetail))
debtorTransAndDetailsTable = 
    table
    `innerJoin` table `on`(\(trans :& detail) ->  just trans.transNo ==. detail.debtorTransNo
                                             &&. just (trans ^. #type) ==. detail.debtorTransType
                          )

salesOrderAndDetailsTable :: From (SqlExpr (Entity SalesOrder) :& SqlExpr (Entity SalesOrderDetail))
salesOrderAndDetailsTable =
   from table
   `innerJoin` table  `on` (\(order :& detail) -> order.orderNo ==. detail.orderNo
                                             &&. order.transType ==. detail.transType
                           )
                           
                    
    

