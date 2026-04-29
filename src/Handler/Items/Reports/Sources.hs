{-# LANGUAGE OverloadedLabels, OverloadedRecordDot, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
module Handler.Items.Reports.Sources
where 

import Import hiding(on, (==.), (!=.),(<=.),(>=.),(||.), selectSource, Value)
-- import qualified Database.Persist as P
import Database.Esqueleto.Experimental
import Database.Esqueleto.Experimental.From(ToFrom)
-- import Handler.Util
import FA
import Handler.Items.Reports.Types
import GL.Utils(generateDateIntervals)

type EntityX a = SqlExpr (Entity a)

instance SqlString StockMasterId -- needed to convert stock_master.stock_id  into a string (not a key)

itemSalesSource param = select $ itemSalesQuery  "M%" "" param
-- TODO add stockFilter
-- TODO add dates
--
itemSalesQuery :: Text -> Text -> ReportParam -> SqlQuery (EntityX DebtorTran :& EntityX DebtorTransDetail :& EntityX StockMove)
itemSalesQuery stockLike _defaultLocation  param =  do
  -- let (stockJoin, stockWhere, stockParams) = stockFilterToSql  stockFilter
  (trans :& detail :&move ) <- from $ ( debtorTransAndDetailsTable
                              `innerJoin` (table @StockMove `on` (\(trans :& detail :& move)
                                                 -> detail.stockId ==. move.stockId
                                                 &&. detail.debtorTransNo ==. just move.transNo
                                                 &&. detail.debtorTransType ==. just (move ^. #type)
                                                 &&. trans.tranDate ==. move.tranDate
                                                 )
                             )
                             )
                             `innerJoinIf` ( if rpShowInactive  param
                                             then Just $ table @StockMaster `on` \((getTable @DebtorTransDetail-> detail) :& stock ) -> castString (stock ^. StockMasterId) ==. detail.stockId &&. not_ stock.inactive
                                             else Nothing
                                           ) 
  where_ (trans ^. #type `in_` valList (map fromEnum [ ST_CUSTDELIVERY, ST_CUSTCREDIT]  ) )
  where_ ((detail.qtyDone !=. val 0) &&. (detail.stockId `like` val stockLike ))
  where_ $ foldr (||.) (val False)
                 do -- List 
                    let tdate = trans.tranDate
                    interval <- paramToDateIntervals param 
                    pure $ case interval of
                       (Just start, Just end) -> tdate `between` (val start, val end)
                       (Nothing, Just end) -> tdate <=. val end
                       (Just start, Nothing) -> tdate >=. val start
                       (Nothing, Nothing) -> val True -- should not happen though
  
  pure (trans :& detail :&move)

  
innerJoinIf :: ToFrom b b' => From a -> Maybe (b, (a :& b') -> SqlExpr (Value Bool)) -> From a
t `innerJoinIf` (Just tableOnJoin) = From do
     (a :& _, fn) <- unFrom $ t `innerJoin` tableOnJoin
     return $ (a, fn)
t `innerJoinIf` Nothing = t
                
debtorTransAndDetailsTable :: From (SqlExpr (Entity DebtorTran) :& SqlExpr (Entity DebtorTransDetail))
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
                           
                    
    

paramToDateIntervals :: ReportParam -> [(Maybe Day, Maybe Day)]
paramToDateIntervals param =
  let pM = (,) <$> rpPeriod param <*> rpNumberOfPeriods param
  in generateDateIntervals (rpFrom param)
                           (rpTo param)
                           pM

