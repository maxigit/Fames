{-# LANGUAGE OverloadedLabels, OverloadedRecordDot, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
module Handler.Items.Reports.Sources
where 

import Import hiding(on, (==.), (!=.),(<=.),(>=.),(>.),(||.), (-.), selectSource, Value, exists)
-- import qualified Database.Persist as P
import Database.Esqueleto.Experimental
import Database.Esqueleto.Experimental.From(ToFrom)
-- import Handler.Util
import FA
import Handler.Items.Reports.Types
import Handler.Items.Sources
import GL.Utils(generateDateIntervals)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Conduit.List(groupOn)
import Util.ForConduit


itemSalesQuery :: Text -> ReportParam -> SqlQuery (EntityX DebtorTran :& EntityX DebtorTransDetail :& EntityX StockMove)
itemSalesQuery stockLike param =  do
  let stockFilter = rpStockFilter param
  (trans :& detail :&move ) <- itemSalesTables stockLike stockFilter (rpShowInactive param)
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

-- salesDetailPrice param tables = 
--     let detail = getTable @DebtorTransDetail tables
--     in ( if rpDeduceTax param
--        then detail.unitPrice -. detail.unitTax
--        else detail.unitPrice
--        )
--        *. (val 1 -. detail.discountPercent) -- don't divide discountPercent per 100, is not a percent but the real factor :-(
--   
-- 
-- salesDetailQuantity tables =
--     let trans = getTable @DebtorTran tables
--         detail = getTable @DebtorTransDetail tables
--     in case_ [ (trans ^. #type ==. val (fromEnum ST_CUSTCREDIT)
--                , detail.quantity )
--              ]
--              detail.qtyDone
-- salesDetailAmount param tables = salesDetailPrice param tables *. salesDetailQuantity tables

orderCategorySourceFor :: ToFrom a a' => a -> (SqlExpr (Value Int) -> a' -> SqlExpr (Value Bool)) ->  SqlConduit () (ForMap Int (Map Text Text)) ()
orderCategorySourceFor query cond =  do
   let catQuery  =  do
                      category <- from (table @OrderCategory)
                      where_ $ exists do
                                e <- from query
                                where_ $ cond category.orderId e
                      orderBy [ asc category.orderId , asc category.category]
                      return category
   selectSource catQuery .| mapC entityVal 
                         .| groupOn (orderCategoryOrderId)
                         .| mapC \(cat :| cats)  -> ForMap  (orderCategoryOrderId cat)
                                                            ( mapFromList [ ( orderCategoryCategory c , orderCategoryValue c)
                                                                          | c <- cat : cats
                                                                          ]
                                                            )
                                                                               
----------------------------------------------------------------

paramToDateIntervals :: ReportParam -> [(Maybe Day, Maybe Day)]
paramToDateIntervals param =
  let pM = (,) <$> rpPeriod param <*> rpNumberOfPeriods param
  in generateDateIntervals (rpFrom param)
                           (rpTo param)
                           pM

