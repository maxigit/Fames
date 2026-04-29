{-# LANGUAGE OverloadedLabels, OverloadedRecordDot, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
module Handler.Items.Reports.Sources
where 

import Import hiding(on, (==.), (!=.),(<=.),(>=.),(||.), selectSource, Value, exists)
-- import qualified Database.Persist as P
import Database.Esqueleto.Experimental
import Database.Esqueleto.Experimental.From(ToFrom)
-- import Handler.Util
import FA
import Handler.Items.Reports.Types
import Handler.Items.Common
import GL.Utils(generateDateIntervals)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Conduit.List(groupOn)
import Util.ForConduit

type EntityX a = SqlExpr (Entity a)

instance SqlString StockMasterId -- needed to convert stock_master.stock_id  into a string (not a key)

itemSalesQuery :: Text -> Text -> ReportParam -> SqlQuery (EntityX DebtorTran :& EntityX DebtorTransDetail :& EntityX StockMove)
itemSalesQuery stockLike _defaultLocation  param =  do
  let stockFilter = rpStockFilter param
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
                                             then Nothing -- show all
                                             else Just $ table @StockMaster `on` \((getTable @DebtorTransDetail-> detail) :& stock ) -> castString (stock ^. StockMasterId) ==. detail.stockId &&. not_ stock.inactive
                                             -- ^^^ only active
                                           ) 
                             `innerJoinIf` (flip fmap (sfCategory stockFilter)
                                                 $ \(catname, fexpr) ->
                                                      (table @ItemCategory)
                                                      -- `on` \((getTable @DebtorTransDetail -> detail) :& category ) 
                                                      `on` \(_trans :& detail :& _move :& category ) 
                                                           -> category.category ==. val catname
                                                              &&. category.stockId ==. detail.stockId
                                                              &&. category.value =%/. fexpr
                                           )
  where_ (trans ^. #type `in_` valList (map fromEnum [ ST_CUSTDELIVERY, ST_CUSTCREDIT]  ) )
  where_ ((detail.qtyDone !=. val 0) &&. (detail.stockId `like` val stockLike ))
  forM (sfSku stockFilter) \sku -> where_ (detail.stockId =%/. sku)
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
innerJoinIf :: ToFrom b b' => From a -> Maybe (b, (a :& b') -> SqlExpr (Value Bool)) -> From a
t `innerJoinIf` (Just tableOnJoin) = From do
     (a :& _, fn) <- unFrom $ t `innerJoin` tableOnJoin
     return $ (a, fn)
t `innerJoinIf` Nothing = t
                
----------------------------------------------------------------
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
                           
                    
    
----------------------------------------------------------------

paramToDateIntervals :: ReportParam -> [(Maybe Day, Maybe Day)]
paramToDateIntervals param =
  let pM = (,) <$> rpPeriod param <*> rpNumberOfPeriods param
  in generateDateIntervals (rpFrom param)
                           (rpTo param)
                           pM

