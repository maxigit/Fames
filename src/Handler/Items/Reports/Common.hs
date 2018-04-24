module Handler.Items.Reports.Common where

import Import
import Handler.Table
import Items.Types
import Handler.Items.Common
import FA
import Data.Time(addDays)
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty(..))


-- * DB
loadItemTransactions :: Handler [(TranKey, TranQP)]
loadItemTransactions = do
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  catFinder <- categoryFinder
  today <- utctDay <$> liftIO getCurrentTime
  let to = today
      from = addDays (-3650) to
  moves <- runDB $ selectList ( [ FA.StockMoveTranDate >=. from
                                , FA.StockMoveTranDate <=. to
                                ]
                                <> filterE id FA.StockMoveStockId (Just . LikeFilter $ stockLike)
                              )
                              []
                               -- [LimitTo 1000]

  let info = mapMaybe (moveToTransInfo . entityVal) moves
  -- set category
  return [ (k { tkCategory = catFinder =<< tkStyle k }, qp) | (k, qp) <- info]

-- * Converter
moveToTransInfo :: StockMove -> Maybe (TranKey, TranQP)
moveToTransInfo FA.StockMove{..} = (key,) <$> tqp where
  (style, var) = skuToStyleVar stockMoveStockId
  key = TranKey stockMoveTranDate customer supplier (Just style) (Just var) Nothing
  (customer, supplier, tqp) = case toEnum stockMoveType of
    ST_CUSTDELIVERY -> ( stockMovePersonId
                       , Nothing
                       , Just $ TranQP (Just qpNeg) Nothing Nothing
                       )
    ST_CUSTCREDIT -> ( stockMovePersonId
                     , Nothing
                     , Just $ TranQP (Just qpNeg) Nothing Nothing
                     )
    ST_SUPPRECEIVE -> ( Nothing
                      , stockMovePersonId
                      , Just $ TranQP Nothing (Just qp) Nothing
                      )
    ST_SUPPCREDIT -> ( Nothing
                     , stockMovePersonId
                     , Just $ TranQP Nothing (Just qp) Nothing
                     )
    ST_INVADJUST -> ( Nothing
                    , Nothing
                    , Just $ TranQP Nothing Nothing (Just $ qprice stockMoveQty stockMoveStandardCost)
                    )
    _ -> (Nothing, Nothing, Nothing)
  qp = qprice stockMoveQty price
  qpNeg = qprice (-stockMoveQty) price
  price = stockMovePrice*(1-stockMoveDiscountPercent/100)
  

-- * Reports
-- Display sales and purchase of an item
itemReport rowGrouper colGrouper= do
  trans <- loadItemTransactions
  -- let grouped = Map.fromListWith(<>) [(grouper k, qp) | (k,qp) <- trans]
  let grouped = groupAsMap (rowGrouper . fst) (:[]) trans
      grouped' = groupAsMap (colGrouper . fst) snd <$> grouped
      summarize group = sconcat (q :| qp) where  (q:qp) = toList group
      profit qp = maybe 0 qpAmount (salesQPrice qp)
                  -  maybe 0 qpAmount (purchQPrice qp)
      showQp Nothing = [whamlet|
                                  <td>
                                  <td>
                                  <td>
                                  <td>
                                  |]
      showQp (Just QPrice{..}) = [whamlet|
                                  <td> #{show $ round qpQty}
                                  <td> #{showDouble qpAmount}
                                  <td> #{showDouble qpMin}
                                  <td> #{showDouble qpMax}
                                  |] where (MinMax qpMin qpMax ) = qpPrice
      widget =  [whamlet|
    $forall (h1, group) <- Map.toList grouped'
      <div.panel.panel-info>
        <div.panel-heading>
          <h2>#{fromMaybe "" h1}
        <div.panel-body>
          <table.table.table-hover.table-striped>
            <tr>
              <th>
              <th> Sales Qty
              <th> Sales Amount
              <th> Sales Min Price
              <th> Sales max Price
              <th> Purch Qty
              <th> Purch Amount
              <th> Purch Min Price
              <th> Purch max Price
              <th> Adjustment Qty
              <th> Adjustment Amount
              <th> Adjustment Min Price
              <th> Adjustment max Price
            $forall (h2,qp) <- Map.toList group
              <tr>
                <td> #{fromMaybe "" h2}
                ^{showQp $ salesQPrice qp}
                ^{showQp $ purchQPrice qp}
                ^{showQp $ adjQPrice qp}
            $with (qp) <- summarize group
                <tr.total>
                  <td> #{showDouble $ profit qp }
                  ^{showQp $ salesQPrice qp}
                  ^{showQp $ purchQPrice qp}
                  ^{showQp $ adjQPrice qp}
                  |]
  return (widget, grouped')


-- *** Csv
toCsv grouped' = let
  showQp Nothing = []
  showQp (Just QPrice{..}) = [ tshow qpQty
                              , tshow qpAmount
                              , tshow qpMin
                              , tshow qpMax
                              ] where (MinMax qpMin qpMax ) = qpPrice
  header = intercalate "," [ "Category"
                          , "Period"
                          ,  "Sales Qty"
                          ,  "Sales Amount"
                          ,  "Sales Min Price"
                          ,  "Sales max Price"
                          ,  "Purch Qty"
                          ,  "Purch Amount"
                          ,  "Purch Min Price"
                          ,  "Purch max Price"
                          ,  "Adjustment Qty"
                          ,  "Adjustment Amount"
                          ,  "Adjustment Min Price"
                          ,  "Adjustment max Price"
                          ]
  in header : do
    (h1, group) <- Map.toList grouped'
    (h2,qp) <- Map.toList group
    return $ intercalate "," $  [ tshowM h1
                      , tshowM h2
                      ]
                      <> (showQp $ salesQPrice qp)
                      <> (showQp $ purchQPrice qp)
                      <> (showQp $ adjQPrice qp)
  
-- ** Utils
-- splitToGroups :: (a -> k) -> (a -> a') ->   [(a,b)] -> [(k, (a',b))]
groupAsMap :: (Semigroup a, Ord k) => (t -> k) -> (t -> a) -> [t] -> Map k a
groupAsMap key f xs = Map.fromListWith (<>) [(key x, f x ) | x <- xs]
