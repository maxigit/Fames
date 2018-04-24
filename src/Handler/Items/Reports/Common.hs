module Handler.Items.Reports.Common where

import Import
import Handler.Table
import Items.Types
import Handler.Items.Common
import FA
import Data.Time(addDays)
import qualified Data.Map as Map


-- * DB
loadItemTransactions :: Handler [(TranKey, TranQP)]
loadItemTransactions = do
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  today <- utctDay <$> liftIO getCurrentTime
  let to = today
      from = addDays (-3650) to
  moves <- runDB $ selectList ( [ FA.StockMoveTranDate >=. from
                                , FA.StockMoveTranDate <=. to
                                ]
                                <> filterE id FA.StockMoveStockId (Just . LikeFilter $ stockLike)
                              )
                              [] -- LimitTo 100]
  return $ mapMaybe (moveToTransInfo . entityVal) moves

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
itemReport = do
  let grouper = tkStyle
  trans <- loadItemTransactions
  -- let grouped = Map.fromListWith(<>) [(grouper k, qp) | (k,qp) <- trans]
  let grouped = groupAsMap (grouper . fst) snd trans
      trans' = Map.toList grouped
  return [whamlet|
    $forall tran <- trans'
      <p>#{tshow tran}
                 |]

-- ** Utils
-- splitToGroups :: (a -> k) -> (a -> a') ->   [(a,b)] -> [(k, (a',b))]
groupAsMap :: (Semigroup a, Ord k) => (t -> k) -> (t -> a) -> [t] -> Map k a
groupAsMap key f xs = Map.fromListWith (<>) [(key x, f x ) | x <- xs]
