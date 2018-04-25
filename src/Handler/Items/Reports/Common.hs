module Handler.Items.Reports.Common where

import Import
import Handler.Table
import Items.Types
import Handler.Items.Common
import FA
import Data.Time(addDays)
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty(..))
import Database.Persist.MySQL(unSqlBackendKey, rawSql, Single(..))

-- * Param
data ReportParam = ReportParam
  { rpFrom :: Maybe Day
  , rpTo :: Maybe Day
  -- , rpCatFilter :: Map Text (Maybe Text)
  , rpStockFilter :: Maybe FilterExpression
  , rpRowRupture :: Column
  , rpColumnRupture :: Column
  }  --deriving Show
paramToCriteria :: ReportParam -> [Filter FA.StockMove]
paramToCriteria ReportParam{..} = (rpFrom <&> (FA.StockMoveTranDate >=.)) ?:
                                  (rpTo <&> (FA.StockMoveTranDate <=.)) ?:
                                  (filterE id FA.StockMoveStockId  rpStockFilter)

-- * Columns
data Column = Column
  { colName :: Text
  , colFn :: TranKey -> Maybe Text
  } 

instance Eq Column where
  a == b = colName a == colName b

-- Heterogenous type
data ColumnValue = ColumnValue
  { cvHtml :: Html
  , cvText :: Text
    
  }

getCols :: Handler [Column]
getCols = do
  today <- utctDay <$> liftIO getCurrentTime
  categories <- categoriesH

  return $ [ Column "Style" (tkStyle)
           , Column "Variation" tkVar
           , Column "52W" (Just . pack . slidingYearShow today . tkDay)
           , Column "Customer" tkCustomer
           , Column "Supplier" tkSupplier
           ] <>
           [ Column name (Just . pack . formatTime defaultTimeLocale format . tkDay)
           | (name, format) <- [ ("Year", "%Y")
                               , ("Year-Month", "%Y-M%m")
                               , ("Month", "%M%m %B")
                               , ("Year-Week", "%Y-%m-W%W")
                               , ("Week", "W%W")
                               ]
             
           ] <>
           [ Column ("Category:" <> cat) (\tk -> Map.lookup cat (tkCategory tk))
           | cat <- categories
           ]
           
  -- return $ map Column $ basic ++ ["category:" <>  cat | cat <- categories]

-- * DB
loadItemTransactions :: ReportParam -> Handler [(TranKey, TranQP)]
loadItemTransactions param = do
  categories <- categoriesH
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  catFinder <- categoryFinderCached
  -- moves <- runDB $ selectList ( criteria
  --                               <> filterE id FA.StockMoveStockId (Just . LikeFilter $ stockLike)
  --                             )
  --                             []

  sales <- loadItemSales param
  purchases <- loadItemPurchases param

  return $ map (computeCategory categories catFinder) (sales <> purchases)
  -- return $ mapMaybe (moveToTransInfo categories catFinder . entityVal) moves

computeCategory categories catFinder (key, tpq) = let
  sku = tkSku key
  cats = mapFromList [(cat, found) | cat <-  categories, Just found <- return $ catFinder cat sku ]
  (style, var) = skuToStyleVar sku
  in (key { tkCategory = mapFromList cats, tkStyle = Just style, tkVar = Just var}, tpq)

loadItemSales :: ReportParam -> Handler [(TranKey, TranQP)]
loadItemSales param = do
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  let sql = intercalate " " $
          "SELECT ??, ??, ?? FROM 0_debtor_trans_details " :
          "JOIN 0_debtor_trans ON (0_debtor_trans_details.debtor_trans_no = 0_debtor_trans.trans_no " :
          " AND 0_debtor_trans_details.debtor_trans_type = 0_debtor_trans.type)  " :
          "JOIN 0_cust_branch USING (debtor_no, branch_code)" :
          "WHERE type IN ("  :
          (tshow $ fromEnum ST_SALESINVOICE) :
          ",":
          (tshow $ fromEnum ST_CUSTCREDIT) :
          ") " :
          ("AND stock_id LIKE '" <> stockLike <> "'") : -- we don't want space between ' and stockLike
          -- " LIMIT 100" :
          []
      (w,p) = unzip $ rpFrom param <&> (\d -> (" AND tran_date >= ?", PersistDay d)) ?:
                       rpTo param <&> (\d -> (" AND tran_date <= ?", PersistDay d)) ?:
                       rpStockFilter param <&> (\e -> let (keyw, v) = filterEKeyword e
                                                      in (" AND stock_id " <> keyw <> " ?", PersistText v)
                                               ) ?:
                       []
        
  sales <- runDB $ rawSql (sql <> intercalate " "w) p
  return $ map (detailToTransInfo) sales

loadItemPurchases :: ReportParam -> Handler [(TranKey, TranQP)]
loadItemPurchases param = do
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  let sql = intercalate " " $
          "SELECT ??, ??, ?? FROM 0_supp_invoice_items " :
          "JOIN 0_supp_trans ON (0_supp_invoice_items.supp_trans_no = 0_supp_trans.trans_no " :
          " AND 0_supp_invoice_items.supp_trans_type = 0_supp_trans.type)  " :
          "JOIN 0_suppliers ON (0_supp_trans.supplier_id =  0_suppliers.supplier_id)" :
          "WHERE type IN ("  :
          (tshow $ fromEnum ST_SUPPINVOICE) :
          ",":
          (tshow $ fromEnum ST_SUPPCREDIT) :
          ") " :
          ("AND stock_id LIKE '" <> stockLike <> "'") : -- we don't want space between ' and stockLike
          -- " LIMIT 100" :
          []
      (w,p) = unzip $ rpFrom param <&> (\d -> (" AND tran_date >= ?", PersistDay d)) ?:
                       rpTo param <&> (\d -> (" AND tran_date <= ?", PersistDay d)) ?:
                       rpStockFilter param <&> (\e -> let (keyw, v) = filterEKeyword e
                                                      in (" AND stock_id " <> keyw <> " ?", PersistText v)
                                               ) ?:
                       []
  purch <- runDB $ rawSql (sql <> intercalate " " w) p
  return $ map (purchToTransInfo) purch

-- * Converter
-- ** StockMove
moveToTransInfo :: [Text] -> (Text -> Text -> Maybe Text) -> StockMove -> Maybe (TranKey, TranQP)
moveToTransInfo categories catFinder FA.StockMove{..} = (key,) <$> tqp where
  (style, var) = skuToStyleVar stockMoveStockId
  categorieValues = [(heading, cat)
                    | heading <- categories
                    , Just cat <- return $ catFinder heading stockMoveStockId
                    ]
  key = TranKey stockMoveTranDate customer supplier  stockMoveStockId (Just style) (Just var) (mapFromList categorieValues)
  (customer, supplier, tqp) = case toEnum stockMoveType of
    ST_CUSTDELIVERY -> ( tshow <$> stockMovePersonId
                       , Nothing
                       , Just $ TranQP (Just qpNeg) Nothing Nothing
                       )
    ST_CUSTCREDIT -> ( tshow <$> stockMovePersonId
                     , Nothing
                     , Just $ TranQP (Just qpNeg) Nothing Nothing
                     )
    ST_SUPPRECEIVE -> ( Nothing
                      , tshow <$> stockMovePersonId
                      , Just $ TranQP Nothing (Just qp) Nothing
                      )
    ST_SUPPCREDIT -> ( Nothing
                     , tshow <$> stockMovePersonId
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
  
-- ** Sales Details
-- detailToTransInfo :: [Text] -> (Text -> Text -> Maybe Text) -> StockMove -> Maybe (TranKey, TranQP)
detailToTransInfo ( Entity _ FA.DebtorTransDetail{..}
                  , Entity _ FA.DebtorTran{..}
                  , Entity _ FA.CustBranch{..}) = (key, tqp) where
  key = TranKey debtorTranTranDate (Just $ decodeHtmlEntities custBranchBrName) Nothing
                debtorTransDetailStockId Nothing Nothing  (mempty)
  tqp = case toEnum <$> debtorTransDetailDebtorTransType of
    Just ST_SALESINVOICE -> TranQP (Just qp) Nothing Nothing
    Just ST_CUSTCREDIT -> TranQP (Just qpNeg) Nothing Nothing
    else_ -> error $ "Shouldn't process transaction of type " <> show else_
  qp = qprice debtorTransDetailQuantity price
  qpNeg = qprice (-debtorTransDetailQuantity) price
  price = debtorTransDetailUnitPrice*(1-debtorTransDetailDiscountPercent/100)

purchToTransInfo ( Entity _ FA.SuppInvoiceItem{..}
                  , Entity _ FA.SuppTran{..}
                  , Entity _ FA.Supplier{..}) = (key, tqp) where
  key = TranKey suppTranTranDate Nothing (Just $ decodeHtmlEntities supplierSuppName)
                suppInvoiceItemStockId Nothing Nothing  (mempty)
  tqp = case toEnum suppTranType of
    ST_SUPPINVOICE -> TranQP Nothing (Just qp) Nothing
    ST_SUPPCREDIT -> TranQP Nothing (Just qpNeg) Nothing
    else_ -> error $ "Shouldn't process transaction of type " <> show else_
  qp = qprice suppInvoiceItemQuantity price
  qpNeg = qprice (-suppInvoiceItemQuantity) price
  price = suppInvoiceItemUnitPrice*suppTranRate

-- * Reports
-- Display sales and purchase of an item
itemReport
  :: ReportParam
     -> Column
     -> Column
     -> Handler (Widget, Map (Maybe Text) (Map (Maybe Text) TranQP))
itemReport param rowGrouper colGrouper= do
  trans <- loadItemTransactions param
  -- let grouped = Map.fromListWith(<>) [(grouper k, qp) | (k,qp) <- trans]
  let grouped = groupAsMap (colFn rowGrouper . fst) (:[]) trans
      grouped' = groupAsMap (colFn colGrouper . fst) snd <$> grouped
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
