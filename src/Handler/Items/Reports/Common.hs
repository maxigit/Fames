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
  , colFn :: ReportParam -> TranKey -> Maybe Text
  } 

instance Eq Column where
  a == b = colName a == colName b

-- Heterogenous type
data ColumnValue = ColumnValue
  { cvHtml :: Html
  , cvText :: Text
    
  }

tkType' tk = case tkType tk of
  ST_SALESINVOICE -> Just "Invoice"
  ST_SUPPINVOICE -> Just "Invoice"
  ST_CUSTCREDIT -> Just "Credit"
  ST_SUPPCREDIT -> Just "Credit"
  _ -> Nothing
getCols :: Handler [Column]
getCols = do
  today <- utctDay <$> liftIO getCurrentTime
  categories <- categoriesH

  return $ [ Column "Style" (const tkStyle)
           , Column "Variation" (const tkVar)
           , Column "Sku" (const $ Just . tkSku)
           , Column "Date" (const $ Just . tshow . tkDay)
           , Column "52W" (\p -> let day0 = addDays 1 $ fromMaybe today (rpTo p)
                                 in Just . pack . slidingYearShow day0 . tkDay
                          )
           , Column "Customer" (const tkCustomer)
           , Column "Supplier" (const tkSupplier)
           , Column "TransactionType" (const $ Just . tshow . tkType)
           , Column "Invoice/Credit" (const tkType')
           ] <>
           [ Column name (const $ Just . pack . formatTime defaultTimeLocale format . tkDay)
           | (name, format) <- [ ("Year", "%Y")
                               , ("Year-Month", "%Y-M%m")
                               , ("Month", "%M%m %B")
                               , ("Year-Week", "%Y-%m-W%W")
                               , ("Week", "W%W")
                               ]
             
           ] <>
           [ Column ("Category:" <> cat) (\_ tk -> Map.lookup cat (tkCategory tk))
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
          "AND quantity != 0" :
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
          "AND quantity != 0" :
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
  key = TranKey stockMoveTranDate customer supplier  stockMoveStockId (Just style) (Just var) (mapFromList categorieValues) (toEnum stockMoveType)
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
                transType
  (tqp, transType) = case toEnum <$> debtorTransDetailDebtorTransType of
    Just ST_SALESINVOICE -> (TranQP (Just qp) Nothing Nothing, ST_SALESINVOICE)
    Just ST_CUSTCREDIT -> (TranQP (Just qpNeg) Nothing Nothing, ST_CUSTCREDIT)
    else_ -> error $ "Shouldn't process transaction of type " <> show else_
  qp = qprice debtorTransDetailQuantity price
  qpNeg = qprice (-debtorTransDetailQuantity) price
  price = debtorTransDetailUnitPrice*(1-debtorTransDetailDiscountPercent/100)

purchToTransInfo ( Entity _ FA.SuppInvoiceItem{..}
                  , Entity _ FA.SuppTran{..}
                  , Entity _ FA.Supplier{..}) = (key, tqp) where
  key = TranKey suppTranTranDate Nothing (Just $ decodeHtmlEntities supplierSuppName)
                suppInvoiceItemStockId Nothing Nothing  (mempty)
                (toEnum suppTranType)
                   
  tqp = case toEnum suppTranType of
    ST_SUPPINVOICE -> TranQP Nothing (Just qp) Nothing
    ST_SUPPCREDIT -> TranQP Nothing (Just qpNeg) Nothing
    else_ -> error $ "Shouldn't process transaction of type " <> show else_
  qp = qprice suppInvoiceItemQuantity price
  qpNeg = qprice (-suppInvoiceItemQuantity) price
  price = suppInvoiceItemUnitPrice*suppTranRate

-- * Reports
-- | Display sales and purchase of an item
itemReport
  :: ReportParam
     -> Column
     -> Column
     -> (Map (Maybe Text) (Map (Maybe Text) TranQP) -> a )
     -> Handler a
itemReport param rowGrouper colGrouper processor = do
  trans <- loadItemTransactions param
  -- let grouped = Map.fromListWith(<>) [(grouper k, qp) | (k,qp) <- trans]
  let grouped = groupAsMap (colFn rowGrouper param . fst) (:[]) trans
      grouped' = groupAsMap (colFn colGrouper param . fst) snd <$> grouped
  return $ processor grouped'


tableProcessor :: Map (Maybe Text) (Map (Maybe Text) TranQP) -> Widget 
tableProcessor grouped = [whamlet|
    $forall (h1, group) <- Map.toList grouped
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
              <th> Summary Qty
              <th> Summary Amount
              <th> Summary Min Price
              <th> Summary max Price
            $forall (h2,qp) <- Map.toList group
              <tr>
                <td> #{fromMaybe "" h2}
                ^{showQp $ salesQPrice qp}
                ^{showQp $ purchQPrice qp}
                ^{showQp $ salesQPrice qp <> (negQP <$> purchQPrice qp)}
            $with (qp) <- summarize group
                <tr.total>
                  <td> Total
                  ^{showQp $ salesQPrice qp}
                  ^{showQp $ purchQPrice qp}
                  ^{showQp $ salesQPrice qp <> (negQP <$> purchQPrice qp)}
                  |]
  where
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


summarize group = sconcat (q :| qp) where  (q:qp) = toList group
negQP  QPrice{..} = QPrice (- qpQty) (-qpAmount) (MinMax (-mn) (-mx)) where MinMax mn mx = qpPrice
-- *** Csv
qpToCsv Nothing = ["", "", "", ""]
qpToCsv (Just QPrice{..}) = [ tshow qpQty
                            , tshow qpAmount
                            , tshow qpMin
                            , tshow qpMax
                            ] where (MinMax qpMin qpMax ) = qpPrice
toCsv param grouped' = let
  header = intercalate "," [ colName $ rpRowRupture param
                          , colName $ rpColumnRupture param
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
                      <> (qpToCsv $ salesQPrice qp)
                      <> (qpToCsv $ purchQPrice qp)
                      <> (qpToCsv $ adjQPrice qp)
-- ** Plot
chartProcessor :: Map (Maybe Text) (Map (Maybe Text) TranQP) -> Widget 
chartProcessor grouped = do
     let plotId = "items-report-plot" :: Text
         xsFor g = map (toJSON .fst) (Map.toList g)
         ysFor g = map (toJSON . fmap qpAmount . salesQPrice . snd)  (Map.toList g)
         traceFor (name, g) = Map.fromList $ [ ("x" :: Text, toJSON $ xsFor g)
                                     , ("y",  toJSON $ ysFor g)
                                     , ("name", toJSON name )
                                     , ("connectgaps", toJSON False )
                                     ]
         jsData = traceShowId $ map traceFor (Map.toList grouped)
     [whamlet|<div id=#{plotId}>
             |]
     addScriptRemote "https://cdn.plot.ly/plotly-latest.min.js"
     toWidget [julius|
          Plotly.plot( #{toJSON plotId}
                    , #{toJSON jsData} 
                    , { margin: { t: 0 }
                      , barmode: 'stack'
                      }
                    );
                |]
  
-- ** Csv
  
itemToCsv param = do
  -- no need to group, we display everything, including all category and columns
  cols <- getCols
  categories <- categoriesH
  trans <- loadItemTransactions param
  let qpCols = [  "Sales Qty"
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
      extraCols = map colName cols
      header = intercalate "," $ extraCols <> qpCols <> categories

      csvLines = header : do  -- []
        (key, qp) <- trans
        return $ intercalate "," (
          [tshowM $ colFn col param key | col <- cols]
            <> (qpToCsv $ salesQPrice qp)
            <> (qpToCsv $ purchQPrice qp)
            <> (qpToCsv $ adjQPrice qp)
            <> (map (tshowM . flip Map.lookup (tkCategory key)) categories)
                                 )
      source = yieldMany (map (<> "\n") csvLines)
  setAttachment  "items-report.csv"
  respondSource "text/csv" (source =$= mapC toFlushBuilder)
                 




  
  
-- *** Plot

-- ** Utils
-- splitToGroups :: (a -> k) -> (a -> a') ->   [(a,b)] -> [(k, (a',b))]
groupAsMap :: (Semigroup a, Ord k) => (t -> k) -> (t -> a) -> [t] -> Map k a
groupAsMap key f xs = Map.fromListWith (<>) [(key x, f x ) | x <- xs]
