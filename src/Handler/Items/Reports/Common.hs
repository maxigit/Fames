module Handler.Items.Reports.Common where

import Import
import Handler.Table
import Items.Types
import Handler.Items.Common
import Handler.Util
import FA
import Data.Time(addDays)
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty(..))
import Data.List(cycle)
import Database.Persist.MySQL(unSqlBackendKey, rawSql, Single(..))
import Data.Aeson.QQ(aesonQQ)

-- * Param
data ReportParam = ReportParam
  { rpFrom :: Maybe Day
  , rpTo :: Maybe Day
  -- , rpCatFilter :: Map Text (Maybe Text)
  , rpStockFilter :: Maybe FilterExpression
  , rpPanelRupture :: Maybe Column
  , rpBand :: Maybe Column
  , rpSerie :: Maybe Column
  , rpColumnRupture :: Column
  , rpTraceParam :: TraceParams
  , rpTraceParam2 :: TraceParams
  , rpTraceParam3 :: TraceParams
  }  --deriving Show
paramToCriteria :: ReportParam -> [Filter FA.StockMove]
paramToCriteria ReportParam{..} = (rpFrom <&> (FA.StockMoveTranDate >=.)) ?:
                                  (rpTo <&> (FA.StockMoveTranDate <=.)) ?:
                                  (filterE id FA.StockMoveStockId  rpStockFilter)
 
-- | Trace parameter for plotting 
data TraceParams = TraceParams
  { tpDataType :: QPType
  , tpDataParams :: Identifiable [TraceParam]
  } 
newtype TraceParam = TraceParam ((QPrice -> Double), Text {- Color-} -> [(Text, Value)])
-- ** Default  style
amountStyle color = [("type", String "scatter")
                    ,("line", [aesonQQ|{
                               color: #{color}
                                }|])
                    ]
quantityStyle color = [("type", String "scatter")
                ,("line", [aesonQQ|{
                               shape:"hvh",
                               color: #{color}
                                }|])
                , ("marker", [aesonQQ|{symbol: "square-open"}|])
                , ("yaxis", "y2")
              ]
priceStyle color = [("type", String "scatter")
                , ("marker", [aesonQQ|{symbol: "diamond"}|])
                , ("yaxis", "y3")
                , ("line", [aesonQQ|{dash:"dash", color:#{color}}|])
              ]
pricesStyle = [(qpMinPrice , const [ ("style", String "scatter")
                             , ("fill", String "tonexty")
                             , ("fillcolor", String "transparent")
                             , ("mode", String "markers")
                             , ("connectgaps", toJSON True )
                             , ("showlegend", toJSON False )
                             ])
               ,(qpAveragePrice , \color -> [ ("style", String "scatter")
                             , ("fill", String "tonexty")
                             , ("connectgaps", toJSON True )
                             , ("line", [aesonQQ|{color:#{color}}|])
                             ])
               ,(qpMaxPrice , \color -> [ ("style", String "scatter")
                             , ("fill", String "tonexty")
                             , ("connectgaps", toJSON True )
                             -- , ("mode", String "markers")
                             , ("line", [aesonQQ|{color:#{color}}|])
                             -- , ("line", [aesonQQ|{width:0}|])
                             ])
                 ]
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
tkType'' tk = case tkType tk of
  ST_SALESINVOICE -> Just "Sales"
  ST_SUPPINVOICE -> Just "Purchase"
  ST_CUSTCREDIT -> Just "Sales"
  ST_SUPPCREDIT -> Just "Purchase"
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
           , Column "Sales/Purchase" (const tkType'')
           , Column "Invoice/Credit" (const tkType')
           ] <>
           [ Column name (const $ Just . pack . formatTime defaultTimeLocale format . tkDay)
           | (name, format) <- [ ("Year", "%Y")
                               , ("Year-Month", "%Y-M%m")
                               , ("Year-1-month", "%Y-%m-01")
                               , ("Month", "%M%m %B")
                               , ("1-Month", "2001-%m-01")
                               , ("1-Day", "2001-%m-%d")
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
  return $ map detailToTransInfo sales

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
  return $ map purchToTransInfo purch

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
  (customer, supplier, tqp) =
    -- case toEnum stockMoveType of
    -- ST_CUSTDELIVERY -> ( tshow <$> stockMovePersonId
    --                    , Nothing
    --                    , Just $ singletonMap  QPSalesInvoice (Just $ qpNeg Outward) Nothing Nothing
    --                    )
    -- ST_CUSTCREDIT -> ( tshow <$> stockMovePersonId
    --                  , Nothing
    --                  , Just $ TranQP (Just $ qpNeg Outward) Nothing Nothing
    --                  )
    -- ST_SUPPRECEIVE -> ( Nothing
    --                   , tshow <$> stockMovePersonId
    --                   , Just $ TranQP Nothing (Just $ qp Inward) Nothing
    --                   )
    -- ST_SUPPCREDIT -> ( Nothing
    --                  , tshow <$> stockMovePersonId
    --                  , Just $ TranQP Nothing (Just $ qp Inward) Nothing
    --                  )
    -- ST_INVADJUST -> ( Nothing
    --                 , Nothing
    --                 , Just $ TranQP Nothing Nothing (Just $ qp Inward)
    --                 )
    -- _ ->
    (Nothing, Nothing, Nothing)
  -- qp io = mkQPrice io stockMoveQty price
  -- qpNeg io = mkQPrice io (-stockMoveQty) price
  -- price = stockMovePrice*(1-stockMoveDiscountPercent/100)
  
-- ** Sales Details
-- detailToTransInfo :: [Text] -> (Text -> Text -> Maybe Text) -> StockMove -> (TranKey, Maybe TranQP)
detailToTransInfo ( Entity _ FA.DebtorTransDetail{..}
                  , Entity _ FA.DebtorTran{..}
                  , Entity _ FA.CustBranch{..}) = (key, tqp) where
  key = TranKey debtorTranTranDate (Just $ decodeHtmlEntities custBranchBrName) Nothing
                debtorTransDetailStockId Nothing Nothing  (mempty)
                transType
  (tqp, transType) = case toEnum <$> debtorTransDetailDebtorTransType of
    Just ST_SALESINVOICE -> (tranQP QPSalesInvoice  (qp Outward), ST_SALESINVOICE)
    Just ST_CUSTCREDIT -> (tranQP QPSalesCredit (qp Inward), ST_CUSTCREDIT)
    else_ -> error $ "Shouldn't process transaction of type " <> show else_
  qp io = mkQPrice io debtorTransDetailQuantity price
  price = debtorTransDetailUnitPrice*(1-debtorTransDetailDiscountPercent/100)

purchToTransInfo ( Entity _ FA.SuppInvoiceItem{..}
                  , Entity _ FA.SuppTran{..}
                  , Entity _ FA.Supplier{..}) = (key, tqp) where
  key = TranKey suppTranTranDate Nothing (Just $ decodeHtmlEntities supplierSuppName)
                suppInvoiceItemStockId Nothing Nothing  (mempty)
                (toEnum suppTranType)
                   
  tqp = case toEnum suppTranType of
    ST_SUPPINVOICE -> tranQP QPPurchInvoice (qp Inward)
    ST_SUPPCREDIT -> tranQP QPPurchCredit (qp Outward)
    else_ -> error $ "Shouldn't process transaction of type " <> show else_
  qp io = mkQPrice io suppInvoiceItemQuantity price
  price = suppInvoiceItemUnitPrice*suppTranRate

-- * Reports
-- | Display sales and purchase of an item
itemReport
  :: ReportParam
     -> Maybe Column
     -> Maybe Column
     -> (Map (Maybe Text) (Map (Maybe Text) [(TranKey, TranQP)]) -> a )
     -> Handler a
itemReport param panelGrouperM colGrouper processor = do
  trans <- loadItemTransactions param
  let grouped = groupAsMap (mkGrouper param panelGrouperM . fst) (:[]) trans
      grouped' = groupAsMap (mkGrouper param colGrouper. fst) (:[]) <$> grouped
  return $ processor grouped'


mkGrouper param = maybe (const Nothing) (flip colFn param)

tableProcessor :: Map (Maybe Text) (Map (Maybe Text) [(TranKey, TranQP)]) -> Widget 
tableProcessor grouped = 
  [whamlet|
    $forall (h1, group) <- Map.toList grouped
        <div.panel.panel-info>
          <div.panel-heading>
            <h2>#{fromMaybe "<All>" h1}
          <div.panel-body>
            <table.table.table-hover.table-striped>
              <tr>
                <th>
                <th> Sales Qty
                <th> Sales Amount
                <th> Sales Min Price
                <th> Sales max Price
                <th> Sales Average Price
                <th> Purch Qty
                <th> Purch Amount
                <th> Purch Min Price
                <th> Purch Max Price
                <th> Purch Average Price
                <th> Summary Qty
                <th> Summary Amount
                <th> Summary Min Price
                <th> Summary max Price
                <th> Summary Average Price
              $forall (h2,qps) <- Map.toList group
                 $with qp <- summarize (map snd qps)
                    <tr>
                      <td> #{fromMaybe "" h2}
                      ^{showQp Outward $ salesQPrice qp}
                      ^{showQp Inward $ purchQPrice qp}
                      ^{showQp Outward $ mconcat [salesQPrice qp , purchQPrice qp]}
              $with (qp) <- summarize (fmap (summarize . map snd) group)
                  <tr.total>
                    <td> Total
                    ^{showQp Outward $ salesQPrice qp}
                    ^{showQp Inward $ purchQPrice qp}
                    ^{showQp Outward $ mconcat [salesQPrice qp , purchQPrice qp]}
                    |]
  where
      showQp _ Nothing = [whamlet|
                                  <td>
                                  <td>
                                  <td>
                                  <td>
                                  <td>
                                  |]
      showQp io (Just qp) = [whamlet|
                                  <td> #{show $ round (qpQty io qp)}
                                  <td> #{showDouble (qpAmount io qp)}
                                  <td> #{showDouble (qpMinPrice qp) }
                                  <td> #{showDouble (qpMaxPrice qp) }
                                  <td> #{showDouble (qpAveragePrice qp)}
                                  |]


summarize group = sconcat (q :| qp) where  (q:qp) = toList group
-- *** Csv
qpToCsv io Nothing = ["", "", "", ""]
qpToCsv io (Just qp) = [ tshow (qpQty io qp)
                       , tshow (qpAmount io qp)
                       , tshow (qpMinPrice qp)
                       , tshow (qpMaxPrice qp)
                       ]
toCsv param grouped' = let
  header = intercalate "," [ tshowM $ colName <$> rpPanelRupture param
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
                      <> (qpToCsv Outward $ salesQPrice qp)
                      <> (qpToCsv Inward $ purchQPrice qp)
                      <> (qpToCsv Inward $ adjQPrice qp)
-- ** Plot
chartProcessor :: ReportParam -> Map (Maybe Text) (Map (Maybe Text) [(TranKey, TranQP)]) -> Widget 
chartProcessor param grouped = do
  -- addScriptRemote "https://cdn.plot.ly/plotly-latest.min.js"
  -- done add report level to fix ajax issue.
  forM_ (zip (Map.toList grouped) [1 :: Int ..]) $ \((panelName, group), i) -> do
     let plotId = "items-report-plot-" <> tshow i 
         bySerie = fmap (groupAsMap (mkGrouper param (rpSerie param) . fst) (:[])) group
     panelChartProcessor param (fromMaybe "All" panelName) plotId bySerie
        
  
panelChartProcessor :: ReportParam -> Text -> Text -> Map (Maybe Text) (Map (Maybe Text) [(TranKey, TranQP)]) -> Widget 
panelChartProcessor param name plotId0 grouped = do
  let plots = forM_ (zip (Map.toList grouped) [1:: Int ..]) $ \((bandName, bands), i) ->
        do
          let byColumn = fmap (groupAsMap (mkGrouper param (Just $ rpColumnRupture param) . fst) snd) bands
              traceParams = [(qtype, tparam )
                            | (TraceParams qtype tparams ) <- [rpTraceParam,  rpTraceParam2 , rpTraceParam3]
                                                               <*> [param]
                            , tparam <- getIdentified tparams
                            ]
              plot = seriesChartProcessor traceParams (fromMaybe "<All>" bandName) plotId byColumn 
              plotId = plotId0 <> "-" <> tshow i
          [whamlet|
            <div id=#{plotId} style="height:#{tshow plotHeight }px">
                ^{plot}
                  |]
      numberOfBands = length grouped
      plotHeight = max 200 (800 `div` numberOfBands)
  [whamlet|
      <div.panel.panel-info>
        <div.panel-heading>
          <h2>#{name}
        <div.panel-body>
          ^{plots}
            |]

    
defaultColors :: [Text]
defaultColors = defaultPlottly where
  defaultPlottly  = ["#1f77b4",  -- muted blue
              "#ff7f0e",  -- safety orange
              "#2ca02c",  -- cooked asparagus green
              "#d62728",  -- brick red
              "#9467bd",  -- muted purple
              "#8c564b",  -- chestnut brown
              "#e377c2",  -- raspberry yogurt pink
              "#7f7f7f",  -- middle gray
              "#bcbd22",  -- curry yellow-green
              "#17becf"   -- blue-teal
             ]

seriesChartProcessor ::[(QPType, TraceParam)]-> Text -> Text -> Map (Maybe Text) (Map (Maybe Text) TranQP) -> Widget 
seriesChartProcessor params name plotId grouped = do
     let xsFor g = map (toJSON . fromMaybe "ALL" . fst) g
         ysFor f g = map (toJSON . f . snd) g
         traceFor param ((name, g'), color) = Map.fromList $ [ ("x" :: Text, toJSON $ xsFor g) 
                                                    , ("y",  toJSON $ ysFor fn g)
                                                    , ("name", toJSON name )
                                                    , ("connectgaps", toJSON False )
                                                    , ("type", "scatter" ) 
                                                    ]
                                                    -- <> maybe [] (\color -> [("color", String color)]) colorM
                                                    <> options color
                                                       where g = sortOn fst (Map.toList g')
                                                             (qtype, TraceParam (valueFn, options)) = param
                                                             fn = fmap valueFn . lookupGrouped qtype
                       
                                                                    
         jsData = map traceFor params <*> (zip (Map.toList grouped) (cycle defaultColors))
     toWidget [julius|
          Plotly.plot( #{toJSON plotId}
                    , #{toJSON jsData} 
                    , { margin: { t: 0 }
                      , title: #{toJSON name}
                      , yaxis2 : {overlaying: 'y', title: "Quantities"}
                      , yaxis3 : {overlaying: 'y', title: "Price"}
                      , updatemenus:
                         [ { buttons: [ { method: 'restyle'
                                        , args: [{type: 'scatter', fill: 'none' }]
                                        , label: "Line"
                                        }
                                      , { method: 'restyle'
                                        , args: [{type: 'bar' }]
                                        , label: "Bar"
                                        }
                                      , { method: 'restyle'
                                        , args: [{type: 'scatter', fill: 'tozeroy' }]
                                        , label: "Area"
                                        }
                                      , { method: 'relayout'
                                        , args: [{barmode: 'stack'}]
                                        , label: "Stack"
                                        }
                                      , { method: 'relayout'
                                        , args: [{barmode: null}]
                                        , label: "Untack"
                                        }
                                  ]
                           }
                         ]
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
            <> (qpToCsv Outward $ salesQPrice qp)
            <> (qpToCsv Inward $ purchQPrice qp)
            <> (qpToCsv Inward $ adjQPrice qp)
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

