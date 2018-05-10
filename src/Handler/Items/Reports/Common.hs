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
import Data.List(cycle,scanl1,scanr1)
import Database.Persist.MySQL(unSqlBackendKey, rawSql, Single(..))
import Data.Aeson.QQ(aesonQQ)

-- * Param
data ReportParam = ReportParam
  { rpFrom :: Maybe Day
  , rpTo :: Maybe Day
  -- , rpCatFilter :: Map Text (Maybe Text)
  , rpStockFilter :: Maybe FilterExpression
  , rpPanelRupture :: ColumnRupture
  , rpBand :: ColumnRupture
  , rpSerie :: ColumnRupture
  , rpColumnRupture :: Column
  , rpTraceParam :: TraceParams
  , rpTraceParam2 :: TraceParams
  , rpTraceParam3 :: TraceParams
  }  --deriving Show
paramToCriteria :: ReportParam -> [Filter FA.StockMove]
paramToCriteria ReportParam{..} = (rpFrom <&> (FA.StockMoveTranDate >=.)) ?:
                                  (rpTo <&> (FA.StockMoveTranDate <=.)) ?:
                                  (filterE id FA.StockMoveStockId  rpStockFilter)
 
-- TODO could it be merged with Column ?
data ColumnRupture = ColumnRupture
   { cpColumn :: Maybe Column
   , cpSortBy :: TraceParams
   , cpLimitTo :: Maybe Int
   }
-- | Trace parameter for plotting 
data TraceParams = TraceParams
  { tpDataType :: QPType
  , tpDataParams :: Identifiable [TraceParam]
  } 
newtype TraceParam = TraceParam ((QPrice -> Double), Text {- Color-} -> [(Text, Value)], RunSum )
data RunSum = RunSum | RunSumBack | RSNormal
-- ** Default  style
amountStyle color = [("type", String "scatter")
                    ,("name", String "Amount")
                    ,("line", [aesonQQ|{
                               color: #{color}
                                }|])
                    ]
-- cumulAmountStyle color = [("type", String "scatter")
--                     ,("name", String "Amount")
--                     ,("fill", "tonextx")
--                     ,("connectgaps", toJSON True)
--                     ,("line", [aesonQQ|{
--                                color: #{color}
--                                 }|])
cumulAmountStyle = amountStyle
quantityStyle color = [("type", String "scatter")
                      ,("name", String "Quantity")
                      ,("line", [aesonQQ|{
                               shape:"hvh",
                               color: #{color},
                               dash: "dash"
                                }|])
                , ("marker", [aesonQQ|{symbol: "square-open"}|])
                , ("yaxis", "y2")
                , ("showlegend", toJSON False)
              ]

quantityAmountStyle :: InOutward -> [(QPrice -> Amount, Text -> [(Text, Value)], RunSum)]
quantityAmountStyle io = [ (qpQty io, quantityStyle, RSNormal)
                         , (qpAmount io, \color -> [("type", String "scatter")
                                                   ,("name", String "Amount")
                                                   ,("line", [aesonQQ|{
                                                           color: #{color},
                                                           shape: "spline",
                                                           width: 1
                                                           }|])
                                         ], RSNormal)
                         ]
priceStyle color = [("type", String "scatter")
                , ("marker", [aesonQQ|{symbol: "diamond"}|])
                , ("yaxis", "y3")
                , ("name", "price")
                , ("line", [aesonQQ|{dash:"dash", color:#{color}}|])
              ]
pricesStyle :: [(QPrice -> Amount, Text -> [(Text, Value)], RunSum)]
pricesStyle = [(qpMinPrice , const [ ("style", String "scatter")
                             , ("fill", String "tonexty")
                             , ("fillcolor", String "transparent")
                             , ("mode", String "markers")
                             , ("connectgaps", toJSON True )
                             , ("showlegend", toJSON False )
                             ], RSNormal)
               ,(qpAveragePrice , \color -> [ ("style", String "scatter")
                             , ("fill", String "tonexty")
                             , ("connectgaps", toJSON True )
                             , ("line", [aesonQQ|{color:#{color}}|])
                             ], RSNormal)
               ,(qpMaxPrice , \color -> [ ("style", String "scatter")
                             , ("fill", String "tonexty")
                             , ("connectgaps", toJSON True )
                             -- , ("mode", String "markers")
                             , ("line", [aesonQQ|{color:#{color}}|])
                             -- , ("line", [aesonQQ|{width:0}|])
                             ], RSNormal)
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
  (cols, _) <- getColsWithDefault
  return cols
getColsWithDefault :: Handler ([Column], (Column, Column, Column))
getColsWithDefault = do
  today <- utctDay <$> liftIO getCurrentTime
  categories <- categoriesH

  let style = Column "Style" (const tkStyle)
      variation = Column "Variation" (const tkVar)
      w52 = Column "52W" (\p -> let day0 = addDays 1 $ fromMaybe today (rpTo p)
                                 in Just . pack . slidingYearShow day0 . tkDay
                          )
      defaultBand =  style
      defaultSerie = variation
      defaultTime = mkDateColumn monthly -- w52
      monthly =  ("Year-1-month", "%Y-%m-01")
      mkDateColumn (name, format) = Column name (const $ Just . pack . formatTime defaultTimeLocale format . tkDay)

      cols = [ style
            , variation
            , Column "Sku" (const $ Just . tkSku)
            , Column "Date" (const $ Just . tshow . tkDay)
            , w52
            , Column "Customer" (const tkCustomer)
            , Column "Supplier" (const tkSupplier)
            , Column "Supplier/Customer" (const (\t -> tkSupplier t <|> tkCustomer t))
            , Column "TransactionType" (const $ Just . tshow . tkType)
            , Column "Sales/Purchase" (const tkType'')
            , Column "Invoice/Credit" (const tkType')
            ] <>
            ( map mkDateColumn [ ("Year", "%Y")
                               , ("Year-Month", "%Y-M%m")
                               , monthly
                               , ("Month", "%M%m %B")
                               , ("1-Month", "2001-%m-01")
                               , ("1-Day", "2001-%m-%d")
                               , ("Year-Week", "%Y-%m-W%W")
                               , ("Week", "W%W")
                               ]
            ) <>
            [ Column ("Category:" <> cat) (\_ tk -> Map.lookup cat (tkCategory tk))
            | cat <- categories
            ]
  return (cols, (defaultBand, defaultSerie, defaultTime))
           
  -- return $ map Column $ basic ++ ["category:" <>  cat | cat <- categories]
-- * DB
loadItemTransactions :: ReportParam
                     -> ([(TranKey, TranQP)] -> QPGroup2)
                     -> Handler QPGroup2 
loadItemTransactions param grouper = do
  categories <- categoriesH
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  catFinder <- categoryFinderCached
  -- moves <- runDB $ selectList ( criteria
  --                               <> filterE id FA.StockMoveStockId (Just . LikeFilter $ stockLike)
  --                             )
  --                             []

  -- for efficiency reason
  -- it is better to group sales and purchase separately and then merge them
  sales <- loadItemSales param
  purchases <- loadItemPurchases param
  let salesGroups = grouper' sales
      purchaseGroups = grouper' purchases
      grouper' = squash . grouper . fmap (computeCategory categories catFinder) 
      -- we squash the third level already, so that we don't keep too many things in memory
      squash = fmap (fmap $ groupT ) :: QPGroup2 -> QPGroup2
      groupT ts = let
        g = groupAsMap fst (:[]) ts
        in Map.toList $ fmap (mconcat . map snd) g

        

  return $ salesGroups <> purchaseGroups

computeCategory :: [Text]
                -> (Text -> Text -> Maybe Text)
                -> (TranKey, t)
                -> (TranKey, t)
computeCategory categories catFinder (key, tpq) = let
  sku = tkSku key
  cats = mapFromList [(cat, found) | cat <-  categories, Just found <- return $ catFinder cat sku ]
  (style, var) = skuToStyleVar sku
  in (key { tkCategory = mapFromList cats, tkStyle = Just style, tkVar = Just var}, tpq)

loadItemSales :: ReportParam -> Handler [(TranKey, TranQP)]
loadItemSales param = do
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  let sql = intercalate " " $
          "SELECT ??, 0_debtor_trans.tran_date, 0_debtor_trans.debtor_no, 0_debtor_trans.branch_code" :
          " FROM 0_debtor_trans_details " :
          "JOIN 0_debtor_trans ON (0_debtor_trans_details.debtor_trans_no = 0_debtor_trans.trans_no " :
          " AND 0_debtor_trans_details.debtor_trans_type = 0_debtor_trans.type)  " :
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
          "SELECT ??, 0_supp_trans.tran_date, 0_supp_trans.rate, 0_supp_trans.supplier_id  FROM 0_supp_invoice_items " :
          "JOIN 0_supp_trans ON (0_supp_invoice_items.supp_trans_no = 0_supp_trans.trans_no " :
          " AND 0_supp_invoice_items.supp_trans_type = 0_supp_trans.type)  " :
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
detailToTransInfo :: (Entity FA.DebtorTransDetail, Single Day, Single (Maybe Int), Single Int) -> (TranKey, TranQP)
detailToTransInfo ( Entity _ FA.DebtorTransDetail{..}
                  , Single debtorTranTranDate
                  , Single debtorNo, Single branchCode)  = (key, tqp) where
  key = TranKey debtorTranTranDate
                (Just $ tshow debtorNo <> "-" <> tshow branchCode   )
                Nothing
                debtorTransDetailStockId Nothing Nothing  (mempty)
                transType
  (tqp, transType) = case toEnum <$> debtorTransDetailDebtorTransType of
    Just ST_SALESINVOICE -> (tranQP QPSalesInvoice  (qp Outward), ST_SALESINVOICE)
    Just ST_CUSTCREDIT -> (tranQP QPSalesCredit (qp Inward), ST_CUSTCREDIT)
    else_ -> error $ "Shouldn't process transaction of type " <> show else_
  qp io = mkQPrice io debtorTransDetailQuantity price
  price = debtorTransDetailUnitPrice*(1-debtorTransDetailDiscountPercent/100)

purchToTransInfo :: (Entity SuppInvoiceItem, Single Day, Single Double, Single Int)
                 -> (TranKey, TranQP)
purchToTransInfo ( Entity _ FA.SuppInvoiceItem{..}
                  , Single suppTranTranDate
                  , Single suppTranRate
                  , Single supplierId) = (key, tqp) where
  suppTranType = fromMaybe (error "supplier transaction should have a ty B") suppInvoiceItemSuppTransType
  key = TranKey suppTranTranDate Nothing (Just $ tshow supplierId)
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
     -> (QPGroup2 -> a )
     -> Handler a
itemReport param panelGrouperM colGrouper processor = do
  let grouper =  groupTranQPs param panelGrouperM colGrouper
  grouped <- loadItemTransactions param grouper
  return $ processor grouped

groupTranQPs :: ReportParam
             -> Maybe Column
             -> Maybe Column
             -> [(TranKey, TranQP)]
             -> QPGroup2
groupTranQPs param panelGrouperM colGrouper trans = let
  grouped = groupAsMap (mkGrouper param panelGrouperM . fst) (:[]) trans
  in QPGroup' $ fmap QPGroup' $ groupAsMap (mkGrouper param colGrouper. fst) (:[]) <$> grouped


mkGrouper param = maybe (const Nothing) (flip colFn param)

tableProcessor :: QPGroup2 -> Widget 
tableProcessor grouped = 
  [whamlet|
    $forall (h1, group) <- qpGroupToList grouped
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
              $forall (h2,qps) <- qpGroupToList group
                 $with qp <- summarize (map snd qps)
                    <tr>
                      <td> #{fromMaybe "" h2}
                      ^{showQp Outward $ salesQPrice qp}
                      ^{showQp Inward $ purchQPrice qp}
                      ^{showQp Outward $ mconcat [salesQPrice qp , purchQPrice qp]}
              $with (qpt) <- summarize (fmap (summarize . map snd) group)
                  <tr.total>
                    <td> Total
                    ^{showQp Outward $ salesQPrice qpt}
                    ^{showQp Inward $ purchQPrice qpt}
                    ^{showQp Outward $ mconcat [salesQPrice qpt , purchQPrice qpt]}
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
  header = intercalate "," [ tshowM $ colName <$> (cpColumn $ rpPanelRupture param)
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
    (h1, group) <- qpGroupToList grouped'
    (h2,qp) <- qpGroupToList group
    return $ intercalate "," $  [ tshowM h1
                      , tshowM h2
                      ]
                      <> (qpToCsv Outward $ salesQPrice qp)
                      <> (qpToCsv Inward $ purchQPrice qp)
                      <> (qpToCsv Inward $ adjQPrice qp)
-- ** Sort and limit
sortAndLimit :: (val -> TranQP) -> ColumnRupture -> Map key val -> [(key, val)]
sortAndLimit collapse ColumnRupture{..} grouped = let
  qtype = tpDataType cpSortBy
  asList = Map.toList grouped
  sorted = case getIdentified (tpDataParams cpSortBy) of
     [] -> asList
     (TraceParam (fn, _, _):_) -> let
                    sortFn (key, val) =  fn <$> lookupGrouped qtype (collapse val)
                    in sortOn sortFn asList
  in maybe id take cpLimitTo sorted

sortAndLimitGroup :: (val -> TranQP)
                  -> ColumnRupture
                  -> QPGroup' val
                  -> [(Maybe Text, val)]
sortAndLimitGroup  collapse  rupture (QPGroup' grouped) = sortAndLimit collapse rupture grouped
-- ** Plot
chartProcessor :: ReportParam -> QPGroup2 -> Widget 
chartProcessor param grouped = do
  -- addScriptRemote "https://cdn.plot.ly/plotly-latest.min.js"
  -- done add report level to fix ajax issue.
  let asList = sortAndLimitGroup (mconcat . map snd . concatMap toList) (rpPanelRupture param) grouped
  forM_ (zip asList [1 :: Int ..]) $ \((panelName, group), i) -> do
     let plotId = "items-report-plot-" <> tshow i 
         bySerie = fmap (groupAsMap (mkGrouper param (cpColumn $ rpSerie param) . fst) (:[])) group
     panelChartProcessor param (fromMaybe "All" panelName) plotId (fmap QPGroup' bySerie)
        
  
panelChartProcessor :: ReportParam -> Text -> Text -> QPGroup2 -> Widget 
panelChartProcessor param name plotId0 grouped = do
  let asList = sortAndLimitGroup (mconcat . map snd . concatMap toList) (rpBand param) grouped
  let plots = forM_ (zip asList [1:: Int ..]) $ \((bandName, bands), i) ->
        do
          let byColumn = fmap (groupAsMap (mkGrouper param (Just $ rpColumnRupture param) . fst) snd) (unQPGroup' bands)
              traceParams = [(qtype, tparam )
                            | (TraceParams qtype tparams ) <- [rpTraceParam,  rpTraceParam2 , rpTraceParam3]
                                                               <*> [param]
                            , tparam <- getIdentified tparams
                            ]
              plot = seriesChartProcessor (rpSerie param) (isNothing $ cpColumn $ rpSerie param) traceParams (fromMaybe "<All>" bandName) plotId byColumn 
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

seriesChartProcessor :: ColumnRupture -> Bool -> [(QPType, TraceParam)]-> Text -> Text -> Map (Maybe Text) (Map (Maybe Text) TranQP) -> Widget 
seriesChartProcessor rupture mono params name plotId grouped = do
     let xsFor g = map (toJSON . fromMaybe "ALL" . fst) g
         ysFor f g = map (toJSON . f . snd) g
         traceFor param ((name, g'), color,groupId) = Map.fromList $ [ ("x" :: Text, toJSON $ xsFor g) 
                                                    , ("y",  toJSON $ ysFor fn g)
                                                    -- , ("name", toJSON name )
                                                    , ("connectgaps", toJSON False )
                                                    , ("type", "scatter" ) 
                                                    , ("legendgroup", String (tshow groupId))
                                                    ]
                                                    -- <> maybe [] (\color -> [("color", String color)]) colorM
                                                    <> options color
                                                    <> (if fromMaybe "" name == "" then [] else [("name", toJSON name)])
                                                       where g'' = sortOn fst (Map.toList g')
                                                             g = case runsum of
                                                                 RunSum -> let (keys, tqs) = unzip g''
                                                                           in zip keys (scanl1 (<>) tqs)
                                                                 RunSumBack -> let (keys, tqs) = unzip g''
                                                                           in zip keys (scanr1 (<>) tqs)
                                                                 RSNormal -> g''
                                                             (qtype, TraceParam (valueFn, options, runsum)) = param
                                                             fn = fmap valueFn . lookupGrouped qtype
                       
                                                                    
         colorIds = zip (cycle defaultColors) [1::Int ..]
         asList = sortAndLimit (mconcat .  toList) rupture  grouped
         jsData = [ traceFor param (name'group, color :: Text, groupId :: Int)
                  | (param, pcId) <- zip params colorIds
                  , (name'group, gcId) <- zip asList colorIds
         -- if there is only one series, we don't need to group legend and colour by serie
                  , let (color, groupId) = if mono {-length grouped == 1-} then pcId else gcId
                  ] -- ) (cycle defaultColors) [1 :: Int ..])
     toWidgetBody [julius|
          Plotly.plot( #{toJSON plotId}
                    , #{toJSON jsData} 
                    , { margin: { t: 0 }
                      , title: #{toJSON name}
                      , yaxis2 : {overlaying: 'y', title: "Quantities", side: "right"}
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
  
itemToCsv param panelGrouperM colGrouper = do
  let grouper =  groupTranQPs param panelGrouperM colGrouper
  -- no need to group, we display everything, including all category and columns
  cols <- getCols
  categories <- categoriesH
  grouped <- loadItemTransactions param grouper
  let trans :: [(TranKey, TranQP)]
      trans = foldMap concat $ fmap toList grouped
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

