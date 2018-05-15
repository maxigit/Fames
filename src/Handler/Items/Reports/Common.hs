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
import GL.Utils(calculateDate)
import GL.Payroll.Settings

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
  }  deriving Show
paramToCriteria :: ReportParam -> [Filter FA.StockMove]
paramToCriteria ReportParam{..} = (rpFrom <&> (FA.StockMoveTranDate >=.)) ?:
                                  (rpTo <&> (FA.StockMoveTranDate <=.)) ?:
                                  (filterE id FA.StockMoveStockId  rpStockFilter)
 
-- TODO could it be merged with Column ?
data ColumnRupture = ColumnRupture
   { cpColumn :: Maybe Column
   , cpSortBy :: TraceParams
   , cpRankMode :: Maybe RankMode
   , cpLimitTo :: Maybe Int
   } deriving Show
-- | Trace parameter for plotting 
data TraceParams = TraceParams
  { tpDataType :: QPType
  , tpDataParams :: Identifiable [TraceParam]
  }  deriving Show
newtype TraceParam = TraceParam ((QPrice -> Double), Text {- Color-} -> [(Text, Value)], RunSum ) 
instance Show TraceParam where
  show _ = "<trace param>"
data RunSum = RunSum | RunSumBack | RSNormal deriving Show
data RankMode = RMResidual | RMResidualAvg | RMTop | RMTotal | RMAverage | RMBests | RMBestAvg | RMBestAndRes deriving (Eq, Ord, Show, Enum, Bounded)
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
  , colFn :: ReportParam -> TranKey -> NMapKey
  } 

instance Eq Column where
  a == b = colName a == colName b

instance Show Column where
  show c = "Column " ++ show (colName c)
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

  let style = Column "Style" (const' $ maybe PersistNull PersistText . tkStyle)
      variation = Column "Variation" (const' $ maybe PersistNull PersistText . tkVar)
      w52 = Column "52W" (\p tk -> let day0 = addDays 1 $ fromMaybe today (rpTo p)
                                       year = slidingYear day0 (tkDay tk)
                                   in mkNMapKey . PersistDay $ fromGregorian year 1 1
                          )
      defaultBand =  style
      defaultSerie = variation
      defaultTime = mkDateColumn monthly -- w52
      monthly =  ("End of Month", calculateDate EndOfMonth)
      mkDateColumn (name, fn) = Column name (const' $ PersistDay . fn . tkDay)
      const' fn = const ( mkNMapKey . fn)

      cols = [ style
            , variation
            , Column "Sku" (const' $ PersistText . tkSku)
            , Column "Date" (const' $ PersistDay . tkDay)
            , w52
            , Column "Customer" (const' $ maybe PersistNull (PersistInt64. fst) . tkCustomer)
            , Column "Supplier" (const' $ maybe PersistNull  PersistInt64 . tkSupplier)
            , Column "Supplier/Customer" (const' $ maybe PersistNull (either (PersistInt64 . fst)
                                                                             PersistInt64
                                                                    ). tkCustomerSupplier)
            , Column "TransactionType" (const' $ PersistInt64 . fromIntegral . fromEnum . tkType)
            , Column "Sales/Purchase" (const' $ maybe PersistNull PersistText . tkType'')
            , Column "Invoice/Credit" (const' $ maybe PersistNull PersistText . tkType')
            ]  <>
            ( map mkDateColumn [ ("End of Year", calculateDate EndOfYear)
                               , ("End of Week", calculateDate (EndOfWeek Sunday))
                               , monthly
                               ]
            ) <>
            [ Column ("Category:" <> cat) (const' $ \tk -> maybe PersistNull PersistText $ Map.lookup cat (tkCategory tk))
            | cat <- categories
            ]
  return (cols, (defaultBand, defaultSerie, defaultTime))
           
  -- return $ map Column $ basic ++ ["category:" <>  cat | cat <- categories]
-- * DB
loadItemTransactions :: ReportParam
                     -> ([(TranKey, TranQP)] -> QPGroup)
                     -> Handler QPGroup 
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
      grouper' = grouper . fmap (computeCategory categories catFinder) 
        

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
  key = TranKey stockMoveTranDate Nothing  stockMoveStockId (Just style) (Just var) (mapFromList categorieValues) (toEnum stockMoveType)
  (_customer, _supplier, tqp) =
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
detailToTransInfo :: (Entity FA.DebtorTransDetail, Single Day, Single ({- Maybe -} Int64), Single Int64) -> (TranKey, TranQP)
detailToTransInfo ( Entity _ FA.DebtorTransDetail{..}
                  , Single debtorTranTranDate
                  , Single debtorNo, Single branchCode)  = (key, tqp) where
  key = TranKey debtorTranTranDate
                (Just $ Left (debtorNo,  branchCode))
                debtorTransDetailStockId Nothing Nothing  (mempty)
                transType
  (tqp, transType) = case toEnum <$> debtorTransDetailDebtorTransType of
    Just ST_SALESINVOICE -> (tranQP QPSalesInvoice  (qp Outward), ST_SALESINVOICE)
    Just ST_CUSTCREDIT -> (tranQP QPSalesCredit (qp Inward), ST_CUSTCREDIT)
    else_ -> error $ "Shouldn't process transaction of type " <> show else_
  qp io = mkQPrice io debtorTransDetailQuantity price
  price = debtorTransDetailUnitPrice*(1-debtorTransDetailDiscountPercent/100)

purchToTransInfo :: (Entity SuppInvoiceItem, Single Day, Single Double, Single Int64)
                 -> (TranKey, TranQP)
purchToTransInfo ( Entity _ FA.SuppInvoiceItem{..}
                  , Single suppTranTranDate
                  , Single suppTranRate
                  , Single supplierId) = (key, tqp) where
  suppTranType = fromMaybe (error "supplier transaction should have a ty B") suppInvoiceItemSuppTransType
  key = TranKey suppTranTranDate (Just $ Right supplierId)
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
     -> [ColumnRupture]
     -> (QPGroup -> a )
     -> Handler a
itemReport param cols processor = do
  let grouper =  groupTranQPs param ((map cpColumn cols))
  grouped <- loadItemTransactions param grouper
  -- ranke
  let ranked = sortAndLimit cols grouped
  return $ processor ranked

groupTranQPs :: ReportParam
             -> [Maybe Column]
             -> [(TranKey, TranQP)]
             -> QPGroup
groupTranQPs param columns trans = let
  go column =  (colName <$> column -- level name
               , mkGrouper param (column) -- TranKey -> NMap Key
               )
  in groupAsNMap (map go columns) trans


mkGrouper :: ReportParam -> Maybe Column -> TranKey -> NMapKey
mkGrouper param = maybe (const $ mkNMapKey PersistNull) (flip colFn param)

pvToText :: PersistValue -> Text
pvToText PersistNull = ""
pvToText pv = either id id . fromPersistValueText $ pv
  
tableProcessor :: QPGroup -> Widget 
tableProcessor grouped = 
  [whamlet|
    $forall (h1, group1) <- nmapToNMapList grouped
        <div.panel.panel-info>
         $with name <- pvToText $ nkKey h1
          <div.panel-heading data-toggle="collapse" data-target="#report-panel-#{name}">
            <h2>#{pvToText $ nkKey h1}
          <div.panel-body.collapse.in id="report-panel-#{name}">
            <table.table.table-hover.table-striped>
              <tr>
                $forall level <-  nmapLevels group1
                  <th> #{fromMaybe "" level}
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
              $forall (keys, qp) <- nmapToList group1
                    <tr>
                      $forall key <- keys
                        <td> #{pvToText $ nkKey key}
                      ^{showQp Outward $ salesQPrice qp}
                      ^{showQp Inward $ purchQPrice qp}
                      ^{showQp Outward $ mconcat [salesQPrice qp , purchQPrice qp]}
              $with (qpt) <- summarize (toList group1)
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


summarize group = mconcat group --  sconcat (q :| qp) where  (q:qp) = toList group
-- *** Csv
qpToCsv io Nothing = ["", "", "", ""]
qpToCsv io (Just qp) = [ tshow (qpQty io qp)
                       , tshow (qpAmount io qp)
                       , tshow (qpMinPrice qp)
                       , tshow (qpMaxPrice qp)
                       ]
toCsv param grouped' = let
  header = intercalate "," $ (map tshowM $ nmapLevels grouped') <>
                          [  "Sales Qty"
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
    (keys, qp) <- nmapToList grouped'
    return $ intercalate "," $  ( map  (pvToText . nkKey) keys )
                             <> (qpToCsv Outward $ salesQPrice qp)
                             <> (qpToCsv Inward $ purchQPrice qp)
                             <> (qpToCsv Inward $ adjQPrice qp)
-- ** Sort and limit
-- sortAndLimit :: (val -> TranQP) -> ColumnRupture -> Map key val -> [(key, val)]
-- sortAndLimit collapse ColumnRupture{..} grouped = let
--   qtype = tpDataType cpSortBy
--   asList = Map.toList grouped
--   sorted = case getIdentified (tpDataParams cpSortBy) of
--      [] -> asList
--      (TraceParam (fn, _, _):_) -> let
--                     sortFn (key, val) =  fn <$> lookupGrouped qtype (collapse val)
--                     in sortOn sortFn asList
--   in maybe id take cpLimitTo sorted

-- sortAndLimitGroup :: (val -> TranQP)
--                   -> ColumnRupture
--                   -> NMap val
--                   -> [(NMapKey, val)]
-- sortAndLimitGroup  collapse  rupture nmap = -- TODO sortAndLimit collapse rupture
--   map firstKey $ nmapToList nmap
--   where firstKey (ks, v) = (fromMaybe PersistNull (headMay ks) , v)

sortAndLimit :: [ColumnRupture] -> QPGroup -> QPGroup
sortAndLimit _ leaf@(NLeaf _) = leaf
sortAndLimit [] nmap = nmap
sortAndLimit (r@ColumnRupture{..}:ruptures) n@(NMap levels m) = let
  qtype = tpDataType cpSortBy 
  -- use the sorting function to compute the rank
  rankFn qp = case (getIdentified (tpDataParams cpSortBy), cpColumn) of
              (_, Nothing) -> Nothing
              ([], _) -> Nothing
              ((TraceParam (fn, _, _):_), _) -> Just . PersistDouble  $ fromMaybe 0 $ fn <$> (lookupGrouped qtype qp)
      
  nmaps = sortOn fst [(NMapKey rank key, nmap)
                     | (NMapKey _ key, nmap0) <- Map.toList m
                     , let nmap = sortAndLimit ruptures nmap0
                     , let rank = rankFn (mconcat $ map snd $  nmapToList nmap)
                     ]

  truncated = case cpColumn *> cpLimitTo of
    Nothing -> nmaps
    Just n -> let (bests, residuals)  = splitAt n nmaps
                  -- replace value with actual rank
                  -- As we are reinserting the result with i
                  -- we need also to get rid of a level (the first ones.)
                  resFor :: [(NMapKey, QPGroup)] -> NMap TranQP
                  resFor res =
                               if length levels >1
                               then
                                 case sortAndLimit ruptures (mconcat $ map snd $ res) of
                                 -- case mconcat [(qpgroup) | (k, qpgroup) <- res  ] of
                                   NMap levels nm -> NMap (drop 0 levels) nm
                                   nm -> nm
                               else
                                 let grouped = mconcat $ map snd $ concatMap nmapToList (map snd res)
                                 in NLeaf $ grouped
                  resRank = rankFn (mconcat $ map snd $ nmapToList $ resFor residuals)
                  rankKey rk prefix = NMapKey rk (PersistText prefix)
                  forBests = [( rankKey  Nothing ("Best-" <> tshow (length bests)) ,  resFor bests)]
                  forResiduals = [( rankKey resRank ("Res-" <> tshow (length residuals)) , resFor residuals)]
                  average prefix nms = [( rankKey Nothing (prefix <> "-" <> tshow (length nms)) , fmap (mulTranQP  (1 / fromIntegral (length nms))) (resFor nms))]

              in case cpRankMode of
                                  Nothing ->  bests
                                  Just RMResidual -> bests <> forResiduals
                                  Just RMTotal -> bests <> [( rankKey Nothing "Total" , resFor nmaps)]
                                  Just RMAverage -> bests <> average "Avg" nmaps
                                  Just RMTop -> bests <> [( rankKey  Nothing "Top" ,  resFor [headEx bests])]
                                  Just RMBests -> forBests
                                  Just RMBestAvg -> bests <> average "AvgBest" bests
                                  Just RMResidualAvg -> bests <> average "AvgRes" residuals
                                  Just RMBestAndRes -> forBests <> forResiduals

  sortIf = id -- set to (sortOn fst) to include residuals in the sort and therefore rank it.
  ranked = [ (NMapKey (Just  $ PersistInt64  $ rank) key, val)
                 | ((NMapKey _ key, val), rank)  <- zip (sortIf truncated) [1..]
                 ]
  in NMap levels (Map.fromList ranked)

  
-- ** Plot
chartProcessor :: ReportParam -> QPGroup -> Widget 
chartProcessor param grouped = do
  -- addScriptRemote "https://cdn.plot.ly/plotly-latest.min.js"
  -- done add report level to fix ajax issue.
  let asList = nmapToNMapList grouped  -- sortAndLimitGroup (mconcat . map snd . concatMap toList) (rpPanelRupture param) grouped
  forM_ (zip asList [1 :: Int ..]) $ \((panelName, nmap), i) -> do
     let plotId = "items-report-plot-" <> tshow i 
         -- bySerie = fmap (groupAsMap (mkGrouper param (cpColumn $ rpSerie param) . fst) (:[])) group
     panelChartProcessor param (pvToText $ nkKey panelName) plotId nmap
        
  
panelChartProcessor :: ReportParam -> Text -> Text -> QPGroup -> Widget 
panelChartProcessor param name plotId0 grouped = do
  let asList = nmapToNMapList grouped --  sortAndLimitGroup (mconcat . map snd . concatMap toList) (rpBand param) grouped
  let plots = forM_ (zip asList [1:: Int ..]) $ \((bandName, bands), i) ->
        do
          let -- byColumn = nmapToNMapList grouped -- fmap (groupAsMap (mkGrouper param (Just $ rpColumnRupture param) . fst) snd) (unQPGroup' bands)
              traceParams = [(qtype, tparam )
                            | (TraceParams qtype tparams ) <- [rpTraceParam,  rpTraceParam2 , rpTraceParam3]
                                                               <*> [param]
                            , tparam <- getIdentified tparams
                            ]
              plot = seriesChartProcessor (rpSerie param) (isNothing $ cpColumn $ rpSerie param) traceParams (pvToText $ nkKey bandName) plotId bands 
              plotId = plotId0 <> "-" <> tshow i
          [whamlet|
            <div id=#{plotId} style="height:#{tshow plotHeight }px">
                ^{plot}
                  |]
      numberOfBands = length asList
      plotHeight = max 200 (800 `div` numberOfBands)
  [whamlet|
      <div.panel.panel-info>
        <div.panel-heading data-toggle="collapse" data-target="#report-panel-#{name}">
          <h2>#{name}
        <div.panel-body.collapse.in id="report-panel-#{name}">
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

seriesChartProcessor :: ColumnRupture -> Bool -> [(QPType, TraceParam)]-> Text -> Text -> QPGroup  -> Widget 
seriesChartProcessor rupture mono params name plotId grouped = do
     let xsFor g = map (toJSON . fst) g
         ysFor f g = map (toJSON . f . snd) g
         traceFor param ((name', g'), color,groupId) = Map.fromList $ [ ("x" :: Text, toJSON $ xsFor g) 
                                                    , ("y",  toJSON $ ysFor fn g)
                                                    -- , ("name", toJSON name )
                                                    , ("connectgaps", toJSON False )
                                                    , ("type", "scatter" ) 
                                                    , ("legendgroup", String (tshow groupId))
                                                    ]
                                                    -- <> maybe [] (\color -> [("color", String color)]) colorM
                                                    <> options color
                                                    <> (if name == PersistNull then [] else [("name", toJSON $ pvToText name)])
                                                       where g'' = [ (pvToText $ nkKey n, mconcat (toList nmap))  | (n, nmap) <- nmapToNMapList g' ] -- flatten everything if needed
                                                             g = case runsum of
                                                                 RunSum -> let (keys, tqs) = unzip g''
                                                                           in zip keys (scanl1 (<>) tqs)
                                                                 RunSumBack -> let (keys, tqs) = unzip g''
                                                                           in zip keys (scanr1 (<>) tqs)
                                                                 RSNormal -> g''
                                                             (qtype, TraceParam (valueFn, options, runsum)) = param
                                                             fn = fmap valueFn . lookupGrouped qtype
                                                             name = nkKey name'
                          
                       
                                                                    
         colorIds = zip (cycle defaultColors) [1::Int ..]
         asList = nmapToNMapList grouped-- sortAndLimit (mconcat .  toList) rupture  grouped
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
  let grouper =  groupTranQPs param [panelGrouperM, colGrouper]
  -- no need to group, we display everything, including all category and columns
  cols <- getCols
  categories <- categoriesH
  grouped <- loadItemTransactions param grouper
  let -- trans :: [(TranKey, TranQP)]
      trans = map snd $ nmapToList grouped
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
        (qp) <- trans
        return $ intercalate "," (
          -- [pvToText $ colFn col param key | col <- cols]
            (qpToCsv Outward $ salesQPrice qp)
            <> (qpToCsv Inward $ purchQPrice qp)
            <> (qpToCsv Inward $ adjQPrice qp)
            -- <> (map (tshowM . flip Map.lookup (tkCategory key)) categories)
                                 )
      source = yieldMany (map (<> "\n") csvLines)
  setAttachment  "items-report.csv"
  respondSource "text/csv" (source =$= mapC toFlushBuilder)
                 




  
  
-- *** Plot

-- ** Utils
-- splitToGroups :: (a -> k) -> (a -> a') ->   [(a,b)] -> [(k, (a',b))]

