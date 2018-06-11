module Handler.Items.Reports.Common where

import Import hiding(computeCategory)
import Handler.Table
import Items.Types
import Handler.Items.Common
import Handler.Util
import FA
import Data.Time(addDays, formatTime, defaultTimeLocale)
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty(..))
import Data.List(cycle,scanl1,scanr1, (!!))
import Database.Persist.MySQL(unSqlBackendKey, rawSql, Single(..))
import Data.Aeson.QQ(aesonQQ)
import GL.Utils(calculateDate, foldTime, Start(..), PeriodFolding(..), dayOfWeek, monthNumber, toYear)
import GL.Payroll.Settings
import Database.Persist.Sql hiding (Column)
import Text.Printf(printf)

-- * Param
data ReportParam = ReportParam
  { rpFrom :: Maybe Day
  , rpTo :: Maybe Day
  , rpPeriod :: Maybe PeriodFolding
  , rpNumberOfPeriods :: Maybe Int
  -- , rpCatFilter :: Map Text (Maybe Text)
  , rpCategoryToFilter :: Maybe Text
  , rpCategoryFilter :: Maybe FilterExpression
  , rpStockFilter :: Maybe FilterExpression
  , rpPanelRupture :: ColumnRupture
  , rpBand :: ColumnRupture
  , rpSerie :: ColumnRupture
  , rpColumnRupture :: Column
  , rpTraceParam :: TraceParams
  , rpTraceParam2 :: TraceParams
  , rpTraceParam3 :: TraceParams
  , rpLoadSales :: Bool
  , rpLoadPurchases :: Bool
  , rpLoadAdjustment :: Bool
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
  , tpDataNorm :: Maybe NormalizeMode
  }  deriving Show

--
data NormalizeMargin
  = NMRow
  | NMColumn
  | NMTotal -- margin of the target. Represent the total before the residual can be removed
  | NMFirst -- first row
  | NMLast -- last row. To comparte sales with last year for example
  | NMTruncated -- 
  deriving (Show, Eq, Enum, Bounded)
data NormalizeTarget
  = NMAll
  | NMPanel
  | NMBand
  | NMSerie
  deriving (Show, Eq, Enum, Bounded)

data NormalizeMode'
  = PercentageOfColumn
  | PercentageOfRow
  | PercentageOfTotal
  | PercentageOfTruncated
  | MaxOfRow
  | MedianOfRow
  deriving (Show, Eq, Enum, Bounded)
data NormalizeMode = NormalizeMode
  { nmMargin :: NormalizeMargin
  , nmTarget :: NormalizeTarget
  }
  deriving (Show, Eq)
  
-- | Parameter to define a plotly trace.
newtype TraceParam = TraceParam ((QPrice -> Double), Text {- Color-} -> [(Text, Value)], RunSum ) 
instance Show TraceParam where
  show _ = "<trace param>"
-- | should we do a a running sum (cumulative total) before displaying the trace ?
-- [1,2,0,5] -> 1,3,3,8 (runsum)
-- backward -> 8,7,5,5,0 (start from the end cumul and decrease to 0)
data RunSum = RunSum | RunSumBack | RSNormal deriving Show
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
periodOptions :: Day -> Maybe Day -> [(Text, PeriodFolding)]
periodOptions today from = let
  beginYear = fromGregorian (toYear today) 1 1
  in [("Whole Year", FoldYearly beginYear)
     ,("Sliding Year (today)", FoldYearly today)
     ,("Sliding Year (from)", FoldYearly (fromMaybe today from))
     -- ,("Fiscal Year", FoldYearly fiscal)
     ,("Monthly", FoldMonthly 2018)
     ,("Weekly", FoldWeekly)
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
  customerMap <- allCustomers False
  supplierMap <- allSuppliers False

  let style = Column "Style" (const' $ maybe PersistNull PersistText . tkStyle)
      variation = Column "Variation" (const' $ maybe PersistNull PersistText . tkVar)
      -- w52 = Column "52W" (\p tk -> let day0 = addDays 1 $ fromMaybe today (rpTo p)
      --                                  year = slidingYear day0 (tkDay tk)
      --                              in mkNMapKey . PersistDay $ fromGregorian year 1 1
                          -- )
      defaultBand =  style
      defaultSerie = variation
      defaultTime = mkDateColumn monthly -- w52
      monthly =  ("Beginning of Month", calculateDate BeginningOfMonth)
      mkDateColumn (name, fn) = Column name fn' where
        fn' p tk = let (d0, _) = foldDay p tk
                       d = fn d0
                   in case (rpPeriod p) of
                     Just FoldWeekly -> -- format
                       let w = dayOfWeek d
                       in NMapKey (Just $ fromEnum w)  (PersistText $ tshow w)
                     -- Just (FoldYearly _) -> -- format
                     --   let (_,m,_) = toGregorian d
                     --   in NMapKey (Just $ m)  (PersistText $ pack $ formatTime defaultTimeLocale "%B" d) 
                     -- Just (FoldMonthly _) -> -- format
                     --   let (_,_,m) = toGregorian d
                     --   in NMapKey Nothing  (PersistInt64 $ fromIntegral m)
                     _ ->  NMapKey Nothing (PersistDay d)
      const' fn = const ( mkNMapKey . fn)
      -- For the supplier and customer key
      -- we want the Map to use the id instead of the map for performance reason
      -- but we need to store the name somewhere.
      -- The easiest way (to be consistent with the other key)
      -- we set the id to the rank but the name to the key.
      -- this doesn't stop the name to be compare when building the map
      -- but at lest the id have been matched beforehand, which should reduce
      -- the number of Text comparison significantly.
      mkCustomerSupplierKey _ tkey = case tkCustomerSupplier tkey of
        Nothing -> NMapKey Nothing PersistNull
        Just (Left (custId, _)) ->
          NMapKey (Just $ fromIntegral custId)
                  (case lookup (FA.DebtorsMasterKey $ fromIntegral custId) customerMap of
                                   Nothing -> PersistNull 
                                   Just cust -> PersistText (decodeHtmlEntities $ FA.debtorsMasterName cust)
                  )
        Just (Right suppId) -> 
          NMapKey (Just $ fromIntegral suppId)
                  (case lookup (FA.SupplierKey $ fromIntegral suppId) supplierMap of
                     Nothing -> PersistNull 
                     Just supp -> PersistText (decodeHtmlEntities $ FA.supplierSuppName supp )
                  )
      mkTransactionType _ tkey = let ktype = tkType tkey
       in NMapKey (Just $ fromEnum ktype)
                  (PersistText $ showTransType ktype)
     
      foldDay p tkey = let
        day = tkDay tkey
        in  case rpPeriod p of
              Nothing -> (day, Start day)
              Just period -> foldTime period (tkDay tkey)

      getPeriod p tkey = let
        (_, Start d) = foldDay p tkey
        in case rpPeriod p of
             Just (FoldMonthly _) -> -- format
               let (_,m,_) = toGregorian d
               in NMapKey (Just m)  (PersistText $ pack $ formatTime defaultTimeLocale "%B" d) 
             Just (FoldYearly _) -> -- format
               let (y,_,_) = toGregorian d
               -- in NMapKey (Just $ fromIntegral y) (PersistText $ pack $ printf "%d-%d" y (y+1))
               -- at the mooment we display the rank-the value, so printing y-y+1
                   -- result in printing y-y-y+1
               in NMapKey (Just $ fromIntegral y) (PersistInt64 $ fromIntegral (y+1))


             _ -> NMapKey Nothing (PersistDay d) where
      -- getDay p tkey = NMapKey Nothing (PersistDay d) where
      --   (d, _) = foldDay p tkey

      cols = [ style
            , variation
            , Column "Sku" (const' $ PersistText . tkSku)
            , Column "Period" getPeriod
            -- , Column "Date" getDay
            , mkDateColumn ("Day", id)
            -- , w52
            -- , Column "Customer" (const' $ maybe PersistNull (PersistInt64. fst) . tkCustomer)
            -- , Column "Supplier" (const' $ maybe PersistNull  PersistInt64 . tkSupplier)
            , Column "Supplier/Customer" mkCustomerSupplierKey --  (const' $ maybe PersistNull (either (PersistInt64 . fst)
                                                               --              PersistInt64
                                                               --     ). tkCustomerSupplier)
            , Column "TransactionType" mkTransactionType
            , Column "Sales/Purchase" (const' $ maybe PersistNull PersistText . tkType'')
            , Column "Invoice/Credit" (const' $ maybe PersistNull PersistText . tkType')
            ]  <>
            ( map mkDateColumn [ ("Beginning of Year", calculateDate BeginningOfYear)
                               , ("Beginning of Week", calculateDate (BeginningOfWeek Sunday))
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
                     -> ([(TranKey, TranQP)] -> NMap TranQP)
                     -> Handler (NMap TranQP) 
loadItemTransactions param grouper = do
  let loadIf f loader = if f param then loader else return []
  categories <- categoriesH
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  catFinder <- categoryFinderCached
  skuToStyleVar <- skuToStyleVarH
  adjustments <- loadIf rpLoadAdjustment $ loadStockAdjustments param
  -- for efficiency reason
  -- it is better to group sales and purchase separately and then merge them
  sales <- loadIf rpLoadSales $ loadItemSales param
  purchases <- loadIf rpLoadPurchases $ loadItemPurchases param
  let salesGroups = grouper' sales
      purchaseGroups = grouper'  purchases
      adjGroups = grouper' adjustments
      grouper' = grouper . fmap (computeCategory skuToStyleVar categories catFinder) 
        

  return $ salesGroups <> purchaseGroups <> adjGroups

computeCategory :: (Text -> (Text, Text))
                -> [Text]
                -> (Text -> FA.StockMasterId -> Maybe Text)
                -> (TranKey, t)
                -> (TranKey, t)
computeCategory skuToStyleVar categories catFinder (key, tpq) = let
  sku = tkSku key
  cats = mapFromList [(cat, found) | cat <-  categories, Just found <- return $ catFinder cat (FA.StockMasterKey sku) ]
  (style, var) = skuToStyleVar sku
  in (key { tkCategory = mapFromList cats, tkStyle = Just style, tkVar = Just var}, tpq)

-- | Allows to load similar slices of time depending on the param
-- example the first 3 months of each year
generateTranDateIntervals :: ReportParam -> [(Text, PersistValue)]
generateTranDateIntervals param = let
  intervals = case (rpFrom param, rpTo param, rpNumberOfPeriods param) of
    (Nothing, Nothing, _)  -> [ ]
    (fromM, toM, Nothing)  -> [ (fromM, toM) ]
    (Just from, Nothing, Just n) -> -- go n year ago
          [ (Just (calculateDate (AddYears (-n)) from), Nothing) ]
    (Nothing, Just to, Just n) -> -- go n year ago
          [ (Nothing, Just to) ]
    (Just from, Just to, Just n) -> let
      period i = case rpPeriod param of
        Just (FoldYearly _) -> calculateDate (AddYears (-i))
        Just (FoldMonthly _) -> calculateDate (AddMonths (-i))
        Just (FoldWeekly) -> calculateDate (AddWeeks (-i))
        Nothing  -> calculateDate (AddYears (-i))
      in [ ( Just (period i from), Just (period i to)) | i <- [0..n] ]
  -- we need AND ((d>= from and d < to) OR (.. and ..))
  -- and some hack to use persist value even if not needed
  in  join $ [(" AND ? AND (", PersistBool True )] :
             [ ( maybe (" (?", PersistBool True)
                       (\d -> (" (tran_date >= ?", PersistDay d))
                       fromM
               ) :
               ( maybe (" AND ?) OR ", PersistBool True)
                       (\d -> (" AND tran_date < ?) OR", PersistDay d))
                       toM
               ) :
               []
             | (fromM, toM) <- intervals
             ]
             <> [[("?) ", PersistBool False)]] -- close the or clause
      

  
loadItemSales :: ReportParam -> Handler [(TranKey, TranQP)]
loadItemSales param = do
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  let catFilterM = (,) <$> rpCategoryToFilter param <*> rpCategoryFilter param
  let sql = intercalate " " $
          "SELECT ??, 0_debtor_trans.tran_date, 0_debtor_trans.debtor_no, 0_debtor_trans.branch_code" :
          " FROM 0_debtor_trans_details " :
          "JOIN 0_debtor_trans ON (0_debtor_trans_details.debtor_trans_no = 0_debtor_trans.trans_no " :
          " AND 0_debtor_trans_details.debtor_trans_type = 0_debtor_trans.type)  " :
          (if isJust catFilterM then "JOIN fames_item_category_cache AS category USING (stock_id)" else "" ) :
          "WHERE type IN ("  :
          (tshow $ fromEnum ST_SALESINVOICE) :
          ",":
          (tshow $ fromEnum ST_CUSTCREDIT) :
          ") " :
          "AND quantity != 0" :
          ("AND stock_id LIKE '" <> stockLike <> "'") : -- we don't want space between ' and stockLike
          -- " LIMIT 100" :
          []
      (w,p) = unzip $ (rpStockFilter param <&> (\e -> let (keyw, v) = filterEKeyword e
                                                      in (" AND stock_id " <> keyw <> " ?", PersistText v)
                                               )) ?:
                       case catFilterM of
                            Nothing -> []
                            Just (catToFilter, catFilter) ->
                                 let (keyw, v) = filterEKeyword catFilter
                                 in [ (" AND category.value " <> keyw <> " ?", PersistText v)
                                    , (" AND category.category = ? ", PersistText catToFilter)
                                    ]
                       <> generateTranDateIntervals param
        
  sales <- runDB $ rawSql (sql <> intercalate " "w) p
  return $ map detailToTransInfo sales

loadItemPurchases :: ReportParam -> Handler [(TranKey, TranQP)]
loadItemPurchases param = do
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  let catFilterM = (,) <$> rpCategoryToFilter param <*> rpCategoryFilter param
  let sql = intercalate " " $
          "SELECT ??, 0_supp_trans.tran_date, 0_supp_trans.rate, 0_supp_trans.supplier_id  FROM 0_supp_invoice_items " :
          "JOIN 0_supp_trans ON (0_supp_invoice_items.supp_trans_no = 0_supp_trans.trans_no " :
          " AND 0_supp_invoice_items.supp_trans_type = 0_supp_trans.type)  " :
          (if isJust catFilterM then "JOIN fames_item_category_cache AS category USING (stock_id)" else "" ) :
          "WHERE type IN ("  :
          (tshow $ fromEnum ST_SUPPINVOICE) :
          ",":
          (tshow $ fromEnum ST_SUPPCREDIT) :
          ") " :
          "AND quantity != 0" :
          ("AND stock_id LIKE '" <> stockLike <> "'") : -- we don't want space between ' and stockLike
          -- " LIMIT 100" :
          []
      (w,p) = unzip $ (rpStockFilter param <&> (\e -> let (keyw, v) = filterEKeyword e
                                                      in (" AND stock_id " <> keyw <> " ?", PersistText v)
                                               )) ?:
                       case catFilterM of
                            Nothing -> []
                            Just (catToFilter, catFilter) ->
                                 let (keyw, v) = filterEKeyword catFilter
                                 in [ (" AND category.value " <> keyw <> " ?", PersistText v)
                                    , (" AND category.category = ? ", PersistText catToFilter)
                                    ]
                       <> generateTranDateIntervals param
  purch <- runDB $ rawSql (sql <> intercalate " " w) p
  return $ map purchToTransInfo purch


loadStockAdjustments :: ReportParam -> Handler [(TranKey, TranQP)]
loadStockAdjustments param = do
  -- We are only interested in what's going in or out of the LOST location
  -- checking what's in DEF doesn't work, as it mixes
  -- transfers from incoming containers  with real adjusment
  lostLocation <- appFALostLocation . appSettings <$> getYesod
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  let catFilterM = (,) <$> rpCategoryToFilter param <*> rpCategoryFilter param
  let sql = intercalate " " $
          "SELECT ??" :
          "FROM 0_stock_moves" :
          (if isJust catFilterM then "JOIN fames_item_category_cache AS category USING (stock_id)" else "" ) :
          ("WHERE ( (type = " <> tshow  (fromEnum ST_INVADJUST)) :
                 (      "AND loc_code != '" <> lostLocation <> "'") : -- lost items are already lost,
                 -- we don't need to kno wif they are written off
                  ")" :
                 (" OR ( type = " <> tshow (fromEnum ST_LOCTRANSFER)) :
                 (      "AND loc_code = '" <> lostLocation <> "'") : --  lost of found item
                 "    )" :
                 ")" :
          " AND qty != 0" :
          ("AND stock_id LIKE '" <> stockLike <> "'") : 
          []

      (w,p) = unzip $ (rpStockFilter param <&> (\e -> let (keyw, v) = filterEKeyword e
                                                      in (" AND stock_id " <> keyw <> " ?", PersistText v)
                                               )) ?:
                       case catFilterM of
                            Nothing -> []
                            Just (catToFilter, catFilter) ->
                                 let (keyw, v) = filterEKeyword catFilter
                                 in [ (" AND category.value " <> keyw <> " ?", PersistText v)
                                    , (" AND category.category = ? ", PersistText catToFilter)
                                    ]
                       <> generateTranDateIntervals param

  moves <- runDB $ rawSql (sql <> intercalate " " w) p
  return $ map moveToTransInfo moves

-- * Converter
-- ** StockMove
moveToTransInfo (Entity _ FA.StockMove{..}) = (key, tqp) where
  key = TranKey stockMoveTranDate
                Nothing
                stockMoveStockId
                Nothing
                Nothing
                mempty
                (toEnum stockMoveType)
  tqp = case toEnum stockMoveType of
    ST_INVADJUST -> tranQP QPAdjustment (mkQPrice Inward stockMoveQty stockMoveStandardCost)
  -- transfers are relative to the LOST location so should be taking as Outward : +ve quantity = loss
    ST_LOCTRANSFER -> tranQP QPAdjustment (mkQPrice Outward stockMoveQty 0)
    _ -> error $ "unexpected transaction type " ++ show (toEnum stockMoveType :: FATransType) ++ " for stock adjustment "
  
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

-- ** Purchase info
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
     -> (NMap TranQP -> a )
     -> Handler a
itemReport param cols processor = do
  let grouper =  groupTranQPs param ((map cpColumn cols))
  grouped <- loadItemTransactions param grouper
  -- ranke
  let ranked = sortAndLimitTranQP cols grouped
  return $ processor ranked

groupTranQPs :: ReportParam
             -> [Maybe Column]
             -> [(TranKey, TranQP)]
             -> NMap TranQP
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
  
nkeyWithRank :: NMapKey -> Text
nkeyWithRank (NMapKey (Just i) key) = tshow i <> "-" <> pvToText key
nkeyWithRank (NMapKey Nothing key) = pvToText key

tableProcessor :: NMap TranQP -> Widget 
tableProcessor grouped = 
  [whamlet|
    $forall (h1, group1) <- nmapToNMapList grouped
        <div.panel.panel-info>
         $with name <- nkeyWithRank h1
          <div.panel-heading data-toggle="collapse" data-target="#report-panel-#{name}">
            <h2>#{nkeyWithRank h1}
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
                <th> Purch Average Price/
                <th> Adjust Qty
                <th> Adjust Amount
                <th> Adjust Min Price
                <th> Adjust Max Price
                <th> Adjust Average Price/
                <th> Summary Qty
                <th> Summary Amount
                <th> Summary Min Price
                <th> Summary max Price
                <th> Summary Average Price
              $forall (keys, qp) <- nmapToList group1
                    <tr>
                      $forall key <- keys
                        <td>
                           #{nkeyWithRank key}
                      ^{showQp Outward $ salesQPrice qp}
                      ^{showQp Inward $ purchQPrice qp}
                      ^{showQp Inward $ adjQPrice qp}
                      ^{showQp Outward $ summaryQPrice qp}
              $with (qpt) <- (nmapMargin group1)
                  <tr.total>
                    <td> Total
                    ^{showQp Outward $ salesQPrice qpt}
                    ^{showQp Outward $ purchQPrice qpt}
                      ^{showQp Inward $ adjQPrice qpt}
                      ^{showQp Outward $ summaryQPrice qpt}
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
sortAndLimitTranQP :: [ColumnRupture] -> NMap TranQP -> NMap TranQP
sortAndLimitTranQP ruptures nmap = let
  mkCol :: ColumnRupture ->  Maybe (NMapKey ->  TranQP -> Double, Maybe RankMode, Maybe Int)
  mkCol (ColumnRupture{..}) = case (getIdentified (tpDataParams cpSortBy), cpColumn) of
    (_, Nothing) -> Nothing
    ([], _) -> Nothing
    ((TraceParam (fn, _, _):_), _) -> Just ( \k mr -> maybe 0 fn $ lookupGrouped (tpDataType cpSortBy) $ mr
                                           , cpRankMode
                                           , cpLimitTo
                                           )
  in sortAndLimit (map mkCol ruptures) nmap
  

-- ** Plot
chartProcessor :: ReportParam -> NMap TranQP -> Widget 
chartProcessor param grouped = do
  -- addScriptRemote "https://cdn.plot.ly/plotly-latest.min.js"
  -- done add report level to fix ajax issue.
  let asList = nmapToNMapList grouped
  forM_ (zip asList [1 :: Int ..]) $ \((panelName, nmap), i) -> do
     let plotId = "items-report-plot-" <> tshow i 
         -- bySerie = fmap (groupAsMap (mkGrouper param (cpColumn $ rpSerie param) . fst) (:[])) group
     panelChartProcessor grouped param (nkeyWithRank panelName) plotId nmap
        
  
panelChartProcessor :: NMap TranQP -> ReportParam -> Text -> Text -> NMap TranQP -> Widget 
panelChartProcessor all param name plotId0 grouped = do
  let asList = nmapToNMapList grouped
  let plots = forM_ (zip asList [1:: Int ..]) $ \((bandName, bands), i) ->
        do
          let -- byColumn = nmapToNMapList grouped -- fmap (groupAsMap (mkGrouper param (Just $ rpColumnRupture param) . fst) snd) (unNMap TranQP' bands)
              traceParams = [(qtype, tparam, tpNorm )
                            | (TraceParams qtype tparams tpNorm ) <- [rpTraceParam,  rpTraceParam2 , rpTraceParam3]
                                                               <*> [param]
                            , tparam <- getIdentified tparams
                            ]
              plot = seriesChartProcessor all grouped (rpSerie param) (isNothing $ cpColumn $ rpSerie param) traceParams (nkeyWithRank bandName) plotId bands 
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

formatSerieValues :: Maybe NormalizeMode -> NMap TranQP -> NMap TranQP -> NMap TranQP -> (TranQP -> Maybe Double) -> [TranQP] -> [Maybe String]
formatSerieValues mode all panel band f g = let
           computeMargin t = case nmMargin <$> mode of
             (Just NMTruncated) -> [mconcat subMargins]
             (Just NMFirst) -> [headEx subMargins]
             (Just NMLast) -> [lastEx subMargins]
             _                  -> [nmapMargin t]
             where subMargins = map (nmapMargin . snd) (nmapToNMapList t)
           margins = case nmTarget <$> mode of
             (Just NMAll ) -> computeMargin all
             (Just NMPanel ) -> computeMargin panel
             (Just NMBand ) -> computeMargin band
             (_ ) -> g

           xs = map f  g
           -- normalize
           -- normalize = case mapMaybe f [nmapMargin grouped] of
           normalize = case mapMaybe (fmap abs . f) margins of
             [] -> const ""
             vs -> let result x =  case mode of
                         (Just _) -> printf "%0.1f" $ x * 100 / sum vs
                         _ -> formatDouble x
                   in result
                   -- in \x -> formatDouble $ x
           in map (fmap normalize) xs

seriesChartProcessor :: NMap TranQP -> NMap TranQP
  -> ColumnRupture -> Bool -> [(QPType, TraceParam, Maybe NormalizeMode )]-> Text -> Text -> NMap TranQP  -> Widget 
seriesChartProcessor all panel rupture mono params name plotId grouped = do
     let xsFor g = map (toJSON . fst) g
         -- ysFor :: Maybe NormalizeMode -> (b -> Maybe Double) -> [ (a, b) ] -> [ Maybe Value ]
         ysFor normM f g = map (fmap toJSON) $ formatSerieValues normM all panel grouped f (map snd g)
         traceFor param ((name', g'), color,groupId) = Map.fromList $ [ ("x" :: Text, toJSON $ xsFor g) 
                                                    , ("y",  toJSON $ ysFor normMode fn g)
                                                    -- , ("name", toJSON name )
                                                    , ("connectgaps", toJSON False )
                                                    , ("type", "scatter" ) 
                                                    , ("legendgroup", String (tshow groupId))
                                                    ]
                                                    -- <> maybe [] (\color -> [("color", String color)]) colorM
                                                    <> options color
                                                    <> (if name == PersistNull then [] else [("name", toJSON $ nkeyWithRank name')])
                                                       where g'' = [ (nkeyWithRank n, mconcat (toList nmap))  | (n, nmap) <- nmapToNMapList g' ] -- flatten everything if needed
                                                             g = case runsum of
                                                                 RunSum -> let (keys, tqs) = unzip g''
                                                                           in zip keys (scanl1 (<>) tqs)
                                                                 RunSumBack -> let (keys, tqs) = unzip g''
                                                                           in zip keys (scanr1 (<>) tqs)
                                                                 RSNormal -> g''
                                                             (qtype, TraceParam (valueFn, options, runsum), normMode) = param
                                                             fn = fmap valueFn . lookupGrouped qtype
                                                             name = nkKey name'
                          
                       
                                                                    
         colorIds = zip (cycle defaultColors) [1::Int ..]
         asList = nmapToNMapList grouped
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

