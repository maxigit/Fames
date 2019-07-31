module Handler.Items.Reports.Common where

import Import hiding(computeCategory, formatAmount, formatQuantity)
import Handler.Table
import Items.Types
import Handler.Items.Common
import Handler.Items.Reports.Forecast
import Handler.Items.Category.Cache
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
import Formatting
import Data.Monoid(Sum(..), First(..))
import Data.Align(align)
import Data.These 
import Lens.Micro.Extras (preview)

-- * Param
data ReportParam = ReportParam
  { rpToday :: Day -- today
  , rpDeduceTax :: Bool 
  , rpFrom :: Maybe Day
  , rpTo :: Maybe Day
  , rpPeriod' :: Maybe PeriodFolding'
  , rpNumberOfPeriods :: Maybe Int
  -- , rpCatFilter :: Map Text (Maybe Text)
  , rpCategoryToFilter :: Maybe Text
  , rpCategoryFilter :: Maybe FilterExpression
  , rpStockFilter :: Maybe FilterExpression
  , rpPanelRupture :: ColumnRupture
  , rpBand :: ColumnRupture
  , rpSerie :: ColumnRupture
  , rpColumnRupture :: ColumnRupture
  , rpDataParam :: DataParams
  , rpDataParam2 :: DataParams
  , rpDataParam3 :: DataParams
  , rpLoadSalesAndInfo :: Maybe SalesInfoMode -- ^ load or not, with or without extra info
  -- , rpLoadOrderInfo :: Bool
  , rpLoadSalesOrders :: Maybe (InOutward, OrderDateColumn, OrderQuantityMode)
  , rpLoadPurchases :: Bool
  , rpLoadAdjustment :: Bool
  -- , rpLoadForecast :: Bool
  , rpForecast :: (Maybe FilePath, Maybe InOutward)
  , rpColourMode :: ColourMode
  }  deriving Show
data OrderDateColumn = OOrderDate | ODeliveryDate deriving (Eq, Show)
data SalesInfoMode = SSalesOnly | SSalesAndOrderInfo deriving (Eq, Show)
data OrderQuantityMode = OOrderedQuantity | OQuantityLeft deriving (Eq, Show)
data ColourMode = Panel'Band'Colour | Band'Colour'Serie | Panel'Colour'Serie | TraceColour deriving (Eq, Show, Bounded, Enum, Ord)
paramToCriteria :: ReportParam -> [Filter FA.StockMove]
paramToCriteria ReportParam{..} = (rpFrom <&> (FA.StockMoveTranDate >=.)) ?:
                                  (rpTo <&> (FA.StockMoveTranDate <=.)) ?:
                                  (filterE id FA.StockMoveStockId  rpStockFilter)
 
rpJustFrom ReportParam{..} = fromMaybe rpToday rpFrom
rpJustTo ReportParam{..} = fromMaybe rpToday rpTo
rpLoadForecast = isJust . rpForecastDir
rpLoadSales = isJust . rpLoadSalesAndInfo
rpLoadOrderInfo = (== Just SSalesAndOrderInfo) . rpLoadSalesAndInfo
rpForecastDir = fst . rpForecast
rpForecastInOut = snd . rpForecast

t :: Text -> Text
t x = x
-- TODO could it be merged with Column ?
data ColumnRupture = ColumnRupture
   { cpColumn :: Maybe Column
   , cpSortBy :: DataParams
   , cpRankMode :: Maybe RankMode
   , cpLimitTo :: Maybe Int
   , cpReverse :: Bool
   } deriving Show
-- | Trace parameter for plotting 
data DataParams' f g = DataParams
  { dpDataType :: QPType
  , dpDataTraceParams :: f (g TraceParam)
  , dpDataNorm :: Maybe NormalizeMode
  }  -- deriving Show

type DataParams = DataParams' Identifiable []

pattern DataParamsU qtype tps normMode <- DataParams qtype (Identifiable (_, tps)) normMode
type DataParam = DataParams' Identity Identity
pattern DataParam qtype tp normMode = DataParams qtype (Identity (Identity tp)) normMode
deriving instance Show DataParams
deriving instance Show DataParam

dpDataTraceParam :: DataParam -> TraceParam
dpDataTraceParam = runIdentity . runIdentity . dpDataTraceParams

 
type Weight = (Sum Double, First PersistValue)
-- We don't want to add the reverse facility in here
-- as reverse is only use for display and not for for getting the residual
-- | a double or a persist value (Text or Date), with a Monoid instance
-- ideally value types shouldn't be mixed
-- used to sort and limit ruptures
-- we also add a up/down field to sort things in reverse
-- data Weight = Weight Bool (Sum Double) (First PersistValue) deriving (Show, Eq)
-- instance Ord Weight where
--   compare (Weight False a b) (Weight False a' b') = compare (a,b) (a',b')
--   compare (Weight True a b) (Weight True a' b') = compare (a',b') (a,b)
--   compare (Weight True _ _ ) (Weight False _ _) =  GT
--   compare (Weight False _ _ ) (Weight True _ _) =  LT
                                           
-- instance Monoid Weight where
--   mempty = Weight False mempty mempty
--   mappend = (<>)
-- instance Semigroup Weight where
--   Weight r a b <> Weight r' a' b' = Weight (r || r') (a <> a') (b <> b')

cpSorter :: ColumnRupture -> NMapKey -> TranQP -> Weight
cpSorter r@ColumnRupture{..} = case getIdentified $ dpDataTraceParams cpSortBy of
    (tp :_)->  \k tqp -> (Sum $ fromMaybe 0 $ (tpValueGetter tp) <$> (lookupGrouped (dpDataType cpSortBy) $ tqp), mempty)
    _ -> \k tqp -> (mempty ,First (Just $ nkKey k))
          
  
--
data NormalizeMargin
  = NMRow
  | NMColumn
  | NMTotal -- margin of the target. Represent the total before the residual can be removed
  | NMFirst -- first row
  | NMLast -- last row. To comparte sales with last year for example
  | NMBestTail -- best of everything but first
  | NMBestInit -- best of everything but last
  | NMTruncated -- 
  | NMRank -- 
  deriving (Show, Eq, Enum, Bounded)
data NormalizeTarget
  = NMAll
  | NMPanel
  | NMBand
  | NMSerie
  deriving (Show, Eq, Enum, Bounded)

data NormalizeMode = NormalizeMode
  { nmMargin :: NormalizeMargin
  , nmTarget :: NormalizeTarget
  }
  deriving (Show, Eq)
  
-- Type of value, used to format it accordingly
data ValueType
  = VAmount --
  | VQuantity
  | VPrice
  | VPercentage
  deriving (Show, Eq, Ord) -- , Enum, Bounded)

-- | Parameter to define a plotly trace.
data TraceParam = TraceParam
  { tpValueGetter :: QPrice -> Double
  , tpValueType :: ValueType
  , tpChartOptions :: Text {- Color-} -> [(Text, Value)]
  , tpRunSum :: RunSum  
  }
instance Show TraceParam where
  show _ = "<trace param>"
-- | should we do a a running sum (cumulative total) before displaying the trace ?
-- [1,2,0,5] -> 1,3,3,8 (runsum)
-- backward -> 8,7,5,5,0 (start from the end cumul and decrease to 0)
data RunSum = RunSum | RunSumBack | RSNormal deriving Show


data TraceType = Line | LineMarker | Smooth | SmoothMarker | Bar | Hv
  deriving (Show, Eq, Ord, Enum, Bounded )

-- ** Default  style
amountStyle 3 = smoothStyle AmountAxis
amountStyle 2 = smoothDotStyle AmountAxis
amountStyle _ = markerLineStyle AmountAxis

lineStyle axis color = [("type", String "scatter")
                    ,("name", String "Amount")
                    ,("mode", String "lines")
                    , axisFor axis
                    ,("line", [aesonQQ|{
                               color: #{color}
                                }|])
                    ]
markerLineStyle axis color = [("type", String "scatter")
                    ,("name", String "Amount")
                    , axisFor axis
                    ,("line", [aesonQQ|{
                               color: #{color}
                                }|])
                    ]
smoothStyle axis color = [("type", String "scatter")
                      ,("mode", String "lines")
                      ,("name", String "Sales")
                      , axisFor axis
                      ,("line", [aesonQQ|{
                               shape:"spline", 
                               color: #{color},
                               dash: "dot",
                               width: 1
                                }|])
                , ("marker", [aesonQQ|{symbol: "square"}|])
              ]
smoothDotStyle axis color = [("type", String "scatter")
                      ,("mode", String "lines+markers")
                      ,("name", String "Sales")
                      , axisFor axis
                      ,("line", [aesonQQ|{
                               shape:"spline", 
                               color: #{color},
                               dash: "dot",
                               width: 1
                                }|])
                , ("marker", [aesonQQ|{symbol: "square"}|])
              ]
-- cumulAmountStyle color = [("type", String "scatter")
--                     ,("name", String "Amount")
--                     ,("fill", "tonextx")
--                     ,("connectgaps", toJSON True)
--                     ,("line", [aesonQQ|{
--                                color: #{color}
--                                 }|])
cumulAmountStyle 3 = smoothStyle CumulAmountAxis
cumulAmountStyle 2 = smoothDotStyle CumulAmountAxis
cumulAmountStyle _ = markerLineStyle CumulAmountAxis
quantityStyle 2 = hvStyle QuantityAxis
quantityStyle n = smoothDotStyle QuantityAxis
hvStyle axis color = [("type", String "scatter")
                      ,("name", String "Quantity")
                      ,("line", [aesonQQ|{
                               shape:"hv",
                               color: #{color},
                               width: 1
                                }|])
                , ("marker", [aesonQQ|{symbol: "square-open"}|])
                , axisFor axis
                , ("showlegend", toJSON False)
              ]

axisFor axis = ("yaxis", ax) where
  ax = case axis of
         PriceAxis ->  "y5"
         AmountAxis -> "y"
         CumulAmountAxis -> "y3"
         QuantityAxis -> "y2"
         CumulQuantityAxis -> "y4"

    

quantityAmountStyle :: Int -> InOutward -> [(QPrice -> Amount, ValueType, Text -> [(Text, Value)], RunSum)]
quantityAmountStyle traceN io = [ (qpQty io, VQuantity,  quantityStyle (traceN+1), RSNormal)
                                , (qpAmount io, VAmount, amountStyle traceN, RSNormal)
                                   -- \color -> [("type", String "scatter")
                                         --           ,("name", String "Amount")
                                         --           ,("line", [aesonQQ|{
                                         --                   color: #{color},
                                         --                   shape: "spline",
                                         --                   width: 1
                                         --                   }|])
                                         -- ], RSNormal)
                         ]
priceStyle color = [("type", String "scatter")
                , ("marker", [aesonQQ|{symbol: "diamond"}|])
                , axisFor PriceAxis
                , ("name", "price")
                , ("line", [aesonQQ|{dash:"dot", width:1, color:#{color}}|])
              ]
pricesStyle :: [(QPrice -> Amount, ValueType,  Text -> [(Text, Value)], RunSum)]
pricesStyle = [(qpMinPrice , VPrice,  const [ ("style", String "scatter")
                             , ("fill", String "tonexty")
                             , ("fillcolor", String "transparent")
                             , ("mode", String "markers")
                             , ("connectgaps", toJSON True )
                             , ("showlegend", toJSON False )
                             ], RSNormal)
               ,(qpAveragePrice, VPrice , \color -> [ ("style", String "scatter")
                             , ("fill", String "tonexty")
                             , ("connectgaps", toJSON True )
                             , ("line", [aesonQQ|{color:#{color}}|])
                             ], RSNormal)
               ,(qpMaxPrice , VPrice, \color -> [ ("style", String "scatter")
                             , ("fill", String "tonexty")
                             , ("connectgaps", toJSON True )
                             -- , ("mode", String "markers")
                             , ("line", [aesonQQ|{color:#{color}}|])
                             -- , ("line", [aesonQQ|{width:0}|])
                             ], RSNormal)
                 ]
data PeriodFolding'
  = PFWholeYear
  | PFSlidingYearFrom
  | PFSlidingYearTomorrow
  | PFQuaterly
  | PFMonthly
  | PFWeekly
  deriving (Show, Eq)

periodOptions :: Day -> Maybe Day -> [(Text, PeriodFolding')]
periodOptions today from = let
  in [("Whole Year", PFWholeYear)
     ,("Sliding Year (to today)", PFSlidingYearTomorrow)
     ,("Sliding Year (from)", PFSlidingYearFrom)
     -- ,("Fiscal Year", FoldYearly fiscal)
     ,("Quaterly", PFQuaterly )
     ,("Monthly", PFMonthly )
     ,("Weekly", PFWeekly)
     ]
rpPeriod :: ReportParam -> Maybe PeriodFolding
rpPeriod rp = let
  today = rpToday rp
  beginYear = fromGregorian (currentYear) 1 1
  currentYear = toYear today
  tomorrowLastYear = calculateDate (AddYears (-1)) . calculateDate (AddDays 1) $ today
  in case rpPeriod' rp of
        Just PFWholeYear -> Just $ FoldYearly beginYear
        Just PFSlidingYearFrom -> Just $ FoldYearly (fromMaybe beginYear (rpFrom rp))
        Just PFSlidingYearTomorrow -> Just $ FoldYearly tomorrowLastYear
        Just PFMonthly ->  Just $ FoldMonthly currentYear
        Just PFQuaterly ->  Just $ FoldQuaterly currentYear
        Just PFWeekly -> Just $ FoldWeekly
        Nothing -> Nothing
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
  today <- todayH
  supplierCustomerColumn <- supplierCustomerColumnH
  categoryColumns <- categoryColumnsH
  customerCategoryColumns <- customerCategoryColumnsH
  orderCategoryColumns <- orderCategoryColumnsH

  let defaultBand =  styleColumn
      defaultSerie = variationColumn
      defaultTime = monthlyColumn
     
      -- getDay p tkey = NMapKey Nothing (PersistDay d) where
      --   (d, _) = foldDay p tkey

      cols = [ styleColumn
             , variationColumn
             , skuColumn
             , periodColumn
             , supplierCustomerColumn
             , transactionTypeColumn
             , salesPurchaseColumn
             , invoiceCreditColumn
             ]  <> dateColumns <> categoryColumns <> customerCategoryColumns <> orderCategoryColumns
  return (cols, (defaultBand, defaultSerie, defaultTime))
           
mkIdentifialParam  (t, tp'runsumS) = Identifiable (t, map mkTraceParam tp'runsumS)
mkTraceParam (f, vtype, options, runsum) = TraceParam f vtype options runsum
-- ** Default parameters
-- | Column colFn take a ReportParam as a parameter.
-- This is only used for a few case.
-- This function create a function suitable for Column from a normal function
constMkKey :: PersistField b => (a -> b) -> p -> a -> NMapKey
constMkKey fn = const ( mkNMapKey . toPersistValue . fn)

-- dataParamGetter :: DataParam -> 
dataParamGetter (DataParam qtype tp _) = fmap (tpValueGetter tp) . lookupGrouped qtype
dataParamGetter _ = const Nothing
-- | Change a day to match belong to the given period.
-- This is useful when plotting data from different period
-- but make them appears on the same abscissa.
-- For example, if the period is a whole year 2018 data, from 2017 needs to be "folded" to 2018
foldDay :: ReportParam -> TranKey -> (Day, Start)
foldDay p tkey = let
  day = tkDay tkey
  in  case rpPeriod p of
        Nothing -> (day, Start day)
        Just period -> foldTime period (tkDay tkey)

mkDateColumn :: (Text, Day -> Day) -> Column
mkDateColumn (name, fn) = Column name fn' where
  fn' p tk = let (d0, _) = foldDay p tk
                 d = fn d0
              in case (rpPeriod p) of
                Just FoldWeekly -> -- format
                  let w = dayOfWeek d
                  in NMapKey (PersistText $ tshow w)
                -- Just (FoldYearly _) -> -- format
                --   let (_,m,_) = toGregorian d
                --   in NMapKey (Just $ m)  (PersistText $ pack $ formatTime defaultTimeLocale "%B" d) 
                -- Just (FoldMonthly _) -> -- format
                --   let (_,_,m) = toGregorian d
                --   in NMapKey Nothing  (PersistInt64 $ fromIntegral m)
                _ ->  NMapKey (PersistDay d)
-- For the supplier and customer key
-- we want the Map to use the id instead of the map for performance reason
-- but we need to store the name somewhere.
-- The easiest way (to be consistent with the other key)
-- we set the id to the rank but the name to the key.
-- this doesn't stop the name to be compare when building the map
-- but at lest the id have been matched beforehand, which should reduce
-- the number of Text comparison significantly.
mkCustomerSupplierKey customerMap supplierMap _ tkey = case tkCustomerSupplier tkey of
  Nothing -> NMapKey PersistNull
  Just (Left (custId, _)) ->
    NMapKey -- (Just $ fromIntegral custId)
            (case lookup (FA.DebtorsMasterKey $ fromIntegral custId) customerMap of
                              Nothing -> PersistNull 
                              Just cust -> PersistText (decodeHtmlEntities $ FA.debtorsMasterName cust)
            )
  Just (Right suppId) -> 
    NMapKey  -- (Just $ fromIntegral suppId)
            (case lookup (FA.SupplierKey $ fromIntegral suppId) supplierMap of
                Nothing -> PersistNull 
                Just supp -> PersistText (decodeHtmlEntities $ FA.supplierSuppName supp )
            )
mkTransactionType _ tkey = let ktype = tkType tkey
  in NMapKey -- (Just $ fromEnum ktype)
            (PersistText $ showTransType ktype)

-- *** Columns
styleColumn = Column "Style" (constMkKey tkStyle)
variationColumn = Column "Variation" (constMkKey tkVar)
skuColumn = Column "Sku" (constMkKey tkSku)
periodColumn = Column "Period" getPeriod where
      getPeriod p tkey = let
        (_, Start d) = foldDay p tkey
        in case rpPeriod p of
             Just (FoldMonthly _) -> -- format
               let (_,_m,_) = toGregorian d
               in NMapKey (PersistText $ pack $ formatTime defaultTimeLocale "%B" d) 
             Just (FoldYearly _) -> -- format
               let (_y,_,_) = toGregorian d
               -- in NMapKey (Just $ fromIntegral y) (PersistText $ pack $ printf "%d-%d" y (y+1))
               -- at the mooment we display the rank-the value, so printing y-y+1
                   -- result in printing y-y-y+1
               -- in NMapKey (Just $ fromIntegral y) (PersistInt64 $ fromIntegral (y+1))
               in NMapKey  (PersistText $ pack $ formatTime defaultTimeLocale "%Y (%d %b)" d)


             _ -> NMapKey (PersistDay d) where
supplierCustomerColumnH = do
  customerMap <- allCustomers False
  supplierMap <- allSuppliers False
  return $ Column "Supplier/Customer" (mkCustomerSupplierKey customerMap supplierMap)
transactionTypeColumn = Column "TransactionType" mkTransactionType
salesPurchaseColumn = Column "Sales/Purchase" (constMkKey $ maybe PersistNull PersistText . tkType'')
invoiceCreditColumn = Column "Invoice/Credit" (constMkKey $ maybe PersistNull PersistText . tkType')
categoryColumnsH = do
  categories <- categoriesH
  return [ Column ("item:" <> cat) (constMkKey $ \tk -> maybe PersistNull PersistText $ Map.lookup cat (tkCategory tk))
         | cat <- categories
         ]

customerCategoryColumnsH = do
  categories <- customerCategoriesH
  -- branch
  customerMap <- allCustomers False
  let getBranch tk = do -- Maybe
        (custId, branchNo) <- tkCustomer tk
        let name = maybe (tshow custId)
                         (decodeHtmlEntities . FA.debtorsMasterName )
                         (Map.lookup (FA.DebtorsMasterKey $ fromIntegral custId) customerMap)
        return $ name <> "#" <> tshow branchNo
  return $ Column "customer:branch" (constMkKey $ maybe PersistNull PersistText . getBranch) : [ Column ("customer:" <> cat) (constMkKey $ \tk -> maybe PersistNull PersistText $ Map.lookup cat (tkCustomerCategory tk))
                                       | cat <- categories
         ]
orderCategoryColumnsH = do
  categories <- orderCategoriesH
  let mkDateCol getDay (name, fn) = Column name fn' where
        fn' _p tk = NMapKey $ maybe PersistNull (PersistDay . fn)  (getDay tk)
  return $ [ Column ("order:" <> cat) (constMkKey $ \tk -> maybe PersistNull PersistText $ Map.lookup cat (tkOrderCategory tk))
           | cat <- categories
           ]
          <> dateColumnsFor "order:date-" (mkDateCol tkOrderDay)
          <> dateColumnsFor "order:delivery-date-" (mkDateCol  tkOrderDeliveryDay)
  
dateColumns@[yearlyColumn, quarterlyColumn, weeklyColumn, monthlyColumn, dailyColumn]
  = dateColumnsFor "" mkDateColumn
dateColumnsFor prefix mkDateCol 
  = map (mkDateCol . first (prefix <>))
                     [ ("Beginning of Year", calculateDate BeginningOfYear)
                     , ("Beginning of Quarter", calculateDate (BeginningOfQuarter))
                     , ("Beginning of Week", calculateDate (BeginningOfWeek Sunday))
                     , ("Beginning of Month", calculateDate BeginningOfMonth)
                     , ("Day", id)
                     ]
w52 = Column "52W" (\p tk -> let day0 = addDays 1 $ fromMaybe (rpToday p) (rpTo p)
                                 year = slidingYear day0 (tkDay tk)
                             in mkNMapKey . PersistDay $ fromGregorian year 1 1
                   )
-- ** Default options
emptyRupture = ColumnRupture Nothing emptyTrace Nothing Nothing False
emptyTrace = DataParams QPSales (mkIdentifialParam noneOption) Nothing
noneOption = ("None" :: Text, [])
amountOutOption n = ("Amount (Out)" ,   [(qpAmount Outward, VAmount, amountStyle n, RSNormal)] )
amountInOption n = ("Amount (In)",     [(qpAmount Inward,  VAmount, amountStyle n, RSNormal)])

-- ** Default trace
bestSalesTrace = DataParams QPSales (mkIdentifialParam $ amountInOption 1) Nothing
-- * DB
loadItemTransactions :: ReportParam
                     -> ([(TranKey, TranQP)] -> NMap TranQP)
                     -> Handler (NMap TranQP) 
loadItemTransactions param grouper = do
  let loadIf f loader = if f param then loader else return []
  categories <- categoriesH
  custCategories <- customerCategoriesH
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  catFinder <- categoryFinderCached
  stockInfo <- loadStockInfo param
  custCatFinder <- customerCategoryFinderCached
  skuToStyleVar <- skuToStyleVarH
  adjustments <- loadIf rpLoadAdjustment $ loadStockAdjustments stockInfo param
  forecasts <- case rpForecast param of
    (Nothing, _) -> return []
    (Just forecastDir, io) -> loadItemForecast io forecastDir stockInfo (rpJustFrom param) (rpJustTo param)
  -- for efficiency reason
  -- it is better to group sales and purchase separately and then merge them
  sales <- loadIf rpLoadSales $ loadItemSales param
  salesOrdersM <- forM (rpLoadSalesOrders param) $ \(io, dateColumn, qtyMode) -> loadItemOrders param io dateColumn qtyMode
  let salesOrders = fromMaybe [] salesOrdersM
  purchases <- loadIf rpLoadPurchases $ loadItemPurchases param
  let salesGroups = grouper' sales
      orderGroups = grouper' salesOrders
      purchaseGroups = grouper'  purchases
      adjGroups = grouper' adjustments
      forecastGroups = grouper' forecasts
      grouper' = grouper . fmap (computeCategory skuToStyleVar categories catFinder custCategories custCatFinder) 
        

  return $ salesGroups <> purchaseGroups <> adjGroups <> forecastGroups <> orderGroups

createInitialStock infoMap day = mapMaybe go (mapToList infoMap) where
  go (sku, info) = do --maybe
    qoh <- iiInitialStock info
    guard (qoh /= 0)
    let key = TranKey day
                Nothing
                sku
                Nothing
                Nothing
                mempty
                mempty
                ST_INVADJUST
                Nothing Nothing mempty
        tqp = tranQP QPAdjustment (mkQPrice Inward qoh 0)


    Just  (key, tqp)
  
computeCategory :: (Text -> (Text, Text))
                -> [Text]
                -> (Text -> FA.StockMasterId -> Maybe Text)
                -> [Text]
                -> (Text -> FA.DebtorsMasterId -> Maybe Text)
                -> (TranKey, t)
                -> (TranKey, t)
computeCategory skuToStyleVar categories catFinder custCategories custCatFinder (key, tpq) = let
  sku = tkSku key
  debtorNo = tkCustomer key
  cats = mapFromList [(cat, found) | cat <-  categories, Just found <- return $ catFinder cat (FA.StockMasterKey sku) ]
  custCats = mapFromList [(cat, found)
                         | cat <-  custCategories
                         , Just found <- return $ custCatFinder cat =<< (FA.DebtorsMasterKey . fromIntegral . fst) <$> debtorNo
                         ]

  (style, var) = skuToStyleVar sku
  in (key { tkCategory = mapFromList cats
          , tkCustomerCategory = mapFromList custCats
          , tkStyle = Just style
          , tkVar = Just var
          }, tpq)

-- | Allows to load similar slices of time depending on the param
-- example the first 3 months of each year
generateTranDateIntervals :: ReportParam -> [(Text, PersistValue)]
generateTranDateIntervals = generateDateIntervals "tran_date"
generateDateIntervals :: Text -> ReportParam -> [(Text, PersistValue)]
generateDateIntervals date_column param = let
  intervals = case (rpFrom param, rpTo param, rpNumberOfPeriods param) of
    (Nothing, Nothing, _)  -> [ (Nothing, Nothing) ]
    (fromM, toM, Nothing)  -> [ (fromM, toM) ]
    -- (Just from, Nothing, Just n) -> -- go n year ago
    --       [ (Just (calculateDate (AddYears (-n)) from), Nothing) ]
    (Nothing, Just to, Just n) -> -- go n year ago
          [ (Nothing, Just to) ]
    (Just from, toM, Just n) -> let
      period i = case rpPeriod param of
        Just (FoldYearly _) -> calculateDate (AddYears (-i))
        Just (FoldMonthly _) -> calculateDate (AddMonths (-i))
        Just (FoldQuaterly _) -> calculateDate (AddMonths (-i*3))
        Just (FoldWeekly) -> calculateDate (AddWeeks (-i))
        Nothing  -> calculateDate (AddYears (-i))
      to = fromMaybe (period (-1) from) toM
      in [ ( Just (period i from)
           , Just ( min (period i to)
                        (period (i-1) from)
                  )
           )
         | i <- [0..n]
         ]
  -- we need AND ((d>= from and d < to) OR (.. and ..))
  -- and some hack to use persist value even if not needed
  in  join $ [(" AND ? AND (", PersistBool True )] :
             [ ( maybe (" (?", PersistBool True)
                       (\d -> (" (" <> date_column <> " >= ?", PersistDay d))
                       fromM
               ) :
               ( maybe (" AND ?) OR ", PersistBool True)
                       (\d -> (" AND " <> date_column <> " < ?) OR", PersistDay d))
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
  let sqlSelect = "SELECT ??, 0_debtor_trans.tran_date, 0_debtor_trans.debtor_no, 0_debtor_trans.branch_code, 0_debtor_trans.order_ "
  let sql0 = intercalate " " $
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
        
      sql = sql0 <> intercalate " " w
  sales <- runDB $ rawSql (sqlSelect <> sql) p
  orderCategoryMap <- if rpLoadOrderInfo param
                      then loadOrderCategoriesFor "0_debtor_trans.order_ " sql p
                      else return mempty
  return $ map (detailToTransInfo (rpDeduceTax param) orderCategoryMap) sales

loadOrderCategoriesFor :: Text -> Text -> [PersistValue] -> Handler (Map Int (Map Text Text))
loadOrderCategoriesFor order_field orderSql params = do
  let sql  = "SELECT ??  "
             <> " FROM fames_order_category_cache "
             <> " JOIN (SELECT distinct " <> order_field <> " " <> orderSql <> ") orders "
             <> "      ON (orders.order_ = fames_order_category_cache.order_id ) "
             <> " ORDER BY order_id" 

  cats <- runDB $ rawSql sql params
  return $ Map.fromListWith (<>) [ (orderCategoryOrderId, Map.singleton orderCategoryCategory orderCategoryValue )
                                    | (Entity _ OrderCategory{..}) <- cats
                                    ]


loadItemOrders :: ReportParam -> InOutward -> OrderDateColumn -> OrderQuantityMode -> Handler [(TranKey, TranQP)]
loadItemOrders param io orderDateColumn qtyMode = do
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  let catFilterM = (,) <$> rpCategoryToFilter param <*> rpCategoryFilter param
  let orderDateField = case orderDateColumn of
        OOrderDate -> "ord_date"
        ODeliveryDate -> "delivery_date"
  let sqlSelect = "SELECT ??, ??,  " <> orderDateField <> " AS effective_date "
  let sql0 = intercalate " " $
         "FROM 0_sales_order_details" :
         "JOIN 0_sales_orders USING(order_no, trans_type)" :
          (if isJust catFilterM then "JOIN fames_item_category_cache AS category ON (stock_id = stk_code)" else "" ) :
          "WHERE trans_type = "  : (tshow $ fromEnum ST_SALESORDER) :
          "AND quantity != 0" :
          ("AND stk_code LIKE '" <> stockLike <> "'") : -- we don't want space between ' and stockLike
          []
      (w,p) = unzip $ (rpStockFilter param <&> (\e -> let (keyw, v) = filterEKeyword e
                                                      in (" AND stk_code " <> keyw <> " ?", PersistText v)
                                               )) ?:
                       case catFilterM of
                            Nothing -> []
                            Just (catToFilter, catFilter) ->
                                 let (keyw, v) = filterEKeyword catFilter
                                 in [ (" AND category.value " <> keyw <> " ?", PersistText v)
                                    , (" AND category.category = ? ", PersistText catToFilter)
                                    ]
                       <> generateDateIntervals orderDateField param
      sql = sql0 <> intercalate " " w
  details <- runDB $ rawSql (sqlSelect <> sql) p
  orderCategoryMap <- loadOrderCategoriesFor "0_sales_orders.order_no AS order_" sql p
  return $ map (orderDetailToTransInfo io qtyMode orderCategoryMap) details

  
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


loadStockAdjustments :: Map Text ItemInitialInfo -> ReportParam -> Handler [(TranKey, TranQP)]
loadStockAdjustments infoMap param = do
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
  let initials = createInitialStock infoMap (rpJustFrom param)

  return $ map (moveToTransInfo infoMap) moves <> initials


-- | Load a set of filtered sku. This correspond of matching the ReportParam criteria.
-- Useful to filter the forecast for example.
loadValidSkus :: ReportParam -> Handler (Set Text)
loadValidSkus  param =  do
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  let catFilterM = (,) <$> rpCategoryToFilter param <*> rpCategoryFilter param
  let sql = intercalate " " $
          "SELECT distinct stock_id " :
          "FROM 0_stock_master" :
          (if isJust catFilterM then "JOIN fames_item_category_cache AS category USING (stock_id)" else "" ) :
          ("WHERE stock_id LIKE '" <> stockLike <> "'") : 
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

  rows <- runDB $ rawSql (sql <> intercalate " " w) p
  return $ setFromList $ map unSingle rows

-- | Basic item information, cost, price initital, stock at the start day (-1)
loadStockInfo :: ReportParam -> Handler (Map Text ItemInitialInfo)
loadStockInfo param = do
  let stockDay = rpJustFrom param
  defaultLocation <- appFADefaultLocation <$> getsYesod appSettings
  base <- basePriceList
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  let catFilterM = (,) <$> rpCategoryToFilter param <*> rpCategoryFilter param
  let sql = intercalate " " $
          "SELECT sm.stock_id, material_cost, price, qoh.qty " :
          "FROM 0_stock_master sm" :
          "LEFT JOIN 0_prices AS p ON (sm.stock_id = p.stock_id AND p.sales_type_id = ?) " :
          ("LEFT JOIN (" <> qoh <> ") qoh ON (sm.stock_id = qoh.stock_id)" ) :
          (if isJust catFilterM then "JOIN fames_item_category_cache AS category ON (sm.stock_id=category.stock_id)" else "" ) :
          (" WHERE sm.stock_id LIKE '" <> stockLike <> "'") : 
          []
      order = " ORDER BY sm.stock_id " 
      (w,p) = unzip $ (rpStockFilter param <&> (\e -> let (keyw, v) = filterEKeyword e
                                                      in (" AND sm.stock_id " <> keyw <> " ?", PersistText v)
                                               )) ?:
                       case catFilterM of
                            Nothing -> []
                            Just (catToFilter, catFilter) ->
                                 let (keyw, v) = filterEKeyword catFilter
                                 in [ (" AND category.value " <> keyw <> " ?", PersistText v)
                                    , (" AND category.category = ? ", PersistText catToFilter)
                                    ]
      qoh = "SELECT stock_id, SUM(qty) as qty FROM 0_stock_moves WHERE tran_date < ? AND loc_code = ? GROUP BY stock_id"
      toInfo (Single sku, Single cost, Single price, Single qoh ) = (sku, ItemInitialInfo cost price qoh)
  rows <- runDB $ rawSql (sql <> intercalate " " w <> order) (toPersistValue base: toPersistValue stockDay : toPersistValue defaultLocation:  p)
  -- traceShowM("Stock Info", rows)
  return . Map.fromList $ map toInfo rows
  
-- * Converter
-- ** StockMove
moveToTransInfo infoMap (Entity _ FA.StockMove{..}) = (key, tqp) where
  key = TranKey stockMoveTranDate
                Nothing
                stockMoveStockId
                Nothing
                Nothing
                mempty
                mempty
                (toEnum stockMoveType)
                Nothing Nothing mempty
  tqp = case toEnum stockMoveType of
    -- Adjustement should be counted with a negative price
    -- indeed a positiv adjustment, means that we found some (therefore should go toward the stock  : Inward)
    -- However, they didn't cost anything so the amount should go towards the sales : Outward
    ST_INVADJUST -> tranQP QPAdjustment (mkQPrice Inward stockMoveQty (-stockMoveStandardCost))
  -- transfers are relative to the LOST location so should be taking as Outward : +ve quantity = loss
    ST_LOCTRANSFER -> tranQP QPAdjustment (mkQPrice Outward stockMoveQty $ fromMaybe 0 costPrice)
    _ -> error $ "unexpected transaction type " ++ show (toEnum stockMoveType :: FATransType) ++ " for stock adjustment "
  costPrice = iiStandardCost =<< lookup stockMoveStockId infoMap  
  
-- ** Sales Details
detailToTransInfo :: Bool -> Map Int (Map Text Text)
                  -> (Entity FA.DebtorTransDetail, Single Day, Single ({- Maybe -} Int64), Single Int64, Single (Maybe Int))
                  -> (TranKey, TranQP)
detailToTransInfo deduceTax orderCategoryMap
        ( Entity _ FA.DebtorTransDetail{..}
                  , Single debtorTranTranDate
                  , Single debtorNo, Single branchCode, Single orderM)  = (key, tqp) where
  key' = TranKey debtorTranTranDate
                (Just $ Left (debtorNo,  branchCode))
                debtorTransDetailStockId Nothing Nothing  mempty mempty
                transType
  key = case flip lookup orderCategoryMap =<<  orderM of
    Nothing -> key' Nothing Nothing mempty
    Just cat -> key' (readMay =<< "date" `lookup` cat) (readMay =<< "delivery-date" `lookup` cat) cat
  (tqp, transType) = case toEnum <$> debtorTransDetailDebtorTransType of
    Just ST_SALESINVOICE -> (tranQP QPSalesInvoice  (qp Outward), ST_SALESINVOICE)
    Just ST_CUSTCREDIT -> (tranQP QPSalesCredit (qp Inward), ST_CUSTCREDIT)
    else_ -> error $ "Shouldn't process transaction of type " <> show else_
  qp io = mkQPrice io debtorTransDetailQuantity price
  price = (if deduceTax
          then debtorTransDetailUnitPrice - debtorTransDetailUnitTax
          else debtorTransDetailUnitPrice
          ) *(1-debtorTransDetailDiscountPercent) -- don't divide per 100, is not a percent but the real factor :-(

-- ** Order details
orderDetailToTransInfo :: InOutward -> OrderQuantityMode -> Map Int (Map Text Text) -- ^ Order category map
                      -> (Entity FA.SalesOrderDetail, Entity FA.SalesOrder, Single Day)
                      -> (TranKey, TranQP)
orderDetailToTransInfo io qtyMode orderCategoryMap (Entity _ FA.SalesOrderDetail{..}
                       , Entity  _ FA.SalesOrder{..}
                       , Single effectiveDate ) = (key, tqp) where
  key = TranKey effectiveDate
               (Just $ Left (fromIntegral salesOrderDebtorNo, fromIntegral salesOrderBranchCode))
               salesOrderDetailStkCode Nothing Nothing mempty mempty
               ST_SALESORDER
               (Just salesOrderOrdDate)
               (Just salesOrderDeliveryDate)
               (fromMaybe mempty (lookup salesOrderOrderNo orderCategoryMap))


  qty = case qtyMode of
          OOrderedQuantity -> salesOrderDetailQuantity
          OQuantityLeft -> salesOrderDetailQuantity - salesOrderDetailQtySent
  qp = mkQPrice io qty price
  price = salesOrderDetailUnitPrice * (1- salesOrderDetailDiscountPercent)
  tqp = tranQP QPSalesOrder qp

-- ** Purchase info
purchToTransInfo :: (Entity SuppInvoiceItem, Single Day, Single Double, Single Int64)
                 -> (TranKey, TranQP)
purchToTransInfo ( Entity _ FA.SuppInvoiceItem{..}
                  , Single suppTranTranDate
                  , Single suppTranRate
                  , Single supplierId) = (key, tqp) where
  suppTranType = fromMaybe (error "supplier transaction should have a ty B") suppInvoiceItemSuppTransType
  key = TranKey suppTranTranDate (Just $ Right supplierId)
                suppInvoiceItemStockId Nothing Nothing  mempty mempty
                (toEnum suppTranType)
                Nothing Nothing mempty
                   
  tqp = case toEnum suppTranType of
    ST_SUPPINVOICE -> tranQP QPPurchInvoice (qp Inward)
    ST_SUPPCREDIT -> tranQP QPPurchCredit (qp Outward)
    else_ -> error $ "Shouldn't process transaction of type " <> show else_
  qp io = mkQPrice io suppInvoiceItemQuantity price
  price = suppInvoiceItemUnitPrice*suppTranRate

-- * Reports
-- ** Common
-- | Display sales and purchase of an item
itemReportWithRank
  :: ReportParam
     -> [ColumnRupture]
     -> (NMap (Sum Double, TranQP) -> a )
     -> Handler a
itemReportWithRank param cols processor = do
  let grouper =  groupTranQPs param ((map cpColumn cols))
  grouped <- loadItemTransactions param grouper
  -- ranke
  let ranked = sortAndLimitTranQP cols grouped
  return $ processor ranked

itemReport :: ReportParam
           -> ([DataParams] -> (ColumnRupture, _) -> NMap TranQP -> b)
           -> Handler b
itemReport param processor = do
  let panel = rpPanelRupture param
      band = rpBand param
      serie = rpSerie param
      col = rpColumnRupture param
      -- for table, the exact meaning of the rupture doesn't matter
      cols = [ panel, band, serie, col]
      ruptures = (panel, (band, (serie, (col, ()))))
      tparams = map ($ param) [rpDataParam, rpDataParam2, rpDataParam3]
  let grouper =  groupTranQPs param ((map cpColumn cols))
  grouped <- loadItemTransactions param grouper

  -- ranke
  return $ processor tparams ruptures grouped

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
  
nkeyWithRank :: (Int, NMapKey) -> Text
nkeyWithRank (i, NMapKey key) = tshow i <> "-" <> pvToText key
-- nkeyWithRank :: NMapKey -> Text
-- nkeyWithRank (NMapKey key)  = pvToText key

commonCss = [cassius|
.text90
    writing-mode: sideways-lr
.just-right
  text-align: right
.topTwo
  background: #d0edf7
  color: #29abe0
span.RunSum::before
  content: ">>"
span.RunSumBack::before
  content: "<<"
.topOne
  background: #dff0d8
  color: #93c54b
                   |]
tableProcessor :: ReportParam -> NMap (Sum Double, TranQP) -> Widget 
tableProcessor param@ReportParam{..} grouped = do
  let levels = drop 1 $ nmapLevels grouped
      -- don't display sales order if it's includeg in either sales or purchase
      displayOrders = case rpLoadSalesOrders of
                    Nothing -> False
                    Just (Outward, _, _) ->  not (rpLoadSales param) -- included in sales but no sales
                    Just (Inward, _, _) -> not rpLoadPurchases --  included in purchases but no purchases
  toWidget commonCss
  [whamlet|
    $forall (h1, group1) <- nmapToNMapListWithRank grouped
        <div.panel.panel-info>
         $with name <- nkeyWithRank h1
          <div.panel-heading data-toggle="collapse" data-target="#report-panel-#{name}">
            <h2>#{nkeyWithRank h1}
          <div.panel-body.collapse.in id="report-panel-#{name}">
            <table.table.table-hover.table-striped.table-hover>
              <tr>
                $forall level <-  levels
                  <th> #{fromMaybe "" level}
                $if rpLoadSales param
                  <th> Sales Qty
                  <th> Sales Amount
                  <th> Sales Min Price
                  <th> Sales max Price
                  <th> Sales Average Price
                $if displayOrders
                  <th> Sales Order Qty
                  <th> Sales Order Amount
                  <th> Sales Order Min Price
                  <th> Sales Order max Price
                  <th> Sales Order Average Price
                $if rpLoadForecast param
                  <th> Forecast Qty
                  <th> Forecast Amount
                  <th> Forecast Min Price
                  <th> Forecast max Price
                  <th> Forecast Average Price
                $if rpLoadPurchases
                  <th> Purch Qty
                  <th> Purch Amount
                  <th> Purch Min Price
                  <th> Purch Max Price
                  <th> Purch Average Price
                $if rpLoadAdjustment
                  <th> Loss Qty
                  <th> Loss Amount
                  <th> Leftover
                <th> Profit Amount
                <th> Sales Through
                <th> %Loss (Qty)
                <th> Margin 
              $forall (keys, (_,qp)) <- nmapToListWithRank group1
                    <tr>
                      $forall key <- keys
                        <td>
                           #{nkeyWithRank key}
                       
                      $if rpLoadSales param
                        ^{showQp Outward $ salesQPrice qp}
                      $if displayOrders
                        $case rpLoadSalesOrders
                          $of Just (Outward, _, _)
                            ^{showQp Outward $ salesQPrice qp}
                          $of Just (Inward, _, _)
                            ^{showQp Inward $ purchQPrice qp}
                          $of Nothing
                      $if rpLoadForecast param
                        ^{showQp Outward $ forecastQPrice qp}
                      $if rpLoadPurchases
                        ^{showQp Inward $ purchQPrice qp}
                      $if rpLoadAdjustment
                        ^{showQpAdj $ adjQPrice qp}
                      ^{showQpMargin qp}
              $if not (null levels)
                $with (_,qpt) <- (nmapMargin group1)
                    <tr.total>
                      <td> Total
                      $forall level <- drop 1 levels
                        <td>
                      $if rpLoadSales param
                        ^{showQp Outward $ salesQPrice qpt}
                      $if displayOrders
                        $case rpLoadSalesOrders
                          $of Just (Outward, _, _)
                            ^{showQp Outward $ salesQPrice qpt}
                          $of Just (Inward, _, _)
                            ^{showQp Inward $ purchQPrice qpt}
                          $of Nothing
                      $if rpLoadForecast param
                        ^{showQp Outward $ forecastQPrice qpt}
                      $if rpLoadPurchases
                        ^{showQp Inward $ purchQPrice qpt}
                      $if rpLoadAdjustment
                        ^{showQpAdj $ adjQPrice qpt}
                      ^{showQpMargin qpt}
                      |]
  where
      showQp _ Nothing = [whamlet|
                                  <td>
                                  <td>
                                  <td>
                                  <td>
                                  <td>
                                  |]
      showQp io (Just qp) = let
        klass = case io of
          Inward -> "negative-good" :: Text
          Outward -> "negative-bad"
        in [whamlet|
                                  <td.just-right class="#{klass}"> #{formatQuantity (qpQty io qp)}
                                  <td.just-right class="#{klass}"> #{formatAmount (qpAmount io qp)}
                                  <td.just-right class="#{klass}"> #{formatPrice (qpMinPrice qp) }
                                  <td.just-right class="#{klass}"> #{formatPrice (qpMaxPrice qp) }
                                  <td.just-right class="#{klass}"> #{formatPrice (qpAveragePrice qp)}
                                  |]
      -- Adjustment are different because the price is negative
      -- posting quantity are good
      -- but negative amount are good too (cost )
      showQpAdj Nothing = [whamlet|
                                  <td>
                                  <td>
                                  |]
      showQpAdj (Just qp) = [whamlet|
                                  <td.just-right.negative-good.positive-bad> #{formatQuantity (qpQty Inward qp)}
                                  <td.just-right.negative-good> #{formatAmount (qpAmount Outward qp)}
                                  |]
      -- ignore margin
      showQpMargin tqp = do
        let _qp = summaryQPrice tqp
            salesM = salesQPrice tqp
            purchM = purchQPrice tqp
            adjM = adjQPrice tqp
            qSold = maybe 0 (qpQty Outward) salesM
            qIn = maybe 0 (qpQty Inward) purchM
            qLoss = maybe 0 (qpQty Outward) adjM
            salesThrough = 100 * qSold / qIn
            lossRatio = 100 * qLoss / qIn
            aSold = maybe 0 (qpAmount Outward) salesM
            aIn = maybe 0 (qpAmount Inward) purchM
            margin = 100 * (aSold - aIn) / aIn
        [whamlet|
                <td.just-righ.negative-good> #{formatQuantity $ qIn - qSold}
                <td.just-right.negative-bad.positive-good> #{formatAmount $ aSold - aIn}
                <td.just-right.negative-bad> #{formatPercentage salesThrough}
                <td.just-right.negative-good.positive-bad> #{formatPercentage lossRatio}
                <td.just-right.negative-bad> #{formatPercentage margin}
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
-- *** Sort and limit
sortAndLimitTranQP :: [ColumnRupture] -> NMap TranQP -> NMap (Sum Double, TranQP)
sortAndLimitTranQP ruptures nmap = let
  mkCol :: ColumnRupture ->  Maybe (NMapKey ->  TranQP -> Sum Double , Maybe RankMode, Maybe Int, Bool)
  mkCol (ColumnRupture{..}) = case (getIdentified (dpDataTraceParams cpSortBy), cpColumn, cpReverse) of
    (_, Nothing, False) -> Nothing
    ([], _col, False) -> Nothing
    ([], _, True) -> Just (\k mr -> Sum 0 , cpRankMode, cpLimitTo, cpReverse)
    ((tp :_), _,_) -> Just ( \k mr -> Sum $ fromMaybe 0 $ (tpValueGetter tp) <$> (lookupGrouped (dpDataType cpSortBy) $ mr)
                           , cpRankMode
                           , cpLimitTo
                           , cpReverse
                           )
  in sortAndLimit (map mkCol ruptures) nmap
  

nmapToNMapListWithRank :: Ord w =>  NMap (w, a) -> [((Int, NMapKey), NMap (w,a))]
nmapToNMapListWithRank  nmap =
  let asList = [ ( (fst (nmapMargin n), k)
                 , n)
               | (k, n) <- nmapToNMapList nmap
               , not (null n)
               ]
      sorted = sortOn fst asList
  in zipWith (\i ((w,k), n) -> ((i,k), n)) [1..]  sorted

nmapToListWithRank :: Ord w => NMap (w, a) -> [([(Int, NMapKey)], (w,a))]
nmapToListWithRank (NLeaf x) = [([], x)]
nmapToListWithRank nmap = do -- []
  ((rank'key), subNMap) <- nmapToNMapListWithRank nmap
  (rks, wa) <- nmapToListWithRank subNMap
  return (rank'key:rks, wa)

-- ** Plot
insertNullNMapLevel nmap = NMap (nmapMargin nmap) (Nothing:nmapLevels nmap) (Map.singleton (mkNMapKey PersistNull) nmap )
chartProcessor :: ReportParam -> NMap (Sum Double, TranQP) -> Widget 
chartProcessor param grouped = do
  case rpColourMode param of
    Band'Colour'Serie -> -- no panel, use band instead
      renderPanelWith  "items-report-chart" (insertNullNMapLevel grouped) (plotChartDiv param $ \n -> max 350 (900 `div` n))
    _ -> renderPanelWith  "items-report-chart" grouped (plotChartDiv param $ \n -> max 350 (900 `div` n))
        
renderPanelWith reportId grouped panelProcessor =  do
  let asList = nmapToNMapListWithRank grouped
  forM_ (zip asList [1 :: Int ..]) $ \((panelKey, nmap), i) -> do
     let plotId = reportId <> "-plot-" <> "-" <> tshow i 
         panelName = nkeyWithRank panelKey
         panelId = reportId <> "-panel-" <> panelName
         panel = panelProcessor grouped plotId nmap
     [whamlet|
      <div.panel.panel-info>
        <div.panel-heading data-toggle="collapse" data-target="#{panelId}">
          <h2>#{panelName}
        <div.panel-body.collapse.in id="#{panelId}" style="max-height:2000px; overflow:auto">
          ^{panel}
            |]
  
-- processRupturesWith :: Monoid w
--                   => (ColumnRupture, rs )
--                   -> (NMap TranQP)
--                   -> (NMap TranQP -> rs -> (NMapKey, NMap TranQP) -> (NMapKey, TranQP, w) )
--                   -> w
processRupturesWith subProcessor parents (rupture, subruptures) nmap =  let
  key'nmaps = nmapToNMapList nmap
  weigher (k,t) = cpSorter rupture k  (nmapMargin t)
  sorted = sortOn weigher key'nmaps
  rev = if cpReverse rupture then reverse else id
  (bests, residuals) = case cpLimitTo rupture of
                         Nothing -> (sorted, [])
                         Just limit ->  splitAt limit sorted

  limited = rev $ makeResidualNoRank (cpRankMode rupture) bests residuals
  in mconcat $ zipWith  (\(k,n) i -> subProcessor k i (nmap, parents) subruptures n) limited [1..]


createKeyRankProcessor f key rank parents ruptures nmap= let
  (p,w) = f key rank
  children  = processRupturesWith p parents ruptures nmap
  in w children
      

dataParamsToDataParam0s :: DataParams -> [DataParam]
dataParamsToDataParam0s (DataParams qtype tparams tpNorm) =
    [ DataParam qtype tparam tpNorm
    | tparam <- getIdentified tparams
    ]
rpDataParam0ss :: ReportParam -> [[DataParam]]
rpDataParam0ss param = map dataParamsToDataParam0s $ [rpDataParam,  rpDataParam2 , rpDataParam3] <*> [param]
rpDataParam0s :: ReportParam -> [DataParam]
rpDataParam0s = join . rpDataParam0ss
  
plotChartDiv :: ReportParam -> (Int -> Int ) -> NMap (Sum Double, TranQP) -> Text -> NMap (Sum Double, TranQP) -> Widget 
plotChartDiv param heightForBands all plotId0 panels = do
  let plotSeries bandName plotId bands =
        let band'colours = case rpColourMode param of
              mode | mode == Band'Colour'Serie || mode == Panel'Colour'Serie -> zip [ (serie, Just $ pvToText (nkKey key))
                                       | (key, serie) <- (nmapToNMapList bands)
                                       ] (map repeat defaultColors)
                      -- ^ aggregate band and serie
              _ ->[((bands, Nothing), cycle defaultColors)]
        in seriesChartProcessor all panels (rpSerie param)
                             ((isNothing $ cpColumn $ rpSerie param) || rpColourMode param == TraceColour) -- mono use a different colour for each trace instead of each serie
                             (rpDataParam0s param) bandName plotId band'colours
  renderPlotDiv plotSeries heightForBands plotId0 (if rpColourMode param == Panel'Colour'Serie then insertNullNMapLevel panels else panels)

-- | Draw a plot per band within a panel
-- renderPlotDiv :: (_ -> _)
--                    ->  (Int -> Int ) -> NMap (Sum Double, TranQP) -> Text -> NMap (Sum Double, TranQP) -> Widget 
renderPlotDiv plotSeries heightForBands plotId0 panels = do
  let asList = nmapToNMapListWithRank panels
      numberOfBands = length asList
      plotHeight = heightForBands numberOfBands -- max 350 (900 `div` numberOfBands)
  forM_ (zip asList [1:: Int ..]) $ \((bandName, bands), i) ->
        do
          let -- byColumn = nmapToNMapList grouped -- fmap (groupAsMap (mkGrouper param (Just $ rpColumnRupture param) . fst) snd) (unNMap TranQP' bands)
              plot = plotSeries (nkeyWithRank bandName) plotId bands 
              plotId = plotId0 <> "-" <> tshow i
          [whamlet|
            <div id=#{plotId} style="height:#{tshow plotHeight }px">
                ^{plot}
                  |]
    
defaultColors :: [Text]
defaultColors = defaultPlottly where
  defaultPlottly  = ["#2ca02c",  -- cooked asparagus green
              "#ff7f0e",  -- safety orange
              "#1f77b4",  -- muted blue
              "#d62728",  -- brick red
              "#9467bd",  -- muted purple
              "#8c564b",  -- chestnut brown
              "#e377c2",  -- raspberry yogurt pink
              "#7f7f7f",  -- middle gray
              "#bcbd22",  -- curry yellow-green
              "#17becf"   -- blue-teal
             ]

-- | Format the values of a serie to text. Include rounding, formatting as well
-- as computing %  (normalizing) if needed. To normalize we need the original set
-- of transactions to calculate 
formatSerieValues :: (Double -> t) -> (Double -> t)
                  -> Maybe NormalizeMode -- normalising, %, by row, col etct
                  -> NMap (Sum Double, TranQP) -- all
                  -> NMap (Sum Double, TranQP)
                  -> NMap (Sum Double, TranQP)
                  -> (TranQP -> Maybe Double) -- get a value from a tran
                  -> NMap (Sum Double, TranQP) -- list of tran to convert
                  -> [Maybe t]
formatSerieValues formatValue formatPercent mode all panel band f nmap = let
  -- key'valueS = [(mkNMapKey (PersistText t), v) | (t, v ) <- text'valueS]
  keys = map fst (nmapToNMapList nmap)
  output = formatSerieValuesNMapXXX formatValue formatPercent mode all panel band f nmap
  in map (flip lookup output ) keys

nmapRunSum :: (Monoid b) => RunSum -> NMap b -> NMap b
nmapRunSum runsum nmap = let
  nmap' = [ (key, mconcat (toList nmap))  | (key, nmap) <- nmapToNMapList nmap ] -- flatten everything if needed
  key'sumS =  case runsum of
       RunSum     -> let (keys, tqs) = unzip nmap'
                     in zip keys (scanl1 mappend tqs)
       RunSumBack -> let (keys, tqs) = unzip nmap'
                     in zip keys (scanr1 mappend tqs)
       RSNormal   -> nmap'
  in nmapFromList (join . headMay $ nmapLevels nmap) key'sumS

-- allows to get the tail or init but always leave a element
-- if needed
getSubMargin :: ([a] -> [a]) -> [a] -> [a]
getSubMargin getter subs = case subs of
  [x] -> [x]
  _ -> getter subs
-- | NMap version of formatSerieValues
formatSerieValuesNMapXXX :: Monoid w => 
                  (Double -> t) -> (Double -> t)
                  -> Maybe NormalizeMode -- normalising, %, by row, col etct
                  -> NMap (w,  TranQP) -- all
                  -> NMap (w,  TranQP)
                  -> NMap (w,  TranQP)
                  -> (TranQP -> Maybe Double) -- get a value from a tran
                  -> NMap (w,  TranQP)-- list of tran to convert
                  -> Map NMapKey t
formatSerieValuesNMapXXX formatAmount formatPercent mode all panel band f0 nmap = let
           f = f0 . snd
           computeMargin t = case nmMargin <$> mode of
             (Just NMTruncated) -> NLeaf (mconcat subMargins)
             (Just NMFirst) -> NLeaf (headEx subMargins)
             (Just NMLast) -> NLeaf (lastEx subMargins)
             (Just NMBestTail) -> NLeaf (maximumByEx (comparing f) $  getSubMargin tailEx subMargins) -- TODO use minLen instead
             (Just NMBestInit) -> NLeaf (maximumByEx (comparing f) $  getSubMargin initEx subMargins)
             (Just NMColumn) -> let
                        -- regroup margin by column TODO computes on0
                        -- all data are already grouped by column, but at the deepest level of nesting
                        -- we need to bring it up
                        alls = nmapToList t
                        in groupAsNMap [(Nothing, lastEx)] alls
             (Just NMRank) -> case nmTarget <$> mode of
               Just NMSerie ->  let -- column by serie , we need the full serie for each column key
                        -- we need to duplicate the levels ... So lookup of a column
                        -- gives the map of all columns/values
                        keys = map fst asList
                        in NMap (nmapMargin nmap) [] (mapFromList $ [(key, nmap ) | key <- keys ] )
               _ -> let -- by band
                        -- regroup margin by column TODO computes on0
                        -- all data are already grouped by column, but at the deepest level of nesting
                        -- we need to bring it up, but keep the list of value
                        alls = [(take 2 $ reverse keys, nmap) | (keys, nmap) <- nmapToList band ]
                        in groupAsNMap [(Nothing, headEx), (Nothing, lastEx)] alls
             _ -> NLeaf (nmapMargin t)
             where subMargins = map (nmapMargin . snd) (nmapToNMapList t)
           margins = case (nmMargin <$> mode, nmTarget <$> mode)  of
             (Just NMColumn, Just NMSerie ) -> computeMargin band -- otherwise, everything is 100%
             (Just NMRank, _ ) -> computeMargin band -- 
             (_, Just NMAll ) -> computeMargin all
             (_, Just NMPanel ) -> computeMargin panel
             (_, Just NMBand ) -> computeMargin band
             (_ ) -> computeMargin nmap
           marginMap = nmapToMap margins

           asList = nmapToNMapList nmap
           key'tranS = nmapMargin <$$>  asList
           -- normalize
           -- normalize = case mapMaybe f [nmapMargin grouped] of
           normalize (col, tqp) = (f tqp >>= go) <&> (col,) where
             go x = do
               norm <- f (nmapMargin margins)
               case nmMargin <$> mode of
                 Just NMColumn -> do
                   marginCol <- lookup col marginMap
                   normCol <- f (nmapMargin marginCol)
                   return $ formatPercent $ x * 100 / abs normCol
                 Just NMRank -> do
                   marginCol <- lookup col marginMap
                   let values = mapMaybe (f . nmapMargin . snd) (nmapToNMapList marginCol)
                       sorted = sort values
                       rankMap :: Map Double Int
                       rankMap = mapFromList $ zip (reverse sorted) [1 :: Int ..]
                   rank <- lookup x rankMap
                   return $ formatPercent (fromIntegral rank)
                 Just _ -> Just $ formatPercent $ x * 100 / abs norm
                 Nothing -> Just $ formatAmount x
           result = mapMaybe normalize key'tranS
           in mapFromList result

formatSerieValuesNMap :: (Double -> t) -> (Double -> t)
                  -> Maybe NormalizeMode -- normalising, %, by row, col etct
                  -> NMap TranQP -- all
                  -> NMap TranQP
                  -> NMap TranQP
                  -> (TranQP -> Maybe Double) -- get a value from a tran
                  -> NMap TranQP-- list of tran to convert
                  -> Map NMapKey t
formatSerieValuesNMap formatAmount formatPercent mode all panel band f nmap = 
  formatSerieValuesNMapXXX formatAmount formatPercent mode all' panel' band' f nmap' where
  all' = convert all
  panel' = convert panel
  band' = convert band
  nmap' = convert nmap
  convert xs = ((),) <$> xs

traceParamForChart mono asList params colours =  let
    colorIds = zip (colours) [1::Int ..]
    in [ (param, name'group, color :: Text, groupId :: Int)
       | (param, pcId) <- zip params colorIds
       , (name'group, gcId) <- zip asList colorIds
       -- if there is only one series, we don't need to group legend and colour by serie
       , let (color, groupId) = if mono {-length grouped == 1-} then pcId else gcId
       ] -- ) (cycle defaultColors) [1 :: Int ..]

seriesChartProcessor :: NMap (Sum Double, TranQP) -> NMap (Sum Double, TranQP)
  -> ColumnRupture
  -> Bool
  -> [DataParam]
  -> Text
  -> Text
  -> [((NMap (Sum Double, TranQP), Maybe Text), [Text] )] -- ^ Series , colour fun
  -> Widget 
seriesChartProcessor all panel rupture mono0 params name plotId grouped'colour = do
     let -- ysFor :: Maybe NormalizeMode -> (b -> Maybe Double) -> [ (a, b) ] -> [ Maybe Value ]
         jsData = do -- List
           ((grouped, tracePrefix), colours) <- grouped'colour
           let ysFor normM f g = map (fmap toJSON) $ formatSerieValues formatDouble (printf "%0.1f")normM all panel grouped f g
               asList = (if cpReverse rupture then reverse else id ) $ [ ((r, prefixedKey ), nmap)
                                                                       | ((r, key), nmap) <- nmapToNMapListWithRank grouped
                                                                       , let prefixedKey = case tracePrefix of
                                                                               Nothing -> key
                                                                               Just pre -> mkNMapKey $ intercalate " - " . filter (not . null) $ [ pre, pvToText (nkKey key)]
                                                                       ]
               mono = case tracePrefix of
                 Nothing -> mono0
                 Just _ -> True
           map (traceFor textValuesFor ysFor) (traceParamForChart mono asList  params colours)
     toWidgetBody [julius|
          Plotly.plot( #{toJSON plotId}
                    , #{toJSON jsData} 
                    , { margin: { t: 30 }
                      , title: #{toJSON name}
                      , yaxis2 : {overlaying: 'y', title: "Quantities", side: "right"}
                      , yaxis3 : {overlaying: 'y', title: "Amount(T)", side: "right"}
                      , yaxis4 : {overlaying: 'y', title: "Quantities(T)", side: "left"}
                      , yaxis5 : {overlaying: 'y', title: "Price"}
                      }
                    );
                |]
  
textValuesFor = map (toJSON . pvToText . fst)

traceFor :: ([(PersistValue, (Sum Double, TranQP))] -> [Value]) -- ^ generate x values/l
         -> ( -- ^ generates ys as Double
               Maybe NormalizeMode -- ^ use how to apply magin
            -> (TranQP -> Maybe Double) -- ^ value getter
            -> NMap (Sum Double, TranQP)
            -> [Maybe Value])
         -> (DataParam
            , ((Int, NMapKey) -- ^ rank and trace/serie name
              , NMap (Sum Double, TranQP)) -- ^ values to graph
              , Text -- ^ colour
              , Int -- ^ group id
              )
         -> Value
traceFor xsFor ysFor (param, (name', g'), color,groupId) = let
    g = [ (nkKey (snd n), mconcat (toList nmap))  | (n, nmap) <- nmapToNMapListWithRank g'' ] -- flatten everything if needed
    g'' = nmapRunSum (tpRunSum tp) g'
    DataParam qtype tp  normMode = param
    fn = fmap (tpValueGetter tp) . lookupGrouped qtype
    name = nkKey (snd name')
    in object $ [ "x" .=  xsFor g 
                , "y" .=  ysFor normMode fn g''
                , "connectgaps" .=  False 
                , "type" .=  String "scatter"  
                , "legendgroup" .= groupId
                ]
                -- <> maybe [] (\color -> [("color", String color)]) colorM
                <> tpChartOptions tp color
                <> (if name == PersistNull then [] else ["name" .= nkeyWithRank name'])

nmapToListWithRunSum :: (Ord w, Monoid a, Monoid w) =>
         RunSum -> NMap (w, a) -> [(PersistValue, (w, a))]
nmapToListWithRunSum runSum g' = let
    g = [ (nkKey (snd n), mconcat (toList nmap))  | (n, nmap) <- nmapToNMapListWithRank g'' ] -- flatten everything if needed
    g'' = nmapRunSum runSum g'
    in g
-- ** Bubble
bubbleProcessor :: ReportParam -> NMap (Sum Double, TranQP) -> Widget 
bubbleProcessor param grouped = do
  renderPanelWith  "items-report-bubble" grouped (renderBubblePlotDiv param $ \n -> max 350 (900 `div` n))
        
  
renderBubblePlotDiv :: ReportParam -> (Int -> Int ) -> NMap (Sum Double, TranQP) -> Text -> NMap (Sum Double, TranQP) -> Widget 
renderBubblePlotDiv param heightForBands all plotId0 panels = do
  let plotSeries bandName plotId bands =
        seriesBubbleProcessor all
                              panels
                              (rpSerie param)
                              (isNothing $ cpColumn $ rpSerie param)
                              (traceParamsForBubble param)
                              bandName plotId bands
  renderPlotDiv plotSeries heightForBands plotId0 panels

-- | Normally, value (or trace params) given in then report parameter
-- should generate traces with first the size and then the colour.
-- things get more complicated because some trace can generated 1 or to 2 params (or nothing)
-- depending on the configuration we group (or not) traces to be pair of size/colour
traceParamsForBubble :: ReportParam -> [[Maybe DataParam]] 
traceParamsForBubble param = 
  let q'tp'norms = [rpDataParam, rpDataParam2] <*> [param]
      -- expand (DataParams qtype tps norm) = [Just (DataParam qtype tp norm) | tp <- tps]
      expand dps = map Just $ dataParamsToDataParam0s dps
      expanded = case q'tp'norms of
            -- if the first param is null and the second is only one, then 2nd is the colour
            ( (DataParamsU _ [] _) : q@(DataParamsU qtype [tparam] norm) : others) -> (Nothing: expand q) : map expand others
            ( q@(DataParamsU _ [_] _) : q'@(DataParamsU _ [_] _) : others) -> (expand q <>  expand q') : map expand others
            _ -> map expand q'tp'norms
  in filter (not . null) expanded
  
seriesBubbleProcessor :: NMap (Sum Double, TranQP) -> NMap (Sum Double, TranQP)
  -> ColumnRupture -> Bool -> [[Maybe DataParam]]-> Text -> Text -> NMap (Sum Double, TranQP)  -> Widget 
seriesBubbleProcessor all panel rupture mono paramss name plotId grouped = do
     let asList = nmapToNMapListWithRank grouped
         jsDatas = map (bubbleTrace all panel grouped asList) paramss
     toWidgetBody [julius|
          Plotly.plot( #{toJSON plotId}
                    , #{toJSON jsDatas}
                    , { margin: { t: 30 }
                      , title: #{toJSON name}
                      , yaxis2 : {overlaying: 'y', title: "Quantities", side: "right"}
                      , yaxis3 : {overlaying: 'y', title: "Amount(T)", side: "right"}
                      , yaxis4 : {overlaying: 'y', title: "Quantities(T)", side: "left"}
                      , yaxis5 : {overlaying: 'y', title: "Price"}
                      }
                    );
                |]
-- | Generate a plot trace for bubble graph
-- bubbleTrace :: [((Int, NMapKey), NMap (Sum Double, TranQP))]
--             -> [Maybe DataParam]
--             -> Value
bubbleTrace all panel band asList params =  
    let (getSize'p : getColour'p :  _) = (map (fmap $ fanl dataParamGetter) params) <> repeat Nothing
        runSumFor getFn'p grp =
          case getFn'p of
            Just (fn, DataParam _ tp normMode) ->
                let runsumed = nmapRunSum (tpRunSum tp) $ grp
                in formatSerieValues id id normMode all panel band fn runsumed 
            _ -> replicate (length grp) Nothing
        (xs, ys, vs, texts, colours, symbols) = unzip6 [  (x, y, abs <$> v, text, colour, symbol)
                              | ((name,group), n) <- zip asList  [1..]
                              , let gForSize = runSumFor getSize'p group
                              , let gForColor = runSumFor getColour'p group
                              , ((k,_), v, colour) <- zip3 (nmapToNMapList group) gForSize gForColor
                              , let x = pvToText $ nkKey k --  :: Int --  # of the serie
                              , let y = nkeyWithRank $ name --  n :: Int --  # of the serie
                              -- , let v =  (fst <$> getSize'p) >>= ($ gsm)  :: Maybe Double -- # of  for the colun
                              , let text = fmap (\vv -> ( x <> " " <> tshow vv)) v
                              -- , let colour = (fst <$> getColour'p) >>= ($ gcm)
                              , let symbol = if maybe False (<0) v then t "diamond" else "circle"
                              ]
        rgb :: (Double, Double, Double) -> Text
        rgb (r, g, b) = pack $ printf "rgb(%d,%d,%d)" (round r :: Int) (round g :: Int) (round b :: Int)
        palette rgbs = toJSON $ zip ix (map toJSON rgbs) where len = length rgbs 
                                                               ix = [ toJSON (fromIntegral i / fromIntegral (len-1) :: Double) | i <- [0..len-1] ]
        gradient3 (r0,g0,b0) (r1,g1,b1) n = zip3 (gradient r0 r1 n) (gradient g0 g1 n) (gradient b0 b1 n)
        gradient :: Double -> Double  -> Int -> [Double]
        gradient a b n = [ a + slope*fromIntegral i | i <- [0..n-1]] where slope = (b - a) / fromIntegral (n -1)
        
        jsData = object [ "x"  .=  xs
                        , "y" .= ys
                        , "text" .= texts
                        , "mode" .= t "markers"
                        , "marker" .= object ( case getSize'p of
                                                Nothing -> [ "size" .= t "40"]
                                                Just (_, p) -> [ "size" .= vs
                                                               , "sizemin" .= t "1"
                                                               , "symbol" .= symbols
                                                               ] <> let diameter = [ "sizemode" .= t "diameter"
                                                                                   , "sizeref" .= case catMaybes vs of
                                                                                       [] -> 1
                                                                                       vss -> 2*(maximumEx vss) / 40
                                                                                   ]
                                                                        area = [ "sizemode" .= t "area"
                                                                               , "sizeref" .= case catMaybes vs of
                                                                                                [] -> 1
                                                                                                vss -> 2*(maximumEx vss) / (40 * 40)
                                                                               ]
                                                                    in case tpValueType (dpDataTraceParam p) of
                                                                             VAmount -> area
                                                                             VQuantity -> area
                                                                             _ -> diameter
                                            <> case getColour'p of
                                                  Nothing -> ["colorscale" .= t "Greens"]
                                                  Just (_,p) -> ["color" .= colours] <> case catMaybes colours of
                                                    [] -> []
                                                    css -> let mx = maximumEx (map abs css)
                                                               (cmin, cscale) =  if minimumEx css < 0
                                                                       then (-mx, palette . map rgb $ gradient3 (150,0,0) (255,200,255) 10 ++ drop 1 ( gradient3 (255,255,200) (0, 150, 0) 10 ) ) 
                                                                       -- then (-mx, toJSON $  [[toJSON $ n/21, String $ "rgb(255,"<> tshow i <> "," <> tshow i <> ")"]  | n <- [0..10 :: Double] , let i = round $ 255*(n/10)]
                                                                       --      <> [[toJSON $ ( n+11)/21, String $ "rgb("<> tshow i <> ",255," <> tshow i <> ")"] | n <- [1..10 :: Double] , let i = round $ 255*(1-n/10)]
                                                                       --      )
                                                                       else (0, palette . map rgb $ gradient3 (255,200,255) (0,150,0) 20 )
                                                           in ["cauto" .= False
                                                              ,"cmin" .= cmin
                                                              ,"zmin" .= cmin
                                                              , "cmax" .= mx
                                                              , "zmax" .= mx
                                                              , "showscale" .= True
                                                              , "colorscale" .= cscale
                                                              ]
                                             )
                                        
                        ]
    in jsData

-- ** Scatter
  -- | Like bubble map but uses 2 measures instead of 2 categories
scatterProcessor :: ReportParam -> NMap (Sum Double, TranQP) -> Widget 
scatterProcessor param grouped = do
  renderPanelWith  "items-report-scatter" grouped (renderScatterPlotDiv param $ \n -> max 350 (900 `div` n))
        
  
renderScatterPlotDiv :: ReportParam -> (Int -> Int ) -> NMap (Sum Double, TranQP) -> Text -> NMap (Sum Double, TranQP) -> Widget 
renderScatterPlotDiv param heightForBands all plotId0 panels = do
  let plotSeries bandName plotId bands =
        seriesScatterProcessor all
                              panels
                              (rpSerie param)
                              (isNothing $ cpColumn $ rpSerie param)
                              [map Just $ rpDataParam0s param]
                              bandName plotId bands
  renderPlotDiv plotSeries heightForBands plotId0 panels

seriesScatterProcessor :: NMap (Sum Double, TranQP) -> NMap (Sum Double, TranQP)
  -> ColumnRupture -> Bool -> [[Maybe DataParam]]-> Text -> Text -> NMap (Sum Double, TranQP)  -> Widget 
seriesScatterProcessor all panel rupture mono paramss name plotId grouped = do
     let asList = nmapToNMapListWithRank grouped
         jsDatas = map (scatterTrace all panel grouped asList) paramss
     toWidgetBody [julius|
          Plotly.plot( #{toJSON plotId}
                    , #{toJSON jsDatas}
                    , { margin: { t: 30 }
                      , title: #{toJSON name}
                      , yaxis2 : {overlaying: 'y', title: "Quantities", side: "right"}
                      , yaxis3 : {overlaying: 'y', title: "Amount(T)", side: "right"}
                      , yaxis4 : {overlaying: 'y', title: "Quantities(T)", side: "left"}
                      , yaxis5 : {overlaying: 'y', title: "Price"}
                      }
                    );
                |]
-- | Generate a plot trace for scatter graph
scatterTrace :: NMap (Sum Double, TranQP)
             -> NMap (Sum Double, TranQP)
             -> NMap (Sum Double, TranQP)
             ->   [((Int, NMapKey), NMap (Sum Double, TranQP))]
            -> [Maybe DataParam]
            -> Value
scatterTrace all panel band asList params =  
    let (getX'p : getY'p : getSize'p:  _) = (map (fmap $ fanl dataParamGetter) params) <> repeat Nothing
        runSumFor getFn'p grp =
          case getFn'p of
            Just (fn, DataParam _ tp normMode) ->
                let runsumed = nmapRunSum (tpRunSum tp) $ grp
                in formatSerieValues id id normMode all panel band fn runsumed 
            _ -> replicate (length grp) Nothing
        (xs, ys, vs, texts, colours, symbols) = unzip6
                              [  (x, y, abs <$> v, text, colour, symbol)
                              | ((name,group), colour) <- zip asList  defaultColors
                              , let gForSize = runSumFor getSize'p group
                              , let gForX = runSumFor getX'p group
                              , let gForY = runSumFor getY'p group
                              , ((k,_), x, y, v) <- zip4 (nmapToNMapList group) gForX gForY gForSize
                              -- , let v =  (fst <$> getSize'p) >>= ($ gsm)  :: Maybe Double -- # of  for the colun
                              -- , let text = pvToText . nkKey $ snd name --  fmap (\vv -> (( pvToText . nkKey $ snd name ) <> " " <> tshow vv)) v
                              , let text = intercalate " - " . filter (not . null) $ map (pvToText . nkKey)   [snd name, k]
                              -- , let colour = (fst <$> getColour'p) >>= ($ gcm)
                              , let symbol = if maybe False (<0) v then t "diamond" else "circle"
                              ]
        jsData = object [ "x"  .=  xs
                        , "y" .= ys
                        , "name" .= t "pipo"
                        , "text" .= texts
                        , "mode" .= t "markers"
                        , "marker" .= object ( case getSize'p of
                                                Nothing -> [ "size" .= t "40"]
                                                Just (_, p) -> [ "size" .= vs
                                                               , "sizemin" .= t "1"
                                                               , "symbol" .= symbols
                                                               ] <> let diameter = [ "sizemode" .= t "diameter"
                                                                                   , "sizeref" .= case catMaybes vs of
                                                                                       [] -> 1
                                                                                       vss -> 2*(maximumEx vss) / 40
                                                                                   ]
                                                                        area = [ "sizemode" .= t "area"
                                                                               , "sizeref" .= case catMaybes vs of
                                                                                                [] -> 1
                                                                                                vss -> 2*(maximumEx vss) / (40 * 40)
                                                                               ]
                                                                    in case tpValueType (dpDataTraceParam p) of
                                                                             VAmount -> area
                                                                             VQuantity -> area
                                                                             _ -> diameter
                                            <> ["color" .= colours]
                                             )
                                        
                        ]
    in jsData
-- ** Pivot
pivotProcessor:: [DataParams] -> _ColumnRuptures -> NMap TranQP -> Widget
pivotProcessor tparams =  do
  processRupturesWith (panelPivotProcessor tparams "items-report-pivot") ()
  
-- each nmap is a panel
panelPivotProcessor :: [DataParams] -> Text -> NMapKey -> Int -> _ -> _ -> NMap TranQP ->  Widget
panelPivotProcessor tparams reportId = createKeyRankProcessor go where
  go key rank = let panelName = nkeyWithRank (rank, key)
                    panelId = reportId <> "-panel-" <> panelName
                    sub = bandPivotProcessor tparams panelId
                    widget children = [whamlet|
                               <div.panel.panel-info>
                                 <div.panel-heading data-toggle="collapse" data-target="#{panelId}">
                                   <h2>#{panelName}
                                 <div.panel-body.collapse.in id="#{panelId}" style="max-height:2000px; overflow:auto">
                                   ^{children}
                       |]
                    in (sub, widget)
  

-- each nmap is band
bandPivotProcessor tparams panelId key rank parents ruptures = createKeyRankProcessor go key rank parents ruptures where
  (_, (ColumnRupture{..},_)) = ruptures
  go key rank =  let sub = collectColumnsForPivot tparams
                          -- get the list of the columns for that we need to
                          -- sort and limit each serie first by calling processRupturesWith
                     widget cw =  let
                          (cols, widgetFs) = unzip cw
                          -- colmns comes with a weight, we need to add up those weight sort
                          -- the keys according to it
                          columnMap = Map.fromListWith (<>) $ join cols
                          sorted' = map fst (sortOn snd $ mapToList columnMap)
                          sorted = if cpReverse then reverse sorted' else sorted'
                          columns = zip [1..] $ sorted
                          in [whamlet|
                              <div>
                                <h3> #{nkeyWithRank (rank, key)}
                                <table.table.table-border.table-hover.table-striped>
                                  <thead>
                                    <tr>
                                      <th>
                                      $forall column <- columns
                                        <th.text90>#{nkeyWithRank column}
                                    <tbody>
                                      $forall wF <- widgetFs
                                        ^{wF columns }
                                        |]
                     in (sub, widget)

-- get the list of the columns for that we need to
 -- nmap is a serie
collectColumnsForPivot :: [DataParams] -> NMapKey -> Int -> _parents -> _ruptures -> NMap TranQP -> [(_, _ -> Widget)]
collectColumnsForPivot tparams key rank parents ruptures@(r, ()) nmap = let
  (band, (panel, (all, ()))) = parents
  -- we are within a serie, we need to get all the used columns
  -- form nmap and return them upstream
  columns = processRupturesWith (\k _ _mar _  n -> [(k, cpSorter r k (nmapMargin n) )]) parents ruptures nmap
  -- columns = processRupturesWith (\k _ _mar _  n -> [(k, [key])]) parents ruptures nmap
  formatPercent tp mode = case nmMargin <$>  mode of
    Just NMRank -> \d -> let rank  = floor d
                             isTop = rank == 1
                             isTop2 = rank == 2
                          in [shamlet|<span :isTop:.topOne :isTop2:.topTwo>#{sformat ords rank}|]

    _ -> formatDouble' tp {tpValueType = VPercentage}
  traces = [formatSerieValuesNMap (formatDouble' tp)
                                  (formatPercent tp normMode)
                                  normMode all panel band
                                  (fmap (tpValueGetter tp) . lookupGrouped qtype) (nmapRunSum (tpRunSum tp) nmap)
           | DataParams qtype tps normMode <- tparams
           , tp <- getIdentified tps
           ]
  widget col's = [whamlet|
   <tr>
     <td>#{nkeyWithRank (rank, key)}
     $forall column <- col's
        <td>
          $forall trace <- traces
            <div.just-right>#{fromMaybe "-" $ lookup (snd column) trace}
                        |]
  in [(columns, widget )]


  
pivotProcessorXXX :: ReportParam -> NMap (Sum Double, TranQP) -> Widget 
pivotProcessorXXX param grouped = do
  renderPanelWith  "items-report-pivot" grouped (panelPivotProcessorXXX param)

panelPivotProcessorXXX :: ReportParam -> NMap (Sum Double, TranQP) -> Text -> NMap (Sum Double, TranQP) -> Widget 
panelPivotProcessorXXX param all plotId0 grouped = do
  let asList = nmapToNMapListWithRank grouped
  forM_ (zip asList [1:: Int ..]) $ \((bandName, bands), i) ->
        do
          let -- byColumn = nmapToNMapList grouped -- fmap (groupAsMap (mkGrouper param (Just $ rpColumnRupture param) . fst) snd) (unNMap TranQP' bands)
              plot = bandPivotProcessorXXX all grouped (rpSerie param) (isNothing $ cpColumn $ rpSerie param) (rpDataParam0s param) (nkeyWithRank bandName) plotId bands 
              plotId = plotId0 <> "-" <> tshow i
          [whamlet|
            <div.negative-bad id=#{plotId}>
                ^{plot}
                  |]
-- | Display an html table (pivot) for each series
bandPivotProcessorXXX :: NMap (Sum Double, TranQP) -> NMap (Sum Double, TranQP)
  -> ColumnRupture -> Bool -> [DataParam]-> Text -> Text -> NMap (Sum Double, TranQP)  -> Widget 
bandPivotProcessorXXX all panel rupture mono params name plotId grouped = let
  name'serieS = nmapToNMapListWithRank grouped
  -- it's a set but should be sorted by rank
  -- The same columns can appears in different series with a different rank
  -- we need to use the weigh and add them up
  columnMap :: Map NMapKey (Sum Double)
  columnMap = Map.fromListWith (<>) [ (colName, fst (nmapMargin nmap))
                          | (_, serie) <- name'serieS
                          , (colName, nmap) <- nmapToNMapList serie
                          ]
  columns = zip [1..] (keys columnMap)
  -- lookupValue :: QPType -> NMap TranQP -> (QPrice -> Double) -> NMapKey -> Maybe Double
  -- lookupValue qtype serie0 valueFn column = do --
  --   tranMap <- lookup column (nmapToMap serie)
  --   let tran = nmapMargin tranMap
  --   qprice <- lookupGrouped qtype tran
  --   return (valueFn qprice)
    -- Maybe valueFn `fmap` ((lookup column (nmapToMap serie) <&> nmapMargin) >>= lookupGrouped qtype) -- <&> valueFn
  formatPercent tp mode = case nmMargin <$>  mode of
    Just NMRank -> \d -> let rank  = floor d
                             isTop = rank == 1
                             isTop2 = rank == 2
                          in [shamlet|<span :isTop:.topOne :isTop2:.topTwo>#{sformat ords rank}|]

    _ -> formatDouble' tp {tpValueType = VPercentage}
  in [whamlet|
       <div>
         <h3> #{name}
         <table.table.table-border.table-hover.table-striped>
           <thead>
             <tr>
               <th> #{fromMaybe "" $ fmap colName (cpColumn rupture)}
               $forall column <- columns
                 <th.text90>#{nkeyWithRank column}
             <tbody>
               $forall (serieName, serie0)  <- name'serieS
                 <tr>
                   <td> #{nkeyWithRank serieName}
                     $forall column <- columns
                       <td>
                        $forall (DataParam qtype tp normMode ) <- params
                          $with serie <- formatSerieValuesNMapXXX (formatDouble' tp) (formatPercent tp normMode) normMode all panel grouped (fmap (tpValueGetter tp) . lookupGrouped qtype) (nmapRunSum (tpRunSum tp) serie0)
                            <div.just-right>#{fromMaybe "-" $ lookup (snd column) serie}
             |]

formatDouble' :: TraceParam -> Double -> Html
formatDouble' tp = formatDouble'' (tpRunSum tp) (tpValueType tp) 
formatDouble'' runsum vtype x = let
  s :: Text
  s = case vtype of
    VAmount -> sformat ("£" % commasFixed) x -- TODO create builder
    VQuantity -> sformat commasFixed' x
    VPercentage -> sformat (fixed 1 % "%") x
    VPrice -> sformat ("£" % fixed 2) x
  
  classes = posneg : tshow vtype : tshow runsum : [] :: [Text]
  posneg = case compare x  0 of
                LT -> "negative"
                EQ -> ""
                GT -> "positive"
  in  [shamlet|
         <span class="#{intercalate " " classes}">
              #{s}|]
        


formatAmount = formatDouble''  RSNormal VAmount
formatQuantity = formatDouble''  RSNormal VQuantity
formatPrice = formatDouble''  RSNormal VPrice
formatPercentage = formatDouble''  RSNormal VPercentage
      
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

  
