module Handler.Items.Reports.Types
where

import Import hiding(computeCategory, formatAmount, formatQuantity, panel, trace, all)
import Items.Types
import Data.Kind(Type)
import GL.Utils(calculateDate, PeriodFolding(..), toYear) -- previousMonthStartingAt, previousWeekDay, nextWeekDay, nextMonthStartingAt)
import GL.Payroll.Settings

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
  , rpShowInactive :: Bool
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
  , rpPurchasesDateOffset :: Maybe Int -- ^ Days to add to purchase date
  , rpLoadPurchaseOrders  :: Maybe (OrderDateColumn, OrderQuantityMode)
  , rpLoadAdjustment :: Bool
  -- , rpLoadForecast :: Bool
  , rpForecast :: (Maybe FilePath, Maybe InOutward, Maybe Day)
  , rpColourMode :: ColourMode
  , rpTraceGroupMode :: Maybe TraceGroupMode
  , rpDateAlignment :: Maybe DateAlignmentMode
  }  deriving Show


data OrderDateColumn = OOrderDate | ODeliveryDate deriving (Eq, Show)
data SalesInfoMode = SSalesOnly | SSalesAndOrderInfo deriving (Eq, Show)
data OrderQuantityMode = OOrderedQuantity | OQuantityLeft deriving (Eq, Show)
data ColourMode = Panel'Band'Serie | Band'Colour'Serie | Panel'Colour'Serie | TraceColour deriving (Eq, Show, Bounded, Enum, Ord)
data TraceGroupMode = GroupSeries | GroupTraces | GroupParams deriving (Eq, Show, Bounded, Enum, Ord)
data DateAlignmentMode = AlignToStart | AlignToEnd deriving (Eq, Show, Bounded, Enum, Ord)

rpJustFrom, rpJustTo :: ReportParam -> Day
rpJustFrom ReportParam{..} = fromMaybe rpToday rpFrom
rpJustTo ReportParam{..} = fromMaybe rpToday rpTo
rpLoadForecast, rpLoadSales, rpLoadOrderInfo :: ReportParam -> Bool
rpLoadForecast = isJust . rpForecastDir
rpLoadSales = isJust . rpLoadSalesAndInfo
rpLoadOrderInfo = (== Just SSalesAndOrderInfo) . rpLoadSalesAndInfo
rpForecastDir :: ReportParam -> Maybe FilePath
rpForecastDir param = dir where (dir, _, _) = rpForecast param
rpForecastInOut :: ReportParam -> Maybe InOutward    
rpForecastInOut param = io where (_, io, _) = rpForecast param
rpForecastStart :: ReportParam -> Maybe Day
rpForecastStart param = start where (_, _, start) = rpForecast param

rpPeriod :: ReportParam -> Maybe PeriodFolding
rpPeriod rp | fromMaybe 0 (rpNumberOfPeriods rp) == 0 = Nothing
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
  
-- Type of value, used to format_ it accordingly
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

pattern DataParamsU :: forall (g :: Type -> Type).
                       QPType
                    -> g TraceParam
                    -> Maybe NormalizeMode
                    -> DataParams' Identifiable g
pattern DataParamsU qtype tps normMode <- DataParams qtype (Identifiable (_, tps)) normMode

type DataParam = DataParams' Identity Identity

pattern DataParam :: QPType
                  -> TraceParam
                  -> Maybe NormalizeMode
                  -> DataParams' Identity Identity
pattern DataParam qtype tp normMode = DataParams qtype (Identity (Identity tp)) normMode

deriving instance Show DataParams
deriving instance Show DataParam

dpDataTraceParam :: DataParam -> TraceParam
dpDataTraceParam = runIdentity . runIdentity . dpDataTraceParams

 

data PeriodFolding'
  = PFWholeYear
  | PFSlidingYearFrom
  | PFSlidingYearTomorrow
  | PFQuaterly
  | PFMonthly
  | PFWeekly
  deriving (Show, Eq)

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

data QPColumnFilter = QPOnly | QPMinMax | QPAvg | QPAll deriving (Eq, Show, Ord, Enum, Bounded)
