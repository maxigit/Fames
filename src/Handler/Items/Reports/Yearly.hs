{-# LANGUAGE OverloadedLabels, OverloadedRecordDot, TypeOperators #-}
module Handler.Items.Reports.Yearly (
getItemsReportYearlyR
)
where

import Import hiding(all)
import Handler.Items.Reports.Sources
import Handler.Items.Reports.Common
import Handler.Items.Reports.Types
import Handler.Items.Reports.Plot
import qualified Database.Esqueleto.Experimental as E
-- import Database.Esqueleto.Experimental((^.))
import FA
import qualified Data.Conduit.List as C
import qualified Data.NoDF as N
import Data.NoDF.Operators
import qualified Data.Foldable as F
import Data.List(scanl1)
import qualified Data.Vector.Sized as N
import GL.Utils
import Data.Time (diffDays, pattern YearMonthDay)
import Data.Aeson.QQ(aesonQQ)
import Yesod.Form.Bootstrap3 (renderBootstrap3, BootstrapFormLayout(..))

data YearlyParam = YearlyParam
    { ypStockFilter :: Maybe FilterExpression
    , ypFacetCategory :: Maybe Text
    , ypCategoryToFilter :: Maybe Text
    , ypCategoryFilter :: Maybe FilterExpression
    , ypUseQuantity :: Bool
    }
    deriving (Show)

defaultYearlyParam = YearlyParam Nothing Nothing Nothing Nothing False

yearlyForm categories paramM = renderBootstrap3 BootstrapBasicForm form where
  form = let categoryOptions = [(cat, cat) | cat <-categories ]
         in YearlyParam 
                    <$> aopt filterEField "sku" (Just $ ypStockFilter =<< paramM)
                    <*> aopt (selectFieldList categoryOptions) "facet" (Just $ ypFacetCategory =<< paramM)
                    <*> aopt (selectFieldList categoryOptions) "to filter" (Just $ ypFacetCategory =<< paramM)
                    <*> aopt filterEField "category" (Just  $ ypCategoryFilter =<< paramM )
                    <*> areq boolField "use quantity" (Just $ fmap ypUseQuantity paramM == Just True)

getItemsReportYearlyR :: Handler Html
getItemsReportYearlyR = do
  today <- todayH
  rpDeduceTax <- appReportDeduceTax <$> getsYesod appSettings 
  categories <- categoriesH
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  -- settings <- getsYesod appSettings
  ((resp, form), encType) <- runFormGet $ yearlyForm categories Nothing
  let yparam = case resp of
                 FormSuccess yparam -> yparam
                 _  -> defaultYearlyParam
  let param = (defaultReportParam today Nothing rpDeduceTax) { rpSkuFilter = ypStockFilter yparam
                                                 , rpCategoryFilter = ypCategoryFilter yparam
                                                 , rpCategoryToFilter = ypCategoryToFilter yparam <|> ypFacetCategory yparam
                                                 } 
           



  -- select everything from the beginning of time grouped by day
  let query = do
               tables <- itemSalesQuery stockLike param
               let trans = E.getTable @DebtorTran tables
               E.groupBy trans.tranDate
               E.orderBy [ E.asc trans.tranDate ]
               let y = yFromTables  (ypUseQuantity yparam) param tables
               return (trans.tranDate, E.sum_ y)
  salesvs <- runDB $ runConduit $ E.selectSource query
                              .| C.mapMaybe (\(E.Value day, E.Value amountm) -> fmap (day,) amountm)
                              .| conduitVector 1000
                              .| sinkList
  let sales = mconcat salesvs :: Vector (Day, Double)
      plots = yearlyTrendPlots today sales
  plot2 <- plot2H param (ypUseQuantity yparam) (fromMaybe "forecast-profile" $ ypFacetCategory yparam)
  defaultLayout do
     [whamlet|
     <div.well>
       <form.form.form-inline role=form method=GET enctype=#{encType}>
         ^{form}
         <button.btn.btn-default type=submit>Submit
     <div.well>
       ^{plots}
     <div.well>
       ^{plot2}
     |]


yFromTables useQty param tables = if useQty
                            then salesDetailQuantity tables
                            else salesDetailAmount param tables

yearlyTrendPlots :: Day -> Vector (Day, Double) -> Widget
yearlyTrendPlots today sales
   | N.SomeSized sales__n <- sales
   , N.Z2 days__n y__n <- sales__n
   , l <- N.length days__n 
   , l > 0
   , N.SomeSized days__all <- fromList [days__n `N.unsafeIndex` 0 .. days__n `N.unsafeIndex` (l-1) ]
   , N.JoinV aJjAA aJjNN   <- N.joinV days__all days__n
   , days__j <- N.walues aJjAA @=> days__all
   , y__j <- F.sum <$> N.walues aJjNN @>$ y__n
   , runningY__j <- N.postscanl' (+) 0 y__j 
   , maYear__j <- runningY__j - ago (AddYears $ -1) 0 days__j runningY__j
   , maQuaterly__j <- runningY__j - ago (AddMonths $ -3) 0 days__j runningY__j
   , let toY v__j = toXY (N.Z2 days__j v__j) 
   , N.SomeSized sales__fiscalYear <- sampleYearly (fromGregorian 2026 04 30) days__j maYear__j
   , N.SomeSized sales__endYear <- sampleYearly (fromGregorian 2025 12 31) days__j maYear__j
   , N.SomeSized sales__toToday <- sampleYearly today days__j maYear__j
   , let marker = [aesonQQ| { mode: "markers" } |] -- , marker: { symbol: "square", size: 12 } } |]
   = do
        [whamlet|<h2> Trend over the years |]
        plotWidget [ [aesonQQ| { hovermode: "closest" } |] ]
                   Nothing [ [ toY maYear__j , traceName "Yearly" ]
                           , [ toY maQuaterly__j , traceName "Quaterly" ]
                           , [ toXY sales__fiscalYear , marker , traceName "Fiscal Year" ]
                           , [ toXY sales__endYear , marker , traceName "End Of Year" ]
                           , [ toXY sales__toToday , marker , traceName "To Today" ]
                           ]

yearlyTrendPlots _ _sales  = error "empty sales"

ago :: N.KnownNat n => DateCalculator -> x -> N.Vector n Day -> N.Vector n x -> N.Vector n x 
ago calc x0 days__n x__n = N.generate go
   where go i = let d = days__n `N.index` i
                    previousYear  = calculateDate calc d
                    numberOfDay = fromInteger $ diffDays d previousYear -- can be 365 or 366 for leapyear
                in if i > numberOfDay
                   then x__n `N.index` ( i - numberOfDay )
                   else x0
sampleYearly :: N.KnownNat n => Day -> N.Vector n Day -> N.Vector n x -> Vector (Day, x)
sampleYearly (YearMonthDay _ month day) days__n y__n
    | N.Wix dNnDD <- N.filterX (\(YearMonthDay _ m d) -> m == month && d == day) days__n
    = N.fromSized $ dNnDD @~> N.Z2 days__n y__n

   


-- | facet by category
plot2H param useQty catName = do
  today <- todayH
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  -- settings <- getsYesod appSettings
  -- ((resp, form), encType) <- runFormGet $ yearlyForm 

  -- select everything from the beginning of time grouped by day
  let query = do
               tables <- E.from ( itemSalesQuery stockLike param
                         `E.innerJoin` E.table @ItemCategory
                         `E.on` \((E.getTable @DebtorTransDetail -> detail) E.:& category)
                                 -> category.category E.==. E.val catName
                                    E.&&. category.stockId E.==. detail.stockId 
                                    )
                                 
               let trans = E.getTable @DebtorTran tables
                   category = E.getTable @ItemCategory tables
               E.groupBy trans.tranDate
               E.groupBy category.value
               E.orderBy [ E.asc trans.tranDate ]
               return (trans.tranDate, category.value, E.sum_ (yFromTables useQty param tables))
  salesvs <- runDB $ runConduit $ E.selectSource query
                              .| C.mapMaybe (\(E.Value day, E.Value cat, E.Value amountm) -> fmap (day, cat,) amountm)
                              .| conduitVector 1000
                              .| sinkList
  let sales = mconcat salesvs :: Vector (Day, Text, Double)
  return $ yearlyFacetsPlot today catName sales

_endOfWeek :: Day -> Day
_endOfWeek = calculateDate (EndOfWeek Sunday) 
    
yearlyFacetsPlot :: Day -> Text -> Vector (Day, Text, Double) -> Widget
yearlyFacetsPlot _today catname sales
    | N.SomeSized sales__n <- sales
    , N.Z3 days__n cat__n y__n <- sales__n
    -- group per week to make plotly
    -- , N.PivV nDdNN colTodN <- N.pivotV (min today . endOfWeek <$> days__n) cat__n
    , N.PivV nDdNN colTodN <- N.pivotV days__n cat__n
    , l <- N.length days__n 
    , l > 0
    , N.SomeSized days__all <- fromList [days__n `N.unsafeIndex` 0 .. days__n `N.unsafeIndex` (l-1) ]
    , N.JoinV aJjAA aJjNN   <- N.joinV days__all days__n
    , allY__j <- F.sum <$> N.walues aJjNN @>$ y__n
    , runningAll__j <- N.postscanl' (+) 0 allY__j
    , days__j <- N.walues aJjAA @=> days__all
    , maAll__j <- runningAll__j - ago (AddYears $ -1) 0 days__j runningAll__j
    = do
       -- let traces =  zipWith go (mapToList colToMaYear__j) [0..]
       let traces =  zipWith3 go (mapToList colToMaYear__j) stackeds__j [0..]
           fillcolor i = defaultColor i <> "1A"
           normal i = [aesonQQ| { line: { color: #{defaultColor i}, width: 1 }
                                , fill: "tozeroy"                     
                                , fillcolor: #{fillcolor i}
                                , legendgroup: #{i}
                                } |]
           stacked i = [aesonQQ| { line: { color: #{defaultColor i}, width: 1 }
                                , fill: "tonexty"                     
                                , legendgroup: #{i}
                                , showlegend: false
                                } |]
           colToMaYear__j = fmap (\dN -> if | y__d <- F.sum <$> dN @>$ y__n
                                            , y__j <- F.sum . take 1 <$> N.walues aJjNN @>$ N.windex nDdNN @>$ y__d  
                                            , runningY__j <- N.postscanl' (+) 0 y__j
                                            -> runningY__j - ago (AddYears $ -1) 0 days__j runningY__j
                                 )
                                 colTodN
           maYears__j = toList colToMaYear__j
           stackeds__j = scanl1 (+) maYears__j
           go (cat, maYear__j) stacked__j col
                        = [ [ toXY (N.fromSized $ N.Z2 days__j maYear__j), traceName cat, yaxis "y", normal col ] 
                          , [ toXY (N.fromSized $ N.Z2 days__j (100 * maYear__j / maAll__j)), traceName cat, yaxis "y2", normal col
                            , [aesonQQ| { showlegend: false} |]
                            ]
                          , [ toXY (N.fromSized $ N.Z2 days__j stacked__j), traceName cat, yaxis "y3", stacked col ] 
                          , [ toXY (N.fromSized $ N.Z2 days__j (100 * stacked__j /maAll__j)), traceName cat, yaxis "y4", stacked col ] 
                          ]
       [whamlet|<h2> #{catname} |]
       plotWidget [ [aesonQQ| { grid: {rows: 4, columns: 1, roworder: "top to bottom" }
                              , clickmode: "select"
                              } |]
                  ]
                  (Just 1600) $ concat traces
yearlyFacetsPlot _ _ _ = error "exhaustive pattern"
   
