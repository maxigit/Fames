{-# LANGUAGE OverloadedLabels, OverloadedRecordDot, TypeOperators #-}
module Handler.Items.Reports.Yearly (
getItemsReportYearlyR
)
where

import Import hiding(all)
import Handler.Items.Reports.Sources
import Handler.Items.Reports.Common
import Handler.Items.Reports.Plot
import qualified Database.Esqueleto.Experimental as E
-- import Database.Esqueleto.Experimental((^.))
import FA
import qualified Data.Conduit.List as C
import qualified Data.NoDF as N
import Data.NoDF.Operators
import qualified Data.Foldable as F
import qualified Data.Vector.Sized as N
import GL.Utils
import Data.Time (diffDays, pattern YearMonthDay)
import Data.Aeson.QQ(aesonQQ)

getItemsReportYearlyR :: Handler Html
getItemsReportYearlyR = do
  today <- todayH
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  -- settings <- getsYesod appSettings
  -- ((resp, form), encType) <- runFormGet $ yearlyForm 
  let param = -- case resp of
              --    FormSuccess p -> p
              defaultReportParam today Nothing

  -- select everything from the beginning of time grouped by day
  let query = do
               tables <- itemSalesQuery stockLike param
               let trans = E.getTable @DebtorTran tables
               E.groupBy trans.tranDate
               E.orderBy [ E.asc trans.tranDate ]
               return (trans.tranDate, E.sum_ (salesDetailAmount param tables))
  salesvs <- runDB $ runConduit $ E.selectSource query
                              .| C.mapMaybe (\(E.Value day, E.Value amountm) -> fmap (day,) amountm)
                              .| conduitVector 1000
                              .| sinkList
  let sales = mconcat salesvs :: Vector (Day, Double)
      plots = yearlyTrendPlots today sales
  defaultLayout plots


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
        plotWidget [ [aesonQQ| { hovermode: "y unified" } |] ]
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

   

