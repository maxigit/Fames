module Handler.Dashboard
( getDMainR 
, getDCustomR
)
where

import Import
import Handler.Items.Reports.Common
import Items.Types
import GL.Utils
import GL.Payroll.Settings

-- Display a dashboard made of different report depending on the configuration file
getDMainR :: Handler Html
getDMainR = defaultLayout $ do
  addScriptRemote "https://cdn.plot.ly/plotly-latest.min.js"
  toWidgetHead commonCss
  toWidgetHead [cassius|
div#pivot-Top-100-1
  th
    writing-mode: lr
  td div
    display: inline
                       |]
  [whamlet|
<div.panel.panel-primary>
  <div.panel-heading data-toggle=collapse data-target="#dashboard-panel-1">
    <h2> Top 100 Items (Beginning of Year)
  <div.panel-body id=dashboard-panel-1>
    <div#test-include-1 style="max-height:400px; overflow:auto">
<div.panel.panel-primary>
  <div.panel-heading data-toggle=collapse data-target="#dashboard-panel-1">
    <h2> Top 100 Items (Beginning of Year)
  <div.panel-body id=dashboard-panel-2>
    <div#test-include-2 style="max-height:400px; overflow:auto">
<div.panel.panel-primary>
  <div.panel-heading data-toggle=collapse data-target="#dashboard-panel-1">
    <h2> Top 100 Items (Beginning of Year)
  <div.panel-body id=dashboard-panel-3>
    <div#test-include-3 style="max-height:400px; overflow:auto">
|]
  toWidgetBody [julius|
                      $("#test-include-1").load("@{DashboardR (DCustomR "top100ItemYear" 800 400)}")
                      $("#test-include-2").load("@{DashboardR (DCustomR "top100StyleYear" 800 400)}")
                      $("#test-include-3").load("@{DashboardR (DCustomR "top100ColourYear" 800 400)}")
                      |]

-- Run report by name (find in configuration file)
getDCustomR :: Text -> Int64 -> Int64 -> Handler Html
getDCustomR reportName width height = do
  role <- currentRole
  when (not $ authorizeFromAttributes role (setFromList [reportName]) ReadRequest)
       (permissionDenied reportName)
     

  let reportMaker = case reportName of
        "top100ItemYearChart" -> top100ItemYearChart "top1"
        "top100ItemYearChart2" -> top100ItemYearChart "top2"
        "top100ItemYear" -> top100ItemYear skuColumn
        "top100StyleYear" -> top100ItemYear styleColumn
        "top100ColourYear" -> top100ItemYear variationColumn
        _ -> error "undefined report"
  widget <- reportMaker
  p <- widgetToPageContent widget
  withUrlRenderer [hamlet|^{pageBody p}|]





-- | Top style
top100ItemYear rupture = do
  today <- utctDay <$> liftIO getCurrentTime
  let tomorrow = calculateDate (AddDays 1) today
      beginYear = fromGregorian (currentYear) 1 1
      currentYear = toYear today
  let param = ReportParam{..}
      rpToday = today
      rpFrom = Just beginYear
      rpTo = Just tomorrow
      rpPeriod' = Just PFSlidingYearFrom
      rpNumberOfPeriods = Just 2
      rpCategoryToFilter = Nothing
      rpCategoryFilter = Nothing
      rpStockFilter = Nothing
      rpPanelRupture = emptyRupture
      rpBand = emptyRupture
      rpSerie = ColumnRupture (Just rupture) bestSalesTrace Nothing (Just 100) False
      rpColumnRupture = periodColumn
      rpTraceParam = TraceParams QPSales (mkIdentifialParam amountOutOption) (Just $ NormalizeMode NMColumn NMSerie )
      rpTraceParam2 = TraceParams QPSales (mkIdentifialParam amountOutOption) (Just $ NormalizeMode NMRank NMBand )
      rpTraceParam3 = emptyTrace
      rpLoadSales = True
      rpLoadPurchases = False
      rpLoadAdjustment = False
      -- TODO factorize
      grouper = [ rpBand, rpSerie
                , ColumnRupture  (Just rpColumnRupture) (TraceParams QPSummary (Identifiable ("Column", [])) Nothing) Nothing Nothing True
                ]
  -- report <- itemReport param grouper (pivotProcessor param)
  report <- itemReport param grouper (\nmap -> panelPivotProcessor nmap param "pivot-Top-100" nmap)
  return $ report
      

top100ItemYearChart plotName = do
  today <- utctDay <$> liftIO getCurrentTime
  let tomorrow = calculateDate (AddDays 1) today
      beginYear = fromGregorian (currentYear) 1 1
      currentYear = toYear today
  let param = ReportParam{..}
      rpToday = today
      rpFrom = Just beginYear
      rpTo = Just tomorrow
      rpPeriod' = Nothing
      rpNumberOfPeriods = Nothing
      rpCategoryToFilter = Nothing
      rpCategoryFilter = Nothing
      rpStockFilter = Nothing
      rpPanelRupture = emptyRupture
      rpBand = emptyRupture
      rpSerie = ColumnRupture (Just skuColumn) bestSalesTrace Nothing (Just 100) False
      rpColumnRupture = monthlyColumn
      rpTraceParam = TraceParams QPSales (mkIdentifialParam amountOutOption) Nothing 
      rpTraceParam2 = emptyTrace
      rpTraceParam3 = emptyTrace
      rpLoadSales = True
      rpLoadPurchases = False
      rpLoadAdjustment = False
      -- TODO factorize
      grouper = [ -- rpPanelRupture,
                  rpBand, rpSerie
                , ColumnRupture  (Just rpColumnRupture) (TraceParams QPSummary (Identifiable ("Column", [])) Nothing) Nothing Nothing True
                ]
  report <- itemReport param grouper (\nmap -> panelChartProcessor nmap param plotName nmap)
  return $ report
