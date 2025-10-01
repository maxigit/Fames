{-# LANGUAGE NamedFieldPuns #-}
module Handler.Dashboard
( getDMainR 
, getDMainFullR
, getDCustomR
, getDYearR
, getDAllYearR
-- * Forecast
, getDForecastR
)
where

import Import hiding(all)
import Handler.Items.Reports.Common
import Handler.Items.Reports.NewForecast
import Items.Types
import GL.Utils
import qualified Data.Map as Map
import Formatting hiding(now)
import Data.Aeson.QQ(aesonQQ)
import Control.Monad.Fail (MonadFail(..))
import System.FilePath.Glob (glob)
import System.FilePath (takeBaseName)

pivotCss = [cassius|
  div.pivot-inline
    th
      writing-mode: lr
    td div
      display: inline
    .display10
      max-height: 410px
      overflow: hidden
      h3, thead
          display:  none
      h4
        text-align: center
      &:hover
        overflow:auto
    span.VQuantity::before
      content: "["
    span.VQuantity::after
      content: "]"
    span.VQuantity
      font-style: italic
      color: grey
                        |]
reportCss = [cassius|
   div.report-summary
     text-align: center
     font-size: small
     font-style: italic
     margin-bottom: 0.5em
|]
reportDiv :: Text -> Handler Widget
reportDiv reportId = do
  widgetE <- dispatchReport reportId 800 400
  case widgetE of
    Left err -> return [whamlet|
       <div id=#{reportId}>
         The report #{reportId} doesn't exists. Contact your administrator.
         <div> #{err}
                        |]
    Right (w,param) -> do
      let datesW = renderParamDates param
      return [whamlet|
                   <div id=#{reportId}>
                     <div>
                        ^{w}
                     <div.report-summary>
                        ^{datesW}
                   |]
                   
renderParamDates :: ReportParam -> Widget
renderParamDates param =
   mconcat $ intersperse [whamlet|&nbsp|] dateWs
   where dateRanges = paramToDateIntervals param
         dateWs = map (\(fromM, toM) -> [whamlet|\< #{showDateM fromM} - #{showDateM toM}>|]) $ sort dateRanges
         showDateM :: Maybe Day -> Text
         showDateM = maybe "" (formatTime0 "%d %b %Y")
-- Display a dashboard made of different report depending on the configuration file
{-# NOINLINE getDMainR #-}
getDMainR :: Handler Html
getDMainR = do
  now <- liftIO $ getCurrentTime
  -- refactor
  currentMonthPcent <- reportDiv "salesCurrentMonthP" 
  topStyleMonthPcent <- reportDiv "top20StyleMonth" 
  topColourMonthPcent <- reportDiv "top20ColourMonth" 
  topSkuMonthPcent <- reportDiv "top20ItemMonth" 
  topStyleJanuaryPcent <- reportDiv "top20StyleJanuary" 
  topColourJanuaryPcent <- reportDiv "top20ColourJanuary" 
  topSkuJanuaryPcent <- reportDiv "top20ItemJanuary" 

  cacheSeconds(3600*23)
  defaultLayout $ do
    addScriptRemote "https://cdn.plot.ly/plotly-latest.min.js"
    toWidgetHead commonCss
    toWidgetHead pivotCss
    toWidgetHead reportCss
    [whamlet|
  <div.panel_.panel_-primary>
    <div.panel_-heading data-toggle=collapse data-target="#dashboard-panel_-1">
      <h2> Monthly Sales
    <div.panel_-body.pivot-inline id=dashboard-panel_-1>
      <div.row>
        <div.col-md-12>
          ^{currentMonthPcent}
      <div.row>
        <div.col-md-4.display10 >
          <h4> Top Styles Monthly
          ^{topStyleMonthPcent}
        <div.col-md-4.display10>
          <h4> Top Colour Monthly
          ^{topColourMonthPcent}
        <div.col-md-4.display10>
          <h4> Top Item Monthly
          ^{topSkuMonthPcent}
  <div.panel_.panel_-primary>
    <div.panel_-heading data-toggle=collapse data-target="#dashboard-panel_-2">
      <h2> Since January
    <div.panel_-body.pivot-inline id=dashboard-panel_-2>
      <div.row>
        <div.col-md-4.display10 >
          <h4> Top Styles 
          ^{topStyleJanuaryPcent}
        <div.col-md-4.display10>
          <h4> Top Colour 
          ^{topColourJanuaryPcent}
        <div.col-md-4.display10>
          <h4> Top Item 
          ^{topSkuJanuaryPcent}
  <div.footer>
  <span.text-right.font-italic>
    Last update #{tshow now}
  |]
-- | Same as main but display qty and amount instead of percent
{-# NOINLINE getDMainFullR #-}
getDMainFullR :: Handler Html
getDMainFullR = do
  now <- liftIO $ getCurrentTime
  let reportDiv' :: Text -> Handler Widget
      reportDiv' reportId = do
        widgetE <- dispatchReport reportId 800 400
        case widgetE of
          Left err -> return [whamlet|
             <div id=#{reportId}>
               The report #{reportId} doesn't exists. Contact your administrator.
               <div> #{err}
                              |]
          Right (w,_) -> do
            return [whamlet|
                         <div id=#{reportId}>
                         ^{w}
                         |]

  -- refactor
  currentMonthFull <- reportDiv' "salesCurrentMonthFull" 
  topStyleMonthFull <- reportDiv' "top20StyleMonthFull" 
  topColourMonthFull <- reportDiv' "top20ColourMonthFull" 
  topSkuMonthFull <- reportDiv' "top20ItemMonthFull" 
  topStyleJanuaryFull <- reportDiv' "top20StyleJanuaryFull" 
  topColourJanuaryFull <- reportDiv' "top20ColourJanuaryFull" 
  topSkuJanuaryFull <- reportDiv' "top20ItemJanuaryFull" 


  cacheSeconds (3600*23)
  defaultLayout $ do
    addScriptRemote "https://cdn.plot.ly/plotly-latest.min.js"
    toWidgetHead commonCss
    toWidgetHead pivotCss 
    toWidgetHead reportCss
    [whamlet|
  <div.panel_.panel_-primary>
    <div.panel_-heading data-toggle=collapse data-target="#dashboard-panel_-1">
      <h2> Monthly Sales
    <div.panel_-body.pivot-inline id=dashboard-panel_-1>
      <div.row>
        <div.col-md-12>
          ^{currentMonthFull}
      <div.row>
        <div.col-md-4.display10 >
          <h4> Top Styles Monthly
          ^{topStyleMonthFull}
        <div.col-md-4.display10>
          <h4> Top Colour Monthly
          ^{topColourMonthFull}
        <div.col-md-4.display10>
          <h4> Top Item Monthly
          ^{topSkuMonthFull}
  <div.panel_.panel_-primary>
    <div.panel_-heading data-toggle=collapse data-target="#dashboard-panel_-2">
      <h2> Since January
    <div.panel_-body.pivot-inline id=dashboard-panel_-2>
      <div.row>
        <div.col-md-4.display10 >
          <h4> Top Styles 
          ^{topStyleJanuaryFull}
        <div.col-md-4.display10>
          <h4> Top Colour 
          ^{topColourJanuaryFull}
        <div.col-md-4.display10>
          <h4> Top Item 
          ^{topSkuJanuaryFull}
  <div.footer>
  <span.text-right.font-italic>
    Last update #{tshow now}
  |]
  -- toWidgetBody [julius|
  --                     $("div#current-month-pcent").load("@{DashboardR (DCustomR "sales-current-month-p" 800 400)}")
  --                     $("div#top-style-month-pcent").load("@{DashboardR (DCustomR "top20StyleMonth" 800 400)}")
  --                     $("div#top-colour-month-pcent").load("@{DashboardR (DCustomR "top20ColourMonth" 800 400)}")
  --                     $("div#top-sku-month-pcent").load("@{DashboardR (DCustomR "top20ItemMonth" 800 400)}")
  --                     $("div#top-style-january-pcent").load("@{DashboardR (DCustomR "top20StyleJanuary" 800 400)}")
  --                     $("div#top-colour-january-pcent").load("@{DashboardR (DCustomR "top20ColourJanuary" 800 400)}")
  --                     $("div#top-sku-january-pcent").load("@{DashboardR (DCustomR "top20ItemJanuary" 800 400)}")
  --                     |]

-- Run report by name (find in configuration file)
{-# NOINLINE getDCustomR #-}
getDCustomR :: Text -> Int64 -> Int64 -> Handler Html
getDCustomR reportName width height = do
  role <- currentRole
  when (not $ authorizeFromAttributes role (setFromList [reportName]) ReadRequest)
       (permissionDenied reportName)
  reportMakerE <- dispatchReport reportName width height
  let widget = case reportMakerE of
        Left err -> error (unpack err)
        Right (w,_) -> w
  p <- widgetToPageContent widget
  withUrlRenderer [hamlet|^{pageBody p}|]

-- enum of type of report
-- allows to check statically that the report we need exist
-- data DashboardReport = |
--  | SalesCurrentMonthP
--  | Top20StyleMonth
--  | Top20ColourMonth
--  | Top20ItemMonth
--  | Top20StyleJanuary
--  | Top20ColourJanuary
--  | Top20ItemJanuary
--  deriving (Show, Eq, Enum, Bounded)
-- | Display full year (fiscal and las 52 weeks)
{-# NOINLINE getDYearR #-}
getDYearR, getDAllYearR :: Handler Html
getDYearR = getDYearR' ""
getDAllYearR = getDYearR' "20"
getDYearR' suffix = do
  now <- liftIO $ getCurrentTime
  -- refactor
  slidingFull <- reportDiv $ "salesSlidingYearFull" ++ suffix
  slidingBack <- reportDiv $ "salesSlidingYearFullBackward" ++ suffix
  currentFull <- reportDiv $ "salesCurrentYearFull" ++ suffix
  fiscalFull <- reportDiv $ "salesCurrentFiscalFull"++ suffix

  cacheSeconds (3600*23)
  defaultLayout $ do
    addScriptRemote "https://cdn.plot.ly/plotly-latest.min.js"
    toWidgetHead reportCss
    [whamlet|
  <div.panel_.panel_-primary>
    <div.panel_-heading data-toggle=collapse data-target="#dashboard-panel_-1">
      <h2> Full Year
    <div.panel_-body.pivot-inline id=dashboard-panel_-1>
      <div.row>
        <div.col-md-12>
          ^{slidingFull}
      <div.row>
        <div.col-md-12>
          ^{currentFull}
      <div.row>
        <div.col-md-12>
          ^{fiscalFull}
      <div.row>
        <div.col-md-12>
          ^{slidingBack}
  <div.footer>
  <span.text-right.font-italic>
    Last update #{tshow now}
  |]

dispatchReport :: Text -> Int64 -> Int64 -> Handler (Either Text (Widget, ReportParam))
dispatchReport reportName __width __height = do
  today <- todayH
  fiscalYear <- fiscalYearH
  let slidingMonth = calculateDate (AddMonths (-1)) today
      beginJanuary = fromGregorian (currentYear) 1 1
      endDecember = fromGregorian currentYear 12 31
      currentYear = toYear today
      slidingYearEnd = today
      slidingYear = calculateDate (AddDays 1) $ calculateDate (AddMonths (-12)) slidingYearEnd
      fiscalYearEnd = calculateDate (AddDays (-1)) $ calculateDate (AddMonths 12) fiscalYear
      
      reportMaker = case reportName of
        "salesCurrentMonthP" -> salesCurrentMonth id reportName
        "top20ItemMonth" -> top20ItemMonth id slidingMonth skuColumn
        "top20StyleMonth" -> top20ItemMonth id slidingMonth styleColumn
        "top20ColourMonth" -> top20ItemMonth id slidingMonth variationColumn
        "top20ItemJanuary" -> top20ItemMonth id beginJanuary skuColumn
        "top20StyleJanuary" -> top20ItemMonth id beginJanuary styleColumn
        "top20ColourJanuary" -> top20ItemMonth id beginJanuary variationColumn
        "top100ItemYearChart" -> top100ItemYearChart "top1"
        "top100ItemYearChart2" -> top100ItemYearChart "top2"
        "top100ItemYearXXX" -> top100ItemYear True skuColumn
        "top100ItemYear" -> top100ItemYear False skuColumn
        "top100StyleYear" -> top100ItemYear False styleColumn
        "top100ColourYear" -> top100ItemYear False variationColumn

        "salesCurrentMonthFull" -> salesCurrentMonth salesCurrentUp reportName 
        "salesCurrentYearFull" -> salesCurrentMonth (salesCurrentYearUp RunSum beginJanuary endDecember) reportName 
        "salesSlidingYearFull" -> salesCurrentMonth (salesCurrentYearUp RunSum slidingYear slidingYearEnd) reportName
        "salesSlidingYearFullBackward" -> salesCurrentMonth (salesCurrentYearUp RunSumBack slidingYear slidingYearEnd) reportName
        "salesCurrentFiscalFull" -> salesCurrentMonth ((\param -> param  {rpNumberOfPeriods = Just 5, rpDataParam2 = emptyTrace}) . salesCurrentYearUp RunSum fiscalYear fiscalYearEnd) reportName
        "salesCurrentMonthFull20" -> salesCurrentMonth (rep20 . salesCurrentUp) reportName 
        "salesCurrentYearFull20" -> salesCurrentMonth (rep20 . salesCurrentYearUp RunSum beginJanuary endDecember) reportName 
        "salesSlidingYearFull20" -> salesCurrentMonth (rep20 . salesCurrentYearUp RunSum slidingYear slidingYearEnd) reportName
        "salesSlidingYearFullBackward20" -> salesCurrentMonth (rep20 . salesCurrentYearUp RunSumBack slidingYear slidingYearEnd) reportName
        "salesCurrentFiscalFull20" -> salesCurrentMonth (rep20 . salesCurrentYearUp RunSum fiscalYear fiscalYearEnd) reportName
        "top20ItemMonthFull" -> top20ItemMonth top20FullUp slidingMonth skuColumn
        "top20StyleMonthFull" -> top20ItemMonth top20FullUp slidingMonth styleColumn
        "top20ColourMonthFull" -> top20ItemMonth top20FullUp slidingMonth variationColumn
        "top20ItemJanuaryFull" -> top20ItemMonth top20FullUp beginJanuary skuColumn
        "top20StyleJanuaryFull" -> top20ItemMonth top20FullUp beginJanuary styleColumn
        "top20ColourJanuaryFull" -> top20ItemMonth top20FullUp beginJanuary variationColumn
        _ -> fail $ "undefined report "  <> unpack reportName
      rep20 param = param { rpNumberOfPeriods = Just 20 }
  report <- reportMaker
  return (Right report)

cumulSales = ("CumulAmount (Out)" ,   [(qpAmount Outward, VAmount, cumulStyle, RunSum)] )
_quantitySales = ("CumulAmount (Out)" ,   [(qpQty Outward, VAmount, cumulStyle, RunSum)] )
amountSales = ("Amount (Out)" ,   [(qpAmount Outward, VAmount, smoothStyle AmountAxis, RSNormal)] )
quantitySales = ("Amount (Out)" ,   [(qpQty Outward, VQuantity, smoothStyle QuantityAxis, RSNormal)] )
cumulStyle color = [("type", String "scatter")
                      ,("mode", String "lines")
                      ,("name", String "Sales")
                      ,("line", [aesonQQ|{
                               shape:"linear", 
                               color: #{color}
                                }|])
                -- , ("marker", [aesonQQ|{symbol: null}|])
                , ("yaxis", "y2")
                , ("showlegend", toJSON True)
              ]
salesCurrentUp :: ReportParam -> ReportParam
salesCurrentUp param = param {rpDataParam, rpDataParam2} where
      rpDataParam = DataParams QPSales (mkIdentifialParam cumulSales) Nothing
      rpDataParam2 = DataParams QPSales (mkIdentifialParam amountSales) Nothing
  
salesCurrentYearUp :: RunSum -> Day -> Day -> ReportParam -> ReportParam
salesCurrentYearUp runsum from to param =
  (salesCurrentUp param) { rpFrom = Just from
        , rpTo = Just to
        , rpToday = to
        , rpPeriod' = Just PFSlidingYearFrom
        , rpColumnRupture = ColumnRupture (Just weeklyColumn) (DataParams QPSummary
                                          (Identifiable ("Column", [])) Nothing) Nothing Nothing False
        , rpDataParam = DataParams QPSales (mkIdentifialParam cumulSales0) Nothing
        }
  where 
      cumulSales0 = ("CumulAmount (Out)" ,   [(qpAmount Outward, VAmount, cumulStyle, runsum)] )
top20FullUp param = param {rpDataParam2,rpDataParam3} where
      rpDataParam2 = DataParams QPSales (mkIdentifialParam amountSales) Nothing
      rpDataParam3 = DataParams QPSales (mkIdentifialParam quantitySales) Nothing
      -- rpDataParam3 = DataParams QPSales (mkIdentifialParam quantityOutOption) Nothing
-- | Sales current months

salesCurrentMonth:: (ReportParam -> ReportParam) -> Text -> Handler (Widget, ReportParam)
salesCurrentMonth f plotName = do
  today <- todayH
  rpDeduceTax <- appReportDeduceTax <$> getsYesod appSettings 
  -- The display period is the current month. However, during the first days of a new month
  -- we display the previous month extended to today
  let beginMonth = calculateDate (BeginningOfMonth) . calculateDate (AddDays  (-7)) $ today
      endMonth = max today (calculateDate EndOfMonth beginMonth)
      param = f ReportParam{rpColumnRupture=columnRupture,..}
      rpToday = today
      rpFrom = Just $ beginMonth
      rpTo = Just endMonth
      rpPeriod' = Just PFWholeYear
      rpNumberOfPeriods = Just 2
      rpCategoryToFilter = Nothing
      rpCategoryFilter = Nothing
      rpStockFilter = Nothing
      rpShowInactive = True
      rpPanelRupture = emptyRupture
      rpBand = emptyRupture
      rpSerie = ColumnRupture  (Just periodColumn) (DataParams QPSummary (Identifiable ("Column", [])) Nothing) Nothing Nothing True
      columnRupture = ColumnRupture  (Just dailyColumn) (DataParams QPSummary (Identifiable ("Column", [])) Nothing) Nothing Nothing False
      rpDataParam = DataParams QPSales (mkIdentifialParam cumulSales_) (Just $ NormalizeMode NMBestInit NMBand )
      rpDataParam2 = DataParams QPSales (mkIdentifialParam amountSales_) (Just $ NormalizeMode NMTotal NMBand )
      rpDataParam3 = emptyTrace
      rpLoadSalesAndInfo = Just SSalesOnly
      rpLoadSalesOrders = Nothing
      rpLoadPurchases = False
      rpPurchasesDateOffset = Nothing
      rpLoadPurchaseOrders = Nothing
      rpLoadAdjustment = False
      rpForecast = (Nothing, Nothing, Nothing)
      rpColourMode = minBound
      rpTraceGroupMode = Nothing
      amountSales_ = ("Amount (Out)" ,   [(qpAmount Outward, VAmount, amountStyle_, RSNormal)] )
      amountStyle_ color = [("type", String "scatter")
                      ,("mode", String "lines")
                      ,("name", String "Sales")
                      ,("line", [aesonQQ|{
                               shape:"spline", 
                               color: #{color},
                               dash: "dot",
                               width: 1
                                }|])
                , ("marker", [aesonQQ|{symbol: "square"}|])
              ]
      cumulSales_ = ("CumulAmount (Out)" ,   [(qpAmount Outward, VAmount, cumulStyle_, RunSum)] )
      cumulStyle_ color = [("type", String "scatter")
                      ,("mode", String "lines")
                      ,("name", String "Sales")
                      ,("line", [aesonQQ|{
                               shape:"linear", 
                               color: #{color}
                                }|])
                -- , ("marker", [aesonQQ|{symbol: null}|])
                , ("yaxis", "y2")
                , ("showlegend", toJSON True)
              ]
      -- TODO factorize
      grouper = [ -- rpPanelRupture,
                  rpBand , rpSerie
                , rpColumnRupture param
                ]
  report <- itemReportWithRank param grouper (\nmap -> plotChartDiv param (const 350) nmap plotName nmap)
  return $ (report, param)


-- | Top style
top20ItemMonth :: (ReportParam -> ReportParam) -> Day -> Column -> Handler (Widget, ReportParam)
top20ItemMonth f begin rupture = do
  today <- todayH
  rpDeduceTax <- appReportDeduceTax <$> getsYesod appSettings 

  let param = f ReportParam{..}
      rpToday = today
      rpFrom = Just begin
      rpTo = Just today
      rpPeriod' = Nothing
      rpNumberOfPeriods = Nothing
      rpCategoryToFilter = Nothing
      rpCategoryFilter = Nothing
      rpStockFilter = Nothing -- Just (LikeFilter "ML1_-A_2-BLK")
      rpShowInactive = True
      rpPanelRupture = emptyRupture
      rpBand = emptyRupture
      rpSerie = ColumnRupture (Just rupture) bestSalesTrace (Just RMResidual) (Just 50) False
      rpColumnRupture = ColumnRupture  Nothing (DataParams QPSummary (Identifiable ("Column", [])) Nothing) Nothing Nothing True
      rpDataParam = DataParams QPSales (mkIdentifialParam $ amountOutOption 1) (Just $ NormalizeMode NMColumn NMSerie )
      rpDataParam2 = emptyTrace
      -- rpDataParam = DataParams QPSales (mkIdentifialParam amountOutOption) (Just $ NormalizeMode NMRank NMBand )
      -- rpDataParam3 = DataParams QPSales (mkIdentifialParam amountOutOption) Nothing
      rpDataParam3 = emptyTrace
      rpLoadSalesAndInfo = Just SSalesOnly
      rpLoadSalesOrders = Nothing
      rpLoadPurchases = False
      rpPurchasesDateOffset = Nothing
      rpLoadPurchaseOrders = Nothing
      rpLoadAdjustment = False
      rpForecast = (Nothing, Nothing, Nothing)
      rpColourMode = minBound
      rpTraceGroupMode = Nothing
  report <- let pivotP tparams = processRupturesWith (createKeyRankProcessor $ \_ _ -> (bandPivotProcessor tparams "pivot-Top20", id) ) ()
            in itemReport param pivotP--  (panelPivotProcessor "pivot-Top-100" (mkNMapKey "New Report"))
  return $ (report, param)

top100ItemYear ::  Bool -> Column -> Handler (Widget, ReportParam)
top100ItemYear which rupture = do
  today <- todayH
  rpDeduceTax <- appReportDeduceTax <$> getsYesod appSettings 

  let beginYear = fromGregorian (currentYear) 1 1
      currentYear = toYear today
  let param = ReportParam{..}
      rpToday = today
      rpFrom = Just beginYear
      rpTo = Just today
      rpPeriod' = Just PFSlidingYearFrom
      rpNumberOfPeriods = Just 2
      rpCategoryToFilter = Nothing
      rpCategoryFilter = Nothing
      rpStockFilter = Nothing -- Just (LikeFilter "ML1_-A_2-BLK")
      rpShowInactive = True
      rpPanelRupture = emptyRupture
      rpBand = emptyRupture
      rpSerie = ColumnRupture (Just rupture) bestSalesTrace (Just RMResidual) (Just 100) False
      rpColumnRupture = ColumnRupture  (Just periodColumn) (DataParams QPSummary (Identifiable ("Column", [])) Nothing) Nothing Nothing True
      rpDataParam2 = DataParams QPSales (mkIdentifialParam $ amountOutOption 1) (Just $ NormalizeMode NMColumn NMSerie )
      rpDataParam = DataParams QPSales (mkIdentifialParam $ amountOutOption 1) (Just $ NormalizeMode NMRank NMBand )
      -- rpDataParam3 = DataParams QPSales (mkIdentifialParam amountOutOption) Nothing
      rpDataParam3 = emptyTrace
      rpLoadSalesAndInfo = Just SSalesOnly
      rpLoadSalesOrders = Nothing
      rpLoadPurchases = False
      rpPurchasesDateOffset = Nothing
      rpLoadPurchaseOrders = Nothing
      rpLoadAdjustment = False
      rpForecast = (Nothing, Nothing, Nothing)
      rpColourMode = minBound
      rpTraceGroupMode = Nothing
      -- TODO factorize
      grouper = [ rpBand, rpSerie
                , rpColumnRupture
                ]
  -- report <- itemReportWithRank param grouper (pivotProcessorXXX param)
  report <- if which 
            then itemReportWithRank param grouper (\nmap -> panelPivotProcessorXXX param nmap "pivot-Top-100" nmap)
            -- else let pivotP tparams = processRupturesWith (\_ _ -> processRupturesWith (bandPivotProcessor tparams "pivot-Top-100") ) ()
            else let pivotP tparams = processRupturesWith (createKeyRankProcessor $ \_ _ -> (bandPivotRankProcessor tparams "pivot-Top-100", \w -> [whamlet|<div#pivot-Top-100>^{w}|]) ) ()
                 in itemReport param pivotP--  (panelPivotProcessor "pivot-Top-100" (mkNMapKey "New Report"))
  return $ (report, param)
      

top100ItemYearChart :: Text -> Handler (Widget, ReportParam)
top100ItemYearChart plotName = do
  today <- todayH
  rpDeduceTax <- appReportDeduceTax <$> getsYesod appSettings 
  let beginYear = fromGregorian (currentYear) 1 1
      currentYear = toYear today
  let param = ReportParam{..}
      rpToday = today
      rpFrom = Just beginYear
      rpTo = Just today
      rpPeriod' = Nothing
      rpNumberOfPeriods = Nothing
      rpCategoryToFilter = Nothing
      rpCategoryFilter = Nothing
      rpStockFilter = Nothing
      rpShowInactive = True
      rpPanelRupture = emptyRupture
      rpBand = emptyRupture
      rpSerie = ColumnRupture (Just skuColumn) bestSalesTrace Nothing (Just 100) False
      rpColumnRupture = ColumnRupture  (Just monthlyColumn) (DataParams QPSummary (Identifiable ("Column", [])) Nothing) Nothing Nothing False
      rpDataParam = DataParams QPSales (mkIdentifialParam $ amountOutOption 1) Nothing 
      rpDataParam2 = emptyTrace
      rpDataParam3 = emptyTrace
      rpLoadSalesAndInfo = Just SSalesOnly
      rpLoadSalesOrders = Nothing
      rpLoadPurchases = False
      rpPurchasesDateOffset = Nothing
      rpLoadPurchaseOrders = Nothing
      rpLoadAdjustment = False
      rpForecast = (Nothing, Nothing, Nothing)
      rpColourMode = minBound
      rpTraceGroupMode = Nothing
      -- TODO factorize
      grouper = [ -- rpPanelRupture,
                  rpBand, rpSerie
                , rpColumnRupture
                ]
  report <- itemReportWithRank param grouper (\nmap -> plotChartDiv param (const 350) nmap plotName nmap)
  return $ (report, param)


-- pivotRankProcessor:: [DataParams] -> _ColumnRuptures -> NMap TranQP -> Widget
-- pivotRankProcessor tparams = 
--   processRupturesWith (panelPivotRankProcessor tparams "items-report-pivotRank") ()
  
-- -- each nmap is a panel_
-- panelPivotRankProcessor :: [DataParams] -> Text -> NMapKey -> Int -> _ -> _ -> NMap TranQP ->  Widget
-- panelPivotRankProcessor tparams reportId = createKeyRankProcessor go where
--   go key rank = let panelName = nkeyWithRank (rank, key)
--                     panelId = reportId <> "-panel_-" <> panelName
--                     sub = bandPivotRankProcessor tparams panelId
--                     widget children = [whamlet|
--                                <div.panel_.panel_-info>
--                                  <div.panel_-heading data-toggle="collapse" data-target="#{panelId}">
--                                    <h2>#{panelName}
--                                  <div.panel_-body.collapse.in id="#{panelId}" style="max-height:2000px; overflow:auto">
--                                    ^{children}
--                        |]
--                     in (sub, widget)
  

-- each nmap is band
bandPivotRankProcessor :: [DataParams] -> p -> NMapKey -> Int -> (NMap TranQP, (NMap TranQP, ())) -> (ColumnRupture, (ColumnRupture, ())) -> NMap TranQP -> Widget
bandPivotRankProcessor tparams __panelId key0 rank0 parents ruptures = createKeyRankProcessor go key0 rank0 parents ruptures where
  (serieRupture, (columRupture,_)) = ruptures
  go key rank =  let sub = collectColumnsForPivotRank tparams
                          -- get the list of the columns for that we need to
                          -- sort and limit each serie first by calling processRupturesWith
                     widget cw =  let
                          (cols, rankMaps) = unzip cw
                          -- colmns comes with a weight, we need to add up those weight sort
                          -- the keys according to it
                          columnMap = Map.fromListWith (<>) $ join cols
                          sorted' = map fst (sortOn snd $ mapToList columnMap)
                          sorted = if cpReverse columRupture then reverse sorted' else sorted'
                          columns = zip [1..] $ sorted

                          -- rankMap gives a map  rank , col widget to display, we need to assemble it first
                          -- we should have any collision
                          rankMap :: Map (Int, NMapKey) Widget 
                          rankMap = mconcat  rankMaps
                          ranks = maybe id take (cpLimitTo serieRupture) [1.. maximumEx (map fst $ keys rankMap)]
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
                                      $forall rk <- ranks
                                        <tr>
                                          <td>#{rk}
                                          $forall column <- columns 
                                            <td>
                                              ^{fromMaybe "-" (lookup (rk, snd column) rankMap)}
                                        |]
                     in (sub, widget)

-- get the list of the columns for that we need to
 -- nmap is a serie
collectColumnsForPivotRank :: [DataParams] -> NMapKey -> Int -> _parents -> _ruptures -> NMap TranQP -> [(_, Map (Int, NMapKey) Widget)]
collectColumnsForPivotRank tparams key __rank parents ruptures@(r, ()) nmap = let
  (band, (panel_, (all, ()))) = parents
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
  
  -- all, we only need the first one, change format function to get an Integer (the rank)
  rankTrace:_ = [formatSerieValuesNMap floor
                                  floor
                                  normMode all panel_ band
                                  (fmap (tpValueGetter tp) . lookupGrouped qtype) (nmapRunSum (tpRunSum tp) nmap)
           | DataParams qtype tps normMode <- tparams
           , tp <- getIdentified tps
           ]
  _:traces = [formatSerieValuesNMap (formatDouble' tp)
                                  (formatPercent tp normMode)
                                  normMode all panel_ band
                                  (fmap (tpValueGetter tp) . lookupGrouped qtype) (nmapRunSum (tpRunSum tp) nmap)
             | DataParams qtype tps normMode <- tparams
             , tp <- getIdentified tps
             ]
  rankMap = [ ((rankForCol, column) , widget rankForCol column) 
            | (column, _) <- columns -- only use trace or nmap keys instead
            , Just rankForCol <- return $ lookup column rankTrace
            ]
  widget __rank column = let
    -- isTop = rank == 1
    -- isTop2 = rank == 2
          -- <div :isTop:.topOne :isTop2:.topTwo>#{pvToText $ nkKey key}
    in [whamlet|
          <div>#{pvToText $ nkKey key}
          $forall trace <- traces
            <div.just-right>#{fromMaybe "-" $ lookup column trace}
                        |]
  in [(columns, mapFromList rankMap )]
  
  
getDForecastR :: Handler Html
getDForecastR = do
  settings <- getsYesod appSettings
  -- load all available forecasts
  dirs <- liftIO $ glob (appForecastProfilesDir settings </> "????-??-??*")
  traceShowM ("DIRS", dirs)
  let day'paths = [ (Down day, path)
                  | path <- dirs
                  , day <- maybeToList $ forecastPathToDay $ takeBaseName path
                  ]
  plot'summarys <- forM (sort $ day'paths) \(Down day, path) -> do
                 (plot, summary) <- getPlotForecastError day path
                 return (day, takeBaseName path, plot, summary)
  let toPercent x = sformat (commasFixedWith' round 1) (x*100)
  defaultLayout $ do
      [whamlet|
      <div.panel.panel-primary>
         <div.panel-heading data-toggle=collapse data-target="#dashboard-summary">
           <h2> Summary
         <div.panel-body.pivot-inline id="dashboard-summary">
              <table.table.table-hover.table-striped>
                <thead>
                  <tr>
                    <th> Date
                    <th> Method
                    <th> Scaled Error
                    <th> % Under estimation
                    <th> % Over estimation
                    <th> % Error estimation
                    <th> % Error Naive
                    <th> % Trend
                <tbody>
                  $forall (day, path, _,summary) <- plot'summarys
                    <tr>
                      <td>#{tshow day}
                      <td>#{path}
                      <th>#{toPercent $ aes summary}%
                      <td>#{toPercent $ underPercent summary}%
                      <td>#{toPercent $ overPercent summary}%
                      <th>#{toPercent $ overallPercent summary}%
                      <td>#{toPercent $ naivePercent summary}%
                      <td>#{toPercent $ trendPercent summary}%
            
      $forall (_day, path, plot,_) <- plot'summarys
            <div.panel.panel-primary>
               <div.panel-heading data-toggle=collapse data-target="#dashboard-#{path}">
                 <h2> #{path}
               <div.panel-body.pivot-inline id="dashboard-#{path}">
                 ^{plot}
      |]

