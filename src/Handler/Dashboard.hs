--
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
import qualified Data.Map as Map
import Formatting
import Data.Aeson.QQ(aesonQQ)

-- Display a dashboard made of different report depending on the configuration file
getDMainR :: Handler Html
getDMainR = defaultLayout $ do
  now <- liftIO $ getCurrentTime
  addScriptRemote "https://cdn.plot.ly/plotly-latest.min.js"
  toWidgetHead commonCss
  toWidgetHead [cassius|
div.pivot-inline
  th
    writing-mode: lr
  td div
    display: inline
  .display10
    max-height: 410px
    overflow: hidden
    &:hover
      overflow:auto
    > div > div
      > h3, thead
        display:  none
    h4
      text-align: center
                       |]
  [whamlet|
<div.panel.panel-primary>
  <div.panel-heading data-toggle=collapse data-target="#dashboard-panel-1">
    <h2> Monthly Sales
  <div.panel-body.pivot-inline id=dashboard-panel-1>
     <div.row>
       <div.col-md-12>
         <div#current-month-pcent>
     <div.row>
       <div.col-md-4.display10 >
         <h4> Top Styles Monthly
         <div#top-style-month-pcent>
       <div.col-md-4.display10>
         <h4> Top Colour Monthly
         <div#top-colour-month-pcent>
       <div.col-md-4.display10>
         <h4> Top Item Monthly
         <div#top-sku-month-pcent>
<div.panel.panel-primary>
  <div.panel-heading data-toggle=collapse data-target="#dashboard-panel-1">
    <h2> Since January
  <div.panel-body.pivot-inline id=dashboard-panel-2>
     <div.row>
       <div.col-md-4.display10 >
         <h4> Top Styles 
         <div#top-style-january-pcent>
       <div.col-md-4.display10>
         <h4> Top Colour 
         <div#top-colour-january-pcent>
       <div.col-md-4.display10>
         <h4> Top Item 
         <div#top-sku-january-pcent>
<div.footer>
<span.text-right.font-italic>
  Last update #{tshow now}
|]
  toWidgetBody [julius|
                      $("div#current-month-pcent").load("@{DashboardR (DCustomR "sales-current-month-p" 800 400)}")
                      $("div#top-style-month-pcent").load("@{DashboardR (DCustomR "top20StyleMonth" 800 400)}")
                      $("div#top-colour-month-pcent").load("@{DashboardR (DCustomR "top20ColourMonth" 800 400)}")
                      $("div#top-sku-month-pcent").load("@{DashboardR (DCustomR "top20ItemMonth" 800 400)}")
                      $("div#top-style-january-pcent").load("@{DashboardR (DCustomR "top20StyleJanuary" 800 400)}")
                      $("div#top-colour-january-pcent").load("@{DashboardR (DCustomR "top20ColourJanuary" 800 400)}")
                      $("div#top-sku-january-pcent").load("@{DashboardR (DCustomR "top20ItemJanuary" 800 400)}")
                      |]

-- Run report by name (find in configuration file)
getDCustomR :: Text -> Int64 -> Int64 -> Handler Html
getDCustomR reportName width height = do
  role <- currentRole
  when (not $ authorizeFromAttributes role (setFromList [reportName]) ReadRequest)
       (permissionDenied reportName)
  today <- utctDay <$> liftIO getCurrentTime
     
  let reportMaker = case reportName of
        "sales-current-month-p" -> salesCurrentMonth reportName
        "top20ItemMonth" -> top20ItemMonth beginMonth skuColumn
        "top20StyleMonth" -> top20ItemMonth beginMonth styleColumn
        "top20ColourMonth" -> top20ItemMonth beginMonth variationColumn
        "top20ItemJanuary" -> top20ItemMonth beginJanuary skuColumn
        "top20StyleJanuary" -> top20ItemMonth beginJanuary styleColumn
        "top20ColourJanuary" -> top20ItemMonth beginJanuary variationColumn
        "top100ItemYearChart" -> top100ItemYearChart "top1"
        "top100ItemYearChart2" -> top100ItemYearChart "top2"
        "top100ItemYearXXX" -> top100ItemYear True skuColumn
        "top100ItemYear" -> top100ItemYear False skuColumn
        "top100StyleYear" -> top100ItemYear False styleColumn
        "top100ColourYear" -> top100ItemYear False variationColumn
        _ -> error "undefined report"
      beginMonth = calculateDate (AddMonths (-1)) today
      beginJanuary = fromGregorian (currentYear) 1 1
      currentYear = toYear today
  widget <- reportMaker
  p <- widgetToPageContent widget
  cacheSeconds (3600*24)
  withUrlRenderer [hamlet|^{pageBody p}|]


-- | Sales current months

salesCurrentMonth plotName = do
  today <- utctDay <$> liftIO getCurrentTime
  let endMonth = calculateDate (AddMonths 1) beginMonth
      beginMonth = calculateDate (BeginningOfMonth) . calculateDate (BeginningOfWeek Monday) $ today
  let param = ReportParam{..}
      rpToday = today
      rpFrom = Just beginMonth
      rpTo = Just endMonth
      rpPeriod' = Just PFWholeYear
      rpNumberOfPeriods = Just 2
      rpCategoryToFilter = Nothing
      rpCategoryFilter = Nothing
      rpStockFilter = Nothing
      rpPanelRupture = emptyRupture
      rpBand = emptyRupture
      rpSerie = ColumnRupture  (Just periodColumn) (TraceParams QPSummary (Identifiable ("Column", [])) Nothing) Nothing Nothing False
      rpColumnRupture = ColumnRupture  (Just dailyColumn) (TraceParams QPSummary (Identifiable ("Column", [])) Nothing) Nothing Nothing False
      rpTraceParam = TraceParams QPSales (mkIdentifialParam cumulSales) (Just $ NormalizeMode NMBestInit NMBand )
      rpTraceParam2 = TraceParams QPSales (mkIdentifialParam amountSales) (Just $ NormalizeMode NMTotal NMBand )
      rpTraceParam3 = emptyTrace
      rpLoadSales = True
      rpLoadPurchases = False
      rpLoadAdjustment = False
      amountSales = ("Amount (Out)" ,   [(qpAmount Outward, VAmount, amountStyle, RSNormal)] )
      amountStyle color = [("type", String "scatter")
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
      cumulSales = ("CumulAmount (Out)" ,   [(qpAmount Outward, VAmount, cumulStyle, RunSum)] )
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
      -- TODO factorize
      grouper = [ -- rpPanelRupture,
                  rpBand, rpSerie
                , rpColumnRupture
                ]
  report <- itemReportXXX param grouper (\nmap -> panelChartProcessor (const 350) nmap param plotName nmap)
  return $ report


-- | Top style
top20ItemMonth begin rupture = do
  today <- utctDay <$> liftIO getCurrentTime
  let tomorrow = calculateDate (AddDays 1) today
  let param = ReportParam{..}
      rpToday = today
      rpFrom = Just begin
      rpTo = Just tomorrow
      rpPeriod' = Nothing
      rpNumberOfPeriods = Nothing
      rpCategoryToFilter = Nothing
      rpCategoryFilter = Nothing
      rpStockFilter = Nothing -- Just (LikeFilter "ML1_-A_2-BLK")
      rpPanelRupture = emptyRupture
      rpBand = emptyRupture
      rpSerie = ColumnRupture (Just rupture) bestSalesTrace (Just RMResidual) (Just 50) False
      rpColumnRupture = ColumnRupture  Nothing (TraceParams QPSummary (Identifiable ("Column", [])) Nothing) Nothing Nothing True
      rpTraceParam = TraceParams QPSales (mkIdentifialParam amountOutOption) (Just $ NormalizeMode NMColumn NMSerie )
      rpTraceParam2 = emptyTrace
      -- rpTraceParam = TraceParams QPSales (mkIdentifialParam amountOutOption) (Just $ NormalizeMode NMRank NMBand )
      -- rpTraceParam3 = TraceParams QPSales (mkIdentifialParam amountOutOption) Nothing
      rpTraceParam3 = emptyTrace
      rpLoadSales = True
      rpLoadPurchases = False
      rpLoadAdjustment = False
  report <- let pivotP tparams = processRupturesWith (createKeyRankProcessor $ \_ _ -> (bandPivotProcessor tparams "pivot-Top20", id) ) ()
            in itemReport param pivotP--  (panelPivotProcessor "pivot-Top-100" (mkNMapKey "New Report"))
  return $ report

top100ItemYear which rupture = do
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
      rpStockFilter = Nothing -- Just (LikeFilter "ML1_-A_2-BLK")
      rpPanelRupture = emptyRupture
      rpBand = emptyRupture
      rpSerie = ColumnRupture (Just rupture) bestSalesTrace (Just RMResidual) (Just 100) False
      rpColumnRupture = ColumnRupture  (Just periodColumn) (TraceParams QPSummary (Identifiable ("Column", [])) Nothing) Nothing Nothing True
      rpTraceParam2 = TraceParams QPSales (mkIdentifialParam amountOutOption) (Just $ NormalizeMode NMColumn NMSerie )
      rpTraceParam = TraceParams QPSales (mkIdentifialParam amountOutOption) (Just $ NormalizeMode NMRank NMBand )
      -- rpTraceParam3 = TraceParams QPSales (mkIdentifialParam amountOutOption) Nothing
      rpTraceParam3 = emptyTrace
      rpLoadSales = True
      rpLoadPurchases = False
      rpLoadAdjustment = False
      -- TODO factorize
      grouper = [ rpBand, rpSerie
                , rpColumnRupture
                ]
  -- report <- itemReportXXX param grouper (pivotProcessorXXX param)
  report <- if which 
            then itemReportXXX param grouper (\nmap -> panelPivotProcessorXXX nmap param "pivot-Top-100" nmap)
            -- else let pivotP tparams = processRupturesWith (\_ _ -> processRupturesWith (bandPivotProcessor tparams "pivot-Top-100") ) ()
            else let pivotP tparams = processRupturesWith (createKeyRankProcessor $ \_ _ -> (bandPivotRankProcessor tparams "pivot-Top-100", \w -> [whamlet|<div#pivot-Top-100>^{w}|]) ) ()
                 in itemReport param pivotP--  (panelPivotProcessor "pivot-Top-100" (mkNMapKey "New Report"))
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
      rpColumnRupture = ColumnRupture  (Just monthlyColumn) (TraceParams QPSummary (Identifiable ("Column", [])) Nothing) Nothing Nothing False
      rpTraceParam = TraceParams QPSales (mkIdentifialParam amountOutOption) Nothing 
      rpTraceParam2 = emptyTrace
      rpTraceParam3 = emptyTrace
      rpLoadSales = True
      rpLoadPurchases = False
      rpLoadAdjustment = False
      -- TODO factorize
      grouper = [ -- rpPanelRupture,
                  rpBand, rpSerie
                , rpColumnRupture
                ]
  report <- itemReportXXX param grouper (\nmap -> panelChartProcessor (const 350) nmap param plotName nmap)
  return $ report

-- pivotRankProcessor:: [TraceParams] -> _ColumnRuptures -> NMap TranQP -> Widget
-- pivotRankProcessor tparams = 
--   processRupturesWith (panelPivotRankProcessor tparams "items-report-pivotRank") ()
  
-- -- each nmap is a panel
-- panelPivotRankProcessor :: [TraceParams] -> Text -> NMapKey -> Int -> _ -> _ -> NMap TranQP ->  Widget
-- panelPivotRankProcessor tparams reportId = createKeyRankProcessor go where
--   go key rank = let panelName = nkeyWithRank (rank, key)
--                     panelId = reportId <> "-panel-" <> panelName
--                     sub = bandPivotRankProcessor tparams panelId
--                     widget children = [whamlet|
--                                <div.panel.panel-info>
--                                  <div.panel-heading data-toggle="collapse" data-target="#{panelId}">
--                                    <h2>#{panelName}
--                                  <div.panel-body.collapse.in id="#{panelId}" style="max-height:2000px; overflow:auto">
--                                    ^{children}
--                        |]
--                     in (sub, widget)
  

-- each nmap is band
bandPivotRankProcessor tparams panelId key rank parents ruptures = createKeyRankProcessor go key rank parents ruptures where
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
collectColumnsForPivotRank :: [TraceParams] -> NMapKey -> Int -> _parents -> _ruptures -> NMap TranQP -> [(_, Map (Int, NMapKey) Widget)]
collectColumnsForPivotRank tparams key rank parents ruptures@(r, ()) nmap = let
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
  
  -- all, we only need the first one, change format function to get an Integer (the rank)
  rankTrace:_ = [formatSerieValuesNMap floor
                                  floor
                                  normMode all panel band
                                  (fmap (tpValueGetter tp) . lookupGrouped qtype) (nmapRunSum (tpRunSum tp) nmap)
           | TraceParams qtype tps normMode <- tparams
           , tp <- getIdentified tps
           ]
  _:traces = [formatSerieValuesNMap (formatDouble' tp)
                                  (formatPercent tp normMode)
                                  normMode all panel band
                                  (fmap (tpValueGetter tp) . lookupGrouped qtype) (nmapRunSum (tpRunSum tp) nmap)
             | TraceParams qtype tps normMode <- tparams
             , tp <- getIdentified tps
             ]
  rankMap = [ ((rankForCol, column) , widget rankForCol column) 
            | (column, _) <- columns -- only use trace or nmap keys instead
            , Just rankForCol <- return $ lookup column rankTrace
            ]
  widget rank column = let
    -- isTop = rank == 1
    -- isTop2 = rank == 2
          -- <div :isTop:.topOne :isTop2:.topTwo>#{pvToText $ nkKey key}
    in [whamlet|
          <div>#{pvToText $ nkKey key}
          $forall trace <- traces
            <div.just-right>#{fromMaybe "-" $ lookup column trace}
                        |]
  in [(columns, mapFromList rankMap )]
