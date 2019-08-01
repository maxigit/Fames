module Handler.Items.Report
( getItemsReportR
, postItemsReportR
, getItemsReport2R
, postItemsReport2R
, getItemsReport3R
, postItemsReport3R
) where

import Import
import Items.Types
import Handler.Items.Reports.Common
import Handler.Items.Common
import Handler.Util
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,)
import Text.Blaze.Html.Renderer.Text(renderHtml)
import qualified Data.List as List
import qualified FA as FA
import GL.Utils
import GL.Payroll.Settings

-- * Form
reportForm :: [Column] -> Maybe ReportParam -> Html -> MForm Handler (FormResult ReportParam, Widget)
reportForm cols paramM extra = do
  today <- todayH
  deduceTax <- appReportDeduceTax <$> getsYesod appSettings 
  categories <- lift $ categoriesH
  forecastDirOptions <- lift getForecastDirOptions

  let colOptions = [(colName c,c) | c <- cols]
      categoryOptions = [(cat, cat) | cat <-categories ]
      inoutwardOptions = [ (ios <> " - " <> dates <> " - " <> qtys, (io, date, qty) ) 
                         | (ios, io) <- [("As Sales" :: Text, Outward), ("As Purchase", Inward)]
                         , (dates, date) <- [ ("Order Date", OOrderDate), ("Delivery Date", ODeliveryDate) ]
                         , (qtys, qty) <- [ ("Ordered Quantity", OOrderedQuantity), ("Left Quantity", OQuantityLeft)]
                         ] 
      forecastOptions = [ ("As Sales - Outward" :: Text, Outward )
                        , ("As Purchase - Inward", Inward)
                        ]
      salesOptions = [ ("Transaction only" :: Text, SSalesOnly )
                     , ("With Order info", SSalesAndOrderInfo)
                     ]
      poOptions = [ (dates <> " - " <> qtys, (date, qty) ) 
                  | (dates, date) <- [ ("Order Date" :: Text, OOrderDate), ("Delivery Date", ODeliveryDate) ]
                  , (qtys, qty) <- [ ("Ordered Quantity", OOrderedQuantity), ("Left Quantity", OQuantityLeft)]
                  ]
  (fFrom, vFrom) <- mopt dayField "from" (Just $ rpFrom =<< paramM )
  (fTo, vTo) <- mopt dayField "to" (Just $ rpTo =<< paramM)
  (fPeriod, vPeriod) <- mopt (selectFieldList $ periodOptions today (rpFrom =<< paramM)) "period" (Just $ rpPeriod' =<< paramM)
  (fPeriodN, vPeriodN) <- mopt intField "number" (Just $ rpNumberOfPeriods =<< paramM)
  (fCategoryToFilter, vCategoryToFilter) <- mopt (selectFieldList categoryOptions ) "category" (Just $ rpCategoryToFilter =<< paramM)
  (fCategoryFilter, vCategoryFilter) <- mopt filterEField  "filter" (Just $ rpCategoryFilter =<< paramM)
  (fStockFilter, vStockFilter) <- mopt filterEField  "sku" (Just $ rpStockFilter =<< paramM)
  (fPanel, wPanel) <- ruptureForm colOptions "Panel" (rpPanelRupture <$> paramM)
  (fBand, wBand) <- ruptureForm colOptions "Band" (rpBand <$> paramM)
  (fSerie, wSerie) <- ruptureForm colOptions "Series" (rpSerie <$> paramM)
  (fColRupture, vColRupture) <- ruptureForm colOptions  "Columns" (rpColumnRupture <$> paramM)
  (fTrace1, wTrace1) <- traceForm 1 False "" (rpDataParam <$> paramM)
  (fTrace2, wTrace2) <- traceForm 2 False " 2" (rpDataParam2 <$> paramM)
  (fTrace3, wTrace3) <- traceForm 3 False " 3"(rpDataParam3 <$> paramM)
  (fSales, vSales) <- mopt (selectFieldList salesOptions) "Sales" (rpLoadSalesAndInfo <$> paramM)
  -- (fOrderInfo, vOrderInfo) <- mreq checkBoxField "OrderInfo" (rpLoadOrderInfo <$> paramM)
  (fOrder, vOrder) <- mopt (selectFieldList inoutwardOptions) "Sales Order" (rpLoadSalesOrders <$> paramM)
  (fPurchases, vPurchases) <- mreq checkBoxField "Purchases" (rpLoadPurchases <$> paramM)
  (fPurchasesDateOffset, vPurchasesDateOffset) <- mopt intField "Purchases Date Offset" (rpPurchasesDateOffset  <$> paramM)
  (fPOrders, vPOrders) <- mopt (selectFieldList poOptions) "Purchase Orders" (rpLoadPurchaseOrders <$> paramM)
  (fAdjustment, vAdjustment) <- mreq checkBoxField "Adjustment" (rpLoadAdjustment <$> paramM)
  (fForecast, vForecast) <- mopt (selectFieldList forecastDirOptions) "Forecast Profile" (rpForecastDir <$> paramM)
  (fForecastInOut, vForecastInOut) <- mopt (selectFieldList forecastOptions) "Forecast Mode" (rpForecastInOut <$> paramM)
  (fForecastStart, vForecastStart) <- mopt dayField "start" (Just $ rpForecastStart =<< paramM )
  (fColourMode, vColourMode) <- mreq (selectField optionsEnum) "Chart Colour Mode" (rpColourMode <$> paramM)
  (fGroupTrace, vGroupTrace) <- mopt (selectField optionsEnum) "Trace Group Mode" (rpTraceGroupMode <$> paramM)
  let fields = [ Left $ mapM_ renderField [vFrom, vTo, vPeriod, vPeriodN]
               , Left $ mapM_ renderField [vStockFilter, vCategoryToFilter, vCategoryFilter]
               , Right ("panel-rupture" :: Text, [wPanel, wBand, wSerie , vColRupture ])
               , Right ("panel-trace", [ wTrace1, wTrace2, wTrace3 ])
               , Left $ mapM_ renderField [vSales, vOrder]
               , Left $ mapM_ renderField [vPurchases, vPurchasesDateOffset, vPOrders ]
               , Left $ mapM_ renderField [vAdjustment]
               , Left $ mapM_ renderField [vForecast, vForecastInOut, vForecastStart]
               , Left $ mapM_ renderField [vColourMode, vGroupTrace]
               ]
  let form = [whamlet|#{extra}|] >> mapM_ (either inline dragablePanel) fields >> dragAndDropW
      inline w = [whamlet| <div.well.well-sm.form-inline>^{w}|]
      dragablePanel (panelId, ws) =  [whamlet|
           <div.well style="background:white" id="#{panelId}">
             $forall w <- ws
               <div.well.well-sm.form-inline
                      style="cursor:move"
                      draggable="true"
                      ondragstart="startDrag('#{panelId}',event)"
                      ondragend="endDrag('#{panelId}',event)"
                      ondragover="dragOver('#{panelId}',event)"
                      ondragenter="dragEnter('#{panelId}',event)"
                      ondragLeave="dragLeave('#{panelId}',event)"
                      ondrop="drop('#{panelId}',event)"
               >
                  <span.glyphicon.glyphicon-move>
                  ^{w}
                                  |]
      dragAndDropW = $(widgetFile "Items/report-dnd")
      -- julius = $(juliusFileReload "/home/max/devel/mae/Fames/src/Handler/Items/Reports/dnd-form.julius")
      -- cassius = $(cassiusFileReload "/home/max/devel/mae/Fames/src/Handler/Items/Reports/dnd-form.cassius")
      report = mkReport today deduceTax fFrom  fTo
          fPeriod  fPeriodN
          fCategoryToFilter  fCategoryFilter
          fStockFilter  fPanel  fBand  fSerie
          fColRupture  fTrace1  fTrace2  fTrace3
          fSales  fOrder fPurchases fPurchasesDateOffset fPOrders  fAdjustment (liftA3 (,,) fForecast fForecastInOut fForecastStart) fColourMode fGroupTrace
  return (report , form)
 
  
{-# NOINLINE mkReport #-}
mkReport today deduceTax fFrom  fTo
   fPeriod  fPeriodN
   fCategoryToFilter  fCategoryFilter
   fStockFilter  fPanel  fBand  fSerie
   fColRupture  fTrace1  fTrace2  fTrace3
   fSales  fOrder fPurchases fPurchasesDateOffset fPOrders  fAdjustment fForecast
   fColourMode fGroupTrace =
  ReportParam <$> pure today <*> pure deduceTax <*> fFrom <*> fTo
  <*> fPeriod <*> fPeriodN
  <*> fCategoryToFilter <*> fCategoryFilter
  <*> fStockFilter <*> fPanel <*> fBand <*> fSerie
  <*> fColRupture <*> fTrace1 <*> fTrace2 <*> fTrace3
  <*> fSales <*> fOrder
  <*> fPurchases <*> fPurchasesDateOffset <*> fPOrders
  <*> fAdjustment
  <*> fForecast
  <*> fColourMode <*> fGroupTrace
  -- noneOption, amountOutOption, amountInOption :: ToJSON a =>  (Text, [(QPrice -> Amount, a -> [(Text, Value)], RunSum)])
traceForm :: Int -> Bool -> String -> Maybe DataParams -> MForm Handler (FormResult DataParams, Widget)
traceForm traceN nomode suffix p = do
  let
    dataTypeOptions = optionsPairs [(drop 2 (tshow qtype), qtype) | qtype <- [minBound..maxBound]]
    dataValueOptions = map (\(t, tps'runsum) -> (t, Identifiable (t, (map mkTraceParam tps'runsum))))
                                    [ noneOption
                                    , ("QA-Sales", quantityAmountStyle traceN Outward)
                                    , ("CumulAmount (Out)" ,   [(qpAmount Outward, VAmount, cumulAmountStyle traceN, RunSum)] )
                                    , ("CumulAmount Backward (Out)" ,   [(qpAmount Outward, VAmount, cumulAmountStyle traceN, RunSumBack)] )
                                    , ("CumulAmount (In)" ,   [(qpAmount Inward, VAmount, cumulAmountStyle traceN, RunSum)] )
                                    , ("Stock" ,   [(qpQty Inward, VQuantity , quantityStyle traceN `nameStyle` "Stock"
                                                    , RunSum)] )
                                    , amountOutOption traceN
                                    , amountInOption traceN
                                    , ("Amount (Bare)",   [(_qpAmount, VAmount, amountStyle traceN, RSNormal)])
                                    , ("CumulAmount (Bare)" ,   [(_qpAmount, VAmount, cumulAmountStyle traceN, RunSum)] )
                                    , ("Quantity (Out)",  [(qpQty Outward, VQuantity, quantityStyle traceN, RSNormal)])
                                    , ("Quantity (In)",   [(qpQty Inward, VQuantity, quantityStyle traceN, RSNormal)])
                                    , ("Quantity (Bare)", [(_qpQty, VQuantity,           quantityStyle traceN, RSNormal)] )
                                    , ("Avg Price",       [(qpAveragePrice, VPrice,   priceStyle, RSNormal)])
                                    , ("Min Price",       [(qpMinPrice, VPrice,       priceStyle, RSNormal)])
                                    , ("Max Price",       [(qpMaxPrice, VPrice,       priceStyle, RSNormal)])
                                    , ("Price Band",          pricesStyle)
                                    ]
  (tType, vType) <-  mreq (selectField dataTypeOptions)
                           (fromString $ "type" <> suffix)
                           (dpDataType <$> p) 
  (tParam, vParam) <- mreq (selectFieldList dataValueOptions)
                                            (fromString $ "value" <> suffix)
                                            (dpDataTraceParams <$> p) 
  (tNormTarget, vNormTarget) <- mopt (selectField optionsEnum) (fromString $ "normTarget" <> suffix) (nmTarget <$$> dpDataNorm <$> p)
  (tNormMargin, vNormMargin) <- mopt (selectField optionsEnum) (fromString $ "normMargin" <> suffix) (nmMargin <$$> dpDataNorm <$> p)
  let form =  mapM_ renderField $ [vType, vParam ] <> if nomode then [] else [ vNormMargin, vNormTarget]
      norm = liftA2 NormalizeMode <$> tNormMargin <*> tNormTarget
  return (DataParams <$> tType <*> tParam <*> norm, form )

ruptureForm :: [(Text, Column)] -> String -> Maybe ColumnRupture -> MForm Handler (FormResult ColumnRupture, Widget)
ruptureForm colOptions title paramM = do
  (fColumn, vColumn) <- mopt (selectFieldList colOptions) (fromString title)  (cpColumn <$> paramM) 
  (fSortBy, wSortBy) <- traceForm 1 True ("" ) (cpSortBy <$> paramM)
  (fRankMode, vRankMode) <- mopt (selectField optionsEnum) ("" ) (cpRankMode <$> paramM)
  (fLimitTo, vLimitTo) <- mopt intField "Limit To" (Just $ cpLimitTo =<< paramM)
  (fReverse, vReverse) <- mreq boolField "reverse" (cpReverse <$> paramM)

  let form = mapM_ (either renderField id) [ Left vColumn
                                           , Right [whamlet|<div.form-group style="margin-bottom: 5px">
                                                                             <label style="width:100%; text-align:center">Sort By
                                                                             <div.form-group>^{wSortBy}
                                                                             |]
                                           , Left vRankMode
                                           , Left vLimitTo
                                           , Left vReverse
                                           ]
  return (ColumnRupture <$> fColumn <*>  fSortBy <*> fRankMode <*> fLimitTo <*> fReverse, form)
  

getForecastDirOptions :: Handler [(Text, FilePath)]
getForecastDirOptions = getSubdirOptions appForecastProfilesDir

-- * Handler

getItemsReportR :: Maybe ReportMode -> Handler TypedContent
getItemsReportR mode = do
  actionM <- lookupGetParam "action"
  case actionM of
    Nothing -> getItemsReportR' mode
    Just _ -> postItemsReportR mode
getItemsReportR' mode = do
  today <- todayH
  deduceTax <- appReportDeduceTax <$> getsYesod appSettings 
  (_, (band, serie, timeColumn)) <- getColsWithDefault
  let -- emptyRupture = ColumnRupture Nothing emptyTrace Nothing Nothing False
      bestSales = DataParams QPSales (mkIdentifialParam $ amountInOption 1) Nothing
      sales = DataParams QPSales (mkIdentifialParam $ amountOutOption 1) Nothing
      emptyTrace = DataParams QPSales (mkIdentifialParam noneOption) Nothing
      past = calculateDate (AddMonths (-3)) today
      tomorrow = calculateDate (AddDays 1) today
  
      defaultReportParam = case mode of
        Just ReportChart -> ReportParam
                            today
                            deduceTax
                           (Just past) --  rpFrom :: Maybe Day
                           (Just tomorrow) --  rpTo :: Maybe Day
                           Nothing --  rpPeriod
                           Nothing --  rpNumberOfPeriods
                           Nothing -- rpToCategoryToFilter
                           Nothing -- rpToCategoryFilter
                           Nothing --  rpStockFilter :: Maybe FilterExpression
                           emptyRupture   -- rpPanelRupture :: ColumnRupture
                           (ColumnRupture (Just band)  bestSales (Just RMResidual) (Just 20) False)--  rpBand :: ColumnRupture
                           (ColumnRupture (Just serie)  bestSales (Just RMResidual) (Just 20) False)--  rpSerie :: ColumnRupture
                           (emptyRupture {cpColumn = Just timeColumn})
                           sales --  rpDataParam :: DataParams
                           emptyTrace --  rpDataParam2 :: DataParams
                           emptyTrace --  rpDataParam3 :: DataParams
                           (Just SSalesOnly) Nothing
                           True Nothing Nothing -- purchases
                           True (Nothing, Nothing, Nothing)
                           minBound Nothing
        _ -> ReportParam   today
                           deduceTax
                           (Just past) --  rpFrom :: Maybe Day
                           (Just tomorrow) --  rpTo :: Maybe Day
                           Nothing --  rpPeriod
                           Nothing --  rpNumberOfPeriods
                           Nothing -- rpToCategoryToFilter
                           Nothing -- rpToCategoryFilter
                           Nothing --  rpStockFilter :: Maybe FilterExpression
                           (ColumnRupture (Just band) bestSales (Just RMResidual) (Just 20) False)--  rpBand :: ColumnRupture
                           (ColumnRupture (Just serie) bestSales (Just RMResidual)  (Just 20) False)--  rpSerie :: ColumnRupture
                           emptyRupture   -- rpPanelRupture :: ColumnRupture
                           (emptyRupture {cpColumn = Just timeColumn}) --  rpColumnRupture :: Column
                           sales --  rpDataParam :: DataParams
                           emptyTrace --  rpDataParam2 :: DataParams
                           emptyTrace --  rpDataParam3 :: DataParams
                           (Just SSalesOnly) Nothing
                           True Nothing Nothing -- Purchases
                           True (Nothing, Nothing, Nothing)
                           minBound Nothing

  renderReportForm ItemsReportR mode (Just defaultReportParam) ok200 Nothing

getItemsReport2R :: Maybe ReportMode -> Handler Html
getItemsReport2R mode = do
  redirect $ ItemsR (ItemsReportR (Just ReportChart))
  -- renderReportForm ItemsReport2R mode Nothing ok200 Nothing

getItemsReport3R mode = do 
  renderReportForm ItemsReport3R mode Nothing ok200 Nothing

runFormAndGetAction form = do
  fromPost@((resp, _), _) <- runFormPostNoToken form
  traceShowM resp
  case resp of
    FormMissing -> (,) <$> runFormGet form <*> lookupGetParam "action"
    _ -> do
        action <- lookupPostParam "action"
        return (fromPost, action)


-- | Adjust if needed date param so that they can be dynamic
-- i.e. calculated on the fly for example to get last 3 months from today
-- It uses some special http field not part of the form.
-- They are meant to be used when storing url
-- for example to get report using the current day (and the last 3 months)
-- one needs to add to url
-- `calculate-from=[AddMonths (-3)]&calculate-to=[AddMonths 0]`
adjustParamDates :: ReportParam -> Handler ReportParam
adjustParamDates param = do
  newFrom <- update (rpFrom param) "calculate-from"
  newTo <- update (rpTo param) "calculate-to"
  return param {rpFrom = newFrom, rpTo = newTo }
  where update day field = do
          calc <- lookupGetParam field
          return $ case fmap readMay calc of
                     Just (Just cs@(_:_)) -> Just $ foldl'(flip ($)) (rpToday param) (map calculateDate cs)
                     Just Nothing -> error $ "Can't parse " ++ unpack field ++  " : " ++ show calc
                     _ -> day
          
  

-- | React on post. Actually used GET So should be removed
postItemsReportR = postItemsReportFor ItemsReportR 
postItemsReportFor route mode = do
  today <- todayH
  cols <- getCols
  (formRan, actionM) <- runFormAndGetAction (reportForm cols Nothing)
  let ((resp, _), _) = formRan
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param0 -> do
      param <- adjustParamDates param0
      let panel = rpPanelRupture param
          band = rpBand param
          serie = rpSerie param
          col = rpColumnRupture param
          -- for table, the exact meaning of the rupture doesn't matter
          tableGrouper = panel : filter (isJust . cpColumn) [band, serie]
          grouper = [ panel, band, serie, col]
      case readMay =<< actionM of

        Just ReportCsv -> do
              result <- itemReportWithRank param grouper (fmap snd) --  (fmap (fmap (summarize . map snd)))
              let source = yieldMany (map (<> "\n") (toCsv param result))
              setAttachment . fromStrict $ "items-report-" <> (tshowM $ colName <$> (cpColumn $ rpPanelRupture param)) <> "-"
                                              <> (tshowM $ colName <$> (cpColumn $ rpBand param)) <> ".csv"
              respondSource "text/csv" (source =$= mapC toFlushBuilder)
        Just ReportRaw -> do
             error "FIXME" -- itemToCsv param panel band
        _ -> do
              report <- case mode of
                    Just ReportChart -> itemReportWithRank param grouper (chartProcessor param)
                    Just ReportPivot -> itemReport param pivotProcessor
                    Just ReportBubble -> itemReportWithRank param grouper (bubbleProcessor param)
                    Just ReportScatter -> itemReportWithRank param grouper (scatterProcessor param)
                    _ -> itemReportWithRank param tableGrouper (tableProcessor param)
                    -- _                -> itemReportWithRank param grouper (pivotProcessorXXX param)
              renderReportForm route mode (Just param) ok200 (Just report)


postItemsReport2R :: Maybe ReportMode -> Handler TypedContent
postItemsReport2R mode = postItemsReportFor ItemsReport2R  (mode <|> Just ReportCsv)

postItemsReport3R :: Maybe ReportMode -> Handler TypedContent
postItemsReport3R = postItemsReportFor ItemsReport3R 
-- ** Renders

renderReportForm :: (Maybe ReportMode -> ItemsR)
                 -> Maybe ReportMode
                 -> Maybe ReportParam
                 -> Status
                 -> Maybe Widget
                 -> Handler TypedContent
renderReportForm  route modeM paramM status resultM = do
  cols <- getCols
  (repForm, repEncType) <- generateFormGet' $ reportForm cols paramM
  let buttons = [(ReportCsv, "Export To Csv" :: Text), (ReportRaw, "Export Raw CSV")]
      navs = ([minBound..maxBound] :: [ReportMode]) List.\\ map fst buttons
      -- ^ We use a button instead for the CSV
      mode = fromMaybe ReportTable modeM
      navClass nav = if mode == nav then "active" else "" :: Html
      fay = $(fayFile "ItemsReport")
      -- in theory we only need plotly when display a chart
      -- but it is need to be loaded on the first page, otherwise
      -- Ajax call with plot won't work
      -- plotly = addScriptRemote "https://cdn.plot.ly/plotly-latest.min.js"
      widget = [whamlet|
    <form #items-report-form role=form method=get action="@{ItemsR (route modeM)}" enctype="#{repEncType}">
        $maybe result <- resultM
          <ul.nav.nav-tabs>
            $forall nav <- navs
              <li class=#{navClass nav}>
                $if nav == ReportCsv
                  <a href="@{ItemsR (ItemsReportR (Just nav))}"> #{splitSnake $ drop 6 $ tshow nav}
                $else
                  <a.view-mode href="#" data-url="@{ItemsR (ItemsReportR (Just nav))}"> #{splitSnake $ drop 6 $ tshow nav}
          <div#items-report-result>
            ^{result}
      <div.well>
        ^{repForm}
        <button.btn type="submit" name="action" value="Submit"> Submit
        $forall (btn, title) <- buttons
          <button.btn.btn-info type="submit" name="action" value="#{tshow btn}">#{title}
    <div.panel.panel-info>
      
      #{reportDoc}
                        |]
  cacheSeconds (23*3600)
  selectRep $ do
    provideRep $ do
      html <- sendResponseStatus status =<< defaultLayout (toWidget commonCss >> widget >> fay) 
      return (html :: Html)
    provideRep $ do -- Ajax. return result
      div <- widgetToPageContent (fromMaybe (return ()) resultM)
      html <- withUrlRenderer (pageBody div)
      returnJson (renderHtml html)

reportDoc = [shamlet|
<div.panel-heading>
  <h3>
    <span.data-toggler.collapsed data-toggle=collapse data-target="#report-doc"> Report Doc
<div.panel-body>
  <div.pre.collapse id=report-doc>
      #{reportDoc'}
|]


reportDoc' = [shamlet|
      <table.table.table-striped.table-hover>
        <thead>
          <tr>
            <th scope="col" >&#xa0;
            <th scope="col" >Table
            <th scope="col" >Chart
            <th scope="col" >Pivot
            <th scope="col" >Bubble
            <th scope="col" >Scatter
        <tbody>
          <tr>
            <td >Panel
            <td >Panel
            <td >Panel
            <td >Panel
            <td >Panel
            <td >Panell

          <tr>
            <td >Band
            <td >Row
            <td >Band
            <td >Band
            <td >Band
            <td >Band

          <tr>
            <td >Serie
            <td >Subrow
            <td >Line
            <td >Row
            <td >Y/Row
            <td >Colour/Label

          <tr>
            <td >Column
            <td >N/A
            <td >X
            <td >X/Column
            <td >X/Column
            <td >Point/Label

          <tr>
            <td >Trace 1
            <td >N/A
            <td >Line
            <td >Z/Subrow
            <td >Z/Size
            <td >X

          <tr>
            <td >Trace 2
            <td >N/A
            <td >Line
            <td >Z/Subrow
            <td >Z/Colour
            <td >Y

          <tr>
            <td >Trace 3
            <td >N/A
            <td >Line
            <td >Z/Subrow
            <td >&#xa0;
            <td >Z/Size
|]
