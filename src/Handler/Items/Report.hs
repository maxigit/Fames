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
  today <- utctDay <$> liftIO getCurrentTime
  categories <- lift $ categoriesH

  let colOptions = [(colName c,c) | c <- cols]
      categoryOptions = [(cat, cat) | cat <-categories ]
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
  (fTrace1, wTrace1) <- traceForm False "" (rpTraceParam <$> paramM)
  (fTrace2, wTrace2) <- traceForm False " 2" (rpTraceParam2 <$> paramM)
  (fTrace3, wTrace3) <- traceForm False " 3"(rpTraceParam3 <$> paramM)
  (fSales, vSales) <- mreq checkBoxField "Sales" (rpLoadSales <$> paramM)
  (fPurchases, vPurchases) <- mreq checkBoxField "Purchases" (rpLoadPurchases <$> paramM)
  (fAdjustment, vAdjustment) <- mreq checkBoxField "Adjustment" (rpLoadAdjustment <$> paramM)
  let fields = [ Right $ mapM_ renderField [vFrom, vTo, vPeriod, vPeriodN]
               , Right $ mapM_ renderField [vStockFilter, vCategoryToFilter, vCategoryFilter]
               , Right wPanel, Right wBand, Right wSerie
               , Right vColRupture
               , Right wTrace1, Right wTrace2, Right wTrace3
               , Right $ mapM_ renderField [vSales, vPurchases, vAdjustment]
               ]
  let form = [whamlet|#{extra}|] >> mapM_ (either renderField inline) fields
      inline w = [whamlet| <div.form-inline>^{w}|]
      report = mkReport today fFrom  fTo
          fPeriod  fPeriodN
          fCategoryToFilter  fCategoryFilter
          fStockFilter  fPanel  fBand  fSerie
          fColRupture  fTrace1  fTrace2  fTrace3
          fSales  fPurchases  fAdjustment
  return (report , form)
 
{-# NOINLINE mkReport #-}
mkReport today fFrom  fTo
   fPeriod  fPeriodN
   fCategoryToFilter  fCategoryFilter
   fStockFilter  fPanel  fBand  fSerie
   fColRupture  fTrace1  fTrace2  fTrace3
   fSales  fPurchases  fAdjustment = 
  ReportParam <$> pure today <*> fFrom <*> fTo
  <*> fPeriod <*> fPeriodN
  <*> fCategoryToFilter <*> fCategoryFilter
  <*> fStockFilter <*> fPanel <*> fBand <*> fSerie
  <*> fColRupture <*> fTrace1 <*> fTrace2 <*> fTrace3
  <*> fSales <*> fPurchases <*> fAdjustment
  -- noneOption, amountOutOption, amountInOption :: ToJSON a =>  (Text, [(QPrice -> Amount, a -> [(Text, Value)], RunSum)])
traceForm :: Bool -> String -> Maybe TraceParams -> MForm Handler (FormResult TraceParams, Widget)
traceForm nomode suffix p = do
  let
    dataTypeOptions = optionsPairs [(drop 2 (tshow qtype), qtype) | qtype <- [minBound..maxBound]]
    dataValueOptions = map (\(t, tps'runsum) -> (t, Identifiable (t, (map mkTraceParam tps'runsum))))
                                    [ noneOption
                                    , ("QA-Sales", quantityAmountStyle Outward)
                                    , ("CumulAmount (Out)" ,   [(qpAmount Outward, VAmount, cumulAmountStyle, RunSum)] )
                                    , ("CumulAmount Backward (Out)" ,   [(qpAmount Outward, VAmount, cumulAmountStyle, RunSumBack)] )
                                    , ("Stock" ,   [(qpQty Inward, VQuantity, quantityStyle, RunSum)] )
                                    , amountOutOption
                                    , amountInOption
                                    , ("Amount (Bare)",   [(_qpAmount, VAmount, amountStyle, RSNormal)])
                                    , ("Quantity (Out)",  [(qpQty Outward, VQuantity, quantityStyle, RSNormal)])
                                    , ("Quantity (In)",   [(qpQty Inward, VQuantity, quantityStyle, RSNormal)])
                                    , ("Quantity (Bare)", [(_qpQty, VQuantity,           quantityStyle, RSNormal)] )
                                    , ("Avg Price",       [(qpAveragePrice, VPrice,   priceStyle, RSNormal)])
                                    , ("Min Price",       [(qpMinPrice, VPrice,       priceStyle, RSNormal)])
                                    , ("Max Price",       [(qpMaxPrice, VPrice,       priceStyle, RSNormal)])
                                    , ("Price Band",          pricesStyle)
                                    ]
  (tType, vType) <-  mreq (selectField dataTypeOptions)
                           (fromString $ "type" <> suffix)
                           (tpDataType <$> p) 
  (tParam, vParam) <- mreq (selectFieldList dataValueOptions)
                                            (fromString $ "value" <> suffix)
                                            (tpDataParams <$> p) 
  (tNormTarget, vNormTarget) <- mopt (selectField optionsEnum) (fromString $ "normTarget" <> suffix) (nmTarget <$$> tpDataNorm <$> p)
  (tNormMargin, vNormMargin) <- mopt (selectField optionsEnum) (fromString $ "normMargin" <> suffix) (nmMargin <$$> tpDataNorm <$> p)
  let form =  mapM_ renderField $ [vType, vParam ] <> if nomode then [] else [ vNormMargin, vNormTarget]
      norm = liftA2 NormalizeMode <$> tNormMargin <*> tNormTarget
  return (TraceParams <$> tType <*> tParam <*> norm, form )

ruptureForm :: [(Text, Column)] -> String -> Maybe ColumnRupture -> MForm Handler (FormResult ColumnRupture, Widget)
ruptureForm colOptions title paramM = do
  (fColumn, vColumn) <- mopt (selectFieldList colOptions) (fromString title)  (cpColumn <$> paramM) 
  (fSortBy, wSortBy) <- traceForm True ("" ) (cpSortBy <$> paramM)
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
  
-- * Handler

getItemsReportR :: Maybe ReportMode -> Handler TypedContent
getItemsReportR mode = do
  today <- utctDay <$> liftIO getCurrentTime
  (_, (band, serie, timeColumn)) <- getColsWithDefault
  let -- emptyRupture = ColumnRupture Nothing emptyTrace Nothing Nothing False
      bestSales = TraceParams QPSales (mkIdentifialParam amountInOption) Nothing
      sales = TraceParams QPSales (mkIdentifialParam amountOutOption) Nothing
      emptyTrace = TraceParams QPSales (mkIdentifialParam noneOption) Nothing
      past = calculateDate (AddMonths (-3)) today
      tomorrow = calculateDate (AddDays 1) today
  
      defaultReportParam = case mode of
        Just ReportChart -> ReportParam
                            today
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
                           sales --  rpTraceParam :: TraceParams
                           emptyTrace --  rpTraceParam2 :: TraceParams
                           emptyTrace --  rpTraceParam3 :: TraceParams
                           True True True
        _ -> ReportParam   today
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
                           sales --  rpTraceParam :: TraceParams
                           emptyTrace --  rpTraceParam2 :: TraceParams
                           emptyTrace --  rpTraceParam3 :: TraceParams
                           True True True

  renderReportForm ItemsReportR mode (Just defaultReportParam) ok200 Nothing

getItemsReport2R :: Maybe ReportMode -> Handler Html
getItemsReport2R mode = do
  redirect $ ItemsR (ItemsReportR (Just ReportChart))
  -- renderReportForm ItemsReport2R mode Nothing ok200 Nothing

getItemsReport3R mode = do 
  renderReportForm ItemsReport3R mode Nothing ok200 Nothing

postItemsReportR = postItemsReportFor ItemsReportR 
postItemsReportFor route mode = do
  today <- utctDay <$> liftIO getCurrentTime
  actionM <- lookupPostParam "action"
  cols <- getCols
  ((resp, formW), enctype) <- runFormPost (reportForm cols Nothing)
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> do
      let panel = rpPanelRupture param
          band = rpBand param
          serie = rpSerie param
          col = rpColumnRupture param
          -- for table, the exact meaning of the rupture doesn't matter
          tableGrouper = panel : filter (isJust . cpColumn) [band, serie]
          grouper = [ panel, band, serie, col]
      case readMay =<< actionM of

        Just ReportCsv -> do
              result <- itemReportXXX param grouper (fmap snd) --  (fmap (fmap (summarize . map snd)))
              let source = yieldMany (map (<> "\n") (toCsv param result))
              setAttachment . fromStrict $ "items-report-" <> (tshowM $ colName <$> (cpColumn $ rpPanelRupture param)) <> "-"
                                              <> (tshowM $ colName <$> (cpColumn $ rpBand param)) <> ".csv"
              respondSource "text/csv" (source =$= mapC toFlushBuilder)
        Just ReportRaw -> do
             error "FIXME" -- itemToCsv param panel band
        _ -> do
              report <- case mode of
                    Just ReportChart -> itemReportXXX param grouper (chartProcessor param)
                    Just ReportPivot -> itemReport param pivotProcessor
                    Just ReportTable -> itemReportXXX param tableGrouper tableProcessor
                    _                -> itemReportXXX param grouper (pivotProcessorXXX param)
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
  (repForm, repEncType) <- generateFormPost $ reportForm cols paramM
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
    <form #items-report-form role=form method=post action="@{ItemsR (route modeM)}" enctype="#{repEncType}">
        $maybe result <- resultM
          <ul.nav.nav-tabs>
            $forall nav <- navs
              <li class=#{navClass nav}>
                $if nav == ReportCsv
                  <a href="@{ItemsR (ItemsReportR (Just nav))}"> #{drop 6 $ tshow nav}
                $else
                  <a.view-mode href="#" data-url="@{ItemsR (ItemsReportR (Just nav))}"> #{drop 6 $ tshow nav}
          <div#items-report-result>
            ^{result}
      <div.well>
        ^{repForm}
        <button.btn type="submit" name="action" value="Submit"> Submit
        $forall (btn, title) <- buttons
          <button.btn.btn-info type="submit" name="action" value="#{tshow btn}">#{title}
                        |]
  selectRep $ do
    provideRep $ do
      html <- sendResponseStatus status =<< defaultLayout (toWidget commonCss >> widget >> fay) 
      return (html :: Html)
    provideRep $ do -- Ajax. return result
      div <- widgetToPageContent (fromMaybe (return ()) resultM)
      html <- withUrlRenderer (pageBody div)
      returnJson (renderHtml html)
