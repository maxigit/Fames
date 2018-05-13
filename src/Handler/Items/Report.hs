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
import Data.Time (addGregorianMonthsClip)

-- * Form
reportForm :: [Column] -> Maybe ReportParam -> Html -> MForm Handler (FormResult ReportParam, Widget)
reportForm cols paramM extra = do
  let colOptions = [(colName c,c) | c <- cols]
  (fFrom, vFrom) <- mopt dayField "from" (Just $ rpFrom =<< paramM )
  (fTo, vTo) <- mopt dayField "to" (Just $ rpTo =<< paramM)
  (fStockFilter, vStockFilter) <- mopt filterEField  "sku" (Just $ rpStockFilter =<< paramM)
  (fPanel, wPanel) <- ruptureForm colOptions "Panel" (rpPanelRupture <$> paramM)
  (fBand, wBand) <- ruptureForm colOptions "Band" (rpBand <$> paramM)
  (fSerie, wSerie) <- ruptureForm colOptions "Series" (rpSerie <$> paramM)
  (fColRupture, vColRupture) <- mreq (selectFieldList colOptions)  "column rupture" (rpColumnRupture <$> paramM) 
  (fTrace1, wTrace1) <- traceForm "" (rpTraceParam <$> paramM)
  (fTrace2, wTrace2) <- traceForm " 2" (rpTraceParam2 <$> paramM)
  (fTrace3, wTrace3) <- traceForm " 3"(rpTraceParam3 <$> paramM)
  let fields = [ Right (renderField vFrom >>  renderField vTo >> renderField vStockFilter)
               , Right wPanel, Right wBand, Right wSerie
               , Left vColRupture
               , Right wTrace1, Right wTrace2, Right wTrace3]
  let form = [whamlet|#{extra}|] >> mapM_ (either renderField inline) fields
      inline w = [whamlet| <div.form-inline>^{w}|]
      report = ReportParam <$> fFrom <*> fTo <*> fStockFilter <*> fPanel <*> fBand <*> fSerie
                           <*> fColRupture <*> fTrace1 <*> fTrace2 <*> fTrace3
  return (report, form)
 
-- noneOption, amountOutOption, amountInOption :: ToJSON a =>  (Text, [(QPrice -> Amount, a -> [(Text, Value)], RunSum)])
noneOption = ("None" :: Text, [])
amountOutOption = ("Amount (Out)" ,   [(qpAmount Outward, amountStyle, RSNormal)] )
amountInOption = ("Amount (In)",     [(qpAmount Inward,  amountStyle, RSNormal)])
mkIdentifialParam  (t, tp'runsumS) = Identifiable (t, map TraceParam tp'runsumS)


traceForm :: String -> Maybe TraceParams -> MForm Handler (FormResult TraceParams, Widget)
traceForm suffix p = do
  let
    dataTypeOptions = optionsPairs [(drop 2 (tshow qtype), qtype) | qtype <- [minBound..maxBound]]
    dataValueOptions = map (\(t, tps'runsum) -> (t, Identifiable (t, (map TraceParam tps'runsum))))
                                    [ noneOption
                                    , ("QA-Sales", quantityAmountStyle Outward)
                                    , ("CumulAmount (Out)" ,   [(qpAmount Outward, cumulAmountStyle, RunSum)] )
                                    , ("CumulAmount Backward (Out)" ,   [(qpAmount Outward, cumulAmountStyle, RunSumBack)] )
                                    , ("Stock" ,   [(qpQty Inward, quantityStyle, RunSum)] )
                                    , amountOutOption
                                    , amountInOption
                                    , ("Amount (Bare)",   [(_qpAmount,        amountStyle, RSNormal)])
                                    , ("Quantity (Out)",  [(qpQty Outward,    quantityStyle, RSNormal)])
                                    , ("Quantity (In)",   [(qpQty Inward,     quantityStyle, RSNormal)])
                                    , ("Quantity (Bare)", [(_qpQty,           quantityStyle, RSNormal)] )
                                    , ("Avg Price",       [(qpAveragePrice,   priceStyle, RSNormal)])
                                    , ("Min Price",       [(qpMinPrice,       priceStyle, RSNormal)])
                                    , ("Max Price",       [(qpMaxPrice,       priceStyle, RSNormal)])
                                    , ("Price Band",          pricesStyle)
                                    ]
  (tType, vType) <-  mreq (selectField dataTypeOptions)
                           (fromString $ "type" <> suffix)
                           (tpDataType <$> p) 
  (tParam, vParam) <- mreq (selectFieldList dataValueOptions)
                                            (fromString $ "value" <> suffix)
                                            (tpDataParams <$> p) 
  let form =  mapM_ renderField [vType, vParam]
  return (TraceParams <$> tType <*> tParam, form )

ruptureForm :: [(Text, Column)] -> String -> Maybe ColumnRupture -> MForm Handler (FormResult ColumnRupture, Widget)
ruptureForm colOptions title paramM = do
  (fColumn, vColumn) <- mopt (selectFieldList colOptions) (fromString title)  (cpColumn <$> paramM) 
  (fSortBy, wSortBy) <- traceForm ("" ) (cpSortBy <$> paramM)
  (fRankMode, vRankMode) <- mopt (selectField optionsEnum) ("" ) (cpRankMode <$> paramM)
  (fLimitTo, vLimitTo) <- mopt intField "Limit To" (Just $ cpLimitTo =<< paramM)

  let form = mapM_ (either renderField id) [ Left vColumn
                                           , Right [whamlet|<div.form-group style="margin-bottom: 5px">
                                                                             <label style="width:100%; text-align:center">Sort By
                                                                             <div.form-group>^{wSortBy}
                                                                             |]
                                           , Left vRankMode
                                           , Left vLimitTo]
  return (ColumnRupture <$> fColumn <*>  fSortBy <*> fRankMode <*> fLimitTo, form)
  
-- * Handler

getItemsReportR :: Maybe ReportMode -> Handler TypedContent
getItemsReportR mode = do
  today <- utctDay <$> liftIO getCurrentTime
  (_, (band, serie, timeColumn)) <- getColsWithDefault
  let emptyRupture = ColumnRupture Nothing emptyTrace Nothing Nothing
      bestSales = TraceParams QPSales (mkIdentifialParam amountInOption)
      sales = TraceParams QPSales (mkIdentifialParam amountOutOption)
      emptyTrace = TraceParams QPSales (mkIdentifialParam noneOption)
      past = addGregorianMonthsClip (-6) today
  
      defaultReportParam = case mode of
        Just ReportChart -> ReportParam
                           (Just past) --  rpFrom :: Maybe Day
                           Nothing --  rpTo :: Maybe Day
                           Nothing --  rpStockFilter :: Maybe FilterExpression
                           emptyRupture   -- rpPanelRupture :: ColumnRupture
                           (ColumnRupture (Just band)  bestSales (Just AsAverage) (Just 20))--  rpBand :: ColumnRupture
                           (ColumnRupture (Just serie)  bestSales (Just AsAverage) (Just 20))--  rpSerie :: ColumnRupture
                           timeColumn --  rpColumnRupture :: Column
                           sales --  rpTraceParam :: TraceParams
                           emptyTrace --  rpTraceParam2 :: TraceParams
                           emptyTrace --  rpTraceParam3 :: TraceParams
        _ -> ReportParam
                           (Just past) --  rpFrom :: Maybe Day
                           Nothing --  rpTo :: Maybe Day
                           Nothing --  rpStockFilter :: Maybe FilterExpression
                           (ColumnRupture (Just band) bestSales (Just AsAverage) (Just 20))--  rpBand :: ColumnRupture
                           (ColumnRupture (Just serie) bestSales (Just AsAverage)  (Just 20))--  rpSerie :: ColumnRupture
                           emptyRupture   -- rpPanelRupture :: ColumnRupture
                           timeColumn --  rpColumnRupture :: Column
                           sales --  rpTraceParam :: TraceParams
                           emptyTrace --  rpTraceParam2 :: TraceParams
                           emptyTrace --  rpTraceParam3 :: TraceParams

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
          grouper = [ panel, band, serie
                    , ColumnRupture  (Just col) (TraceParams QPSummary (Identifiable ("Column", []))) Nothing Nothing
                    ]
      case readMay =<< actionM of

        Just ReportCsv -> do
              result <- itemReport param grouper id --  (fmap (fmap (summarize . map snd)))
              let source = yieldMany (map (<> "\n") (toCsv param result))
              setAttachment . fromStrict $ "items-report-" <> (tshowM $ colName <$> (cpColumn $ rpPanelRupture param)) <> "-"
                                              <> (tshowM $ colName <$> (cpColumn $ rpBand param)) <> ".csv"
              respondSource "text/csv" (source =$= mapC toFlushBuilder)
        Just ReportRaw -> do
             error "FIXME" -- itemToCsv param panel band
        _ -> do
              report <- case mode of
                    Just ReportChart -> itemReport param grouper (chartProcessor param)
                    _ -> itemReport param tableGrouper tableProcessor
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
      html <- sendResponseStatus status =<< defaultLayout (widget >> fay) 
      return (html :: Html)
    provideRep $ do -- Ajax. return result
      div <- widgetToPageContent (fromMaybe (return ()) resultM)
      html <- withUrlRenderer (pageBody div)
      returnJson (renderHtml html)
