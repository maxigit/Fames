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
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,)
import Text.Blaze.Html.Renderer.Text(renderHtml)
import qualified Data.List as List
import qualified FA as FA


-- * Form
reportForm :: [Column] -> Maybe ReportParam -> _
reportForm cols paramM = let
  colOptions = [(colName c,c) | c <- cols]
  dataTypeOptions = optionsPairs [(drop 2 (tshow qtype), qtype) | qtype <- [minBound..maxBound]]
  dataValueOptions = map (\(t, tps) -> (t, Identifiable (t, (map TraceParam tps))))
                                  [ ("None" :: Text, [])
                                  , ("Amount (Out)" ,   [(qpAmount Outward, amountStyle)] )
                                  , ("Amount (In)",     [(qpAmount Inward,  amountStyle)])
                                  , ("Amount (Bare)",   [(_qpAmount,        amountStyle)])
                                  , ("Quantity (Out)",  [(qpQty Outward,    quantityStyle)])
                                  , ("Quantity (In)",   [(qpQty Inward,     quantityStyle)])
                                  , ("Quantity (Bare)", [(_qpQty,           quantityStyle)] )
                                  , ("Avg Price",       [(qpAveragePrice,   priceStyle)])
                                  , ("Min Price",       [(qpMinPrice,       priceStyle)])
                                  , ("Max Price",       [(qpMaxPrice,       priceStyle)])
                                  , ("Price Band",          pricesStyle)
                                  ]
  traceForm suffix p = TraceParams <$> (areq (selectField dataTypeOptions)
                                             (fromString $ "data type" <> suffix)
                                             (tpDataType <$> p) )
                                   <*> (areq (selectFieldList dataValueOptions)
                                             (fromString $ "data value" <> suffix)
                                             (tpDataParams <$> p) )
  form = ReportParam
    <$> (aopt dayField "from" (Just $ rpFrom =<< paramM ))
    <*> (aopt dayField "to" (Just $ rpTo =<< paramM))
    <*> (aopt filterEField  "sku" (Just $ rpStockFilter =<< paramM))
    <*> (aopt (selectFieldList colOptions)  "Panel" (rpPanelRupture <$> paramM) )
    <*> (aopt (selectFieldList colOptions)  "Band" (rpBand <$> paramM) )
    <*> (aopt (selectFieldList colOptions)  "Series" (rpSerie <$> paramM) )
    <*> (areq (selectFieldList colOptions)  "column rupture" (rpColumnRupture <$> paramM) )
    <*> traceForm "" (rpTraceParam <$> paramM)
    <*> traceForm " 2" (rpTraceParam2 <$> paramM)
    <*> traceForm " 3"(rpTraceParam3 <$> paramM)
  in  renderBootstrap3 BootstrapBasicForm form
 
-- * Handler

getItemsReportR :: Maybe ReportMode -> Handler TypedContent
getItemsReportR mode = do
  renderReportForm ItemsReportR mode Nothing ok200 Nothing

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
      case readMay =<< actionM of
        Just ReportCsv -> do
              result <- itemReport param (rpPanelRupture param) (rpBand param) (fmap (fmap (summarize . map snd)))
              let source = yieldMany (map (<> "\n") (toCsv param result))
              setAttachment . fromStrict $ "items-report-" <> (tshowM $ colName <$> (rpPanelRupture param)) <> "-"
                                              <> (tshowM $ colName <$> rpBand param) <> ".csv"
              respondSource "text/csv" (source =$= mapC toFlushBuilder)
        Just ReportRaw -> do
             error "FIXME" -- itemToCsv param
        _ -> do
              let processor = case mode of
                    Just ReportTable -> tableProcessor
                    Just ReportChart -> chartProcessor param
                    _ -> tableProcessor
              report <- itemReport param (rpPanelRupture param) (rpBand param) processor
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
      plotly = addScriptRemote "https://cdn.plot.ly/plotly-latest.min.js"
      widget = [whamlet|
    <form #items-report-form role=form method=post action="@{ItemsR (route modeM)}" enctype="#{repEncType}">
      <div.well>
        ^{repForm}
        <button.btn type="submit" name="action" value="Submit"> Submit
        $forall (btn, title) <- buttons
          <button.btn.btn-info type="submit" name="action" value="#{tshow btn}">#{title}
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
                        |]
  selectRep $ do
    provideRep $ do
      html <- sendResponseStatus status =<< defaultLayout (widget >> fay >> plotly) 
      return (html :: Html)
    provideRep $ do -- Ajax. return result
      div <- widgetToPageContent (fromMaybe (return ()) resultM)
      html <- withUrlRenderer (pageBody div)
      returnJson (renderHtml html)
