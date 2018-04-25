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
import qualified FA as FA


-- * Form
reportForm :: [Column] -> Maybe ReportParam -> _
reportForm cols paramM = let
  colOptions = [(colName c,c) | c <- cols]
  form = ReportParam
    <$> (aopt dayField "from" (Just $ rpFrom =<< paramM ))
    <*> (aopt dayField "to" (Just $ rpTo =<< paramM))
    <*> (aopt filterEField  "sku" (Just $ rpStockFilter =<< paramM))
    <*> (areq (selectFieldList colOptions)  "row rupture" (rpRowRupture <$> paramM) )
    <*> (areq (selectFieldList colOptions)  "column rupture" (rpColumnRupture <$> paramM) )
  in  renderBootstrap3 BootstrapBasicForm form
 
-- * Handler

getItemsReportR :: Maybe ReportMode -> Handler TypedContent
getItemsReportR mode = do
  renderReportForm ItemsReportR mode Nothing ok200 Nothing

getItemsReport2R mode = do
  renderReportForm ItemsReport2R mode Nothing ok200 Nothing

getItemsReport3R mode = do 
  renderReportForm ItemsReport3R mode Nothing ok200 Nothing

postItemsReportR = postItemsReportFor ItemsReportR 
postItemsReportFor route mode = do
  today <- utctDay <$> liftIO getCurrentTime
  cols <- getCols
  ((resp, formW), enctype) <- runFormPost (reportForm cols Nothing)
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> do
      (report, result) <- itemReport param (rpRowRupture param) (rpColumnRupture param)
      case mode of
        Just ReportCsv -> do
              let source = yieldMany (map (<> "\n") (toCsv result))
              respondSource "text/csv" (source =$= mapC toFlushBuilder)
        _ -> do
              renderReportForm route mode (Just param) ok200 (Just report)

postItemsReport2R :: Maybe ReportMode -> Handler TypedContent
postItemsReport2R = postItemsReportFor ItemsReport2R 

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
  let navs = [minBound..maxBound] :: [ReportMode]
      mode = fromMaybe ReportChart modeM
      navClass nav = if mode == nav then "active" else "" :: Html
      fay = $(fayFile "ItemsReport")
      widget = [whamlet|
    <form #items-report-form role=form method=post action="@{ItemsR (route modeM)}" enctype="#{repEncType}">
      <div.well>
        ^{repForm}
        <button.btn type="submit">Submit
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
      html <- sendResponseStatus status =<< defaultLayout (widget >> fay) 
      return (html :: Html)
    provideRep $ do -- Ajax. return result
      div <- widgetToPageContent (fromMaybe (return ()) resultM)
      html <- withUrlRenderer (pageBody div)
      returnJson (renderHtml html)
