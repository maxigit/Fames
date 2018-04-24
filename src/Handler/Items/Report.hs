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

-- * Type
data ReportParam = ReportParam

-- * Form
reportForm paramM = let
  form = pure ReportParam
  in  renderBootstrap3 BootstrapBasicForm form
 
-- * Handler
getItemsReportR :: Maybe ReportMode -> Handler TypedContent
getItemsReportR mode = do
  renderReportForm mode Nothing ok200 Nothing

getItemsReport2R mode = do
  renderReportForm mode Nothing ok200 Nothing

getItemsReport3R = getItemsReport2R

postItemsReportR mode = do
  today <- utctDay <$> liftIO getCurrentTime
  ((resp, formW), enctype) <- runFormPost (reportForm Nothing)
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> do
      (report, result) <- itemReport tkCategory (Just . slidingYearShow today . tkDay)
      case mode of
        Just ReportCsv -> do
              let source = yieldMany (map (<> "\n") (toCsv result))
              respondSource "text/csv" (source =$= mapC toFlushBuilder)
        _ -> do
              renderReportForm mode (Just param) ok200 (Just report)

postItemsReport2R :: Maybe ReportMode -> Handler TypedContent
postItemsReport2R mode = do
  today <- utctDay <$> liftIO getCurrentTime
  ((resp, formW), enctype) <- runFormPost (reportForm Nothing)
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> do
      (report, result) <- itemReport tkStyle (Just . slidingYearShow today . tkDay)
      case mode of
        Just ReportCsv -> do
              let source = yieldMany (map (<> "\n") (toCsv result))
              respondSource "text/csv" (source =$= mapC toFlushBuilder)
        _ -> do
              renderReportForm mode (Just param) ok200 (Just report)

postItemsReport3R :: Maybe ReportMode -> Handler TypedContent
postItemsReport3R mode = do
  today <- utctDay <$> liftIO getCurrentTime
  ((resp, formW), enctype) <- runFormPost (reportForm Nothing)
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> do
      (report, result) <- itemReport tkStyle tkVar
      case mode of
        Just ReportCsv -> do
              let source = yieldMany (map (<> "\n") (toCsv result))
              respondSource "text/csv" (source =$= mapC toFlushBuilder)
        _ -> do
              renderReportForm mode (Just param) ok200 (Just report)
-- ** Renders

renderReportForm  :: Maybe ReportMode -> Maybe ReportParam  -> Status -> Maybe Widget -> Handler TypedContent
renderReportForm  modeM paramM status resultM = do
  (repForm, repEncType) <- generateFormPost $ reportForm paramM
  let navs = [minBound..maxBound] :: [ReportMode]
      mode = fromMaybe ReportChart modeM
      navClass nav = if mode == nav then "active" else "" :: Html
      fay = $(fayFile "ItemsReport")
      widget = [whamlet|
    <form #items-report-form role=form method=post action="@{ItemsR (ItemsReportR modeM)}" enctype="#{repEncType}">
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
