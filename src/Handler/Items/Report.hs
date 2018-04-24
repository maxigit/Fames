module Handler.Items.Report
( getItemsReportR
, postItemsReportR
) where

import Import
import Items.Types
import Handler.Items.Reports.Common
import Handler.Items.Common
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,)

-- * Type
data ReportParam = ReportParam

-- * Form
reportForm paramM = let
  form = pure ReportParam
  in  renderBootstrap3 BootstrapBasicForm form
 
-- * Handler
getItemsReportR :: Maybe ReportMode -> Handler Html
getItemsReportR mode = do
  renderReportForm mode Nothing ok200 Nothing

postItemsReportR mode = do
  today <- utctDay <$> liftIO getCurrentTime
  ((resp, formW), enctype) <- runFormPost (reportForm Nothing)
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> do
      report <- itemReport tkCategory (Just . slidingYearShow today . tkDay)
      renderReportForm mode (Just param) ok200 (Just report)


-- ** Renders

renderReportForm  :: Maybe ReportMode -> Maybe ReportParam  -> Status -> Maybe Widget -> Handler Html
renderReportForm  modeM paramM status resultM = do
  (repForm, repEncType) <- generateFormPost $ reportForm paramM
  let navs = [minBound..maxBound] :: [ReportMode]
      mode = fromMaybe ReportChart modeM
      navClass nav = if mode == nav then "active" else "" :: Html
      fay = $(fayFile "ItemsReport")
  defaultLayout $ fay >> [whamlet|
    <form #items-report-form role=form method=post action="@{ItemsR (ItemsReportR modeM)}" enctype="#{repEncType}">
      <div.well>
        ^{repForm}
        <button.btn type="submit">Submit
      <ul.nav.nav-tabs>
      <div#items-report-result>
        $maybe result <- resultM
          $forall nav <- navs
            <li class=#{navClass nav}>
              <a.view-mode href="#" data-url="@{ItemsR (ItemsReportR (Just nav))}"> #{drop 6 $ tshow nav}
          ^{result}
                        |]


  
