module Handler.Items.Report
( getItemsReportR
, postItemsReportR
) where

import Import
import Handler.Table
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,)
import Items
import FA

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
  ((resp, formW), enctype) <- runFormPost (reportForm Nothing)
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> do
      report <- return $ return ()
      renderReportForm mode (Just param) ok200 (Just report)


-- ** Renders

renderReportForm  :: Maybe ReportMode -> Maybe ReportParam  -> Status -> Maybe Widget -> Handler Html
renderReportForm  modeM paramM status resultM = do
  (repForm, repEncType) <- generateFormPost $ reportForm paramM
  defaultLayout [whamlet|
    <form #item-report role=form method=post action="@{ItemsR (ItemsReportR modeM)}" enctype="#{repEncType}">
      <div.well>
        ^{repForm}
        <button.btn type="submit">Submit
        $maybe result <- resultM
          ^{result}
                        |]


  
