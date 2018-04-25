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

-- * Type
data ReportParam = ReportParam
  { rpFrom :: Maybe Day
  , rpTo :: Maybe Day
  -- , rpCatFilter :: Map Text (Maybe Text)
  , rpStockFilter :: Maybe FilterExpression
  , rpRowRupture :: Maybe Text
  } deriving Show

-- * Form
reportForm :: [Text] -> Maybe ReportParam -> _
reportForm cols paramM = let
  colOptions = [(c,c) | c <- cols]
  form = ReportParam
    <$> (aopt dayField "from" (Just $ rpFrom =<< paramM ))
    <*> (aopt dayField "to" (Just $ rpTo =<< paramM))
    <*> (aopt filterEField  "sku" (Just $ rpStockFilter =<< paramM))
    <*> (aopt (selectFieldList colOptions)  "row rupture" (Just $ rpRowRupture =<< paramM) )
  in  renderBootstrap3 BootstrapBasicForm form
 
paramToCriteria :: ReportParam -> [Filter FA.StockMove]
paramToCriteria ReportParam{..} = (rpFrom <&> (FA.StockMoveTranDate >=.)) ?:
                                  (rpTo <&> (FA.StockMoveTranDate <=.)) ?:
                                  (filterE id FA.StockMoveStockId  rpStockFilter)

-- * Handler
getCols :: Handler [Text]
getCols = do
  let basic = ["Style", "Sku", "Variation", "Week", "Month", "Quarter", "52W", "Year", "Season"]
  categories <- categoriesH
  return $ basic ++ ["category:" <>  cat | cat <- categories]


getItemsReportR :: Maybe ReportMode -> Handler TypedContent
getItemsReportR mode = do
  renderReportForm ItemsReportR mode Nothing ok200 Nothing

getItemsReport2R mode = do
  renderReportForm ItemsReport2R mode Nothing ok200 Nothing

getItemsReport3R mode = do 
  renderReportForm ItemsReport3R mode Nothing ok200 Nothing

postItemsReportR mode = do
  today <- utctDay <$> liftIO getCurrentTime
  cols <- getCols
  ((resp, formW), enctype) <- runFormPost (reportForm cols Nothing)
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> do
      (report, result) <- itemReport (paramToCriteria param) tkCategory (Just . slidingYearShow today . tkDay)
      case mode of
        Just ReportCsv -> do
              let source = yieldMany (map (<> "\n") (toCsv result))
              respondSource "text/csv" (source =$= mapC toFlushBuilder)
        _ -> do
              renderReportForm ItemsReportR mode (Just param) ok200 (Just report)

postItemsReport2R :: Maybe ReportMode -> Handler TypedContent
postItemsReport2R mode = do
  today <- utctDay <$> liftIO getCurrentTime
  cols <- getCols
  ((resp, formW), enctype) <- runFormPost (reportForm cols Nothing)
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> do
      (report, result) <- itemReport (paramToCriteria param) tkStyle (Just . slidingYearShow today . tkDay)
      case mode of
        Just ReportCsv -> do
              let source = yieldMany (map (<> "\n") (toCsv result))
              respondSource "text/csv" (source =$= mapC toFlushBuilder)
        _ -> do
              renderReportForm ItemsReport2R mode (Just param) ok200 (Just report)

postItemsReport3R :: Maybe ReportMode -> Handler TypedContent
postItemsReport3R mode = do
  cols <- getCols
  today <- utctDay <$> liftIO getCurrentTime
  ((resp, formW), enctype) <- runFormPost (reportForm cols Nothing)
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> do
      (report, result) <- itemReport (paramToCriteria param) tkStyle tkVar
      case mode of
        Just ReportCsv -> do
              let source = yieldMany (map (<> "\n") (toCsv result))
              respondSource "text/csv" (source =$= mapC toFlushBuilder)
        _ -> do
              renderReportForm ItemsReport3R mode (Just param) ok200 (Just report)
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
