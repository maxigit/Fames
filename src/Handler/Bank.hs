module Handler.Bank 
( getGLBankR
, getGLBankDetailsR
, getGLBankReconciliateR
, postGLBankReconciliateR
, getGLBankHelpR
, getGLBankFXR
, postGLBankFXR
, getGLBankStatementGenR
) where


import Import hiding(object)
import qualified BankReconciliate as B
import BankReconciliate()
import Database.Persist.MySQL     (MySQLConf (..), Single(..), rawSql)
import System.Directory
import Text.Regex.TDFA ((=~)) -- , makeRegex, Regex)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
-- import Data.These
import Data.Time (diffDays,addDays, formatTime, defaultTimeLocale, utcToLocalTime, getCurrentTimeZone)
import Lens.Micro.Extras (preview)
import FA as FA
import GL.Utils
import GL.Payroll.Settings
import Text.Shakespeare.Text (st)
import Data.List(mapAccumL, nub)
import qualified Data.Map as Map
import Formatting
import Handler.CsvUtils
import Util.Cache
-- import Data.Decimal(realFracToDecimal)
import Control.Monad.State(State, evalState)
import qualified Text.Regex as Rg
-- import CategoryRule(RegexSub(..), regexSub, subRegex)
import CategoryRule(regexSub, subRegex)
import Formatting as F
import Util.Decimal

-- Transaction to reconciliate
-- Transaction from a statement missing in FA
-- may be automattically saved to FA
data ToRec = ToRec
    { trTrans :: B.Transaction
    , trSaveButtonHtml :: Maybe Html -- save button for auto
    , trSaveObjectHtml :: Maybe Html -- object for auto
    }
toRec :: B.Transaction -> ToRec
toRec t = ToRec t Nothing Nothing


commonCss :: __Css
commonCss = [cassius|
tr.private
  font-style: italic
  opacity: 0.5
table:hover tr.private
    font-style: normal
    opacity: 1
td.balance-no-match
  .balances
    display: none
  .diff-balance:
    display: inline
td.balance-no-match:hover
  .balances
    display: inline
  .diff-balance
    display: none
td.balance > span:not(.guessed-value)
  font-weight: bold
|]

recJs :: __Julius
recJs = [julius|

function updateRecTotal () {
   var opening = Number($("label:contains('Opening Balance')+input")[0].value) || 0;
   var closing = Number($("label:contains('Closing Balance')+input")[0].value) || 0 ;
   var inputs = $("td.update-rec input:checked");
   var rec = 0;
   $(inputs).each(function(input){
      var td= $(inputs[input]).parents("td");
      rec += Number(td.attr("data-amount"));
   })
   $("input.opening-balance").val(opening)
   $("input.closing-balance").val(closing)
   $("input.rec-total").val(rec);
   var diff = closing-opening-rec;
       diff = Math.round(diff*100+Number.EPSILON)/100
   var diffE = $("input.rec-difference");
   diffE.val(diff);
   if(Math.abs(diff)<1e-2) {
     diffE.removeClass("text-danger");
     diffE.removeClass("bg-danger");
     diffE.addClass("text-success");
     diffE.addClass("bg-success");
   } else {
     diffE.addClass("text-danger");
     diffE.addClass("bg-danger");
     diffE.removeClass("text-success");
     diffE.removeClass("bg-success");
   }
}

function toggleAll(e) {
  var panel= $(e.target).parents("table");
  var checkboxes = $(panel).find("input[type='checkbox']");
     $(checkboxes).prop('checked', $(e.target).prop('checked'));
  updateRecTotal();
}

function updateCheckBox(e) {
   // check if we need to untoggle the parent
   var panel = $(e.target).parents("table");
   var checkboxes = $(panel).find("input[type='checkbox']").not('.toggle-all');
   var toggle = $(panel).find("input.toggle-all");
   // check if all are unchecked checked
   if($(checkboxes).filter('input:checked').length == 0) {
     $(toggle).prop('checked',false);
   } else {
     $(toggle).prop('checked',true);
   }

  // propagate if needed
  if($(panel).find('td.update-rec').length > 0) {
     updateRecTotal()
  }
}
$(document).ready (function() {
  $("input [data-init-checked]").prop('checked',true);
  $("input [type=checkbox]").not('[data-init-checked]').prop('checked',false);
  $("input.toggle-all [type=checkbox]").prop('checked',false);
  $("td.update-rec input").change(updateRecTotal);
  $("input[type='checkbox']").not('.toggle-all').change(updateCheckBox);
  $("label:contains('Opening Balance')+input").change(updateRecTotal);
  $("label:contains('Closing Balance')+input").change(updateRecTotal);
  $("input.toggle-all").change(toggleAll);
  updateRecTotal();
})
|]

saveButtonJs :: __Julius
saveButtonJs = [julius|
  function hideSaveButton(elem) {
     elem.classList.remove("btn-danger");
     elem.classList.add("btn-muted");
  }

|]
getGLBankR  :: Maybe Int -> Handler Html
getGLBankR pagem = do
  todaym <- lookupGetParam "today"
  today <- maybe todayH return (todaym >>= readMay)

  dbConf <- appDatabaseConf <$> getsYesod appSettings
  faURL <- getsYesod (pack . appFAExternalURL . appSettings)
  settingss <- getsYesod (appBankStatements . appSettings)

  role <- currentRole
  -- only keep authorised settings and which have a position
  -- setting the position to Nothing allows to quickly deactivate a panel 
  -- it can still be access via the Bank details page
  let settings = filter filterSettings (mapToList settings')
      settings' = fromMaybe mempty $ headMay (maybe id (drop . (subtract 1)) pagem $ toList settingss)
      filterSettings (account, bsetting) = isJust (bsPosition bsetting) && (canViewBankStatement role account || canViewBankLightStatement role account)
      -- chose the display function depending on permission
      display (account, bs) = if canViewBankStatement role account
                              then displaySummary today dbConf faURL account bs
                              else displayLightSummary today dbConf faURL account bs
  panels <- forM (sortOn (bsPosition . snd) settings) display
  barW <- pageBar pagem
  defaultLayout $ toWidget commonCss >> toWidget saveButtonJs >> barW >> dateBarW (todaym >>= readMay) pagem >> (mconcat panels)

pageBar :: Maybe Int -> Handler Widget
pageBar pagem = do
  settingss <- getsYesod (appBankStatements . appSettings)
  let pages = zip (keys settingss) [1..]
      nonEmpty t = if null t then "Main" else t
      

  return [whamlet|
    <nav.navbar.navbar-default>
      <ul.nav.navbar-nav>
        $forall (title, page) <- pages
          $with current <- Just page == pagem
            <li :current:.active>
              <a href="@{GLR $ GLBankR (Just page)}"> #{nonEmpty title}
          |]

dateBarW :: Maybe Day -> Maybe Int -> Widget
dateBarW todaym pagem = [whamlet|
  <div.well>
    <form method=GET action="@{GLR $ GLBankR pagem}">
      <label for=today>End date
      <input type=date name="today" value="#{tshowM todaym}">
      <button.btn.btn-default type=submit>Submit
|]
  

canViewBankStatement :: Role -> Text -> Bool
canViewBankStatement role account = authorizeFromAttributes role (setFromList ["bank/" <> account ]) ReadRequest
canViewBankLightStatement :: Role -> Text -> Bool
canViewBankLightStatement role account = authorizeFromAttributes role (setFromList ["bank/light/" <> account ]) ReadRequest


-- | Displays a collapsible panel 
displayPanel :: Text -> Bool -> Text -> Widget -> Widget
displayPanel panelClass collapsed account = displayPanel' panelClass collapsed account [shamlet|<h2>#{account}|]
displayPanel' :: Text -> Bool -> Text -> Html -> Widget -> Widget
displayPanel' panelClass collapsed account title content =
  let panelId = "bank-"  <> filter (' ' /=) account
      expanded = not collapsed
  in [whamlet|
    <div.panel class="panel-#{panelClass}">
      <div.panel-heading data-toggle="collapse" data-target="##{panelId}">
          #{title}
      <div.panel-body.collapse :collapsed:.out :expanded:.in id="#{panelId}">
        ^{content}
        |]

loadReconciliatedTrans :: Maybe Day
                       -> MySQLConf
                       -> BankStatementSettings
                       -> Handler (([ToRec], [B.Transaction]), Maybe UTCTime)
loadReconciliatedTrans todaym dbConf settings =  do
  result <- case mkRecOptions todaym dbConf settings of
    (Just path, options) -> do
      ((torec0, l_all), upTime) <- liftIO $ withCurrentDirectory path  ((,) <$> B.main' options <*> B.updateTime options)
      torec <- mapM (autoRec settings) torec0
      return ((torec, l_all ), upTime)
    (Nothing, options) -> do
      -- fake the same system by setting fake path and load the transaction
      fas <- liftIO$ B.readFa options
      let ts = map B.faTransToTransaction fas

      return ((map toRec (filter (isNothing . B._sRecDate) ts), ts), Nothing)
  -- dates are fuzzy regarding the loading of FA Transaction
  -- to compensate for the fact that date between FA Transaction and bank statement can differst
  -- however, when given a end date, we don't want any unrec FA transaction after the end
  return $ case (todaym) of
            Nothing -> result
            Just today -> let torecFiltered = filter ((<= today) . B._sDate . trTrans) torec
                              ((torec, l_all), upTime) = result
                          in ((torecFiltered, l_all), upTime)
    

mkRecOptions :: Maybe Day -> MySQLConf -> BankStatementSettings -> (Maybe FilePath, B.Options)
mkRecOptions endDate dbConf BankStatementSettings{bsMode=BankUseStatement{..},..} =  let
      statementFiles = unpack bsStatementGlob
      faCredential = myConnInfo dbConf
      dailyFiles = unpack bsDailyGlob
      output = ""
      startDate = bsStartDate
      -- endDate = today
      faMode = B.BankAccountId (bsBankAccount)
      aggregateMode = B.BEST
      initialBalance = toDecimalWithRounding (Round 2) <$> bsInitialBalance
      discardFilter = unpack <$> bsDiscardRegex
  in (Just bsPath, B.Options{..})
mkRecOptions endDate dbConf BankStatementSettings{bsMode=BankNoStatement,..} =  let
      statementFiles = " "
      faCredential = myConnInfo dbConf
      dailyFiles = " "
      output = ""
      startDate = bsStartDate
      -- endDate = Nothing -- Just today
      faMode = B.BankAccountId (bsBankAccount)
      aggregateMode = B.BEST
      initialBalance = toDecimalWithRounding (Round 2) <$> bsInitialBalance
      discardFilter = unpack <$> bsDiscardRegex
  in (Nothing, B.Options{..})

-- | Display all non matching transaction as well as a preview of last the statement 
displaySummary :: Day -> _DB -> Text -> Text ->  BankStatementSettings -> Handler Widget
displaySummary today dbConf faURL title bankSettings@BankStatementSettings{..}= do
  object <- getObjectH
  let 
  -- let options = B.Options{..}
  --     statementFiles = unpack bsStatementGlob
  --     faCredential = myConnInfo dbConf
  --     dailyFiles = unpack bsDailyGlob
  --     output = ""
  --     startDate = bsStartDate
  --     endDate = Nothing -- Just today
  --     faMode = B.BankAccountId (bsBankAccount)
  --     aggregateMode = B.BEST
      blacklist = map unpack bsLightBlacklist
  --     initialBalance = Nothing
  --     discardFilter = unpack <$> bsDiscardRegex
  
  -- (stransz, banks) <- lift $ withCurrentDirectory bsPath (B.main' options)
  ((stransz, banks), updatedAtm) <- loadReconciliatedTrans (Just today) dbConf bankSettings
  -- we sort by date
  tz <- liftIO getCurrentTimeZone
  let sortTrans = sortOn sorter
      sorter = liftA3 (,,) (Down . B._sDate) (Down . B._sDayPos) (Down . B._sAmount)
      hideBlacklisted t = if keepLight blacklist t then ["public" ] else ["private"]
      sorted = (sortOn (sorter . trTrans)) stransz
      ok = null sorted
      collapsed = fromMaybe ok bsCollapsed
      summaryLimit = fromMaybe 10 bsSummaryLimit
      defaultCalculator = Oldest [ AddDays (-1)
                                 , Chain [ NextDayOfWeek Friday Tuesday
                                         ,  AddWeeks (-1)
                                         ]
                                 ]
      summaryDateCalculator = fromMaybe defaultCalculator bsSummaryDateCalculator
      lastDay = calculateDate summaryDateCalculator today
      (news, olds) = partition ((>= lastDay) . B._sDate)  (sortTrans banks)
      lastBanks = news <> (take (summaryLimit - length news) olds)
      lastW = renderTransactions bsSummaryPageSize True object faURL lastBanks hideBlacklisted (Just "Total") (const False)
      number'object trec =  (trSaveButtonHtml trec, (liftA2 (<|>) trSaveObjectHtml (toHtml <$$> object . trTrans)) $ trec)
      tableW = renderToRecs bsRecSummaryPageSize True number'object  faURL sorted hideBlacklisted (Just "Total") ((B.FA ==) . B._sSource)
      titleW = [shamlet|
        <div.row>
          <div.col-md-4>
            <h2> #{title}
          <h4.col-md-2>
            $maybe updated <- updatedAtm
              <label> Last Update
              <div>
                <span>#{formatTime defaultTimeLocale "%a %d %b %Y -- %R" $ utcToLocalTime tz updated }
          <h4.col-md-2.col-md-offset-3>
            $case sorted 
              $of []
              $of _
                $with bal <- sum (map (B._sAmount . trTrans) sorted)
                  <label> Discrepencies
                  #{tshow $ bal}
          <h4.col-md-1>
            <label> Balance
            <span.text-right>#{maybe ""  (tshow . validValue) $  headMay $ mapMaybe B._sBalance lastBanks }
                       |]
  return $ displayPanel' (if ok then "success" else "danger" :: Text)  collapsed title titleW [whamlet|
        <div.row>
            <div.col-md-2>
              $if ok   
                <h3> Everything is fine
              $else
                <h3> Discrepencies
            <div.col-md-2><h4>
              <a href="@{GLR (GLBankDetailsR title)}"> Details
            <div.col-md-2><h4>
              <a href="@{GLR (GLBankReconciliateR title)}"> Reconciliate
        ^{tableW}
        <div>
           <h3> Last #{summaryLimit}
        ^{lastW}
                     |]

displayLightSummary :: Day -> _DB -> Text -> Text ->  BankStatementSettings -> Handler Widget
displayLightSummary today dbConf faURL title bankSettings@BankStatementSettings{..}= do
  object <- getObjectH
  ((stranszUnfiltered, banksUnfiltered), updatedAtm) <- loadReconciliatedTrans (Just today) dbConf bankSettings
  -- we sort by date
  tz <- liftIO getCurrentTimeZone
  let sortTrans = sortOn sorter
      sorter = liftA3 (,,) (Down . B._sDate) (Down . B._sDayPos) (Down . B._sAmount)
      sorted = filter (keepLight blacklist . trTrans) $ (sortOn (sorter . trTrans)) stranszUnfiltered
      ok = null sorted
      blacklist = map unpack bsLightBlacklist
      lastBanks = take 10 $ filter (keepLight blacklist) $ sortTrans banksUnfiltered
      lastW = renderTransactions bsSummaryPageSize False object faURL lastBanks (const []) (Just "Total") (const False)
      number'object trec =  (trSaveButtonHtml trec, (liftA2 (<|>) trSaveObjectHtml (toHtml <$$> object . trTrans)) $ trec)
      tableW = renderToRecs bsRecSummaryPageSize False number'object faURL sorted  (const [])(Just "Total") ((B.FA ==) . B._sSource)
      titleW = [shamlet|
        <div.row>
          <div.col-md-4>
            <h2> #{title}
          <h4.col-md-2.col-md-offset-6>
                    $maybe updated <- updatedAtm
                      <label> Last Update
                      <div>
                        <span>#{formatTime defaultTimeLocale "%a %d %b %Y -- %R" $ utcToLocalTime tz updated }

                       |]
  return $ displayPanel' (if ok then "success" else "danger" :: Text) ok title titleW [whamlet|
        <a href="@{GLR (GLBankDetailsR title)}">
          $if ok   
            <p> Everything is fine
          $else
            <h3> Discrepencies
        ^{tableW}
        <h3> Last 10
        ^{lastW}
                     |]

-- | Only keep transactions in which aren not in the black list
keepLight :: [String] -> B.Transaction -> Bool
keepLight blacklist tr = B._sAmount tr >0
                         && not (blacklisted (B._sDescription tr))
                         && B._sSource tr == B.HSBC
                         where blacklisted s = any (s =~) blacklist


linkToFA :: (FATransType -> Int -> Text) -> B.Transaction -> Html
linkToFA urlForFA' trans = case (readMay (B._sType trans), B._sNumber trans) of
  (Just t, Just no) | B._sSource trans == B.FA ->
           let eType = toEnum t
               longType = showTransType eType :: Text
               directionClass = if B._sAmount trans > 0
                                then "text-success" :: Text
                                else "text-danger"
           in [shamlet|
     <a href="#{urlForFA' (eType) no}" class="#{directionClass}" target=_blank data-toggle="tooltip" title="#{longType}">
       #{transactionIcon eType}
       |]
  (Just t, Nothing) -> transactionIconSpan (toEnum t)
  _ -> toHtml (B._sType trans)

_not_used_transactionIcon' :: Text -> Html
_not_used_transactionIcon' t = case readMay t of
  Just eType -> transactionIcon $ toEnum eType
  Nothing -> toHtml t
renderTransactions :: Maybe Int -> Bool ->  (B.Transaction -> Maybe Text) -> Text -> [B.Transaction] -> (B.Transaction -> [Text]) -> Maybe Text -> (B.Transaction -> Bool) -> Widget
renderTransactions pageSize canViewBalance object faURL sorted mkClasses totalTitle danger = 
  let
    number'object =  (Nothing,) . fmap toHtml . object . trTrans
  in renderToRecs pageSize canViewBalance number'object faURL (map toRec sorted) mkClasses totalTitle danger

renderToRecs :: Maybe Int -> Bool ->  (ToRec -> ( Maybe Html, Maybe Html )) -> Text -> [ToRec] -> (B.Transaction -> [Text]) -> Maybe Text -> (B.Transaction -> Bool) -> Widget
renderToRecs pageSize canViewBalance number'object faURL sorted mkClasses totalTitle danger =
      let (ins, outs) = partition (> 0) (map (B._sAmount . trTrans) sorted)
          inTotal = sum ins
          outTotal = sum outs
          total = inTotal + outTotal
          paging = case pageSize of
            Nothing -> []
            Just s -> [("data-page-length" :: Text, tshow s)]
         
      in [whamlet| 
        <table *{datatable} *{paging} data-ordering=false>
          <thead>
            <tr>
              <th>Date
              <th>Source
              <th>Type
              <th>Description
              <th>Number
              <th>Object
              <th>Paid Out
              <th>Paid In
              $if canViewBalance
                <th>Balance
          $forall torec@(ToRec trans _ _ ) <- sorted
            $with isDanger <- danger trans
              $with (numberm, objectm) <- number'object torec
                <tr :isDanger:.text-danger :isDanger:.bg-danger class="#{intercalate " " (mkClasses trans)}">
                    <td>#{tshow $ B._sDate trans}
                    <td>#{tshow $ B._sSource trans}
                    <td>^{linkToFA (urlForFA faURL) trans}
                    <td>#{B._sDescription trans}
                    <td.text-right>
                      $case numberm
                        $of (Just num)
                          #{num}
                        $of Nothing
                          #{maybe "-" tshow $ B._sNumber trans}
                    <td>#{fromMaybe "-" objectm}
                    $if B._sAmount trans > 0
                      <td>
                      <td.text-right>#{showDecimal $  B._sAmount trans}
                    $else
                      <td.text-right>#{showDecimal $ negate  $    B._sAmount trans}
                      <td>
                  $if canViewBalance
                      <td.text-right> ^{render $ fmap (fmap showDecimal)  $ B._sBalance trans}
          <tr>
            $maybe totalTitle <-  totalTitle
              <th> #{totalTitle}
              <th.text-right> #{tshow total}
              <th>
              <th>
              <th>
              <th>
              <th.text-right>#{tshow $ negate outTotal}
              <th.text-right>#{tshow inTotal}
              $if canViewBalance
                <th> #{tshow total}
              |]

settingsFor :: Text -> Handler BankStatementSettings
settingsFor account = do
  role <- currentRole
  when (not $ canViewBankStatement role account)
     (permissionDenied account)
  allSettings <- getsYesod (appBankStatements . appSettings)
  case lookup account (concat allSettings) of
    Nothing -> error $ "Bank Account " <> unpack account <> " not found!"
    Just settings -> return settings
  
{-# NOINLINE getGLBankDetailsR #-}
getGLBankDetailsR :: Text -> Handler Html
getGLBankDetailsR account = do
  settings <- settingsFor account
  defaultLayout =<< displayDetailsInPanel account settings 

displayDetailsInPanel :: Text -> BankStatementSettings -> Handler Widget
displayDetailsInPanel account bankSettings@BankStatementSettings{..} = do
  today <- todayH
  dbConf <- appDatabaseConf <$> getsYesod appSettings
  faURL <- getsYesod (pack . appFAExternalURL . appSettings)
  __object <- getObjectH
  
  (,) (stransz, __banks) _ <- loadReconciliatedTrans (Just today) dbConf bankSettings

  let number'object =  liftA2 (,) trSaveButtonHtml trSaveObjectHtml 
      tableW = renderToRecs Nothing True number'object faURL stransz (const []) (Just "Total") ((B.FA ==) . B._sSource)
      ok = null stransz
  return $ displayPanel (if ok then "succes" else "danger") ok account [whamlet|
            <div.row>
              <div.col-md-2>
                <h3> By amount
              <div.col-md-2><h4>
                <a href="@{GLR (GLBankReconciliateR account)}"> Reconciliate
              <div.col-md-2><h4>
                <a href="@{GLR (GLBankStatementGenR account)}"> Download Statement
           ^{tableW}
                     |]
-- * Reconciliationg
data RecParam = RecParam
   { rpStartDate :: Maybe Day
   , rpOpeningBalance :: Double
   , rpEndDate :: Maybe Day
   , rpClosingBalance :: Double
   , rpRecDate :: Maybe Day
   } deriving Show
defaultParam :: RecParam
defaultParam = RecParam Nothing 0 Nothing  0 Nothing

recForm :: Maybe RecParam -> Html -> MForm Handler (FormResult RecParam, Widget)
recForm  paramM = renderBootstrap3 BootstrapBasicForm form  where
  form = RecParam <$> aopt dayField "start" (rpStartDate <$> paramM)
                  <*> areq doubleField "Opening Balance" (rpOpeningBalance <$> paramM)
                  <*> aopt dayField "end date" (rpEndDate <$> paramM)
                  <*> areq doubleField "Closing Balance" (rpClosingBalance <$> paramM)
                  <*> aopt dayField "reconciliate" (rpRecDate <$> paramM)

{-# NOINLINE getGLBankReconciliateR #-}
getGLBankReconciliateR :: Text -> Handler Html
getGLBankReconciliateR account = do
  today <- todayH
  renderReconciliate account defaultParam  {rpStartDate = Just $ calculateDate (AddMonths (-3)) today}
  
{-# NOINLINE postGLBankReconciliateR #-}
postGLBankReconciliateR :: Text -> Handler Html
postGLBankReconciliateR account = do
  ((resp, __formW), __encType) <- runFormPost (recForm Nothing)
  case resp of
    FormMissing -> error "Form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> do
        actionM <- lookupPostParam "action"
        case actionM of
          Nothing -> getGLBankReconciliateR account
          Just action -> do
            when (action == "reconciliate") $
              case rpRecDate param of
                Nothing -> setWarning "Please set a reconciliation date"
                Just recDate -> saveReconciliation account recDate
            renderReconciliate account param


renderReconciliate :: Text -> RecParam -> Handler Html
renderReconciliate account param = do
  dbConf <- appDatabaseConf <$> getsYesod appSettings
  (form, encType) <- generateFormPost (recForm $ Just param)
  faURL <- getsYesod (pack . appFAExternalURL . appSettings)
  object <- getObjectH
  bankSettings <- settingsFor account
  let optionsm = mkRecOptions Nothing dbConf bankSettings
  st'sts0 <- case optionsm of
    (Nothing, options) -> do
      -- No statement file provided
      -- Assume that payment out should reset the account balance to 0
      fas <- liftIO $ B.readFa options
      let ts = map B.faTransToTransaction fas
          autoBalance t = if B._sAmount t < 0 then Just (pure 0) else Nothing
      return $ [These t { B._sSource = B.HSBC, B._sBalance = autoBalance t } t | t <- ts]
      
    (Just path, options) -> do
      (hts, _) <- liftIO $ withCurrentDirectory path (B.loadAllTrans options {B.aggregateMode = B.ALL_BEST})
      let byDays = B.badsByDay hts
          -- group by rec
      return $ map (bimap B.hsbcTransToTransaction B.faTransToTransaction)  byDays
  let
      st'sts = filter transFilter $ st'sts0
      recGroup' = groupAsMap (B._sRecDate . B.thatFirst) (:[]) st'sts
      -- sort FA Transaction within each group by pos and recalculate the balance
      recGroup = resortFA recGroup'
      -- exclude a pair if both date are outside the range
      transFilter t | d <- mergeTheseWith B._sDate B._sDate max t,   Just start <- (bsStartDate bankSettings), d < start = False
      transFilter t | d <- mergeTheseWith B._sDate B._sDate min t,   Just end <- rpEndDate param , d > end  = False
      transFilter t@(These _ fa) = isNothing (B._sRecDate fa) -- not reconciliated, must show
                              || ( maybe True (mergeTheseWith B._sDate B._sDate  max t >=)  (rpStartDate param)
                                  && maybe True (mergeTheseWith B._sDate B._sDate  min t <=)  (rpEndDate param)
                                  )
      transFilter _ = True

      -- filterDate  = B.filterDate (mergeTheseWith B._sDate B._sDate  max)
      --                            (mergeTheseWith B._sDate B._sDate min)
      --                            options {B.startDate = rpStartDate param, B.endDate = rpEndDate param }

      panels = map (displayRecGroup forInitRec faURL object) (sortPanel $ mapToList recGroup)
      -- put nothing first then by reverse order
      sortPanel (main@(Nothing,_ ):others) = main : reverse others
      sortPanel others = reverse others
      -- if a trans is taken into account to calculated the reconciliated amount
      -- we are only interesed in the item reconciliated in the current reconciliation period
      -- and the one ready to be (ie match FA And statements)
      forInitRec _ | bsMode bankSettings == BankNoStatement = (False, True)  -- don't pre check if no statement.
      forInitRec (These h fa) = (result, result) where
        result = maybe True (B._sDate h <=) (rpRecDate param) 
               && maybe True (\(tRecDate, recDate) -> tRecDate == recDate ) ((,) <$> B._sRecDate  fa <*> rpRecDate param)
      forInitRec (That fa) = (r, r) where r = isJust $ B._sRecDate fa
      forInitRec _ = (False, False)
      reconciliated :: Double
      reconciliated = fromRational . toRational $ sum $ map (B._sAmount . B.thisFirst) (filter (fst. forInitRec) st'sts)
      statusW = [whamlet|
          <label>Opening
          <input.opening-balance value="#{formatDouble $ rpOpeningBalance param}" readonly>
          <label>Closing
          <input value="#{formatDouble $ rpClosingBalance param}" readonly>
          <label>Reconciliated
          <input.rec-total value="#{formatDouble $ reconciliated}" readonly>
          <label>Missing
          <input.rec-difference value="#{formatDouble $ (rpClosingBalance param - rpOpeningBalance param) - reconciliated}" readonly>
          |]
      emptyPanel = displayPanel "info" False  " " "" -- needed so that the statusBar doesn't hide the bottom of the last relevant panel

  defaultLayout $ do
      toWidget commonCss
      toWidget recJs
      [whamlet|
      <form.form-inline action="@{GLR (GLBankReconciliateR account)}" method=POST enctype="#{encType}">
        <div.well>
          ^{form}
          <button.btn.btn-primary name=action value="submit">Submit
        <div.row>
          <div.col-md-2>
            <h3> Details
          <div.col-md-2><h4>
            <a href="@{GLR (GLBankDetailsR account)}"> Details

        ^{mconcat panels}
        ^{emptyPanel}
        <div.well.footer.navbar-fixed-bottom>
          ^{statusW}
          <button.btn.btn-warning name=action value="reconciliate">Save
              |]
  

resortFA :: Map (Maybe Day) [These B.Transaction B.Transaction] -> Map (Maybe Day) [These B.Transaction B.Transaction]
resortFA groups = let
  sorted = sortFas <$> groups
  sortFas :: [These B.Transaction B.Transaction] -> [These B.Transaction B.Transaction]
  sortFas st'sts = sortOn (((,) <$> B._sDate <*> B._sDayPos) . B.thisFirst) st'sts
  -- sortFas thfs = case partitionThese thfs of
  --   ([], ([], fas)) -> map That (sortOn ((,) <$> B._sDate <*> B._sDayPos) fas)
  --   _ -> thfs
  in rebalanceFA sorted
  
-- | Recalculate FA balance according to their new order
rebalanceFA :: Map (Maybe Day) [These B.Transaction B.Transaction] -> Map (Maybe Day) [These B.Transaction B.Transaction]
rebalanceFA groups = let
  -- groups are sorted by date, however Nothing is first instead of being last
  lastDate = join ( maximumMay (keys groups)) <&> addDays 1000
  groupList = sortOn (\(k, _) -> k <|> (lastDate) ) (mapToList groups)
  fas = mapMaybe (preview there) (concatMap snd groupList)
  runBalance balance0 = flip evalState balance0 (traverse (traverse runBalanceS) groupList)
  runBalanceS :: [These B.Transaction B.Transaction] -> State _amount [These B.Transaction B.Transaction]
  runBalanceS l_group = do
    flip traverse  l_group $ \thf -> do
      traverse forceUpdateBalanceS thf
  forceUpdateBalanceS t' = do
    t <- B.updateBalanceS (t' {B._sBalance = Nothing}) -- clear balance, so that it gets updated
    -- compare if balance has changed
    return $ case (validValue <$> B._sBalance t, validValue <$> B._sBalance t') of
               (Just b, Just b') | b == b' -> t { B._sBalance = Just (Provided b)  }
               _ -> t
      
    
  result = case sortOn ((,) <$> B._sDate <*> B._sDayPos) fas of
       [] -> groups
       (fa:_) -> groupAsMap fst ( snd ) $ runBalance ((\a -> validValue a - B._sAmount fa ) <$> B._sBalance fa)
       -- (fa:_) -> runBalance ((\a -> validValue a - validValue a) <$> B._sBalance fa)
  in result

getOpenings :: [These B.Transaction B.Transaction] -> (Maybe (ValidField B.Amount), Maybe (ValidField B.Amount))
-- getOpenings st'sts = (headMay ts >>= (\t -> (subtract (B._sAmount t)) <$$> (B._sBalance t)) , lastMay ts >>= B._sBalance) where 
--   ts = case partitionThese st'sts of
--     ([], ([], fas)) -> fas
--     _ -> mapMaybe (preview here) st'sts
getOpenings st'sts = (headMay ts >>= (\t -> (subtract (B._sAmount t)) <$$> (B._sBalance t)) , lastMay ts >>= B._sBalance) where 
  -- if first hsbc balance is unknow us FA
  ts = case headMay st'sts >>= preview here >>= B._sBalance of
         Just _ -> mapMaybe (preview here) st'sts -- all bank
         Nothing -> mapMaybe (preview there) st'sts -- all fast

displayRecGroup :: (These B.Transaction B.Transaction -> (Bool, Bool)) -> Text -> (B.Transaction -> Maybe Text) -> (Maybe Day, [These B.Transaction B.Transaction]) -> Widget 
displayRecGroup toCheck faURL object (recDateM, st'sts0) = let
  -- st'sts = sortOn (((,) <$> B._sDate <*> B._sDayPos) . B.thisFirst) st'sts0
  st'sts = st'sts0
  (opening', close') = getOpenings st'sts
  opening = validValue <$> opening'
  close = validValue <$> close'
  title = [shamlet|
     <div.row>
       <div.col-md-2>
         <h2> #{ maybe "" tshow recDateM}
       <h4.col-md-1.col-md-offset-7>
         <label> Opening
         <div.text-right>#{maybe "" tshow opening }
       <h4.col-md-1>
         <label> Close
         <div.text-right>#{maybe "" tshow close }
       <h4.col-md-1>
         <label>  Net
         <div.text-right>#{ tshow (fromMaybe 0 close - fromMaybe 0 opening)}
        |]
      -- check if the difference of the two date is acceptable

  dateClass (This _) = ""
  dateClass (That _) = ""
  dateClass (These h f) = let d = diffDays (B._sDate h) (B._sDate f)
                          in if abs(d)  > 14
                              then "text-danger" :: Text
                              else if abs(d) > 5
                                  then "text-warning"
                                  else ""
  rowClass (This _) = "bg-warning text-warning "
  rowClass (That _) = "bg-danger text-danger"
  rowClass (These _ _) = "" :: Text
  eqDouble x y = abs (validValue x - validValue y ) < 1e-4
  widget = [whamlet|
    <table *{datatable} data-paging=false data-ordering=false>
      <thead>
        <tr>
              <th.all>Date 
              <th.data-priority=150>D
              <th.none>FA Date
              <th.data-priority=50>Source
              <th.data-priority=100>Type 
              <th.data-priority=110>FA Type
              <th.data-priority=10>Description
              <th.none>FA Ref.
              <th.none>Number 
              <th.data-priority=105>Object
              <th.all data-class-name=text-right>Paid Out
              <th.all data-class-name=text-right>Paid In
              <th.data-priority=1 data-class-name=text-right>Balance
              <th.data-priority=200 data-class-name=text-right>FA Bal
              <th.data-priority=150 data-class-name=text-right>Diff
              <th.all>Rec
                <input.toggle-all type=checkbox checked>
      $forall st'st <- st'sts
        $with (trans, fatrans, transM, fatransM) <- (B.thisFirst st'st, B.thatFirst st'st, preview here st'st, preview there st'st)
          <tr class="#{rowClass st'st}">
              <td>#{tshow $ B._sDate trans}
              $case (B._sDate <$> transM, B._sDate <$> fatransM)
                $of (Just d, Just d')
                  $with diff <- diffDays d d'
                      <td.date-diff class="#{dateClass st'st}">
                          $if diff > 0
                            +#{tshow diff}
                          $elseif diff < 0
                            #{tshow diff}
                $of _
                    <td>
              <td class="#{dateClass st'st}">#{maybe "" (tshow . B._sDate) fatransM}
              <td>
                #{maybe "-" (tshow . B._sSource) transM}/#{maybe "-" (tshow . B._sSource) fatransM}
              <td>#{maybe "" B._sType transM}
              <td>^{maybe "" (linkToFA (urlForFA faURL)) fatransM}
              <td>#{maybe "" B._sDescription transM}
              <td>#{maybe "" B._sDescription fatransM}
              <td>#{maybe "-" tshow $ B._sNumber trans}
              <td>#{fromMaybe "-" (object fatrans)}
              $if B._sAmount trans > 0
                  <td>
                  <td.balance>#{showDecimal $  B._sAmount trans}
              $else
                  <td.balance>#{showDecimal $ negate  $    B._sAmount trans}
                  <td>
              $if liftA2 eqDouble (B._sBalance =<< transM) (B._sBalance =<< fatransM)  == Just True
                <td.balance.balance-match.bg-success.text-success>
                 ^{render $ fmap (fmap showDecimal) (B._sBalance trans) }
                <td.balance>
                 ^{render $ fmap (fmap showDecimal) (B._sBalance fatrans) }
              $else
                <td.balance.balance-no-match>
                 ^{render $ fmap (fmap showDecimal) (B._sBalance trans) }
                <td.balance>
                 ^{maybe "" (render . fmap showDecimal) (B._sBalance =<< fatransM) }
              $with [bal, balfa] <- map (maybe 0 validValue . B._sBalance) [trans, fatrans]
                  $with diff <- balfa - bal
                     <td>
                      <div.diff-balance>
                        $if diff > 0
                          +#{showDecimal diff}
                        $else
                          #{showDecimal diff}
                
              $with (checked, update) <- toCheck st'st
                <td :update:.update-rec data-amount="#{tshow (B._sAmount trans)}">
                  $if isThese st'st 
                    $if isJust (B._sRecDate fatrans)
                        <input type=hidden name="already-#{faId fatrans}" value=off>
                        <input type=checkbox name="keepset-#{faId fatrans}" data-init-checked checked>
                    $else
                        <input type=checkbox name="set-#{faId fatrans}":checked:checked :checked:data-init-checked>
              |]
  in displayPanel' ("primary" :: Text) False (tshowM recDateM) title widget

-- | Computes the checkbox prefix for a transaction.
faId :: B.Transaction -> Text
faId strans = let
  prefix = if isJust (B._sRecDate strans)
           then "rec-"
           else "rec-"
  in prefix <> pack (B._sType strans) <> "-" <> maybe "0" tshow (B._sNumber strans)

saveReconciliation :: Text -> Day -> Handler ()
saveReconciliation account recDate = do
  (params, _) <- runRequestBody
  BankStatementSettings{..} <- settingsFor account
  let toSet' = catMaybes [ decodeKey key
              | (key', __value) <- params
              -- , value == "on"
              , Just key <-  [stripPrefix "set-rec-" key']
              ]
      already = catMaybes [ decodeKey key
                | (key', __value) <- params
                , Just key <-  [stripPrefix "already-rec-" key']
                ]
      
      tokeep = catMaybes [ decodeKey key
               | (key', __value) <- params
               -- , value == "on"
               , Just key <-  [stripPrefix "keepset-rec-" key']
               ]
      toUnset = setFromList already \\ setFromList tokeep :: Set (Int, Int)
      decodeKey key = let (a,b) = break (=='-') key in (,)  <$> readMay a <*> readMay  (drop 1 b)
  runDB $ do
    saveRecDate (tshow bsBankAccount) recDate toSet'
    unsetRecDate (tshow bsBankAccount) (setToList toUnset)

-- saveRecDate :: Day -> [(Int, Int)] -> Handler ()
-- bank account is needed to not update both
-- side of a bank transfer
saveRecDate :: Text -> Day -> [(Int, Int)] -> SqlHandler ()
saveRecDate bankAccount recDate transIds = do
  forM_ transIds $ \(t, no) -> 
     updateWhere [ FA.BankTranType ==. Just t
                 , FA.BankTranTransNo ==. Just no
                 , FA.BankTranBankAct ==. bankAccount
                 ]
                 [ FA.BankTranReconciled =. Just recDate]

unsetRecDate :: Text -> [(Int, Int)] -> SqlHandler ()
unsetRecDate bankAccount transIds = do
  forM_ transIds $ \(t, no) -> 
     updateWhere [ FA.BankTranType ==. Just t
                 , FA.BankTranTransNo ==. Just no
                 , FA.BankTranBankAct ==. bankAccount
                 ]
                 [FA.BankTranReconciled =. Nothing]



  

  -- get all ids

getObject :: Map Text Text -> Map Text Text -> B.Transaction -> Maybe Text
getObject customerMap supplierMap trans = do -- Maybe
  object <- pack <$> B._sObject trans
  let mapped = case toEnum <$> readMay (B._sType trans) of
                 Just ST_CUSTPAYMENT -> lookup object customerMap
                 Just ST_SUPPAYMENT -> lookup object supplierMap
                 _ -> Nothing
  return . decodeHtmlEntities $ fromMaybe object mapped

getObjectH :: Handler (B.Transaction -> Maybe Text)
getObjectH = do
  customerMap' <- allCustomers False
  supplierMap' <- allSuppliers False
  let customerMap :: Map Text Text
      customerMap = mapFromList [(tshow $ FA.unDebtorsMasterKey key, decodeHtmlEntities $ FA.debtorsMasterName cust)
                                | (key, cust) <- mapToList customerMap'
                                ]
      supplierMap = mapFromList [(tshow $ FA.unSupplierKey key, decodeHtmlEntities $ FA.supplierSuppName sup)
                                | (key, sup) <- mapToList supplierMap'
                                ]
  return (getObject customerMap supplierMap)

-- * Help : How to get the required files
{-# NOINLINE getGLBankHelpR #-}
getGLBankHelpR :: Handler Html
getGLBankHelpR = do
  let panels  = map (uncurry $ displayPanel "primary" True)
                    [ ("Common", commonHelp)
                    ,("HSBC current", hsbcHelp )
                    ,("HSBC BMM", bmmHelp )
                    ,("HSBC Credit Card", ccardHelp )
                    , ("Paypal", paypalHelp)
                    ]
  
  defaultLayout $ mconcat panels

commonHelp, paypalHelp, hsbcHelp, bmmHelp, ccardHelp :: Widget
commonHelp = [whamlet|
<b>Fames</b> accepts two types of statements, daily and full statements.
<p><b>Daily</b> statements corresponds to current balance and can overlap with each others. This usually occurs
because each statement only contains the last n transactions which can be the curren days and a few day before.
In that case, for example, yesterday transactions will be in the today daily statement as well as yesterday statement if available.
<p> <b>Full statement</b> on the other hand shouldn't overlap even though they can overlap with the daily one.
<p> Depending on the source, daily statement or bank statemen may have the same format or not be available.
|]
paypalHelp = [whamlet|
<h3> Daily
<h3> Full Statement
<p> Paypal statement must be downloaded from the activity download page.
<p>  To do so, go on the <a href="https://www.paypal.com/listing/transactions">Activity</a> page and follow
the <code>download</code> link.
-- <p>  Alternatively the following <a href="https://business.paypal.com/merchantdata/reportHome?reportType=DLOG">link</a> should work.
-- <p>  Enter the desired dates and create the report. You should then be able to download and save the report.
<p> The file then needs to cleaned up (see your administrator)
  <div.well><ul>
      <li><code>s/"//g</code> and <code>s/,/, /g</code> on the header
      <li>Foreign currencies need to be removed <code>2,$v/GBP/d</code>
      <li>As well as incomplete transcation <code>2,$v/Completed/d</code>
|]
hsbcHelp = [whamlet||]
bmmHelp = [whamlet||]
ccardHelp = [whamlet|
<p>Credit card account needs actually two (or more) statement to be complet.
<p>One correspond to the account itself and then there is a statment per card.
<p> Even though they are called statement on the HSBC website, they don't follow
<p>the monthly statement format but the daily one. They all need to be saved as daily statment.
<p>On HSBC website, statement can be seen on the <b>Transactions List</b> page.
<p>This can be either a <b>Business card startment</b> or a <b>Card Holder</b> statment.
<p>One can go from one to the other by either clicking on the balance or the <b>Business card</b> button.
|]

-- * FX

data FXParam = FXParam { fxStart :: Day, fxEnd :: Day, fxCurrency :: Text}
  deriving (Show, Eq)

fxForm :: Maybe FXParam -> Html -> MForm Handler (FormResult FXParam, Widget)
fxForm paramM = renderBootstrap3 BootstrapBasicForm form  where
  form = FXParam <$> areq dayField "start" (fxStart <$> paramM)
                 <*> areq dayField "end" (fxEnd <$> paramM)
                 <*> areq textField "" (fxCurrency <$> paramM)

{-# NOINLINE getGLBankFXR #-}
getGLBankFXR :: Handler Html
getGLBankFXR = do
  today <- todayH
  let fxStart = calculateDate (AddYears (-1)) today
      fxEnd = today -- calculateDate (AddDays 1) today

  -- load currencies, take the second one, the first one being the company currency
  currencies <- runDB $ selectList [FA.CurrencyInactive ==. False] [LimitTo 2]
  let fxCurrency = case currencies of 
        _ : Entity curr _ : _ -> FA.unCurrencyKey curr
        _ -> ""

  renderFX (FXParam{..}) -- (return ())


{-# NOINLINE postGLBankFXR #-}
postGLBankFXR :: Handler Html
postGLBankFXR = do
  ((resp, __formW), __encType) <- runFormPost (fxForm Nothing)
  case resp of
    FormMissing -> error "Form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> renderFX param


renderFX :: FXParam -> Handler Html
renderFX param  = do
  let curr = fxCurrency param
  (form, encType) <- generateFormPost (fxForm $ Just param)
  trans <- loadFXTrans param
  defaultLayout $ do
    [whamlet|
    <form.form-inline method=POST enctype="#{encType}">
      <div.well>
        ^{form}
        <button.btn.btn-primary name=action value="submit">Submit
    <table#fx-table *{datatable} data-paging=false data-ordering=false data-dom="">
     <thead>
        <tr>
          <th>Date
          <th>Description
          <th>FX Amount 
          <th>Home Amount 
          <th>Rate
          <th>Total FX
          <th>Total Home
          <th>Average rate
     $forall tran <- trans
       <tr>
        <td>#{tshow $ fxDate tran}
        <td>#{fxDescription tran}
        <td.text-right>#{formatDouble $ fxFXAmount tran} #{curr}
        <td.text-right>#{formatDouble $ fxHomeAmount tran}
        <td.text-right>#{sformat  (fixed 4) (fxRate tran)}
        $with (totalH, totalFX) <- fxExtra tran
          <td.text-right>#{formatDouble $ totalFX} #{curr}
          <td.text-right>#{formatDouble $ totalH}
          <td.text-right>#{sformat (fixed 4) (totalFX / totalH)}
            |]

  

-- We are only interested in money in, ie when we buy 
loadFXTrans :: FXParam -> Handler [FXTrans (Double, Double)]
loadFXTrans param = do
  setWarning( "This report doesn't use (yet) supplier payments but only bank transfer")
  loadFXTransfers param
  -- bank transfers into FX Account - easy
  -- supplier (USD) to current -- with rate
  -- bank deposit -- into FX account, how to we know the rate 


data FXTrans e = FXTrans
  { fxDate :: !Day
  , fxDescription :: !Text
  , fxHomeAmount :: !Double
  , fxFXAmount :: !Double
  , fxTransNo :: !Int
  , fxTransType :: !Int
  , fxRate :: !Double
  , fxExtra :: e
  }

loadFXTransfers :: FXParam -> Handler [FXTrans (Double, Double)]
loadFXTransfers FXParam{..} = do
  let sql = [st|
    SELECT to_.trans_date, to_.person_id, to_.amount, -from_.amount, to_.trans_no, to_.type
    FROM 0_bank_trans AS to_
    JOIN 0_bank_accounts AS bank ON (to_.bank_act = bank.id AND bank_curr_code = ?)
    JOIN 0_bank_trans AS from_ ON (to_.type = from_.type AND to_.trans_no = from_.trans_no  )
    WHERE to_.type = 4
    AND to_.trans_date between ? AND ?
    AND to_.amount > 0
    AND from_.amount < 0
    ORDER BY to_.trans_date , to_.trans_no
               |]
  rows <- runDB $ rawSql sql [toPersistValue fxCurrency, toPersistValue fxStart, toPersistValue fxEnd] 
  let fxTrans = map transferToFXs rows
      fxTransPlus = mapAccumL runBalance (0,0) fxTrans
      runBalance (totalHome, totalFX) FXTrans{..} = ((newHome, newFX), FXTrans{fxExtra=l_fxExtra,..} ) where
        newHome = totalHome + fxHomeAmount
        newFX = totalFX + fxFXAmount
        l_fxExtra = (newHome, newFX)

  return (snd fxTransPlus)


transferToFXs :: ( Single Day, Single Text, Single Double, Single Double
                 , Single Int, Single Int
                 )
              -> FXTrans ()
transferToFXs (Single fxDate, Single fxDescription, Single fxFXAmount, Single fxHomeAmount, Single fxTransNo, Single fxTransType) = FXTrans{..}
  where fxRate = fxFXAmount/fxHomeAmount
        fxExtra = ()


-- * Generate statement
-- generating statement can be usefull to reconciliate
-- account which doesn't have statement but have for example
-- known balances. This is a case of cash account.
-- The procedure is then to generate a statement with the unreconciliated
-- transaction and add the balance manually  in the generated file.
{-# NOINLINE getGLBankStatementGenR #-}
getGLBankStatementGenR :: Text -> Handler TypedContent
getGLBankStatementGenR account= do
  dbConf <- appDatabaseConf <$> getsYesod appSettings
  bankSettings <- settingsFor account
  let (pathm, options) = mkRecOptions Nothing dbConf bankSettings
  let withDir = case pathm of
        Nothing -> id
        Just path -> withCurrentDirectory path
       
  (hts, _) <- liftIO $ withDir (B.loadAllTrans options)
  let fas = concat $ catThat $ map snd hts
      hs = [ h  {B._hBalance = Nothing, B._hType = convertType (B._hType h) } -- reset balance
             | fa <- fas
             , let t = B.faTransToTransaction fa
             , let h = B.transactionToHsbcTrans t
             ]
      convertType t = maybe t (showTransType . toEnum) (readMay t)
  let content = B.encodeHSBCTransactions (sortOn sorter hs )
      dates = map B._hDate hs
      minDate = minimumMay dates
      maxDate = maximumMay dates
      sorter = liftA2 (,) B._hDate B._hDayPos
      filename = "gen-statement-" <> (maybe "" (formatTime defaultTimeLocale "%F") minDate)
                 <> "--" <> (maybe "" (formatTime defaultTimeLocale "%F") maxDate)
                 <> ".csv"
  setAttachment $ fromString filename
  respond "text/csv" content
-- * Auto rec
-- generate FA transactions to match statement ones.
-- This can be for example, supplier payment corresponding to outsanding invoice
-- or bank transfer.
autoRec :: BankStatementSettings -> B.Transaction -> Handler ToRec 
autoRec settings0 t = do
  BankStatementSettings{..} <- fillAutoSettingsCached settings0
  faURL <- getsYesod (pack . appFAExternalURL . appSettings)
  let rules' = concatMap mapToList bsRules 
      description = toLower $ B._sDescription t
      parseRegex s = case break (=='/') (toLower s) of
        (regex, '/':l_replace ) -> regexSub regex l_replace
        (regex, "") -> regexSub regex description
        _ -> error "Pattern should be exhaustive"

      rules = map (first (parseRegex . unpack)) rules' -- map (first $ Rg.mkRegex . unpack) rules'
      match = headMay [ (rule, memo)
                      | (r, rule) <- rules
                      , memo <- maybeToList $ subRegex r description
                      ]
  case match of
        (Just (BankAutoTransfer target, memo)) -> generatePrefillFundTransferLink faURL bsBankAccount target memo t
        (Just (BankAutoSupplier target, memo)) -> generatePrefillSupplierPaymentLink faURL bsBankAccount target memo t
        (Just (BankAutoCustomer target, memo)) -> generatePrefillCustomerPaymentLink faURL bsBankAccount target memo t
        _ -> return $ toRec t

-- | Open the FA fund transfer page with prefilled parameter
generatePrefillFundTransferLink :: Text -> Int -> Int -> String ->  B.Transaction -> Handler ToRec
generatePrefillFundTransferLink faURL current target memo t@B.Transaction{..} = do
  let (from, to, dirClass ) = if _sAmount > 0 -- transfer TO current
                    then (target, current, "text-success" :: Text)
                    else (current, target, "text-danger")
  -- find target name
  bankm <- runDB $ get (FA.BankAccountKey target)
  let targetName = maybe ("#" <> tshow target) FA.bankAccountBankAccountName bankm
      object =  [shamlet|
    <span class="#{dirClass}">
       <span.guessed-value>#{decodeHtmlEntities targetName}
                        |]
      save = [shamlet|
  <form method=POST action="#{faURL}/gl/bank_transfer.php" target=_blank>
    <input type=hidden name=DatePaid  value="#{formatTime defaultTimeLocale "%Y/%m/%d" _sDate}">
    <input type=hidden name=amount  value="#{tshow $ abs _sAmount}">
    <input type=hidden name=FromBankAccount  value="#{tshow from}">
    <input type=hidden name=ToBankAccount  value="#{tshow to}">
    <input type=hidden name=memo_  value="#{memo}">
    #{saveButton}
    |]
  return $ ToRec t {B._sType = show (fromEnum ST_BANKTRANSFER) } (Just save)  (Just object)


saveButton :: Html
saveButton = [shamlet|
   <button.btn.btn-sm.btn-danger onclick="hideSaveButton(this);"> 
     <span.glyphicon.glyphicon-save data-toggle="tooltip" title="Save">
     <span.hidden>Save
   |]
generatePrefillSupplierPaymentLink :: Text -> Int -> Int -> String ->  B.Transaction -> Handler ToRec
generatePrefillSupplierPaymentLink faURL current target memo t@B.Transaction{..} | _sAmount < 0 = do
  suppm <- runDB $ get (FA.SupplierKey target)
  let targetName = maybe ("#" <> tshow target) FA.supplierSuppName suppm
      object =  [shamlet|
    <span class>
       <span.guessed-value>#{decodeHtmlEntities targetName}
                        |]
      save = [shamlet|
  <form method=GET action="#{faURL}/purchasing/supplier_payment.php" target=_blank>
    <input type=hidden name=DatePaid  value="#{formatTime defaultTimeLocale "%Y/%m/%d" _sDate}">
    <input type=hidden name=amount  value="#{tshow $ abs _sAmount}">
    <input type=hidden name=bank_account  value="#{tshow current}">
    <input type=hidden name=supplier_id value="#{tshow target}">
    <input type=hidden name=memo_  value="#{memo}">
    <span>
    #{saveButton}
    |]
  return $ ToRec t {B._sType = show (fromEnum ST_SUPPAYMENT)}  (Just save) (Just object)
generatePrefillSupplierPaymentLink faURL current target memo t@B.Transaction{..} = return $ toRec t

generatePrefillCustomerPaymentLink :: Text -> Int -> Int -> String ->  B.Transaction -> Handler ToRec
generatePrefillCustomerPaymentLink faURL current target memo t@B.Transaction{..} | _sAmount > 0 = do
  suppm <- runDB $ get (FA.DebtorsMasterKey target)
  let targetName = maybe ("#" <> tshow target) FA.debtorsMasterName suppm
      object = [shamlet|
    <span>
       <span.guessed-value>#{decodeHtmlEntities targetName}
    |]
      save = [shamlet|
  <form method=GET action="#{faURL}/sales/customer_payments.php" target=_blank>
    <input type=hidden name=DateBanked  value="#{formatTime defaultTimeLocale "%Y/%m/%d" _sDate}">
    <input type=hidden name=amount  value="#{tshow $ abs _sAmount}">
    <input type=hidden name=bank_account  value="#{tshow current}">
    <input type=hidden name=customer_id value="#{tshow target}">
    <input type=hidden name=memo_  value="#{memo}">
    <span>
    #{saveButton}
    |]
  return $ ToRec t {B._sType = show (fromEnum ST_CUSTPAYMENT)}  (Just save) (Just object)
generatePrefillCustomerPaymentLink faURL current target memo t@B.Transaction{..} = return $ toRec t

-- | Create rules for supplier and customer based on previous payment
fillAutoSettingsCached bankSettings = cache0 False (cacheDay 1) ("bank-settings/" <> tshow (bsBankAccount bankSettings)) $ do
  dbConf <- appDatabaseConf <$> getsYesod appSettings
  let optionsm = mkRecOptions Nothing dbConf bankSettings
  recs <- case optionsm of
    (Nothing, _) -> return [] -- :: Handler [These B.HSBCTransacations B.FATransaction]
    (Just path, options) -> do
      (hts, _) <- liftIO $ withCurrentDirectory path (B.loadAllTrans options {B.aggregateMode = B.ALL_BEST})
      return $ B.badsByDay hts
  -- transform number and special character into wildcard
  let customerRules = rulesFromRec ST_CUSTPAYMENT BankAutoCustomer recs
      supplierRules = rulesFromRec ST_SUPPAYMENT BankAutoSupplier recs
      -- transRules = rulesFromRec ST_BANKTRANSFER BankAutoTransfer recs
  -- lift $ writeFile "/home/max/Webshot/recs.hs" (fromString $ show ("RECS", filter isThese recs, "customerRules", customerRules, "supp", supplierRules))
  return $ bankSettings {bsRules = bsRules bankSettings <> (customerRules : supplierRules : [])}
  

rulesFromRec eType ruleFn recs = 
  let numberSub = (Rg.mkRegex " [a-z]*([0-9]{2,}[-a-z]*)+",  " [-a-zA-Z0-9]{2,}" )
      specialSub = (Rg.mkRegex "[()/*?+]", ".") -- avoid insertiong of specical charctter in regex
      spaces = (Rg.mkRegex " +", " +") -- many spaces = many spaces
      ltdSub = (Rg.mkRegex " ltd", "( ...)?")
      stripEnd = (Rg.mkRegex " \\+$", " *") -- remove trailing spaces at the end
      mkReg s = (subRegex0 stripEnd$ subRegex0 spaces $ subRegex0 ltdSub $ subRegex0 numberSub $ subRegex0 specialSub $ toLower s)
      subRegex0 (reg, rep) s = Rg.subRegex reg s rep 
      -- don't keep regex which have only wild card or space
      valid r = not $ null $ filter (`notElem` (".*?+ $^()" :: String)) (r :: String)

  -- customer rules
  -- we take the beginning of the payment reference
      entity0 = groupAsMap fst ((:[]) . snd) [ ("^" ++ reg ++ "$", entity :: Int)
                                               | These h f <- recs
                                               , B._fType f ==  show (fromEnum eType)
                                               , entity <- maybeToList (readMay $ B._fObject f)
                                               , let reg = mkReg $ B._hDescription h
                                               , valid reg
                                               ]
      entity1 = fmap (nub . sort) entity0
      --  and only keep the one without ambiguity
  in  mapFromList [ (pack ref, ruleFn debtor)
                  | (ref, [debtor]) <-  Map.toList entity1
                  ]

showDecimal :: B.Amount -> Text
showDecimal = F.sformat commasDecimal
