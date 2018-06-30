module Handler.Bank 
( getGLBankR
, getGLBankDetailsR
, getGLBankReconciliateR
, postGLBankReconciliateR
) where


import Import
import qualified BankReconciliate as B
import BankReconciliate()
import Database.Persist.MySQL     (MySQLConf (..))
import System.Directory
import Text.Regex.TDFA ((=~), makeRegex, Regex)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput, bootstrapSubmit,BootstrapSubmit(..))
import Data.These
import Data.Time (diffDays)
import Lens.Micro.Extras (preview)
import FA as FA
import GL.Utils
import GL.Payroll.Settings

commonCss = [cassius|
tr.private
  font-style: italic
  opacity: 0.5
  &:hover
    font-style: normal
    opacity: 1
|]

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
   var diffE = $("input.rec-difference");
   diffE.val(diff);
   if(diff==0) {
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

getGLBankR  :: Handler Html
getGLBankR = do
  today <- utctDay <$> liftIO getCurrentTime
  dbConf <- appDatabaseConf <$> getsYesod appSettings
  faURL <- getsYesod (pack . appFAExternalURL . appSettings)
  settings' <- getsYesod (appBankStatements . appSettings)

  role <- currentRole
  -- only keep authorised settings and which have a position
  -- setting the position to Nothing allows to quickly deactivate a panel 
  -- it can still be access via the Bank details page
  let settings = filter filterSettings (mapToList settings')
      filterSettings (account, bsetting) = isJust (bsPosition bsetting) && (canViewBankStatement role account || canViewBankLightStatement role account)
      -- chose the display function depending on permission
      display (account, bs) = if canViewBankStatement role account
                              then displaySummary today dbConf faURL account bs
                              else displayLightSummary today dbConf faURL account bs
  panels <- forM (sortOn (bsPosition . snd) settings) display
  defaultLayout $ toWidget commonCss >> (mconcat panels)


canViewBankStatement :: Role -> Text -> Bool
canViewBankStatement role account = authorizeFromAttributes role (setFromList ["bank/" <> account ]) ReadRequest
canViewBankLightStatement :: Role -> Text -> Bool
canViewBankLightStatement role account = authorizeFromAttributes role (setFromList ["bank/light/" <> account ]) ReadRequest


-- | Displays a collapsible panel 
displayPanel :: Bool -> Bool -> Text -> Widget -> Widget
displayPanel ok collapsed account content =
  let panelId = "bank-"  <> account
  in [whamlet|
    <div.panel :ok:.panel-success:.panel-danger>
      <div.panel-heading data-toggle="collapse" data-target="##{panelId}">
          <h2 style=>#{account}
      <div.panel-body.collapse :collapsed:.out:.in id="#{panelId}">
        ^{content}
        |]

-- | Display all non matching transaction as well as a preview of last the statement 
displaySummary :: Day -> _DB -> Text -> Text ->  BankStatementSettings -> Handler Widget
displaySummary today dbConf faURL title BankStatementSettings{..}= do
  object <- getObjectH
  let options = B.Options{..}
      hsbcFiles = unpack bsStatementGlob
      faCredential = myConnInfo dbConf
      statementFiles = unpack bsDailyGlob
      output = ""
      startDate = bsStartDate
      endDate = Nothing -- Just today
      faMode = B.BankAccountId (bsBankAccount)
      aggregateMode = B.BEST
      blacklist = map unpack bsLightBlacklist
  
  (stransz, banks) <- lift $ withCurrentDirectory bsPath (B.main options)
  -- we sort by date
  let sortTrans = sortOn (liftA3 (,,) (Down . B._sDate) (Down . B._sDayPos) (Down . B._sAmount))
      hideBlacklisted t = if keepLight blacklist t then ["public" ] else ["private"]
      sorted = sortTrans stransz
      ok = null sorted
      lastBanks = take 10 $ sortTrans banks
      lastW = renderTransactions object faURL lastBanks hideBlacklisted (Just "Total") (const False)
      tableW = renderTransactions object faURL sorted hideBlacklisted (Just "Total") ((B.FA ==) . B._sSource)
  return $ displayPanel ok ok title [whamlet|
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
           <h3> Last 10
        ^{lastW}
                     |]

displayLightSummary :: Day -> _DB -> Text -> Text ->  BankStatementSettings -> Handler Widget
displayLightSummary today dbConf faURL title BankStatementSettings{..}= do
  object <- getObjectH
  let options = B.Options{..}
      hsbcFiles = unpack bsStatementGlob
      faCredential = myConnInfo dbConf
      statementFiles = unpack bsDailyGlob
      output = ""
      startDate = bsStartDate
      endDate = Nothing -- Just today
      faMode = B.BankAccountId (bsBankAccount)
      aggregateMode = B.BEST
  
  (stransz, banks) <- lift $ withCurrentDirectory bsPath (B.main options)
  -- we sort by date
  let sortTrans = filter (keepLight blacklist) . sortOn (liftA3 (,,) (Down . B._sDate) (Down . B._sDayPos) (Down . B._sAmount))
      sorted = sortTrans stransz
      ok = null sorted
      blacklist = map unpack bsLightBlacklist
      lastBanks = take 10 $ sortTrans banks
      lastW = renderTransactions object faURL lastBanks (const []) (Just "Total") (const False)
      tableW = renderTransactions object faURL sorted  (const [])(Just "Total") ((B.FA ==) . B._sSource)
  return $ displayPanel ok ok title [whamlet|
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
keepLight :: [String] -> B.STrans -> Bool
keepLight blacklist tr = B._sAmount tr >0
                         && not (blacklisted (B._sDescription tr))
                         && B._sSource tr == B.HSBC
                         where blacklisted s = any (s =~) blacklist


linkToFA :: (FATransType -> Int -> Text) -> B.STrans -> Html
linkToFA urlForFA' trans = case (readMay (B._sType trans), B._sNumber trans) of
  (Just t, Just no) | B._sSource trans == B.FA ->
           let eType = toEnum t
               trans = showTransType eType :: Text
           in [shamlet|
     <a href="#{urlForFA' (eType) no}" target=_blank>
       #{trans}
       |]
  _ -> toHtml (B._sType trans)

renderTransactions :: (B.STrans -> Maybe Text) -> Text -> [B.STrans] -> (B.STrans -> [Text]) -> Maybe Text -> (B.STrans -> Bool) -> Widget
renderTransactions object faURL sorted mkClasses totalTitle danger = 
      let (ins, outs) = partition (> 0) (map B._sAmount sorted)
          inTotal = sum ins
          outTotal = sum outs
          total = inTotal + outTotal
         
      in [whamlet| 
        <table.table.table-hover.table-border.table-striped>
          <tr>
            <th>Date
            <th>Source
            <th>Type
            <th>Description
            <th>Number
            <th>Object
            <th>Paid Out
            <th>Paid In
          $forall trans <- sorted
            $with isDanger <- danger trans
              <tr :isDanger:.text-danger :isDanger:.bg-danger class="#{intercalate " " (mkClasses trans)}">
                  <td>#{tshow $ B._sDate trans}
                  <td>#{tshow $ B._sSource trans}
                  <td>^{linkToFA (urlForFA faURL) trans}
                  <td>#{B._sDescription trans}
                  <td>#{maybe "-" tshow $ B._sNumber trans}
                  <td>#{fromMaybe "-" (object trans)}
                  $if B._sAmount trans > 0
                    <td>
                    <td>#{tshow $  B._sAmount trans}
                  $else
                    <td>#{tshow $ negate  $    B._sAmount trans}
                    <td>
          <tr>
            $maybe totalTitle <-  totalTitle
              <th> #{totalTitle}
              <th> #{tshow total}
              <th>
              <th>
              <th>
              <th>
              <th>#{tshow $ negate outTotal}
              <th>#{tshow inTotal}
              |]

settingsFor :: Text -> Handler BankStatementSettings
settingsFor account = do
  role <- currentRole
  when (not $ canViewBankStatement role account)
     (permissionDenied account)
  allSettings <- getsYesod (appBankStatements . appSettings)
  case lookup account allSettings of
    Nothing -> error $ "Bank Account " <> unpack account <> " not found!"
    Just settings -> return settings
  
getGLBankDetailsR :: Text -> Handler Html
getGLBankDetailsR account = do
  settings <- settingsFor account
  defaultLayout =<< displayDetailsInPanel account settings 

displayDetailsInPanel :: Text -> BankStatementSettings -> Handler Widget
displayDetailsInPanel account BankStatementSettings{..} = do
  today <- utctDay <$> liftIO getCurrentTime
  dbConf <- appDatabaseConf <$> getsYesod appSettings
  faURL <- getsYesod (pack . appFAExternalURL . appSettings)
  object <- getObjectH
  let options = B.Options{..}
      hsbcFiles = unpack bsStatementGlob
      faCredential = myConnInfo dbConf
      statementFiles = unpack bsDailyGlob
      output = ""
      startDate = bsStartDate
      endDate = Nothing -- Just today
      faMode = B.BankAccountId (bsBankAccount)
      aggregateMode = B.BEST
  
  (stransz, banks) <- lift $ withCurrentDirectory bsPath (B.main options)
  let tableW = renderTransactions object faURL stransz (const []) (Just "Total") ((B.FA ==) . B._sSource)
      ok = null stransz
  return $ displayPanel ok ok account [whamlet|
           <h3> By amount
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
defaultParam = RecParam Nothing 0 Nothing  0 Nothing

recForm  paramM = renderBootstrap3 BootstrapBasicForm form  where
  form = RecParam <$> aopt dayField "start" (rpStartDate <$> paramM)
                  <*> areq doubleField "Opening Balance" (rpOpeningBalance <$> paramM)
                  <*> aopt dayField "end date" (rpEndDate <$> paramM)
                  <*> areq doubleField "Closing Balance" (rpClosingBalance <$> paramM)
                  <*> aopt dayField "reconciliate" (rpRecDate <$> paramM)

getGLBankReconciliateR :: Text -> Handler Html
getGLBankReconciliateR account = do
  today <- utctDay <$> liftIO getCurrentTime
  renderReconciliate account defaultParam  {rpStartDate = Just $ calculateDate (AddMonths (-3)) today}
  
postGLBankReconciliateR :: Text -> Handler Html
postGLBankReconciliateR account = do
  ((resp, formW), encType) <- runFormPost (recForm Nothing)
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
                Just recDate -> saveReconciliation recDate
            renderReconciliate account param


renderReconciliate :: Text -> RecParam -> Handler Html
renderReconciliate account param = do
  dbConf <- appDatabaseConf <$> getsYesod appSettings
  (form, encType) <- generateFormPost (recForm $ Just param)
  faURL <- getsYesod (pack . appFAExternalURL . appSettings)
  object <- getObjectH
  BankStatementSettings{..} <- settingsFor account
  let options = B.Options{..}
      hsbcFiles = unpack bsStatementGlob
      faCredential = myConnInfo dbConf
      statementFiles = unpack bsDailyGlob
      output = ""
      startDate = bsStartDate
      endDate = Nothing -- Just today
      faMode = B.BankAccountId (bsBankAccount)
      aggregateMode = B.ALL_BEST

  (hts,_) <- lift $ withCurrentDirectory bsPath (B.loadAllTrans options)
  let byDays = B.badsByDay hts
      -- group by rec
      st'sts = filter transFilter $ map (bimap B.hToS B.fToS)  byDays
      recGroup = groupAsMap (B._sRecDate . B.thatFirst) (:[]) st'sts
      -- exclude a pair if both date are outside the range
      transFilter t | d <- mergeTheseWith B._sDate B._sDate max t,   Just start <- bsStartDate, d < start = False
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
      forInitRec (These h fa) = maybe True (B._sDate h <=) (rpRecDate param) 
                              && maybe True (\(tRecDate, recDate) -> tRecDate == recDate ) ((,) <$> B._sRecDate  fa <*> rpRecDate param)
      forInitRec (That fa) = isJust $ B._sRecDate fa
      forInitRec _ = False
      reconciliated :: Double
      reconciliated = fromRational . toRational $ sum $ map (B._sAmount . B.thisFirst) (filter forInitRec st'sts)
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
  defaultLayout $ do
    toWidget commonCss
    toWidget recJs
    [whamlet|
    <form.form-inline action="@{GLR (GLBankReconciliateR account)}" method=POST enctype="#{encType}">
      <div.well>
        ^{form}
        <button.btn.btn-primary name=action value="submit">Submit
      ^{mconcat panels}
      <div.well.footer.navbar-fixed-bottom>
       ^{statusW}
        <button.btn.btn-warning name=action value="reconciliate">Save
            |]
  
displayRecGroup :: (These B.STrans B.STrans -> Bool) -> Text -> (B.STrans -> Maybe Text) -> (Maybe Day, [These B.STrans B.STrans]) -> Widget
displayRecGroup toCheck faURL object (recDateM, st'sts0) = let
  st'sts = sortOn (((,) <$> B._sDate <*> B._sDayPos) . B.thisFirst) st'sts0
  title = maybe "" tshow recDateM
      -- check if the difference of the two date is acceptable
  dateClass (This _) = ""
  dateClass (That _) = ""
  dateClass (These h f) = let d = diffDays (B._sDate h) (B._sDate f)
                          in if abs(d)  > 28
                              then "text-danger" :: Text
                              else if abs(d) > 7
                                  then "text-warning"
                                  else "text-success"
  rowClass (This _) = "bg-warning text-warning "
  rowClass (That _) = "bg-danger text-danger"
  rowClass (These _ _) = "" :: Text
  widget = [whamlet|
    <table.table.table-hover.table-border>
      <tr>
              <th>Date 
              <th>FA Date
              <th>Source
              <th>Source
              <th>Type 
              <th>FA Type
              <th>Description
              <th>FA Ref.
              <th>Number 
              <th>Object
              <th>Paid Out
              <th>Paid In
              <th>Reconciliate
                <input.toggle-all type=checkbox checked>
      $forall st'st <- st'sts
        $with (trans, fatrans, transM, fatransM) <- (B.thisFirst st'st, B.thatFirst st'st, preview here st'st, preview there st'st)
          <tr class="#{rowClass st'st}">
              <td>#{maybe "" (tshow . B._sDate) transM}
              <td class="#{dateClass st'st}">#{maybe "" (tshow . B._sDate) fatransM}
              <td>#{maybe "" (tshow . B._sSource) transM}
              <td>#{maybe "" (tshow . B._sSource) fatransM}
              <td>#{maybe "" B._sType transM}
              <td>^{maybe "" (linkToFA (urlForFA faURL)) fatransM}
              <td>#{maybe "" B._sDescription transM}
              <td>#{maybe "" B._sDescription fatransM}
              <td>#{maybe "-" tshow $ B._sNumber trans}
              <td>#{fromMaybe "-" (object fatrans)}
              $if B._sAmount trans > 0
                  <td>
                  <td>#{tshow $  B._sAmount trans}
              $else
                  <td>#{tshow $ negate  $    B._sAmount trans}
                  <td>
              $with checked <- toCheck st'st
                <td :checked:.update-rec data-amount="#{tshow (B._sAmount trans)}">
                  $if isThese st'st 
                    $if isJust (B._sRecDate fatrans)
                        <input type=hidden name="already-#{faId fatrans}" value=off>
                        <input type=checkbox name="keepset-#{faId fatrans}" data-init-checked checked>
                    $else
                        <input type=checkbox name="set-#{faId fatrans}":checked:checked :checked:data-init-checked>
              |]
  in displayPanel False False title widget

-- | Computes the checkbox prefix for a transaction.
faId :: B.STrans -> Text
faId strans = let
  prefix = if isJust (B._sRecDate strans)
           then "rec-"
           else "rec-"
  in prefix <> pack (B._sType strans) <> "-" <> maybe "0" tshow (B._sNumber strans)

saveReconciliation :: Day -> Handler ()
saveReconciliation recDate = do
  (params, _) <- runRequestBody
  let toSet' = catMaybes [ decodeKey key
              | (key', value) <- params
              -- , value == "on"
              , Just key <-  [stripPrefix "set-rec-" key']
              ]
      already = catMaybes [ decodeKey key
                | (key', value) <- params
                , Just key <-  [stripPrefix "already-rec-" key']
                ]
      
      tokeep = catMaybes [ decodeKey key
               | (key', value) <- params
               -- , value == "on"
               , Just key <-  [stripPrefix "keepset-rec-" key']
               ]
      toUnset = setFromList already \\ setFromList tokeep :: Set (Int, Int)
      decodeKey key = let (a,b) = break (=='-') key in (,)  <$> readMay a <*> readMay  (drop 1 b)
  runDB $ do
    saveRecDate recDate toSet'
    unsetRecDate (setToList toUnset)

-- saveRecDate :: Day -> [(Int, Int)] -> Handler ()
saveRecDate recDate transIds = do
  forM_ transIds $ \(t, no) -> 
     updateWhere [FA.BankTranType ==. Just t, FA.BankTranTransNo ==. Just no]
                 [FA.BankTranReconciled =. Just recDate]

-- unsetRecDate :: [(Int, Int)] -> Handler ()
unsetRecDate transIds = do
  forM_ transIds $ \(t, no) -> 
     updateWhere [FA.BankTranType ==. Just t, FA.BankTranTransNo ==. Just no]
                 [FA.BankTranReconciled =. Nothing]



  

  -- get all ids

getObject :: Map Text Text -> Map Text Text -> B.STrans -> Maybe Text
getObject customerMap supplierMap trans = do -- Maybe
  object <- pack <$> B._sObject trans
  let mapped = case toEnum <$> readMay (B._sType trans) of
                 Just ST_CUSTPAYMENT -> lookup object customerMap
                 Just ST_SUPPAYMENT -> lookup object supplierMap
                 _ -> Nothing
  return . decodeHtmlEntities $ fromMaybe object mapped

getObjectH :: Handler (B.STrans -> Maybe Text)
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
