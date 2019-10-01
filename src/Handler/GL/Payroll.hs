{-# LANGUAGE ImplicitParams #-}
module Handler.GL.Payroll
-- * Export
( getGLPayrollR
, postGLPayrollValidateR
, postGLPayrollSaveR
, getGLPayrollViewR
, getGLPayrollEditR
, postGLPayrollEditR
, postGLPayrollRejectR
, postGLPayrollToFAR
, postGLPayrollToPayrooR
, getGLPayrollVoidFAR
, postGLPayrollVoidFAR
, module Handler.GL.Payroll.Summary
, module Handler.GL.Payroll.Calendar
, module Handler.GL.Payroll.Import
) where
-- * Import
import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput, bootstrapSubmit,BootstrapSubmit(..))
import Handler.Table
import qualified GL.Payroll.Timesheet as TS
import qualified GL.Payroll.Report as TS
import Handler.GL.Payroll.Common
import Handler.GL.Payroll.Summary
import Handler.GL.Payroll.Calendar
import Handler.GL.Payroll.QuickAdd
import Handler.GL.Payroll.Import
import GL.Payroll.Parser
import GL.Payroll.Settings
import Data.Text (strip)
import Database.Persist.MySQL
import Data.Time.Calendar
import Lens.Micro.Extras (view, preview)
import Lens.Micro hiding ((<&>))
import Data.These
import Data.Align
import Data.List.NonEmpty (NonEmpty(..))
import Data.List(nub)
import  qualified WH.FA.Types as WFA
import  qualified WH.FA.Curl as WFA
import Control.Monad.Except
import Text.Printf(printf)
import qualified Data.Map as Map
import Handler.Util
import Locker


-- * Types
data Mode = Validate | Save deriving (Eq, Read, Show)
data UploadParam = UploadParam
  { upTimesheet :: Textarea
  , upPreviousKey :: Maybe DocumentHash
  } deriving (Eq, Read, Show)

data FAParam = FAParam
  { ffDate :: Day
  , ffReferenceSuffix :: Maybe Text
  , ffPayments :: Map Text (Maybe Double, Maybe Day)
  } deriving (Eq, Read, Show)
-- * Form
uploadForm :: Mode -> Maybe UploadParam -> _Markup -> _ (FormResult UploadParam, Widget)
uploadForm mode paramM = let
  form _ = UploadParam <$> areq textareaField "Timesheet" (upTimesheet <$> paramM)
                       <*> areq hiddenField "key" (Just $ upPreviousKey =<< paramM)
  in renderBootstrap3 BootstrapBasicForm . form $ mode

-- faForm :: _ (FormResult Day, Widget)
faForm = renderBootstrap3 BootstrapBasicForm form  where
  form = FAParam <$> areq dayField "Processing date" Nothing
                 <*> aopt textField "Reference Suffix" Nothing
                 <*> pure mempty

voidForm = renderBootstrap3 BootstrapBasicForm form where
  form = areq boolField "Keep payments" (Just True)
  
-- * Handlers
-- ** upload and show timesheets
{-# NOINLINE getGLPayrollR #-}
getGLPayrollR :: Handler Html
getGLPayrollR = do
  pendingW <-displayPendingSheets
  lastW <-displayLastSheets 100
  renderMain Validate Nothing ok200 (setInfo ([shamlet|<h3>Enter a timesheet|] >> timesheetStyleCheat)) (pendingW >> lastW)
{-# NOINLINE postGLPayrollValidateR #-}
postGLPayrollValidateR :: Handler Html
postGLPayrollValidateR = do
  actionM <- lookupPostParam "action"
  case actionM of
   Just "quickadd" -> processTimesheet Validate quickadd
   _ -> processTimesheet Validate validate

validate :: UploadParam -> DocumentHash -> Handler Html
validate param key = do
          viewPayrollAmountPermissions' <- viewPayrollAmountPermissions
          viewPayrollDurationPermissions' <- viewPayrollDurationPermissions
          let ?viewPayrollAmountPermissions = viewPayrollAmountPermissions'
              ?viewPayrollDurationPermissions = viewPayrollDurationPermissions'
          settings <- appSettings <$> getYesod
          timesheetE <- parseTimesheetH param
          case validateTimesheet (appPayroll settings) =<< timesheetE of
            Left e -> setError (toHtml e) >> renderMain Validate (Just param) badRequest400 (setInfo "Enter a timesheet") (return ())
            Right timesheet -> do
                  timedShifts <- loadTimedShiftsFromTS $ timesheetPayrooForSummary timesheet
                  let ref = invoiceRef (appPayroll settings) timesheet :: String
                  (documentKey'msgM) <- runDB $ loadAndCheckDocumentKey key
                  forM documentKey'msgM  $ \(Entity _ doc, msg) -> do
                                 setWarning msg >> return ""
                  renderMain Save
                             (Just param {upPreviousKey = Just key})
                             ok200
                             (setInfo . toHtml $ "Timesheet " <> ref  <> " is valid.")
                             (do
                                 TS.filterTimesheet (const False) isDACUnlocked timesheet  `forM` (displayShifts displayDAC . TS._deductionAndCosts)
                                 displayTimesheetCalendar timedShifts (timesheetPayrooForSummary timesheet)
                                 -- TS.filterTimesheet isShiftDurationUnlocked (const True) timesheet `forM` displayTimesheet
                                 TS.filterTimesheet isShiftViewable isDACUnlocked timesheet `forM` (\ts -> do
                                      let vs = case toList (views (appPayroll settings) ) of
                                                    [] -> [[]]
                                                    vs -> vs
                                      forM vs $ \cols -> displayEmployeeSummary' (columnWeightFromList cols) . timesheetPayrooForSummary $ ts)
                                 return ()
                             )

postGLPayrollSaveR = do
  actionM <- lookupPostParam "action"
  case actionM of
   Just "quickadd" -> processTimesheet Validate saveQuickadd
   _ -> processTimesheet Save save
   where
      save param key = do
          -- if the document has been modified we need to validate it instead of saving it
          if Just key == upPreviousKey param
          then do
            timesheetE <- parseTimesheetH param
            case timesheetE of
              Left e -> setError (toHtml e)
                        >> renderMain Validate (Just param) badRequest400 (setInfo "Enter a timesheet") (return ())
              Right timesheet -> do
                    (tKey, ref) <- saveTimeSheet key timesheet
                    let tId = unSqlBackendKey (unTimesheetKey tKey)
                    pushLinks ("View Timesheet " <> ref)
                              (GLR $ GLPayrollViewR tId)
                              []
                    renderMain Validate Nothing created201 (setInfo "Timesheet saved sucessfully") (return ())
          else validate (param { upPreviousKey = Nothing}) key


{-# NOINLINE postGLPayrollToFAR #-}
postGLPayrollToFAR :: Int64 -> Handler Html
postGLPayrollToFAR key = do
  setting <- appSettings <$> getYesod
  modelE <- loadTimesheet key
  ((resp, formW), enctype) <- runFormPost faForm
  faParam' <- case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess day -> return day
  -- extract payment information
  faParam <- setFaParamPayments faParam'
  case modelE of
    Nothing -> error $ "Can't load Timesheet with id #" ++ show key
    Just (ts, shifts, items) -> do
      r <- postTimesheetToFA faParam (TimesheetKey $ SqlBackendKey key) ts shifts items
      case r of
        Left e -> setError (toHtml e) >> getGLPayrollViewR key
        Right e -> do
          -- update status
          runDB $ update (TimesheetKey $ SqlBackendKey key) [TimesheetStatus =. Process]
          setSuccess (toHtml $ tshow e)

          renderMain Validate Nothing created201 (setInfo "Timesheet saved to Front Account sucessfully") (return ())

setFaParamPayments :: FAParam ->  Handler FAParam
setFaParamPayments param = do
  (pp, _) <- runRequestBody
  let paramTo :: Read a => Text -> (Text, Text) -> Maybe (Text, a)
      paramTo field (name, value) = do -- Maybe
        employee <- stripPrefix ("payment-" <> field <> "-for-") name 
        val  <- readMay value
        return (employee, val)

      toMap :: Read a => Text -> Map Text a
      toMap field = mapFromList $ mapMaybe (paramTo field)  pp
      amounts = toMap "amount" :: Map Text Double
      dates = toMap "date" :: Map Text Day
      amount'dateMap = align amounts dates :: Map Text (These Double Day)
      paymentMap = (\a'd -> (preview here a'd, preview there a'd)) <$> amount'dateMap
  return param {ffPayments = paymentMap}




  
-- ** Individual timesheet
{-# NOINLINE getGLPayrollViewR #-}
getGLPayrollViewR :: Int64 -> Handler Html
getGLPayrollViewR key = do
  operatorMap <- allOperators
  modelE <- loadTimesheet key
  viewPayrollAmountPermissions' <- viewPayrollAmountPermissions
  viewPayrollDurationPermissions' <- viewPayrollDurationPermissions
  psettings <- appPayroll <$> getsYesod appSettings
  let ?viewPayrollAmountPermissions = viewPayrollAmountPermissions'
      ?viewPayrollDurationPermissions = viewPayrollDurationPermissions'
      -- Date used for invoice and payment
      -- We don't provide a default value
      -- to prevent accidental save to FrontAccounting
  (faFormW, faEncType)  <- generateFormPost faForm
  case modelE of
    Nothing -> error $ "Can't load Timesheet with id #" ++ show key
    Just (tse, shifts, items) -> do
      let ts = entityVal tse
          tsOId = modelToTimesheetOpId tse shifts items
          ts' = map (\(op, _) -> maybe (tshow op) operatorNickname (lookup op operatorMap)) tsOId
          -- reports' = [ ("Timesheet" :: Text, displayShifts)
          --           , ("By Employees", displayShifts . (TS.groupShiftsBy id))
          --           , ("By Week", displayShifts . (TS.groupShiftsBy (\(e,_,_) -> e)))
          --           , ("By Day" , displayShifts . ( TS.groupShiftsBy (\(a,b,h) -> (h,b,a))))
          --           ]
          dacsReport = ("Deductions and Costs" :: Text, displayShifts displayDAC . TS._deductionAndCosts, TS.filterTimesheet (const False) isDACUnlocked )
          summaryReport = ("Summary", displayEmployeeSummary , TS.filterTimesheet isShiftViewable isDACUnlocked)
          summaries0 = [(viewName, displayEmployeeSummary' (columnWeightFromList colnames)  , TS.filterTimesheet isShiftViewable isDACUnlocked)
                  | (viewName, colnames) <- Map.toList $ views psettings
                  ]
          summaries = case summaries0 of
            [] -> [summaryReport]
            ss -> ss
      timedShifts <- loadTimedShiftsFromTS ts'
      let calendar = ("Calendar", displayTimesheetCalendar timedShifts, TS.filterTimesheet isShiftDurationUnlocked (const False) )
          -- reports = [(name, report  . TS._shifts, TS.filterTimesheet isShiftViewable (const False) ) | (name, report)  <- reports']
          reports0 = [calendar, dacsReport] <> summaries
          reports = reports0 <> (if timesheetStatus ts /= Process
                                then [("Generate Payments", addFAForm . generatePaymentForm, TS.filterTimesheet isShiftViewable isDACUnlocked)]
                                else  [] )
          addFAForm w = [whamlet|
            <form role=form method=post action=@{GLR $ GLPayrollToFAR key} enctype=#{faEncType}>
              ^{w}
              ^{faFormW}
              <button type="submit" .btn.btn-danger>Save To FrontAccounting
                                |]
          
      let showDate d =  formatTime defaultTimeLocale "%a %d %h %Y" d
      defaultLayout $ do
        setTitle . toHtml $ "Timesheet " <> timesheetReference ts
        [whamlet|
          <div.row>
            <div.col-md-2><h2> #{timesheetReference ts} 
            <div.col-md-4.col-md-offset-6><h3> #{showDate $ timesheetStart ts } - #{showDate $ timesheetEnd ts}
          $forall (name, trans, filter') <- reports
             $maybe object <- filter' ts'
              <div.panel.panel-info>
                <div.panel-heading> #{name}
                <div.panel-body>
                  <div>^{trans object }
          $if (timesheetStatus ts /= Process)
            <form role=form method=post action=@{GLR $ GLPayrollRejectR key}>
              <button type="submit" .btn.btn-warning>Reject 
            <form role=form method=post action=@{GLR $ GLPayrollToPayrooR key}>
              <button type="submit" .btn.btn-info>Download Payroo 
          $else 
            <form role=form method=get action=@{GLR $ GLPayrollVoidFAR key}>
              <button type="submit" .btn.btn-danger>Void in FrontAccounting
                              |]

{-# NOINLINE getGLPayrollEditR #-}
getGLPayrollEditR :: Int64 -> Handler Html
getGLPayrollEditR key = return "todo"
{-# NOINLINE postGLPayrollEditR #-}
postGLPayrollEditR :: Int64 -> Handler Html
postGLPayrollEditR key = return "todo"

-- | Delete the timesheet and its component
-- unless it has transaction linked to it
{-# NOINLINE postGLPayrollRejectR #-}
postGLPayrollRejectR :: Int64 -> Handler Html
postGLPayrollRejectR tId = do
  let key = TimesheetKey $ SqlBackendKey tId
  timesheetM <- runDB $ get key
  case timesheetM of
    Nothing -> do
      renderMain Validate Nothing gone410 (setError $ toHtml $  "Timesheet #" <> tshow tId <> " doesn't exists.") (return ())
    Just timesheet -> case timesheetStatus timesheet of
      Process  -> renderMain Validate Nothing badRequest400 (setError $ toHtml $ "Timesheet #" <> tshow tId <> " has already been processed.") (return ())
      Pending -> do
            c <- runDB $ do
             criteria <- transactionMapFilterForTS tId
             count  ((TransactionMapVoided ==. False) : criteria)
            if c == 0
            then  do
              runDB $ deleteCascade key  >> deleteWhere [TransactionMapEventNo ==. fromIntegral tId , TransactionMapEventType ==. TimesheetE, TransactionMapVoided ==. True]
              renderMain Validate Nothing ok200 (setSuccess $ toHtml $ "Timesheet #" <> tshow tId <> " has been deleted sucessfully" ) (return ())
            else do
               -- linked to Fa transaction, only clear
               runDB $ do
                  deleteWhere [PayrollShiftTimesheet ==. key]
                  deleteWhere [PayrollItemTimesheet ==. key]
               let msg = do
                              setWarning $ toHtml $ "Timesheet #" <> tshow tId <> " is linked to FA transaction and can not be deleted"
                              setSuccess $ toHtml $ "Timesheet #" <> tshow tId <> " has been cleared sucessfully" 
               renderMain Validate Nothing ok200 msg  (return ())
            
      -- _  -> renderMain Validate Nothing preconditionFailed412 (setError $ toHtml $ "Timesheet #" <> tshow tId <> " is not pending.") (return ())

  

{-# NOINLINE postGLPayrollToPayrooR #-}
postGLPayrollToPayrooR :: Int64 -> Handler TypedContent
postGLPayrollToPayrooR key = do
  header <- headerFromSettings
  operatorMap <- allOperators
  opFinder <- operatorFinderWithError
  modelE <- loadTimesheet key
  viewPayrollAmountPermissions' <- viewPayrollAmountPermissions
  viewPayrollDurationPermissions' <- viewPayrollDurationPermissions
  let ?viewPayrollAmountPermissions = viewPayrollAmountPermissions'
      ?viewPayrollDurationPermissions = viewPayrollDurationPermissions'
  case modelE of
    Nothing -> error $ "Can't load Timesheet with id #" ++ show key
    Just (tsE, shifts, items) -> do
      let tsOId = modelToTimesheetOpId tsE shifts items
          ts = entityVal tsE
          start = timesheetStart ts
          end = timesheetEnd ts
      tsE <- timesheetOpIdToO'SH tsOId
      case tsE of
        Right ts' ->  do
            let payroos = TS.payroo timesheet
                source = yieldMany (map (<>"\n") payroos)
                timesheet = fmap ( \(Entity _ Operator{..} ,empS,_)
                                   -> TS.PayrooEmployee 
                                          (unpack operatorFirstname)
                                          (unpack operatorSurname)
                                          (payrollId empS)
                                          (TS.Employee (unpack operatorNickname) Nothing)
                                  ) ts'
            setAttachment (fromStrict $ "payroo" <> tshow start <> "--" <> tshow end  <> ".csv")
            respondSource "text/csv" (source .| mapC toFlushBuilder)
        Left e -> error $ "Problem generating payroo csv: " <> unpack e

-- ** Void
{-# NOINLINE getGLPayrollVoidFAR #-}
getGLPayrollVoidFAR :: Int64 -> Handler Html
getGLPayrollVoidFAR timesheetId = do
  faURL <- getsYesod (pack . appFAExternalURL . appSettings) :: Handler Text
  (voidFormW, voidEncType) <- generateFormPost voidForm
  -- display things to delete
  transactions <- runDB $ do
    criteria <- transactionMapFilterForTS timesheetId
    selectList criteria  [Asc TransactionMapFaTransType, Asc TransactionMapFaTransNo ]
  let t x = x :: Text
      tranMap = groupAsMap (liftA3 (,,) transactionMapFaTransType transactionMapFaTransNo transactionMapVoided . entityVal)
                           ((:[]) . transactionMapEventType . entityVal)
                           transactions
      transTable = [whamlet|
      <table.table.table-hover.table-strip>
        <tr>
            <th> Trans Type
            <th> Trans No
            <th> Event Type
            <th> Voided
        $forall ((transType, transNo, voided ), events) <- mapToList tranMap
          <tr :voided:.text-muded>
            <td> #{t $ showTransType transType}
            <td>
              <a href="#{urlForFA faURL transType transNo}">#{tshow transNo}
            <td>
              $forall event <- nub (sort events)
                <span>#{tshow event}
            <td>
              $if voided
                   Voided
                   |]
  defaultLayout $ infoPanel ("Transaction for timesheet #" <> tshow timesheetId) [whamlet|
     <form role=form method=POST action="@{GLR $ GLPayrollVoidFAR timesheetId}" encType="#{voidEncType}">
        ^{transTable}
        ^{voidFormW}
        <button type="submit" .btn.btn-danger>Void
      |]
  
  
{-# NOINLINE postGLPayrollVoidFAR #-}
postGLPayrollVoidFAR :: Int64 -> Handler Html
postGLPayrollVoidFAR timesheetId = do
  today <- todayH
  ((resp, formW), enctype) <- runFormPost voidForm
  keepPayment <- case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess keepPayment_ -> return keepPayment_
  runDB $ do
    criteria0 <- transactionMapFilterForTS timesheetId 
    let criteria = ( if keepPayment
                     then [TransactionMapFaTransType !=. ST_SUPPAYMENT]
                     else []
                   ) <> criteria0
    nb <- lift $ voidTransactions today (const . Just $ "Timesheet #" <> tshow timesheetId <> " voided") criteria
    update (TimesheetKey $ SqlBackendKey timesheetId) [TimesheetStatus =. Pending]

    lift $ setSuccess. toHtml $ tshow nb <> " FA transaction(s) have been successufully voide"
  getGLPayrollViewR timesheetId
  -- delete FA transaction

  
-- Create a filter load all transaction connected to the timesheet
  -- using also link via payroll shifts and items
transactionMapFilterForTS :: Int64 -> SqlHandler [Filter TransactionMap]
transactionMapFilterForTS timesheetId =  do
  let key = TimesheetKey (SqlBackendKey timesheetId)
  shiftIds <- selectKeysList [PayrollShiftTimesheet ==. key ] []
  itemIds <- selectKeysList [PayrollItemTimesheet ==. key] []


  let for event unkey keys = [ TransactionMapEventType ==. event
                             , TransactionMapEventNo <-. map (fromIntegral . unSqlBackendKey . unkey ) keys
                             ]
  return $ [TransactionMapEventType ==. TimesheetE  , TransactionMapEventNo ==. fromIntegral timesheetId]
          ||. for PayrollShiftE unPayrollShiftKey shiftIds
          ||. for PayrollItemE unPayrollItemKey itemIds

-- ** Quick Add
quickadd :: UploadParam -> DocumentHash -> Handler Html
quickadd  param key = do
  wE <- saveQuickAdd False (unTextarea $ upTimesheet param) key
  case wE of
    Left e -> setError (toHtml e) >> renderMain Validate (Just param) badRequest400 (setInfo "Enter a valid timesheet") (return ())
    Right w -> renderMain Save (Just $ param { upPreviousKey = Just key}) ok200 (return()) w
    -- Right w -> defaultLayout [whamlet|
    --    <form #quick-form role=form method=post action=@{GLR GLPayrollSaveR }> 
    --      <input type=hidden name=quickadd value="#{unTextarea $ upTimesheet param}">
    --      ^{w}
    --      <button type="submit" name="action" value="quickadd" class="btn btn-danger">Quick Add
saveQuickadd :: UploadParam -> DocumentHash -> Handler Html
saveQuickadd  param key = do
  if   Just key /= upPreviousKey param
      then quickadd param key
      else do
        wE <- saveQuickAdd True (unTextarea $ upTimesheet param) key
        case wE of
          Left e -> setError (toHtml e) >> renderMain Validate (Just param) badRequest400 (setInfo "Enter a valid timesheet") (return ())
          Right w -> do
            setSuccess "Quickadds processed properly"
            getGLPayrollR
            -- renderMain Validate Nothing created201 msg w

-- ** Renders
-- | Renders the main page. It displays recent timesheet and also allows to upload a new one.
-- The given mode is actually the next one, if the spreadsheet needs to be validated or saved.
renderMain :: Mode -> (Maybe UploadParam) -> Status -> Handler() -> Widget -> Handler Html
renderMain mode paramM status message pre = do
  let (action, button,btn) = case mode of
        Validate -> (GLPayrollValidateR, "validate" :: Text, "primary" :: Text)
        Save -> (GLPayrollSaveR, "save", "danger")
  (upFormW, upEncType) <- generateFormPost $ uploadForm mode paramM
  message
  sendResponseStatus status =<< defaultLayout
     [whamlet|
     ^{pre}
     <div.well>
       <form #upload-form role=form method=post action=@{GLR action} enctype=#{upEncType}>
         ^{upFormW}
        <button type="submit" name="#{button}" value="#{tshow mode}" class="btn btn-#{btn}">#{button}
        <button type="submit" name="action" value="quickadd" class="btn btn-danger">Quick Add
        |]
-- | Quick documentation to remind the timesheet syntax
timesheetStyleCheat :: Html
timesheetStyleCheat = [shamlet|
<p>
  The timesheet is made of a sections, each one representing an operator
  The timesheet <b>MUST</b> start with the first date of the related period and the followed
  by many operator sections.
  An operator section starts with the operator name and be followed by it's <b>shift</b> or cost and deductions
  on the same or a different line.

<p>
  The <b>shift</b> represents a amount of time an can be either
  <ul>
    <li> A duration in (decimal)hour, example
      <code>7.5</code> corresponds to 7 hours and 30 min
    <li> A duration in hour,minute, example
      <code>7h30</code>.
    <li> A time range, example
      <code>09:30-13:00</code>
    <li> Holidays must be preceeded by <code>!</code>
      example, <code>!7.5</code> corresponds to 7h30 of holidays
    <li> A day can also been skipped using <code>_</code>
      This mean that the next duration will concern the next days
      example
    
<p> In each section, <b>shifts</b>  represents one day starting at the beginning of the period.
  A day (3 first letter in uppercase) can be specified to specify the day, example:
    <code> Alice Wed 4
<p>
  Means, Alice worked 4 hours on Wednesday.
    <code> Alice Wed 4 3 
<p>
  Means Alice workd 4 hours on Wednesday and 3 on Thursday
  But <code> Alice Wed 4 _ 3 </code>
  Means Alice workd 4 hours on Wednesday and 3 on <b>Friday</b>
<p> A multiplicator can be used to repeat a shift, example
  <code>Alice 5x8</code>
  Will be equivalent to <code>Alice 8 8 8 8 8</code>
<p> Finally, <code>|</code> can be use to specify two shifts on the same day
Example, <code>Alice Wed 4|!4</code>
Means, that Alice worked 4 hours on Wednesday morning on took her afternoon off.
                                
<p>
<p>Please note that entering a day without shift information doesn't skip to the next day, so <code>Mon Mon</code>
Doesn't go the next Monday even though <code>Mon 8 Mon</code> does.
To do so, days needs to be explicetly skipped using <code>_</code>
Example, <code>Alice Mon _ Mon 8</code>, means Alice worked 8 hours on the 2nd Monday of the period.
|]

-- * Processing
processTimesheet :: Mode -> (UploadParam -> DocumentHash -> Handler r) -> Handler r
processTimesheet mode post = do
  ((resp, formW), enctype) <- runFormPost (uploadForm mode Nothing)
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show (mode, a)
    FormSuccess param -> do
      let bytes = encodeUtf8 . unTextarea $ upTimesheet param
          key = computeDocumentKey bytes
      post param key

parseTimesheet :: [Text] -> Textarea -> (Either String (TS.Timesheet String TS.PayrooEmployee))
parseTimesheet header0 ts = parseFastTimesheet strings where
  header = map unpack header0
  strings' = map (unpack . strip) . lines $ unTextarea $ ts
  -- insert the header after the first line which should be the period definition
  strings = case strings' of
    (x:xs) -> x : header ++ xs
    [] -> header

parseTimesheetH :: UploadParam -> Handler (Either String (TS.Timesheet String TS.PayrooEmployee))
parseTimesheetH param = do
  header <- headerFromSettings
  return $ parseTimesheet header (upTimesheet param)

-- * To Front Accounting
postTimesheetToFA :: FAParam
                  -> TimesheetId
                  -> Entity Timesheet
                  -> [Entity PayrollShift]
                  -> [Entity PayrollItem]
                  -> Handler (Either Text Int)
postTimesheetToFA param key timesheet shifts items = do
      settings <- appSettings <$> getYesod
      mkAccount <- mkAccountH
      let today = ffDate param
      let tsOId = modelToTimesheetOpId timesheet shifts items
      runExceptT $ do
         ts <- ExceptT $ timesheetOpIdToO'SH tsOId
         let tsSkus = ( \(op,setting, shiftKey) -> ( TS.Sku . unpack . faSKU $  setting
                                                 , shiftKey
                                                 )
                      ) <$> ts
             tsPayment =  (\(Entity _ op, settings, shiftKey) -> operatorNickname op) <$> ts
         grnIds <- saveGRNs settings key tsSkus
         invoiceId <- saveInvoice mkAccount today settings ts grnIds
         paymentIds <- savePayments today (ffReferenceSuffix param) (ffPayments param) settings key tsPayment invoiceId
         credits <- saveExternalPayments settings key invoiceId mkAccount today ts
         return invoiceId


  
