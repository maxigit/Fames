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
import Data.List.NonEmpty (NonEmpty(..))
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

-- * Form
uploadForm :: Mode -> Maybe UploadParam -> _Markup -> _ (FormResult UploadParam, Widget)
uploadForm mode paramM = let
  form _ = UploadParam <$> areq textareaField "Timesheet" (upTimesheet <$> paramM)
                       <*> areq hiddenField "key" (Just $ upPreviousKey =<< paramM)
  in renderBootstrap3 BootstrapBasicForm . form $ mode
-- * Handlers
-- ** upload and show timesheets
getGLPayrollR :: Handler Html
getGLPayrollR = do
  pendingW <-displayPendingSheets
  lastW <-displayLastSheets 10
  renderMain Validate Nothing ok200 (setInfo ([shamlet|<h3>Enter a timesheet|] >> timesheetStyleCheat)) (pendingW >> lastW)
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
                                 displayTimesheetCalendar (timesheetPayrooForSummary timesheet)
                                 -- TS.filterTimesheet isShiftDurationUnlocked (const True) timesheet `forM` displayTimesheet
                                 TS.filterTimesheet isShiftViewable isDACUnlocked timesheet `forM` (displayEmployeeSummary . timesheetPayrooForSummary)
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


postGLPayrollToFAR :: Int64 -> Handler Html
postGLPayrollToFAR key = do
  setting <- appSettings <$> getYesod
  modelE <- loadTimesheet key
  case modelE of
    Nothing -> error $ "Can't load Timesheet with id #" ++ show key
    Just (ts, shifts, items) -> do
      r <- postTimesheetToFA (TimesheetKey $ SqlBackendKey key) ts shifts items
      case r of
        Left e -> setError (toHtml e) >> getGLPayrollViewR key
        Right e -> do
          -- update status
          runDB $ update (TimesheetKey $ SqlBackendKey key) [TimesheetStatus =. Process]
          setSuccess (toHtml $ tshow e)

          renderMain Validate Nothing created201 (setInfo "Timesheet saved to Front Account sucessfully") (return ())

-- ** Individual timesheet
getGLPayrollViewR :: Int64 -> Handler Html
getGLPayrollViewR key = do
  operatorMap <- allOperators
  modelE <- loadTimesheet key
  viewPayrollAmountPermissions' <- viewPayrollAmountPermissions
  viewPayrollDurationPermissions' <- viewPayrollDurationPermissions
  let ?viewPayrollAmountPermissions = viewPayrollAmountPermissions'
      ?viewPayrollDurationPermissions = viewPayrollDurationPermissions'
  case modelE of
    Nothing -> error $ "Can't load Timesheet with id #" ++ show key
    Just (ts, shifts, items) -> do
      let tsOId = modelToTimesheetOpId ts shifts items
          ts' = map (\(op, _) -> maybe (tshow op) operatorNickname (lookup op operatorMap)) tsOId
          -- reports' = [ ("Timesheet" :: Text, displayShifts)
          --           , ("By Employees", displayShifts . (TS.groupShiftsBy id))
          --           , ("By Week", displayShifts . (TS.groupShiftsBy (\(e,_,_) -> e)))
          --           , ("By Day" , displayShifts . ( TS.groupShiftsBy (\(a,b,h) -> (h,b,a))))
          --           ]
          dacsReport = ("Deductions and Costs" :: Text, displayShifts displayDAC . TS._deductionAndCosts, TS.filterTimesheet (const False) isDACUnlocked )
          summaryReport = ("Summary", displayEmployeeSummary , TS.filterTimesheet isShiftViewable isDACUnlocked)
          calendar = ("Calendar", displayTimesheetCalendar, TS.filterTimesheet isShiftDurationUnlocked (const False) )
          -- reports = [(name, report  . TS._shifts, TS.filterTimesheet isShiftViewable (const False) ) | (name, report)  <- reports']
          reports = [calendar, dacsReport, summaryReport]
      defaultLayout $ [whamlet|
          $forall (name, trans, filter') <- reports
             $maybe object <- filter' ts'
              <div.panel.panel-info>
                <div.panel-heading> #{name}
                <div.panel-body>
                  <div>^{trans object }
          $if (timesheetStatus (entityVal ts) /= Process)
            <form role=form method=post action=@{GLR $ GLPayrollToFAR key}>
              <button type="submit" .btn.btn-danger>Save To FrontAccounting
            <form role=form method=post action=@{GLR $ GLPayrollRejectR key}>
              <button type="submit" .btn.btn-warning>Reject 
            <form role=form method=post action=@{GLR $ GLPayrollToPayrooR key}>
              <button type="submit" .btn.btn-info>Download Payroo 
                              |]

getGLPayrollEditR :: Int64 -> Handler Html
getGLPayrollEditR key = return "todo"
postGLPayrollEditR :: Int64 -> Handler Html
postGLPayrollEditR key = return "todo"

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
            runDB $ deleteCascade key 
            renderMain Validate Nothing ok200 (setSuccess $ toHtml $ "Timesheet #" <> tshow tId <> " has been deleted sucessfully" ) (return ())
            
      -- _  -> renderMain Validate Nothing preconditionFailed412 (setError $ toHtml $ "Timesheet #" <> tshow tId <> " is not pending.") (return ())

  

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
            respondSource "text/csv" (source =$= mapC toFlushBuilder)
        Left e -> error $ "Problem generating payroo csv: " <> unpack e


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
postTimesheetToFA :: TimesheetId
                  -> Entity Timesheet
                  -> [Entity PayrollShift]
                  -> [Entity PayrollItem]
                  -> HandlerT App IO (Either Text Int)
postTimesheetToFA key timesheet shifts items = do
      settings <- appSettings <$> getYesod
      today <- todayH
      let tsOId = modelToTimesheetOpId timesheet shifts items
      runExceptT $ do
         ts <- ExceptT $ timesheetOpIdToO'SH tsOId
         let tsSkus = ( \(op,setting, shiftKey) -> ( TS.Sku . unpack . faSKU $  setting
                                                 , shiftKey
                                                 )
                      ) <$> ts
             tsPayment =  (\(Entity _ op, settings, shiftKey) -> operatorNickname op) <$> ts
         grnIds <- saveGRNs settings key tsSkus
         invoiceId <- saveInvoice settings ts grnIds
         paymentIds <- savePayments settings key tsPayment invoiceId
         credits <- saveExternalPayments settings key invoiceId today ts
         let invoiceId = 2044
         return invoiceId
