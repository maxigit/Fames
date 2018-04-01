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



-- * Types
data Mode = Validate | Save deriving (Eq, Read, Show)
data UploadParam = UploadParam {
  upTimesheet :: Textarea
  } deriving (Eq, Read, Show)

-- * Form
uploadForm :: Mode -> Maybe UploadParam -> _Markup -> _ (FormResult UploadParam, Widget)
uploadForm mode paramM = let
  form _ = UploadParam <$> areq textareaField (disableOnSave "Timesheet") (upTimesheet <$> paramM)
  disableOnSave fsettings = case mode of
    Validate -> fsettings
    Save -> fsettings {fsAttrs = [("readonly", "")]}
  in renderBootstrap3 BootstrapBasicForm . form $ mode
-- * Handlers
-- ** upload and show timesheets
getGLPayrollR :: Handler Html
getGLPayrollR = do
  pendingW <-displayPendingSheets
  renderMain Validate Nothing ok200 (setInfo "Enter a timesheet") pendingW
postGLPayrollValidateR :: Handler Html
postGLPayrollValidateR = do
  actionM <- lookupPostParam "action"
  case actionM of
   Just "quickadd" -> processTimesheet Validate quickadd
   _ -> processTimesheet Validate validate
  where validate param key = do
          settings <- appSettings <$> getYesod
          timesheetE <- parseTimesheetH param
          case validateTimesheet (appPayroll settings) =<< timesheetE of
            Left e -> setError (toHtml e) >> renderMain Validate (Just param) badRequest400 (setInfo "Enter a timesheet") (return ())
            Right timesheet -> do
                  let ref = invoiceRef (appPayroll settings) timesheet :: String
                  (documentKey'msgM) <- runDB $ loadAndCheckDocumentKey key
                  forM documentKey'msgM  $ \(Entity _ doc, msg) -> do
                                 setWarning msg >> return ""
                  renderMain Save (Just param) ok200 (setInfo . toHtml $ "Timesheet " <> ref  <> " is valid.") (do
                        displayTimesheet timesheet
                        displayEmployeeSummary (timesheetPayrooForSummary timesheet)
                        displayTimesheetCalendar (timesheetPayrooForSummary timesheet)
                        )

postGLPayrollSaveR = do
  actionM <- lookupPostParam "action"
  case actionM of
   Just "quickadd" -> processTimesheet Validate saveQuickadd
   _ -> processTimesheet Save save
   where
      save param key = do
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
          setSuccess (toHtml $ tshow e)

          renderMain Validate Nothing created201 (setInfo "Timesheet saved to Front Account sucessfully") (return ())

-- ** Individual timesheet
getGLPayrollViewR :: Int64 -> Handler Html
getGLPayrollViewR key = do
  operatorMap <- allOperators
  modelE <- loadTimesheet key
  case modelE of
    Nothing -> error $ "Can't load Timesheet with id #" ++ show key
    Just (ts, shifts, items) -> do
      let tsOId = modelToTimesheetOpId ts shifts items
          ts' = map (\(op, _) -> maybe (tshow op) operatorNickname (lookup op operatorMap)) tsOId
          shifts' = TS._shifts ts'
          reports' = [ ("Timesheet" :: Text, displayShifts)
                    , ("By Employees", displayShifts . (TS.groupShiftsBy id))
                    , ("By Week", displayShifts . (TS.groupShiftsBy (\(e,_,_) -> e)))
                    , ("By Day" , displayShifts . ( TS.groupShiftsBy (\(a,b,h) -> (h,b,a))))
                    ]
          dacs =  TS._deductionAndCosts ts'
          dacsReport = ("Deductions and Costs", displayShifts dacs)
          summaryReport = ("Summary", displayEmployeeSummary ts')
          calendar = ("Calendar", displayTimesheetCalendar ts')
          reports = [(name, report shifts') | (name, report) <- reports']
            ++ [dacsReport, summaryReport, calendar]
      defaultLayout $ [whamlet|
          $forall (name, trans) <- reports
             <div.panel.panel-info>
               <div.panel-heading> #{name}
               <div.panel-body>
                <div>^{trans}
          $if (timesheetStatus (entityVal ts) /= Process)
            <form role=form method=post action=@{GLR $ GLPayrollToFAR key}>
              <button type="submit" .btn.btn-danger>Save To FrontAccounting
                              |]

getGLPayrollEditR :: Int64 -> Handler Html
getGLPayrollEditR key = return "todo"
postGLPayrollEditR :: Int64 -> Handler Html
postGLPayrollEditR key = return "todo"

postGLPayrollRejectR :: Int64 -> Handler Html
postGLPayrollRejectR key = return "todo"

-- ** Quick Add
quickadd :: UploadParam -> DocumentHash -> Handler Html
quickadd  param key = do
  wE <- saveQuickAdd False (unTextarea $ upTimesheet param) key
  case wE of
    Left e -> setError (toHtml e) >> renderMain Validate (Just param) badRequest400 (setInfo "Enter a valid timesheet") (return ())
    Right w -> renderMain Save (Just param) ok200 (return()) w
    -- Right w -> defaultLayout [whamlet|
    --    <form #quick-form role=form method=post action=@{GLR GLPayrollSaveR }> 
    --      <input type=hidden name=quickadd value="#{unTextarea $ upTimesheet param}">
    --      ^{w}
    --      <button type="submit" name="action" value="quickadd" class="btn btn-danger">Quick Add
saveQuickadd :: UploadParam -> DocumentHash -> Handler Html
saveQuickadd  param key = do
  wE <- saveQuickAdd True (unTextarea $ upTimesheet param) key
  case wE of
    Left e -> setError (toHtml e) >> renderMain Validate (Just param) badRequest400 (setInfo "Enter a valid timesheet") (return ())
    Right w -> do
      let msg = setSuccess "Quickadds processed properly"
      renderMain Save (Just param) created201 msg w

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
      today <- utctDay <$> liftIO getCurrentTime
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
