module Handler.GL.Payroll
( getGLPayrollR
, postGLPayrollValidateR
, postGLPayrollSaveR
, getGLPayrollViewR
, getGLPayrollEditR
, postGLPayrollEditR
, postGLPayrollRejectR
) where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput, bootstrapSubmit,BootstrapSubmit(..))
import qualified GL.Payroll.Timesheet as TS
import qualified GL.Payroll.Report as TS
import GL.Payroll.Parser
import GL.Payroll.Settings
import Data.Text (strip)
import Database.Persist.MySQL

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
postGLPayrollValidateR = processTimesheet Validate go
  where go param key = do
          timesheetE <- parseTimesheetH param
          case timesheetE of
            Left e -> setError (toHtml e) >> renderMain Validate (Just param) badRequest400 (setInfo "Enter a timesheet") (return ())
            Right timesheet -> do
                  (documentKey'msgM) <- runDB $ loadAndCheckDocumentKey key
                  forM documentKey'msgM  $ \(Entity _ doc, msg) -> do
                                 setWarning msg >> return ""
                  let report = TS.display $ TS._shifts timesheet
                      reportLines = lines $ pack report  :: [Text]
                  renderMain Save (Just param) ok200 (setInfo "Enter a timesheet")
                             [whamlet|<div.well>
                                         $forall line <- reportLines
                                           <p> #{line}
                                     |]

postGLPayrollSaveR = processTimesheet Save go where
  go param key = do
       timesheetE <- parseTimesheetH param
       case timesheetE of
         Left e -> setError (toHtml e) 
                   >> renderMain Validate (Just param) badRequest400 (setInfo "Enter a timesheet") (return ())
         Right timesheet -> do
              saveTimeSheet key "todo" timesheet
              renderMain Validate Nothing created201 (setInfo "Timesheet saved sucessfully") (return ())


-- ** Individual timesheet
getGLPayrollViewR :: Int64 -> Handler Html
getGLPayrollViewR key = return "todo"

getGLPayrollEditR :: Int64 -> Handler Html
getGLPayrollEditR key = return "todo"
postGLPayrollEditR :: Int64 -> Handler Html
postGLPayrollEditR key = return "todo"

postGLPayrollRejectR :: Int64 -> Handler Html
postGLPayrollRejectR key = return "todo"

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
     
parseTimesheet :: [Text] -> Textarea -> (Either String TS.Timesheet)
parseTimesheet header0 ts = parseFastTimesheet strings where
  header = map unpack header0
  strings' = map (unpack . strip) . lines $ unTextarea $ ts
  -- insert the header after the first line which should be the period definition
  strings = case strings' of
    (x:xs) -> x : header ++ xs
    [] -> header

parseTimesheetH :: UploadParam -> Handler (Either String TS.Timesheet)
parseTimesheetH param = do
  header <- headerFromSettings
  return $ parseTimesheet header (upTimesheet param)

-- * Rendering
displayPendingSheets :: Handler Widget
displayPendingSheets = do
  timesheets <- runDB $ selectList [TimesheetStatus ==. Pending] []
  return [whamlet|
     <div.panel.panel-info>
       <div.panel-heading> Pending Timesheets
       ^{displayTimesheetList timesheets}
          |]
displayTimesheetList :: [Entity Timesheet] -> Widget
displayTimesheetList timesheets = [whamlet|
<table.table.table-hover.table-striped>
  <tr>
    <th> Id
    <th> Reference
    <th> Period Type
    <th> Start
    <th> End
    <th> Status
  $forall (Entity key ts) <- timesheets
    <tr>
      <td> <a href="@{GLR (GLPayrollViewR (unSqlBackendKey $ unTimesheetKey key))}">
            ##{tshow $ unSqlBackendKey $ unTimesheetKey key}
      <td> #{timesheetReference ts}
      <td> #{timesheetFrequency ts}
      <td> #{tshow $ timesheetStart ts}
      <td> #{tshow $ timesheetEnd ts}
      <td> #{tshow $ timesheetStatus ts}
|]
-- * DB access
-- | Save a timesheet.
saveTimeSheet :: DocumentHash -> Text -> TS.Timesheet -> Handler TimesheetId
saveTimeSheet key ref timesheet = do
  opFinder <- operatorFinder
  let (model, shiftsFn) =  timesheetToModel opFinder timesheet
  runDB $ do
    docKey <- createDocumentKey (DocumentType "timesheet") key ref ""

    modelId <- insert (model docKey)
    insertMany_ (shiftsFn modelId)
    return modelId

-- * Converter
-- | Convert a Payroll Timesheet to it's DB model.
-- It doesn't return a list of Shift but a function creating them
-- as we are missing the Timesheet id.
timesheetToModel :: _ -> TS.Timesheet -> (_ -> Timesheet, TimesheetId -> [Shift])
timesheetToModel opFinder ts = (model, shiftsFn) where
  start = TS._weekStart ts
  model dockKey = Timesheet "todo" dockKey start (TS.period start) "Weekly" Pending
  shiftsFn i = map (mkShift i) (TS._shifts ts)
  mkShift i shift= let
    (employee, day, shiftType) = TS._shiftKey shift
    Entity operatorKey _ = fromMaybe (error $ "Operator " ++ TS._nickName employee ++ " not found")
                            (opFinder  (pack $ TS._nickName employee))
    in Shift i
                    (TS._duration shift)
                    (TS._cost shift)
                    operatorKey
                    (Just day)
                    (tshow shiftType)

-- * Configuration

-- | A returns a list of employees from the payroll settings
-- completed with the operator information (from table)
-- joined by nickname
-- Only employees present in both list are returned
getEmployeeInfo:: Handler [(Entity Operator, EmployeeSettings)]
getEmployeeInfo = do
  opFinder <- operatorFinder
  payrollSettings <- getsYesod (appPayroll . appSettings)
  return   [(op, emp) | (nickname, emp) <- mapToList (employees payrollSettings)
                      , Just op <- return $ opFinder nickname
                      ]

-- | Employees description to insert at the beginnig of the timesheet
headerFromSettings :: Handler [Text]
headerFromSettings = do
  infos <- getEmployeeInfo
  return $ map employeeDescription infos
  
-- | Line describing an employee in a timesheet format
employeeDescription :: (Entity Operator, EmployeeSettings) -> Text
employeeDescription (Entity _ op, emp) = intercalate " "
  [ operatorNickname op 
  , operatorFirstname op
  , operatorSurname op
  , timesheet emp
  ]


