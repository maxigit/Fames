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
import Data.Text (strip)

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
getGLPayrollR = renderMain Validate Nothing ok200 (setInfo "Enter a timesheet") (return ())
postGLPayrollValidateR :: Handler Html
postGLPayrollValidateR = processTimesheet Validate go
  where go param = do
          case parseTimesheet (upTimesheet param) of
            Left e -> setError (toHtml e) >> renderMain Validate (Just param) badRequest400 (setInfo "Enter a timesheet") (return ())
            Right timesheet -> do
                  let report = TS.display $ TS._shifts timesheet
                  renderMain Save (Just param) ok200 (setInfo "Enter a timesheet")
                             [whamlet|<div.well>
                                         <p>#{report}|]

postGLPayrollSaveR = processTimesheet Save go where
  go param = do
     case parseTimesheet (upTimesheet param) of
         Left e -> setError (toHtml e)
                   >> renderMain Validate (Just param) badRequest400 (setInfo "Enter a timesheet") (return ())
         Right timesheet -> do
              saveTimeSheet timesheet
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
processTimesheet :: Mode -> (UploadParam -> Handler r) -> Handler r
processTimesheet mode post = do
  ((resp, formW), enctype) <- runFormPost (uploadForm mode Nothing)
  case resp of 
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show (mode, a)
    FormSuccess param -> post param
     
parseTimesheet :: Textarea -> (Either String TS.Timesheet)
parseTimesheet ts = parseFastTimesheet strings where
  strings = map (unpack . strip) . lines $ unTextarea $ traceShowId ts


-- * DB access
saveTimeSheet :: TS.Timesheet -> Handler TimesheetId
saveTimeSheet timesheet = do
  let (model, shiftsFn) =  timesheetToModel timesheet
  runDB $ do
    modelId <- insert model
    insertMany_ (shiftsFn modelId)
    return modelId

-- * Converter
-- | Convert a Payroll Timesheet to it's DB model.
-- It doesn't return a list of Shift but a function creating them
-- as we are missing the Timesheet id.
timesheetToModel :: TS.Timesheet -> (Timesheet, TimesheetId -> [Shift])
timesheetToModel ts = (model, shiftsFn) where
  start = TS._weekStart ts
  model = Timesheet "todo" start (TS.period start) "weekly"
  shiftsFn i = map (mkShift i) (TS._shifts ts)
  mkShift i shift= Shift i
                    (TS._duration shift)
                    (TS._cost shift)
                    (OperatorKey 1)
                    (Just day)
                    (tshow shiftType)
             where (employee, day, shiftType) = TS._shiftKey shift
    

