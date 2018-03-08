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
) where
-- * Import
import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput, bootstrapSubmit,BootstrapSubmit(..))
import qualified GL.Payroll.Timesheet as TS
import qualified GL.Payroll.Report as TS
import GL.Payroll.Parser
import GL.Payroll.Settings
import Data.Text (strip)
import Database.Persist.MySQL
import Data.Time.Calendar
import Lens.Micro.Extras (view)

import  qualified WH.FA.Types as WFA
import  qualified WH.FA.Curl as WFA
import Control.Monad.Except
import Text.Printf(printf)


-- * Types
data Mode = Validate | Save deriving (Eq, Read, Show)
data UploadParam = UploadParam {
  upTimesheet :: Textarea
  } deriving (Eq, Read, Show)

-- ** Orphan Instances
instance TS.Display Text where
  display = unpack

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
                  renderMain Save (Just param) ok200 (setInfo "Enter a timesheet") (displayTimesheet timesheet)

postGLPayrollSaveR = processTimesheet Save go where
  go param key = do
       timesheetE <- parseTimesheetH param
       case timesheetE of
         Left e -> setError (toHtml e) 
                   >> renderMain Validate (Just param) badRequest400 (setInfo "Enter a timesheet") (return ())
         Right timesheet -> do
              tKey <- saveTimeSheet key timesheet
              let tId = unSqlBackendKey (unTimesheetKey tKey)
              pushLinks ("View Timesheet #" <> tshow tId)
                        (GLR $ GLPayrollViewR tId)
                        []
              renderMain Validate Nothing created201 (setInfo "Timesheet saved sucessfully") (return ())


postGLPayrollToFAR :: Int64 -> Handler Html
postGLPayrollToFAR key = do
  setting <- appSettings <$> getYesod
  modelE <- loadTimesheet key
  case modelE of
    Nothing -> error $ "Can't load Timesheet with id #" ++ show key
    Just (ts, shifts, _) -> do
      r <- postTimesheetToFA ts shifts
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
    Just (ts, shifts, _) -> do
      let tsOId = modelToTimesheetOpId ts shifts
          ts' = map (\op -> maybe (tshow op) operatorNickname (lookup op operatorMap)) tsOId
          shifts' = TS._shifts ts'
          reports = [ ("Timesheet" :: Text, displayShifts)
                    , ("By Employees", displayShifts . (TS.groupShiftsBy id))
                    , ("By Week", displayShifts . (TS.groupShiftsBy (\(e,_,_) -> e)))
                    , ("ByDay" , displayShifts . ( TS.groupShiftsBy (\(a,b,h) -> (h,b,a))))
                    ] 
      defaultLayout $ [whamlet|
          $forall (name, trans) <- reports
             <div.panel.panel-info>
               <div.panel-heading> #{name}
               <div.panel-body> ^{trans shifts'}
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
     
parseTimesheet :: [Text] -> Textarea -> (Either String (TS.Timesheet TS.PayrooEmployee))
parseTimesheet header0 ts = parseFastTimesheet strings where
  header = map unpack header0
  strings' = map (unpack . strip) . lines $ unTextarea $ ts
  -- insert the header after the first line which should be the period definition
  strings = case strings' of
    (x:xs) -> x : header ++ xs
    [] -> header

parseTimesheetH :: UploadParam -> Handler (Either String (TS.Timesheet TS.PayrooEmployee))
parseTimesheetH param = do
  header <- headerFromSettings
  return $ parseTimesheet header (upTimesheet param)

-- * To Front Accounting
postTimesheetToFA timesheet shifts = do
      settings <- appSettings <$> getYesod
      let tsOId = modelToTimesheetOpId timesheet shifts
      runExceptT $ do
         ts <- ExceptT $ timesheetOpIdToO'SH tsOId
      -- Right ts <- timesheetOpIdToO'SH tsOId
         let tsSkus = TS.Sku . unpack . faSKU . snd <$> ts
         grnIds <- saveGRNs settings tsSkus
         traceShowM ("GRN",grnIds)
         return grnIds
         saveInvoice settings tsSkus grnIds

saveGRNs :: AppSettings -> TS.Timesheet TS.Sku -> ExceptT Text Handler [(Int, Maybe Int)]
saveGRNs settings timesheet = do
  let connectInfo = WFA.FAConnectInfo (appFAURL settings) (appFAUser settings) (appFAPassword settings)
      psettings = appPayroll settings
      textcarts = concat [TS.textcarts typ timesheet | typ <- [minBound..maxBound]]
      mkDetail shift = WFA.GRNDetail (pack . TS.sku $ TS._shiftKey shift)
                                     (TS._duration shift)
                                     (view TS.hourlyRate shift)
      mkGRN (TS.Textcart (day, shiftType, shifts))  = let
        location = (case shiftType of
                      TS.Holiday -> grnHolidayLocation 
                      TS.Work -> grnWorkLocation
                   ) psettings
        ref = Just $ tshow day <> "-" <> (tshow shiftType)
        in WFA.GRN (grnSupplier psettings)
                   day
                   ref
                   Nothing
                   location
                   Nothing -- delivery
                   ""
                   (map mkDetail shifts)
      grns = map mkGRN textcarts
  ids <- mapM (\grn -> ExceptT . liftIO $ WFA.postGRN connectInfo grn) grns
  return $ zip ids [Just $ length (WFA.grnDetails grn) | grn <- grns]

saveInvoice settings timesheet deliveries = do
  today <- utctDay <$> liftIO getCurrentTime
  let connectInfo = WFA.FAConnectInfo (appFAURL settings) (appFAUser settings) (appFAPassword settings)
      psettings = appPayroll settings
      invoice = WFA.PurchaseInvoice  (grnSupplier psettings)
                                     Nothing -- reference
                                     (tshow today)
                                     today
                                     (addDays 10 today)
                                     "Todo"
                                     deliveries
                                     []
  ExceptT $ liftIO $ WFA.postPurchaseInvoice connectInfo invoice



  
  -- group text cart by date and type

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
      <td> #{tshow $ timesheetFrequency ts}
      <td> #{tshow $ timesheetStart ts}
      <td> #{tshow $ timesheetEnd ts}
      <td> #{tshow $ timesheetStatus ts}
|]

displayTimesheet :: (TS.Timesheet TS.PayrooEmployee) -> Widget
displayTimesheet timesheet = displayShifts $ TS._shifts timesheet
displayShifts shifts = let
  report = TS.display shifts
  reportLines = lines $ pack report  :: [Text]
  in [whamlet|<div.well>
        $forall line <- reportLines
          <p> #{line}

     |]
-- * DB access
-- | Save a timesheet.
saveTimeSheet :: DocumentHash -> (TS.Timesheet TS.PayrooEmployee) -> Handler TimesheetId
saveTimeSheet key timesheet = do
  opFinder <- operatorFinderWithError
  payrollSettings <- getsYesod (appPayroll . appSettings)
  let period' = period payrollSettings
      ref = pack $ timesheetRef period' timesheet
  case timesheetOpIdToModel period' <$> timesheetEmployeeToOpId opFinder timesheet of
      Left e ->  do
        setError (toHtml e)
        invalidArgs ["Some Operators doesn't exists in the database or in the configuration file"]
        error "Should't go there"
      Right (model, shiftsFn) -> runDB $ do
          docKey <- createDocumentKey (DocumentType "timesheet") key ref ""
          modelId <- insert (model docKey)
          insertMany_ (shiftsFn modelId)
          return modelId

loadTimesheet :: Int64 -> Handler (Maybe (Entity Timesheet, [Entity Shift], [Entity PayrollItem]))
loadTimesheet key64 = do
  let key = TimesheetKey (SqlBackendKey key64)
  runDB $ do
    tsM <- get key
    case tsM of
      Nothing -> return Nothing
      Just ts -> do
        shifts <- selectList [ShiftTimesheet ==. key] []
        items <- selectList [PayrollItemTimesheet ==. key] []
        return $ Just (Entity key ts, shifts, items)

-- * Converter
-- | Convert a Payroll Timesheet to it's DB model.
-- It doesn't return a list of Shift but a function creating them
-- as we are missing the Timesheet id.
-- ** Timesheet conversion
timesheetOpIdToModel :: (PayrollFrequency -> Day -> (Integer, Int, String, Day))
                 -> (TS.Timesheet OperatorId)
                 -> (_ -> Timesheet, TimesheetId -> [Shift])
timesheetOpIdToModel period' ts = (model, shiftsFn) where
  start = TS._periodStart ts
  (y, _, s, end) = period' (TS._frequency ts )start
  ref = show y ++ "-" ++ s
  model dockKey = Timesheet (pack ref) dockKey start end (TS._frequency ts) Pending
  shiftsFn i = map (mkShift i) (TS._shifts ts)
  mkShift i shift= let
    (operator, day, shiftType) = TS._shiftKey shift
    in Shift i
                    (TS._duration shift)
                    (TS._cost shift)
                    operator
                    (Just day)
                    (tshow shiftType)

modelToTimesheetOpId :: Entity Timesheet -> [Entity Shift] -> (TS.Timesheet OperatorId)
modelToTimesheetOpId (Entity _ timesheet) shiftEs = let
  readType "Holiday" = TS.Holiday
  readType _ = TS.Work
  mkShift (Entity _ shift) = TS.Shift  (mkShiftKey shift) Nothing (shiftDuration shift) (shiftCost shift)
  mkShiftKey shift = ( shiftOperator shift
                     , fromMaybe (error "TODO") (shiftDate shift)
                     , readType (shiftType shift)
                     )
  in TS.Timesheet (map mkShift shiftEs) (timesheetStart timesheet) (timesheetFrequency timesheet)

timesheetEmployeeToOpId :: (Monad m , TS.HasEmployee e)
                      => (Text -> m (Entity Operator))
                      -> TS.Timesheet e
                      -> m (TS.Timesheet OperatorId)
timesheetEmployeeToOpId opFinder ts = 
  entityKey <$$> traverse (opFinder . pack . view TS.nickName) ts

timesheetOpIdToOp opFinder ts = 
  entityVal <$$> traverse (opFinder . pack . view TS.nickName) ts

timesheetOpIdToO'S :: [(Entity Operator, EmployeeSettings)]
                        -> TS.Timesheet OperatorId
                        -> Either Text (TS.Timesheet (Entity Operator, EmployeeSettings ))
timesheetOpIdToO'S employeeInfos ts = let
  m :: Map OperatorId (Entity Operator, EmployeeSettings)
  m = mapFromList [(entityKey oe, oe'emp) | oe'emp@(oe, _) <- employeeInfos ]
  findInfo :: OperatorId -> Either Text (Entity Operator, EmployeeSettings)
  findInfo key = maybe (Left $ "Can't find settings or operator info for operator #" <> tshow key )
                       Right
                       (lookup key m)
  in traverse findInfo ts
  

timesheetOpIdToO'SH :: TS.Timesheet OperatorId
                   -> Handler (Either Text (TS.Timesheet (Entity Operator, EmployeeSettings)))
timesheetOpIdToO'SH ts = do
  employeeInfos <- getEmployeeInfo
  return $ timesheetOpIdToO'S employeeInfos ts
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


period :: PayrollSettings -> PayrollFrequency -> Day -> (Integer, Int, String, Day)
period settings TS.Weekly day = let
  (y, w) = TS.weekNumber (firstTaxWeek settings) day
  w' = printf "W%02d" w
  in (y, w, w', addDays 6 day )
period settings TS.Monthly day = let
  (y, m) = TS.monthNumber (firstTaxMonth settings) day
  m' = printf "M%02d" m
  in (y, m, m', addDays (-1) (addGregorianMonthsClip 1 day ))

timesheetRef period' ts = let
  (year, periodN, periodS, start) = period' (TS._frequency ts) (TS._periodStart ts)
  in show year  <> periodS
