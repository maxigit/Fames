{-# LANGUAGE DuplicateRecordFields #-}
module Handler.GL.Payroll.Common
where

-- * Import
import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput, bootstrapSubmit,BootstrapSubmit(..))
import Handler.Table
import qualified GL.Payroll.Timesheet as TS
import qualified GL.Payroll.Report as TS
import GL.Payroll.Parser
import GL.Payroll.Settings
import GL.Utils
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
import Data.List(iterate, cycle)

-- ** Orphan Instances
instance TS.Display Text where
  display = unpack

-- * DB access
-- | Save a timesheet.
saveTimeSheet :: DocumentHash -> (TS.Timesheet String TS.PayrooEmployee) -> Handler (TimesheetId, Text)
saveTimeSheet key timesheet = do
  opFinder <- operatorFinderWithError
  payrollSettings <- getsYesod (appPayroll . appSettings)
  let period' = timesheetPeriod payrollSettings timesheet
      ref = pack $ timesheetRef period' timesheet
  case timesheetOpIdToModel ref <$> timesheetEmployeeToOpId opFinder timesheet of
      Left e ->  do
        setError (toHtml e)
        invalidArgs ["Some Operators doesn't exists in the database or in the configuration file"]
        error "Should't go there"
      Right (model, shiftsFn, itemsFn) -> runDB $ do
          saveTimeSheetModel key ref (model, shiftsFn, itemsFn)

-- saveTimeSheetModel :: DocumentHash -> (TS.Timesheet String TS.PayrooEmployee) -> Handler (TimesheetId, Text)
saveTimeSheetModel :: DocumentHash
                   -> Text
                   -> ( Key DocumentKey -> Timesheet
                      , Key Timesheet -> [PayrollShift],
                       Key Timesheet -> [PayrollItem])
                   -> ReaderT SqlBackend Handler (Key Timesheet, Text)
saveTimeSheetModel key ref (model, shiftsFn, itemsFn) = do
          docKey <- createDocumentKey (DocumentType "timesheet") key ref ""
          modelId <- insert (model docKey)
          insertMany_ (shiftsFn modelId)
          insertMany_ (itemsFn modelId)
          return (modelId, ref)

loadTimesheet :: Int64 -> Handler (Maybe (Entity Timesheet, [Entity PayrollShift], [Entity PayrollItem]))
loadTimesheet key64 = do
  let key = TimesheetKey (SqlBackendKey key64)
  es <- loadTimesheets [TimesheetId ==. key]
  return $ listToMaybe es

loadTimesheets :: [Filter Timesheet] -> Handler [ (Entity Timesheet, [Entity PayrollShift], [Entity PayrollItem])]
loadTimesheets criteria = do
  runDB $ do
    tss <- selectList criteria []
    forM tss $ \ts -> do
        let key = entityKey ts
        shifts <- selectList [PayrollShiftTimesheet ==. key] []
        items <- selectList [PayrollItemTimesheet ==. key] []
        return (ts, shifts, items)

-- * Converter
-- | Convert a Payroll Timesheet to it's DB model.
-- It doesn't return a list of Shift but a function creating them
-- as we are missing the Timesheet id.
-- ** Timesheet conversion
timesheetOpIdToModel :: Text
                 -> (TS.Timesheet String OperatorId)
                 -> ( Key DocumentKey -> Timesheet
                    , TimesheetId -> [PayrollShift]
                    , TimesheetId -> [PayrollItem]
                    )
timesheetOpIdToModel ref ts = (model, shiftsFn, itemsFn) where
  start = TS._periodStart ts
  model dockKey = Timesheet ref dockKey start (TS.periodEnd ts) (TS._frequency ts) Pending
  shiftsFn i = map (mkShift i) (TS._shifts ts)
  mkShift i shift= let
    (operator, day, shiftType) = TS._shiftKey shift
    in PayrollShift i
                    (TS._duration shift)
                    (TS._cost shift)
                    operator
                    (Just day)
                    (tshow shiftType)
  itemsFn i = concatMap (mkItem i) (TS._deductionAndCosts ts)
  mkItem :: TimesheetId -> TS.DeductionAndCost (String, OperatorId) -> [PayrollItem]
  mkItem i dac = mergeTheseWith (mkDAC Deduction) (mkDAC Cost) (<>) (TS._dacDac dac) where
                  (payee, op) = TS._dacKey dac
                  mkDAC dacType amount = [ PayrollItem i amount dacType op (pack payee) "" ]

modelToTimesheetOpId :: Entity Timesheet
                     -> [Entity PayrollShift]
                     -> [Entity PayrollItem]
                     -> (TS.Timesheet Text (OperatorId, PayrollShiftId))
modelToTimesheetOpId (Entity _ timesheet) shiftEs itemEs = let
  readType "Holiday" = TS.Holiday
  readType _ = TS.Work
  mkShift (Entity key shift) = TS.Shift  (mkShiftKey key shift) Nothing (payrollShiftDuration shift) (payrollShiftCost shift)
  mkShiftKey key shift = ( (payrollShiftOperator shift, key)

                     , fromMaybe (error "TODO") (payrollShiftDate shift)
                     , readType (payrollShiftType shift)
                     )
  mkItems = (TS.groupDacsBy id) . (map mkItem)
  mkItem (Entity _ i) = TS.DeductionAndCost (payrollItemPayee i, (payrollItemOperator i, PayrollShiftKey (SqlBackendKey 1)))
                                 ((case payrollItemType  i of
                                     Cost -> That
                                     Deduction -> This)
                                   (payrollItemAmount i)
                                 )
  in TS.Timesheet (map mkShift shiftEs) (timesheetStart timesheet) (timesheetFrequency timesheet) (mkItems itemEs)

timesheetEmployeeToOpId :: (Monad m , TS.HasEmployee e)
                      => (Text -> m (Entity Operator))
                      -> TS.Timesheet p e
                      -> m (TS.Timesheet p OperatorId)
timesheetEmployeeToOpId opFinder ts =
  entityKey <$$> traverse (opFinder . pack . view TS.nickName) ts

timesheetOpIdToO'S :: [(Entity Operator, EmployeeSettings)]
                   -> Map Text PayrollExternalSettings
                   -> TS.Timesheet Text (OperatorId, PayrollShiftId)
                   -> Either Text ( TS.Timesheet (Text, PayrollExternalSettings)
                                                 ( Entity Operator
                                                 , EmployeeSettings
                                                 , PayrollShiftId))
timesheetOpIdToO'S employeeInfos payeeMap ts = let
  m :: Map OperatorId (Entity Operator, EmployeeSettings)
  m = mapFromList [(entityKey oe, oe'emp) | oe'emp@(oe, _) <- employeeInfos ]
  findInfo :: (OperatorId, PayrollShiftId) -> Either Text (Entity Operator, EmployeeSettings, PayrollShiftId)
  findInfo (opKey, shiftKey  ) = maybe (Left $ "Can't find settings or operator info for operator #" <> tshow opKey )
                                 (\(o,e) -> Right (o, e, shiftKey))
                                 (lookup opKey m)
  findPayee :: Text -> Either Text (Text, PayrollExternalSettings)
  findPayee payee =  maybe (Left $ "Can't find settings for payee '" <> payee <>"''")
                           (Right . (payee,))
                           (lookup payee payeeMap)
  -- in TS.traversePayee findPayee <$> traverse findInfo ts
  in traverse findInfo ts >>= TS.traversePayee findPayee

timesheetOpIdToOSettings :: [(Entity Operator, EmployeeSettings)]
                   -> TS.Timesheet p (OperatorId, PayrollShiftId)
                   -> Either Text ( TS.Timesheet p
                                                 ( Entity Operator
                                                 , EmployeeSettings
                                                 , PayrollShiftId))
timesheetOpIdToOSettings employeeInfos ts = let
  m :: Map OperatorId (Entity Operator, EmployeeSettings)
  m = mapFromList [(entityKey oe, oe'emp) | oe'emp@(oe, _) <- employeeInfos ]
  findInfo :: (OperatorId, PayrollShiftId) -> Either Text (Entity Operator, EmployeeSettings, PayrollShiftId)
  findInfo (opKey, shiftKey  ) = maybe (Left $ "Can't find settings or operator info for operator #" <> tshow opKey )
                                 (\(o,e) -> Right (o, e, shiftKey))
                                 (lookup opKey m)
  in traverse findInfo ts

timesheetPayeeToPSettings :: Map Text PayrollExternalSettings
                          -> TS.Timesheet Text e
                          -> Either Text (TS.Timesheet (Text, PayrollExternalSettings) e)
timesheetPayeeToPSettings payeeMap = let
  findPayee :: Text -> Either Text (Text, PayrollExternalSettings)
  findPayee payee =  maybe (Left $ "Can't find settings for payee '" <> payee <>"''")
                           (Right . (payee,))
                           (lookup payee payeeMap)
  -- in TS.traversePayee findPayee <$> traverse findInfo ts
  in TS.traversePayee findPayee
  
timesheetOpIdToO'SH :: TS.Timesheet Text (OperatorId, PayrollShiftId)
                    -> Handler (Either Text
                                       (TS.Timesheet (Text, PayrollExternalSettings)
                                       (Entity Operator, EmployeeSettings, PayrollShiftId)
                                       )
                               )
timesheetOpIdToO'SH ts = do
  employeeInfos <- getEmployeeInfo
  payrollSettings <- getsYesod (appPayroll . appSettings)
  return $ timesheetOpIdToO'S employeeInfos (externals payrollSettings) ts

timesheetPayrooForSummary :: TS.Timesheet String TS.PayrooEmployee
                          -> TS.Timesheet Text Text
timesheetPayrooForSummary timesheet = let
  ts = (pack . TS._nickName . TS._payrooEmployee' ) <$> timesheet
  in runIdentity $ TS.traversePayee (Identity . pack) ts

timesheetOpIdToText employeeInfos ts = do
  tsO <- timesheetOpIdToOSettings employeeInfos ts
  return $ fmap (\(o,_,_) -> operatorNickname (entityVal o)) tsO

timesheetOpIdToTextH :: TS.Timesheet p (OperatorId, PayrollShiftId)
  -> HandlerT App IO (Either Text (TS.Timesheet p Text))
timesheetOpIdToTextH ts = do
  employeeInfos <- getEmployeeInfo
  return $ timesheetOpIdToText employeeInfos ts


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
  (y, w) = TS.weekNumber (TS.Start $ firstTaxWeek settings) day
  w' = printf "W%02d" w
  in (y, w, w', addDays 6 day )
period settings TS.Monthly day = let
  (y, m) = TS.monthNumber (TS.Start $ firstTaxMonth settings) day
  m' = printf "M%02d" m
  in (y, m, m', addDays (-1) (addGregorianMonthsClip 1 day ))

timesheetRef period ts = let
  start = TS._periodStart ts
  in TS.longRef period start

grnRef settings timesheet day shiftType = let
  period = timesheetPeriod settings timesheet
  periodRef =  TS.shortRef period day
  in pack $ periodRef <> "-" <> TS.dayRef period day <> "-" <> show shiftType


employeePaymentRef settings timesheet name = let
  period = timesheetPeriod settings timesheet
  day = TS._periodStart timesheet
  in pack (TS.shortRef period day) <> "/" <> name

invoiceRef settings timesheet = let
  period = timesheetPeriod settings timesheet 
  day = TS._periodStart timesheet
  in pack $ TS.longRef period day

timesheetPeriod settings timesheet = let
  frequency = TS._frequency timesheet
  day = TS._periodStart timesheet
  start = TS.Start $ case frequency of
    TS.Weekly -> firstTaxWeek settings
    TS.Monthly -> firstTaxMonth settings
  original = TS.Period frequency start
  in TS.adjustPeriodYearFor day original

-- check the timesheet date matchs the beginning of a period
validateTimesheet settings timesheet = let
  start = TS._periodStart timesheet
  valid = case TS._frequency timesheet of
    TS.Weekly -> diffDays start (firstTaxWeek settings) `mod` 7 == 0
    TS.Monthly -> let (_,_,day) = toGregorian start
                      (_,_,day') = toGregorian (firstTaxMonth settings)
                  in day == day' 
  in if valid
     then Right timesheet
     else Left "Timesheet start doesn't match  beginning of a period"

  
          
-- * Rendering
-- ** Pending Sheet
displayPendingSheets :: Handler Widget
displayPendingSheets = do
  timesheets <- runDB $ selectList [TimesheetStatus ==. Pending] []
  return [whamlet|
     <div.panel.panel-info>
       <div.panel-heading> Pending Timesheets
       ^{displayTimesheetList timesheets}
          |]

-- ** Timesheet list
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

-- ** Timesheet
displayTimesheet :: (TS.Timesheet String TS.PayrooEmployee) -> Widget
displayTimesheet timesheet = do
  displayShifts $ TS._shifts timesheet
  displayShifts $ TS._deductionAndCosts timesheet

displayShifts shifts = let
  report = TS.display shifts
  reportLines = lines $ pack report  :: [Text]
  in [whamlet|<div.well>
        $forall line <- reportLines
          <p> #{line}
     |]

-- ** Employee Summary
-- | Displays a table with all payment information for each employee t
displayEmployeeSummary :: TS.Timesheet Text Text -> Widget
displayEmployeeSummary timesheet = let
  summaries = TS.paymentSummary timesheet
  (cols, colnames) = employeeSummaryColumns summaries
  rows = employeeSummaryRows summaries
  in employeeSummaryTable cols colnames rows

-- | Display a table with all payment informaiton
-- the rows are different from the summaries. This allows
-- the header to generate different column that then one actually needed.
-- this is usefull when displaying different timesheet in different tables
-- but with all the table  having the same header.
employeeSummaryColumns summaries = let 
  toCols getter = map (Just getter,)  .  Map.keys $ mconcat $ map getter summaries
  deductions =  toCols TS._deductions
  netDeductions =  toCols TS._netDeductions
  costs =  toCols TS._costs
  hours = toCols convertHours
  convertHours s = Map.fromList [(tshow k <> "\n(hrs)", h) | (k,h)  <- Map.toList (TS._totalHours s)]
    
  -- | Columns are either straight field (Nothing)
  -- or the name of a payee in in the given dacs map (Just ...)
  columns = [(Nothing,) "Employee", (Nothing,) ("To Pay" :: Text) ]
            ++ netDeductions
            ++ [(Nothing,) "Net"]
            ++ deductions
            ++ [(Nothing,) "Gross"]
            ++ costs
            ++ [ (Nothing,) "Total Cost"] --  :: [ (Maybe (Text -> Map String TS.Amount), Text) ]
            ++ hours
  colNames (_, col) = (toHtmlWithBreak col, [])
  in (columns, colNames)

employeeSummaryTable columns colNames rows = let 
  table =  displayTableRowsAndHeader columns colNames rows 
  in [whamlet|
            <table.table.table-bordered.table-striped.table-hover>
             ^{table}
            |]
     >> toWidget [cassius|
         table tr.total
            font-weight: 700
            |]

employeeSummaryRows summaries = let 
  -- | Columns are either straight field (Nothing)
  -- or the name of a payee in in the given dacs map (Just ...)
  colFns = map mkColFn summaries
  formatDouble' x | abs x < 1e-2 = ""
                  | x < 0 = [shamlet|<span.text-danger>#{formatDouble x}|]
                  | otherwise = toHtml $ formatDouble x
          
  rows = (zip colFns (map (const []) summaries)) ++ totalRows
  -- we computes total rows as a list, as it could be empty
  totalRows = case map (\s -> s { TS._sumEmployee = ()}) summaries of
             [] -> []
             (x:xs) -> let
               total = (sconcat (x :| xs)) { TS._sumEmployee = "Total"}
               totalRow col = mkColFn total col
               in [(totalRow, ["total"])]
  mkColFn summary@TS.EmployeeSummary{..}  col = let
    value = case col of
              (Nothing, "Employee") -> Just (toHtml $ _sumEmployee, [])
              (Nothing, "Net") -> Just (formatDouble' _net, ["text-right"])
              (Nothing, "Gross") -> Just (formatDouble' _gross, ["text-right"])
              (Nothing, "To Pay") -> Just (formatDouble' _finalPayment, ["text-right"])
              (Nothing, "Total Cost") -> Just (formatDouble' _totalCost, ["text-right"])
              (Just getter, payee) -> let value =  lookup payee (getter summary)
                                    in ((, ["text-right"]).formatDouble') <$> value
              _ -> Nothing
    in value
  in rows

-- ** Calendar
-- | Display timesheet as a calendar
displayTimesheetCalendar :: (TS.Timesheet payee Text) -> Widget
displayTimesheetCalendar timesheet = do
  let periodStart = TS._periodStart timesheet
      periodEnd = TS.periodEnd timesheet
      -- get start and end of displayed calendar
      -- for weekly, it's just the week
      -- for month, we start on Sunday
      (start, end) = case TS._frequency timesheet of
        TS.Weekly -> (periodStart, periodEnd)
        TS.Monthly -> ( previousWeekDay Sunday periodStart
                   , nextWeekDay Saturday periodEnd)
  displayCalendar start end periodStart periodEnd (TS._shifts timesheet)

-- displayCalendar :: Show emp =>  Day -> Day -> Day -> Day
--                 -> Map Text (Map Day [TS.Shift emp])
--                 -> Widget
displayCalendar start end firstActive lastActive shifts = do
  let shiftMap' = TS.groupBy (^. TS.shiftKey . _1 ) shifts
      shiftMap = TS.groupBy (^. TS.day) <$> shiftMap'
      columns = Nothing : map Just [0..6]
      -- columns = map dayOfWeek (take 7  $ iterate (addDays 1) start )
      weekStarts = takeWhile (<end) (iterate  (addDays 7) start)
      colDisplay Nothing = ("Operator", [])
      colDisplay (Just index) = let weekDay = dayOfWeek (addDays index start)
                         in (toHtml $ show weekDay , case weekDay of
                               Saturday -> ["weekend"]
                               Sunday -> ["weekend"]
                               _ -> [])
      operators =  keys shiftMap
      rows = concatMap (rowsForWeek firstActive lastActive shiftMap operator'colors) weekStarts
      colors = ["green", "blue", "orange", "purple", "brown"] :: [Text]
      operator'colors = zip operators (cycle colors)
      css = [cassius|
             td.Saturday, td.Sunday
                background: #fee
             span.badge.Holiday
                color: white
                background: red
             .dayOfMonth
                text-align:right
                font-size: 1.2em
                font-style: italic
                &.weekend
                   color: red
                &.inactive
                  color: gray
                  |]
  displayTable columns colDisplay rows >> toWidget css

calendarFn shiftMap (operator,color) weekStart Nothing = Just $ ([shamlet|<span style="color:#{color}">#{operator}|], [])
calendarFn shiftMap (operator,color) weekStart (Just col) = do -- Maybe
  let day = addDays col weekStart
      maxDuration = 9 -- TODO pass as parameter
      durationWidth d =  formatDouble $ d / maxDuration * 100
      bg shift = case TS._shiftKey shift of
        TS.Work -> "background:" <> color
        TS.Holiday -> ""

  dateMap <- lookup operator shiftMap
  shifts <- lookup day dateMap
  let types = TS.groupShiftsBy (^. TS.shiftType ) shifts
      html = [shamlet|
    $forall shift <-  types
      $with duration <- TS._duration shift
        <span.badge class=#{show $ TS._shiftKey shift} style="width:#{durationWidth duration}%;#{bg shift}">
          #{duration}
                 |]
  return (html, [])

-- Display a week. The first line is the day of month
-- then each operator
rowsForWeek firstActive lastActive shiftMap operators weekStart  = let
  header = (headFn, [])
  headFn Nothing = Just (toHtml $ show weekStart, [])
  headFn (Just offset) = let
    day = addDays offset weekStart 
    (_, _, dayOfMonth) = toGregorian day
    active = day >= firstActive && day <= lastActive
    activeClass = if active
                  then "active" :: Text
                  else "inactive"
    in Just (toHtml  ((printf "%02d" dayOfMonth) :: String) , ["dayOfMonth", activeClass])

  rows = [ (calendarFn shiftMap op weekStart, [])
         | op <- operators
         ]
  in header:rows


-- * To Front Accounting
-- ** GRN
saveGRNs :: AppSettings -> TimesheetId -> TS.Timesheet _ (TS.Sku, PayrollShiftId) -> ExceptT Text Handler [(Int, [PayrollShiftId])]
saveGRNs settings key timesheet = do
  let connectInfo = WFA.FAConnectInfo (appFAURL settings) (appFAUser settings) (appFAPassword settings)
      psettings = appPayroll settings
      -- in order to know which shifts go into which textcarts
      -- we need to group all shifts so that each groug generate only one textcarts
      -- This sort of defeats the object of using the textcarts function.
      -- shifts are grouped by day and work type
      cartShiftsMap = TS.groupBy (\shift -> let (_, day, stype) = TS._shiftKey shift
                                            in (day, stype)
                                 )
                                 (TS._shifts timesheet)
      carts :: [(TS.Textcart, [PayrollShiftId])]
      carts = [ ( textcart
                , pKeys
                )
              | ((_, stype), shifts) <- Map.toList cartShiftsMap
              , let pKeys = map (snd . view _1 . TS._shiftKey) shifts
              , let shiftsSku = (\((a,_), b, c) -> (a,b,c)) <$$> shifts
              -- normal we should have exactly 1 textcart
              , textcart <- TS.textcarts stype (TS.Timesheet shiftsSku
                                                             (TS._periodStart timesheet)
                                                             (TS._frequency timesheet)
                                                             []
                                               )
              ]
      mkDetail shift = WFA.GRNDetail (pack . TS.sku $ TS._shiftKey shift)
                                     (TS._duration shift)
                                     (view TS.hourlyRate shift)
      mkGRN (TS.Textcart (day, shiftType, shifts), pKeys)  = let
        location = (case shiftType of
                      TS.Holiday -> grnHolidayLocation
                      TS.Work -> grnWorkLocation
                   ) psettings
        ref = grnRef (appPayroll settings) timesheet day shiftType
        in ( WFA.GRN (wagesSupplier psettings)
                     day
                     (Just ref)
                     (Just ref)
                     location
                     Nothing -- delivery
                     ""
                     (map mkDetail shifts)
           , pKeys
           )
      grns = map mkGRN carts
  mapM ( \(grn, keys) -> do
           faId <- ExceptT . liftIO $ WFA.postGRN connectInfo grn
           -- save payroll details instead of timesheet
           ExceptT $ runDB $ do
             insertMany_ [ TransactionMap ST_SUPPRECEIVE faId PayrollShiftE (fromIntegral $ unSqlBackendKey $ unPayrollShiftKey key)
                         | key <- keys
                         ]
             return (Right ())
           return (faId, keys)
       ) grns

-- ** Invoice
saveInvoice :: AppSettings
            -> TS.Timesheet (Text, PayrollExternalSettings) _
            -> [(Int, [PayrollShiftId])]
            -> ExceptT Text Handler Int
saveInvoice settings timesheet deliveries = do
  today <- utctDay <$> liftIO getCurrentTime
  let connectInfo = WFA.FAConnectInfo (appFAURL settings) (appFAUser settings) (appFAPassword settings)
      psettings = appPayroll settings
      ref = invoiceRef (appPayroll settings) timesheet
      invoice = WFA.PurchaseInvoice  (wagesSupplier psettings)

                                     (Just ref)
                                     ref -- supplier ref
                                     today
                                     (addDays 10 today)
                                     "" -- memo
                                     [(id, Just (length keys)) | (id, keys) <- deliveries]
                                     (itemsForCosts timesheet)
  faId <- ExceptT $ liftIO $ WFA.postPurchaseInvoice connectInfo invoice
  ExceptT $ runDB $ do
    insertMany_ [ TransactionMap ST_SUPPINVOICE faId PayrollShiftE (fromIntegral $ unSqlBackendKey $ unPayrollShiftKey key)
                | (_, keys) <- deliveries
                , key <- keys
                ]
    return (Right ())
  return faId


-- | Computes the GL Items corresponding to the extra costs.
-- Only the costs are added to the invoice, as the invoice reflects
-- what the employer has to pay  in total.
-- The deduction are paid by the employees and so are not releveant for the final invoice.
itemsForCosts :: TS.Timesheet (Text, PayrollExternalSettings)
                              (Entity Operator, EmployeeSettings, PayrollShiftId)
              -> [WFA.GLItem]
itemsForCosts timesheet = let
  costs = filter (isJust . preview TS.dacCost) (TS._deductionAndCosts timesheet)
  mkItem dac = let
   ((payee, payeeSettings), (opE, opSettings, _)) = TS._dacKey dac
   Just amount = preview TS.dacCost dac
   account = costGlAccount payeeSettings
   memo = payee <> " " <> (operatorNickname $ entityVal opE)
   in  WFA.GLItem account
                  (dimension1 (opSettings :: EmployeeSettings ))
                  (dimension2 (opSettings :: EmployeeSettings))
                  amount
                  (Just memo)
  in map mkItem costs

-- ** Payment
savePayments :: Ord p
             => AppSettings
             -> TimesheetId
             -> TS.Timesheet p Text
             -> Int
             -> ExceptT Text Handler [Int]
savePayments settings key timesheet invoiceId = do
  today <- utctDay <$> liftIO getCurrentTime
  let connectInfo = WFA.FAConnectInfo (appFAURL settings) (appFAUser settings) (appFAPassword settings)
      psettings = appPayroll settings
      ref = employeePaymentRef psettings timesheet
      payments = employeePayments ref today psettings timesheet (Just invoiceId)

  paymentIds <- mapM (ExceptT . liftIO . WFA.postSupplierPayment  connectInfo) payments
  ExceptT $ runDB $ do
    insertMany_ [TransactionMap ST_SUPPAYMENT faId TimesheetE (fromIntegral $ unSqlBackendKey $ unTimesheetKey key)
                | faId <- paymentIds
                ]
    return (Right())
  return  paymentIds

-- | Create employee payments and allocated them
-- to the given invoice if given
employeePayments :: Ord p
                 => (Text -> Text)
                 -> Day
                 -> PayrollSettings
                 -> (TS.Timesheet p Text)
                 -> Maybe Int
                 -> [WFA.SupplierPayment]
employeePayments ref paymentDate settings timesheet invoiceM =
  let summaries =  TS.paymentSummary timesheet
  in mapMaybe (employeePayment ref paymentDate settings invoiceM) summaries
  

employeePayment :: Ord p
                => (Text -> Text)
                -> Day
                -> PayrollSettings
                -> Maybe Int
                -> TS.EmployeeSummary p Text
                -> Maybe WFA.SupplierPayment
employeePayment _ _ _ _ summary | TS._finalPayment summary <= 0 = Nothing
employeePayment ref paymentDate settings invM summary = let 
  amount = TS._finalPayment summary
  allocations = maybeToList $ invM <&> \inv -> WFA.PaymentTransaction inv ST_SUPPINVOICE amount
  payment = WFA.SupplierPayment (wagesSupplier settings)
                                (wagesBankAccount settings)
                                amount
                                paymentDate
                                (Just $ ref $ TS._sumEmployee summary)
                                (Just 0) -- charge
                                allocations
  in Just payment


-- ** External payments
-- Payments to external entities can be either stored as a direct payment using the main Wages supplier
-- or by a pair of credit note/invoice. The credit note being made on the main wages supplier
-- and the invoice on the external supplier. This is a way of "transfering part of an invoice" from a
-- supplier to another one.
saveExternalPayments :: AppSettings
                     -> TimesheetId
                     -> Int -- ^ Invoice num
                     -> Day -- ^ transaction date
                     -> TS.Timesheet (Text, PayrollExternalSettings) emp 
                     ->  ExceptT Text Handler [Either (Int, Maybe Int) Int]
saveExternalPayments settings key invoiceNo day timesheet = do
  let connectInfo = WFA.FAConnectInfo (appFAURL settings) (appFAUser settings) (appFAPassword settings)
      psettings = appPayroll settings
      reference = invoiceRef psettings timesheet
      pairs = (makeExternalPayments psettings (Just invoiceNo) day reference timesheet) -- :: [( WFA.PurchaseCreditNote, WFA.PurchaseInvoice)]
      tId = fromIntegral . unSqlBackendKey $ unTimesheetKey key

  ids <- mapExceptT lift $ forM pairs $ either
      (\(credit, inv) ->  do
        crId <- ExceptT $ WFA.postPurchaseCreditNote connectInfo credit
        invId <- ExceptT $ WFA.postPurchaseInvoice connectInfo inv
        return $ Left  (crId, Just invId)
      )
      (\payment ->  do
        pId <- ExceptT $ WFA.postSupplierPayment connectInfo payment
        return $ Right pId
      )
  lift $ runDB $ insertMany_ $ concatMap
        (\x -> case x of
                 Left (crId, Nothing) -> error "Should not happend" --  [ TransactionMap ST_SUPPCREDIT crId TimesheetE tId
                 Left (crId, Just invId) -> [ TransactionMap ST_SUPPCREDIT crId TimesheetE tId
                                            , TransactionMap ST_SUPPINVOICE invId TimesheetE tId
                                            ]
                 Right pId -> [ TransactionMap ST_SUPPAYMENT pId TimesheetE tId ]
        ) ids
  return ids


makeExternalPayments :: PayrollSettings
                     -> Maybe Int  -- ^ Invoice number
                     -> Day -- ^ transaction date
                     -> Text -- ^ invoice reference
                     -> TS.Timesheet (Text, PayrollExternalSettings) emp
                     -> [Either (WFA.PurchaseCreditNote, WFA.PurchaseInvoice)
                                 WFA.SupplierPayment]
makeExternalPayments psettings invoiceNo day ref ts = let
  -- We need to group payments by type, ie supplier, reference and due date
  -- Then payments  needs to be group by GLAccount, dimensions
  -- group by Settings (supplier)
  externalItems = concatMap (dacToExternalItems day ref . fmap fst) (TS._deductionAndCosts ts)
  (pairs, payments) = partitionEithers externalItems
  pairMap = unifiesRef pairs
  paymentMap = unifiesRef payments

  in map (Left . makeExternalPair day invoiceNo) (mapToList pairMap)
     <>  map (Right . makeExternalPayment (wagesSupplier psettings) invoiceNo) (mapToList paymentMap)

-- regroup items by reference (supplier/account, ref, date).
-- We don't want to date to be part of the reference, so we need
-- to make sure that each ref as only one date. If not, we need to tweak the ref so they are unique
unifiesRef :: Ord key => [((key, Text, Day), item)] -> Map (key, Text, Day)  [item]
unifiesRef xs = let
  -- group by r
  ref ((k,r,_), _) = (k,r)
  groupByRef = TS.groupBy ref xs
  -- change the ref in case of multiple day
  groupByDay gs = let
    byDay = TS.groupBy (\((_,_,d), _) -> d) gs
    suffixes = "": ["-" <> tshow i | i <- [2..] ]
    -- flatten and add suffix
    in [ ((k, r <> suffix, d), i)
       | (items, suffix) <- zip (toList byDay) suffixes
       , ((k, r, d) , i) <- items
       ]


  xs' = fmap groupByDay groupByRef
  -- regroup using the new ref
  -- could probably optimised
  in  fmap (fmap snd) $ TS.groupBy fst (concat $ toList xs')
  

  

-- | Return a list of either payment to be made or GL Item to be credited and invoiced
dacToExternalItems :: Day
             -> Text 
             -> TS.DeductionAndCost (Text, PayrollExternalSettings)
             -> [ Either ((Int, Text, Day), WFA.GLItem) -- credit/invoice
                         -- ^ -- supplier
                         ((Int, Text, Day), Double) -- payment
                         -- ^ Bank Account
                ] 
dacToExternalItems day tsReference dac = let
    (payee, settings) = dac ^. TS.dacKey
    extract memo_ amountPrism settingsFn = case (dac ^? amountPrism,  settingsFn settings) of
      (Just amount, Just set) | amount > 1e-2 -> let
                             ref = tsReference <> "-" <> fromMaybe payee (paymentRef set)
                             due = calculateDate (paymentTerm set) day 
                             in case paymentSettings set of
                               (DACSupplierSettings supplier glAccount dim1 dim2 memo)  -> 
                                     let item = WFA.GLItem glAccount dim1 dim2 amount (memo <|> Just memo_)
                                     in [Left ((supplier, ref, due), item)]
                               (DACPaymentSettings bankAccount)  ->  [Right ((bankAccount, ref, due), amount )]
      _ -> []
  in extract (payee <> "^") TS.dacDeduction deductionsPaymentSettings
     <>  extract ("^" <> payee) TS.dacCost costPaymentSettings
  
makeExternalPair :: Day -- ^ Transactioin date
                 -> Maybe Int -- ^ Invoice number to allocate Credit note
                 -> ((Int, Text, Day), [WFA.GLItem])
                 -> (WFA.PurchaseCreditNote, WFA.PurchaseInvoice)
makeExternalPair day invoiceNo ((supplier, reference, dueDate), glItems) = let
    credit = WFA.PurchaseCreditNote (supplier)
                                  Nothing
                                  (reference <> "-REV") -- supp reference
                                  day -- date
                                  dueDate  -- due
                                  ("")
                                  invoiceNo -- against invoice
                                  []
                                  glItems
    invoice = WFA.PurchaseInvoice (supplier)
                                  Nothing
                                  reference -- supp reference
                                  day -- date
                                  dueDate  -- due
                                  ("")
                                  []
                                  glItems
    in (credit, invoice)



makeExternalPayment :: Int
                    -> Maybe Int
                    -> ((Int, Text, Day), [Double])
                    -> WFA.SupplierPayment
makeExternalPayment supplier invoiceNo ((bankAccount, reference, dueDate), amounts) = let
  amount = sum amounts
  allocations = [ WFA.PaymentTransaction inv ST_SUPPINVOICE amount | inv <- maybeToList invoiceNo ]
  payment = WFA.SupplierPayment supplier
                                bankAccount
                                amount
                                dueDate
                                (Just reference)
                                (Nothing)
                                allocations
  in payment







 
