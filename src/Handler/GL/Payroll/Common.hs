{-# OPTIONS_GHC -Wno-unused-top-binds #-} -- TODO remove
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-} -- TODO remove
{-# OPTIONS_GHC -Wno-unused-do-bind #-} -- TODO remove
{-# OPTIONS_GHC -Wno-name-shadowing #-} -- TODO remove
{-# OPTIONS_GHC -Wno-orphans #-} -- TODO remove
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
module Handler.GL.Payroll.Common
where

import Import
import Handler.Table
import qualified GL.Payroll.Timesheet as TS
import qualified GL.Payroll.Report as TS
import GL.Payroll.Settings
import GL.Utils
import Database.Persist.MySQL
import Data.Time.Calendar hiding(DayOfWeek(..), dayOfWeek)
import Lens.Micro.Extras (view, preview)
import Lens.Micro hiding ((<&>))
import Data.List.NonEmpty (NonEmpty(..))
import  qualified WH.FA.Types as WFA
import  qualified WH.FA.Curl as WFA
import qualified FA as FA
import Control.Monad.Except
import Text.Printf(printf)
import qualified Data.Map as Map
import Data.List(iterate, cycle)
import Locker
import  Util.Cache

-- ** Orphan Instances 
instance TS.Display Text where
  display = unpack

-- * Types 
-- | Extended version of Timesheest ShiftType adding an option corresponding
-- to work which has been timed
data ShiftType = ShiftType TS.ShiftType | Timed deriving (Show, Eq, Ord)
   
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
          saveTimeSheetModel Nothing key ref (model, shiftsFn, itemsFn)

-- saveTimeSheetModel :: DocumentHash -> (TS.Timesheet String TS.PayrooEmployee) -> Handler (TimesheetId, Text)
saveTimeSheetModel :: Maybe Int --  ^ FA Invoice if already exits (imp)
                   -> DocumentHash
                   -> Text
                   -> ( Key DocumentKey -> Timesheet
                      , Key Timesheet -> [PayrollShift],
                       Key Timesheet -> [PayrollItem])
                   -> ReaderT SqlBackend Handler (Key Timesheet, Text)
saveTimeSheetModel invM key ref (model, shiftsFn, itemsFn) = do
          docKey <- createDocumentKey (DocumentType "timesheet") key ref ""
          modelId <- insert (model docKey)
          shiftKeys <- insertMany (shiftsFn modelId)
          forM invM $ \faId -> insertMany_ [ TransactionMap ST_SUPPINVOICE faId PayrollShiftE (fromIntegral $ unSqlBackendKey shiftKey) False
                                           | (PayrollShiftKey shiftKey) <- shiftKeys
                                           ]
          insertMany_ (itemsFn modelId)
          return (modelId, ref)

loadTimesheet :: Int64 -> Handler (Maybe (Entity Timesheet, [Entity PayrollShift], [Entity PayrollItem]))
loadTimesheet key64 = do
  let key = TimesheetKey (SqlBackendKey key64)
  es <- loadTimesheets [TimesheetId ==. key] [] []
  return $ listToMaybe es

loadTimesheets :: [Filter Timesheet]
               -> [Filter PayrollShift]
               -> [Filter PayrollItem]
               -> Handler [ (Entity Timesheet, [Entity PayrollShift], [Entity PayrollItem])]
loadTimesheets criteria shiftCriteria itemCriteria = do
  runDB $ do
    tss <- selectList criteria []
    forM tss $ \ts -> do
        let key = entityKey ts
        shifts <- selectList ((PayrollShiftTimesheet ==. key) : shiftCriteria) []
        items <- selectList ((PayrollItemTimesheet ==. key) : itemCriteria) []
        return (ts, shifts, items)

-- | Find the time a operator effectively during a given shift
-- ie lookup in mop.action table
loadTimedFromMOPactions :: Day -> Day -> Handler [TS.Shift (Text, Day, ShiftType)]
loadTimedFromMOPactions start end = do
    let sql =    "   select date, fOp.nickname, fOp.operator_id, sum(duration)/60 "
             <> "   from mop.session "
             <> "   join mop.actiongroup ON (actiongroup.id = groupId) "
             <> "   join mop.operator as mopOp on (mopOp.id = operatorId) "
             <> "   join fames_operator as fOp on (mopOp.name = fOp.nickname) "
             <> "   where invalid = 0 "
             <> "   and actiongroup.id IN (SELECT distinct actionGroupId FROM mop.action)"
             <> "   and date >= ? and date <= ? "
             <> "   group by date, operatorId "
        toShift (Single date, Single opName, Single opKey, Single duration) =
          TS.Shift (opName, date, Timed) Nothing (makeDurationLock opKey  duration) (makeAmountLock opKey 0 )

    rows <- runDB $ rawSql sql [PersistDay start, PersistDay end]
    return $ map toShift rows

addTimedFromMop :: Day -> Day -> [TS.Shift (Text, Day, TS.ShiftType)] -> Handler [TS.Shift (Text, Day, ShiftType)]
addTimedFromMop start end shifts = do
   let shifts' = (_3 %~ ShiftType )<$$> shifts
       operatorSet = setFromList $ map (view $ TS.shiftKey . _1 ) shifts :: Set Text
   timed <- loadTimedFromMOPactions start end

   return $ shifts' <> filter (flip member operatorSet . view (TS.shiftKey . _1 )) timed
-- | Keep timed shift for operator present in the actual shifts
addTimedShifts :: [TS.Shift (Text, Day, TS.ShiftType)] -> [(TS.Shift (Text, Day, ShiftType))] -> [(TS.Shift (Text, Day, ShiftType))]
addTimedShifts shifts timed = let
  shifts' = (_3 %~ ShiftType )<$$> shifts
  operatorSet = setFromList $ map (view $ TS.shiftKey . _1 ) shifts :: Set Text
  in shifts' <> filter (flip member operatorSet . view (TS.shiftKey . _1 )) timed

loadTimedShiftsFromTS :: TS.Timesheet paye Text -> Handler [TS.Shift (Text, Day, ShiftType)]
loadTimedShiftsFromTS timesheet = do
  loadTimedFromMOPactions (TS._periodStart timesheet) (TS.periodEnd timesheet)


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
                    (unsafeUnlock $ TS._duration shift)
                    (unsafeUnlock $ TS._cost shift)
                    operator
                    (Just day)
                    (tshow shiftType)
  itemsFn i = concatMap (mkItem i) (TS._deductionAndCosts ts)
  mkItem :: TimesheetId -> TS.DeductionAndCost (String, OperatorId) -> [PayrollItem]
  mkItem i dac = mergeTheseWith (mkDAC Deduction) (mkDAC Cost) (<>) (bimap unsafeUnlock unsafeUnlock $ TS._dacDac dac) where
                  (payee, op) = TS._dacKey dac
                  mkDAC dacType amount = [ PayrollItem i amount dacType op (pack payee) "" ]

makeLock name opKey = lock $ makeLock' name (tshow . unSqlBackendKey . unOperatorKey $  opKey)
makeLock' name extra = ["Payroll/" <> name <> "/" <> extra]
makeDurationLock = makeLock "duration"
makeAmountLock = makeLock "amount"

modelToTimesheetOpId :: Entity Timesheet
                     -> [Entity PayrollShift]
                     -> [Entity PayrollItem]
                     -> (TS.Timesheet Text (OperatorId, PayrollShiftId))
modelToTimesheetOpId (Entity _ timesheet) shiftEs itemEs = let
  readType "Holiday" = TS.Holiday
  readType _ = TS.Work
  makeLock name opKey = lock $ makeLock' name (tshow . unSqlBackendKey . unOperatorKey $  opKey)
  makeLock' name extra = ["Payroll/" <> name <> "/" <> extra]
  mkShift (Entity key shift) = TS.Shift  (mkShiftKey key shift) Nothing
                                         (makeDurationLock (payrollShiftOperator shift) $ payrollShiftDuration shift)
                                         (makeAmountLock (payrollShiftOperator shift) $ payrollShiftCost shift)
  mkShiftKey key shift = ( (payrollShiftOperator shift, key)

                     , fromMaybe (error "TODO") (payrollShiftDate shift)
                     , readType (payrollShiftType shift)
                     )
  mkItems = (TS.groupDacsBy id) . (map mkItem)
  mkItem (Entity _ i) = TS.DeductionAndCost
                        (payrollItemPayee i, (payrollItemOperator i, PayrollShiftKey (SqlBackendKey 1)))
                        ( ( case payrollItemType  i of
                            Cost -> That
                            Deduction -> This
                          ) (restrict (makeLock' "external" (payrollItemPayee i)) $ makeLock  "amount" (payrollItemOperator i) (payrollItemAmount i))
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
  -> Handler (Either Text (TS.Timesheet p Text))
timesheetOpIdToTextH ts = do
  employeeInfos <- getEmployeeInfo
  return $ timesheetOpIdToText employeeInfos ts

-- ** GL Account 
-- | GL Account should be Int, however
-- There definition in FA Allows leading 0 and so need to be converted to Text
-- A solution would be to format them using printf like.
-- Instead, we load all account from the DB And create a Map
mkAccountH :: Handler (Int -> WFA.GLAccount)
mkAccountH = cache0 False (cacheMinute 5) "gl-account-maker" $ do
  accounts <- runDB $ selectList [] []
  let accountMap = [ (asNum, account)
                   | (Entity key _) <- accounts
                   , let account = FA.unChartMasterKey key
                   , Just asNum <- [readMay account]
                   ]
  return $ \acc -> WFA.GLAccount $ findWithDefault (tshow acc) acc accountMap
  

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
  , "#" <> tshow (payrollId emp)
  , "$" <> tshow (hourlyRate emp)
  ]

period :: PayrollSettings -> PayrollFrequency -> Day -> (Integer, Int, String, Day)
period settings TS.Weekly day = let
  (y, w) = weekNumber (Start $ firstTaxWeek settings) day
  w' = printf "W%02d" w
  in (y, w, w', addDays 6 day )
period settings TS.Monthly day = let
  (y, m) = monthNumber (Start $ firstTaxMonth settings) day
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
  start = Start $ case frequency of
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

displayLastSheets :: Int -> Handler Widget
displayLastSheets n = do
  timesheets <- runDB $ selectList [TimesheetStatus !=. Pending] [LimitTo n, Desc TimesheetId ]
  return [whamlet|
     <div.panel.panel-primary>
       <div.panel-heading.datatable> Last Timesheets
       ^{displayTimesheetList timesheets}
          |]

-- ** Timesheet list 
displayTimesheetList :: [Entity Timesheet] -> Widget
displayTimesheetList timesheets = [whamlet|
<table.table.table-hover.table-striped.nowrap.nowrap.responsive style=width:100%>
  <thead>
    <tr>
      <th data-priority=0 > Id
      <th data-priority=1 > Reference
      <th data-priority=3 > Period Type
      <th data-priority=20 > Start
      <th data-priority=50 > End
      <th data-priority=2 > Status
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
displayTimesheet :: ( ?viewPayrollAmountPermissions :: (Text -> Granted)
                    , ?viewPayrollDurationPermissions :: (Text -> Granted))
                 => (TS.Timesheet String TS.PayrooEmployee) -> Widget
displayTimesheet timesheet = do
  let e g = either (return "") show . unlock g 
      amountF = e ?viewPayrollAmountPermissions
      durationF = e ?viewPayrollDurationPermissions
  
  displayShifts (TS.displayShift amountF durationF) $ TS._shifts timesheet
  displayShifts (TS.displayDAC amountF durationF) $ TS._deductionAndCosts timesheet

displayShifts display shifts = let
  reportLines = map display shifts
  -- reportLines = lines $ pack report  :: [Text]
  in [whamlet|<div.well>
        $forall line <- reportLines
          <p> #{line}
     |]

e g = either (return "") show . unlock g 

displayDAC = let
  amountF = e ?viewPayrollAmountPermissions
  durationF = e ?viewPayrollDurationPermissions
  in TS.displayDAC amountF durationF

-- ** Employee Summary 
-- | Displays a table with all payment information for each employee t
displayEmployeeSummary :: (?viewPayrollAmountPermissions :: (Text -> Granted))
                       => TS.Timesheet Text Text -> Widget
displayEmployeeSummary = displayEmployeeSummary' [] []
displayEmployeeSummary' :: (?viewPayrollAmountPermissions :: (Text -> Granted) )
                        => [Text ] 
                        -> [ ( Text, [PayrollFormula] ) ]
                        -> TS.Timesheet Text Text -> Widget
displayEmployeeSummary' nameOrder formulas timesheet= let
  columnWeight = columnWeightFromList (map fst formulas)
  -- add a weight to employe name
  empWeights :: Map Text Int
  empWeights = Map.fromList $ zip nameOrder [-1000000 .. 0 ]
  addWeight e = (findWithDefault 1 e empWeights, e)
        
  summaries = map (tweakSummary formulas . fmap snd ) $ TS.paymentSummary $ fmap addWeight timesheet
              --                           ^^^^^^^^ remove weight           ^^^^^^^^^^^^^ add weight
  (cols0, colnames) = employeeSummaryColumns summaries
  
  -- weight return a maybe weight
  cols = map fst
       . sortOn snd
       $ [ (col, weight)
         | (col@(_, colname), i) <- zip cols0 [1..]
         , Just weight <- [columnWeight colname i]
         ]
  rows = employeeSummaryRows summaries
  in employeeSummaryTable cols colnames rows

-- | Computes a weight function compatible with displayEmployeeSummary'
-- by taking a list of column name to display in order
columnWeightFromList :: [Text] -> (Text -> Int -> Maybe Int)
columnWeightFromList [] __col i = Just i
columnWeightFromList colnames col _ = let
  weights = Map.fromList (zip colnames [1..])
  in lookup col weights
  
-- Add virtual columns corresponding to the give formulas
tweakSummary :: [(Text, [PayrollFormula])] -> TS.EmployeeSummary Text Text -> TS.EmployeeSummary Text Text
tweakSummary formulas emp =  let
  val118 = (\x -> x - 118) <$> emp ^. TS.gross -- unside a locker
  val512 = (\x -> x - 512) <$> emp ^. TS.gross -- unside a locker
  deducs = emp ^. TS.netDeductions
  suffixKeys suffix = Map.mapKeysMonotonic (<> suffix) 
  emp' = emp & TS.deductions %~ (suffixKeys "(e)")
                 & TS.costs %~ (suffixKeys "(r)")
  in emp' & TS.netDeductions .~ deducs <> mapFromList ([ ("Qualified Earnings (W)", val118)
                                                       , ("Qualified Earnings (M)", val512)
                                                       ] <> 
                                                       [ (name, evalFormulas emp' formula)
                                                       | (name, formula) <- formulas
                                                       , [PFVariable name] /= formula
                                                       , [] /= formula
                                                       -- \^ we need to filter normal field
                                                       -- but only keep calculated formula
                                                       ] 
                                                     )

evalFormulas :: TS.EmployeeSummary Text Text -> [PayrollFormula] -> TS.Amount
evalFormulas emp = sum . map (fromMaybe 0 . evalFormula emp) 
evalFormula :: TS.EmployeeSummary Text Text -> PayrollFormula -> Maybe TS.Amount
evalFormula  emp formula = let
   values = TS._deductions emp
             <> TS._netDeductions emp
             <> TS._costs emp
             <> Map.fromList ([ ("Gross",  TS._gross emp)
                             , ("Net", TS._net emp)
                             , ("To Pay", TS._finalPayment emp)
                             , ("Total Cost", TS._totalCost emp)
                             ]
                             <> [(tshow k, h)
                                | (k,h)  <- Map.toList (TS._totalHours emp)
                                ]
                             )
   in case formula of
       PFVariable var -> lookup var values
       PFNegVariable var -> negate <$$> lookup var values
       PFValue v -> return (pure v)
  
-- | Return columns compatibles with employeeSummaryTable
employeeSummaryColumns :: [TS.EmployeeSummary Text e]
                       -> ([(Maybe (TS.EmployeeSummary Text e -> Map Text TS.Amount),
                             Text)] --  ^ columns (value getter, column name)
                          , (a, Text) -> (Html, [a1]) --  ^ function to render column names
                          )
employeeSummaryColumns summaries = let 
  toCols getter = map (Just getter,)  .  Map.keys $ mconcat $ map getter summaries
  deductions =  toCols TS._deductions
  netDeductions =  toCols TS._netDeductions
  costs =  toCols (TS._costs)
  -- hours = toCols (_ . unlock ?viewPayrollAmountPermissions . convertHours)
  hours = toCols convertHours
  convertHours s = Map.fromList [(tshow k <> "\n(hrs)", h)
                                | (k,h)  <- Map.toList (TS._totalHours s)
                                ]
    
  -- -| Columns are either straight field (Nothing)
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

-- | Display a table with all payment informaiton
-- the rows are different from the summaries. This allows
-- the header to generate different column that then one actually needed.
-- this is usefull when displaying different timesheet in different tables
-- but with all the table  having the same header.
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

-- | Default colours, taken from plottly. Factorize with reports
defaultColors :: [Text]
defaultColors = defaultPlottly where
  defaultPlottly  = ["#1f77b4",  -- muted blue
              -- "#ff7f0e",  -- safety orange
              "#2ca02c",  -- cooked asparagus green
              -- "#d62728",  -- brick red
              "#2728d6",  -- brick red
              "#9467bd",  -- muted purple
              "#8c564b",  -- chestnut brown
              "#e377c2",  -- raspberry yogurt pink
              "#77c2e3",  -- raspberry yogurt pink
              -- "#7f7f7f",  -- middle gray
              "#bcbd22",  -- curry yellow-green
              "#17becf"   -- blue-teal
             ]
employeeSummaryRows summaries = let 
  -- -| Columns are either straight field (Nothing)
  -- or the name of a payee in in the given dacs map (Just ...)
  colFns = map mkColFn summaries
          
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
formatDouble' x = either (const ("<locked>" :: Html)) formatDoubl'' $ unlock ?viewPayrollAmountPermissions x
formatDoubl'' x | abs x < 1e-2 = ""
                | x < 0 = [shamlet|<span.text-danger>#{formatDouble x}|]
                | otherwise = toHtml $ formatDouble x

-- | Export a form with all payment to be made and an option to change the amount and the date
generatePaymentForm timesheet = let
  summaries = TS.paymentSummary timesheet
  in [whamlet|
   <table.table.table-bordered.table-striped.table-hover>
     <tr>
       <th> Employee
       <th> Amount
       <th> Date
     $forall summary <- summaries
       <tr>
         <td> #{toHtml $ fst $ TS._sumEmployee summary}
         <td.text-right>
            <input type=number step=0.01 min=0 name="payment-amount-for-#{snd (TS._sumEmployee summary)}" value="#{formatDouble' $ TS._net summary}">
         <td>
            <input name="payment-date-for-#{snd (TS._sumEmployee summary)}" type=date>
             |]

-- ** Calendar 
-- | Display timesheet as a calendar
-- displayTimesheetCalendar :: (?viewPayrollDurationPermissions:: Text -> Granted)
--                          =>  [TS.Shift paye Text] -> (TS.Timesheet payee Text) -> Widget
displayTimesheetCalendar timed timesheet = do
  let periodStart = TS._periodStart timesheet
      periodEnd = TS.periodEnd timesheet
      -- some timesheet might have for some reason shift outside of the authorized period
      -- we need to be able to display it anyway.
  let allShifts = TS._shifts timesheet
      days = map (^. TS.day) allShifts
      (start, end) = case days of
          [] -> case TS._frequency timesheet of
                         TS.Weekly -> (periodStart, periodEnd)
                         TS.Monthly -> ( previousWeekDay Sunday periodStart
                                       , nextWeekDay Saturday periodEnd)
          (d:ds) -> let minDay0 = minimum (ncons d ds)
                        maxDay0 = maximum (ncons d ds)
                        startDay = case TS._frequency timesheet of
                                      TS.Weekly -> dayOfWeek periodStart
                                      TS.Monthly -> Monday
                        endDay = predCyclic startDay 
                        minDay = previousWeekDay startDay (min minDay0 periodStart)
                        maxDay = nextWeekDay endDay (max maxDay0 periodEnd)
                    in (minDay, maxDay)

      -- get start and end of displayed calendar
      -- for weekly, it's just the week
      -- for month, we start on Sunday
  displayCalendar (TS._frequency timesheet == TS.Monthly )
                  start end
                  periodStart periodEnd
                  (addTimedShifts allShifts timed)

-- displayCalendar :: Show emp =>  Day -> Day -> Day -> Day
--                 -> Map Text (Map Day [TS.Shift emp])
--                 -> Widget
displayCalendar :: (?viewPayrollDurationPermissions::Text -> Granted)
  => Bool --  ^ display total
  -> Day
  -> Day
  -> Day
  -> Day
  -> [TS.Shift (Text, Day, ShiftType)]
  -> Widget
displayCalendar displayTotal start end firstActive lastActive shifts = do
  let shiftMap' = TS.groupBy (^. TS.shiftKey . _1 ) shifts
      shiftMap = TS.groupBy (^. TS.day) <$> shiftMap'
      columns = ((Left False) : (map Right [0..6])) <> [Left True]
      -- columns = map dayOfWeek (take 7  $ iterate (addDays 1) start )
      weekStarts = takeWhile (<end) (iterate  (addDays 7) start)
      colDisplay (Left False) = ("Operator", [])
      colDisplay (Left True) = ("Total", ["total"])
      colDisplay (Right index) = let weekDay = dayOfWeek (addDays index start)
                         in (toHtml $ show weekDay , case weekDay of
                               Saturday -> ["weekend"]
                               Sunday -> ["weekend"]
                               _ -> [])
      operators =  keys shiftMap
      rows' = concatMap (rowsForWeek firstActive lastActive shiftMap operator'colors) weekStarts
      rows = if displayTotal
                   then rows' <> rowTotal start shiftMap operator'colors 
                   else rows'
      colors = defaultColors --  ["green", "blue", "orange", "purple", "brown"] :: [Text]
      operator'colors = zip operators (cycle colors)
      css = [cassius|
             table tr.total
                font-weight: 700
             td.Saturday, td.Sunday
                background: #fee
             span.badge.Holiday
                color: white
                background: lightgray
             span.badge.timed-extra
                color: white
                background: orangered
             span.badge.timed-slacking-off
                color: white
                background: darkorange
             span.badge.timed-bad
                color: white
                background: orange
             span.badge.timed-ok
                color: white
                background: gold
             span.badge.timed-good
                color: white
                background: lawngreen
             span.badge.Work
                color: white
                background: black
             .dayOfMonth
                text-align:right
                font-size: 1.2em
                font-style: italic
                &.weekend
                   color: red
                &.inactive
                  color: gray
             td.total
                background: darkgray
             span.hover-only
                display: none
             td:hover span.hover-only
                  display: inline-block
             span.Xnon-hover
                display: inline-block
             td:hover span.Xnon-hover
                  display: none
                  |]
  displayTable columns colDisplay rows >> toWidget css

calendarFn :: (?viewPayrollDurationPermissions :: Text -> Granted)
           => (Map Text (Map Day [TS.Shift (Text,Day,ShiftType)]))
           -> (Text, Text)
           -> Day
           -> Either Bool Integer
           -> Maybe (Html, [Text])
calendarFn __shiftMap (operator,color) __weekStart (Left False) = Just $ ([shamlet|<span style="color:#{color}">#{operator}|], [])
calendarFn shiftMap (operator,color) weekStart (Left True) = do -- Maybe
  -- display total, for that we need all the shifts for the given week
  let days = map (`addDays` weekStart) [0..6]
  dateMap <- lookup operator shiftMap
  let shifts = concat $ mapMaybe (flip lookup dateMap) days
      types = TS.groupShiftsBy (^. _3 ) shifts
  Just (displayTimeBadges color (9*5) types, ["total"])

calendarFn shiftMap (operator,color) weekStart (Right col) = do -- Maybe
  let day = addDays col weekStart
      maxDuration = 9 -- TODO pass as parameter

  dateMap <- lookup operator shiftMap
  shifts <- lookup day dateMap
  let types = TS.groupShiftsBy (^. _3) shifts
      html = displayTimeBadges color maxDuration types
  return (html, [])

-- | Display shift as badges
displayTimeBadges :: (?viewPayrollDurationPermissions :: Text -> Granted)
                  => Text -> Double -> [TS.Shift ShiftType] -> Html
displayTimeBadges color maxDuration durations0 = 
  let durationWidth d =  formatDouble $ max 20 $ min maxDuration d / maxDuration * 100
      (timeds, durations) = partitionEithers $ map shiftToE durations0
      timedM = if null timeds then Nothing else either (const Nothing) Just (unlock' $ sum timeds)
      _a = timeds
      workedM = case filter ((== TS.Work) . TS._shiftKey)  durations of
                   [] -> Nothing
                   ts -> either (const Nothing) Just $ unlock' $ sum $ map TS._duration ts
      shiftToE shift = case TS._shiftKey shift of
        Timed -> Left (TS._duration shift)
        ShiftType st -> Right $ shift {TS._shiftKey = st}
      bg shift = case TS._shiftKey shift of
        TS.Work ->  "background:" <> color
        -- TS.Work | Just timed <- timedM, Right duration <- unlock' (TS._duration shift),  timed <= duration -> "background:" <> color
        -- TS.Work | Nothing <- timedM  -> "background:" <> color
        _ -> "" -- use css default. gray for holiday, red for Work < than clocked timed
      classForTimed Nothing _ = "timed-extra"
      classForTimed (Just worked) timed = let ratio = timed /worked
         in case () of
              () | ratio > 1   -> "timed-extra" :: Text
              () | ratio >= (7/7.5) -> "timed-good"
              () | ratio >= (6/7.5) -> "timed-ok"
              () | ratio >= (5/7.5) -> "timed-bad"
              _                -> "timed-slacking-off"
      unlock' = unlock ?viewPayrollDurationPermissions
      invAndMul a b = a/b
  in [shamlet|
    $forall shift <-  durations
      $with durationE <- unlock' (TS._duration shift)
         $case durationE
           $of Left e 
             <.invisible>LOCK #{tshow e}
           $of Right duration
              <span.badge.non-hover class=#{show $ TS._shiftKey shift} style="width:#{durationWidth $ fromMaybe duration timedM}%;#{bg shift};color:white">
                #{formatHours duration}
    $maybe timed <- timedM
       $with timeAdjusted <- maybe timed (subtract timed) workedM
        <span.badge.non-hover class=#{classForTimed workedM timed} style="width:#{durationWidth $ abs timeAdjusted}%">
            #{formatHours $ abs $ timeAdjusted}
        <span.hover-only>
          <p> Paied for: #{formatHours $ fromMaybe 0 workedM} hours
          <p> Including Holidays : #{either (const "0") formatHours $ unlock' $ sum $ map TS._duration durations} hours
          <p> Timed : #{formatHours timed} hours
          <p> Idle : #{formatHours timeAdjusted} hours (#{formatDouble $ maybe 100 (invAndMul (100 * timeAdjusted)) workedM }%)
                 |]
          
-- Display a week. The first line is the day of month
-- then each operator
rowsForWeek :: (?viewPayrollDurationPermissions::Text -> Granted)
  => Day
  -> Day
  -> Map Text (Map Day [TS.Shift (Text, Day, ShiftType)])
  -> [(Text, Text)]
  -> Day
  -> [(Either Bool Integer -> Maybe (Html, [Text]), [t])]
rowsForWeek firstActive lastActive shiftMap operators weekStart  = let
  header = (headFn, [])
  headFn (Left False) = Just (toHtml $ formatTime defaultTimeLocale "%d %B %Y"  weekStart, [])
  headFn (Left True) = Nothing
  headFn (Right offset) = let
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

rowTotal :: (?viewPayrollDurationPermissions::Text -> Granted)
  => Day
  -> Map Text (Map Day [TS.Shift (Text, Day, ShiftType)])
  -> [(Text, Text)]
  -> [(Either Bool Integer -> Maybe (Html, [Text]), [Text])]
rowTotal periodStart shiftMap operators = let
  header = (headerFn, ["total"])
  headerFn (Left False) = Just ("Operator", [])
  headerFn (Left True) = Just ("Summary", [])
  headerFn _ =  Nothing
  shiftMap0 = shiftMap <&> \dateMap -> let
    shifts = join (toList dateMap)
    -- in TS.groupShiftsBy (^. _2) (shifts & mapped . mapped . TS.shiftKey . _2 .~ periodStart)
    in singletonMap periodStart shifts
  go _ (Right _) = Nothing
  go op col = calendarFn shiftMap0 op periodStart col
  rows = [ (go op , [])
         | op <- operators
         ]
 in header:rows
  

-- * To Front Accounting 
-- ** GRN 
saveGRNs :: AppSettings -> TimesheetId -> TS.Timesheet _ (TS.Sku, PayrollShiftId) -> ExceptT Text Handler [(Int, Map TS.Sku [PayrollShiftId])]
saveGRNs settings __key timesheet = do
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
      carts :: [(TS.Textcart, Map TS.Sku [PayrollShiftId])]
      carts = [ ( textcart
                , pKeys
                )
              | ((_, stype), shifts) <- Map.toList cartShiftsMap
              , let shiftKey = snd . view _1 . TS._shiftKey
              , let shiftSku = fst . view _1 . TS._shiftKey
              , let shiftsSku = (\((a,_), b, c) -> (a,b,c)) <$$> shifts
              , let pKeys =  Map.fromListWith (++) $ zip (map shiftSku shifts)
                                                         (map (pure . shiftKey) shifts)
              -- normal we should have exactly 1 textcart
              , textcart <- TS.textcarts stype (TS.Timesheet shiftsSku
                                                             (TS._periodStart timesheet)
                                                             (TS._frequency timesheet)
                                                             []
                                               )
              ]
      mkDetail shift = WFA.GRNDetail (pack . TS.sku $ TS._shiftKey shift)
                                     (unsafeUnlock $ TS._duration shift)
                                     (unsafeUnlock $ view TS.hourlyRate shift)
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
  mapM ( \(grn, keyMap) -> do
           faId <- ExceptT . liftIO $ WFA.postGRN connectInfo grn
           -- save payroll details instead of timesheet
           ExceptT $ runDB $ do
             insertMany_ [ TransactionMap ST_SUPPRECEIVE faId PayrollShiftE (fromIntegral $ unSqlBackendKey $ unPayrollShiftKey key) False
                         | keys <- toList keyMap
                         , key <- keys
                         ]
             return (Right ())
           return (faId, keyMap)
       ) grns

-- ** Invoice 
saveInvoice :: (Int -> WFA.GLAccount) -> Day
            -> AppSettings
            -> TS.Timesheet (Text, PayrollExternalSettings) _
            -> [(Int, Map TS.Sku [PayrollShiftId])]
            -> ExceptT Text Handler Int
saveInvoice mkAccount today settings timesheet deliveries = do
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
                                     (itemsForCosts mkAccount timesheet)
  faId <- ExceptT $ liftIO $ WFA.postPurchaseInvoice connectInfo invoice
  ExceptT $ runDB $ do
    insertMany_ [ TransactionMap ST_SUPPINVOICE faId PayrollShiftE (fromIntegral $ unSqlBackendKey $ unPayrollShiftKey key) False
                | (_, keyMap) <- deliveries
                , keys <- toList keyMap
                , key <- keys
                ]
    return (Right ())
  return faId


-- | Computes the GL Items corresponding to the extra costs.
-- Only the costs are added to the invoice, as the invoice reflects
-- what the employer has to pay  in total.
-- The deduction are paid by the employees and so are not releveant for the final invoice.
itemsForCosts :: (Int -> WFA.GLAccount)
              -> TS.Timesheet (Text, PayrollExternalSettings)
                              (Entity Operator, EmployeeSettings, PayrollShiftId)
              -> [WFA.GLItem]
itemsForCosts mkAccount timesheet = let
  costs = filter (isJust . preview TS.dacCost) (TS._deductionAndCosts timesheet)
  mkItem dac = let
   ((payee, payeeSettings), (opE, opSettings, _)) = TS._dacKey dac
   Just amount = preview TS.dacCost dac
   account = costGlAccount payeeSettings
   memo = payee <> " " <> (operatorNickname $ entityVal opE)
   in  WFA.GLItem (mkAccount account)
                  (dimension1 (opSettings :: EmployeeSettings ))
                  (dimension2 (opSettings :: EmployeeSettings))
                  (unsafeUnlock amount)
                  Nothing
                  (Just memo)
  in map mkItem costs

-- ** Payment 
savePayments :: Ord p
             => Day --  ^ Payment date
             -> Maybe Text --  ^ Payment reference suffix. Too avoid duplicate
             -> Map Text (Maybe Double , Maybe Day) --  ^ Payments overriding
             -> AppSettings
             -> TimesheetId
             -> TS.Timesheet p Text
             -> Int
             -> ExceptT Text Handler [Int]
savePayments today refSuffix paymentMap settings key timesheet invoiceId = do
  let connectInfo = WFA.FAConnectInfo (appFAURL settings) (appFAUser settings) (appFAPassword settings)
      psettings = appPayroll settings
      ref = employeePaymentRef psettings timesheet
      payments0 = employeePayments ref today paymentMap psettings timesheet (Just invoiceId)
      payments = [ payment {WFA.spReference = Just ref}
                 | payment@WFA.SupplierPayment{..} <- payments0
                 -- ad the suffix only if reference is set
                 , let ref = maybe ""(<> fromMaybe "" refSuffix) spReference
                 ]

  paymentIds <- mapM (ExceptT . liftIO . WFA.postSupplierPayment  connectInfo) payments
  ExceptT $ runDB $ do
    insertMany_ [TransactionMap ST_SUPPAYMENT faId TimesheetE (fromIntegral $ unSqlBackendKey $ unTimesheetKey key) False
                | faId <- paymentIds
                ]
    return (Right())
  return  paymentIds

-- | Create employee payments and allocated them
-- to the given invoice if given
employeePayments :: Ord p
                 => (Text -> Text)
                 -> Day
                 -> Map Text (Maybe Double, Maybe Day)
                 -> PayrollSettings
                 -> (TS.Timesheet p Text)
                 -> Maybe Int
                 -> [WFA.SupplierPayment]
employeePayments ref paymentDate paymentMap settings timesheet invoiceM =
  let summaries =  TS.paymentSummary timesheet
  in mapMaybe (employeePayment ref paymentDate paymentMap settings invoiceM) summaries
  

employeePayment :: Ord p
                => (Text -> Text)
                -> Day
                 -> Map Text (Maybe Double, Maybe Day)
                -> PayrollSettings
                -> Maybe Int
                -> TS.EmployeeSummary p Text
                -> Maybe WFA.SupplierPayment
employeePayment _ _ _ _ _ summary | TS._finalPayment summary <= 0 = Nothing
employeePayment ref paymentDate paymentMap settings invM summary = do -- Maybe
    (userAmountM, userDate ) <- lookup (TS._sumEmployee summary) paymentMap
    userAmount <- userAmountM
    let amount = min userAmount (unsafeUnlock $ TS._finalPayment summary)
    guard (amount >0)
    let netRefund = negate . sum . filter (<0) . map unsafeUnlock . toList $ TS._deductions summary
        allocations = case amount -netRefund of
          allocated | allocated > 0  -> maybeToList $ invM <&> \inv -> WFA.PaymentTransaction inv ST_SUPPINVOICE
                                                                            (amount - netRefund 
                                                                            )
          _ -> [] -- allocat things manually if needed
        payment = WFA.SupplierPayment (wagesSupplier settings)
                                      (wagesBankAccount settings)
                                      amount
                                      (fromMaybe paymentDate userDate)
                                      (Just $ ref $ TS._sumEmployee summary)
                                      (Just 0) -- charge
                                      allocations
    Just payment


-- ** External payments 
-- Payments to external entities can be either stored as a direct payment using the main Wages supplier
-- or by a pair of credit note/invoice. The credit note being made on the main wages supplier
-- and the invoice on the external supplier. This is a way of "transfering part of an invoice" from a
-- supplier to another one.
saveExternalPayments :: AppSettings
                     -> TimesheetId
                     -> Int --  ^ Invoice num
                     -> (Int -> WFA.GLAccount) --  ^ Convert account no to account in Text (might include adding leading 0)
                     -> Day --  ^ transaction date
                     -> TS.Timesheet (Text, PayrollExternalSettings) emp 
                     ->  ExceptT Text Handler [Either (Int, Maybe Int) Int]
saveExternalPayments settings key invoiceNo mkAccount day timesheet = do
  let connectInfo = WFA.FAConnectInfo (appFAURL settings) (appFAUser settings) (appFAPassword settings)
      psettings = appPayroll settings
      reference = invoiceRef psettings timesheet
      pairs = (makeExternalPayments psettings (Just invoiceNo) mkAccount day reference timesheet) -- :: [( WFA.PurchaseCreditNote, WFA.PurchaseInvoice)]
      tId = fromIntegral . unSqlBackendKey $ unTimesheetKey key

  ids <- mapExceptT liftIO $ forM pairs $ either
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
                 Left (__crId, Nothing) -> error "Should not happend" --  [ TransactionMap ST_SUPPCREDIT crId TimesheetE tId
                 Left (crId, Just invId) -> [ TransactionMap ST_SUPPCREDIT crId TimesheetE tId False
                                            , TransactionMap ST_SUPPINVOICE invId TimesheetE tId False
                                            ]
                 Right pId -> [ TransactionMap ST_SUPPAYMENT pId TimesheetE tId False ]
        ) ids
  return ids


makeExternalPayments :: PayrollSettings
                     -> Maybe Int  -- ^ Invoice number
                     -> (Int -> WFA.GLAccount)
                     -> Day --  ^ transaction date
                     -> Text --  ^ invoice reference
                     -> TS.Timesheet (Text, PayrollExternalSettings) emp
                     -> [Either (WFA.PurchaseCreditNote, WFA.PurchaseInvoice)
                                 WFA.SupplierPayment]
makeExternalPayments psettings invoiceNo mkAccount day ref ts = let
  -- We need to group payments by type, ie supplier, reference and due date
  -- Then payments  needs to be group by GLAccount, dimensions
  -- group by Settings (supplier)
  externalItems = concatMap (dacToExternalItems mkAccount day ref . fmap fst) (TS._deductionAndCosts ts)
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
dacToExternalItems :: (Int -> WFA.GLAccount)
             -> Day
             -> Text 
             -> TS.DeductionAndCost (Text, PayrollExternalSettings)
             -> [ Either ((Int, Text, Day), WFA.GLItem) -- credit/invoice
                         --  ^ -- supplier
                         ((Int, Text, Day), Double) -- payment
                         --  ^ Bank Account
                ] 
dacToExternalItems mkAccount day tsReference dac = let
    (payee, settings) = dac ^. TS.dacKey
    extract memo_ amountPrism settingsFn = case (unsafeUnlock <$> dac ^? amountPrism,  settingsFn settings) of
      (Just amount, Just set) | amount > 1e-2 -> let
                             ref = tsReference <> "-" <> fromMaybe payee (paymentRef set)
                             due = calculateDate (paymentTerm set) day 
                             in case paymentSettings set of
                               (DACSupplierSettings supplier glAccount dim1 dim2 memo)  -> 
                                     let item = WFA.GLItem (mkAccount glAccount) dim1 dim2 amount Nothing (memo <|> Just memo_)
                                     in [Left ((supplier, ref, due), item)]
                               (DACPaymentSettings bankAccount)  ->  [Right ((bankAccount, ref, due), amount )]
      _ -> []
  in extract (payee <> "^") TS.dacDeduction deductionsPaymentSettings
     <>  extract ("^" <> payee) TS.dacCost costPaymentSettings
  
makeExternalPair :: Day --  ^ Transactioin date
                 -> Maybe Int --  ^ Invoice number to allocate Credit note
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







 

viewPayrollAmountPermissions = do
  role <- currentRole
  let unlocker operator = granter role (ViewPriv, operator)
  return unlocker
 
viewPayrollDurationPermissions = do
  role <- currentRole
  let unlocker operator = granter role (ViewPriv, operator)
  return unlocker


isShiftDurationUnlocked = isUnlocked ?viewPayrollDurationPermissions . TS._duration
isShiftAmountUnlocked = isUnlocked ?viewPayrollDurationPermissions . TS._duration
isShiftUnlocked = liftA2 (&&) isShiftDurationUnlocked isShiftAmountUnlocked
isDACUnlocked = isUnlocked ?viewPayrollAmountPermissions . TS.dacTotal 
isShiftViewable = liftA2 (||) isShiftAmountUnlocked isShiftDurationUnlocked

-- ** Voiding 

-- | Void transaction in FA From transaction map and mark them as voided
voidTransactions :: Day -> (TransactionMap -> Maybe Text) -> [Filter TransactionMap] -> Handler Int
voidTransactions date commentFn criteria= do
  settings <- getsYesod appSettings
  let connectInfo = WFA.FAConnectInfo (appFAURL settings) (appFAUser settings) (appFAPassword settings)
  trans0 <- runDB $ selectList ([TransactionMapVoided ==. False] <>  criteria) []
  -- remove duplicate
  let tranMap = mapFromList $ map (fanl ( liftA3 (,,) transactionMapFaTransType
                                                      transactionMapFaTransNo
                                                      transactionMapVoided
                                                      . entityVal
                                        )
                                  ) trans0 :: Map _ (Entity TransactionMap)
  -- and remove GRN last (so that their corresponding invoice has already been deleted
      trans = uncurry (<>) $ partition ((/= ST_SUPPRECEIVE) . transactionMapFaTransType . entityVal)
                                       (toList tranMap)
  e <- runExceptT $ mapM (\tran -> voidFATransaction connectInfo date (commentFn . entityVal $ tran) tran) $ trans
  case e of
    Left err -> error (unpack err)
    Right _ -> return (length trans)

voidFATransaction :: WFA.FAConnectInfo -> Day -> Maybe Text -> Entity TransactionMap -> ExceptT Text Handler ()
voidFATransaction connectInfo vtDate comment (Entity __tId TransactionMap{..}) = do
  let vtTransNo = transactionMapFaTransNo
      vtTransType = transactionMapFaTransType
      vtComment = Just $ fromMaybe "Voided by Fames" comment
  ExceptT $ liftIO $ WFA.postVoid connectInfo WFA.VoidTransaction{..}
  -- mark the transaction as voided (all the rows)  not just the one matching the curren entity
  lift $  runDB $ updateWhere [ TransactionMapFaTransType ==. transactionMapFaTransType
                              , TransactionMapFaTransNo ==. transactionMapFaTransNo
                              ] [TransactionMapVoided =. True]
  return ()
    
