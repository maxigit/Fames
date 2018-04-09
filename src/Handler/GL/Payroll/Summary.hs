{-# LANGUAGE ImplicitParams #-}
-- | Displays a summary of all the timesheet
module Handler.GL.Payroll.Summary
( getGLPayrollSummaryR
, postGLPayrollSummaryR
)
where
-- * Import
import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput, bootstrapSubmit,BootstrapSubmit(..))
import Handler.GL.Payroll.Common
import GL.Payroll.Settings
import GL.Utils
import qualified GL.Payroll.Timesheet as TS
import qualified GL.Payroll.Report as TS
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import Data.Time (addGregorianMonthsClip, addDays)
import Locker

-- * Forms
-- ** Type
data SummaryParam = SummaryParam
  { from :: Maybe Day
  , to :: Maybe Day
  , reference :: Maybe FilterExpression 
  , frequency :: Maybe PayrollFrequency 
  , dueDateMap :: Map Text Day -- override expected due date for a give timesheet
  }
-- ** Form
filterForm :: Maybe SummaryParam -> _ -- (FormResult SummaryParam, Widget)
filterForm paramM = let
  form = SummaryParam
                 <$> aopt dayField "From" (from <$> paramM)
                 <*> aopt dayField "To" (to <$> paramM)
                 <*> aopt filterEField "Reference" (reference <$> paramM)
                 <*> aopt (selectField optionsEnum)  "Frequency" (frequency <$> paramM)
                 <*> pure (mempty :: Map Text Day)
  in renderBootstrap3 BootstrapBasicForm form

-- * Handler
getGLPayrollSummaryR :: Handler Html
getGLPayrollSummaryR = do
  today <- utctDay <$> liftIO getCurrentTime
  let lastMonth = addGregorianMonthsClip (-1) today
  renderMain (Just $ SummaryParam (Just lastMonth)
                                 (Just today)
                                 Nothing
                                 Nothing
                                 mempty)

postGLPayrollSummaryR :: Handler Html
postGLPayrollSummaryR = do
  ((resp, formW), encType) <- runFormPost (filterForm Nothing)
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param' -> do
      param <- setDueDateMap param'
      renderMain (Just $ param)


-- ** Rendering
renderMain :: Maybe SummaryParam -> Handler Html
renderMain paramM = do
  (formW, encType) <- generateFormPost (filterForm paramM)
  let ?viewPayrollAmountPermissions = (const Forbidden)
  resultM <- mapM processSummary paramM
  defaultLayout [whamlet|
    <form #payroll-summary role=form method=post action=@{GLR GLPayrollSummaryR} enctype=#{encType}>
      <div.well>
        ^{formW}
        <button.btn type="submit" >Search
      $maybe result <- resultM
        ^{result}
      |]

-- ** Process
processSummary :: (?viewPayrollAmountPermissions :: (Text -> Granted))
               => SummaryParam -> Handler Widget
processSummary param =  do
  settings <- appSettings <$> getYesod
  timesheets <- loadTimesheet' param
  let psettings = appPayroll settings
      timesheetDues  = map (withDueDate psettings param) timesheets
      timesheetName ts = invoiceRef psettings ts <> " -- <" <> tshow (TS._periodStart ts) <> "> - <" <> tshow (TS.periodEnd ts) <> ">"
      withRef = (\(ts, d) -> ((ts, timesheetName ts), d)) <$> timesheetDues
      groups = makeSummaries withRef
      totals = [total | (_,_, total) <- groups]
      (cols, colnames) = employeeSummaryColumns totals
  return [whamlet|
   $forall (day, ss, total) <- groups
     <div.panel.panel-info>
       <div.panel-heading> #{tshow day}
       <div.panel-body>
           ^{employeeSummaryTable' day cols colnames (employeeSummaryRows ss)}
          |]

-- | Modify columns and row to add due date boxes
employeeSummaryTable' day cols0 colnames rows0 = let
  cols = cols0 ++ [(Nothing, "Due Date" )]
  rows = map addDueDate rows0
  -- for total row, don't change anything
  addDueDate (fn, ["total"])= (fn, ["total"]) 
  addDueDate (fn, klass)= (fn', klass) where
      fn' (Nothing, "Due Date") =  let
        Just (ref, _ ) = fn (Nothing, "Employee")
        in Just ([shamlet|
                         <input name="#{ref}" type="date" value="#{tshow day}">
                         |], [])
      fn' col = fn col
  in employeeSummaryTable cols colnames rows

-- * Misc
computeDueDate :: PayrollSettings -> TS.Timesheet p e -> Day
computeDueDate settings ts = let
  start = TS.periodEnd ts
  (_year', _month', day') = toGregorian $ addDays 6 (firstTaxWeek settings) -- get end period
  in calculateDate (DayOfMonth day' day') start

withDueDate :: PayrollSettings -> SummaryParam -> TS.Timesheet p e ->  (TS.Timesheet p e, Day)
withDueDate psettings params ts = let
  key = invoiceRef psettings ts
  def = computeDueDate psettings ts
  found = lookup key (dueDateMap params) 
  in (ts, fromMaybe def found)

setDueDateMap :: SummaryParam -> Handler SummaryParam
setDueDateMap param = do
  (pp, _) <- runRequestBody
  let dueMap = mapFromList [(takeWhile (/=' ') ref, day)
                           | (ref, dayS) <- pp
                           , Just day <- return $ readMay dayS
                           ]
  return param { dueDateMap = dueMap }


-- Groups shifts by due date and summarize each one, as well as 
makeSummaries :: [((TS.Timesheet Text Text, Text), Day)] -> [ (Day
                                                      , [TS.EmployeeSummary Text Text]
                                                      , TS.EmployeeSummary Text Text)]
makeSummaries timesheets = let
  byDue = fst <$$> TS.groupBy snd timesheets
  summaries = [ (day, summaries, total)
              | (day, tss) <- mapToList  byDue
              , let summaries@(s:ss) = [ (sconcat $ s :| ss ) {TS._sumEmployee = ref}
                                       |  (ts, ref) <- sortWith (TS._periodStart . fst) tss
                                       , let (s:ss) = TS.paymentSummary ts
                                       ]
              , let total =  sconcat (s :| ss)
              ]
  in summaries
  
-- * DBAccess
loadTimesheet' :: SummaryParam -> Handler [TS.Timesheet Text Text]
loadTimesheet' param = do
  let filter =  from param <&> (TimesheetStart >=.)  ?:
                to param <&> (TimesheetStart <=. ) ?:
                frequency param <&> (TimesheetFrequency ==. ) ?:
                (filterE id TimesheetReference (reference param))
  case filter of
    [] -> return []
    _ -> do
      modelEs <- loadTimesheets filter
      let timesheetOpIds = map (\(e,ss, is) -> modelToTimesheetOpId e ss is) modelEs
      ts' <- mapM timesheetOpIdToTextH timesheetOpIds
      case sequence ts' of
        Left e -> error $ unpack e
        Right s -> return s
