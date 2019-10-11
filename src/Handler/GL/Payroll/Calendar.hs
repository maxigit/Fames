{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-} -- TODO remove
-- | Displays a calendar view of the selected shifts
module Handler.GL.Payroll.Calendar
( getGLPayrollCalendarR
, postGLPayrollCalendarR
)
where
import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Handler.GL.Payroll.Common
import GL.Payroll.Settings
import GL.Utils
import qualified GL.Payroll.Timesheet as TS
import Data.Time (addGregorianMonthsClip)
import Lens.Micro ((^.))
-- * Forms
-- ** Type
data CalendarParam = CalendarParam
  { from :: Maybe Day
  , to :: Maybe Day
  , reference :: Maybe FilterExpression 
  , frequency :: Maybe PayrollFrequency 
  , shiftType :: Maybe TS.ShiftType
  }
-- ** Form
filterForm :: Maybe CalendarParam -> _ -- (FormResult CalendarParam, Widget)
filterForm paramM = let
  form = CalendarParam
                 <$> aopt dayField "From" (from <$> paramM)
                 <*> aopt dayField "To" (to <$> paramM)
                 <*> aopt filterEField "Reference" (reference <$> paramM)
                 <*> aopt (selectField optionsEnum)  "Frequency" (frequency <$> paramM)
                 <*> aopt (selectField optionsEnum)  "Type" (shiftType <$> paramM)
  in renderBootstrap3 BootstrapBasicForm form

-- * Handler
{-# NOINLINE getGLPayrollCalendarR #-}
getGLPayrollCalendarR :: Handler Html
getGLPayrollCalendarR = do
  today <- todayH
  let lastMonth = addGregorianMonthsClip (-1) today
  renderMain (Just $ CalendarParam (Just lastMonth)
                                   (Just today)
                                   Nothing
                                   Nothing
                                   Nothing
             )

{-# NOINLINE postGLPayrollCalendarR #-}
postGLPayrollCalendarR :: Handler Html
postGLPayrollCalendarR = do
  ((resp, __formW), __encType) <- runFormPost (filterForm Nothing)
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param -> do
      renderMain (Just $ param)


-- ** Rendering
renderMain :: Maybe CalendarParam -> Handler Html
renderMain paramM = do
  (formW, encType) <- generateFormPost (filterForm paramM)
  resultM <- mapM processCalendar paramM
  defaultLayout [whamlet|
    <form #payroll-calendar role=form method=post action=@{GLR GLPayrollCalendarR} enctype=#{encType}>
      <div.well>
        ^{formW}
        <button.btn type="submit" >Search
      $maybe result <- resultM
        ^{result}
      |]

-- ** Process
processCalendar :: CalendarParam -> Handler Widget
processCalendar param =  do
  timesheets <- loadTimesheet' param
  viewPayrollDurationPermissions' <- viewPayrollDurationPermissions
  let ?viewPayrollDurationPermissions = viewPayrollDurationPermissions'
  -- group all shifts
  let allShifts = filter isShiftUnlocked $ concatMap TS._shifts timesheets
      days = map (^. TS.day) allShifts
  case days of
    [] -> setWarning "No timesheet selected. Please amend the range" >> return ""
    (d:ds) ->
      do
              let minDay0 = minimum (ncons d ds)
                  maxDay0 = maximum (ncons d ds)
                  firstActive = fromMaybe minDay0 (from param)
                  lastActive = fromMaybe maxDay0 (to param)
                  minDay = previousWeekDay Monday (min minDay0 firstActive)
                  maxDay = nextWeekDay Saturday (max maxDay0 lastActive)
              timedShifts <- addTimedFromMop firstActive lastActive allShifts
              return $ displayCalendar True minDay maxDay firstActive lastActive timedShifts

-- * DBAccess
loadTimesheet' :: CalendarParam -> Handler [TS.Timesheet Text Text]
loadTimesheet' param = do
  -- TODO factorize
  let filter =  (from param <&> (TimesheetStart >=.))  ?:
                (to param <&> (TimesheetStart <=. )) ?:
                (frequency param <&> (TimesheetFrequency ==. )) ?:
                (filterE id TimesheetReference (reference param))
      shiftCriteria = (shiftType param <&> ((PayrollShiftType ==.) . tshow)) ?: []
  case filter of
    [] -> return []
    _ -> do
      modelEs <- loadTimesheets filter shiftCriteria []
      let timesheetOpIds = map (\(e,ss, is) -> modelToTimesheetOpId e ss is) modelEs
      ts' <- mapM timesheetOpIdToTextH timesheetOpIds
      case sequence ts' of
        Left e -> error $ unpack e
        Right s -> return s
