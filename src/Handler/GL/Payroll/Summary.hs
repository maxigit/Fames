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
import qualified GL.Payroll.Timesheet as TS
import qualified GL.Payroll.Report as TS
import Data.Maybe


-- * Forms
-- ** Type
data SummaryParam = SummaryParam
  { from :: Maybe Day
  , to :: Maybe Day
  , refRegex :: Maybe Text -- regulax expressions
  , dueDateMap :: Map Text Day -- override expected due date for a give timesheet
  }
-- ** Form
filterForm :: Maybe SummaryParam -> _ -- (FormResult SummaryParam, Widget)
filterForm paramM = let
  form = SummaryParam
                 <$> aopt dayField "From" (from <$> paramM)
                 <*> aopt dayField "To" (to <$> paramM)
                 <*> aopt textField "Reference" (refRegex <$> paramM)
                 <*> pure (mempty :: Map Text Day)
  in renderBootstrap3 BootstrapBasicForm form

-- * Handler
getGLPayrollSummaryR :: Handler Html
getGLPayrollSummaryR = renderMain Nothing

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
  resultM <- mapM processSummary paramM
  defaultLayout [whamlet|
    <div.well>
      <form #payroll-summary role=form method=post action=@{GLR GLPayrollSummaryR} enctype=#{encType}>
        ^{formW}
        <button.btn type="submit" >Search
    $maybe result <- resultM
      ^{result}
      |]

-- ** Process
processSummary :: SummaryParam -> Handler Widget
processSummary param =  do
  settings <- appSettings <$> getYesod
  timesheets <- loadTimesheet' param
  return [whamlet|
    <div.well>
      <ul>
      $forall ts <- timesheets
         <li> #{tshow $ ts}
          |]

-- * Misc
computeDueDate :: TS.Timesheet p e -> Day
computeDueDate ts = TS._periodStart ts 

withDueDate :: PayrollSettings -> SummaryParam -> TS.Timesheet p e ->  (TS.Timesheet p e, Day)
withDueDate psettings params ts = let
  key = invoiceRef psettings ts
  def = computeDueDate ts
  found = lookup key (dueDateMap params) 
  in (ts, fromMaybe def found)

setDueDateMap :: SummaryParam -> Handler SummaryParam
setDueDateMap param = do
  return param
-- * DBAccess
-- loadTimesheet' :: SummaryParam -> Handler [(TS.Timesheet p e , Day)]
loadTimesheet' param = do
  let filter =  from param <&> (TimesheetStart >=.)  ?:
                to param <&> (TimesheetStart <=. ) ?:
                refRegex param <&> (TimesheetReference <=. ) ?:
                []
  case filter of
    [] -> return []
    _ -> do
      modelEs <- loadTimesheets filter
      return $ map (\(e,ss, is) -> modelToTimesheetOpId e ss is) modelEs
