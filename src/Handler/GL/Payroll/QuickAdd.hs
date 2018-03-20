module Handler.GL.Payroll.QuickAdd
where
-- * Import
import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput, bootstrapSubmit,BootstrapSubmit(..))
import Handler.GL.Payroll.Common
import GL.Payroll.Settings
import qualified GL.Payroll.Timesheet as TS
import qualified GL.Payroll.Report as TS
import qualified GL.Payroll.Parser as TS
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import Data.Time (addGregorianMonthsClip)
import Data.Either
import Data.List.Split
import Control.Monad.Except (ExceptT(..), runExceptT)

-- * Quick Add
saveQuickAdd :: Text -> DocumentHash -> Handler (Either Text Widget)
saveQuickAdd  text key = do
  header <- headerFromSettings
  opFinder <- operatorFinderWithError
  payrollSettings <- getsYesod (appPayroll . appSettings)
  runExceptT $ do
    timesheets <- ExceptT . return $ splitTimesheet text

    ws <- forM timesheets $ \(ref, texts) -> do
      tEs <- lift $ loadTimesheets [TimesheetReference ==. ref]
      case tEs of
        [ (Entity key t, _, _) ] -> do
          let start = timesheetStart t
          timesheet <- ExceptT. return $ generateAdds header key start texts
          let period' = timesheetPeriod payrollSettings timesheet
          (model, shiftsFn, itemsFn) <- ExceptT . return $ timesheetOpIdToModel ref <$> timesheetEmployeeToOpId opFinder timesheet
          let shifts = shiftsFn key
              items = itemsFn key


          return [whamlet|
              <div.panel>
                <div.panel-header><h3>#{ref}
                <div.panel-body>
                   ^{displayTimesheet timesheet}
                   ^{displayEmployeeSummary (timesheetPayrooForSummary timesheet)}
                         |]

          -- insertMany_ shifts
          -- insertMany_ items
        _ -> ExceptT . return $ Left $ "No timesheet with reference '"  <> ref <> "''"

    return (mconcat ws)

-- | Split timesheet into multiple
-- A Section start with ">>"timesheetRef
splitTimesheet :: Text -> Either Text [(Text, [Text])]
splitTimesheet text = do
  let quickparse l = case stripPrefix ">>" l of
          Nothing -> Left l
          Just day -> Right day
  
      lines' = map quickparse (lines text)
      groups = split (whenElt  isRight) lines'
      mkSection (Right day:ss) = Right (day, lefts ss)
      mkSection ((Left l):_) = Left . pack $ "Section doesn't start with timesheet reference but '" <> unpack l <>  "' instead."
      mkSection _ = Left "Shoudn't not happen"
  sections <- traverse mkSection groups
  return sections


generateAdds :: [Text] -> _ -> Day -> [Text] -> Either Text (TS.Timesheet String TS.PayrooEmployee)
generateAdds header key start texts = do
  let toParse = tshow start : header <> texts
  ts <- parseFastTimesheet' toParse
  return ts

parseFastTimesheet' :: [Text] -> Either Text (TS.Timesheet String TS.PayrooEmployee)
parseFastTimesheet' texts = either (Left . pack) Right (TS.parseFastTimesheet (map unpack texts))
