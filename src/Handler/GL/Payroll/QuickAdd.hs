{-# LANGUAGE ImplicitParams #-}
module Handler.GL.Payroll.QuickAdd
where
-- * Import
import Import
import Handler.GL.Payroll.Common
import GL.Payroll.Settings
import qualified GL.Payroll.Timesheet as TS
import qualified GL.Payroll.Parser as TS
import Data.Either
import Data.List.Split
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Text (strip)
import Database.Persist.Sql(unSqlBackendKey)

-- * Quick Add
saveQuickAdd :: Bool -> Text -> DocumentHash -> Handler (Either Text Widget)
saveQuickAdd  save text __key  = do
  header <- headerFromSettings
  opFinder <- operatorFinderWithError
  operatorMap <- allOperators
  payrollSettings <- getsYesod (appPayroll . appSettings)
  viewPayrollAmountPermissions' <- viewPayrollAmountPermissions
  viewPayrollDurationPermissions' <- viewPayrollAmountPermissions
  let ?viewPayrollAmountPermissions = viewPayrollAmountPermissions'
      ?viewPayrollDurationPermissions = viewPayrollDurationPermissions'
  let views_ = case toList (views payrollSettings) of
                 [] -> [[]]
                 vs -> vs
  runExceptT $ do
    timesheets <- ExceptT . return $ splitTimesheet text

    ws <- forM timesheets $ \(ref, texts) -> do
      tEs <- lift $ loadTimesheets [TimesheetReference ==. ref] [] []
      case tEs of
        [ (oldE@(Entity key oldT), oldShifts, oldItems) ] -> do
          let start = timesheetStart oldT
              frequency = timesheetFrequency oldT
          let tsOId = modelToTimesheetOpId oldE oldShifts oldItems
              oldTts' = map (\(op, _) -> maybe (tshow op) operatorNickname (lookup op operatorMap)) tsOId
          timesheet <- ExceptT. return $ generateAdds header key frequency start texts
          (__model, shiftsFn, itemsFn) <- ExceptT . return $ timesheetOpIdToModel ref <$> timesheetEmployeeToOpId opFinder timesheet
          let shifts = shiftsFn key
              items = itemsFn key
              shifts0 = map (Entity (PayrollShiftKey 0)) shifts
              items0 = map (Entity (PayrollItemKey 0)) items
              mergedOId = modelToTimesheetOpId oldE (oldShifts <> shifts0) (oldItems <> items0)
              mergedTts' = map (\(op, _) -> maybe (tshow op) operatorNickname (lookup op operatorMap)) mergedOId

          when save $ lift $ do
            runDB $ do
                        insertMany_ shifts
                        insertMany_ items
            pushLinks ("View Timesheet " <> ref)
                        (GLR $ GLPayrollViewR (unSqlBackendKey $ unTimesheetKey key ))
                        []
  
          return [whamlet|
              <div.panel.panel-success>
                <div.panel-heading><h3>#{ref}
                <div.panel-body>
                   <div.well>
                     <h3> Existing
                     ^{displayEmployeeSummary oldTts'}
                   <div.well>
                     <h3> Extra
                     ^{displayTimesheet timesheet}
                     ^{displayEmployeeSummary (timesheetPayrooForSummary timesheet)}
                   <div.well>
                     <h3> Final
                     $forall cols <- views_
                      ^{displayEmployeeSummary' (columnWeightFromList cols) mergedTts'}
                         |]
        _ -> ExceptT . return $ Left $ "No timesheet with reference '"  <> ref <> "''"

    return (mconcat ws)

-- | Split timesheet into multiple
-- A Section start with ">>"timesheetRef
splitTimesheet :: Text -> Either Text [(Text, [Text])]
splitTimesheet text = do
  let quickparse l = case stripPrefix ">>" l of
          Nothing -> Left l
          Just ref -> Right ref
  
      lines' = map (quickparse . strip) (lines text)
      groups = filter (not . null) $ split (keepDelimsL $ whenElt  isRight) lines'
      mkSection (Right ref:ss) = Right (ref, lefts ss)
      mkSection ((Left l):_) = Left . pack $ "Section doesn't start with timesheet reference but '" <> unpack l <>  "' instead."
      mkSection _ = Left "Shoudn't not happen"
  sections <- traverse mkSection groups
  return sections


generateAdds :: [Text] -> _ -> PayrollFrequency -> Day -> [Text] -> Either Text (TS.Timesheet String TS.PayrooEmployee)
generateAdds header __key frequency start texts = do
  let toParse = (tshow frequency <> " " <>  tshow start) : header <> texts
  ts <- parseFastTimesheet' toParse
  return ts

parseFastTimesheet' :: [Text] -> Either Text (TS.Timesheet String TS.PayrooEmployee)
parseFastTimesheet' texts = either (Left . pack) Right (TS.parseFastTimesheet (map unpack texts))
