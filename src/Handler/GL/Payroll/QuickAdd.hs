{-# LANGUAGE ImplicitParams, NamedFieldPuns #-}
module Handler.GL.Payroll.QuickAdd
where
import Import
import Handler.GL.Payroll.Common
import GL.Payroll.Settings
import qualified GL.Payroll.Timesheet as TS
import GL.Payroll.Timesheet (EmployeeSummary(..))
import GL.Payroll.Report(paymentSummary)
import qualified GL.Payroll.Parser as TS
import Data.Either
import Data.List.Split
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Text (strip)
import Database.Persist.Sql(unSqlBackendKey)
import Data.Align
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NE

-- * Quick Add 
saveQuickAdd :: Bool -> Text -> Maybe ([ TS.EmployeeSummary Text Text ], TS.EmployeeSummary Text ()) -> DocumentHash -> Handler (Either Text Widget)
saveQuickAdd  save text expectedSummariesM __key  = do
  header <- headerFromSettings
  opFinder <- operatorFinderWithError
  operatorMap <- allOperators
  payrollSettings <- getsYesod (appPayroll . appSettings)
  viewPayrollAmountPermissions' <- viewPayrollAmountPermissions
  viewPayrollDurationPermissions' <- viewPayrollAmountPermissions
  let ?viewPayrollAmountPermissions = viewPayrollAmountPermissions'
      ?viewPayrollDurationPermissions = viewPayrollDurationPermissions'
  let views_ = toList (views payrollSettings)
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
              -- Check that the final summay matches the expected one.
              -- This append when uploading payroo when on top of reading the DAC
              -- we can check that all totals add up we can be different if we
              -- for example have forgotten to input holidays or bonus in PAYROO
              diffWithExpecteds = case expectedSummariesM of 
                                          Just expectedSummaries | not save -> case checkCostSummaries mergedTts' expectedSummaries of
                                                                   [] -> Nothing -- Ok
                                                                   diffs -> Just diffs
                                          _ -> Nothing

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
                     $forall (cols, orders) <- views_
                      ^{displayEmployeeSummary' orders cols mergedTts'}
              $maybe diff <-  diffWithExpecteds
                 <div.panel.panel-warning>
                   <div.panel-heading>
                     <h3> there are some difference with PAYROO
                   <div.panel-body>
                      <h4> Difference (Expected - actual)
                      $with (cols, colnames) <- employeeSummaryColumns diff
                            ^{employeeSummaryTable cols colnames (employeeSummaryRows diff) }
                      $with (cols, colnames) <- employeeSummaryColumns diff
                        $with Just (expectedSummaries, total) <- expectedSummariesM
                          <h4> Expected (from PAYROO)
                            ^{employeeSummaryTable cols colnames (employeeSummaryRows expectedSummaries) }
                            ^{employeeSummaryTable cols colnames (employeeSummaryRows [fmap (const "Grand Total") total]) }
                      $with summaries <- paymentSummary mergedTts'
                            $with (cols, colnames) <- employeeSummaryColumns summaries
                                  <h4> Computed (PAYROO + Actual Timesheet)
                                      ^{employeeSummaryTable cols colnames (employeeSummaryRows summaries) }
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


-- | Check that total costs between the time sheet and expected summaries adds up
-- and return the differences if needs to be
checkCostSummaries :: TS.Timesheet Text Text -> ([ TS.EmployeeSummary Text Text ], TS.EmployeeSummary Text ()) -> [TS.EmployeeSummary Text Text]
checkCostSummaries timesheet (expectedSummaries, total) = let
   summaryMap = paymentSummary timesheet
   diffs = alignWith checkSummary (mkMap summaryMap) (mkMap expectedSummaries)
   timesheetSummary = sconcat $ map (fmap  $ const ()) $ NE.fromList $ toList summaryMap
   checkTotal = case checkSummary $ These timesheetSummary total of
                   Nothing -> []
                   Just diff -> [ fmap (const "Total - diff") diff 
                                , fmap (const "Total - Expected") total
                                ]
   in (toList $ Map.mapMaybe id diffs)
      <> checkTotal
      
   where mkMap :: [ TS.EmployeeSummary Text Text ] -> Map Text (TS.EmployeeSummary Text Text)
         mkMap summaries = mapFromList [ (TS._sumEmployee s, s)
                                       | s <- summaries
                                       ]
         -- Real summary on the left, expected summary on the right
         checkSummary :: Eq e => These (TS.EmployeeSummary Text e) (TS.EmployeeSummary Text e) -> Maybe _
         checkSummary famesEitherPayroo = let
            nullSummary = EmployeeSummary (_sumEmployee fames)
                                          0
                                          0
                                          0
                                          0
                                          mempty
                                          mempty
                                          mempty
                                          mempty
            -- For This and That, instead of returning just the existing summary
            -- we need to use  `diff` which set small values to 0 (otherwise the == between Double
            -- might report false negative
            (fames, payroo) = these (,nullSummary) (nullSummary,) (,) famesEitherPayroo
            diff = EmployeeSummary (_sumEmployee fames)
                                          (sub _finalPayment)
                                          (sub _totalCost)
                                          (sub _net)
                                          (sub _gross)
                                          (subm _deductions)
                                          (subm _netDeductions)
                                          (subm _costs)
                                          mempty
            sub f = case f fames - f payroo of
                         r | (abs r) < 1e-4 -> 0
                         r                -> r
            subm f = Map.filter ((>=1e-4) . abs) $ alignWith subThese (f fames) (f payroo)
            subThese (That a) = a
            subThese (This b) = -b
            subThese (These a b) = case a - b of
                                    r | abs r < 1e-4 -> 0
                                    r                -> r
            in if diff == nullSummary
               then Nothing
               else Just diff
    
