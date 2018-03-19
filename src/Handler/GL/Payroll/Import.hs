module Handler.GL.Payroll.Import
( getGLPayrollImportR
, postGLPayrollImportR
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
import Data.List.NonEmpty (NonEmpty(..))
import Data.Time (addDays, addGregorianMonthsClip)
import qualified FA as FA
import Database.Persist.MySQL(unSqlBackendKey) -- , rawSql, Single(..))

-- * Type
data ImportParam = ImportParam
   { from :: Maybe Day
   , to :: Maybe Day
   , refRegex :: Maybe Text -- regulax expressions
   , toImport :: Set Int 
   } deriving Show

data Invoice = Invoice
  { trans :: FA.SuppTran
  , glTrans :: [FA.GlTran]
  , items :: [(FA.SuppInvoiceItem, FA.GrnItem, FA.GrnBatch)]
  , selected :: Bool
  , model :: Maybe (Timesheet, [PayrollShift], [PayrollItem])
  , key :: Maybe TimesheetId
  }

-- * Form
-- ** Type
-- ** Form
importForm :: Maybe ImportParam -> _ -- (FormResult SummaryParam, Widget)
importForm paramM = let
  form = ImportParam
                 <$> aopt dayField "From" (from <$> paramM)
                 <*> aopt dayField "To" (to <$> paramM)
                 <*> aopt textField "Reference" (refRegex <$> paramM)
                 <*> pure (mempty)
  in renderBootstrap3 BootstrapBasicForm form
-- * Handler
getGLPayrollImportR :: Handler Html
getGLPayrollImportR = do
  settings <- appSettings <$> getYesod
  let day = firstTaxWeek $ appPayroll settings
  renderMain (Just $ ImportParam (Just day)
                                 (Just $ addDays (-1) day)
                                 Nothing
                                 mempty)

renderMain :: Maybe ImportParam -> Handler Html
renderMain paramM = do
  (formW, encType) <- generateFormPost (importForm paramM)
  resultM <- mapM process paramM
  defaultLayout [whamlet|
    <form #payroll-summary role=form method=post action=@{GLR GLPayrollImportR} enctype=#{encType}>
      <div.well>
        ^{formW}
        <button.btn type="submit" >Search
      $maybe result <- resultM
        ^{result}
      |]

process :: ImportParam -> Handler Widget
process param = do
  invoices' <- runDB $ selectInvoice param
  invoices <- mapM (loadInvoice param) invoices'

  let main=  [whamlet|
    <div.well>
      <table.table.table-hover.table-border.table-striped>
          <tr>
            <th>Trans No
            <th>Reference
            <th>Supp Reference
            <th>Trans Date
            <th>Deductions
            <th>Costs
            <th>Shifts
            <th>Total
            <th>
        $forall invoice@( Invoice inv _ _ selected ts tidM ) <- invoices
          <tr>
            <td>
              <button.btn.btn-default data-toggle=modal data-target="##{invoiceModalRef invoice}" type=button>
                #{FA.suppTranTransNo inv}
            <td>#{FA.suppTranReference inv}
            <td>#{FA.suppTranSuppReference inv}
            <td>#{tshow $ FA.suppTranTranDate inv}
            <td> #{showAmountM (invoiceFADeductions invoice) (invoiceTimesheetDeductions invoice)}
            <td> #{showAmountM (invoiceFACosts invoice) (invoiceTimesheetCosts invoice)}
            <td> #{showAmountM (invoiceFAShiftCost invoice) (invoiceTimesheetShiftCost invoice)}
            <td> #{showAmountM (FA.suppTranOvAmount inv) (invoiceTimesheetTotal invoice)}
            <td>
              <input type=checkbox name="selected-#{tshow $ FA.suppTranTransNo inv}" :selected:checked>
            <td>
              $maybe tid' <- tidM
                $with tid <- unSqlBackendKey (unTimesheetKey tid')
                  <a href=@{GLR $GLPayrollViewR tid}>#{tshow tid}
              
                   |]
  let modals = concatMap modalInvoice invoices
  return $ main <> modals

showAmountM :: Double -> Maybe Double -> Html
showAmountM fa Nothing = toHtml $ tshow fa
showAmountM fa (Just ts) | fa == ts = [shamlet|<span.badge.badge-success>#{tshow fa}|]
                         | otherwise = [shamlet|
        <p>
          <span.bg-info.text-info>#{tshow fa}
        <p>
          <span.bg-danger.text-danger>#{tshow ts}
        |]

invoiceTimesheetCosts invoice = do
  (_, _, dacs) <- model invoice
  let costs = filter ((== Cost). payrollItemType) dacs
  return $ sum $ map payrollItemAmount costs

invoiceTimesheetDeductions invoice = do
  (_, _, dacs) <- model invoice
  let costs = filter ((== Deduction). payrollItemType) dacs
  return $ sum $ map payrollItemAmount costs

invoiceTimesheetShiftCost invoice = do
  (_, shifts, _) <- model invoice
  return $ sum $ map payrollShiftCost shifts

invoiceTimesheetTotal inv = liftA2 (+) (invoiceTimesheetShiftCost inv) (invoiceTimesheetCosts inv)


invoiceFACosts invoice = let
  costs = filter (isNothing . FA.glTranStockId) (glTrans invoice)
  in sum $ map FA.glTranAmount costs

invoiceFAShiftCost invoice = let
  costs = filter (isJust . FA.glTranStockId) (glTrans invoice)
  in sum $ map FA.glTranAmount costs

invoiceFADeductions invoice = 0
modalInvoice :: Invoice -> Widget
modalInvoice invoice =  let
  inv = trans invoice
  -- transW = [whamlet|
  --     <table>
  --       $forall tr <- glTrans inv
  --                  |]
  transH = entitiesToTable getHaskellName  (map (Entity (FA.GlTranKey 0)) (glTrans invoice))
  itemsW = [whamlet|
   <table.table.table-border.table-striped.table-hover>
     $forall (item, grn, batch) <- items invoice
       <tr>
         <td>#{tshow $ FA.grnBatchDeliveryDate batch}
         <td>#{FA.grnItemItemCode grn}
         <td>#{fromMaybe "" $ FA.grnBatchLocCode batch}
         <td>#{tshow $ FA.suppInvoiceItemQuantity item}
         <td>#{tshow $ FA.suppInvoiceItemQuantity item * FA.suppInvoiceItemUnitPrice item}
                   |]
  
  in  [whamlet|
                  <div.modal id="#{invoiceModalRef invoice}" role=dialog>
                    <div.modal-dialog style="width:80%">
                      <div.modal-content>
                        <div.modal-header>
                          <table.table><tr>
                            <td>#{FA.suppTranReference inv}
                            <td>#{FA.suppTranSuppReference inv}
                            <td>#{tshow $ FA.suppTranTranDate inv}
                            <td> #{tshow $ FA.suppTranOvAmount inv}
                        <div.modal-body>
                          #{transH}
                          ^{itemsW}
                        <div.modal-footer>
                          <button.btn.bnt-default data-dismiss=modal type=button>Close
                  |]
invoiceModalRef inv = "modal-" <> tshow (FA.suppTranTransNo (trans inv))
                                        
loadInvoice :: ImportParam -> Entity FA.SuppTran -> Handler Invoice
loadInvoice param (Entity invKey inv) = do
  -- find timesheet
  traceShowM ("loading invoice", invKey)
  let no = FA.suppTranTransNo inv
  timesheetM <- loadTimesheets [TimesheetReference ==. (FA.suppTranReference inv)]
  trans <- runDB $ selectList [ FA.GlTranType ==. fromEnum ST_SUPPINVOICE
                              , FA.GlTranTypeNo ==. no
                              -- , FA.GlTranStockId ==. Nothing
                              , FA.GlTranAmount >. 0
                              ] []

  items <- runDB $ selectList [ FA.SuppInvoiceItemSuppTransNo ==. Just no
                              , FA.SuppInvoiceItemSuppTransType ==. Just (fromEnum ST_SUPPINVOICE)
                              , FA.SuppInvoiceItemGrnItemId !=. Nothing
                              , FA.SuppInvoiceItemGrnItemId !=. Just 0
                              ]
                              []
  items'batch <- runDB $ forM items $ \iteme -> do
          traceShowM ("loading", FA.suppTranReference inv, "item#", entityKey iteme, FA.suppInvoiceItemGrnItemId (entityVal iteme))
          let grnId = FA.GrnItemKey $ fromJust  (FA.suppInvoiceItemGrnItemId (entityVal iteme))
          grn <- getJust grnId
          let batchId = FA.GrnBatchKey $ fromJust (FA.grnItemGrnBatchId grn)
          batch <- getJust batchId
          return (entityVal iteme, grn, batch)

  opF <- skuFinder


  let invoice0  = Invoice inv
                    (map entityVal trans)
                    items'batch
                    (FA.suppTranTransNo inv `elem` (toImport param))
                    Nothing
                    Nothing
      (ts, tkey) = case timesheetM of
                  [(Entity key ts, shifts, items)] -> ( Just (ts, map entityVal shifts, map entityVal items)
                                                      , Just key
                                                      )
                  _ -> (Just . dummyModel $ invoiceToModel opFinder invoice0, Nothing)
      opFinder name = maybe (error $ "Can't find operator with name " <> unpack name)
                            entityKey
                           (opF name <|> opF (drop 3 name))
  return  invoice0 {model=ts, key = tkey}

  
skuFinder :: Handler (Text -> Maybe (Entity Operator))
skuFinder = do
  info <- getEmployeeInfo
  let bySku = mapFromList [(faSKU emp, opE ) | (opE, emp) <- info ] :: Map Text (Entity Operator)
  traceShowM bySku
  return $ flip lookup bySku
  

postGLPayrollImportR :: Handler Html
postGLPayrollImportR =  do
  ((resp, formW), encType) <- runFormPost (importForm Nothing)
  case resp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess param' -> do
      param <- setParams param'
      renderMain (Just param)


setParams :: ImportParam -> Handler ImportParam
setParams param = do
  (pp, _) <- runRequestBody
  let selected = [ inv
                 | (key, val) <- pp
                 , Just inv <- return $ readMay =<<  stripPrefix "selected-" key
                 , val == "on"
                 ]
  traceShowM("PP", pp, selected)
  return param { toImport = setFromList selected }

-- * 
selectInvoice param = do
  invoices <- selectList ( Just (FA.SuppTranSupplierId ==. Just 27) ?:
                           Just (FA.SuppTranType ==. fromEnum ST_SUPPINVOICE) ?:
                           from param <&> (FA.SuppTranTranDate >=.) ?:
                           to param <&> (FA.SuppTranTranDate <=.) ?:
                           [])
                         [Asc FA.SuppTranTranDate, Asc FA.SuppTranReference]
  return invoices

-- * Converters
invoiceToModel :: (Text -> OperatorId)
               -> Invoice
               -> ( DocumentKeyId -> Timesheet
                  , TimesheetId -> [PayrollShift]
                  , TimesheetId -> [PayrollItem]
                  )
invoiceToModel opFinder invoice = let
  inv = trans invoice
  ref = FA.suppTranSuppReference inv
  start = FA.suppTranTranDate inv
  frequency = if 'M' `elem` ref then TS.Monthly else TS.Weekly
  timesheet' = TS.Timesheet [] start frequency []
  timesheet docKey = Timesheet ref
                               docKey
                               start
                               (TS.periodEnd timesheet') -- recursif !!
                               frequency
                               Pending

  x fns i = map ($ i) fns
  shifts = x $ map (mkShift opFinder) (items invoice)
  costs = x $ mapMaybe (mkCost opFinder) (glTrans invoice)

  in (timesheet, shifts, costs)

mkShift
  :: (Text -> Key Operator)
     -> (FA.SuppInvoiceItem, t, FA.GrnBatch)
     -> Key Timesheet
     -> PayrollShift
mkShift opFinder (item, grn, batch) tId = let
  duration = FA.suppInvoiceItemQuantity item
  cost = duration * FA.suppInvoiceItemUnitPrice item
  typ = case unpack $ fromJust $ FA.grnBatchLocCode batch of
              "LOST" -> TS.Holiday
              _ -> TS.Work
  
  operator = opFinder (FA.suppInvoiceItemStockId item)
  in PayrollShift tId duration cost operator (Just $ FA.grnBatchDeliveryDate batch) (tshow typ)
  
mkCost :: (Text -> Key Operator) -> FA.GlTran -> Maybe (Key Timesheet -> PayrollItem)
mkCost opFinder gl = do -- Maybe
  stockId <-  FA.glTranStockId gl
  let  amount = FA.glTranAmount gl
       account = FA.glTranAccount gl
       payee = case unpack account of
         "7006" -> "NI"
         "7007" -> "NEST"
         _ -> error $ "no payee for " <> show account
       operator = opFinder stockId
     
  return $ \tId -> PayrollItem  tId amount Cost operator payee account
 
dummyModel :: ( DocumentKeyId -> Timesheet
              , TimesheetId -> [PayrollShift]
              , TimesheetId -> [PayrollItem]
              ) -> (Timesheet, [PayrollShift], [PayrollItem])
dummyModel (t, ss, is)= let
  docKey = DocumentKeyKey' 0
  tId = TimesheetKey 0
  in (t docKey, ss tId, is tId)
