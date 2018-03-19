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
import Data.Time (addGregorianMonthsClip)
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
  , timesheet :: Maybe Timesheet
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
  renderMain (Just $ ImportParam (Just . firstTaxWeek $ appPayroll settings)
                                 Nothing
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

  return $ [whamlet|
    <div.well>
      <table.table.table-hover.table-border.table-striped>
          <tr>
            <th>Trans No
            <th>Reference
            <th>Supp Reference
            <th>Trans Date
            <th>Amount
            <th>Timesheet
            <th>
        $forall ( Invoice inv _ _ selected tsM ) <- invoices
          <tr>
            <td>#{FA.suppTranTransNo inv}
            <td>#{FA.suppTranReference inv}
            <td>#{FA.suppTranSuppReference inv}
            <td>#{tshow $ FA.suppTranTranDate inv}
            <td>#{tshow $ FA.suppTranOvAmount inv}
            <td>
              <input type=checkbox name="selected-#{tshow $ FA.suppTranTransNo inv}" :selected:checked>
            <td>
              $maybe ts <- tsM
                #{timesheetReference ts}
                   |]

loadInvoice param (Entity _ inv) = do
  -- find timesheet
  let no = FA.suppTranTransNo inv
  timesheet <- runDB$ selectFirst [TimesheetReference ==. (FA.suppTranReference inv)] []
  trans <- runDB $ selectList [ FA.GlTranType ==. fromEnum ST_SUPPINVOICE
                              , FA.GlTranTypeNo ==. no
                              , FA.GlTranStockId ==. Nothing
                              ] []

  items <- runDB $ selectList [ FA.SuppInvoiceItemSuppTransNo ==. Just no
                              , FA.SuppInvoiceItemSuppTransType ==. Just (fromEnum ST_SUPPINVOICE)
                              , FA.SuppInvoiceItemGrnItemId !=. Nothing
                              ]
                              []
  items'batch <- runDB $ forM items $ \iteme -> do
          let grnId = FA.GrnItemKey $ fromJust  (FA.suppInvoiceItemGrnItemId (entityVal iteme))
          grn <- getJust grnId
          let batchId = FA.GrnBatchKey $ fromJust (FA.grnItemGrnBatchId grn)
          batch <- getJust batchId
          return (entityVal iteme, grn, batch)
        

  return $ Invoice inv
                   (map entityVal trans)
                   items'batch
                   (FA.suppTranTransNo inv `elem` (toImport param))
                   (entityVal <$> timesheet)

  
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
