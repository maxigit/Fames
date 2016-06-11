module Handler.FA.Def where
import Import
import FA


getFAAreasDefR :: Handler Html 
getFAAreasDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Area]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAAttachmentsDefR :: Handler Html 
getFAAttachmentsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Attachment]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAAuditTrailDefR :: Handler Html 
getFAAuditTrailDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.AuditTrail]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFABankAccountsDefR :: Handler Html 
getFABankAccountsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.BankAccount]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFABankTransDefR :: Handler Html 
getFABankTransDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.BankTran]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFABomDefR :: Handler Html 
getFABomDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Bom]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFABudgetTransDefR :: Handler Html 
getFABudgetTransDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.BudgetTran]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAChartClassDefR :: Handler Html 
getFAChartClassDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.ChartClas]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAChartMasterDefR :: Handler Html 
getFAChartMasterDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.ChartMaster]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAChartTypesDefR :: Handler Html 
getFAChartTypesDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.ChartType]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFACommentsDefR :: Handler Html 
getFACommentsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Comment]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFACreditStatusDefR :: Handler Html 
getFACreditStatusDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.CreditStatu]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFACrmCategoriesDefR :: Handler Html 
getFACrmCategoriesDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.CrmCategorie]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFACrmContactsDefR :: Handler Html 
getFACrmContactsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.CrmContact]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFACrmPersonsDefR :: Handler Html 
getFACrmPersonsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.CrmPerson]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFACurrenciesDefR :: Handler Html 
getFACurrenciesDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Currencie]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFACustAllocationsDefR :: Handler Html 
getFACustAllocationsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.CustAllocation]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFACustBranchDefR :: Handler Html 
getFACustBranchDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.CustBranch]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFADebtorsMasterDefR :: Handler Html 
getFADebtorsMasterDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.DebtorsMaster]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFADebtorTransDefR :: Handler Html 
getFADebtorTransDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.DebtorTran]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFADebtorTransDetailsDefR :: Handler Html 
getFADebtorTransDetailsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.DebtorTransDetail]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFADimensionsDefR :: Handler Html 
getFADimensionsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Dimension]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAExchangeRatesDefR :: Handler Html 
getFAExchangeRatesDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.ExchangeRate]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAFiscalYearDefR :: Handler Html 
getFAFiscalYearDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.FiscalYear]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAGlTransDefR :: Handler Html 
getFAGlTransDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.GlTran]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAGrnBatchDefR :: Handler Html 
getFAGrnBatchDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.GrnBatch]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAGrnItemsDefR :: Handler Html 
getFAGrnItemsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.GrnItem]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAGroupsDefR :: Handler Html 
getFAGroupsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Group]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAItemCodesDefR :: Handler Html 
getFAItemCodesDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.ItemCode]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAItemTaxTypesDefR :: Handler Html 
getFAItemTaxTypesDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.ItemTaxType]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAItemTaxTypeExemptionsDefR :: Handler Html 
getFAItemTaxTypeExemptionsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.ItemTaxTypeExemption]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAItemUnitsDefR :: Handler Html 
getFAItemUnitsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.ItemUnit]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFALocationsDefR :: Handler Html 
getFALocationsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Location]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFALocStockDefR :: Handler Html 
getFALocStockDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.LocStock]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAMovementTypesDefR :: Handler Html 
getFAMovementTypesDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.MovementType]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAPaymentTermsDefR :: Handler Html 
getFAPaymentTermsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.PaymentTerm]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAPricesDefR :: Handler Html 
getFAPricesDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Price]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAPrintersDefR :: Handler Html 
getFAPrintersDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Printer]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAPrintProfilesDefR :: Handler Html 
getFAPrintProfilesDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.PrintProfile]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAPurchDataDefR :: Handler Html 
getFAPurchDataDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.PurchData]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAPurchOrdersDefR :: Handler Html 
getFAPurchOrdersDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.PurchOrder]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAPurchOrderDetailsDefR :: Handler Html 
getFAPurchOrderDetailsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.PurchOrderDetail]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAQuickEntriesDefR :: Handler Html 
getFAQuickEntriesDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.QuickEntrie]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAQuickEntryLinesDefR :: Handler Html 
getFAQuickEntryLinesDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.QuickEntryLine]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFARecurrentInvoicesDefR :: Handler Html 
getFARecurrentInvoicesDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.RecurrentInvoice]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFARefsDefR :: Handler Html 
getFARefsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Ref]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASalesmanDefR :: Handler Html 
getFASalesmanDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Salesman]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASalesOrdersDefR :: Handler Html 
getFASalesOrdersDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.SalesOrder]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASalesOrderDetailsDefR :: Handler Html 
getFASalesOrderDetailsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.SalesOrderDetail]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASalesPosDefR :: Handler Html 
getFASalesPosDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.SalesPo]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASalesTypesDefR :: Handler Html 
getFASalesTypesDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.SalesType]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASecurityRolesDefR :: Handler Html 
getFASecurityRolesDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.SecurityRole]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAShippersDefR :: Handler Html 
getFAShippersDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Shipper]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASqlTrailDefR :: Handler Html 
getFASqlTrailDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.SqlTrail]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAStockCategoryDefR :: Handler Html 
getFAStockCategoryDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.StockCategory]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAStockMasterDefR :: Handler Html 
getFAStockMasterDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.StockMaster]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAStockMovesDefR :: Handler Html 
getFAStockMovesDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.StockMove]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASuppliersDefR :: Handler Html 
getFASuppliersDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Supplier]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASuppAllocationsDefR :: Handler Html 
getFASuppAllocationsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.SuppAllocation]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASuppInvoiceItemsDefR :: Handler Html 
getFASuppInvoiceItemsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.SuppInvoiceItem]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASuppTransDefR :: Handler Html 
getFASuppTransDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.SuppTran]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASysPrefsDefR :: Handler Html 
getFASysPrefsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.SysPref]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASysTypesDefR :: Handler Html 
getFASysTypesDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.SysType]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFATagsDefR :: Handler Html 
getFATagsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Tag]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFATagAssociationsDefR :: Handler Html 
getFATagAssociationsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.TagAssociation]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFATaxGroupsDefR :: Handler Html 
getFATaxGroupsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.TaxGroup]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFATaxGroupItemsDefR :: Handler Html 
getFATaxGroupItemsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.TaxGroupItem]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFATaxTypesDefR :: Handler Html 
getFATaxTypesDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.TaxType]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFATransTaxDetailsDefR :: Handler Html 
getFATransTaxDetailsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.TransTaxDetail]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAUseronlineDefR :: Handler Html 
getFAUseronlineDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Useronline]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAUsersDefR :: Handler Html 
getFAUsersDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.User]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAVoidedDefR :: Handler Html 
getFAVoidedDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Voided]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAWorkcentresDefR :: Handler Html 
getFAWorkcentresDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Workcentre]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAWorkordersDefR :: Handler Html 
getFAWorkordersDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Workorder]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAWoIssuesDefR :: Handler Html 
getFAWoIssuesDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.WoIssue]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAWoIssueItemsDefR :: Handler Html 
getFAWoIssueItemsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.WoIssueItem]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAWoManufactureDefR :: Handler Html 
getFAWoManufactureDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.WoManufacture]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAWoRequirementsDefR :: Handler Html 
getFAWoRequirementsDefR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.WoRequirement]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)
