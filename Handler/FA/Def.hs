module Handler.FA.Def where
import Import
import FA


getFAAreasR :: Handler Html 
getFAAreasR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Area]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAAttachmentsR :: Handler Html 
getFAAttachmentsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Attachment]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAAuditTrailR :: Handler Html 
getFAAuditTrailR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.AuditTrail]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFABankAccountsR :: Handler Html 
getFABankAccountsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.BankAccount]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFABankTransR :: Handler Html 
getFABankTransR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.BankTran]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFABomR :: Handler Html 
getFABomR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Bom]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFABudgetTransR :: Handler Html 
getFABudgetTransR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.BudgetTran]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAChartClassR :: Handler Html 
getFAChartClassR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.ChartClas]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAChartMasterR :: Handler Html 
getFAChartMasterR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.ChartMaster]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAChartTypesR :: Handler Html 
getFAChartTypesR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.ChartType]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFACommentsR :: Handler Html 
getFACommentsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Comment]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFACreditStatusR :: Handler Html 
getFACreditStatusR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.CreditStatu]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFACrmCategoriesR :: Handler Html 
getFACrmCategoriesR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.CrmCategorie]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFACrmContactsR :: Handler Html 
getFACrmContactsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.CrmContact]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFACrmPersonsR :: Handler Html 
getFACrmPersonsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.CrmPerson]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFACurrenciesR :: Handler Html 
getFACurrenciesR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Currencie]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFACustAllocationsR :: Handler Html 
getFACustAllocationsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.CustAllocation]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFACustBranchR :: Handler Html 
getFACustBranchR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.CustBranch]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFADebtorsMasterR :: Handler Html 
getFADebtorsMasterR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.DebtorsMaster]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFADebtorTransR :: Handler Html 
getFADebtorTransR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.DebtorTran]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFADebtorTransDetailsR :: Handler Html 
getFADebtorTransDetailsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.DebtorTransDetail]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFADimensionsR :: Handler Html 
getFADimensionsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Dimension]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAExchangeRatesR :: Handler Html 
getFAExchangeRatesR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.ExchangeRate]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAFiscalYearR :: Handler Html 
getFAFiscalYearR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.FiscalYear]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAGlTransR :: Handler Html 
getFAGlTransR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.GlTran]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAGrnBatchR :: Handler Html 
getFAGrnBatchR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.GrnBatch]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAGrnItemsR :: Handler Html 
getFAGrnItemsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.GrnItem]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAGroupsR :: Handler Html 
getFAGroupsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Group]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAItemCodesR :: Handler Html 
getFAItemCodesR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.ItemCode]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAItemTaxTypesR :: Handler Html 
getFAItemTaxTypesR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.ItemTaxType]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAItemTaxTypeExemptionsR :: Handler Html 
getFAItemTaxTypeExemptionsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.ItemTaxTypeExemption]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAItemUnitsR :: Handler Html 
getFAItemUnitsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.ItemUnit]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFALocationsR :: Handler Html 
getFALocationsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Location]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFALocStockR :: Handler Html 
getFALocStockR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.LocStock]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAMovementTypesR :: Handler Html 
getFAMovementTypesR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.MovementType]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAPaymentTermsR :: Handler Html 
getFAPaymentTermsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.PaymentTerm]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAPricesR :: Handler Html 
getFAPricesR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Price]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAPrintersR :: Handler Html 
getFAPrintersR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Printer]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAPrintProfilesR :: Handler Html 
getFAPrintProfilesR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.PrintProfile]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAPurchDataR :: Handler Html 
getFAPurchDataR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.PurchData]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAPurchOrdersR :: Handler Html 
getFAPurchOrdersR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.PurchOrder]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAPurchOrderDetailsR :: Handler Html 
getFAPurchOrderDetailsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.PurchOrderDetail]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAQuickEntriesR :: Handler Html 
getFAQuickEntriesR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.QuickEntrie]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAQuickEntryLinesR :: Handler Html 
getFAQuickEntryLinesR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.QuickEntryLine]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFARecurrentInvoicesR :: Handler Html 
getFARecurrentInvoicesR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.RecurrentInvoice]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFARefsR :: Handler Html 
getFARefsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Ref]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASalesmanR :: Handler Html 
getFASalesmanR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Salesman]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASalesOrdersR :: Handler Html 
getFASalesOrdersR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.SalesOrder]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASalesOrderDetailsR :: Handler Html 
getFASalesOrderDetailsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.SalesOrderDetail]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASalesPosR :: Handler Html 
getFASalesPosR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.SalesPo]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASalesTypesR :: Handler Html 
getFASalesTypesR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.SalesType]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASecurityRolesR :: Handler Html 
getFASecurityRolesR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.SecurityRole]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAShippersR :: Handler Html 
getFAShippersR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Shipper]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASqlTrailR :: Handler Html 
getFASqlTrailR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.SqlTrail]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAStockCategoryR :: Handler Html 
getFAStockCategoryR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.StockCategory]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAStockMasterR :: Handler Html 
getFAStockMasterR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.StockMaster]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAStockMovesR :: Handler Html 
getFAStockMovesR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.StockMove]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASuppliersR :: Handler Html 
getFASuppliersR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Supplier]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASuppAllocationsR :: Handler Html 
getFASuppAllocationsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.SuppAllocation]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASuppInvoiceItemsR :: Handler Html 
getFASuppInvoiceItemsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.SuppInvoiceItem]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASuppTransR :: Handler Html 
getFASuppTransR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.SuppTran]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASysPrefsR :: Handler Html 
getFASysPrefsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.SysPref]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFASysTypesR :: Handler Html 
getFASysTypesR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.SysType]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFATagsR :: Handler Html 
getFATagsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Tag]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFATagAssociationsR :: Handler Html 
getFATagAssociationsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.TagAssociation]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFATaxGroupsR :: Handler Html 
getFATaxGroupsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.TaxGroup]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFATaxGroupItemsR :: Handler Html 
getFATaxGroupItemsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.TaxGroupItem]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFATaxTypesR :: Handler Html 
getFATaxTypesR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.TaxType]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFATransTaxDetailsR :: Handler Html 
getFATransTaxDetailsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.TransTaxDetail]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAUseronlineR :: Handler Html 
getFAUseronlineR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Useronline]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAUsersR :: Handler Html 
getFAUsersR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.User]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAVoidedR :: Handler Html 
getFAVoidedR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Voided]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAWorkcentresR :: Handler Html 
getFAWorkcentresR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Workcentre]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAWorkordersR :: Handler Html 
getFAWorkordersR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.Workorder]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAWoIssuesR :: Handler Html 
getFAWoIssuesR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.WoIssue]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAWoIssueItemsR :: Handler Html 
getFAWoIssueItemsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.WoIssueItem]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAWoManufactureR :: Handler Html 
getFAWoManufactureR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.WoManufacture]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)

getFAWoRequirementsR :: Handler Html 
getFAWoRequirementsR = do 
  entities <- runDB $ selectList [] []
  let typed = entities :: [Entity FA.WoRequirement]
  defaultLayout $ toWidget (entitiesToTable getDBName entities)
