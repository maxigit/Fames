module Handler.FA.Def where
import Import
import FA


getFAAreasR :: Handler Html 
getFAAreasR = entityTableHandler FAAreasR ([] :: [Filter FA.Area]) 

getFAAttachmentsR :: Handler Html 
getFAAttachmentsR = entityTableHandler FAAttachmentsR ([] :: [Filter FA.Attachment]) 

getFAAuditTrailR :: Handler Html 
getFAAuditTrailR = entityTableHandler FAAuditTrailR ([] :: [Filter FA.AuditTrail]) 

getFABankAccountsR :: Handler Html 
getFABankAccountsR = entityTableHandler FABankAccountsR ([] :: [Filter FA.BankAccount]) 

getFABankTransR :: Handler Html 
getFABankTransR = entityTableHandler FABankTransR ([] :: [Filter FA.BankTran]) 

getFABomR :: Handler Html 
getFABomR = entityTableHandler FABomR ([] :: [Filter FA.Bom]) 

getFABudgetTransR :: Handler Html 
getFABudgetTransR = entityTableHandler FABudgetTransR ([] :: [Filter FA.BudgetTran]) 

getFAChartClassR :: Handler Html 
getFAChartClassR = entityTableHandler FAChartClassR ([] :: [Filter FA.ChartClass]) 

getFAChartMasterR :: Handler Html 
getFAChartMasterR = entityTableHandler FAChartMasterR ([] :: [Filter FA.ChartMaster]) 

getFAChartTypesR :: Handler Html 
getFAChartTypesR = entityTableHandler FAChartTypesR ([] :: [Filter FA.ChartType]) 

getFACommentsR :: Handler Html 
getFACommentsR = entityTableHandler FACommentsR ([] :: [Filter FA.Comment]) 

getFACreditStatusR :: Handler Html 
getFACreditStatusR = entityTableHandler FACreditStatusR ([] :: [Filter FA.CreditStatu]) 

getFACrmCategoriesR :: Handler Html 
getFACrmCategoriesR = entityTableHandler FACrmCategoriesR ([] :: [Filter FA.CrmCategory]) 

getFACrmContactsR :: Handler Html 
getFACrmContactsR = entityTableHandler FACrmContactsR ([] :: [Filter FA.CrmContact]) 

getFACrmPersonsR :: Handler Html 
getFACrmPersonsR = entityTableHandler FACrmPersonsR ([] :: [Filter FA.CrmPerson]) 

getFACurrenciesR :: Handler Html 
getFACurrenciesR = entityTableHandler FACurrenciesR ([] :: [Filter FA.Currency]) 

getFACustAllocationsR :: Handler Html 
getFACustAllocationsR = entityTableHandler FACustAllocationsR ([] :: [Filter FA.CustAllocation]) 

getFACustBranchR :: Handler Html 
getFACustBranchR = entityTableHandler FACustBranchR ([] :: [Filter FA.CustBranch]) 

getFADebtorsMasterR :: Handler Html 
getFADebtorsMasterR = entityTableHandler FADebtorsMasterR ([] :: [Filter FA.DebtorsMaster]) 

getFADebtorTransR :: Handler Html 
getFADebtorTransR = entityTableHandler FADebtorTransR ([] :: [Filter FA.DebtorTran]) 

getFADebtorTransDetailsR :: Handler Html 
getFADebtorTransDetailsR = entityTableHandler FADebtorTransDetailsR ([] :: [Filter FA.DebtorTransDetail]) 

getFADimensionsR :: Handler Html 
getFADimensionsR = entityTableHandler FADimensionsR ([] :: [Filter FA.Dimension]) 

getFAExchangeRatesR :: Handler Html 
getFAExchangeRatesR = entityTableHandler FAExchangeRatesR ([] :: [Filter FA.ExchangeRate]) 

getFAFiscalYearR :: Handler Html 
getFAFiscalYearR = entityTableHandler FAFiscalYearR ([] :: [Filter FA.FiscalYear]) 

getFAGlTransR :: Handler Html 
getFAGlTransR = entityTableHandler FAGlTransR ([] :: [Filter FA.GlTran]) 

getFAGrnBatchR :: Handler Html 
getFAGrnBatchR = entityTableHandler FAGrnBatchR ([] :: [Filter FA.GrnBatch]) 

getFAGrnItemsR :: Handler Html 
getFAGrnItemsR = entityTableHandler FAGrnItemsR ([] :: [Filter FA.GrnItem]) 

getFAGroupsR :: Handler Html 
getFAGroupsR = entityTableHandler FAGroupsR ([] :: [Filter FA.Group]) 

getFAItemCodesR :: Handler Html 
getFAItemCodesR = entityTableHandler FAItemCodesR ([] :: [Filter FA.ItemCode]) 

getFAItemTaxTypesR :: Handler Html 
getFAItemTaxTypesR = entityTableHandler FAItemTaxTypesR ([] :: [Filter FA.ItemTaxType]) 

getFAItemTaxTypeExemptionsR :: Handler Html 
getFAItemTaxTypeExemptionsR = entityTableHandler FAItemTaxTypeExemptionsR ([] :: [Filter FA.ItemTaxTypeExemption]) 

getFAItemUnitsR :: Handler Html 
getFAItemUnitsR = entityTableHandler FAItemUnitsR ([] :: [Filter FA.ItemUnit]) 

getFALocationsR :: Handler Html 
getFALocationsR = entityTableHandler FALocationsR ([] :: [Filter FA.Location]) 

getFALocStockR :: Handler Html 
getFALocStockR = entityTableHandler FALocStockR ([] :: [Filter FA.LocStock]) 

getFAMovementTypesR :: Handler Html 
getFAMovementTypesR = entityTableHandler FAMovementTypesR ([] :: [Filter FA.MovementType]) 

getFAPaymentTermsR :: Handler Html 
getFAPaymentTermsR = entityTableHandler FAPaymentTermsR ([] :: [Filter FA.PaymentTerm]) 

getFAPricesR :: Handler Html 
getFAPricesR = entityTableHandler FAPricesR ([] :: [Filter FA.Price]) 

getFAPrintersR :: Handler Html 
getFAPrintersR = entityTableHandler FAPrintersR ([] :: [Filter FA.Printer]) 

getFAPrintProfilesR :: Handler Html 
getFAPrintProfilesR = entityTableHandler FAPrintProfilesR ([] :: [Filter FA.PrintProfile]) 

getFAPurchDataR :: Handler Html 
getFAPurchDataR = entityTableHandler FAPurchDataR ([] :: [Filter FA.PurchData]) 

getFAPurchOrdersR :: Handler Html 
getFAPurchOrdersR = entityTableHandler FAPurchOrdersR ([] :: [Filter FA.PurchOrder]) 

getFAPurchOrderDetailsR :: Handler Html 
getFAPurchOrderDetailsR = entityTableHandler FAPurchOrderDetailsR ([] :: [Filter FA.PurchOrderDetail]) 

getFAQuickEntriesR :: Handler Html 
getFAQuickEntriesR = entityTableHandler FAQuickEntriesR ([] :: [Filter FA.QuickEntry]) 

getFAQuickEntryLinesR :: Handler Html 
getFAQuickEntryLinesR = entityTableHandler FAQuickEntryLinesR ([] :: [Filter FA.QuickEntryLine]) 

getFARecurrentInvoicesR :: Handler Html 
getFARecurrentInvoicesR = entityTableHandler FARecurrentInvoicesR ([] :: [Filter FA.RecurrentInvoice]) 

getFARefsR :: Handler Html 
getFARefsR = entityTableHandler FARefsR ([] :: [Filter FA.Ref]) 

getFASalesmanR :: Handler Html 
getFASalesmanR = entityTableHandler FASalesmanR ([] :: [Filter FA.Salesman]) 

getFASalesOrdersR :: Handler Html 
getFASalesOrdersR = entityTableHandler FASalesOrdersR ([] :: [Filter FA.SalesOrder]) 

getFASalesOrderDetailsR :: Handler Html 
getFASalesOrderDetailsR = entityTableHandler FASalesOrderDetailsR ([] :: [Filter FA.SalesOrderDetail]) 

getFASalesPosR :: Handler Html 
getFASalesPosR = entityTableHandler FASalesPosR ([] :: [Filter FA.SalesPo]) 

getFASalesTypesR :: Handler Html 
getFASalesTypesR = entityTableHandler FASalesTypesR ([] :: [Filter FA.SalesType]) 

getFASecurityRolesR :: Handler Html 
getFASecurityRolesR = entityTableHandler FASecurityRolesR ([] :: [Filter FA.SecurityRole]) 

getFAShippersR :: Handler Html 
getFAShippersR = entityTableHandler FAShippersR ([] :: [Filter FA.Shipper]) 

getFASqlTrailR :: Handler Html 
getFASqlTrailR = entityTableHandler FASqlTrailR ([] :: [Filter FA.SqlTrail]) 

getFAStockCategoryR :: Handler Html 
getFAStockCategoryR = entityTableHandler FAStockCategoryR ([] :: [Filter FA.StockCategory]) 

getFAStockMasterR :: Handler Html 
getFAStockMasterR = entityTableHandler FAStockMasterR ([] :: [Filter FA.StockMaster]) 

getFAStockMovesR :: Handler Html 
getFAStockMovesR = entityTableHandler FAStockMovesR ([] :: [Filter FA.StockMove]) 

getFASuppliersR :: Handler Html 
getFASuppliersR = entityTableHandler FASuppliersR ([] :: [Filter FA.Supplier]) 

getFASuppAllocationsR :: Handler Html 
getFASuppAllocationsR = entityTableHandler FASuppAllocationsR ([] :: [Filter FA.SuppAllocation]) 

getFASuppInvoiceItemsR :: Handler Html 
getFASuppInvoiceItemsR = entityTableHandler FASuppInvoiceItemsR ([] :: [Filter FA.SuppInvoiceItem]) 

getFASuppTransR :: Handler Html 
getFASuppTransR = entityTableHandler FASuppTransR ([] :: [Filter FA.SuppTran]) 

getFASysPrefsR :: Handler Html 
getFASysPrefsR = entityTableHandler FASysPrefsR ([] :: [Filter FA.SysPref]) 

getFASysTypesR :: Handler Html 
getFASysTypesR = entityTableHandler FASysTypesR ([] :: [Filter FA.SysType]) 

getFATagsR :: Handler Html 
getFATagsR = entityTableHandler FATagsR ([] :: [Filter FA.Tag]) 

getFATagAssociationsR :: Handler Html 
getFATagAssociationsR = entityTableHandler FATagAssociationsR ([] :: [Filter FA.TagAssociation]) 

getFATaxGroupsR :: Handler Html 
getFATaxGroupsR = entityTableHandler FATaxGroupsR ([] :: [Filter FA.TaxGroup]) 

getFATaxGroupItemsR :: Handler Html 
getFATaxGroupItemsR = entityTableHandler FATaxGroupItemsR ([] :: [Filter FA.TaxGroupItem]) 

getFATaxTypesR :: Handler Html 
getFATaxTypesR = entityTableHandler FATaxTypesR ([] :: [Filter FA.TaxType]) 

getFATransTaxDetailsR :: Handler Html 
getFATransTaxDetailsR = entityTableHandler FATransTaxDetailsR ([] :: [Filter FA.TransTaxDetail]) 

getFAUseronlineR :: Handler Html 
getFAUseronlineR = entityTableHandler FAUseronlineR ([] :: [Filter FA.Useronline]) 

getFAUsersR :: Handler Html 
getFAUsersR = entityTableHandler FAUsersR ([] :: [Filter FA.User]) 

getFAVoidedR :: Handler Html 
getFAVoidedR = entityTableHandler FAVoidedR ([] :: [Filter FA.Voided]) 

getFAWorkcentresR :: Handler Html 
getFAWorkcentresR = entityTableHandler FAWorkcentresR ([] :: [Filter FA.Workcentre]) 

getFAWorkordersR :: Handler Html 
getFAWorkordersR = entityTableHandler FAWorkordersR ([] :: [Filter FA.Workorder]) 

getFAWoIssuesR :: Handler Html 
getFAWoIssuesR = entityTableHandler FAWoIssuesR ([] :: [Filter FA.WoIssue]) 

getFAWoIssueItemsR :: Handler Html 
getFAWoIssueItemsR = entityTableHandler FAWoIssueItemsR ([] :: [Filter FA.WoIssueItem]) 

getFAWoManufactureR :: Handler Html 
getFAWoManufactureR = entityTableHandler FAWoManufactureR ([] :: [Filter FA.WoManufacture]) 

getFAWoRequirementsR :: Handler Html 
getFAWoRequirementsR = entityTableHandler FAWoRequirementsR ([] :: [Filter FA.WoRequirement]) 
