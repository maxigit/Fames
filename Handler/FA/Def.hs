-- Warning ! This code has been generated !
-- Handler
module Handler.FA.Def where
import Import
import FA


getFAAreasR :: Handler Html 
getFAAreasR = entityTableHandler (FA'R FAAreasR) ([] :: [Filter FA.Area]) 

getFAAttachmentsR :: Handler Html 
getFAAttachmentsR = entityTableHandler (FA'R FAAttachmentsR) ([] :: [Filter FA.Attachment]) 

getFAAuditTrailR :: Handler Html 
getFAAuditTrailR = entityTableHandler (FA'R FAAuditTrailR) ([] :: [Filter FA.AuditTrail]) 

getFABankAccountsR :: Handler Html 
getFABankAccountsR = entityTableHandler (FA'R FABankAccountsR) ([] :: [Filter FA.BankAccount]) 

getFABankTransR :: Handler Html 
getFABankTransR = entityTableHandler (FA'R FABankTransR) ([] :: [Filter FA.BankTran]) 

getFABomR :: Handler Html 
getFABomR = entityTableHandler (FA'R FABomR) ([] :: [Filter FA.Bom]) 

getFABudgetTransR :: Handler Html 
getFABudgetTransR = entityTableHandler (FA'R FABudgetTransR) ([] :: [Filter FA.BudgetTran]) 

getFAChartClassR :: Handler Html 
getFAChartClassR = entityTableHandler (FA'R FAChartClassR) ([] :: [Filter FA.ChartClass]) 

getFAChartMasterR :: Handler Html 
getFAChartMasterR = entityTableHandler (FA'R FAChartMasterR) ([] :: [Filter FA.ChartMaster]) 

getFAChartTypesR :: Handler Html 
getFAChartTypesR = entityTableHandler (FA'R FAChartTypesR) ([] :: [Filter FA.ChartType]) 

getFACommentsR :: Handler Html 
getFACommentsR = entityTableHandler (FA'R FACommentsR) ([] :: [Filter FA.Comment]) 

getFACreditStatusR :: Handler Html 
getFACreditStatusR = entityTableHandler (FA'R FACreditStatusR) ([] :: [Filter FA.CreditStatu]) 

getFACrmCategoriesR :: Handler Html 
getFACrmCategoriesR = entityTableHandler (FA'R FACrmCategoriesR) ([] :: [Filter FA.CrmCategory]) 

getFACrmContactsR :: Handler Html 
getFACrmContactsR = entityTableHandler (FA'R FACrmContactsR) ([] :: [Filter FA.CrmContact]) 

getFACrmPersonsR :: Handler Html 
getFACrmPersonsR = entityTableHandler (FA'R FACrmPersonsR) ([] :: [Filter FA.CrmPerson]) 

getFACurrenciesR :: Handler Html 
getFACurrenciesR = entityTableHandler (FA'R FACurrenciesR) ([] :: [Filter FA.Currency]) 

getFACustAllocationsR :: Handler Html 
getFACustAllocationsR = entityTableHandler (FA'R FACustAllocationsR) ([] :: [Filter FA.CustAllocation]) 

getFACustBranchR :: Handler Html 
getFACustBranchR = entityTableHandler (FA'R FACustBranchR) ([] :: [Filter FA.CustBranch]) 

getFADashboardRemindersR :: Handler Html 
getFADashboardRemindersR = entityTableHandler (FA'R FADashboardRemindersR) ([] :: [Filter FA.DashboardReminder]) 

getFADashboardWidgetsR :: Handler Html 
getFADashboardWidgetsR = entityTableHandler (FA'R FADashboardWidgetsR) ([] :: [Filter FA.DashboardWidget]) 

getFADebtorsMasterR :: Handler Html 
getFADebtorsMasterR = entityTableHandler (FA'R FADebtorsMasterR) ([] :: [Filter FA.DebtorsMaster]) 

getFADebtorTransR :: Handler Html 
getFADebtorTransR = entityTableHandler (FA'R FADebtorTransR) ([] :: [Filter FA.DebtorTran]) 

getFADebtorTransDetailsR :: Handler Html 
getFADebtorTransDetailsR = entityTableHandler (FA'R FADebtorTransDetailsR) ([] :: [Filter FA.DebtorTransDetail]) 

getFADenormOrderDetailsQueueR :: Handler Html 
getFADenormOrderDetailsQueueR = entityTableHandler (FA'R FADenormOrderDetailsQueueR) ([] :: [Filter FA.DenormOrderDetailsQueue]) 

getFADenormQohR :: Handler Html 
getFADenormQohR = entityTableHandler (FA'R FADenormQohR) ([] :: [Filter FA.DenormQoh]) 

getFADimensionsR :: Handler Html 
getFADimensionsR = entityTableHandler (FA'R FADimensionsR) ([] :: [Filter FA.Dimension]) 

getFAExchangeRatesR :: Handler Html 
getFAExchangeRatesR = entityTableHandler (FA'R FAExchangeRatesR) ([] :: [Filter FA.ExchangeRate]) 

getFAFiscalYearR :: Handler Html 
getFAFiscalYearR = entityTableHandler (FA'R FAFiscalYearR) ([] :: [Filter FA.FiscalYear]) 

getFAGlToStockR :: Handler Html 
getFAGlToStockR = entityTableHandler (FA'R FAGlToStockR) ([] :: [Filter FA.GlToStock]) 

getFAGlTransR :: Handler Html 
getFAGlTransR = entityTableHandler (FA'R FAGlTransR) ([] :: [Filter FA.GlTran]) 

getFAGrnBatchR :: Handler Html 
getFAGrnBatchR = entityTableHandler (FA'R FAGrnBatchR) ([] :: [Filter FA.GrnBatch]) 

getFAGrnItemsR :: Handler Html 
getFAGrnItemsR = entityTableHandler (FA'R FAGrnItemsR) ([] :: [Filter FA.GrnItem]) 

getFAGroupsR :: Handler Html 
getFAGroupsR = entityTableHandler (FA'R FAGroupsR) ([] :: [Filter FA.Group]) 

getFAItemCodesR :: Handler Html 
getFAItemCodesR = entityTableHandler (FA'R FAItemCodesR) ([] :: [Filter FA.ItemCode]) 

getFAItemTaxTypesR :: Handler Html 
getFAItemTaxTypesR = entityTableHandler (FA'R FAItemTaxTypesR) ([] :: [Filter FA.ItemTaxType]) 

getFAItemTaxTypeExemptionsR :: Handler Html 
getFAItemTaxTypeExemptionsR = entityTableHandler (FA'R FAItemTaxTypeExemptionsR) ([] :: [Filter FA.ItemTaxTypeExemption]) 

getFAItemUnitsR :: Handler Html 
getFAItemUnitsR = entityTableHandler (FA'R FAItemUnitsR) ([] :: [Filter FA.ItemUnit]) 

getFALocationsR :: Handler Html 
getFALocationsR = entityTableHandler (FA'R FALocationsR) ([] :: [Filter FA.Location]) 

getFALocStockR :: Handler Html 
getFALocStockR = entityTableHandler (FA'R FALocStockR) ([] :: [Filter FA.LocStock]) 

getFAMopActionsR :: Handler Html 
getFAMopActionsR = entityTableHandler (FA'R FAMopActionsR) ([] :: [Filter FA.MopAction]) 

getFAMovementTypesR :: Handler Html 
getFAMovementTypesR = entityTableHandler (FA'R FAMovementTypesR) ([] :: [Filter FA.MovementType]) 

getFAMyStockR :: Handler Html 
getFAMyStockR = entityTableHandler (FA'R FAMyStockR) ([] :: [Filter FA.MyStock]) 

getFAOrderSummaryViewR :: Handler Html 
getFAOrderSummaryViewR = entityTableHandler (FA'R FAOrderSummaryViewR) ([] :: [Filter FA.OrderSummaryView]) 

getFAPaymentTermsR :: Handler Html 
getFAPaymentTermsR = entityTableHandler (FA'R FAPaymentTermsR) ([] :: [Filter FA.PaymentTerm]) 

getFAPricesR :: Handler Html 
getFAPricesR = entityTableHandler (FA'R FAPricesR) ([] :: [Filter FA.Price]) 

getFAPrintersR :: Handler Html 
getFAPrintersR = entityTableHandler (FA'R FAPrintersR) ([] :: [Filter FA.Printer]) 

getFAPrintProfilesR :: Handler Html 
getFAPrintProfilesR = entityTableHandler (FA'R FAPrintProfilesR) ([] :: [Filter FA.PrintProfile]) 

getFAPurchDataR :: Handler Html 
getFAPurchDataR = entityTableHandler (FA'R FAPurchDataR) ([] :: [Filter FA.PurchData]) 

getFAPurchOrdersR :: Handler Html 
getFAPurchOrdersR = entityTableHandler (FA'R FAPurchOrdersR) ([] :: [Filter FA.PurchOrder]) 

getFAPurchOrderDetailsR :: Handler Html 
getFAPurchOrderDetailsR = entityTableHandler (FA'R FAPurchOrderDetailsR) ([] :: [Filter FA.PurchOrderDetail]) 

getFAQuickEntriesR :: Handler Html 
getFAQuickEntriesR = entityTableHandler (FA'R FAQuickEntriesR) ([] :: [Filter FA.QuickEntry]) 

getFAQuickEntryLinesR :: Handler Html 
getFAQuickEntryLinesR = entityTableHandler (FA'R FAQuickEntryLinesR) ([] :: [Filter FA.QuickEntryLine]) 

getFARecurrentInvoicesR :: Handler Html 
getFARecurrentInvoicesR = entityTableHandler (FA'R FARecurrentInvoicesR) ([] :: [Filter FA.RecurrentInvoice]) 

getFARefsR :: Handler Html 
getFARefsR = entityTableHandler (FA'R FARefsR) ([] :: [Filter FA.Ref]) 

getFARequisitionsR :: Handler Html 
getFARequisitionsR = entityTableHandler (FA'R FARequisitionsR) ([] :: [Filter FA.Requisition]) 

getFARequisitionDetailsR :: Handler Html 
getFARequisitionDetailsR = entityTableHandler (FA'R FARequisitionDetailsR) ([] :: [Filter FA.RequisitionDetail]) 

getFASalesmanR :: Handler Html 
getFASalesmanR = entityTableHandler (FA'R FASalesmanR) ([] :: [Filter FA.Salesman]) 

getFASalesOrdersR :: Handler Html 
getFASalesOrdersR = entityTableHandler (FA'R FASalesOrdersR) ([] :: [Filter FA.SalesOrder]) 

getFASalesOrderDetailsR :: Handler Html 
getFASalesOrderDetailsR = entityTableHandler (FA'R FASalesOrderDetailsR) ([] :: [Filter FA.SalesOrderDetail]) 

getFASalesOrderDetails2R :: Handler Html 
getFASalesOrderDetails2R = entityTableHandler (FA'R FASalesOrderDetails2R) ([] :: [Filter FA.SalesOrderDetails2]) 

getFASalesPosR :: Handler Html 
getFASalesPosR = entityTableHandler (FA'R FASalesPosR) ([] :: [Filter FA.SalesPo]) 

getFASalesTypesR :: Handler Html 
getFASalesTypesR = entityTableHandler (FA'R FASalesTypesR) ([] :: [Filter FA.SalesType]) 

getFASecurityRolesR :: Handler Html 
getFASecurityRolesR = entityTableHandler (FA'R FASecurityRolesR) ([] :: [Filter FA.SecurityRole]) 

getFAShippersR :: Handler Html 
getFAShippersR = entityTableHandler (FA'R FAShippersR) ([] :: [Filter FA.Shipper]) 

getFASqlTrailR :: Handler Html 
getFASqlTrailR = entityTableHandler (FA'R FASqlTrailR) ([] :: [Filter FA.SqlTrail]) 

getFAStockAuditR :: Handler Html 
getFAStockAuditR = entityTableHandler (FA'R FAStockAuditR) ([] :: [Filter FA.StockAudit]) 

getFAStockCategoryR :: Handler Html 
getFAStockCategoryR = entityTableHandler (FA'R FAStockCategoryR) ([] :: [Filter FA.StockCategory]) 

getFAStockMasterR :: Handler Html 
getFAStockMasterR = entityTableHandler (FA'R FAStockMasterR) ([] :: [Filter FA.StockMaster]) 

getFAStockMovesR :: Handler Html 
getFAStockMovesR = entityTableHandler (FA'R FAStockMovesR) ([] :: [Filter FA.StockMove]) 

getFAStockToCostR :: Handler Html 
getFAStockToCostR = entityTableHandler (FA'R FAStockToCostR) ([] :: [Filter FA.StockToCost]) 

getFASuppliersR :: Handler Html 
getFASuppliersR = entityTableHandler (FA'R FASuppliersR) ([] :: [Filter FA.Supplier]) 

getFASuppAllocationsR :: Handler Html 
getFASuppAllocationsR = entityTableHandler (FA'R FASuppAllocationsR) ([] :: [Filter FA.SuppAllocation]) 

getFASuppInvoiceItemsR :: Handler Html 
getFASuppInvoiceItemsR = entityTableHandler (FA'R FASuppInvoiceItemsR) ([] :: [Filter FA.SuppInvoiceItem]) 

getFASuppTransR :: Handler Html 
getFASuppTransR = entityTableHandler (FA'R FASuppTransR) ([] :: [Filter FA.SuppTran]) 

getFASysPrefsR :: Handler Html 
getFASysPrefsR = entityTableHandler (FA'R FASysPrefsR) ([] :: [Filter FA.SysPref]) 

getFASysTypesR :: Handler Html 
getFASysTypesR = entityTableHandler (FA'R FASysTypesR) ([] :: [Filter FA.SysType]) 

getFATagsR :: Handler Html 
getFATagsR = entityTableHandler (FA'R FATagsR) ([] :: [Filter FA.Tag]) 

getFATagAssociationsR :: Handler Html 
getFATagAssociationsR = entityTableHandler (FA'R FATagAssociationsR) ([] :: [Filter FA.TagAssociation]) 

getFATaxGroupsR :: Handler Html 
getFATaxGroupsR = entityTableHandler (FA'R FATaxGroupsR) ([] :: [Filter FA.TaxGroup]) 

getFATaxGroupItemsR :: Handler Html 
getFATaxGroupItemsR = entityTableHandler (FA'R FATaxGroupItemsR) ([] :: [Filter FA.TaxGroupItem]) 

getFATaxRepR :: Handler Html 
getFATaxRepR = entityTableHandler (FA'R FATaxRepR) ([] :: [Filter FA.TaxRep]) 

getFATaxRepLineR :: Handler Html 
getFATaxRepLineR = entityTableHandler (FA'R FATaxRepLineR) ([] :: [Filter FA.TaxRepLine]) 

getFATaxRepLineAccountsR :: Handler Html 
getFATaxRepLineAccountsR = entityTableHandler (FA'R FATaxRepLineAccountsR) ([] :: [Filter FA.TaxRepLineAccount]) 

getFATaxTypesR :: Handler Html 
getFATaxTypesR = entityTableHandler (FA'R FATaxTypesR) ([] :: [Filter FA.TaxType]) 

getFATopickR :: Handler Html 
getFATopickR = entityTableHandler (FA'R FATopickR) ([] :: [Filter FA.Topick]) 

getFATransTaxDetailsR :: Handler Html 
getFATransTaxDetailsR = entityTableHandler (FA'R FATransTaxDetailsR) ([] :: [Filter FA.TransTaxDetail]) 

getFATransToRateR :: Handler Html 
getFATransToRateR = entityTableHandler (FA'R FATransToRateR) ([] :: [Filter FA.TransToRate]) 

getFATransToStockR :: Handler Html 
getFATransToStockR = entityTableHandler (FA'R FATransToStockR) ([] :: [Filter FA.TransToStock]) 

getFAUseronlineR :: Handler Html 
getFAUseronlineR = entityTableHandler (FA'R FAUseronlineR) ([] :: [Filter FA.Useronline]) 

getFAUsersR :: Handler Html 
getFAUsersR = entityTableHandler (FA'R FAUsersR) ([] :: [Filter FA.User]) 

getFAVoidedR :: Handler Html 
getFAVoidedR = entityTableHandler (FA'R FAVoidedR) ([] :: [Filter FA.Voided]) 

getFAWorkcentresR :: Handler Html 
getFAWorkcentresR = entityTableHandler (FA'R FAWorkcentresR) ([] :: [Filter FA.Workcentre]) 

getFAWorkordersR :: Handler Html 
getFAWorkordersR = entityTableHandler (FA'R FAWorkordersR) ([] :: [Filter FA.Workorder]) 

getFAWoIssuesR :: Handler Html 
getFAWoIssuesR = entityTableHandler (FA'R FAWoIssuesR) ([] :: [Filter FA.WoIssue]) 

getFAWoIssueItemsR :: Handler Html 
getFAWoIssueItemsR = entityTableHandler (FA'R FAWoIssueItemsR) ([] :: [Filter FA.WoIssueItem]) 

getFAWoManufactureR :: Handler Html 
getFAWoManufactureR = entityTableHandler (FA'R FAWoManufactureR) ([] :: [Filter FA.WoManufacture]) 

getFAWoRequirementsR :: Handler Html 
getFAWoRequirementsR = entityTableHandler (FA'R FAWoRequirementsR) ([] :: [Filter FA.WoRequirement]) 

