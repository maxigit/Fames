-- Warning ! This code has been generated !
-- Handler
module Handler.FA.Def where
import Import
import FA


{-# NOINLINE getFAAreasR #-}
getFAAreasR :: Handler Html 
getFAAreasR = entityTableHandler (FA'R FAAreasR) ([] :: [Filter FA.Area]) 

{-# NOINLINE getFAAttachmentsR #-}
getFAAttachmentsR :: Handler Html 
getFAAttachmentsR = entityTableHandler (FA'R FAAttachmentsR) ([] :: [Filter FA.Attachment]) 

{-# NOINLINE getFAAuditTrailR #-}
getFAAuditTrailR :: Handler Html 
getFAAuditTrailR = entityTableHandler (FA'R FAAuditTrailR) ([] :: [Filter FA.AuditTrail]) 

{-# NOINLINE getFABankAccountsR #-}
getFABankAccountsR :: Handler Html 
getFABankAccountsR = entityTableHandler (FA'R FABankAccountsR) ([] :: [Filter FA.BankAccount]) 

{-# NOINLINE getFABankTransR #-}
getFABankTransR :: Handler Html 
getFABankTransR = entityTableHandler (FA'R FABankTransR) ([] :: [Filter FA.BankTran]) 

{-# NOINLINE getFABomR #-}
getFABomR :: Handler Html 
getFABomR = entityTableHandler (FA'R FABomR) ([] :: [Filter FA.Bom]) 

{-# NOINLINE getFABudgetTransR #-}
getFABudgetTransR :: Handler Html 
getFABudgetTransR = entityTableHandler (FA'R FABudgetTransR) ([] :: [Filter FA.BudgetTran]) 

{-# NOINLINE getFAChartClassR #-}
getFAChartClassR :: Handler Html 
getFAChartClassR = entityTableHandler (FA'R FAChartClassR) ([] :: [Filter FA.ChartClass]) 

{-# NOINLINE getFAChartMasterR #-}
getFAChartMasterR :: Handler Html 
getFAChartMasterR = entityTableHandler (FA'R FAChartMasterR) ([] :: [Filter FA.ChartMaster]) 

{-# NOINLINE getFAChartTypesR #-}
getFAChartTypesR :: Handler Html 
getFAChartTypesR = entityTableHandler (FA'R FAChartTypesR) ([] :: [Filter FA.ChartType]) 

{-# NOINLINE getFACommentsR #-}
getFACommentsR :: Handler Html 
getFACommentsR = entityTableHandler (FA'R FACommentsR) ([] :: [Filter FA.Comment]) 

{-# NOINLINE getFACreditStatusR #-}
getFACreditStatusR :: Handler Html 
getFACreditStatusR = entityTableHandler (FA'R FACreditStatusR) ([] :: [Filter FA.CreditStatu]) 

{-# NOINLINE getFACrmCategoriesR #-}
getFACrmCategoriesR :: Handler Html 
getFACrmCategoriesR = entityTableHandler (FA'R FACrmCategoriesR) ([] :: [Filter FA.CrmCategory]) 

{-# NOINLINE getFACrmContactsR #-}
getFACrmContactsR :: Handler Html 
getFACrmContactsR = entityTableHandler (FA'R FACrmContactsR) ([] :: [Filter FA.CrmContact]) 

{-# NOINLINE getFACrmPersonsR #-}
getFACrmPersonsR :: Handler Html 
getFACrmPersonsR = entityTableHandler (FA'R FACrmPersonsR) ([] :: [Filter FA.CrmPerson]) 

{-# NOINLINE getFACurrenciesR #-}
getFACurrenciesR :: Handler Html 
getFACurrenciesR = entityTableHandler (FA'R FACurrenciesR) ([] :: [Filter FA.Currency]) 

{-# NOINLINE getFACustAllocationsR #-}
getFACustAllocationsR :: Handler Html 
getFACustAllocationsR = entityTableHandler (FA'R FACustAllocationsR) ([] :: [Filter FA.CustAllocation]) 

{-# NOINLINE getFACustBranchR #-}
getFACustBranchR :: Handler Html 
getFACustBranchR = entityTableHandler (FA'R FACustBranchR) ([] :: [Filter FA.CustBranch]) 

{-# NOINLINE getFADashboardRemindersR #-}
getFADashboardRemindersR :: Handler Html 
getFADashboardRemindersR = entityTableHandler (FA'R FADashboardRemindersR) ([] :: [Filter FA.DashboardReminder]) 

{-# NOINLINE getFADashboardWidgetsR #-}
getFADashboardWidgetsR :: Handler Html 
getFADashboardWidgetsR = entityTableHandler (FA'R FADashboardWidgetsR) ([] :: [Filter FA.DashboardWidget]) 

{-# NOINLINE getFADebtorsMasterR #-}
getFADebtorsMasterR :: Handler Html 
getFADebtorsMasterR = entityTableHandler (FA'R FADebtorsMasterR) ([] :: [Filter FA.DebtorsMaster]) 

{-# NOINLINE getFADebtorTransR #-}
getFADebtorTransR :: Handler Html 
getFADebtorTransR = entityTableHandler (FA'R FADebtorTransR) ([] :: [Filter FA.DebtorTran]) 

{-# NOINLINE getFADebtorTransDetailsR #-}
getFADebtorTransDetailsR :: Handler Html 
getFADebtorTransDetailsR = entityTableHandler (FA'R FADebtorTransDetailsR) ([] :: [Filter FA.DebtorTransDetail]) 

{-# NOINLINE getFADenormOrderDetailsQueueR #-}
getFADenormOrderDetailsQueueR :: Handler Html 
getFADenormOrderDetailsQueueR = entityTableHandler (FA'R FADenormOrderDetailsQueueR) ([] :: [Filter FA.DenormOrderDetailsQueue]) 

{-# NOINLINE getFADenormQohR #-}
getFADenormQohR :: Handler Html 
getFADenormQohR = entityTableHandler (FA'R FADenormQohR) ([] :: [Filter FA.DenormQoh]) 

{-# NOINLINE getFADimensionsR #-}
getFADimensionsR :: Handler Html 
getFADimensionsR = entityTableHandler (FA'R FADimensionsR) ([] :: [Filter FA.Dimension]) 

{-# NOINLINE getFAExchangeRatesR #-}
getFAExchangeRatesR :: Handler Html 
getFAExchangeRatesR = entityTableHandler (FA'R FAExchangeRatesR) ([] :: [Filter FA.ExchangeRate]) 

{-# NOINLINE getFAFiscalYearR #-}
getFAFiscalYearR :: Handler Html 
getFAFiscalYearR = entityTableHandler (FA'R FAFiscalYearR) ([] :: [Filter FA.FiscalYear]) 

{-# NOINLINE getFAGlToStockR #-}
getFAGlToStockR :: Handler Html 
getFAGlToStockR = entityTableHandler (FA'R FAGlToStockR) ([] :: [Filter FA.GlToStock]) 

{-# NOINLINE getFAGlTransR #-}
getFAGlTransR :: Handler Html 
getFAGlTransR = entityTableHandler (FA'R FAGlTransR) ([] :: [Filter FA.GlTran]) 

{-# NOINLINE getFAGrnBatchR #-}
getFAGrnBatchR :: Handler Html 
getFAGrnBatchR = entityTableHandler (FA'R FAGrnBatchR) ([] :: [Filter FA.GrnBatch]) 

{-# NOINLINE getFAGrnItemsR #-}
getFAGrnItemsR :: Handler Html 
getFAGrnItemsR = entityTableHandler (FA'R FAGrnItemsR) ([] :: [Filter FA.GrnItem]) 

{-# NOINLINE getFAGroupsR #-}
getFAGroupsR :: Handler Html 
getFAGroupsR = entityTableHandler (FA'R FAGroupsR) ([] :: [Filter FA.Group]) 

{-# NOINLINE getFAItemCodesR #-}
getFAItemCodesR :: Handler Html 
getFAItemCodesR = entityTableHandler (FA'R FAItemCodesR) ([] :: [Filter FA.ItemCode]) 

{-# NOINLINE getFAItemTaxTypesR #-}
getFAItemTaxTypesR :: Handler Html 
getFAItemTaxTypesR = entityTableHandler (FA'R FAItemTaxTypesR) ([] :: [Filter FA.ItemTaxType]) 

{-# NOINLINE getFAItemTaxTypeExemptionsR #-}
getFAItemTaxTypeExemptionsR :: Handler Html 
getFAItemTaxTypeExemptionsR = entityTableHandler (FA'R FAItemTaxTypeExemptionsR) ([] :: [Filter FA.ItemTaxTypeExemption]) 

{-# NOINLINE getFAItemUnitsR #-}
getFAItemUnitsR :: Handler Html 
getFAItemUnitsR = entityTableHandler (FA'R FAItemUnitsR) ([] :: [Filter FA.ItemUnit]) 

{-# NOINLINE getFALocationsR #-}
getFALocationsR :: Handler Html 
getFALocationsR = entityTableHandler (FA'R FALocationsR) ([] :: [Filter FA.Location]) 

{-# NOINLINE getFALocStockR #-}
getFALocStockR :: Handler Html 
getFALocStockR = entityTableHandler (FA'R FALocStockR) ([] :: [Filter FA.LocStock]) 

{-# NOINLINE getFAMopActionsR #-}
getFAMopActionsR :: Handler Html 
getFAMopActionsR = entityTableHandler (FA'R FAMopActionsR) ([] :: [Filter FA.MopAction]) 

{-# NOINLINE getFAMovementTypesR #-}
getFAMovementTypesR :: Handler Html 
getFAMovementTypesR = entityTableHandler (FA'R FAMovementTypesR) ([] :: [Filter FA.MovementType]) 

{-# NOINLINE getFAMyStockR #-}
getFAMyStockR :: Handler Html 
getFAMyStockR = entityTableHandler (FA'R FAMyStockR) ([] :: [Filter FA.MyStock]) 

{-# NOINLINE getFAOrderSummaryViewR #-}
getFAOrderSummaryViewR :: Handler Html 
getFAOrderSummaryViewR = entityTableHandler (FA'R FAOrderSummaryViewR) ([] :: [Filter FA.OrderSummaryView]) 

{-# NOINLINE getFAPaymentTermsR #-}
getFAPaymentTermsR :: Handler Html 
getFAPaymentTermsR = entityTableHandler (FA'R FAPaymentTermsR) ([] :: [Filter FA.PaymentTerm]) 

{-# NOINLINE getFAPricesR #-}
getFAPricesR :: Handler Html 
getFAPricesR = entityTableHandler (FA'R FAPricesR) ([] :: [Filter FA.Price]) 

{-# NOINLINE getFAPrintersR #-}
getFAPrintersR :: Handler Html 
getFAPrintersR = entityTableHandler (FA'R FAPrintersR) ([] :: [Filter FA.Printer]) 

{-# NOINLINE getFAPrintProfilesR #-}
getFAPrintProfilesR :: Handler Html 
getFAPrintProfilesR = entityTableHandler (FA'R FAPrintProfilesR) ([] :: [Filter FA.PrintProfile]) 

{-# NOINLINE getFAPurchDataR #-}
getFAPurchDataR :: Handler Html 
getFAPurchDataR = entityTableHandler (FA'R FAPurchDataR) ([] :: [Filter FA.PurchData]) 

{-# NOINLINE getFAPurchOrdersR #-}
getFAPurchOrdersR :: Handler Html 
getFAPurchOrdersR = entityTableHandler (FA'R FAPurchOrdersR) ([] :: [Filter FA.PurchOrder]) 

{-# NOINLINE getFAPurchOrderDetailsR #-}
getFAPurchOrderDetailsR :: Handler Html 
getFAPurchOrderDetailsR = entityTableHandler (FA'R FAPurchOrderDetailsR) ([] :: [Filter FA.PurchOrderDetail]) 

{-# NOINLINE getFAQuickEntriesR #-}
getFAQuickEntriesR :: Handler Html 
getFAQuickEntriesR = entityTableHandler (FA'R FAQuickEntriesR) ([] :: [Filter FA.QuickEntry]) 

{-# NOINLINE getFAQuickEntryLinesR #-}
getFAQuickEntryLinesR :: Handler Html 
getFAQuickEntryLinesR = entityTableHandler (FA'R FAQuickEntryLinesR) ([] :: [Filter FA.QuickEntryLine]) 

{-# NOINLINE getFARecurrentInvoicesR #-}
getFARecurrentInvoicesR :: Handler Html 
getFARecurrentInvoicesR = entityTableHandler (FA'R FARecurrentInvoicesR) ([] :: [Filter FA.RecurrentInvoice]) 

{-# NOINLINE getFARefsR #-}
getFARefsR :: Handler Html 
getFARefsR = entityTableHandler (FA'R FARefsR) ([] :: [Filter FA.Ref]) 

{-# NOINLINE getFARequisitionsR #-}
getFARequisitionsR :: Handler Html 
getFARequisitionsR = entityTableHandler (FA'R FARequisitionsR) ([] :: [Filter FA.Requisition]) 

{-# NOINLINE getFARequisitionDetailsR #-}
getFARequisitionDetailsR :: Handler Html 
getFARequisitionDetailsR = entityTableHandler (FA'R FARequisitionDetailsR) ([] :: [Filter FA.RequisitionDetail]) 

{-# NOINLINE getFASalesmanR #-}
getFASalesmanR :: Handler Html 
getFASalesmanR = entityTableHandler (FA'R FASalesmanR) ([] :: [Filter FA.Salesman]) 

{-# NOINLINE getFASalesOrdersR #-}
getFASalesOrdersR :: Handler Html 
getFASalesOrdersR = entityTableHandler (FA'R FASalesOrdersR) ([] :: [Filter FA.SalesOrder]) 

{-# NOINLINE getFASalesOrderDetailsR #-}
getFASalesOrderDetailsR :: Handler Html 
getFASalesOrderDetailsR = entityTableHandler (FA'R FASalesOrderDetailsR) ([] :: [Filter FA.SalesOrderDetail]) 

{-# NOINLINE getFASalesOrderDetails2R #-}
getFASalesOrderDetails2R :: Handler Html 
getFASalesOrderDetails2R = entityTableHandler (FA'R FASalesOrderDetails2R) ([] :: [Filter FA.SalesOrderDetails2]) 

{-# NOINLINE getFASalesPosR #-}
getFASalesPosR :: Handler Html 
getFASalesPosR = entityTableHandler (FA'R FASalesPosR) ([] :: [Filter FA.SalesPo]) 

{-# NOINLINE getFASalesTypesR #-}
getFASalesTypesR :: Handler Html 
getFASalesTypesR = entityTableHandler (FA'R FASalesTypesR) ([] :: [Filter FA.SalesType]) 

{-# NOINLINE getFASecurityRolesR #-}
getFASecurityRolesR :: Handler Html 
getFASecurityRolesR = entityTableHandler (FA'R FASecurityRolesR) ([] :: [Filter FA.SecurityRole]) 

{-# NOINLINE getFAShippersR #-}
getFAShippersR :: Handler Html 
getFAShippersR = entityTableHandler (FA'R FAShippersR) ([] :: [Filter FA.Shipper]) 

{-# NOINLINE getFASqlTrailR #-}
getFASqlTrailR :: Handler Html 
getFASqlTrailR = entityTableHandler (FA'R FASqlTrailR) ([] :: [Filter FA.SqlTrail]) 

{-# NOINLINE getFAStockAuditR #-}
getFAStockAuditR :: Handler Html 
getFAStockAuditR = entityTableHandler (FA'R FAStockAuditR) ([] :: [Filter FA.StockAudit]) 

{-# NOINLINE getFAStockCategoryR #-}
getFAStockCategoryR :: Handler Html 
getFAStockCategoryR = entityTableHandler (FA'R FAStockCategoryR) ([] :: [Filter FA.StockCategory]) 

{-# NOINLINE getFAStockMasterR #-}
getFAStockMasterR :: Handler Html 
getFAStockMasterR = entityTableHandler (FA'R FAStockMasterR) ([] :: [Filter FA.StockMaster]) 

{-# NOINLINE getFAStockMovesR #-}
getFAStockMovesR :: Handler Html 
getFAStockMovesR = entityTableHandler (FA'R FAStockMovesR) ([] :: [Filter FA.StockMove]) 

{-# NOINLINE getFAStockToCostR #-}
getFAStockToCostR :: Handler Html 
getFAStockToCostR = entityTableHandler (FA'R FAStockToCostR) ([] :: [Filter FA.StockToCost]) 

{-# NOINLINE getFASuppliersR #-}
getFASuppliersR :: Handler Html 
getFASuppliersR = entityTableHandler (FA'R FASuppliersR) ([] :: [Filter FA.Supplier]) 

{-# NOINLINE getFASuppAllocationsR #-}
getFASuppAllocationsR :: Handler Html 
getFASuppAllocationsR = entityTableHandler (FA'R FASuppAllocationsR) ([] :: [Filter FA.SuppAllocation]) 

{-# NOINLINE getFASuppInvoiceItemsR #-}
getFASuppInvoiceItemsR :: Handler Html 
getFASuppInvoiceItemsR = entityTableHandler (FA'R FASuppInvoiceItemsR) ([] :: [Filter FA.SuppInvoiceItem]) 

{-# NOINLINE getFASuppTransR #-}
getFASuppTransR :: Handler Html 
getFASuppTransR = entityTableHandler (FA'R FASuppTransR) ([] :: [Filter FA.SuppTran]) 

{-# NOINLINE getFASysPrefsR #-}
getFASysPrefsR :: Handler Html 
getFASysPrefsR = entityTableHandler (FA'R FASysPrefsR) ([] :: [Filter FA.SysPref]) 

{-# NOINLINE getFASysTypesR #-}
getFASysTypesR :: Handler Html 
getFASysTypesR = entityTableHandler (FA'R FASysTypesR) ([] :: [Filter FA.SysType]) 

{-# NOINLINE getFATagsR #-}
getFATagsR :: Handler Html 
getFATagsR = entityTableHandler (FA'R FATagsR) ([] :: [Filter FA.Tag]) 

{-# NOINLINE getFATagAssociationsR #-}
getFATagAssociationsR :: Handler Html 
getFATagAssociationsR = entityTableHandler (FA'R FATagAssociationsR) ([] :: [Filter FA.TagAssociation]) 

{-# NOINLINE getFATaxGroupsR #-}
getFATaxGroupsR :: Handler Html 
getFATaxGroupsR = entityTableHandler (FA'R FATaxGroupsR) ([] :: [Filter FA.TaxGroup]) 

{-# NOINLINE getFATaxGroupItemsR #-}
getFATaxGroupItemsR :: Handler Html 
getFATaxGroupItemsR = entityTableHandler (FA'R FATaxGroupItemsR) ([] :: [Filter FA.TaxGroupItem]) 

{-# NOINLINE getFATaxRepR #-}
getFATaxRepR :: Handler Html 
getFATaxRepR = entityTableHandler (FA'R FATaxRepR) ([] :: [Filter FA.TaxRep]) 

{-# NOINLINE getFATaxRepLineR #-}
getFATaxRepLineR :: Handler Html 
getFATaxRepLineR = entityTableHandler (FA'R FATaxRepLineR) ([] :: [Filter FA.TaxRepLine]) 

{-# NOINLINE getFATaxRepLineAccountsR #-}
getFATaxRepLineAccountsR :: Handler Html 
getFATaxRepLineAccountsR = entityTableHandler (FA'R FATaxRepLineAccountsR) ([] :: [Filter FA.TaxRepLineAccount]) 

{-# NOINLINE getFATaxTypesR #-}
getFATaxTypesR :: Handler Html 
getFATaxTypesR = entityTableHandler (FA'R FATaxTypesR) ([] :: [Filter FA.TaxType]) 

{-# NOINLINE getFATopickR #-}
getFATopickR :: Handler Html 
getFATopickR = entityTableHandler (FA'R FATopickR) ([] :: [Filter FA.Topick]) 

{-# NOINLINE getFATransTaxDetailsR #-}
getFATransTaxDetailsR :: Handler Html 
getFATransTaxDetailsR = entityTableHandler (FA'R FATransTaxDetailsR) ([] :: [Filter FA.TransTaxDetail]) 

{-# NOINLINE getFATransToRateR #-}
getFATransToRateR :: Handler Html 
getFATransToRateR = entityTableHandler (FA'R FATransToRateR) ([] :: [Filter FA.TransToRate]) 

{-# NOINLINE getFATransToStockR #-}
getFATransToStockR :: Handler Html 
getFATransToStockR = entityTableHandler (FA'R FATransToStockR) ([] :: [Filter FA.TransToStock]) 

{-# NOINLINE getFAUseronlineR #-}
getFAUseronlineR :: Handler Html 
getFAUseronlineR = entityTableHandler (FA'R FAUseronlineR) ([] :: [Filter FA.Useronline]) 

{-# NOINLINE getFAUsersR #-}
getFAUsersR :: Handler Html 
getFAUsersR = entityTableHandler (FA'R FAUsersR) ([] :: [Filter FA.User]) 

{-# NOINLINE getFAVoidedR #-}
getFAVoidedR :: Handler Html 
getFAVoidedR = entityTableHandler (FA'R FAVoidedR) ([] :: [Filter FA.Voided]) 

{-# NOINLINE getFAWorkcentresR #-}
getFAWorkcentresR :: Handler Html 
getFAWorkcentresR = entityTableHandler (FA'R FAWorkcentresR) ([] :: [Filter FA.Workcentre]) 

{-# NOINLINE getFAWorkordersR #-}
getFAWorkordersR :: Handler Html 
getFAWorkordersR = entityTableHandler (FA'R FAWorkordersR) ([] :: [Filter FA.Workorder]) 

{-# NOINLINE getFAWoIssuesR #-}
getFAWoIssuesR :: Handler Html 
getFAWoIssuesR = entityTableHandler (FA'R FAWoIssuesR) ([] :: [Filter FA.WoIssue]) 

{-# NOINLINE getFAWoIssueItemsR #-}
getFAWoIssueItemsR :: Handler Html 
getFAWoIssueItemsR = entityTableHandler (FA'R FAWoIssueItemsR) ([] :: [Filter FA.WoIssueItem]) 

{-# NOINLINE getFAWoManufactureR #-}
getFAWoManufactureR :: Handler Html 
getFAWoManufactureR = entityTableHandler (FA'R FAWoManufactureR) ([] :: [Filter FA.WoManufacture]) 

{-# NOINLINE getFAWoRequirementsR #-}
getFAWoRequirementsR :: Handler Html 
getFAWoRequirementsR = entityTableHandler (FA'R FAWoRequirementsR) ([] :: [Filter FA.WoRequirement]) 

