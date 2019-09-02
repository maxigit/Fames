-- Warning ! This code has been generated !
-- Handler
module Handler.DC.Def where
import Import
import DC


{-# NOINLINE getDCAccesslogR #-}
getDCAccesslogR :: Handler Html 
getDCAccesslogR = entityTableHandler (DC'R DCAccesslogR) ([] :: [Filter DC.AccesslogT]) 

{-# NOINLINE getDCActionsR #-}
getDCActionsR :: Handler Html 
getDCActionsR = entityTableHandler (DC'R DCActionsR) ([] :: [Filter DC.ActionsT]) 

{-# NOINLINE getDCAdvancedHelpIndexR #-}
getDCAdvancedHelpIndexR :: Handler Html 
getDCAdvancedHelpIndexR = entityTableHandler (DC'R DCAdvancedHelpIndexR) ([] :: [Filter DC.AdvancedHelpIndexT]) 

{-# NOINLINE getDCAuthcacheP13nKeyValueR #-}
getDCAuthcacheP13nKeyValueR :: Handler Html 
getDCAuthcacheP13nKeyValueR = entityTableHandler (DC'R DCAuthcacheP13nKeyValueR) ([] :: [Filter DC.AuthcacheP13nKeyValueT]) 

{-# NOINLINE getDCAuthmapR #-}
getDCAuthmapR :: Handler Html 
getDCAuthmapR = entityTableHandler (DC'R DCAuthmapR) ([] :: [Filter DC.AuthmapT]) 

{-# NOINLINE getDCBatchR #-}
getDCBatchR :: Handler Html 
getDCBatchR = entityTableHandler (DC'R DCBatchR) ([] :: [Filter DC.BatchT]) 

{-# NOINLINE getDCBlockR #-}
getDCBlockR :: Handler Html 
getDCBlockR = entityTableHandler (DC'R DCBlockR) ([] :: [Filter DC.BlockT]) 

{-# NOINLINE getDCBlockedIpsR #-}
getDCBlockedIpsR :: Handler Html 
getDCBlockedIpsR = entityTableHandler (DC'R DCBlockedIpsR) ([] :: [Filter DC.BlockedIpsT]) 

{-# NOINLINE getDCBlockCurrentSearchR #-}
getDCBlockCurrentSearchR :: Handler Html 
getDCBlockCurrentSearchR = entityTableHandler (DC'R DCBlockCurrentSearchR) ([] :: [Filter DC.BlockCurrentSearchT]) 

{-# NOINLINE getDCBlockCustomR #-}
getDCBlockCustomR :: Handler Html 
getDCBlockCustomR = entityTableHandler (DC'R DCBlockCustomR) ([] :: [Filter DC.BlockCustomT]) 

{-# NOINLINE getDCBlockNodeTypeR #-}
getDCBlockNodeTypeR :: Handler Html 
getDCBlockNodeTypeR = entityTableHandler (DC'R DCBlockNodeTypeR) ([] :: [Filter DC.BlockNodeTypeT]) 

{-# NOINLINE getDCBlockRoleR #-}
getDCBlockRoleR :: Handler Html 
getDCBlockRoleR = entityTableHandler (DC'R DCBlockRoleR) ([] :: [Filter DC.BlockRoleT]) 

{-# NOINLINE getDCCacheR #-}
getDCCacheR :: Handler Html 
getDCCacheR = entityTableHandler (DC'R DCCacheR) ([] :: [Filter DC.CacheT]) 

{-# NOINLINE getDCCacheAdminMenuR #-}
getDCCacheAdminMenuR :: Handler Html 
getDCCacheAdminMenuR = entityTableHandler (DC'R DCCacheAdminMenuR) ([] :: [Filter DC.CacheAdminMenuT]) 

{-# NOINLINE getDCCacheAuthcacheDebugR #-}
getDCCacheAuthcacheDebugR :: Handler Html 
getDCCacheAuthcacheDebugR = entityTableHandler (DC'R DCCacheAuthcacheDebugR) ([] :: [Filter DC.CacheAuthcacheDebugT]) 

{-# NOINLINE getDCCacheAuthcacheKeyR #-}
getDCCacheAuthcacheKeyR :: Handler Html 
getDCCacheAuthcacheKeyR = entityTableHandler (DC'R DCCacheAuthcacheKeyR) ([] :: [Filter DC.CacheAuthcacheKeyT]) 

{-# NOINLINE getDCCacheAuthcacheP13nR #-}
getDCCacheAuthcacheP13nR :: Handler Html 
getDCCacheAuthcacheP13nR = entityTableHandler (DC'R DCCacheAuthcacheP13nR) ([] :: [Filter DC.CacheAuthcacheP13nT]) 

{-# NOINLINE getDCCacheBlockR #-}
getDCCacheBlockR :: Handler Html 
getDCCacheBlockR = entityTableHandler (DC'R DCCacheBlockR) ([] :: [Filter DC.CacheBlockT]) 

{-# NOINLINE getDCCacheBootstrapR #-}
getDCCacheBootstrapR :: Handler Html 
getDCCacheBootstrapR = entityTableHandler (DC'R DCCacheBootstrapR) ([] :: [Filter DC.CacheBootstrapT]) 

{-# NOINLINE getDCCacheCommerceShippingRatesR #-}
getDCCacheCommerceShippingRatesR :: Handler Html 
getDCCacheCommerceShippingRatesR = entityTableHandler (DC'R DCCacheCommerceShippingRatesR) ([] :: [Filter DC.CacheCommerceShippingRatesT]) 

{-# NOINLINE getDCCacheDisplayCacheR #-}
getDCCacheDisplayCacheR :: Handler Html 
getDCCacheDisplayCacheR = entityTableHandler (DC'R DCCacheDisplayCacheR) ([] :: [Filter DC.CacheDisplayCacheT]) 

{-# NOINLINE getDCCacheEntityCommentR #-}
getDCCacheEntityCommentR :: Handler Html 
getDCCacheEntityCommentR = entityTableHandler (DC'R DCCacheEntityCommentR) ([] :: [Filter DC.CacheEntityCommentT]) 

{-# NOINLINE getDCCacheEntityFileR #-}
getDCCacheEntityFileR :: Handler Html 
getDCCacheEntityFileR = entityTableHandler (DC'R DCCacheEntityFileR) ([] :: [Filter DC.CacheEntityFileT]) 

{-# NOINLINE getDCCacheEntityMessageR #-}
getDCCacheEntityMessageR :: Handler Html 
getDCCacheEntityMessageR = entityTableHandler (DC'R DCCacheEntityMessageR) ([] :: [Filter DC.CacheEntityMessageT]) 

{-# NOINLINE getDCCacheEntityMessageTypeR #-}
getDCCacheEntityMessageTypeR :: Handler Html 
getDCCacheEntityMessageTypeR = entityTableHandler (DC'R DCCacheEntityMessageTypeR) ([] :: [Filter DC.CacheEntityMessageTypeT]) 

{-# NOINLINE getDCCacheEntityMessageTypeCategoryR #-}
getDCCacheEntityMessageTypeCategoryR :: Handler Html 
getDCCacheEntityMessageTypeCategoryR = entityTableHandler (DC'R DCCacheEntityMessageTypeCategoryR) ([] :: [Filter DC.CacheEntityMessageTypeCategoryT]) 

{-# NOINLINE getDCCacheEntityNodeR #-}
getDCCacheEntityNodeR :: Handler Html 
getDCCacheEntityNodeR = entityTableHandler (DC'R DCCacheEntityNodeR) ([] :: [Filter DC.CacheEntityNodeT]) 

{-# NOINLINE getDCCacheEntityTaxonomyTermR #-}
getDCCacheEntityTaxonomyTermR :: Handler Html 
getDCCacheEntityTaxonomyTermR = entityTableHandler (DC'R DCCacheEntityTaxonomyTermR) ([] :: [Filter DC.CacheEntityTaxonomyTermT]) 

{-# NOINLINE getDCCacheEntityTaxonomyVocabularyR #-}
getDCCacheEntityTaxonomyVocabularyR :: Handler Html 
getDCCacheEntityTaxonomyVocabularyR = entityTableHandler (DC'R DCCacheEntityTaxonomyVocabularyR) ([] :: [Filter DC.CacheEntityTaxonomyVocabularyT]) 

{-# NOINLINE getDCCacheEntityUserR #-}
getDCCacheEntityUserR :: Handler Html 
getDCCacheEntityUserR = entityTableHandler (DC'R DCCacheEntityUserR) ([] :: [Filter DC.CacheEntityUserT]) 

{-# NOINLINE getDCCacheFieldR #-}
getDCCacheFieldR :: Handler Html 
getDCCacheFieldR = entityTableHandler (DC'R DCCacheFieldR) ([] :: [Filter DC.CacheFieldT]) 

{-# NOINLINE getDCCacheFilterR #-}
getDCCacheFilterR :: Handler Html 
getDCCacheFilterR = entityTableHandler (DC'R DCCacheFilterR) ([] :: [Filter DC.CacheFilterT]) 

{-# NOINLINE getDCCacheFormR #-}
getDCCacheFormR :: Handler Html 
getDCCacheFormR = entityTableHandler (DC'R DCCacheFormR) ([] :: [Filter DC.CacheFormT]) 

{-# NOINLINE getDCCacheImageR #-}
getDCCacheImageR :: Handler Html 
getDCCacheImageR = entityTableHandler (DC'R DCCacheImageR) ([] :: [Filter DC.CacheImageT]) 

{-# NOINLINE getDCCacheLibrariesR #-}
getDCCacheLibrariesR :: Handler Html 
getDCCacheLibrariesR = entityTableHandler (DC'R DCCacheLibrariesR) ([] :: [Filter DC.CacheLibrariesT]) 

{-# NOINLINE getDCCacheMenuR #-}
getDCCacheMenuR :: Handler Html 
getDCCacheMenuR = entityTableHandler (DC'R DCCacheMenuR) ([] :: [Filter DC.CacheMenuT]) 

{-# NOINLINE getDCCacheMetatagR #-}
getDCCacheMetatagR :: Handler Html 
getDCCacheMetatagR = entityTableHandler (DC'R DCCacheMetatagR) ([] :: [Filter DC.CacheMetatagT]) 

{-# NOINLINE getDCCachePageR #-}
getDCCachePageR :: Handler Html 
getDCCachePageR = entityTableHandler (DC'R DCCachePageR) ([] :: [Filter DC.CachePageT]) 

{-# NOINLINE getDCCachePathR #-}
getDCCachePathR :: Handler Html 
getDCCachePathR = entityTableHandler (DC'R DCCachePathR) ([] :: [Filter DC.CachePathT]) 

{-# NOINLINE getDCCachePathAliasR #-}
getDCCachePathAliasR :: Handler Html 
getDCCachePathAliasR = entityTableHandler (DC'R DCCachePathAliasR) ([] :: [Filter DC.CachePathAliasT]) 

{-# NOINLINE getDCCachePathSourceR #-}
getDCCachePathSourceR :: Handler Html 
getDCCachePathSourceR = entityTableHandler (DC'R DCCachePathSourceR) ([] :: [Filter DC.CachePathSourceT]) 

{-# NOINLINE getDCCacheRulesR #-}
getDCCacheRulesR :: Handler Html 
getDCCacheRulesR = entityTableHandler (DC'R DCCacheRulesR) ([] :: [Filter DC.CacheRulesT]) 

{-# NOINLINE getDCCacheTokenR #-}
getDCCacheTokenR :: Handler Html 
getDCCacheTokenR = entityTableHandler (DC'R DCCacheTokenR) ([] :: [Filter DC.CacheTokenT]) 

{-# NOINLINE getDCCacheUpdateR #-}
getDCCacheUpdateR :: Handler Html 
getDCCacheUpdateR = entityTableHandler (DC'R DCCacheUpdateR) ([] :: [Filter DC.CacheUpdateT]) 

{-# NOINLINE getDCCacheViewsR #-}
getDCCacheViewsR :: Handler Html 
getDCCacheViewsR = entityTableHandler (DC'R DCCacheViewsR) ([] :: [Filter DC.CacheViewsT]) 

{-# NOINLINE getDCCacheViewsDataR #-}
getDCCacheViewsDataR :: Handler Html 
getDCCacheViewsDataR = entityTableHandler (DC'R DCCacheViewsDataR) ([] :: [Filter DC.CacheViewsDataT]) 

{-# NOINLINE getDCCmpMenuPermsR #-}
getDCCmpMenuPermsR :: Handler Html 
getDCCmpMenuPermsR = entityTableHandler (DC'R DCCmpMenuPermsR) ([] :: [Filter DC.CmpMenuPermsT]) 

{-# NOINLINE getDCCmpPermissionsR #-}
getDCCmpPermissionsR :: Handler Html 
getDCCmpPermissionsR = entityTableHandler (DC'R DCCmpPermissionsR) ([] :: [Filter DC.CmpPermissionsT]) 

{-# NOINLINE getDCCommentR #-}
getDCCommentR :: Handler Html 
getDCCommentR = entityTableHandler (DC'R DCCommentR) ([] :: [Filter DC.CommentT]) 

{-# NOINLINE getDCCommerceAddressbookDefaultsR #-}
getDCCommerceAddressbookDefaultsR :: Handler Html 
getDCCommerceAddressbookDefaultsR = entityTableHandler (DC'R DCCommerceAddressbookDefaultsR) ([] :: [Filter DC.CommerceAddressbookDefaultsT]) 

{-# NOINLINE getDCCommerceAutoskuPatternsR #-}
getDCCommerceAutoskuPatternsR :: Handler Html 
getDCCommerceAutoskuPatternsR = entityTableHandler (DC'R DCCommerceAutoskuPatternsR) ([] :: [Filter DC.CommerceAutoskuPatternsT]) 

{-# NOINLINE getDCCommerceCalculatedPriceR #-}
getDCCommerceCalculatedPriceR :: Handler Html 
getDCCommerceCalculatedPriceR = entityTableHandler (DC'R DCCommerceCalculatedPriceR) ([] :: [Filter DC.CommerceCalculatedPriceT]) 

{-# NOINLINE getDCCommerceCheckoutPaneR #-}
getDCCommerceCheckoutPaneR :: Handler Html 
getDCCommerceCheckoutPaneR = entityTableHandler (DC'R DCCommerceCheckoutPaneR) ([] :: [Filter DC.CommerceCheckoutPaneT]) 

{-# NOINLINE getDCCommerceCustomerProfileR #-}
getDCCommerceCustomerProfileR :: Handler Html 
getDCCommerceCustomerProfileR = entityTableHandler (DC'R DCCommerceCustomerProfileR) ([] :: [Filter DC.CommerceCustomerProfileT]) 

{-# NOINLINE getDCCommerceCustomerProfileRevisionR #-}
getDCCommerceCustomerProfileRevisionR :: Handler Html 
getDCCommerceCustomerProfileRevisionR = entityTableHandler (DC'R DCCommerceCustomerProfileRevisionR) ([] :: [Filter DC.CommerceCustomerProfileRevisionT]) 

{-# NOINLINE getDCCommerceDiscountR #-}
getDCCommerceDiscountR :: Handler Html 
getDCCommerceDiscountR = entityTableHandler (DC'R DCCommerceDiscountR) ([] :: [Filter DC.CommerceDiscountT]) 

{-# NOINLINE getDCCommerceDiscountOfferR #-}
getDCCommerceDiscountOfferR :: Handler Html 
getDCCommerceDiscountOfferR = entityTableHandler (DC'R DCCommerceDiscountOfferR) ([] :: [Filter DC.CommerceDiscountOfferT]) 

{-# NOINLINE getDCCommerceFlatRateServiceR #-}
getDCCommerceFlatRateServiceR :: Handler Html 
getDCCommerceFlatRateServiceR = entityTableHandler (DC'R DCCommerceFlatRateServiceR) ([] :: [Filter DC.CommerceFlatRateServiceT]) 

{-# NOINLINE getDCCommerceLineItemR #-}
getDCCommerceLineItemR :: Handler Html 
getDCCommerceLineItemR = entityTableHandler (DC'R DCCommerceLineItemR) ([] :: [Filter DC.CommerceLineItemT]) 

{-# NOINLINE getDCCommerceOrderR #-}
getDCCommerceOrderR :: Handler Html 
getDCCommerceOrderR = entityTableHandler (DC'R DCCommerceOrderR) ([] :: [Filter DC.CommerceOrderT]) 

{-# NOINLINE getDCCommerceOrderRevisionR #-}
getDCCommerceOrderRevisionR :: Handler Html 
getDCCommerceOrderRevisionR = entityTableHandler (DC'R DCCommerceOrderRevisionR) ([] :: [Filter DC.CommerceOrderRevisionT]) 

{-# NOINLINE getDCCommercePaymentTransactionR #-}
getDCCommercePaymentTransactionR :: Handler Html 
getDCCommercePaymentTransactionR = entityTableHandler (DC'R DCCommercePaymentTransactionR) ([] :: [Filter DC.CommercePaymentTransactionT]) 

{-# NOINLINE getDCCommercePaymentTransactionRevisionR #-}
getDCCommercePaymentTransactionRevisionR :: Handler Html 
getDCCommercePaymentTransactionRevisionR = entityTableHandler (DC'R DCCommercePaymentTransactionRevisionR) ([] :: [Filter DC.CommercePaymentTransactionRevisionT]) 

{-# NOINLINE getDCCommerceProductR #-}
getDCCommerceProductR :: Handler Html 
getDCCommerceProductR = entityTableHandler (DC'R DCCommerceProductR) ([] :: [Filter DC.CommerceProductT]) 

{-# NOINLINE getDCCommerceProductRevisionR #-}
getDCCommerceProductRevisionR :: Handler Html 
getDCCommerceProductRevisionR = entityTableHandler (DC'R DCCommerceProductRevisionR) ([] :: [Filter DC.CommerceProductRevisionT]) 

{-# NOINLINE getDCCommerceProductTypeR #-}
getDCCommerceProductTypeR :: Handler Html 
getDCCommerceProductTypeR = entityTableHandler (DC'R DCCommerceProductTypeR) ([] :: [Filter DC.CommerceProductTypeT]) 

{-# NOINLINE getDCCommerceTaxRateR #-}
getDCCommerceTaxRateR :: Handler Html 
getDCCommerceTaxRateR = entityTableHandler (DC'R DCCommerceTaxRateR) ([] :: [Filter DC.CommerceTaxRateT]) 

{-# NOINLINE getDCCommerceTaxTypeR #-}
getDCCommerceTaxTypeR :: Handler Html 
getDCCommerceTaxTypeR = entityTableHandler (DC'R DCCommerceTaxTypeR) ([] :: [Filter DC.CommerceTaxTypeT]) 

{-# NOINLINE getDCContactR #-}
getDCContactR :: Handler Html 
getDCContactR = entityTableHandler (DC'R DCContactR) ([] :: [Filter DC.ContactT]) 

{-# NOINLINE getDCCtoolsCssCacheR #-}
getDCCtoolsCssCacheR :: Handler Html 
getDCCtoolsCssCacheR = entityTableHandler (DC'R DCCtoolsCssCacheR) ([] :: [Filter DC.CtoolsCssCacheT]) 

{-# NOINLINE getDCCtoolsObjectCacheR #-}
getDCCtoolsObjectCacheR :: Handler Html 
getDCCtoolsObjectCacheR = entityTableHandler (DC'R DCCtoolsObjectCacheR) ([] :: [Filter DC.CtoolsObjectCacheT]) 

{-# NOINLINE getDCCurrentSearchR #-}
getDCCurrentSearchR :: Handler Html 
getDCCurrentSearchR = entityTableHandler (DC'R DCCurrentSearchR) ([] :: [Filter DC.CurrentSearchT]) 

{-# NOINLINE getDCDateFormatsR #-}
getDCDateFormatsR :: Handler Html 
getDCDateFormatsR = entityTableHandler (DC'R DCDateFormatsR) ([] :: [Filter DC.DateFormatsT]) 

{-# NOINLINE getDCDateFormatLocaleR #-}
getDCDateFormatLocaleR :: Handler Html 
getDCDateFormatLocaleR = entityTableHandler (DC'R DCDateFormatLocaleR) ([] :: [Filter DC.DateFormatLocaleT]) 

{-# NOINLINE getDCDateFormatTypeR #-}
getDCDateFormatTypeR :: Handler Html 
getDCDateFormatTypeR = entityTableHandler (DC'R DCDateFormatTypeR) ([] :: [Filter DC.DateFormatTypeT]) 

{-# NOINLINE getDCFacetapiR #-}
getDCFacetapiR :: Handler Html 
getDCFacetapiR = entityTableHandler (DC'R DCFacetapiR) ([] :: [Filter DC.FacetapiT]) 

{-# NOINLINE getDCFeedsImporterR #-}
getDCFeedsImporterR :: Handler Html 
getDCFeedsImporterR = entityTableHandler (DC'R DCFeedsImporterR) ([] :: [Filter DC.FeedsImporterT]) 

{-# NOINLINE getDCFeedsItemR #-}
getDCFeedsItemR :: Handler Html 
getDCFeedsItemR = entityTableHandler (DC'R DCFeedsItemR) ([] :: [Filter DC.FeedsItemT]) 

{-# NOINLINE getDCFeedsLogR #-}
getDCFeedsLogR :: Handler Html 
getDCFeedsLogR = entityTableHandler (DC'R DCFeedsLogR) ([] :: [Filter DC.FeedsLogT]) 

{-# NOINLINE getDCFeedsPushSubscriptionsR #-}
getDCFeedsPushSubscriptionsR :: Handler Html 
getDCFeedsPushSubscriptionsR = entityTableHandler (DC'R DCFeedsPushSubscriptionsR) ([] :: [Filter DC.FeedsPushSubscriptionsT]) 

{-# NOINLINE getDCFeedsSourceR #-}
getDCFeedsSourceR :: Handler Html 
getDCFeedsSourceR = entityTableHandler (DC'R DCFeedsSourceR) ([] :: [Filter DC.FeedsSourceT]) 

{-# NOINLINE getDCFeedsTamperR #-}
getDCFeedsTamperR :: Handler Html 
getDCFeedsTamperR = entityTableHandler (DC'R DCFeedsTamperR) ([] :: [Filter DC.FeedsTamperT]) 

{-# NOINLINE getDCFieldConfigR #-}
getDCFieldConfigR :: Handler Html 
getDCFieldConfigR = entityTableHandler (DC'R DCFieldConfigR) ([] :: [Filter DC.FieldConfigT]) 

{-# NOINLINE getDCFieldConfigInstanceR #-}
getDCFieldConfigInstanceR :: Handler Html 
getDCFieldConfigInstanceR = entityTableHandler (DC'R DCFieldConfigInstanceR) ([] :: [Filter DC.FieldConfigInstanceT]) 

{-# NOINLINE getDCFieldDataBodyR #-}
getDCFieldDataBodyR :: Handler Html 
getDCFieldDataBodyR = entityTableHandler (DC'R DCFieldDataBodyR) ([] :: [Filter DC.FieldDataBodyT]) 

{-# NOINLINE getDCFieldDataCommentBodyR #-}
getDCFieldDataCommentBodyR :: Handler Html 
getDCFieldDataCommentBodyR = entityTableHandler (DC'R DCFieldDataCommentBodyR) ([] :: [Filter DC.FieldDataCommentBodyT]) 

{-# NOINLINE getDCFieldDataCommerceCustomerAddressR #-}
getDCFieldDataCommerceCustomerAddressR :: Handler Html 
getDCFieldDataCommerceCustomerAddressR = entityTableHandler (DC'R DCFieldDataCommerceCustomerAddressR) ([] :: [Filter DC.FieldDataCommerceCustomerAddressT]) 

{-# NOINLINE getDCFieldDataCommerceCustomerBillingR #-}
getDCFieldDataCommerceCustomerBillingR :: Handler Html 
getDCFieldDataCommerceCustomerBillingR = entityTableHandler (DC'R DCFieldDataCommerceCustomerBillingR) ([] :: [Filter DC.FieldDataCommerceCustomerBillingT]) 

{-# NOINLINE getDCFieldDataCommerceCustomerShippingR #-}
getDCFieldDataCommerceCustomerShippingR :: Handler Html 
getDCFieldDataCommerceCustomerShippingR = entityTableHandler (DC'R DCFieldDataCommerceCustomerShippingR) ([] :: [Filter DC.FieldDataCommerceCustomerShippingT]) 

{-# NOINLINE getDCFieldDataCommerceDiscountsR #-}
getDCFieldDataCommerceDiscountsR :: Handler Html 
getDCFieldDataCommerceDiscountsR = entityTableHandler (DC'R DCFieldDataCommerceDiscountsR) ([] :: [Filter DC.FieldDataCommerceDiscountsT]) 

{-# NOINLINE getDCFieldDataCommerceDiscountDateR #-}
getDCFieldDataCommerceDiscountDateR :: Handler Html 
getDCFieldDataCommerceDiscountDateR = entityTableHandler (DC'R DCFieldDataCommerceDiscountDateR) ([] :: [Filter DC.FieldDataCommerceDiscountDateT]) 

{-# NOINLINE getDCFieldDataCommerceDiscountOfferR #-}
getDCFieldDataCommerceDiscountOfferR :: Handler Html 
getDCFieldDataCommerceDiscountOfferR = entityTableHandler (DC'R DCFieldDataCommerceDiscountOfferR) ([] :: [Filter DC.FieldDataCommerceDiscountOfferT]) 

{-# NOINLINE getDCFieldDataCommerceDisplayPathR #-}
getDCFieldDataCommerceDisplayPathR :: Handler Html 
getDCFieldDataCommerceDisplayPathR = entityTableHandler (DC'R DCFieldDataCommerceDisplayPathR) ([] :: [Filter DC.FieldDataCommerceDisplayPathT]) 

{-# NOINLINE getDCFieldDataCommerceFixedAmountR #-}
getDCFieldDataCommerceFixedAmountR :: Handler Html 
getDCFieldDataCommerceFixedAmountR = entityTableHandler (DC'R DCFieldDataCommerceFixedAmountR) ([] :: [Filter DC.FieldDataCommerceFixedAmountT]) 

{-# NOINLINE getDCFieldDataCommerceFreeProductsR #-}
getDCFieldDataCommerceFreeProductsR :: Handler Html 
getDCFieldDataCommerceFreeProductsR = entityTableHandler (DC'R DCFieldDataCommerceFreeProductsR) ([] :: [Filter DC.FieldDataCommerceFreeProductsT]) 

{-# NOINLINE getDCFieldDataCommerceFreeShippingR #-}
getDCFieldDataCommerceFreeShippingR :: Handler Html 
getDCFieldDataCommerceFreeShippingR = entityTableHandler (DC'R DCFieldDataCommerceFreeShippingR) ([] :: [Filter DC.FieldDataCommerceFreeShippingT]) 

{-# NOINLINE getDCFieldDataCommerceLineItemsR #-}
getDCFieldDataCommerceLineItemsR :: Handler Html 
getDCFieldDataCommerceLineItemsR = entityTableHandler (DC'R DCFieldDataCommerceLineItemsR) ([] :: [Filter DC.FieldDataCommerceLineItemsT]) 

{-# NOINLINE getDCFieldDataCommerceOrderTotalR #-}
getDCFieldDataCommerceOrderTotalR :: Handler Html 
getDCFieldDataCommerceOrderTotalR = entityTableHandler (DC'R DCFieldDataCommerceOrderTotalR) ([] :: [Filter DC.FieldDataCommerceOrderTotalT]) 

{-# NOINLINE getDCFieldDataCommercePercentageR #-}
getDCFieldDataCommercePercentageR :: Handler Html 
getDCFieldDataCommercePercentageR = entityTableHandler (DC'R DCFieldDataCommercePercentageR) ([] :: [Filter DC.FieldDataCommercePercentageT]) 

{-# NOINLINE getDCFieldDataCommercePriceR #-}
getDCFieldDataCommercePriceR :: Handler Html 
getDCFieldDataCommercePriceR = entityTableHandler (DC'R DCFieldDataCommercePriceR) ([] :: [Filter DC.FieldDataCommercePriceT]) 

{-# NOINLINE getDCFieldDataCommerceProductR #-}
getDCFieldDataCommerceProductR :: Handler Html 
getDCFieldDataCommerceProductR = entityTableHandler (DC'R DCFieldDataCommerceProductR) ([] :: [Filter DC.FieldDataCommerceProductT]) 

{-# NOINLINE getDCFieldDataCommerceShippingServiceR #-}
getDCFieldDataCommerceShippingServiceR :: Handler Html 
getDCFieldDataCommerceShippingServiceR = entityTableHandler (DC'R DCFieldDataCommerceShippingServiceR) ([] :: [Filter DC.FieldDataCommerceShippingServiceT]) 

{-# NOINLINE getDCFieldDataCommerceTotalR #-}
getDCFieldDataCommerceTotalR :: Handler Html 
getDCFieldDataCommerceTotalR = entityTableHandler (DC'R DCFieldDataCommerceTotalR) ([] :: [Filter DC.FieldDataCommerceTotalT]) 

{-# NOINLINE getDCFieldDataCommerceUnitPriceR #-}
getDCFieldDataCommerceUnitPriceR :: Handler Html 
getDCFieldDataCommerceUnitPriceR = entityTableHandler (DC'R DCFieldDataCommerceUnitPriceR) ([] :: [Filter DC.FieldDataCommerceUnitPriceT]) 

{-# NOINLINE getDCFieldDataFieldBrandR #-}
getDCFieldDataFieldBrandR :: Handler Html 
getDCFieldDataFieldBrandR = entityTableHandler (DC'R DCFieldDataFieldBrandR) ([] :: [Filter DC.FieldDataFieldBrandT]) 

{-# NOINLINE getDCFieldDataFieldCollectionR #-}
getDCFieldDataFieldCollectionR :: Handler Html 
getDCFieldDataFieldCollectionR = entityTableHandler (DC'R DCFieldDataFieldCollectionR) ([] :: [Filter DC.FieldDataFieldCollectionT]) 

{-# NOINLINE getDCFieldDataFieldColourR #-}
getDCFieldDataFieldColourR :: Handler Html 
getDCFieldDataFieldColourR = entityTableHandler (DC'R DCFieldDataFieldColourR) ([] :: [Filter DC.FieldDataFieldColourT]) 

{-# NOINLINE getDCFieldDataFieldColourCodeR #-}
getDCFieldDataFieldColourCodeR :: Handler Html 
getDCFieldDataFieldColourCodeR = entityTableHandler (DC'R DCFieldDataFieldColourCodeR) ([] :: [Filter DC.FieldDataFieldColourCodeT]) 

{-# NOINLINE getDCFieldDataFieldHeadlineR #-}
getDCFieldDataFieldHeadlineR :: Handler Html 
getDCFieldDataFieldHeadlineR = entityTableHandler (DC'R DCFieldDataFieldHeadlineR) ([] :: [Filter DC.FieldDataFieldHeadlineT]) 

{-# NOINLINE getDCFieldDataFieldImageR #-}
getDCFieldDataFieldImageR :: Handler Html 
getDCFieldDataFieldImageR = entityTableHandler (DC'R DCFieldDataFieldImageR) ([] :: [Filter DC.FieldDataFieldImageT]) 

{-# NOINLINE getDCFieldDataFieldImagesR #-}
getDCFieldDataFieldImagesR :: Handler Html 
getDCFieldDataFieldImagesR = entityTableHandler (DC'R DCFieldDataFieldImagesR) ([] :: [Filter DC.FieldDataFieldImagesT]) 

{-# NOINLINE getDCFieldDataFieldLinkR #-}
getDCFieldDataFieldLinkR :: Handler Html 
getDCFieldDataFieldLinkR = entityTableHandler (DC'R DCFieldDataFieldLinkR) ([] :: [Filter DC.FieldDataFieldLinkT]) 

{-# NOINLINE getDCFieldDataFieldPricePl01R #-}
getDCFieldDataFieldPricePl01R :: Handler Html 
getDCFieldDataFieldPricePl01R = entityTableHandler (DC'R DCFieldDataFieldPricePl01R) ([] :: [Filter DC.FieldDataFieldPricePl01T]) 

{-# NOINLINE getDCFieldDataFieldPricePl02R #-}
getDCFieldDataFieldPricePl02R :: Handler Html 
getDCFieldDataFieldPricePl02R = entityTableHandler (DC'R DCFieldDataFieldPricePl02R) ([] :: [Filter DC.FieldDataFieldPricePl02T]) 

{-# NOINLINE getDCFieldDataFieldPricePl03R #-}
getDCFieldDataFieldPricePl03R :: Handler Html 
getDCFieldDataFieldPricePl03R = entityTableHandler (DC'R DCFieldDataFieldPricePl03R) ([] :: [Filter DC.FieldDataFieldPricePl03T]) 

{-# NOINLINE getDCFieldDataFieldPricePl04R #-}
getDCFieldDataFieldPricePl04R :: Handler Html 
getDCFieldDataFieldPricePl04R = entityTableHandler (DC'R DCFieldDataFieldPricePl04R) ([] :: [Filter DC.FieldDataFieldPricePl04T]) 

{-# NOINLINE getDCFieldDataFieldPricePl05R #-}
getDCFieldDataFieldPricePl05R :: Handler Html 
getDCFieldDataFieldPricePl05R = entityTableHandler (DC'R DCFieldDataFieldPricePl05R) ([] :: [Filter DC.FieldDataFieldPricePl05T]) 

{-# NOINLINE getDCFieldDataFieldPricePl06R #-}
getDCFieldDataFieldPricePl06R :: Handler Html 
getDCFieldDataFieldPricePl06R = entityTableHandler (DC'R DCFieldDataFieldPricePl06R) ([] :: [Filter DC.FieldDataFieldPricePl06T]) 

{-# NOINLINE getDCFieldDataFieldPricePl07R #-}
getDCFieldDataFieldPricePl07R :: Handler Html 
getDCFieldDataFieldPricePl07R = entityTableHandler (DC'R DCFieldDataFieldPricePl07R) ([] :: [Filter DC.FieldDataFieldPricePl07T]) 

{-# NOINLINE getDCFieldDataFieldPricePl08R #-}
getDCFieldDataFieldPricePl08R :: Handler Html 
getDCFieldDataFieldPricePl08R = entityTableHandler (DC'R DCFieldDataFieldPricePl08R) ([] :: [Filter DC.FieldDataFieldPricePl08T]) 

{-# NOINLINE getDCFieldDataFieldPricePl09R #-}
getDCFieldDataFieldPricePl09R :: Handler Html 
getDCFieldDataFieldPricePl09R = entityTableHandler (DC'R DCFieldDataFieldPricePl09R) ([] :: [Filter DC.FieldDataFieldPricePl09T]) 

{-# NOINLINE getDCFieldDataFieldPricePl10R #-}
getDCFieldDataFieldPricePl10R :: Handler Html 
getDCFieldDataFieldPricePl10R = entityTableHandler (DC'R DCFieldDataFieldPricePl10R) ([] :: [Filter DC.FieldDataFieldPricePl10T]) 

{-# NOINLINE getDCFieldDataFieldPricePl11R #-}
getDCFieldDataFieldPricePl11R :: Handler Html 
getDCFieldDataFieldPricePl11R = entityTableHandler (DC'R DCFieldDataFieldPricePl11R) ([] :: [Filter DC.FieldDataFieldPricePl11T]) 

{-# NOINLINE getDCFieldDataFieldPricePl12R #-}
getDCFieldDataFieldPricePl12R :: Handler Html 
getDCFieldDataFieldPricePl12R = entityTableHandler (DC'R DCFieldDataFieldPricePl12R) ([] :: [Filter DC.FieldDataFieldPricePl12T]) 

{-# NOINLINE getDCFieldDataFieldPricePl13R #-}
getDCFieldDataFieldPricePl13R :: Handler Html 
getDCFieldDataFieldPricePl13R = entityTableHandler (DC'R DCFieldDataFieldPricePl13R) ([] :: [Filter DC.FieldDataFieldPricePl13T]) 

{-# NOINLINE getDCFieldDataFieldPricePl14R #-}
getDCFieldDataFieldPricePl14R :: Handler Html 
getDCFieldDataFieldPricePl14R = entityTableHandler (DC'R DCFieldDataFieldPricePl14R) ([] :: [Filter DC.FieldDataFieldPricePl14T]) 

{-# NOINLINE getDCFieldDataFieldProductR #-}
getDCFieldDataFieldProductR :: Handler Html 
getDCFieldDataFieldProductR = entityTableHandler (DC'R DCFieldDataFieldProductR) ([] :: [Filter DC.FieldDataFieldProductT]) 

{-# NOINLINE getDCFieldDataFieldProductCategoryR #-}
getDCFieldDataFieldProductCategoryR :: Handler Html 
getDCFieldDataFieldProductCategoryR = entityTableHandler (DC'R DCFieldDataFieldProductCategoryR) ([] :: [Filter DC.FieldDataFieldProductCategoryT]) 

{-# NOINLINE getDCFieldDataFieldRequiredDeliveryDateR #-}
getDCFieldDataFieldRequiredDeliveryDateR :: Handler Html 
getDCFieldDataFieldRequiredDeliveryDateR = entityTableHandler (DC'R DCFieldDataFieldRequiredDeliveryDateR) ([] :: [Filter DC.FieldDataFieldRequiredDeliveryDateT]) 

{-# NOINLINE getDCFieldDataFieldRgbR #-}
getDCFieldDataFieldRgbR :: Handler Html 
getDCFieldDataFieldRgbR = entityTableHandler (DC'R DCFieldDataFieldRgbR) ([] :: [Filter DC.FieldDataFieldRgbT]) 

{-# NOINLINE getDCFieldDataFieldSpecialRequestR #-}
getDCFieldDataFieldSpecialRequestR :: Handler Html 
getDCFieldDataFieldSpecialRequestR = entityTableHandler (DC'R DCFieldDataFieldSpecialRequestR) ([] :: [Filter DC.FieldDataFieldSpecialRequestT]) 

{-# NOINLINE getDCFieldDataFieldStockStatusR #-}
getDCFieldDataFieldStockStatusR :: Handler Html 
getDCFieldDataFieldStockStatusR = entityTableHandler (DC'R DCFieldDataFieldStockStatusR) ([] :: [Filter DC.FieldDataFieldStockStatusT]) 

{-# NOINLINE getDCFieldDataFieldTaglineR #-}
getDCFieldDataFieldTaglineR :: Handler Html 
getDCFieldDataFieldTaglineR = entityTableHandler (DC'R DCFieldDataFieldTaglineR) ([] :: [Filter DC.FieldDataFieldTaglineT]) 

{-# NOINLINE getDCFieldDataFieldTrimColourR #-}
getDCFieldDataFieldTrimColourR :: Handler Html 
getDCFieldDataFieldTrimColourR = entityTableHandler (DC'R DCFieldDataFieldTrimColourR) ([] :: [Filter DC.FieldDataFieldTrimColourT]) 

{-# NOINLINE getDCFieldDataFieldWholesalePriceR #-}
getDCFieldDataFieldWholesalePriceR :: Handler Html 
getDCFieldDataFieldWholesalePriceR = entityTableHandler (DC'R DCFieldDataFieldWholesalePriceR) ([] :: [Filter DC.FieldDataFieldWholesalePriceT]) 

{-# NOINLINE getDCFieldDataInlineConditionsR #-}
getDCFieldDataInlineConditionsR :: Handler Html 
getDCFieldDataInlineConditionsR = entityTableHandler (DC'R DCFieldDataInlineConditionsR) ([] :: [Filter DC.FieldDataInlineConditionsT]) 

{-# NOINLINE getDCFieldDataMessageCommerceBodyR #-}
getDCFieldDataMessageCommerceBodyR :: Handler Html 
getDCFieldDataMessageCommerceBodyR = entityTableHandler (DC'R DCFieldDataMessageCommerceBodyR) ([] :: [Filter DC.FieldDataMessageCommerceBodyT]) 

{-# NOINLINE getDCFieldDataMessageCommerceLineItemR #-}
getDCFieldDataMessageCommerceLineItemR :: Handler Html 
getDCFieldDataMessageCommerceLineItemR = entityTableHandler (DC'R DCFieldDataMessageCommerceLineItemR) ([] :: [Filter DC.FieldDataMessageCommerceLineItemT]) 

{-# NOINLINE getDCFieldDataMessageCommerceOrderR #-}
getDCFieldDataMessageCommerceOrderR :: Handler Html 
getDCFieldDataMessageCommerceOrderR = entityTableHandler (DC'R DCFieldDataMessageCommerceOrderR) ([] :: [Filter DC.FieldDataMessageCommerceOrderT]) 

{-# NOINLINE getDCFieldDataMessageCommercePaymentR #-}
getDCFieldDataMessageCommercePaymentR :: Handler Html 
getDCFieldDataMessageCommercePaymentR = entityTableHandler (DC'R DCFieldDataMessageCommercePaymentR) ([] :: [Filter DC.FieldDataMessageCommercePaymentT]) 

{-# NOINLINE getDCFieldDataMessageOrderDisplayNameR #-}
getDCFieldDataMessageOrderDisplayNameR :: Handler Html 
getDCFieldDataMessageOrderDisplayNameR = entityTableHandler (DC'R DCFieldDataMessageOrderDisplayNameR) ([] :: [Filter DC.FieldDataMessageOrderDisplayNameT]) 

{-# NOINLINE getDCFieldDataMessageTextR #-}
getDCFieldDataMessageTextR :: Handler Html 
getDCFieldDataMessageTextR = entityTableHandler (DC'R DCFieldDataMessageTextR) ([] :: [Filter DC.FieldDataMessageTextT]) 

{-# NOINLINE getDCFieldDataMessageTextSubjectR #-}
getDCFieldDataMessageTextSubjectR :: Handler Html 
getDCFieldDataMessageTextSubjectR = entityTableHandler (DC'R DCFieldDataMessageTextSubjectR) ([] :: [Filter DC.FieldDataMessageTextSubjectT]) 

{-# NOINLINE getDCFieldDataTitleFieldR #-}
getDCFieldDataTitleFieldR :: Handler Html 
getDCFieldDataTitleFieldR = entityTableHandler (DC'R DCFieldDataTitleFieldR) ([] :: [Filter DC.FieldDataTitleFieldT]) 

{-# NOINLINE getDCFieldDeletedData11R #-}
getDCFieldDeletedData11R :: Handler Html 
getDCFieldDeletedData11R = entityTableHandler (DC'R DCFieldDeletedData11R) ([] :: [Filter DC.FieldDeletedData11T]) 

{-# NOINLINE getDCFieldDeletedData22R #-}
getDCFieldDeletedData22R :: Handler Html 
getDCFieldDeletedData22R = entityTableHandler (DC'R DCFieldDeletedData22R) ([] :: [Filter DC.FieldDeletedData22T]) 

{-# NOINLINE getDCFieldDeletedData23R #-}
getDCFieldDeletedData23R :: Handler Html 
getDCFieldDeletedData23R = entityTableHandler (DC'R DCFieldDeletedData23R) ([] :: [Filter DC.FieldDeletedData23T]) 

{-# NOINLINE getDCFieldDeletedData24R #-}
getDCFieldDeletedData24R :: Handler Html 
getDCFieldDeletedData24R = entityTableHandler (DC'R DCFieldDeletedData24R) ([] :: [Filter DC.FieldDeletedData24T]) 

{-# NOINLINE getDCFieldDeletedData25R #-}
getDCFieldDeletedData25R :: Handler Html 
getDCFieldDeletedData25R = entityTableHandler (DC'R DCFieldDeletedData25R) ([] :: [Filter DC.FieldDeletedData25T]) 

{-# NOINLINE getDCFieldDeletedRevision11R #-}
getDCFieldDeletedRevision11R :: Handler Html 
getDCFieldDeletedRevision11R = entityTableHandler (DC'R DCFieldDeletedRevision11R) ([] :: [Filter DC.FieldDeletedRevision11T]) 

{-# NOINLINE getDCFieldDeletedRevision22R #-}
getDCFieldDeletedRevision22R :: Handler Html 
getDCFieldDeletedRevision22R = entityTableHandler (DC'R DCFieldDeletedRevision22R) ([] :: [Filter DC.FieldDeletedRevision22T]) 

{-# NOINLINE getDCFieldDeletedRevision23R #-}
getDCFieldDeletedRevision23R :: Handler Html 
getDCFieldDeletedRevision23R = entityTableHandler (DC'R DCFieldDeletedRevision23R) ([] :: [Filter DC.FieldDeletedRevision23T]) 

{-# NOINLINE getDCFieldDeletedRevision24R #-}
getDCFieldDeletedRevision24R :: Handler Html 
getDCFieldDeletedRevision24R = entityTableHandler (DC'R DCFieldDeletedRevision24R) ([] :: [Filter DC.FieldDeletedRevision24T]) 

{-# NOINLINE getDCFieldDeletedRevision25R #-}
getDCFieldDeletedRevision25R :: Handler Html 
getDCFieldDeletedRevision25R = entityTableHandler (DC'R DCFieldDeletedRevision25R) ([] :: [Filter DC.FieldDeletedRevision25T]) 

{-# NOINLINE getDCFieldGroupR #-}
getDCFieldGroupR :: Handler Html 
getDCFieldGroupR = entityTableHandler (DC'R DCFieldGroupR) ([] :: [Filter DC.FieldGroupT]) 

{-# NOINLINE getDCFieldRevisionBodyR #-}
getDCFieldRevisionBodyR :: Handler Html 
getDCFieldRevisionBodyR = entityTableHandler (DC'R DCFieldRevisionBodyR) ([] :: [Filter DC.FieldRevisionBodyT]) 

{-# NOINLINE getDCFieldRevisionCommentBodyR #-}
getDCFieldRevisionCommentBodyR :: Handler Html 
getDCFieldRevisionCommentBodyR = entityTableHandler (DC'R DCFieldRevisionCommentBodyR) ([] :: [Filter DC.FieldRevisionCommentBodyT]) 

{-# NOINLINE getDCFieldRevisionCommerceCustomerAddressR #-}
getDCFieldRevisionCommerceCustomerAddressR :: Handler Html 
getDCFieldRevisionCommerceCustomerAddressR = entityTableHandler (DC'R DCFieldRevisionCommerceCustomerAddressR) ([] :: [Filter DC.FieldRevisionCommerceCustomerAddressT]) 

{-# NOINLINE getDCFieldRevisionCommerceCustomerBillingR #-}
getDCFieldRevisionCommerceCustomerBillingR :: Handler Html 
getDCFieldRevisionCommerceCustomerBillingR = entityTableHandler (DC'R DCFieldRevisionCommerceCustomerBillingR) ([] :: [Filter DC.FieldRevisionCommerceCustomerBillingT]) 

{-# NOINLINE getDCFieldRevisionCommerceCustomerShippingR #-}
getDCFieldRevisionCommerceCustomerShippingR :: Handler Html 
getDCFieldRevisionCommerceCustomerShippingR = entityTableHandler (DC'R DCFieldRevisionCommerceCustomerShippingR) ([] :: [Filter DC.FieldRevisionCommerceCustomerShippingT]) 

{-# NOINLINE getDCFieldRevisionCommerceDiscountsR #-}
getDCFieldRevisionCommerceDiscountsR :: Handler Html 
getDCFieldRevisionCommerceDiscountsR = entityTableHandler (DC'R DCFieldRevisionCommerceDiscountsR) ([] :: [Filter DC.FieldRevisionCommerceDiscountsT]) 

{-# NOINLINE getDCFieldRevisionCommerceDiscountDateR #-}
getDCFieldRevisionCommerceDiscountDateR :: Handler Html 
getDCFieldRevisionCommerceDiscountDateR = entityTableHandler (DC'R DCFieldRevisionCommerceDiscountDateR) ([] :: [Filter DC.FieldRevisionCommerceDiscountDateT]) 

{-# NOINLINE getDCFieldRevisionCommerceDiscountOfferR #-}
getDCFieldRevisionCommerceDiscountOfferR :: Handler Html 
getDCFieldRevisionCommerceDiscountOfferR = entityTableHandler (DC'R DCFieldRevisionCommerceDiscountOfferR) ([] :: [Filter DC.FieldRevisionCommerceDiscountOfferT]) 

{-# NOINLINE getDCFieldRevisionCommerceDisplayPathR #-}
getDCFieldRevisionCommerceDisplayPathR :: Handler Html 
getDCFieldRevisionCommerceDisplayPathR = entityTableHandler (DC'R DCFieldRevisionCommerceDisplayPathR) ([] :: [Filter DC.FieldRevisionCommerceDisplayPathT]) 

{-# NOINLINE getDCFieldRevisionCommerceFixedAmountR #-}
getDCFieldRevisionCommerceFixedAmountR :: Handler Html 
getDCFieldRevisionCommerceFixedAmountR = entityTableHandler (DC'R DCFieldRevisionCommerceFixedAmountR) ([] :: [Filter DC.FieldRevisionCommerceFixedAmountT]) 

{-# NOINLINE getDCFieldRevisionCommerceFreeProductsR #-}
getDCFieldRevisionCommerceFreeProductsR :: Handler Html 
getDCFieldRevisionCommerceFreeProductsR = entityTableHandler (DC'R DCFieldRevisionCommerceFreeProductsR) ([] :: [Filter DC.FieldRevisionCommerceFreeProductsT]) 

{-# NOINLINE getDCFieldRevisionCommerceFreeShippingR #-}
getDCFieldRevisionCommerceFreeShippingR :: Handler Html 
getDCFieldRevisionCommerceFreeShippingR = entityTableHandler (DC'R DCFieldRevisionCommerceFreeShippingR) ([] :: [Filter DC.FieldRevisionCommerceFreeShippingT]) 

{-# NOINLINE getDCFieldRevisionCommerceLineItemsR #-}
getDCFieldRevisionCommerceLineItemsR :: Handler Html 
getDCFieldRevisionCommerceLineItemsR = entityTableHandler (DC'R DCFieldRevisionCommerceLineItemsR) ([] :: [Filter DC.FieldRevisionCommerceLineItemsT]) 

{-# NOINLINE getDCFieldRevisionCommerceOrderTotalR #-}
getDCFieldRevisionCommerceOrderTotalR :: Handler Html 
getDCFieldRevisionCommerceOrderTotalR = entityTableHandler (DC'R DCFieldRevisionCommerceOrderTotalR) ([] :: [Filter DC.FieldRevisionCommerceOrderTotalT]) 

{-# NOINLINE getDCFieldRevisionCommercePercentageR #-}
getDCFieldRevisionCommercePercentageR :: Handler Html 
getDCFieldRevisionCommercePercentageR = entityTableHandler (DC'R DCFieldRevisionCommercePercentageR) ([] :: [Filter DC.FieldRevisionCommercePercentageT]) 

{-# NOINLINE getDCFieldRevisionCommercePriceR #-}
getDCFieldRevisionCommercePriceR :: Handler Html 
getDCFieldRevisionCommercePriceR = entityTableHandler (DC'R DCFieldRevisionCommercePriceR) ([] :: [Filter DC.FieldRevisionCommercePriceT]) 

{-# NOINLINE getDCFieldRevisionCommerceProductR #-}
getDCFieldRevisionCommerceProductR :: Handler Html 
getDCFieldRevisionCommerceProductR = entityTableHandler (DC'R DCFieldRevisionCommerceProductR) ([] :: [Filter DC.FieldRevisionCommerceProductT]) 

{-# NOINLINE getDCFieldRevisionCommerceShippingServiceR #-}
getDCFieldRevisionCommerceShippingServiceR :: Handler Html 
getDCFieldRevisionCommerceShippingServiceR = entityTableHandler (DC'R DCFieldRevisionCommerceShippingServiceR) ([] :: [Filter DC.FieldRevisionCommerceShippingServiceT]) 

{-# NOINLINE getDCFieldRevisionCommerceTotalR #-}
getDCFieldRevisionCommerceTotalR :: Handler Html 
getDCFieldRevisionCommerceTotalR = entityTableHandler (DC'R DCFieldRevisionCommerceTotalR) ([] :: [Filter DC.FieldRevisionCommerceTotalT]) 

{-# NOINLINE getDCFieldRevisionCommerceUnitPriceR #-}
getDCFieldRevisionCommerceUnitPriceR :: Handler Html 
getDCFieldRevisionCommerceUnitPriceR = entityTableHandler (DC'R DCFieldRevisionCommerceUnitPriceR) ([] :: [Filter DC.FieldRevisionCommerceUnitPriceT]) 

{-# NOINLINE getDCFieldRevisionFieldBrandR #-}
getDCFieldRevisionFieldBrandR :: Handler Html 
getDCFieldRevisionFieldBrandR = entityTableHandler (DC'R DCFieldRevisionFieldBrandR) ([] :: [Filter DC.FieldRevisionFieldBrandT]) 

{-# NOINLINE getDCFieldRevisionFieldCollectionR #-}
getDCFieldRevisionFieldCollectionR :: Handler Html 
getDCFieldRevisionFieldCollectionR = entityTableHandler (DC'R DCFieldRevisionFieldCollectionR) ([] :: [Filter DC.FieldRevisionFieldCollectionT]) 

{-# NOINLINE getDCFieldRevisionFieldColourR #-}
getDCFieldRevisionFieldColourR :: Handler Html 
getDCFieldRevisionFieldColourR = entityTableHandler (DC'R DCFieldRevisionFieldColourR) ([] :: [Filter DC.FieldRevisionFieldColourT]) 

{-# NOINLINE getDCFieldRevisionFieldColourCodeR #-}
getDCFieldRevisionFieldColourCodeR :: Handler Html 
getDCFieldRevisionFieldColourCodeR = entityTableHandler (DC'R DCFieldRevisionFieldColourCodeR) ([] :: [Filter DC.FieldRevisionFieldColourCodeT]) 

{-# NOINLINE getDCFieldRevisionFieldHeadlineR #-}
getDCFieldRevisionFieldHeadlineR :: Handler Html 
getDCFieldRevisionFieldHeadlineR = entityTableHandler (DC'R DCFieldRevisionFieldHeadlineR) ([] :: [Filter DC.FieldRevisionFieldHeadlineT]) 

{-# NOINLINE getDCFieldRevisionFieldImageR #-}
getDCFieldRevisionFieldImageR :: Handler Html 
getDCFieldRevisionFieldImageR = entityTableHandler (DC'R DCFieldRevisionFieldImageR) ([] :: [Filter DC.FieldRevisionFieldImageT]) 

{-# NOINLINE getDCFieldRevisionFieldImagesR #-}
getDCFieldRevisionFieldImagesR :: Handler Html 
getDCFieldRevisionFieldImagesR = entityTableHandler (DC'R DCFieldRevisionFieldImagesR) ([] :: [Filter DC.FieldRevisionFieldImagesT]) 

{-# NOINLINE getDCFieldRevisionFieldLinkR #-}
getDCFieldRevisionFieldLinkR :: Handler Html 
getDCFieldRevisionFieldLinkR = entityTableHandler (DC'R DCFieldRevisionFieldLinkR) ([] :: [Filter DC.FieldRevisionFieldLinkT]) 

{-# NOINLINE getDCFieldRevisionFieldPricePl01R #-}
getDCFieldRevisionFieldPricePl01R :: Handler Html 
getDCFieldRevisionFieldPricePl01R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl01R) ([] :: [Filter DC.FieldRevisionFieldPricePl01T]) 

{-# NOINLINE getDCFieldRevisionFieldPricePl02R #-}
getDCFieldRevisionFieldPricePl02R :: Handler Html 
getDCFieldRevisionFieldPricePl02R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl02R) ([] :: [Filter DC.FieldRevisionFieldPricePl02T]) 

{-# NOINLINE getDCFieldRevisionFieldPricePl03R #-}
getDCFieldRevisionFieldPricePl03R :: Handler Html 
getDCFieldRevisionFieldPricePl03R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl03R) ([] :: [Filter DC.FieldRevisionFieldPricePl03T]) 

{-# NOINLINE getDCFieldRevisionFieldPricePl04R #-}
getDCFieldRevisionFieldPricePl04R :: Handler Html 
getDCFieldRevisionFieldPricePl04R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl04R) ([] :: [Filter DC.FieldRevisionFieldPricePl04T]) 

{-# NOINLINE getDCFieldRevisionFieldPricePl05R #-}
getDCFieldRevisionFieldPricePl05R :: Handler Html 
getDCFieldRevisionFieldPricePl05R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl05R) ([] :: [Filter DC.FieldRevisionFieldPricePl05T]) 

{-# NOINLINE getDCFieldRevisionFieldPricePl06R #-}
getDCFieldRevisionFieldPricePl06R :: Handler Html 
getDCFieldRevisionFieldPricePl06R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl06R) ([] :: [Filter DC.FieldRevisionFieldPricePl06T]) 

{-# NOINLINE getDCFieldRevisionFieldPricePl07R #-}
getDCFieldRevisionFieldPricePl07R :: Handler Html 
getDCFieldRevisionFieldPricePl07R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl07R) ([] :: [Filter DC.FieldRevisionFieldPricePl07T]) 

{-# NOINLINE getDCFieldRevisionFieldPricePl08R #-}
getDCFieldRevisionFieldPricePl08R :: Handler Html 
getDCFieldRevisionFieldPricePl08R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl08R) ([] :: [Filter DC.FieldRevisionFieldPricePl08T]) 

{-# NOINLINE getDCFieldRevisionFieldPricePl09R #-}
getDCFieldRevisionFieldPricePl09R :: Handler Html 
getDCFieldRevisionFieldPricePl09R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl09R) ([] :: [Filter DC.FieldRevisionFieldPricePl09T]) 

{-# NOINLINE getDCFieldRevisionFieldPricePl10R #-}
getDCFieldRevisionFieldPricePl10R :: Handler Html 
getDCFieldRevisionFieldPricePl10R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl10R) ([] :: [Filter DC.FieldRevisionFieldPricePl10T]) 

{-# NOINLINE getDCFieldRevisionFieldPricePl11R #-}
getDCFieldRevisionFieldPricePl11R :: Handler Html 
getDCFieldRevisionFieldPricePl11R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl11R) ([] :: [Filter DC.FieldRevisionFieldPricePl11T]) 

{-# NOINLINE getDCFieldRevisionFieldPricePl12R #-}
getDCFieldRevisionFieldPricePl12R :: Handler Html 
getDCFieldRevisionFieldPricePl12R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl12R) ([] :: [Filter DC.FieldRevisionFieldPricePl12T]) 

{-# NOINLINE getDCFieldRevisionFieldPricePl13R #-}
getDCFieldRevisionFieldPricePl13R :: Handler Html 
getDCFieldRevisionFieldPricePl13R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl13R) ([] :: [Filter DC.FieldRevisionFieldPricePl13T]) 

{-# NOINLINE getDCFieldRevisionFieldPricePl14R #-}
getDCFieldRevisionFieldPricePl14R :: Handler Html 
getDCFieldRevisionFieldPricePl14R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl14R) ([] :: [Filter DC.FieldRevisionFieldPricePl14T]) 

{-# NOINLINE getDCFieldRevisionFieldProductR #-}
getDCFieldRevisionFieldProductR :: Handler Html 
getDCFieldRevisionFieldProductR = entityTableHandler (DC'R DCFieldRevisionFieldProductR) ([] :: [Filter DC.FieldRevisionFieldProductT]) 

{-# NOINLINE getDCFieldRevisionFieldProductCategoryR #-}
getDCFieldRevisionFieldProductCategoryR :: Handler Html 
getDCFieldRevisionFieldProductCategoryR = entityTableHandler (DC'R DCFieldRevisionFieldProductCategoryR) ([] :: [Filter DC.FieldRevisionFieldProductCategoryT]) 

{-# NOINLINE getDCFieldRevisionFieldRequiredDeliveryDateR #-}
getDCFieldRevisionFieldRequiredDeliveryDateR :: Handler Html 
getDCFieldRevisionFieldRequiredDeliveryDateR = entityTableHandler (DC'R DCFieldRevisionFieldRequiredDeliveryDateR) ([] :: [Filter DC.FieldRevisionFieldRequiredDeliveryDateT]) 

{-# NOINLINE getDCFieldRevisionFieldRgbR #-}
getDCFieldRevisionFieldRgbR :: Handler Html 
getDCFieldRevisionFieldRgbR = entityTableHandler (DC'R DCFieldRevisionFieldRgbR) ([] :: [Filter DC.FieldRevisionFieldRgbT]) 

{-# NOINLINE getDCFieldRevisionFieldSpecialRequestR #-}
getDCFieldRevisionFieldSpecialRequestR :: Handler Html 
getDCFieldRevisionFieldSpecialRequestR = entityTableHandler (DC'R DCFieldRevisionFieldSpecialRequestR) ([] :: [Filter DC.FieldRevisionFieldSpecialRequestT]) 

{-# NOINLINE getDCFieldRevisionFieldStockStatusR #-}
getDCFieldRevisionFieldStockStatusR :: Handler Html 
getDCFieldRevisionFieldStockStatusR = entityTableHandler (DC'R DCFieldRevisionFieldStockStatusR) ([] :: [Filter DC.FieldRevisionFieldStockStatusT]) 

{-# NOINLINE getDCFieldRevisionFieldTaglineR #-}
getDCFieldRevisionFieldTaglineR :: Handler Html 
getDCFieldRevisionFieldTaglineR = entityTableHandler (DC'R DCFieldRevisionFieldTaglineR) ([] :: [Filter DC.FieldRevisionFieldTaglineT]) 

{-# NOINLINE getDCFieldRevisionFieldTrimColourR #-}
getDCFieldRevisionFieldTrimColourR :: Handler Html 
getDCFieldRevisionFieldTrimColourR = entityTableHandler (DC'R DCFieldRevisionFieldTrimColourR) ([] :: [Filter DC.FieldRevisionFieldTrimColourT]) 

{-# NOINLINE getDCFieldRevisionFieldWholesalePriceR #-}
getDCFieldRevisionFieldWholesalePriceR :: Handler Html 
getDCFieldRevisionFieldWholesalePriceR = entityTableHandler (DC'R DCFieldRevisionFieldWholesalePriceR) ([] :: [Filter DC.FieldRevisionFieldWholesalePriceT]) 

{-# NOINLINE getDCFieldRevisionInlineConditionsR #-}
getDCFieldRevisionInlineConditionsR :: Handler Html 
getDCFieldRevisionInlineConditionsR = entityTableHandler (DC'R DCFieldRevisionInlineConditionsR) ([] :: [Filter DC.FieldRevisionInlineConditionsT]) 

{-# NOINLINE getDCFieldRevisionMessageCommerceBodyR #-}
getDCFieldRevisionMessageCommerceBodyR :: Handler Html 
getDCFieldRevisionMessageCommerceBodyR = entityTableHandler (DC'R DCFieldRevisionMessageCommerceBodyR) ([] :: [Filter DC.FieldRevisionMessageCommerceBodyT]) 

{-# NOINLINE getDCFieldRevisionMessageCommerceLineItemR #-}
getDCFieldRevisionMessageCommerceLineItemR :: Handler Html 
getDCFieldRevisionMessageCommerceLineItemR = entityTableHandler (DC'R DCFieldRevisionMessageCommerceLineItemR) ([] :: [Filter DC.FieldRevisionMessageCommerceLineItemT]) 

{-# NOINLINE getDCFieldRevisionMessageCommerceOrderR #-}
getDCFieldRevisionMessageCommerceOrderR :: Handler Html 
getDCFieldRevisionMessageCommerceOrderR = entityTableHandler (DC'R DCFieldRevisionMessageCommerceOrderR) ([] :: [Filter DC.FieldRevisionMessageCommerceOrderT]) 

{-# NOINLINE getDCFieldRevisionMessageCommercePaymentR #-}
getDCFieldRevisionMessageCommercePaymentR :: Handler Html 
getDCFieldRevisionMessageCommercePaymentR = entityTableHandler (DC'R DCFieldRevisionMessageCommercePaymentR) ([] :: [Filter DC.FieldRevisionMessageCommercePaymentT]) 

{-# NOINLINE getDCFieldRevisionMessageOrderDisplayNameR #-}
getDCFieldRevisionMessageOrderDisplayNameR :: Handler Html 
getDCFieldRevisionMessageOrderDisplayNameR = entityTableHandler (DC'R DCFieldRevisionMessageOrderDisplayNameR) ([] :: [Filter DC.FieldRevisionMessageOrderDisplayNameT]) 

{-# NOINLINE getDCFieldRevisionMessageTextR #-}
getDCFieldRevisionMessageTextR :: Handler Html 
getDCFieldRevisionMessageTextR = entityTableHandler (DC'R DCFieldRevisionMessageTextR) ([] :: [Filter DC.FieldRevisionMessageTextT]) 

{-# NOINLINE getDCFieldRevisionMessageTextSubjectR #-}
getDCFieldRevisionMessageTextSubjectR :: Handler Html 
getDCFieldRevisionMessageTextSubjectR = entityTableHandler (DC'R DCFieldRevisionMessageTextSubjectR) ([] :: [Filter DC.FieldRevisionMessageTextSubjectT]) 

{-# NOINLINE getDCFieldRevisionTitleFieldR #-}
getDCFieldRevisionTitleFieldR :: Handler Html 
getDCFieldRevisionTitleFieldR = entityTableHandler (DC'R DCFieldRevisionTitleFieldR) ([] :: [Filter DC.FieldRevisionTitleFieldT]) 

{-# NOINLINE getDCFileManagedR #-}
getDCFileManagedR :: Handler Html 
getDCFileManagedR = entityTableHandler (DC'R DCFileManagedR) ([] :: [Filter DC.FileManagedT]) 

{-# NOINLINE getDCFileUsageR #-}
getDCFileUsageR :: Handler Html 
getDCFileUsageR = entityTableHandler (DC'R DCFileUsageR) ([] :: [Filter DC.FileUsageT]) 

{-# NOINLINE getDCFilterR #-}
getDCFilterR :: Handler Html 
getDCFilterR = entityTableHandler (DC'R DCFilterR) ([] :: [Filter DC.FilterT]) 

{-# NOINLINE getDCFilterFormatR #-}
getDCFilterFormatR :: Handler Html 
getDCFilterFormatR = entityTableHandler (DC'R DCFilterFormatR) ([] :: [Filter DC.FilterFormatT]) 

{-# NOINLINE getDCFloodR #-}
getDCFloodR :: Handler Html 
getDCFloodR = entityTableHandler (DC'R DCFloodR) ([] :: [Filter DC.FloodT]) 

{-# NOINLINE getDCHistoryR #-}
getDCHistoryR :: Handler Html 
getDCHistoryR = entityTableHandler (DC'R DCHistoryR) ([] :: [Filter DC.HistoryT]) 

{-# NOINLINE getDCHoneypotUserR #-}
getDCHoneypotUserR :: Handler Html 
getDCHoneypotUserR = entityTableHandler (DC'R DCHoneypotUserR) ([] :: [Filter DC.HoneypotUserT]) 

{-# NOINLINE getDCImageEffectsR #-}
getDCImageEffectsR :: Handler Html 
getDCImageEffectsR = entityTableHandler (DC'R DCImageEffectsR) ([] :: [Filter DC.ImageEffectsT]) 

{-# NOINLINE getDCImageStylesR #-}
getDCImageStylesR :: Handler Html 
getDCImageStylesR = entityTableHandler (DC'R DCImageStylesR) ([] :: [Filter DC.ImageStylesT]) 

{-# NOINLINE getDCJobScheduleR #-}
getDCJobScheduleR :: Handler Html 
getDCJobScheduleR = entityTableHandler (DC'R DCJobScheduleR) ([] :: [Filter DC.JobScheduleT]) 

{-# NOINLINE getDCMasqueradeR #-}
getDCMasqueradeR :: Handler Html 
getDCMasqueradeR = entityTableHandler (DC'R DCMasqueradeR) ([] :: [Filter DC.MasqueradeT]) 

{-# NOINLINE getDCMasqueradeUsersR #-}
getDCMasqueradeUsersR :: Handler Html 
getDCMasqueradeUsersR = entityTableHandler (DC'R DCMasqueradeUsersR) ([] :: [Filter DC.MasqueradeUsersT]) 

{-# NOINLINE getDCMegamenuR #-}
getDCMegamenuR :: Handler Html 
getDCMegamenuR = entityTableHandler (DC'R DCMegamenuR) ([] :: [Filter DC.MegamenuT]) 

{-# NOINLINE getDCMenuCustomR #-}
getDCMenuCustomR :: Handler Html 
getDCMenuCustomR = entityTableHandler (DC'R DCMenuCustomR) ([] :: [Filter DC.MenuCustomT]) 

{-# NOINLINE getDCMenuLinksR #-}
getDCMenuLinksR :: Handler Html 
getDCMenuLinksR = entityTableHandler (DC'R DCMenuLinksR) ([] :: [Filter DC.MenuLinksT]) 

{-# NOINLINE getDCMenuRouterR #-}
getDCMenuRouterR :: Handler Html 
getDCMenuRouterR = entityTableHandler (DC'R DCMenuRouterR) ([] :: [Filter DC.MenuRouterT]) 

{-# NOINLINE getDCMessageR #-}
getDCMessageR :: Handler Html 
getDCMessageR = entityTableHandler (DC'R DCMessageR) ([] :: [Filter DC.MessageT]) 

{-# NOINLINE getDCMessageTypeR #-}
getDCMessageTypeR :: Handler Html 
getDCMessageTypeR = entityTableHandler (DC'R DCMessageTypeR) ([] :: [Filter DC.MessageTypeT]) 

{-# NOINLINE getDCMessageTypeCategoryR #-}
getDCMessageTypeCategoryR :: Handler Html 
getDCMessageTypeCategoryR = entityTableHandler (DC'R DCMessageTypeCategoryR) ([] :: [Filter DC.MessageTypeCategoryT]) 

{-# NOINLINE getDCMetatagR #-}
getDCMetatagR :: Handler Html 
getDCMetatagR = entityTableHandler (DC'R DCMetatagR) ([] :: [Filter DC.MetatagT]) 

{-# NOINLINE getDCMetatagConfigR #-}
getDCMetatagConfigR :: Handler Html 
getDCMetatagConfigR = entityTableHandler (DC'R DCMetatagConfigR) ([] :: [Filter DC.MetatagConfigT]) 

{-# NOINLINE getDCMigrateLogR #-}
getDCMigrateLogR :: Handler Html 
getDCMigrateLogR = entityTableHandler (DC'R DCMigrateLogR) ([] :: [Filter DC.MigrateLogT]) 

{-# NOINLINE getDCMigrateMapCommercekickstartadpushR #-}
getDCMigrateMapCommercekickstartadpushR :: Handler Html 
getDCMigrateMapCommercekickstartadpushR = entityTableHandler (DC'R DCMigrateMapCommercekickstartadpushR) ([] :: [Filter DC.MigrateMapCommercekickstartadpushT]) 

{-# NOINLINE getDCMigrateMapCommercekickstartnodeR #-}
getDCMigrateMapCommercekickstartnodeR :: Handler Html 
getDCMigrateMapCommercekickstartnodeR = entityTableHandler (DC'R DCMigrateMapCommercekickstartnodeR) ([] :: [Filter DC.MigrateMapCommercekickstartnodeT]) 

{-# NOINLINE getDCMigrateMapCommercekickstartpagesR #-}
getDCMigrateMapCommercekickstartpagesR :: Handler Html 
getDCMigrateMapCommercekickstartpagesR = entityTableHandler (DC'R DCMigrateMapCommercekickstartpagesR) ([] :: [Filter DC.MigrateMapCommercekickstartpagesT]) 

{-# NOINLINE getDCMigrateMapCommercekickstartproductR #-}
getDCMigrateMapCommercekickstartproductR :: Handler Html 
getDCMigrateMapCommercekickstartproductR = entityTableHandler (DC'R DCMigrateMapCommercekickstartproductR) ([] :: [Filter DC.MigrateMapCommercekickstartproductT]) 

{-# NOINLINE getDCMigrateMapCommercekickstartslideshowR #-}
getDCMigrateMapCommercekickstartslideshowR :: Handler Html 
getDCMigrateMapCommercekickstartslideshowR = entityTableHandler (DC'R DCMigrateMapCommercekickstartslideshowR) ([] :: [Filter DC.MigrateMapCommercekickstartslideshowT]) 

{-# NOINLINE getDCMigrateMessageCommercekickstartadpushR #-}
getDCMigrateMessageCommercekickstartadpushR :: Handler Html 
getDCMigrateMessageCommercekickstartadpushR = entityTableHandler (DC'R DCMigrateMessageCommercekickstartadpushR) ([] :: [Filter DC.MigrateMessageCommercekickstartadpushT]) 

{-# NOINLINE getDCMigrateMessageCommercekickstartnodeR #-}
getDCMigrateMessageCommercekickstartnodeR :: Handler Html 
getDCMigrateMessageCommercekickstartnodeR = entityTableHandler (DC'R DCMigrateMessageCommercekickstartnodeR) ([] :: [Filter DC.MigrateMessageCommercekickstartnodeT]) 

{-# NOINLINE getDCMigrateMessageCommercekickstartpagesR #-}
getDCMigrateMessageCommercekickstartpagesR :: Handler Html 
getDCMigrateMessageCommercekickstartpagesR = entityTableHandler (DC'R DCMigrateMessageCommercekickstartpagesR) ([] :: [Filter DC.MigrateMessageCommercekickstartpagesT]) 

{-# NOINLINE getDCMigrateMessageCommercekickstartproductR #-}
getDCMigrateMessageCommercekickstartproductR :: Handler Html 
getDCMigrateMessageCommercekickstartproductR = entityTableHandler (DC'R DCMigrateMessageCommercekickstartproductR) ([] :: [Filter DC.MigrateMessageCommercekickstartproductT]) 

{-# NOINLINE getDCMigrateMessageCommercekickstartslideshowR #-}
getDCMigrateMessageCommercekickstartslideshowR :: Handler Html 
getDCMigrateMessageCommercekickstartslideshowR = entityTableHandler (DC'R DCMigrateMessageCommercekickstartslideshowR) ([] :: [Filter DC.MigrateMessageCommercekickstartslideshowT]) 

{-# NOINLINE getDCMigrateStatusR #-}
getDCMigrateStatusR :: Handler Html 
getDCMigrateStatusR = entityTableHandler (DC'R DCMigrateStatusR) ([] :: [Filter DC.MigrateStatusT]) 

{-# NOINLINE getDCNodeR #-}
getDCNodeR :: Handler Html 
getDCNodeR = entityTableHandler (DC'R DCNodeR) ([] :: [Filter DC.NodeT]) 

{-# NOINLINE getDCNodeAccessR #-}
getDCNodeAccessR :: Handler Html 
getDCNodeAccessR = entityTableHandler (DC'R DCNodeAccessR) ([] :: [Filter DC.NodeAccessT]) 

{-# NOINLINE getDCNodeCommentStatisticsR #-}
getDCNodeCommentStatisticsR :: Handler Html 
getDCNodeCommentStatisticsR = entityTableHandler (DC'R DCNodeCommentStatisticsR) ([] :: [Filter DC.NodeCommentStatisticsT]) 

{-# NOINLINE getDCNodeCounterR #-}
getDCNodeCounterR :: Handler Html 
getDCNodeCounterR = entityTableHandler (DC'R DCNodeCounterR) ([] :: [Filter DC.NodeCounterT]) 

{-# NOINLINE getDCNodeRevisionR #-}
getDCNodeRevisionR :: Handler Html 
getDCNodeRevisionR = entityTableHandler (DC'R DCNodeRevisionR) ([] :: [Filter DC.NodeRevisionT]) 

{-# NOINLINE getDCNodeSpambotR #-}
getDCNodeSpambotR :: Handler Html 
getDCNodeSpambotR = entityTableHandler (DC'R DCNodeSpambotR) ([] :: [Filter DC.NodeSpambotT]) 

{-# NOINLINE getDCNodeTypeR #-}
getDCNodeTypeR :: Handler Html 
getDCNodeTypeR = entityTableHandler (DC'R DCNodeTypeR) ([] :: [Filter DC.NodeTypeT]) 

{-# NOINLINE getDCPageTitleR #-}
getDCPageTitleR :: Handler Html 
getDCPageTitleR = entityTableHandler (DC'R DCPageTitleR) ([] :: [Filter DC.PageTitleT]) 

{-# NOINLINE getDCQueueR #-}
getDCQueueR :: Handler Html 
getDCQueueR = entityTableHandler (DC'R DCQueueR) ([] :: [Filter DC.QueueT]) 

{-# NOINLINE getDCRedirectR #-}
getDCRedirectR :: Handler Html 
getDCRedirectR = entityTableHandler (DC'R DCRedirectR) ([] :: [Filter DC.RedirectT]) 

{-# NOINLINE getDCRegistryR #-}
getDCRegistryR :: Handler Html 
getDCRegistryR = entityTableHandler (DC'R DCRegistryR) ([] :: [Filter DC.RegistryT]) 

{-# NOINLINE getDCRegistryFileR #-}
getDCRegistryFileR :: Handler Html 
getDCRegistryFileR = entityTableHandler (DC'R DCRegistryFileR) ([] :: [Filter DC.RegistryFileT]) 

{-# NOINLINE getDCRoleR #-}
getDCRoleR :: Handler Html 
getDCRoleR = entityTableHandler (DC'R DCRoleR) ([] :: [Filter DC.RoleT]) 

{-# NOINLINE getDCRolePermissionR #-}
getDCRolePermissionR :: Handler Html 
getDCRolePermissionR = entityTableHandler (DC'R DCRolePermissionR) ([] :: [Filter DC.RolePermissionT]) 

{-# NOINLINE getDCRulesConfigR #-}
getDCRulesConfigR :: Handler Html 
getDCRulesConfigR = entityTableHandler (DC'R DCRulesConfigR) ([] :: [Filter DC.RulesConfigT]) 

{-# NOINLINE getDCRulesDependenciesR #-}
getDCRulesDependenciesR :: Handler Html 
getDCRulesDependenciesR = entityTableHandler (DC'R DCRulesDependenciesR) ([] :: [Filter DC.RulesDependenciesT]) 

{-# NOINLINE getDCRulesTagsR #-}
getDCRulesTagsR :: Handler Html 
getDCRulesTagsR = entityTableHandler (DC'R DCRulesTagsR) ([] :: [Filter DC.RulesTagsT]) 

{-# NOINLINE getDCRulesTriggerR #-}
getDCRulesTriggerR :: Handler Html 
getDCRulesTriggerR = entityTableHandler (DC'R DCRulesTriggerR) ([] :: [Filter DC.RulesTriggerT]) 

{-# NOINLINE getDCSearchApiDbCustomProductDisplayFieldProductCommercePriR #-}
getDCSearchApiDbCustomProductDisplayFieldProductCommercePriR :: Handler Html 
getDCSearchApiDbCustomProductDisplayFieldProductCommercePriR = entityTableHandler (DC'R DCSearchApiDbCustomProductDisplayFieldProductCommercePriR) ([] :: [Filter DC.SearchApiDbCustomProductDisplayFieldProductCommercePriT]) 

{-# NOINLINE getDCSearchApiDbCustomProductDisplaySearchApiLanguageR #-}
getDCSearchApiDbCustomProductDisplaySearchApiLanguageR :: Handler Html 
getDCSearchApiDbCustomProductDisplaySearchApiLanguageR = entityTableHandler (DC'R DCSearchApiDbCustomProductDisplaySearchApiLanguageR) ([] :: [Filter DC.SearchApiDbCustomProductDisplaySearchApiLanguageT]) 

{-# NOINLINE getDCSearchApiDbProductDisplayR #-}
getDCSearchApiDbProductDisplayR :: Handler Html 
getDCSearchApiDbProductDisplayR = entityTableHandler (DC'R DCSearchApiDbProductDisplayR) ([] :: [Filter DC.SearchApiDbProductDisplayT]) 

{-# NOINLINE getDCSearchApiDbProductDisplayCreatedR #-}
getDCSearchApiDbProductDisplayCreatedR :: Handler Html 
getDCSearchApiDbProductDisplayCreatedR = entityTableHandler (DC'R DCSearchApiDbProductDisplayCreatedR) ([] :: [Filter DC.SearchApiDbProductDisplayCreatedT]) 

{-# NOINLINE getDCSearchApiDbProductDisplayFieldBrandR #-}
getDCSearchApiDbProductDisplayFieldBrandR :: Handler Html 
getDCSearchApiDbProductDisplayFieldBrandR = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldBrandR) ([] :: [Filter DC.SearchApiDbProductDisplayFieldBrandT]) 

{-# NOINLINE getDCSearchApiDbProductDisplayFieldCollectionR #-}
getDCSearchApiDbProductDisplayFieldCollectionR :: Handler Html 
getDCSearchApiDbProductDisplayFieldCollectionR = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldCollectionR) ([] :: [Filter DC.SearchApiDbProductDisplayFieldCollectionT]) 

{-# NOINLINE getDCSearchApiDbProductDisplayFieldProductCategoryR #-}
getDCSearchApiDbProductDisplayFieldProductCategoryR :: Handler Html 
getDCSearchApiDbProductDisplayFieldProductCategoryR = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldProductCategoryR) ([] :: [Filter DC.SearchApiDbProductDisplayFieldProductCategoryT]) 

{-# NOINLINE getDCSearchApiDbProductDisplayFieldProductFieldColourR #-}
getDCSearchApiDbProductDisplayFieldProductFieldColourR :: Handler Html 
getDCSearchApiDbProductDisplayFieldProductFieldColourR = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldProductFieldColourR) ([] :: [Filter DC.SearchApiDbProductDisplayFieldProductFieldColourT]) 

{-# NOINLINE getDCSearchApiDbProductDisplayFieldProductFieldPricePl06R #-}
getDCSearchApiDbProductDisplayFieldProductFieldPricePl06R :: Handler Html 
getDCSearchApiDbProductDisplayFieldProductFieldPricePl06R = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldProductFieldPricePl06R) ([] :: [Filter DC.SearchApiDbProductDisplayFieldProductFieldPricePl06T]) 

{-# NOINLINE getDCSearchApiDbProductDisplayFieldProductFieldPricePl06AR #-}
getDCSearchApiDbProductDisplayFieldProductFieldPricePl06AR :: Handler Html 
getDCSearchApiDbProductDisplayFieldProductFieldPricePl06AR = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldProductFieldPricePl06AR) ([] :: [Filter DC.SearchApiDbProductDisplayFieldProductFieldPricePl06AT]) 

{-# NOINLINE getDCSearchApiDbProductDisplayFieldProductFieldPricePl12AR #-}
getDCSearchApiDbProductDisplayFieldProductFieldPricePl12AR :: Handler Html 
getDCSearchApiDbProductDisplayFieldProductFieldPricePl12AR = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldProductFieldPricePl12AR) ([] :: [Filter DC.SearchApiDbProductDisplayFieldProductFieldPricePl12AT]) 

{-# NOINLINE getDCSearchApiDbProductDisplayFieldProductFieldStockStatusR #-}
getDCSearchApiDbProductDisplayFieldProductFieldStockStatusR :: Handler Html 
getDCSearchApiDbProductDisplayFieldProductFieldStockStatusR = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldProductFieldStockStatusR) ([] :: [Filter DC.SearchApiDbProductDisplayFieldProductFieldStockStatusT]) 

{-# NOINLINE getDCSearchApiDbProductDisplayFieldProductFieldTrimColourR #-}
getDCSearchApiDbProductDisplayFieldProductFieldTrimColourR :: Handler Html 
getDCSearchApiDbProductDisplayFieldProductFieldTrimColourR = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldProductFieldTrimColourR) ([] :: [Filter DC.SearchApiDbProductDisplayFieldProductFieldTrimColourT]) 

{-# NOINLINE getDCSearchApiDbProductDisplayFieldProductTitleR #-}
getDCSearchApiDbProductDisplayFieldProductTitleR :: Handler Html 
getDCSearchApiDbProductDisplayFieldProductTitleR = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldProductTitleR) ([] :: [Filter DC.SearchApiDbProductDisplayFieldProductTitleT]) 

{-# NOINLINE getDCSearchApiDbProductDisplayFieldProductTitle1R #-}
getDCSearchApiDbProductDisplayFieldProductTitle1R :: Handler Html 
getDCSearchApiDbProductDisplayFieldProductTitle1R = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldProductTitle1R) ([] :: [Filter DC.SearchApiDbProductDisplayFieldProductTitle1T]) 

{-# NOINLINE getDCSearchApiDbProductDisplayFieldProductTitle2R #-}
getDCSearchApiDbProductDisplayFieldProductTitle2R :: Handler Html 
getDCSearchApiDbProductDisplayFieldProductTitle2R = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldProductTitle2R) ([] :: [Filter DC.SearchApiDbProductDisplayFieldProductTitle2T]) 

{-# NOINLINE getDCSearchApiDbProductDisplayFieldProductTitle3R #-}
getDCSearchApiDbProductDisplayFieldProductTitle3R :: Handler Html 
getDCSearchApiDbProductDisplayFieldProductTitle3R = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldProductTitle3R) ([] :: [Filter DC.SearchApiDbProductDisplayFieldProductTitle3T]) 

{-# NOINLINE getDCSearchApiDbProductDisplaySearchApiAggregation1R #-}
getDCSearchApiDbProductDisplaySearchApiAggregation1R :: Handler Html 
getDCSearchApiDbProductDisplaySearchApiAggregation1R = entityTableHandler (DC'R DCSearchApiDbProductDisplaySearchApiAggregation1R) ([] :: [Filter DC.SearchApiDbProductDisplaySearchApiAggregation1T]) 

{-# NOINLINE getDCSearchApiDbProductDisplaySearchApiLanguageR #-}
getDCSearchApiDbProductDisplaySearchApiLanguageR :: Handler Html 
getDCSearchApiDbProductDisplaySearchApiLanguageR = entityTableHandler (DC'R DCSearchApiDbProductDisplaySearchApiLanguageR) ([] :: [Filter DC.SearchApiDbProductDisplaySearchApiLanguageT]) 

{-# NOINLINE getDCSearchApiDbProductDisplayStatusR #-}
getDCSearchApiDbProductDisplayStatusR :: Handler Html 
getDCSearchApiDbProductDisplayStatusR = entityTableHandler (DC'R DCSearchApiDbProductDisplayStatusR) ([] :: [Filter DC.SearchApiDbProductDisplayStatusT]) 

{-# NOINLINE getDCSearchApiDbProductDisplayTextR #-}
getDCSearchApiDbProductDisplayTextR :: Handler Html 
getDCSearchApiDbProductDisplayTextR = entityTableHandler (DC'R DCSearchApiDbProductDisplayTextR) ([] :: [Filter DC.SearchApiDbProductDisplayTextT]) 

{-# NOINLINE getDCSearchApiDbProductDisplayTitleR #-}
getDCSearchApiDbProductDisplayTitleR :: Handler Html 
getDCSearchApiDbProductDisplayTitleR = entityTableHandler (DC'R DCSearchApiDbProductDisplayTitleR) ([] :: [Filter DC.SearchApiDbProductDisplayTitleT]) 

{-# NOINLINE getDCSearchApiDbProductSearchFieldProductFieldColourR #-}
getDCSearchApiDbProductSearchFieldProductFieldColourR :: Handler Html 
getDCSearchApiDbProductSearchFieldProductFieldColourR = entityTableHandler (DC'R DCSearchApiDbProductSearchFieldProductFieldColourR) ([] :: [Filter DC.SearchApiDbProductSearchFieldProductFieldColourT]) 

{-# NOINLINE getDCSearchApiDbProductSearchSearchApiLanguageR #-}
getDCSearchApiDbProductSearchSearchApiLanguageR :: Handler Html 
getDCSearchApiDbProductSearchSearchApiLanguageR = entityTableHandler (DC'R DCSearchApiDbProductSearchSearchApiLanguageR) ([] :: [Filter DC.SearchApiDbProductSearchSearchApiLanguageT]) 

{-# NOINLINE getDCSearchApiDbProductSearchTitleFieldR #-}
getDCSearchApiDbProductSearchTitleFieldR :: Handler Html 
getDCSearchApiDbProductSearchTitleFieldR = entityTableHandler (DC'R DCSearchApiDbProductSearchTitleFieldR) ([] :: [Filter DC.SearchApiDbProductSearchTitleFieldT]) 

{-# NOINLINE getDCSearchApiIndexR #-}
getDCSearchApiIndexR :: Handler Html 
getDCSearchApiIndexR = entityTableHandler (DC'R DCSearchApiIndexR) ([] :: [Filter DC.SearchApiIndexT]) 

{-# NOINLINE getDCSearchApiItemR #-}
getDCSearchApiItemR :: Handler Html 
getDCSearchApiItemR = entityTableHandler (DC'R DCSearchApiItemR) ([] :: [Filter DC.SearchApiItemT]) 

{-# NOINLINE getDCSearchApiServerR #-}
getDCSearchApiServerR :: Handler Html 
getDCSearchApiServerR = entityTableHandler (DC'R DCSearchApiServerR) ([] :: [Filter DC.SearchApiServerT]) 

{-# NOINLINE getDCSearchApiSortR #-}
getDCSearchApiSortR :: Handler Html 
getDCSearchApiSortR = entityTableHandler (DC'R DCSearchApiSortR) ([] :: [Filter DC.SearchApiSortT]) 

{-# NOINLINE getDCSearchApiTaskR #-}
getDCSearchApiTaskR :: Handler Html 
getDCSearchApiTaskR = entityTableHandler (DC'R DCSearchApiTaskR) ([] :: [Filter DC.SearchApiTaskT]) 

{-# NOINLINE getDCSecurityReviewR #-}
getDCSecurityReviewR :: Handler Html 
getDCSecurityReviewR = entityTableHandler (DC'R DCSecurityReviewR) ([] :: [Filter DC.SecurityReviewT]) 

{-# NOINLINE getDCSemaphoreR #-}
getDCSemaphoreR :: Handler Html 
getDCSemaphoreR = entityTableHandler (DC'R DCSemaphoreR) ([] :: [Filter DC.SemaphoreT]) 

{-# NOINLINE getDCSequencesR #-}
getDCSequencesR :: Handler Html 
getDCSequencesR = entityTableHandler (DC'R DCSequencesR) ([] :: [Filter DC.SequencesT]) 

{-# NOINLINE getDCSessionsR #-}
getDCSessionsR :: Handler Html 
getDCSessionsR = entityTableHandler (DC'R DCSessionsR) ([] :: [Filter DC.SessionsT]) 

{-# NOINLINE getDCSiteVerifyR #-}
getDCSiteVerifyR :: Handler Html 
getDCSiteVerifyR = entityTableHandler (DC'R DCSiteVerifyR) ([] :: [Filter DC.SiteVerifyT]) 

{-# NOINLINE getDCSystemR #-}
getDCSystemR :: Handler Html 
getDCSystemR = entityTableHandler (DC'R DCSystemR) ([] :: [Filter DC.SystemT]) 

{-# NOINLINE getDCTaxonomyIndexR #-}
getDCTaxonomyIndexR :: Handler Html 
getDCTaxonomyIndexR = entityTableHandler (DC'R DCTaxonomyIndexR) ([] :: [Filter DC.TaxonomyIndexT]) 

{-# NOINLINE getDCTaxonomyMenuR #-}
getDCTaxonomyMenuR :: Handler Html 
getDCTaxonomyMenuR = entityTableHandler (DC'R DCTaxonomyMenuR) ([] :: [Filter DC.TaxonomyMenuT]) 

{-# NOINLINE getDCTaxonomyTermDataR #-}
getDCTaxonomyTermDataR :: Handler Html 
getDCTaxonomyTermDataR = entityTableHandler (DC'R DCTaxonomyTermDataR) ([] :: [Filter DC.TaxonomyTermDataT]) 

{-# NOINLINE getDCTaxonomyTermHierarchyR #-}
getDCTaxonomyTermHierarchyR :: Handler Html 
getDCTaxonomyTermHierarchyR = entityTableHandler (DC'R DCTaxonomyTermHierarchyR) ([] :: [Filter DC.TaxonomyTermHierarchyT]) 

{-# NOINLINE getDCTaxonomyToolsRoleAccessR #-}
getDCTaxonomyToolsRoleAccessR :: Handler Html 
getDCTaxonomyToolsRoleAccessR = entityTableHandler (DC'R DCTaxonomyToolsRoleAccessR) ([] :: [Filter DC.TaxonomyToolsRoleAccessT]) 

{-# NOINLINE getDCTaxonomyVocabularyR #-}
getDCTaxonomyVocabularyR :: Handler Html 
getDCTaxonomyVocabularyR = entityTableHandler (DC'R DCTaxonomyVocabularyR) ([] :: [Filter DC.TaxonomyVocabularyT]) 

{-# NOINLINE getDCUrlAliasR #-}
getDCUrlAliasR :: Handler Html 
getDCUrlAliasR = entityTableHandler (DC'R DCUrlAliasR) ([] :: [Filter DC.UrlAliasT]) 

{-# NOINLINE getDCUsersR #-}
getDCUsersR :: Handler Html 
getDCUsersR = entityTableHandler (DC'R DCUsersR) ([] :: [Filter DC.UsersT]) 

{-# NOINLINE getDCUsersRolesR #-}
getDCUsersRolesR :: Handler Html 
getDCUsersRolesR = entityTableHandler (DC'R DCUsersRolesR) ([] :: [Filter DC.UsersRolesT]) 

{-# NOINLINE getDCVariableR #-}
getDCVariableR :: Handler Html 
getDCVariableR = entityTableHandler (DC'R DCVariableR) ([] :: [Filter DC.VariableT]) 

{-# NOINLINE getDCViewsDisplayR #-}
getDCViewsDisplayR :: Handler Html 
getDCViewsDisplayR = entityTableHandler (DC'R DCViewsDisplayR) ([] :: [Filter DC.ViewsDisplayT]) 

{-# NOINLINE getDCViewsViewR #-}
getDCViewsViewR :: Handler Html 
getDCViewsViewR = entityTableHandler (DC'R DCViewsViewR) ([] :: [Filter DC.ViewsViewT]) 

{-# NOINLINE getDCWatchdogR #-}
getDCWatchdogR :: Handler Html 
getDCWatchdogR = entityTableHandler (DC'R DCWatchdogR) ([] :: [Filter DC.WatchdogT]) 

{-# NOINLINE getDCXmlsitemapR #-}
getDCXmlsitemapR :: Handler Html 
getDCXmlsitemapR = entityTableHandler (DC'R DCXmlsitemapR) ([] :: [Filter DC.XmlsitemapT]) 

{-# NOINLINE getDCXmlsitemapSitemapR #-}
getDCXmlsitemapSitemapR :: Handler Html 
getDCXmlsitemapSitemapR = entityTableHandler (DC'R DCXmlsitemapSitemapR) ([] :: [Filter DC.XmlsitemapSitemapT]) 

