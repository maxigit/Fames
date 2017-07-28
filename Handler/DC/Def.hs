-- Warning ! This code has been generated !
-- Handler
module Handler.DC.Def where
import Import
import DC


getDCAccesslogR :: Handler Html 
getDCAccesslogR = entityTableHandler (DC'R DCAccesslogR) ([] :: [Filter DC.AccesslogT]) 

getDCActionsR :: Handler Html 
getDCActionsR = entityTableHandler (DC'R DCActionsR) ([] :: [Filter DC.ActionsT]) 

getDCAdvancedHelpIndexR :: Handler Html 
getDCAdvancedHelpIndexR = entityTableHandler (DC'R DCAdvancedHelpIndexR) ([] :: [Filter DC.AdvancedHelpIndexT]) 

getDCAuthcacheP13nKeyValueR :: Handler Html 
getDCAuthcacheP13nKeyValueR = entityTableHandler (DC'R DCAuthcacheP13nKeyValueR) ([] :: [Filter DC.AuthcacheP13nKeyValueT]) 

getDCAuthmapR :: Handler Html 
getDCAuthmapR = entityTableHandler (DC'R DCAuthmapR) ([] :: [Filter DC.AuthmapT]) 

getDCBatchR :: Handler Html 
getDCBatchR = entityTableHandler (DC'R DCBatchR) ([] :: [Filter DC.BatchT]) 

getDCBlockR :: Handler Html 
getDCBlockR = entityTableHandler (DC'R DCBlockR) ([] :: [Filter DC.BlockT]) 

getDCBlockedIpsR :: Handler Html 
getDCBlockedIpsR = entityTableHandler (DC'R DCBlockedIpsR) ([] :: [Filter DC.BlockedIpsT]) 

getDCBlockCurrentSearchR :: Handler Html 
getDCBlockCurrentSearchR = entityTableHandler (DC'R DCBlockCurrentSearchR) ([] :: [Filter DC.BlockCurrentSearchT]) 

getDCBlockCustomR :: Handler Html 
getDCBlockCustomR = entityTableHandler (DC'R DCBlockCustomR) ([] :: [Filter DC.BlockCustomT]) 

getDCBlockNodeTypeR :: Handler Html 
getDCBlockNodeTypeR = entityTableHandler (DC'R DCBlockNodeTypeR) ([] :: [Filter DC.BlockNodeTypeT]) 

getDCBlockRoleR :: Handler Html 
getDCBlockRoleR = entityTableHandler (DC'R DCBlockRoleR) ([] :: [Filter DC.BlockRoleT]) 

getDCCacheR :: Handler Html 
getDCCacheR = entityTableHandler (DC'R DCCacheR) ([] :: [Filter DC.CacheT]) 

getDCCacheAdminMenuR :: Handler Html 
getDCCacheAdminMenuR = entityTableHandler (DC'R DCCacheAdminMenuR) ([] :: [Filter DC.CacheAdminMenuT]) 

getDCCacheAuthcacheDebugR :: Handler Html 
getDCCacheAuthcacheDebugR = entityTableHandler (DC'R DCCacheAuthcacheDebugR) ([] :: [Filter DC.CacheAuthcacheDebugT]) 

getDCCacheAuthcacheKeyR :: Handler Html 
getDCCacheAuthcacheKeyR = entityTableHandler (DC'R DCCacheAuthcacheKeyR) ([] :: [Filter DC.CacheAuthcacheKeyT]) 

getDCCacheAuthcacheP13nR :: Handler Html 
getDCCacheAuthcacheP13nR = entityTableHandler (DC'R DCCacheAuthcacheP13nR) ([] :: [Filter DC.CacheAuthcacheP13nT]) 

getDCCacheBlockR :: Handler Html 
getDCCacheBlockR = entityTableHandler (DC'R DCCacheBlockR) ([] :: [Filter DC.CacheBlockT]) 

getDCCacheBootstrapR :: Handler Html 
getDCCacheBootstrapR = entityTableHandler (DC'R DCCacheBootstrapR) ([] :: [Filter DC.CacheBootstrapT]) 

getDCCacheCommerceShippingRatesR :: Handler Html 
getDCCacheCommerceShippingRatesR = entityTableHandler (DC'R DCCacheCommerceShippingRatesR) ([] :: [Filter DC.CacheCommerceShippingRatesT]) 

getDCCacheDisplayCacheR :: Handler Html 
getDCCacheDisplayCacheR = entityTableHandler (DC'R DCCacheDisplayCacheR) ([] :: [Filter DC.CacheDisplayCacheT]) 

getDCCacheEntityCommentR :: Handler Html 
getDCCacheEntityCommentR = entityTableHandler (DC'R DCCacheEntityCommentR) ([] :: [Filter DC.CacheEntityCommentT]) 

getDCCacheEntityFileR :: Handler Html 
getDCCacheEntityFileR = entityTableHandler (DC'R DCCacheEntityFileR) ([] :: [Filter DC.CacheEntityFileT]) 

getDCCacheEntityMessageR :: Handler Html 
getDCCacheEntityMessageR = entityTableHandler (DC'R DCCacheEntityMessageR) ([] :: [Filter DC.CacheEntityMessageT]) 

getDCCacheEntityMessageTypeR :: Handler Html 
getDCCacheEntityMessageTypeR = entityTableHandler (DC'R DCCacheEntityMessageTypeR) ([] :: [Filter DC.CacheEntityMessageTypeT]) 

getDCCacheEntityMessageTypeCategoryR :: Handler Html 
getDCCacheEntityMessageTypeCategoryR = entityTableHandler (DC'R DCCacheEntityMessageTypeCategoryR) ([] :: [Filter DC.CacheEntityMessageTypeCategoryT]) 

getDCCacheEntityNodeR :: Handler Html 
getDCCacheEntityNodeR = entityTableHandler (DC'R DCCacheEntityNodeR) ([] :: [Filter DC.CacheEntityNodeT]) 

getDCCacheEntityTaxonomyTermR :: Handler Html 
getDCCacheEntityTaxonomyTermR = entityTableHandler (DC'R DCCacheEntityTaxonomyTermR) ([] :: [Filter DC.CacheEntityTaxonomyTermT]) 

getDCCacheEntityTaxonomyVocabularyR :: Handler Html 
getDCCacheEntityTaxonomyVocabularyR = entityTableHandler (DC'R DCCacheEntityTaxonomyVocabularyR) ([] :: [Filter DC.CacheEntityTaxonomyVocabularyT]) 

getDCCacheEntityUserR :: Handler Html 
getDCCacheEntityUserR = entityTableHandler (DC'R DCCacheEntityUserR) ([] :: [Filter DC.CacheEntityUserT]) 

getDCCacheFieldR :: Handler Html 
getDCCacheFieldR = entityTableHandler (DC'R DCCacheFieldR) ([] :: [Filter DC.CacheFieldT]) 

getDCCacheFilterR :: Handler Html 
getDCCacheFilterR = entityTableHandler (DC'R DCCacheFilterR) ([] :: [Filter DC.CacheFilterT]) 

getDCCacheFormR :: Handler Html 
getDCCacheFormR = entityTableHandler (DC'R DCCacheFormR) ([] :: [Filter DC.CacheFormT]) 

getDCCacheImageR :: Handler Html 
getDCCacheImageR = entityTableHandler (DC'R DCCacheImageR) ([] :: [Filter DC.CacheImageT]) 

getDCCacheLibrariesR :: Handler Html 
getDCCacheLibrariesR = entityTableHandler (DC'R DCCacheLibrariesR) ([] :: [Filter DC.CacheLibrariesT]) 

getDCCacheMenuR :: Handler Html 
getDCCacheMenuR = entityTableHandler (DC'R DCCacheMenuR) ([] :: [Filter DC.CacheMenuT]) 

getDCCacheMetatagR :: Handler Html 
getDCCacheMetatagR = entityTableHandler (DC'R DCCacheMetatagR) ([] :: [Filter DC.CacheMetatagT]) 

getDCCachePageR :: Handler Html 
getDCCachePageR = entityTableHandler (DC'R DCCachePageR) ([] :: [Filter DC.CachePageT]) 

getDCCachePathR :: Handler Html 
getDCCachePathR = entityTableHandler (DC'R DCCachePathR) ([] :: [Filter DC.CachePathT]) 

getDCCachePathAliasR :: Handler Html 
getDCCachePathAliasR = entityTableHandler (DC'R DCCachePathAliasR) ([] :: [Filter DC.CachePathAliasT]) 

getDCCachePathSourceR :: Handler Html 
getDCCachePathSourceR = entityTableHandler (DC'R DCCachePathSourceR) ([] :: [Filter DC.CachePathSourceT]) 

getDCCacheRulesR :: Handler Html 
getDCCacheRulesR = entityTableHandler (DC'R DCCacheRulesR) ([] :: [Filter DC.CacheRulesT]) 

getDCCacheTokenR :: Handler Html 
getDCCacheTokenR = entityTableHandler (DC'R DCCacheTokenR) ([] :: [Filter DC.CacheTokenT]) 

getDCCacheUpdateR :: Handler Html 
getDCCacheUpdateR = entityTableHandler (DC'R DCCacheUpdateR) ([] :: [Filter DC.CacheUpdateT]) 

getDCCacheViewsR :: Handler Html 
getDCCacheViewsR = entityTableHandler (DC'R DCCacheViewsR) ([] :: [Filter DC.CacheViewsT]) 

getDCCacheViewsDataR :: Handler Html 
getDCCacheViewsDataR = entityTableHandler (DC'R DCCacheViewsDataR) ([] :: [Filter DC.CacheViewsDataT]) 

getDCCmpMenuPermsR :: Handler Html 
getDCCmpMenuPermsR = entityTableHandler (DC'R DCCmpMenuPermsR) ([] :: [Filter DC.CmpMenuPermsT]) 

getDCCmpPermissionsR :: Handler Html 
getDCCmpPermissionsR = entityTableHandler (DC'R DCCmpPermissionsR) ([] :: [Filter DC.CmpPermissionsT]) 

getDCCommentR :: Handler Html 
getDCCommentR = entityTableHandler (DC'R DCCommentR) ([] :: [Filter DC.CommentT]) 

getDCCommerceAddressbookDefaultsR :: Handler Html 
getDCCommerceAddressbookDefaultsR = entityTableHandler (DC'R DCCommerceAddressbookDefaultsR) ([] :: [Filter DC.CommerceAddressbookDefaultsT]) 

getDCCommerceAutoskuPatternsR :: Handler Html 
getDCCommerceAutoskuPatternsR = entityTableHandler (DC'R DCCommerceAutoskuPatternsR) ([] :: [Filter DC.CommerceAutoskuPatternsT]) 

getDCCommerceCalculatedPriceR :: Handler Html 
getDCCommerceCalculatedPriceR = entityTableHandler (DC'R DCCommerceCalculatedPriceR) ([] :: [Filter DC.CommerceCalculatedPriceT]) 

getDCCommerceCheckoutPaneR :: Handler Html 
getDCCommerceCheckoutPaneR = entityTableHandler (DC'R DCCommerceCheckoutPaneR) ([] :: [Filter DC.CommerceCheckoutPaneT]) 

getDCCommerceCustomerProfileR :: Handler Html 
getDCCommerceCustomerProfileR = entityTableHandler (DC'R DCCommerceCustomerProfileR) ([] :: [Filter DC.CommerceCustomerProfileT]) 

getDCCommerceCustomerProfileRevisionR :: Handler Html 
getDCCommerceCustomerProfileRevisionR = entityTableHandler (DC'R DCCommerceCustomerProfileRevisionR) ([] :: [Filter DC.CommerceCustomerProfileRevisionT]) 

getDCCommerceDiscountR :: Handler Html 
getDCCommerceDiscountR = entityTableHandler (DC'R DCCommerceDiscountR) ([] :: [Filter DC.CommerceDiscountT]) 

getDCCommerceDiscountOfferR :: Handler Html 
getDCCommerceDiscountOfferR = entityTableHandler (DC'R DCCommerceDiscountOfferR) ([] :: [Filter DC.CommerceDiscountOfferT]) 

getDCCommerceFlatRateServiceR :: Handler Html 
getDCCommerceFlatRateServiceR = entityTableHandler (DC'R DCCommerceFlatRateServiceR) ([] :: [Filter DC.CommerceFlatRateServiceT]) 

getDCCommerceLineItemR :: Handler Html 
getDCCommerceLineItemR = entityTableHandler (DC'R DCCommerceLineItemR) ([] :: [Filter DC.CommerceLineItemT]) 

getDCCommerceOrderR :: Handler Html 
getDCCommerceOrderR = entityTableHandler (DC'R DCCommerceOrderR) ([] :: [Filter DC.CommerceOrderT]) 

getDCCommerceOrderRevisionR :: Handler Html 
getDCCommerceOrderRevisionR = entityTableHandler (DC'R DCCommerceOrderRevisionR) ([] :: [Filter DC.CommerceOrderRevisionT]) 

getDCCommercePaymentTransactionR :: Handler Html 
getDCCommercePaymentTransactionR = entityTableHandler (DC'R DCCommercePaymentTransactionR) ([] :: [Filter DC.CommercePaymentTransactionT]) 

getDCCommercePaymentTransactionRevisionR :: Handler Html 
getDCCommercePaymentTransactionRevisionR = entityTableHandler (DC'R DCCommercePaymentTransactionRevisionR) ([] :: [Filter DC.CommercePaymentTransactionRevisionT]) 

getDCCommerceProductR :: Handler Html 
getDCCommerceProductR = entityTableHandler (DC'R DCCommerceProductR) ([] :: [Filter DC.CommerceProductT]) 

getDCCommerceProductRevisionR :: Handler Html 
getDCCommerceProductRevisionR = entityTableHandler (DC'R DCCommerceProductRevisionR) ([] :: [Filter DC.CommerceProductRevisionT]) 

getDCCommerceProductTypeR :: Handler Html 
getDCCommerceProductTypeR = entityTableHandler (DC'R DCCommerceProductTypeR) ([] :: [Filter DC.CommerceProductTypeT]) 

getDCCommerceTaxRateR :: Handler Html 
getDCCommerceTaxRateR = entityTableHandler (DC'R DCCommerceTaxRateR) ([] :: [Filter DC.CommerceTaxRateT]) 

getDCCommerceTaxTypeR :: Handler Html 
getDCCommerceTaxTypeR = entityTableHandler (DC'R DCCommerceTaxTypeR) ([] :: [Filter DC.CommerceTaxTypeT]) 

getDCContactR :: Handler Html 
getDCContactR = entityTableHandler (DC'R DCContactR) ([] :: [Filter DC.ContactT]) 

getDCCtoolsCssCacheR :: Handler Html 
getDCCtoolsCssCacheR = entityTableHandler (DC'R DCCtoolsCssCacheR) ([] :: [Filter DC.CtoolsCssCacheT]) 

getDCCtoolsObjectCacheR :: Handler Html 
getDCCtoolsObjectCacheR = entityTableHandler (DC'R DCCtoolsObjectCacheR) ([] :: [Filter DC.CtoolsObjectCacheT]) 

getDCCurrentSearchR :: Handler Html 
getDCCurrentSearchR = entityTableHandler (DC'R DCCurrentSearchR) ([] :: [Filter DC.CurrentSearchT]) 

getDCDateFormatsR :: Handler Html 
getDCDateFormatsR = entityTableHandler (DC'R DCDateFormatsR) ([] :: [Filter DC.DateFormatsT]) 

getDCDateFormatLocaleR :: Handler Html 
getDCDateFormatLocaleR = entityTableHandler (DC'R DCDateFormatLocaleR) ([] :: [Filter DC.DateFormatLocaleT]) 

getDCDateFormatTypeR :: Handler Html 
getDCDateFormatTypeR = entityTableHandler (DC'R DCDateFormatTypeR) ([] :: [Filter DC.DateFormatTypeT]) 

getDCFacetapiR :: Handler Html 
getDCFacetapiR = entityTableHandler (DC'R DCFacetapiR) ([] :: [Filter DC.FacetapiT]) 

getDCFeedsImporterR :: Handler Html 
getDCFeedsImporterR = entityTableHandler (DC'R DCFeedsImporterR) ([] :: [Filter DC.FeedsImporterT]) 

getDCFeedsItemR :: Handler Html 
getDCFeedsItemR = entityTableHandler (DC'R DCFeedsItemR) ([] :: [Filter DC.FeedsItemT]) 

getDCFeedsLogR :: Handler Html 
getDCFeedsLogR = entityTableHandler (DC'R DCFeedsLogR) ([] :: [Filter DC.FeedsLogT]) 

getDCFeedsPushSubscriptionsR :: Handler Html 
getDCFeedsPushSubscriptionsR = entityTableHandler (DC'R DCFeedsPushSubscriptionsR) ([] :: [Filter DC.FeedsPushSubscriptionsT]) 

getDCFeedsSourceR :: Handler Html 
getDCFeedsSourceR = entityTableHandler (DC'R DCFeedsSourceR) ([] :: [Filter DC.FeedsSourceT]) 

getDCFeedsTamperR :: Handler Html 
getDCFeedsTamperR = entityTableHandler (DC'R DCFeedsTamperR) ([] :: [Filter DC.FeedsTamperT]) 

getDCFieldConfigR :: Handler Html 
getDCFieldConfigR = entityTableHandler (DC'R DCFieldConfigR) ([] :: [Filter DC.FieldConfigT]) 

getDCFieldConfigInstanceR :: Handler Html 
getDCFieldConfigInstanceR = entityTableHandler (DC'R DCFieldConfigInstanceR) ([] :: [Filter DC.FieldConfigInstanceT]) 

getDCFieldDataBodyR :: Handler Html 
getDCFieldDataBodyR = entityTableHandler (DC'R DCFieldDataBodyR) ([] :: [Filter DC.FieldDataBodyT]) 

getDCFieldDataCommentBodyR :: Handler Html 
getDCFieldDataCommentBodyR = entityTableHandler (DC'R DCFieldDataCommentBodyR) ([] :: [Filter DC.FieldDataCommentBodyT]) 

getDCFieldDataCommerceCustomerAddressR :: Handler Html 
getDCFieldDataCommerceCustomerAddressR = entityTableHandler (DC'R DCFieldDataCommerceCustomerAddressR) ([] :: [Filter DC.FieldDataCommerceCustomerAddressT]) 

getDCFieldDataCommerceCustomerBillingR :: Handler Html 
getDCFieldDataCommerceCustomerBillingR = entityTableHandler (DC'R DCFieldDataCommerceCustomerBillingR) ([] :: [Filter DC.FieldDataCommerceCustomerBillingT]) 

getDCFieldDataCommerceCustomerShippingR :: Handler Html 
getDCFieldDataCommerceCustomerShippingR = entityTableHandler (DC'R DCFieldDataCommerceCustomerShippingR) ([] :: [Filter DC.FieldDataCommerceCustomerShippingT]) 

getDCFieldDataCommerceDiscountsR :: Handler Html 
getDCFieldDataCommerceDiscountsR = entityTableHandler (DC'R DCFieldDataCommerceDiscountsR) ([] :: [Filter DC.FieldDataCommerceDiscountsT]) 

getDCFieldDataCommerceDiscountDateR :: Handler Html 
getDCFieldDataCommerceDiscountDateR = entityTableHandler (DC'R DCFieldDataCommerceDiscountDateR) ([] :: [Filter DC.FieldDataCommerceDiscountDateT]) 

getDCFieldDataCommerceDiscountOfferR :: Handler Html 
getDCFieldDataCommerceDiscountOfferR = entityTableHandler (DC'R DCFieldDataCommerceDiscountOfferR) ([] :: [Filter DC.FieldDataCommerceDiscountOfferT]) 

getDCFieldDataCommerceDisplayPathR :: Handler Html 
getDCFieldDataCommerceDisplayPathR = entityTableHandler (DC'R DCFieldDataCommerceDisplayPathR) ([] :: [Filter DC.FieldDataCommerceDisplayPathT]) 

getDCFieldDataCommerceFixedAmountR :: Handler Html 
getDCFieldDataCommerceFixedAmountR = entityTableHandler (DC'R DCFieldDataCommerceFixedAmountR) ([] :: [Filter DC.FieldDataCommerceFixedAmountT]) 

getDCFieldDataCommerceFreeProductsR :: Handler Html 
getDCFieldDataCommerceFreeProductsR = entityTableHandler (DC'R DCFieldDataCommerceFreeProductsR) ([] :: [Filter DC.FieldDataCommerceFreeProductsT]) 

getDCFieldDataCommerceFreeShippingR :: Handler Html 
getDCFieldDataCommerceFreeShippingR = entityTableHandler (DC'R DCFieldDataCommerceFreeShippingR) ([] :: [Filter DC.FieldDataCommerceFreeShippingT]) 

getDCFieldDataCommerceLineItemsR :: Handler Html 
getDCFieldDataCommerceLineItemsR = entityTableHandler (DC'R DCFieldDataCommerceLineItemsR) ([] :: [Filter DC.FieldDataCommerceLineItemsT]) 

getDCFieldDataCommerceOrderTotalR :: Handler Html 
getDCFieldDataCommerceOrderTotalR = entityTableHandler (DC'R DCFieldDataCommerceOrderTotalR) ([] :: [Filter DC.FieldDataCommerceOrderTotalT]) 

getDCFieldDataCommercePercentageR :: Handler Html 
getDCFieldDataCommercePercentageR = entityTableHandler (DC'R DCFieldDataCommercePercentageR) ([] :: [Filter DC.FieldDataCommercePercentageT]) 

getDCFieldDataCommercePriceR :: Handler Html 
getDCFieldDataCommercePriceR = entityTableHandler (DC'R DCFieldDataCommercePriceR) ([] :: [Filter DC.FieldDataCommercePriceT]) 

getDCFieldDataCommerceProductR :: Handler Html 
getDCFieldDataCommerceProductR = entityTableHandler (DC'R DCFieldDataCommerceProductR) ([] :: [Filter DC.FieldDataCommerceProductT]) 

getDCFieldDataCommerceShippingServiceR :: Handler Html 
getDCFieldDataCommerceShippingServiceR = entityTableHandler (DC'R DCFieldDataCommerceShippingServiceR) ([] :: [Filter DC.FieldDataCommerceShippingServiceT]) 

getDCFieldDataCommerceTotalR :: Handler Html 
getDCFieldDataCommerceTotalR = entityTableHandler (DC'R DCFieldDataCommerceTotalR) ([] :: [Filter DC.FieldDataCommerceTotalT]) 

getDCFieldDataCommerceUnitPriceR :: Handler Html 
getDCFieldDataCommerceUnitPriceR = entityTableHandler (DC'R DCFieldDataCommerceUnitPriceR) ([] :: [Filter DC.FieldDataCommerceUnitPriceT]) 

getDCFieldDataFieldBrandR :: Handler Html 
getDCFieldDataFieldBrandR = entityTableHandler (DC'R DCFieldDataFieldBrandR) ([] :: [Filter DC.FieldDataFieldBrandT]) 

getDCFieldDataFieldCollectionR :: Handler Html 
getDCFieldDataFieldCollectionR = entityTableHandler (DC'R DCFieldDataFieldCollectionR) ([] :: [Filter DC.FieldDataFieldCollectionT]) 

getDCFieldDataFieldColourR :: Handler Html 
getDCFieldDataFieldColourR = entityTableHandler (DC'R DCFieldDataFieldColourR) ([] :: [Filter DC.FieldDataFieldColourT]) 

getDCFieldDataFieldColourCodeR :: Handler Html 
getDCFieldDataFieldColourCodeR = entityTableHandler (DC'R DCFieldDataFieldColourCodeR) ([] :: [Filter DC.FieldDataFieldColourCodeT]) 

getDCFieldDataFieldHeadlineR :: Handler Html 
getDCFieldDataFieldHeadlineR = entityTableHandler (DC'R DCFieldDataFieldHeadlineR) ([] :: [Filter DC.FieldDataFieldHeadlineT]) 

getDCFieldDataFieldImageR :: Handler Html 
getDCFieldDataFieldImageR = entityTableHandler (DC'R DCFieldDataFieldImageR) ([] :: [Filter DC.FieldDataFieldImageT]) 

getDCFieldDataFieldImagesR :: Handler Html 
getDCFieldDataFieldImagesR = entityTableHandler (DC'R DCFieldDataFieldImagesR) ([] :: [Filter DC.FieldDataFieldImagesT]) 

getDCFieldDataFieldLinkR :: Handler Html 
getDCFieldDataFieldLinkR = entityTableHandler (DC'R DCFieldDataFieldLinkR) ([] :: [Filter DC.FieldDataFieldLinkT]) 

getDCFieldDataFieldPricePl01R :: Handler Html 
getDCFieldDataFieldPricePl01R = entityTableHandler (DC'R DCFieldDataFieldPricePl01R) ([] :: [Filter DC.FieldDataFieldPricePl01T]) 

getDCFieldDataFieldPricePl02R :: Handler Html 
getDCFieldDataFieldPricePl02R = entityTableHandler (DC'R DCFieldDataFieldPricePl02R) ([] :: [Filter DC.FieldDataFieldPricePl02T]) 

getDCFieldDataFieldPricePl03R :: Handler Html 
getDCFieldDataFieldPricePl03R = entityTableHandler (DC'R DCFieldDataFieldPricePl03R) ([] :: [Filter DC.FieldDataFieldPricePl03T]) 

getDCFieldDataFieldPricePl04R :: Handler Html 
getDCFieldDataFieldPricePl04R = entityTableHandler (DC'R DCFieldDataFieldPricePl04R) ([] :: [Filter DC.FieldDataFieldPricePl04T]) 

getDCFieldDataFieldPricePl05R :: Handler Html 
getDCFieldDataFieldPricePl05R = entityTableHandler (DC'R DCFieldDataFieldPricePl05R) ([] :: [Filter DC.FieldDataFieldPricePl05T]) 

getDCFieldDataFieldPricePl06R :: Handler Html 
getDCFieldDataFieldPricePl06R = entityTableHandler (DC'R DCFieldDataFieldPricePl06R) ([] :: [Filter DC.FieldDataFieldPricePl06T]) 

getDCFieldDataFieldPricePl07R :: Handler Html 
getDCFieldDataFieldPricePl07R = entityTableHandler (DC'R DCFieldDataFieldPricePl07R) ([] :: [Filter DC.FieldDataFieldPricePl07T]) 

getDCFieldDataFieldPricePl08R :: Handler Html 
getDCFieldDataFieldPricePl08R = entityTableHandler (DC'R DCFieldDataFieldPricePl08R) ([] :: [Filter DC.FieldDataFieldPricePl08T]) 

getDCFieldDataFieldPricePl09R :: Handler Html 
getDCFieldDataFieldPricePl09R = entityTableHandler (DC'R DCFieldDataFieldPricePl09R) ([] :: [Filter DC.FieldDataFieldPricePl09T]) 

getDCFieldDataFieldPricePl10R :: Handler Html 
getDCFieldDataFieldPricePl10R = entityTableHandler (DC'R DCFieldDataFieldPricePl10R) ([] :: [Filter DC.FieldDataFieldPricePl10T]) 

getDCFieldDataFieldPricePl11R :: Handler Html 
getDCFieldDataFieldPricePl11R = entityTableHandler (DC'R DCFieldDataFieldPricePl11R) ([] :: [Filter DC.FieldDataFieldPricePl11T]) 

getDCFieldDataFieldPricePl12R :: Handler Html 
getDCFieldDataFieldPricePl12R = entityTableHandler (DC'R DCFieldDataFieldPricePl12R) ([] :: [Filter DC.FieldDataFieldPricePl12T]) 

getDCFieldDataFieldPricePl13R :: Handler Html 
getDCFieldDataFieldPricePl13R = entityTableHandler (DC'R DCFieldDataFieldPricePl13R) ([] :: [Filter DC.FieldDataFieldPricePl13T]) 

getDCFieldDataFieldPricePl14R :: Handler Html 
getDCFieldDataFieldPricePl14R = entityTableHandler (DC'R DCFieldDataFieldPricePl14R) ([] :: [Filter DC.FieldDataFieldPricePl14T]) 

getDCFieldDataFieldProductR :: Handler Html 
getDCFieldDataFieldProductR = entityTableHandler (DC'R DCFieldDataFieldProductR) ([] :: [Filter DC.FieldDataFieldProductT]) 

getDCFieldDataFieldProductCategoryR :: Handler Html 
getDCFieldDataFieldProductCategoryR = entityTableHandler (DC'R DCFieldDataFieldProductCategoryR) ([] :: [Filter DC.FieldDataFieldProductCategoryT]) 

getDCFieldDataFieldRequiredDeliveryDateR :: Handler Html 
getDCFieldDataFieldRequiredDeliveryDateR = entityTableHandler (DC'R DCFieldDataFieldRequiredDeliveryDateR) ([] :: [Filter DC.FieldDataFieldRequiredDeliveryDateT]) 

getDCFieldDataFieldRgbR :: Handler Html 
getDCFieldDataFieldRgbR = entityTableHandler (DC'R DCFieldDataFieldRgbR) ([] :: [Filter DC.FieldDataFieldRgbT]) 

getDCFieldDataFieldSpecialRequestR :: Handler Html 
getDCFieldDataFieldSpecialRequestR = entityTableHandler (DC'R DCFieldDataFieldSpecialRequestR) ([] :: [Filter DC.FieldDataFieldSpecialRequestT]) 

getDCFieldDataFieldStockStatusR :: Handler Html 
getDCFieldDataFieldStockStatusR = entityTableHandler (DC'R DCFieldDataFieldStockStatusR) ([] :: [Filter DC.FieldDataFieldStockStatusT]) 

getDCFieldDataFieldTaglineR :: Handler Html 
getDCFieldDataFieldTaglineR = entityTableHandler (DC'R DCFieldDataFieldTaglineR) ([] :: [Filter DC.FieldDataFieldTaglineT]) 

getDCFieldDataFieldTrimColourR :: Handler Html 
getDCFieldDataFieldTrimColourR = entityTableHandler (DC'R DCFieldDataFieldTrimColourR) ([] :: [Filter DC.FieldDataFieldTrimColourT]) 

getDCFieldDataFieldWholesalePriceR :: Handler Html 
getDCFieldDataFieldWholesalePriceR = entityTableHandler (DC'R DCFieldDataFieldWholesalePriceR) ([] :: [Filter DC.FieldDataFieldWholesalePriceT]) 

getDCFieldDataInlineConditionsR :: Handler Html 
getDCFieldDataInlineConditionsR = entityTableHandler (DC'R DCFieldDataInlineConditionsR) ([] :: [Filter DC.FieldDataInlineConditionsT]) 

getDCFieldDataMessageCommerceBodyR :: Handler Html 
getDCFieldDataMessageCommerceBodyR = entityTableHandler (DC'R DCFieldDataMessageCommerceBodyR) ([] :: [Filter DC.FieldDataMessageCommerceBodyT]) 

getDCFieldDataMessageCommerceLineItemR :: Handler Html 
getDCFieldDataMessageCommerceLineItemR = entityTableHandler (DC'R DCFieldDataMessageCommerceLineItemR) ([] :: [Filter DC.FieldDataMessageCommerceLineItemT]) 

getDCFieldDataMessageCommerceOrderR :: Handler Html 
getDCFieldDataMessageCommerceOrderR = entityTableHandler (DC'R DCFieldDataMessageCommerceOrderR) ([] :: [Filter DC.FieldDataMessageCommerceOrderT]) 

getDCFieldDataMessageCommercePaymentR :: Handler Html 
getDCFieldDataMessageCommercePaymentR = entityTableHandler (DC'R DCFieldDataMessageCommercePaymentR) ([] :: [Filter DC.FieldDataMessageCommercePaymentT]) 

getDCFieldDataMessageOrderDisplayNameR :: Handler Html 
getDCFieldDataMessageOrderDisplayNameR = entityTableHandler (DC'R DCFieldDataMessageOrderDisplayNameR) ([] :: [Filter DC.FieldDataMessageOrderDisplayNameT]) 

getDCFieldDataMessageTextR :: Handler Html 
getDCFieldDataMessageTextR = entityTableHandler (DC'R DCFieldDataMessageTextR) ([] :: [Filter DC.FieldDataMessageTextT]) 

getDCFieldDataMessageTextSubjectR :: Handler Html 
getDCFieldDataMessageTextSubjectR = entityTableHandler (DC'R DCFieldDataMessageTextSubjectR) ([] :: [Filter DC.FieldDataMessageTextSubjectT]) 

getDCFieldDataTitleFieldR :: Handler Html 
getDCFieldDataTitleFieldR = entityTableHandler (DC'R DCFieldDataTitleFieldR) ([] :: [Filter DC.FieldDataTitleFieldT]) 

getDCFieldDeletedData11R :: Handler Html 
getDCFieldDeletedData11R = entityTableHandler (DC'R DCFieldDeletedData11R) ([] :: [Filter DC.FieldDeletedData11T]) 

getDCFieldDeletedData22R :: Handler Html 
getDCFieldDeletedData22R = entityTableHandler (DC'R DCFieldDeletedData22R) ([] :: [Filter DC.FieldDeletedData22T]) 

getDCFieldDeletedData23R :: Handler Html 
getDCFieldDeletedData23R = entityTableHandler (DC'R DCFieldDeletedData23R) ([] :: [Filter DC.FieldDeletedData23T]) 

getDCFieldDeletedData24R :: Handler Html 
getDCFieldDeletedData24R = entityTableHandler (DC'R DCFieldDeletedData24R) ([] :: [Filter DC.FieldDeletedData24T]) 

getDCFieldDeletedData25R :: Handler Html 
getDCFieldDeletedData25R = entityTableHandler (DC'R DCFieldDeletedData25R) ([] :: [Filter DC.FieldDeletedData25T]) 

getDCFieldDeletedRevision11R :: Handler Html 
getDCFieldDeletedRevision11R = entityTableHandler (DC'R DCFieldDeletedRevision11R) ([] :: [Filter DC.FieldDeletedRevision11T]) 

getDCFieldDeletedRevision22R :: Handler Html 
getDCFieldDeletedRevision22R = entityTableHandler (DC'R DCFieldDeletedRevision22R) ([] :: [Filter DC.FieldDeletedRevision22T]) 

getDCFieldDeletedRevision23R :: Handler Html 
getDCFieldDeletedRevision23R = entityTableHandler (DC'R DCFieldDeletedRevision23R) ([] :: [Filter DC.FieldDeletedRevision23T]) 

getDCFieldDeletedRevision24R :: Handler Html 
getDCFieldDeletedRevision24R = entityTableHandler (DC'R DCFieldDeletedRevision24R) ([] :: [Filter DC.FieldDeletedRevision24T]) 

getDCFieldDeletedRevision25R :: Handler Html 
getDCFieldDeletedRevision25R = entityTableHandler (DC'R DCFieldDeletedRevision25R) ([] :: [Filter DC.FieldDeletedRevision25T]) 

getDCFieldGroupR :: Handler Html 
getDCFieldGroupR = entityTableHandler (DC'R DCFieldGroupR) ([] :: [Filter DC.FieldGroupT]) 

getDCFieldRevisionBodyR :: Handler Html 
getDCFieldRevisionBodyR = entityTableHandler (DC'R DCFieldRevisionBodyR) ([] :: [Filter DC.FieldRevisionBodyT]) 

getDCFieldRevisionCommentBodyR :: Handler Html 
getDCFieldRevisionCommentBodyR = entityTableHandler (DC'R DCFieldRevisionCommentBodyR) ([] :: [Filter DC.FieldRevisionCommentBodyT]) 

getDCFieldRevisionCommerceCustomerAddressR :: Handler Html 
getDCFieldRevisionCommerceCustomerAddressR = entityTableHandler (DC'R DCFieldRevisionCommerceCustomerAddressR) ([] :: [Filter DC.FieldRevisionCommerceCustomerAddressT]) 

getDCFieldRevisionCommerceCustomerBillingR :: Handler Html 
getDCFieldRevisionCommerceCustomerBillingR = entityTableHandler (DC'R DCFieldRevisionCommerceCustomerBillingR) ([] :: [Filter DC.FieldRevisionCommerceCustomerBillingT]) 

getDCFieldRevisionCommerceCustomerShippingR :: Handler Html 
getDCFieldRevisionCommerceCustomerShippingR = entityTableHandler (DC'R DCFieldRevisionCommerceCustomerShippingR) ([] :: [Filter DC.FieldRevisionCommerceCustomerShippingT]) 

getDCFieldRevisionCommerceDiscountsR :: Handler Html 
getDCFieldRevisionCommerceDiscountsR = entityTableHandler (DC'R DCFieldRevisionCommerceDiscountsR) ([] :: [Filter DC.FieldRevisionCommerceDiscountsT]) 

getDCFieldRevisionCommerceDiscountDateR :: Handler Html 
getDCFieldRevisionCommerceDiscountDateR = entityTableHandler (DC'R DCFieldRevisionCommerceDiscountDateR) ([] :: [Filter DC.FieldRevisionCommerceDiscountDateT]) 

getDCFieldRevisionCommerceDiscountOfferR :: Handler Html 
getDCFieldRevisionCommerceDiscountOfferR = entityTableHandler (DC'R DCFieldRevisionCommerceDiscountOfferR) ([] :: [Filter DC.FieldRevisionCommerceDiscountOfferT]) 

getDCFieldRevisionCommerceDisplayPathR :: Handler Html 
getDCFieldRevisionCommerceDisplayPathR = entityTableHandler (DC'R DCFieldRevisionCommerceDisplayPathR) ([] :: [Filter DC.FieldRevisionCommerceDisplayPathT]) 

getDCFieldRevisionCommerceFixedAmountR :: Handler Html 
getDCFieldRevisionCommerceFixedAmountR = entityTableHandler (DC'R DCFieldRevisionCommerceFixedAmountR) ([] :: [Filter DC.FieldRevisionCommerceFixedAmountT]) 

getDCFieldRevisionCommerceFreeProductsR :: Handler Html 
getDCFieldRevisionCommerceFreeProductsR = entityTableHandler (DC'R DCFieldRevisionCommerceFreeProductsR) ([] :: [Filter DC.FieldRevisionCommerceFreeProductsT]) 

getDCFieldRevisionCommerceFreeShippingR :: Handler Html 
getDCFieldRevisionCommerceFreeShippingR = entityTableHandler (DC'R DCFieldRevisionCommerceFreeShippingR) ([] :: [Filter DC.FieldRevisionCommerceFreeShippingT]) 

getDCFieldRevisionCommerceLineItemsR :: Handler Html 
getDCFieldRevisionCommerceLineItemsR = entityTableHandler (DC'R DCFieldRevisionCommerceLineItemsR) ([] :: [Filter DC.FieldRevisionCommerceLineItemsT]) 

getDCFieldRevisionCommerceOrderTotalR :: Handler Html 
getDCFieldRevisionCommerceOrderTotalR = entityTableHandler (DC'R DCFieldRevisionCommerceOrderTotalR) ([] :: [Filter DC.FieldRevisionCommerceOrderTotalT]) 

getDCFieldRevisionCommercePercentageR :: Handler Html 
getDCFieldRevisionCommercePercentageR = entityTableHandler (DC'R DCFieldRevisionCommercePercentageR) ([] :: [Filter DC.FieldRevisionCommercePercentageT]) 

getDCFieldRevisionCommercePriceR :: Handler Html 
getDCFieldRevisionCommercePriceR = entityTableHandler (DC'R DCFieldRevisionCommercePriceR) ([] :: [Filter DC.FieldRevisionCommercePriceT]) 

getDCFieldRevisionCommerceProductR :: Handler Html 
getDCFieldRevisionCommerceProductR = entityTableHandler (DC'R DCFieldRevisionCommerceProductR) ([] :: [Filter DC.FieldRevisionCommerceProductT]) 

getDCFieldRevisionCommerceShippingServiceR :: Handler Html 
getDCFieldRevisionCommerceShippingServiceR = entityTableHandler (DC'R DCFieldRevisionCommerceShippingServiceR) ([] :: [Filter DC.FieldRevisionCommerceShippingServiceT]) 

getDCFieldRevisionCommerceTotalR :: Handler Html 
getDCFieldRevisionCommerceTotalR = entityTableHandler (DC'R DCFieldRevisionCommerceTotalR) ([] :: [Filter DC.FieldRevisionCommerceTotalT]) 

getDCFieldRevisionCommerceUnitPriceR :: Handler Html 
getDCFieldRevisionCommerceUnitPriceR = entityTableHandler (DC'R DCFieldRevisionCommerceUnitPriceR) ([] :: [Filter DC.FieldRevisionCommerceUnitPriceT]) 

getDCFieldRevisionFieldBrandR :: Handler Html 
getDCFieldRevisionFieldBrandR = entityTableHandler (DC'R DCFieldRevisionFieldBrandR) ([] :: [Filter DC.FieldRevisionFieldBrandT]) 

getDCFieldRevisionFieldCollectionR :: Handler Html 
getDCFieldRevisionFieldCollectionR = entityTableHandler (DC'R DCFieldRevisionFieldCollectionR) ([] :: [Filter DC.FieldRevisionFieldCollectionT]) 

getDCFieldRevisionFieldColourR :: Handler Html 
getDCFieldRevisionFieldColourR = entityTableHandler (DC'R DCFieldRevisionFieldColourR) ([] :: [Filter DC.FieldRevisionFieldColourT]) 

getDCFieldRevisionFieldColourCodeR :: Handler Html 
getDCFieldRevisionFieldColourCodeR = entityTableHandler (DC'R DCFieldRevisionFieldColourCodeR) ([] :: [Filter DC.FieldRevisionFieldColourCodeT]) 

getDCFieldRevisionFieldHeadlineR :: Handler Html 
getDCFieldRevisionFieldHeadlineR = entityTableHandler (DC'R DCFieldRevisionFieldHeadlineR) ([] :: [Filter DC.FieldRevisionFieldHeadlineT]) 

getDCFieldRevisionFieldImageR :: Handler Html 
getDCFieldRevisionFieldImageR = entityTableHandler (DC'R DCFieldRevisionFieldImageR) ([] :: [Filter DC.FieldRevisionFieldImageT]) 

getDCFieldRevisionFieldImagesR :: Handler Html 
getDCFieldRevisionFieldImagesR = entityTableHandler (DC'R DCFieldRevisionFieldImagesR) ([] :: [Filter DC.FieldRevisionFieldImagesT]) 

getDCFieldRevisionFieldLinkR :: Handler Html 
getDCFieldRevisionFieldLinkR = entityTableHandler (DC'R DCFieldRevisionFieldLinkR) ([] :: [Filter DC.FieldRevisionFieldLinkT]) 

getDCFieldRevisionFieldPricePl01R :: Handler Html 
getDCFieldRevisionFieldPricePl01R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl01R) ([] :: [Filter DC.FieldRevisionFieldPricePl01T]) 

getDCFieldRevisionFieldPricePl02R :: Handler Html 
getDCFieldRevisionFieldPricePl02R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl02R) ([] :: [Filter DC.FieldRevisionFieldPricePl02T]) 

getDCFieldRevisionFieldPricePl03R :: Handler Html 
getDCFieldRevisionFieldPricePl03R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl03R) ([] :: [Filter DC.FieldRevisionFieldPricePl03T]) 

getDCFieldRevisionFieldPricePl04R :: Handler Html 
getDCFieldRevisionFieldPricePl04R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl04R) ([] :: [Filter DC.FieldRevisionFieldPricePl04T]) 

getDCFieldRevisionFieldPricePl05R :: Handler Html 
getDCFieldRevisionFieldPricePl05R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl05R) ([] :: [Filter DC.FieldRevisionFieldPricePl05T]) 

getDCFieldRevisionFieldPricePl06R :: Handler Html 
getDCFieldRevisionFieldPricePl06R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl06R) ([] :: [Filter DC.FieldRevisionFieldPricePl06T]) 

getDCFieldRevisionFieldPricePl07R :: Handler Html 
getDCFieldRevisionFieldPricePl07R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl07R) ([] :: [Filter DC.FieldRevisionFieldPricePl07T]) 

getDCFieldRevisionFieldPricePl08R :: Handler Html 
getDCFieldRevisionFieldPricePl08R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl08R) ([] :: [Filter DC.FieldRevisionFieldPricePl08T]) 

getDCFieldRevisionFieldPricePl09R :: Handler Html 
getDCFieldRevisionFieldPricePl09R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl09R) ([] :: [Filter DC.FieldRevisionFieldPricePl09T]) 

getDCFieldRevisionFieldPricePl10R :: Handler Html 
getDCFieldRevisionFieldPricePl10R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl10R) ([] :: [Filter DC.FieldRevisionFieldPricePl10T]) 

getDCFieldRevisionFieldPricePl11R :: Handler Html 
getDCFieldRevisionFieldPricePl11R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl11R) ([] :: [Filter DC.FieldRevisionFieldPricePl11T]) 

getDCFieldRevisionFieldPricePl12R :: Handler Html 
getDCFieldRevisionFieldPricePl12R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl12R) ([] :: [Filter DC.FieldRevisionFieldPricePl12T]) 

getDCFieldRevisionFieldPricePl13R :: Handler Html 
getDCFieldRevisionFieldPricePl13R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl13R) ([] :: [Filter DC.FieldRevisionFieldPricePl13T]) 

getDCFieldRevisionFieldPricePl14R :: Handler Html 
getDCFieldRevisionFieldPricePl14R = entityTableHandler (DC'R DCFieldRevisionFieldPricePl14R) ([] :: [Filter DC.FieldRevisionFieldPricePl14T]) 

getDCFieldRevisionFieldProductR :: Handler Html 
getDCFieldRevisionFieldProductR = entityTableHandler (DC'R DCFieldRevisionFieldProductR) ([] :: [Filter DC.FieldRevisionFieldProductT]) 

getDCFieldRevisionFieldProductCategoryR :: Handler Html 
getDCFieldRevisionFieldProductCategoryR = entityTableHandler (DC'R DCFieldRevisionFieldProductCategoryR) ([] :: [Filter DC.FieldRevisionFieldProductCategoryT]) 

getDCFieldRevisionFieldRequiredDeliveryDateR :: Handler Html 
getDCFieldRevisionFieldRequiredDeliveryDateR = entityTableHandler (DC'R DCFieldRevisionFieldRequiredDeliveryDateR) ([] :: [Filter DC.FieldRevisionFieldRequiredDeliveryDateT]) 

getDCFieldRevisionFieldRgbR :: Handler Html 
getDCFieldRevisionFieldRgbR = entityTableHandler (DC'R DCFieldRevisionFieldRgbR) ([] :: [Filter DC.FieldRevisionFieldRgbT]) 

getDCFieldRevisionFieldSpecialRequestR :: Handler Html 
getDCFieldRevisionFieldSpecialRequestR = entityTableHandler (DC'R DCFieldRevisionFieldSpecialRequestR) ([] :: [Filter DC.FieldRevisionFieldSpecialRequestT]) 

getDCFieldRevisionFieldStockStatusR :: Handler Html 
getDCFieldRevisionFieldStockStatusR = entityTableHandler (DC'R DCFieldRevisionFieldStockStatusR) ([] :: [Filter DC.FieldRevisionFieldStockStatusT]) 

getDCFieldRevisionFieldTaglineR :: Handler Html 
getDCFieldRevisionFieldTaglineR = entityTableHandler (DC'R DCFieldRevisionFieldTaglineR) ([] :: [Filter DC.FieldRevisionFieldTaglineT]) 

getDCFieldRevisionFieldTrimColourR :: Handler Html 
getDCFieldRevisionFieldTrimColourR = entityTableHandler (DC'R DCFieldRevisionFieldTrimColourR) ([] :: [Filter DC.FieldRevisionFieldTrimColourT]) 

getDCFieldRevisionFieldWholesalePriceR :: Handler Html 
getDCFieldRevisionFieldWholesalePriceR = entityTableHandler (DC'R DCFieldRevisionFieldWholesalePriceR) ([] :: [Filter DC.FieldRevisionFieldWholesalePriceT]) 

getDCFieldRevisionInlineConditionsR :: Handler Html 
getDCFieldRevisionInlineConditionsR = entityTableHandler (DC'R DCFieldRevisionInlineConditionsR) ([] :: [Filter DC.FieldRevisionInlineConditionsT]) 

getDCFieldRevisionMessageCommerceBodyR :: Handler Html 
getDCFieldRevisionMessageCommerceBodyR = entityTableHandler (DC'R DCFieldRevisionMessageCommerceBodyR) ([] :: [Filter DC.FieldRevisionMessageCommerceBodyT]) 

getDCFieldRevisionMessageCommerceLineItemR :: Handler Html 
getDCFieldRevisionMessageCommerceLineItemR = entityTableHandler (DC'R DCFieldRevisionMessageCommerceLineItemR) ([] :: [Filter DC.FieldRevisionMessageCommerceLineItemT]) 

getDCFieldRevisionMessageCommerceOrderR :: Handler Html 
getDCFieldRevisionMessageCommerceOrderR = entityTableHandler (DC'R DCFieldRevisionMessageCommerceOrderR) ([] :: [Filter DC.FieldRevisionMessageCommerceOrderT]) 

getDCFieldRevisionMessageCommercePaymentR :: Handler Html 
getDCFieldRevisionMessageCommercePaymentR = entityTableHandler (DC'R DCFieldRevisionMessageCommercePaymentR) ([] :: [Filter DC.FieldRevisionMessageCommercePaymentT]) 

getDCFieldRevisionMessageOrderDisplayNameR :: Handler Html 
getDCFieldRevisionMessageOrderDisplayNameR = entityTableHandler (DC'R DCFieldRevisionMessageOrderDisplayNameR) ([] :: [Filter DC.FieldRevisionMessageOrderDisplayNameT]) 

getDCFieldRevisionMessageTextR :: Handler Html 
getDCFieldRevisionMessageTextR = entityTableHandler (DC'R DCFieldRevisionMessageTextR) ([] :: [Filter DC.FieldRevisionMessageTextT]) 

getDCFieldRevisionMessageTextSubjectR :: Handler Html 
getDCFieldRevisionMessageTextSubjectR = entityTableHandler (DC'R DCFieldRevisionMessageTextSubjectR) ([] :: [Filter DC.FieldRevisionMessageTextSubjectT]) 

getDCFieldRevisionTitleFieldR :: Handler Html 
getDCFieldRevisionTitleFieldR = entityTableHandler (DC'R DCFieldRevisionTitleFieldR) ([] :: [Filter DC.FieldRevisionTitleFieldT]) 

getDCFileManagedR :: Handler Html 
getDCFileManagedR = entityTableHandler (DC'R DCFileManagedR) ([] :: [Filter DC.FileManagedT]) 

getDCFileUsageR :: Handler Html 
getDCFileUsageR = entityTableHandler (DC'R DCFileUsageR) ([] :: [Filter DC.FileUsageT]) 

getDCFilterR :: Handler Html 
getDCFilterR = entityTableHandler (DC'R DCFilterR) ([] :: [Filter DC.FilterT]) 

getDCFilterFormatR :: Handler Html 
getDCFilterFormatR = entityTableHandler (DC'R DCFilterFormatR) ([] :: [Filter DC.FilterFormatT]) 

getDCFloodR :: Handler Html 
getDCFloodR = entityTableHandler (DC'R DCFloodR) ([] :: [Filter DC.FloodT]) 

getDCHistoryR :: Handler Html 
getDCHistoryR = entityTableHandler (DC'R DCHistoryR) ([] :: [Filter DC.HistoryT]) 

getDCHoneypotUserR :: Handler Html 
getDCHoneypotUserR = entityTableHandler (DC'R DCHoneypotUserR) ([] :: [Filter DC.HoneypotUserT]) 

getDCImageEffectsR :: Handler Html 
getDCImageEffectsR = entityTableHandler (DC'R DCImageEffectsR) ([] :: [Filter DC.ImageEffectsT]) 

getDCImageStylesR :: Handler Html 
getDCImageStylesR = entityTableHandler (DC'R DCImageStylesR) ([] :: [Filter DC.ImageStylesT]) 

getDCJobScheduleR :: Handler Html 
getDCJobScheduleR = entityTableHandler (DC'R DCJobScheduleR) ([] :: [Filter DC.JobScheduleT]) 

getDCMasqueradeR :: Handler Html 
getDCMasqueradeR = entityTableHandler (DC'R DCMasqueradeR) ([] :: [Filter DC.MasqueradeT]) 

getDCMasqueradeUsersR :: Handler Html 
getDCMasqueradeUsersR = entityTableHandler (DC'R DCMasqueradeUsersR) ([] :: [Filter DC.MasqueradeUsersT]) 

getDCMegamenuR :: Handler Html 
getDCMegamenuR = entityTableHandler (DC'R DCMegamenuR) ([] :: [Filter DC.MegamenuT]) 

getDCMenuCustomR :: Handler Html 
getDCMenuCustomR = entityTableHandler (DC'R DCMenuCustomR) ([] :: [Filter DC.MenuCustomT]) 

getDCMenuLinksR :: Handler Html 
getDCMenuLinksR = entityTableHandler (DC'R DCMenuLinksR) ([] :: [Filter DC.MenuLinksT]) 

getDCMenuRouterR :: Handler Html 
getDCMenuRouterR = entityTableHandler (DC'R DCMenuRouterR) ([] :: [Filter DC.MenuRouterT]) 

getDCMessageR :: Handler Html 
getDCMessageR = entityTableHandler (DC'R DCMessageR) ([] :: [Filter DC.MessageT]) 

getDCMessageTypeR :: Handler Html 
getDCMessageTypeR = entityTableHandler (DC'R DCMessageTypeR) ([] :: [Filter DC.MessageTypeT]) 

getDCMessageTypeCategoryR :: Handler Html 
getDCMessageTypeCategoryR = entityTableHandler (DC'R DCMessageTypeCategoryR) ([] :: [Filter DC.MessageTypeCategoryT]) 

getDCMetatagR :: Handler Html 
getDCMetatagR = entityTableHandler (DC'R DCMetatagR) ([] :: [Filter DC.MetatagT]) 

getDCMetatagConfigR :: Handler Html 
getDCMetatagConfigR = entityTableHandler (DC'R DCMetatagConfigR) ([] :: [Filter DC.MetatagConfigT]) 

getDCMigrateLogR :: Handler Html 
getDCMigrateLogR = entityTableHandler (DC'R DCMigrateLogR) ([] :: [Filter DC.MigrateLogT]) 

getDCMigrateMapCommercekickstartadpushR :: Handler Html 
getDCMigrateMapCommercekickstartadpushR = entityTableHandler (DC'R DCMigrateMapCommercekickstartadpushR) ([] :: [Filter DC.MigrateMapCommercekickstartadpushT]) 

getDCMigrateMapCommercekickstartnodeR :: Handler Html 
getDCMigrateMapCommercekickstartnodeR = entityTableHandler (DC'R DCMigrateMapCommercekickstartnodeR) ([] :: [Filter DC.MigrateMapCommercekickstartnodeT]) 

getDCMigrateMapCommercekickstartpagesR :: Handler Html 
getDCMigrateMapCommercekickstartpagesR = entityTableHandler (DC'R DCMigrateMapCommercekickstartpagesR) ([] :: [Filter DC.MigrateMapCommercekickstartpagesT]) 

getDCMigrateMapCommercekickstartproductR :: Handler Html 
getDCMigrateMapCommercekickstartproductR = entityTableHandler (DC'R DCMigrateMapCommercekickstartproductR) ([] :: [Filter DC.MigrateMapCommercekickstartproductT]) 

getDCMigrateMapCommercekickstartslideshowR :: Handler Html 
getDCMigrateMapCommercekickstartslideshowR = entityTableHandler (DC'R DCMigrateMapCommercekickstartslideshowR) ([] :: [Filter DC.MigrateMapCommercekickstartslideshowT]) 

getDCMigrateMessageCommercekickstartadpushR :: Handler Html 
getDCMigrateMessageCommercekickstartadpushR = entityTableHandler (DC'R DCMigrateMessageCommercekickstartadpushR) ([] :: [Filter DC.MigrateMessageCommercekickstartadpushT]) 

getDCMigrateMessageCommercekickstartnodeR :: Handler Html 
getDCMigrateMessageCommercekickstartnodeR = entityTableHandler (DC'R DCMigrateMessageCommercekickstartnodeR) ([] :: [Filter DC.MigrateMessageCommercekickstartnodeT]) 

getDCMigrateMessageCommercekickstartpagesR :: Handler Html 
getDCMigrateMessageCommercekickstartpagesR = entityTableHandler (DC'R DCMigrateMessageCommercekickstartpagesR) ([] :: [Filter DC.MigrateMessageCommercekickstartpagesT]) 

getDCMigrateMessageCommercekickstartproductR :: Handler Html 
getDCMigrateMessageCommercekickstartproductR = entityTableHandler (DC'R DCMigrateMessageCommercekickstartproductR) ([] :: [Filter DC.MigrateMessageCommercekickstartproductT]) 

getDCMigrateMessageCommercekickstartslideshowR :: Handler Html 
getDCMigrateMessageCommercekickstartslideshowR = entityTableHandler (DC'R DCMigrateMessageCommercekickstartslideshowR) ([] :: [Filter DC.MigrateMessageCommercekickstartslideshowT]) 

getDCMigrateStatusR :: Handler Html 
getDCMigrateStatusR = entityTableHandler (DC'R DCMigrateStatusR) ([] :: [Filter DC.MigrateStatusT]) 

getDCNodeR :: Handler Html 
getDCNodeR = entityTableHandler (DC'R DCNodeR) ([] :: [Filter DC.NodeT]) 

getDCNodeAccessR :: Handler Html 
getDCNodeAccessR = entityTableHandler (DC'R DCNodeAccessR) ([] :: [Filter DC.NodeAccessT]) 

getDCNodeCommentStatisticsR :: Handler Html 
getDCNodeCommentStatisticsR = entityTableHandler (DC'R DCNodeCommentStatisticsR) ([] :: [Filter DC.NodeCommentStatisticsT]) 

getDCNodeCounterR :: Handler Html 
getDCNodeCounterR = entityTableHandler (DC'R DCNodeCounterR) ([] :: [Filter DC.NodeCounterT]) 

getDCNodeRevisionR :: Handler Html 
getDCNodeRevisionR = entityTableHandler (DC'R DCNodeRevisionR) ([] :: [Filter DC.NodeRevisionT]) 

getDCNodeSpambotR :: Handler Html 
getDCNodeSpambotR = entityTableHandler (DC'R DCNodeSpambotR) ([] :: [Filter DC.NodeSpambotT]) 

getDCNodeTypeR :: Handler Html 
getDCNodeTypeR = entityTableHandler (DC'R DCNodeTypeR) ([] :: [Filter DC.NodeTypeT]) 

getDCPageTitleR :: Handler Html 
getDCPageTitleR = entityTableHandler (DC'R DCPageTitleR) ([] :: [Filter DC.PageTitleT]) 

getDCQueueR :: Handler Html 
getDCQueueR = entityTableHandler (DC'R DCQueueR) ([] :: [Filter DC.QueueT]) 

getDCRedirectR :: Handler Html 
getDCRedirectR = entityTableHandler (DC'R DCRedirectR) ([] :: [Filter DC.RedirectT]) 

getDCRegistryR :: Handler Html 
getDCRegistryR = entityTableHandler (DC'R DCRegistryR) ([] :: [Filter DC.RegistryT]) 

getDCRegistryFileR :: Handler Html 
getDCRegistryFileR = entityTableHandler (DC'R DCRegistryFileR) ([] :: [Filter DC.RegistryFileT]) 

getDCRoleR :: Handler Html 
getDCRoleR = entityTableHandler (DC'R DCRoleR) ([] :: [Filter DC.RoleT]) 

getDCRolePermissionR :: Handler Html 
getDCRolePermissionR = entityTableHandler (DC'R DCRolePermissionR) ([] :: [Filter DC.RolePermissionT]) 

getDCRulesConfigR :: Handler Html 
getDCRulesConfigR = entityTableHandler (DC'R DCRulesConfigR) ([] :: [Filter DC.RulesConfigT]) 

getDCRulesDependenciesR :: Handler Html 
getDCRulesDependenciesR = entityTableHandler (DC'R DCRulesDependenciesR) ([] :: [Filter DC.RulesDependenciesT]) 

getDCRulesTagsR :: Handler Html 
getDCRulesTagsR = entityTableHandler (DC'R DCRulesTagsR) ([] :: [Filter DC.RulesTagsT]) 

getDCRulesTriggerR :: Handler Html 
getDCRulesTriggerR = entityTableHandler (DC'R DCRulesTriggerR) ([] :: [Filter DC.RulesTriggerT]) 

getDCSearchApiDbCustomProductDisplayFieldProductCommercePriR :: Handler Html 
getDCSearchApiDbCustomProductDisplayFieldProductCommercePriR = entityTableHandler (DC'R DCSearchApiDbCustomProductDisplayFieldProductCommercePriR) ([] :: [Filter DC.SearchApiDbCustomProductDisplayFieldProductCommercePriT]) 

getDCSearchApiDbCustomProductDisplaySearchApiLanguageR :: Handler Html 
getDCSearchApiDbCustomProductDisplaySearchApiLanguageR = entityTableHandler (DC'R DCSearchApiDbCustomProductDisplaySearchApiLanguageR) ([] :: [Filter DC.SearchApiDbCustomProductDisplaySearchApiLanguageT]) 

getDCSearchApiDbProductDisplayR :: Handler Html 
getDCSearchApiDbProductDisplayR = entityTableHandler (DC'R DCSearchApiDbProductDisplayR) ([] :: [Filter DC.SearchApiDbProductDisplayT]) 

getDCSearchApiDbProductDisplayCreatedR :: Handler Html 
getDCSearchApiDbProductDisplayCreatedR = entityTableHandler (DC'R DCSearchApiDbProductDisplayCreatedR) ([] :: [Filter DC.SearchApiDbProductDisplayCreatedT]) 

getDCSearchApiDbProductDisplayFieldBrandR :: Handler Html 
getDCSearchApiDbProductDisplayFieldBrandR = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldBrandR) ([] :: [Filter DC.SearchApiDbProductDisplayFieldBrandT]) 

getDCSearchApiDbProductDisplayFieldCollectionR :: Handler Html 
getDCSearchApiDbProductDisplayFieldCollectionR = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldCollectionR) ([] :: [Filter DC.SearchApiDbProductDisplayFieldCollectionT]) 

getDCSearchApiDbProductDisplayFieldProductCategoryR :: Handler Html 
getDCSearchApiDbProductDisplayFieldProductCategoryR = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldProductCategoryR) ([] :: [Filter DC.SearchApiDbProductDisplayFieldProductCategoryT]) 

getDCSearchApiDbProductDisplayFieldProductFieldColourR :: Handler Html 
getDCSearchApiDbProductDisplayFieldProductFieldColourR = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldProductFieldColourR) ([] :: [Filter DC.SearchApiDbProductDisplayFieldProductFieldColourT]) 

getDCSearchApiDbProductDisplayFieldProductFieldPricePl06R :: Handler Html 
getDCSearchApiDbProductDisplayFieldProductFieldPricePl06R = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldProductFieldPricePl06R) ([] :: [Filter DC.SearchApiDbProductDisplayFieldProductFieldPricePl06T]) 

getDCSearchApiDbProductDisplayFieldProductFieldPricePl06AR :: Handler Html 
getDCSearchApiDbProductDisplayFieldProductFieldPricePl06AR = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldProductFieldPricePl06AR) ([] :: [Filter DC.SearchApiDbProductDisplayFieldProductFieldPricePl06AT]) 

getDCSearchApiDbProductDisplayFieldProductFieldPricePl12AR :: Handler Html 
getDCSearchApiDbProductDisplayFieldProductFieldPricePl12AR = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldProductFieldPricePl12AR) ([] :: [Filter DC.SearchApiDbProductDisplayFieldProductFieldPricePl12AT]) 

getDCSearchApiDbProductDisplayFieldProductFieldStockStatusR :: Handler Html 
getDCSearchApiDbProductDisplayFieldProductFieldStockStatusR = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldProductFieldStockStatusR) ([] :: [Filter DC.SearchApiDbProductDisplayFieldProductFieldStockStatusT]) 

getDCSearchApiDbProductDisplayFieldProductFieldTrimColourR :: Handler Html 
getDCSearchApiDbProductDisplayFieldProductFieldTrimColourR = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldProductFieldTrimColourR) ([] :: [Filter DC.SearchApiDbProductDisplayFieldProductFieldTrimColourT]) 

getDCSearchApiDbProductDisplayFieldProductTitleR :: Handler Html 
getDCSearchApiDbProductDisplayFieldProductTitleR = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldProductTitleR) ([] :: [Filter DC.SearchApiDbProductDisplayFieldProductTitleT]) 

getDCSearchApiDbProductDisplayFieldProductTitle1R :: Handler Html 
getDCSearchApiDbProductDisplayFieldProductTitle1R = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldProductTitle1R) ([] :: [Filter DC.SearchApiDbProductDisplayFieldProductTitle1T]) 

getDCSearchApiDbProductDisplayFieldProductTitle2R :: Handler Html 
getDCSearchApiDbProductDisplayFieldProductTitle2R = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldProductTitle2R) ([] :: [Filter DC.SearchApiDbProductDisplayFieldProductTitle2T]) 

getDCSearchApiDbProductDisplayFieldProductTitle3R :: Handler Html 
getDCSearchApiDbProductDisplayFieldProductTitle3R = entityTableHandler (DC'R DCSearchApiDbProductDisplayFieldProductTitle3R) ([] :: [Filter DC.SearchApiDbProductDisplayFieldProductTitle3T]) 

getDCSearchApiDbProductDisplaySearchApiAggregation1R :: Handler Html 
getDCSearchApiDbProductDisplaySearchApiAggregation1R = entityTableHandler (DC'R DCSearchApiDbProductDisplaySearchApiAggregation1R) ([] :: [Filter DC.SearchApiDbProductDisplaySearchApiAggregation1T]) 

getDCSearchApiDbProductDisplaySearchApiLanguageR :: Handler Html 
getDCSearchApiDbProductDisplaySearchApiLanguageR = entityTableHandler (DC'R DCSearchApiDbProductDisplaySearchApiLanguageR) ([] :: [Filter DC.SearchApiDbProductDisplaySearchApiLanguageT]) 

getDCSearchApiDbProductDisplayStatusR :: Handler Html 
getDCSearchApiDbProductDisplayStatusR = entityTableHandler (DC'R DCSearchApiDbProductDisplayStatusR) ([] :: [Filter DC.SearchApiDbProductDisplayStatusT]) 

getDCSearchApiDbProductDisplayTextR :: Handler Html 
getDCSearchApiDbProductDisplayTextR = entityTableHandler (DC'R DCSearchApiDbProductDisplayTextR) ([] :: [Filter DC.SearchApiDbProductDisplayTextT]) 

getDCSearchApiDbProductDisplayTitleR :: Handler Html 
getDCSearchApiDbProductDisplayTitleR = entityTableHandler (DC'R DCSearchApiDbProductDisplayTitleR) ([] :: [Filter DC.SearchApiDbProductDisplayTitleT]) 

getDCSearchApiDbProductSearchFieldProductFieldColourR :: Handler Html 
getDCSearchApiDbProductSearchFieldProductFieldColourR = entityTableHandler (DC'R DCSearchApiDbProductSearchFieldProductFieldColourR) ([] :: [Filter DC.SearchApiDbProductSearchFieldProductFieldColourT]) 

getDCSearchApiDbProductSearchSearchApiLanguageR :: Handler Html 
getDCSearchApiDbProductSearchSearchApiLanguageR = entityTableHandler (DC'R DCSearchApiDbProductSearchSearchApiLanguageR) ([] :: [Filter DC.SearchApiDbProductSearchSearchApiLanguageT]) 

getDCSearchApiDbProductSearchTitleFieldR :: Handler Html 
getDCSearchApiDbProductSearchTitleFieldR = entityTableHandler (DC'R DCSearchApiDbProductSearchTitleFieldR) ([] :: [Filter DC.SearchApiDbProductSearchTitleFieldT]) 

getDCSearchApiIndexR :: Handler Html 
getDCSearchApiIndexR = entityTableHandler (DC'R DCSearchApiIndexR) ([] :: [Filter DC.SearchApiIndexT]) 

getDCSearchApiItemR :: Handler Html 
getDCSearchApiItemR = entityTableHandler (DC'R DCSearchApiItemR) ([] :: [Filter DC.SearchApiItemT]) 

getDCSearchApiServerR :: Handler Html 
getDCSearchApiServerR = entityTableHandler (DC'R DCSearchApiServerR) ([] :: [Filter DC.SearchApiServerT]) 

getDCSearchApiSortR :: Handler Html 
getDCSearchApiSortR = entityTableHandler (DC'R DCSearchApiSortR) ([] :: [Filter DC.SearchApiSortT]) 

getDCSearchApiTaskR :: Handler Html 
getDCSearchApiTaskR = entityTableHandler (DC'R DCSearchApiTaskR) ([] :: [Filter DC.SearchApiTaskT]) 

getDCSecurityReviewR :: Handler Html 
getDCSecurityReviewR = entityTableHandler (DC'R DCSecurityReviewR) ([] :: [Filter DC.SecurityReviewT]) 

getDCSemaphoreR :: Handler Html 
getDCSemaphoreR = entityTableHandler (DC'R DCSemaphoreR) ([] :: [Filter DC.SemaphoreT]) 

getDCSequencesR :: Handler Html 
getDCSequencesR = entityTableHandler (DC'R DCSequencesR) ([] :: [Filter DC.SequencesT]) 

getDCSessionsR :: Handler Html 
getDCSessionsR = entityTableHandler (DC'R DCSessionsR) ([] :: [Filter DC.SessionsT]) 

getDCSiteVerifyR :: Handler Html 
getDCSiteVerifyR = entityTableHandler (DC'R DCSiteVerifyR) ([] :: [Filter DC.SiteVerifyT]) 

getDCSystemR :: Handler Html 
getDCSystemR = entityTableHandler (DC'R DCSystemR) ([] :: [Filter DC.SystemT]) 

getDCTaxonomyIndexR :: Handler Html 
getDCTaxonomyIndexR = entityTableHandler (DC'R DCTaxonomyIndexR) ([] :: [Filter DC.TaxonomyIndexT]) 

getDCTaxonomyMenuR :: Handler Html 
getDCTaxonomyMenuR = entityTableHandler (DC'R DCTaxonomyMenuR) ([] :: [Filter DC.TaxonomyMenuT]) 

getDCTaxonomyTermDataR :: Handler Html 
getDCTaxonomyTermDataR = entityTableHandler (DC'R DCTaxonomyTermDataR) ([] :: [Filter DC.TaxonomyTermDataT]) 

getDCTaxonomyTermHierarchyR :: Handler Html 
getDCTaxonomyTermHierarchyR = entityTableHandler (DC'R DCTaxonomyTermHierarchyR) ([] :: [Filter DC.TaxonomyTermHierarchyT]) 

getDCTaxonomyToolsRoleAccessR :: Handler Html 
getDCTaxonomyToolsRoleAccessR = entityTableHandler (DC'R DCTaxonomyToolsRoleAccessR) ([] :: [Filter DC.TaxonomyToolsRoleAccessT]) 

getDCTaxonomyVocabularyR :: Handler Html 
getDCTaxonomyVocabularyR = entityTableHandler (DC'R DCTaxonomyVocabularyR) ([] :: [Filter DC.TaxonomyVocabularyT]) 

getDCUrlAliasR :: Handler Html 
getDCUrlAliasR = entityTableHandler (DC'R DCUrlAliasR) ([] :: [Filter DC.UrlAliasT]) 

getDCUsersR :: Handler Html 
getDCUsersR = entityTableHandler (DC'R DCUsersR) ([] :: [Filter DC.UsersT]) 

getDCUsersRolesR :: Handler Html 
getDCUsersRolesR = entityTableHandler (DC'R DCUsersRolesR) ([] :: [Filter DC.UsersRolesT]) 

getDCVariableR :: Handler Html 
getDCVariableR = entityTableHandler (DC'R DCVariableR) ([] :: [Filter DC.VariableT]) 

getDCViewsDisplayR :: Handler Html 
getDCViewsDisplayR = entityTableHandler (DC'R DCViewsDisplayR) ([] :: [Filter DC.ViewsDisplayT]) 

getDCViewsViewR :: Handler Html 
getDCViewsViewR = entityTableHandler (DC'R DCViewsViewR) ([] :: [Filter DC.ViewsViewT]) 

getDCWatchdogR :: Handler Html 
getDCWatchdogR = entityTableHandler (DC'R DCWatchdogR) ([] :: [Filter DC.WatchdogT]) 

getDCXmlsitemapR :: Handler Html 
getDCXmlsitemapR = entityTableHandler (DC'R DCXmlsitemapR) ([] :: [Filter DC.XmlsitemapT]) 

getDCXmlsitemapSitemapR :: Handler Html 
getDCXmlsitemapSitemapR = entityTableHandler (DC'R DCXmlsitemapSitemapR) ([] :: [Filter DC.XmlsitemapSitemapT]) 

