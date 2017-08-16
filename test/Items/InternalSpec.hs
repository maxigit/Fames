module Items.InternalSpec (spec) where

import TestImport
import Items.Internal
import Items.Types

iMaster = (StockMasterF {smfCategoryId = Identity 23
                          , smfTaxTypeId = Identity 1
                          , smfDescription = Identity "Bob. White"
                          , smfLongDescription = Identity ""
                          , smfUnits = Identity "ea."
                          , smfMbFlag = Identity "B"
                          , smfSalesAccount = Identity "4000"
                          , smfCogsAccount = Identity "5001"
                          , smfInventoryAccount = Identity "1001"
                          , smfAdjustmentAccount = Identity "8610"
                          , smfAssemblyAccount = Identity "1001"
                          , smfDimensionId = Identity (Just 17)
                          , smfDimension2Id = Identity (Just 0)
                          , smfActualCost = Identity 0.0
                          , smfLastCost = Identity 0.0
                          , smfMaterialCost = Identity 0.0
                          , smfLabourCost = Identity 0.0
                          , smfOverheadCost = Identity 0.0
                          , smfInactive = Identity True
                          , smfNoSale = Identity False
                          , smfEditable = Identity False})

iPrices = (mapFromList [(11
                     ,PriceF {pfStockId = Identity "Bob-White"
                             , pfSalesTypeId = Identity 11
                             , pfCurrAbrev = Identity "GBP"
                             , pfPrice = Identity 8.0})
                    ,(13
                     ,PriceF {pfStockId = Identity "Bob-White"
                             , pfSalesTypeId = Identity 13
                             , pfCurrAbrev = Identity "GBP"
                             , pfPrice = Identity 8.0})
                    ,(15
                     ,PriceF {pfStockId = Identity "Bob-White"
                             , pfSalesTypeId = Identity 15
                             , pfCurrAbrev = Identity "GBP"
                             , pfPrice = Identity 8.0})
                    ,(16
                     ,PriceF {pfStockId = Identity "Bob-White"
                             , pfSalesTypeId = Identity 16
                             , pfCurrAbrev = Identity "GBP"
                             , pfPrice = Identity 8.0})])

iFA = ItemStatusF {isfQoh = Identity 0
                   , isfAllQoh = Identity 0
                   , isfOnDemand = Identity 0
                   , isfAllOnDemand = Identity 0
                   , isfOnOrder = Identity 0
                   , isfUsed = Identity False}
      
iWebStatus = ItemWebStatusF {iwfProductDisplay = Identity Nothing
                            , iwfActive = Identity True}

fullWhite= [[ItemInfo {iiStyle = "Bob"
                     , iiVariation = "White"
                     , iiInfo = ItemMasterAndPrices {impMaster = Just iMaster
                                                    , impSalesPrices = Nothing
                                                    , impPurchasePrices = Nothing
                                                    , impFAStatus = Nothing
                                                    , impWebStatus = Nothing
                                                    , impWebPrices = Nothing}}]
          
          ,[ItemInfo {iiStyle = "Bob"
                     , iiVariation = "White"
                     , iiInfo = ItemMasterAndPrices {impMaster = Nothing
                                                    , impSalesPrices = Just iPrices
                                                    , impPurchasePrices = Nothing
                                                    , impFAStatus = Nothing
                                                    , impWebStatus = Nothing
                                                    , impWebPrices = Nothing}}]
          
          
          ,[ItemInfo {iiStyle = "Bob"
                     , iiVariation = "White"
                     , iiInfo = ItemMasterAndPrices {impMaster = Nothing
                                                    , impSalesPrices = Nothing
                                                    , impPurchasePrices = Nothing
                                                    , impFAStatus = Just iFA
                                                    , impWebStatus = Nothing
                                                    , impWebPrices = Nothing}}]
          
          ,[ItemInfo {iiStyle = "Bob"
                     , iiVariation = "White"
                     , iiInfo = ItemMasterAndPrices {impMaster = Nothing
                                                    , impSalesPrices = Nothing
                                                    , impPurchasePrices = Nothing
                                                    , impFAStatus = Nothing
                                                    , impWebStatus = Just iWebStatus
                                                    , impWebPrices = Nothing}}]
          ,[]]
  
spec :: Spec
spec = describe "bug" $ do
  it "merges info" $ do
    mergeInfoSources fullWhite `shouldBe` [ItemInfo {iiStyle = "Bob"
                                            , iiVariation ="White"
                                            , iiInfo = ItemMasterAndPrices {impMaster = Just iMaster
                                                                           , impSalesPrices = Just iPrices
                                                                           , impPurchasePrices = Nothing
                                                                           , impFAStatus = Just iFA
                                                                           , impWebStatus = Just iWebStatus
                                                                           , impWebPrices = Nothing}   }]
  it "computes diff" $ do
    let bob = ItemInfo {iiStyle = "Bob"
                       , iiVariation ="White"
                       , iiInfo = ItemMasterAndPrices {impMaster = Just iMaster
                                                      , impSalesPrices = Just iPrices
                                                      , impPurchasePrices = Nothing
                                                      , impFAStatus = Just iFA
                                                                           , impWebStatus = Just iWebStatus
                                                                           , impWebPrices = Nothing}   }
    let bobF = ItemInfo {iiStyle = "Bob"
                       , iiVariation ="White"
                       , iiInfo = ItemMasterAndPrices {impMaster = Nothing -- Just iMaster
                                                      , impSalesPrices = Nothing -- Just iPrices
                                                      , impPurchasePrices = Nothing
                                                      , impFAStatus = Nothing -- Just iFA
                                                      , impWebStatus = Nothing -- Just iWebStatus
                                                      , impWebPrices = Nothing}   }
    computeDiff bob bob `shouldBe`  bobF
    

