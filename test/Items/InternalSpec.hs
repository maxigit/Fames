module Items.InternalSpec (spec) where

import TestImport
import Items.Internal
import Items.Types
import Test.QuickCheck(property, (===), (==>))
import qualified Data.IntMap as IM


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

fullWhite= [[ItemInfo {iiStyle = "Bob"
                     , iiVariation = "White"
                     , iiInfo = ItemMasterAndPrices {impMaster = Just iMaster
                                                    , impSalesPrices = Nothing
                                                    , impPurchasePrices = Nothing
                                                    , impFAStatus = Nothing
                                                    }}]
          
          ,[ItemInfo {iiStyle = "Bob"
                     , iiVariation = "White"
                     , iiInfo = ItemMasterAndPrices {impMaster = Nothing
                                                    , impSalesPrices = Just iPrices
                                                    , impPurchasePrices = Nothing
                                                    , impFAStatus = Nothing
                                                    }}]
          
          
          ,[ItemInfo {iiStyle = "Bob"
                     , iiVariation = "White"
                     , iiInfo = ItemMasterAndPrices {impMaster = Nothing
                                                    , impSalesPrices = Nothing
                                                    , impPurchasePrices = Nothing
                                                    , impFAStatus = Just iFA
                                                    }}]
          
          ,[ItemInfo {iiStyle = "Bob"
                     , iiVariation = "White"
                     , iiInfo = ItemMasterAndPrices {impMaster = Nothing
                                                    , impSalesPrices = Nothing
                                                    , impPurchasePrices = Nothing
                                                    , impFAStatus = Nothing
                                                    }}]
          ,[]]
  
spec :: Spec
spec = bug >> forecast
bug = describe "bug" $ do
  it "merges info" $ do
    mergeInfoSources fullWhite `shouldBe` [ItemInfo {iiStyle = "Bob"
                                            , iiVariation ="White"
                                            , iiInfo = ItemMasterAndPrices {impMaster = Just iMaster
                                                                           , impSalesPrices = Just iPrices
                                                                           , impPurchasePrices = Nothing
                                                                           , impFAStatus = Just iFA
                                                                           }   }]
  it "computes diff" $ do
    let bob = ItemInfo {iiStyle = "Bob"
                       , iiVariation ="White"
                       , iiInfo = ItemMasterAndPrices {impMaster = Just iMaster
                                                      , impSalesPrices = Just iPrices
                                                      , impPurchasePrices = Nothing
                                                      , impFAStatus = Just iFA
                                                                           }   }
    let bobF = ItemInfo {iiStyle = "Bob"
                       , iiVariation ="White"
                       , iiInfo = ItemMasterAndPrices {
                           impMaster = Just (StockMasterF {smfCategoryId = ([] ,23)
                                                          , smfTaxTypeId = ([] ,1)
                                                          , smfDescription = ([] ,"Bob. White")
                                                          , smfLongDescription = ([] ,"")
                                                          , smfUnits = ([] ,"ea.")
                                                          , smfMbFlag = ([] ,"B")
                                                          , smfSalesAccount = ([] ,"4000")
                                                          , smfCogsAccount = ([] ,"5001")
                                                          , smfInventoryAccount = ([] ,"1001")
                                                          , smfAdjustmentAccount = ([] ,"8610")
                                                          , smfAssemblyAccount = ([] ,"1001")
                                                          , smfDimensionId = ([] ,Just 17)
                                                          , smfDimension2Id = ([] ,Just 0)
                                                          , smfActualCost = ([] ,0.0)
                                                          , smfLastCost = ([] ,0.0)
                                                          , smfMaterialCost = ([] ,0.0)
                                                          , smfLabourCost = ([] ,0.0)
                                                          , smfOverheadCost = ([] ,0.0)
                                                          , smfInactive = ([] ,True)
                                                          , smfNoSale = ([] ,False)
                                                          , smfEditable = ([] ,False)})
                           , impSalesPrices = Just (IM.fromList [(11 ,PriceF { pfStockId = ([] ,"Bob-White")
                                                                          , pfSalesTypeId = ([] ,11)
                                                                          , pfCurrAbrev = ([] ,"GBP")
                                                                          , pfPrice = ([] ,8.0)})
                                                             ,(13 ,PriceF {pfStockId = ([] ,"Bob-White")
                                                                          , pfSalesTypeId = ([] ,13)
                                                                          , pfCurrAbrev = ([] ,"GBP")
                                                                          , pfPrice = ([] ,8.0)})
                                                             ,(15 ,PriceF {pfStockId = ([] ,"Bob-White")
                                                                          , pfSalesTypeId = ([] ,15)
                                                                          , pfCurrAbrev = ([] ,"GBP")
                                                                          , pfPrice = ([] ,8.0)})
                                                             ,(16 ,PriceF {pfStockId = ([] ,"Bob-White")
                                                                          , pfSalesTypeId = ([] ,16)
                                                                          , pfCurrAbrev = ([] ,"GBP")
                                                                          , pfPrice = ([] ,8.0)})])
                           , impPurchasePrices = Just (IM.fromList [])
                           , impFAStatus = Just (ItemStatusF {isfQoh = ([] ,0)
                                                             , isfAllQoh = ([] ,0)
                                                             , isfOnDemand = ([] ,0)
                                                             , isfAllOnDemand = ([] ,0)
                                                             , isfOnOrder = ([] ,0)
                                                             , isfUsed = ([] ,False)})
                           }}

    computeDiff bob bob `shouldBe`  bobF
    
shouldBeAlmost a b = (abs (a-b) <1e-6) `shouldBe` True
forecast :: Spec
forecast = describe "Forecast" $ do
  let profile = seasonProfile [1,1,1,1,2,3,0,0,0,1,0,0]
      --                       J F M A M J J A S O N D
  context "#SeasonProfile" $ do
    it "sums to one" $ property $ 
      \xs -> let SeasonProfile p = seasonProfile (xs :: [Double])
             in not (null xs) && (abs (sum xs) > 1e-2)  ==> abs (sum p - 1) < 1e-2
    it "has all months " $ property $
      \xs -> let SeasonProfile p = seasonProfile xs
             in length p === 12

  context "#weightForRange" $ do
    it "matches full month" $ do
      weightForRange profile (fromGregorian 2017 01 01) (fromGregorian 2017 01 31) `shouldBeAlmost` 0.1
      weightForRange profile (fromGregorian 2017 05 01) (fromGregorian 2017 05 31) `shouldBeAlmost` 0.2
      weightForRange profile (fromGregorian 2017 12 01) (fromGregorian 2017 12 31) `shouldBeAlmost` 0
    it "matches partial month" $ do
      weightForRange profile (fromGregorian 2017 01 01) (fromGregorian 2017 01 16) `shouldBeAlmost` (16/31*0.1)
    it "matches across 1  month" $ do
      weightForRange profile (fromGregorian 2017 01 01) (fromGregorian 2017 02 28) `shouldBeAlmost` 0.2
      weightForRange profile (fromGregorian 2017 05 01) (fromGregorian 2017 06 30) `shouldBeAlmost` 0.5
      weightForRange profile (fromGregorian 2017 12 01) (fromGregorian 2017 12 31) `shouldBeAlmost` 0
      weightForRange profile (fromGregorian 2017 01 16) (fromGregorian 2017 02 14) `shouldBeAlmost`  ((16/31+14/28) *0.1)
    it "matches across  years" $ do
      weightForRange profile (fromGregorian 2017 06 01) (fromGregorian 2018 01 16) `shouldBeAlmost` (0.3 + 0.1 + 0.1*(16/31))
      weightForRange profile (fromGregorian 2017 06 01) (fromGregorian 2019 05 31) `shouldBeAlmost` 2


  

