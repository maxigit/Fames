{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
module Items.CategorySpec (spec) where

import TestImport
import Items.Internal
import Items.Types
import qualified FA as FA 
import qualified Data.Map as Map 
import CategoryRule
import Handler.Util
import Data.Yaml(decodeEither)
import Text.Shakespeare.Text (sbt)

defStock = FA.StockMaster{..} where
  stockMasterCategoryId =  1
  stockMasterTaxTypeId =  2
  stockMasterDescription =  "Fake product"
  stockMasterLongDescription =  "Long descriptio"
  stockMasterUnits =  "ea"
  stockMasterMbFlag =  "B"
  stockMasterSalesAccount =  "4000"
  stockMasterCogsAccount =  "5000"
  stockMasterInventoryAccount =  "2001"
  stockMasterAdjustmentAccount =  "8000"
  stockMasterAssemblyAccount =  "7000"
  stockMasterDimensionId =  Just 2
  stockMasterDimension2Id =  Nothing
  stockMasterActualCost = 3.4
  stockMasterLastCost =  5
  stockMasterMaterialCost =  2
  stockMasterLabourCost =  1
  stockMasterOverheadCost =  2.3
  stockMasterInactive =  False
  stockMasterNoSale =  False
  stockMasterEditable = True

applyJSONRules stock json = 
  case decodeEither $ encodeUtf8 json of
    Right rulesMap ->  let
      categories = categoriesFor <$> Map.toList rulesMap <*> [stock]
      in sort [(itemCategoryCategory, itemCategoryValue) | ItemCategory{..} <- catMaybes categories]
    Left err -> error (show err)
  
spec = describe "@Category StockMaster to category" $ do
  describe "#fromJSON" $ it "" pending
  describe "#toJSON" $ it "" pending
  context "applying rules" $ do
    let stock = Entity (FA.StockMasterKey "T-Shirt Blue") defStock
        rules = [sbt|
                    |style:                   
                    |  - "(.*) (.*)": "\\1"
                    |colour:                   
                    |  - "(.*) (.*)": "\\2"
                    |]
    it "finds all category" $ do
       applyJSONRules stock rules `shouldBe` sort [("style", "T-Shirt"), ("colour", "Blue"), ("PriceBand", "A")]
    it "finds composed  category" $ do
       let rules = [sbt|
                       |style:                   
                       |  - "(.*) (.*)": "\\1"
                       |colour:                   
                       |    source: sku
                       |    rules:
                       |    - "(.*) (.*)": "\\2"
                       |"colour-style":
                       |  source: "($colour) {$style}"
                       |]
       applyJSONRules stock rules `shouldBe` sort [("style", "T-Shirt"), ("colour", "Blue"), ("colour-style", "(Blue) {T-shirt}")]
