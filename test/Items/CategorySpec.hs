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
import qualified Data.Map as Map

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
      (FA.StockMasterKey sku) = entityKey stock
      resultMap = computeCategories (join $ map Map.toList rulesMap) mempty (unpack sku)
      in traceShow ("RULES", rulesMap) $ Map.toList resultMap
    Left err -> error (show err)

spec = describe "@Category StockMaster to category" $ do
  describe "#fromJSON" $ it "" pending
  describe "#toJSON" $ it "" pending
  describe "expand source" $ do
    it "leaves everything if nothing to replace" $ do
      expandSource (Map.fromList [("A", "3")]) "A" `shouldBe` "A"
    it "replace category between <>" $ do
      expandSource (Map.fromList [("A", "3")]) "$A" `shouldBe` "3"
  context "applying rules" $ do
    let stock = Entity (FA.StockMasterKey "T-Shirt Blue") defStock
    it "finds all category" $ do
       let rules = [sbt|
                    |- style:
                    |  - "\\1": "(.*) (.*)"
                    |  colour:
                    |  - "\\2": "(.*) (.*)"
                    |]
       applyJSONRules stock rules `shouldBe` sort [("style", "T-Shirt"), ("colour", "Blue")]
    it "check matches in order" $ do
       let rules = [sbt|
                    |- style:
                    |  - "Dress": "Drss (.*)"
                    |  - "\\1": "(.*) (.*)"
                    |  colour:
                    |  - "Bluuue": "(.*) Blue"
                    |  - "\\2": "(.*) (.*)"
                    |]
       applyJSONRules stock rules `shouldBe` sort [("style", "T-Shirt"), ("colour", "Bluuue")]
    it "finds all prices" $ do
       let rules = [sbt|
                    |- PriceBand:
                    |  - A:
                    |    - source: sales_price
                    |      from: 5
                    |]
       applyJSONRules stock rules `shouldBe` sort [("PriceBand", "A")]
    it "finds composed  category" $ do
       let rules = [sbt|
                       |- style:
                       |  - "\\1": "(.*) (.*)"
                       |  colour:
                       |  - "\\2":
                       |    - source: sku
                       |      match: "(.*) (.*)"
                       |- "colour-style":
                       |    source: "($colour) {$style}"
                       |    rules:
                       |    - "\\0": ".*"
                       |]

       applyJSONRules stock rules `shouldBe` sort [("style", "T-Shirt"), ("colour", "Blue"), ("colour-style", "(Blue) {T-Shirt}")]