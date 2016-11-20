{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes#-}
module Handler.WHStocktakeSpec where

import TestImport
import Text.Shakespeare.Text (st)

spec :: Spec
spec = appSpec

relPath path = "test/Handler/WHStocktakeSpec/" ++ path

uploadSTSheet route status path = do
  get (WarehouseR WHStocktakeR)
  statusIs 200

  request $ do
    setMethod "POST"
    setUrl (WarehouseR route)
    addToken_ "form#upload-form"
    fileByLabel "upload" path "text/plain"
    byLabel "encoding" (tshow $ 1)

  -- printBody
  statusIs status

-- write the sheet to a temporary file and upload it.
-- more convenient for testing
postSTSheet status sheet = do
  path <- saveToTempFile sheet
  uploadSTSheet WHStocktakeR status path

saveSTSheet status sheet = do
  path <- saveToTempFile sheet
  uploadSTSheet WHStocktakeSaveR status path

findBarcodes = do
  entities <- runDB $ selectList [] []
  return $ map (stocktakeBarcode . entityVal) entities

appSpec = withAppNoDB BypassAuth $ do
  describe "upload stocktake" $ do
    describe "upload page" $ do
      it "propose to upload a file" $ do
        get (WarehouseR WHStocktakeR)
        statusIs 200

        bodyContains "upload"

    describe "upload" $ do
      it "parses correctly a correct file" $ do
        postSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
|]
      it "saves" $ do
        saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
|]
        barcodes <- findBarcodes
        let types = barcodes :: [Text]
        liftIO $ barcodes `shouldBe`  ["ST16NV00399X"]

    describe "process blanks" $ do
      it "fills barcode prefix" $ do
        postSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
t-shirt,red,120,shelf-1,400E,34,20,17,2016/11/10,Jack
|]
        mapM (htmlAnyContain "table td.stocktakeBarcode")
             ["ST16NV00399X","ST16NV00400E"]

          
      it "fills barcode sequence" $ do
        postSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
t-shirt,red,120,shelf-1,,34,20,17,2016/11/10,Jack
t-shirt,red,120,shelf-1,00401L,34,20,17,2016/11/10,Jack
|]
        mapM (htmlAnyContain "table td.stocktakeBarcode")
             ["ST16NV00399X","ST16NV00400E","ST16NV00401L"]

      it "fills everything else" $ do 
        postSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
,red,120,shelf-1,400E,,,,,
|]
        htmlAllContain "table td.stocktakeStyle" "t-shirt"
        htmlCount "table td.stocktakeStyle" 2

        htmlAllContain "table td.stocktakeOperator" "Jack"
        htmlCount "table td.stocktakeOperator" 2

      it "groups mixed colours with the same barcode" $ do
        postSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
,red,120,-,,,,,
|]
        htmlAllContain "table td.stocktakeBarcode" "ST16NV00399X"
        htmlCount "table td.stocktakeBarcode" 2

    describe "validation" $ do
      it "detects incorrect barcodes" $ do
          postSTSheet 422 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-  shirt,black,120,shelf-1,ST16NV00399Z,34,20,17,2016/11/10,Jack
,r  ed,120,-,,,,,
|]  
          htmlAllContain "table td.stocktakeBarcode.error" "ST16NV00399Z"

      it "detects incorrect barcode sequence" $ do
          postSTSheet 422 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-  shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
t-  shirt,red,120,shelf-1,,34,20,17,2016/11/10,Jack
t-  shirt,red,120,shelf-1,00400E,34,20,17,2016/11/10,Jack
|]  
          htmlAllContain "table td.stocktakeBarcode.error" "ST16NV00399X"

      it "check everything is the same for group" $ do
          postSTSheet 422 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-  shirt,black,120,shelf-1,ST16NV00399Z,34,20,17,2016/11/10,Jack
,r  ed,120,shelf-2,-,,,,,
|]  
          htmlAllContain "table td.stocktakeLocation.error" "shelf-2"

