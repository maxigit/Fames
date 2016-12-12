{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes#-}
module Handler.WHStocktakeSpec where

import TestImport
import Text.Shakespeare.Text (st)
import ModelField
import Yesod.Auth (requireAuthId)
import Database.Persist.Sql (toSqlKey)

spec :: Spec
spec = appSpec

relPath path = "test/Handler/WHStocktakeSpec/" ++ path

uploadSTSheet route status path overrideM = do

  logAsAdmin 
  if isJust overrideM
     then  -- we need the override button to be present
       get (WarehouseR WHStocktakeSaveR)
     else 
       get (WarehouseR WHStocktakeValidateR)
  statusIs 200

  runDB $ do
    insertUnique $ Operator "John" "Smith" "Jack" True
  request $ do
    setMethod "POST"
    setUrl (WarehouseR route)
    addToken_ "form#upload-form"
    fileByLabel "upload" path "text/plain"
    byLabel "encoding" (tshow 1)
    case overrideM of
      Nothing -> return ()
      Just False -> addPostParam "f4" "no"
      Just True -> addPostParam "f4" "yes"
    -- forM_ overrideM  (\override ->  byLabel "override" "")

  statusIs status

-- write the sheet to a temporary file and upload it.
-- more convenient for testing
postSTSheet status sheet = do
  path <- saveToTempFile sheet
  uploadSTSheet WHStocktakeValidateR status path Nothing

saveSTSheet status sheet = do
  path <- saveToTempFile sheet
  uploadSTSheet WHStocktakeSaveR status path (Just False)

updateSTSheet status sheet = do
  path <- saveToTempFile sheet
  uploadSTSheet WHStocktakeSaveR status path (Just True)

findBarcodes getBarcode = do
  entities <- runDB $ selectList [] []
  return $ map (stocktakeBarcode . entityVal) entities

appSpec = withAppWipe BypassAuth $ do
  describe "upload stocktake" $ do
    describe "upload page" $ do
      it "proposes to upload a file" $ do
        logAsAdmin 
        get (WarehouseR WHStocktakeValidateR)
        statusIs 200

        bodyContains "upload"

    describe "upload" $ do
      it "parses correctly a correct file" $ do
        logAsAdmin
        postSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
|]

      it "saves stocktakes" $ do
        saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
|]
        barcodes <- findBarcodes stocktakeBarcode
        let types = barcodes :: [Text]
        liftIO $ barcodes `shouldBe`  ["ST16NV00399X"]

      it "saves boxtakes" $ do
        saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
|]
        barcodes <- findBarcodes boxtakeBarcode
        let types = barcodes :: [Text]
        liftIO $ barcodes `shouldBe`  ["ST16NV00399X"]

      it "doesn't save twice the same file" $ do
        saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
|]
        saveSTSheet 422 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
|]
        bodyContains "Document has already been uploaded"
      
--       it "@current saves twice" $ do
--         saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
-- t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
-- |]
--         saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
-- t-shirt,black,120,shelf-2,ST16NV00399X,34,20,17,2016/11/10,Jack
-- |]
--         barcodes <- findBarcodes boxtakeBarcode
--         let types = barcodes :: [Text]
--         liftIO $ barcodes `shouldBe`  ["ST16NV00399X"]

    describe "process blanks" $ do
      it "fills barcode prefix" $ do
        postSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
t-shirt,red,120,shelf-1,400E,34,20,17,2016/11/10,Jack
|]
        mapM (htmlAnyContain "table td.stocktakeBarcode")
             ["ST16NV00399X","ST16NV00400E"]


      it "highlights guessed valued" $ do
        postSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
t-shirt,red,120,shelf-1,400E,34,20,17,2016/11/10,
|]
        htmlAnyContain "table td.stocktakeOperator span.guessed-value" "Jack"
      
          
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
t-shirt,red,120,shelf-1,-,,,,,Jack
|]
        htmlAllContain "table td.stocktakeBarcode" "ST16NV00399X"
        htmlCount "table td.stocktakeBarcode" 2


    describe "overriding existing stocktake" $ do
      it "Creates a new stocktake item if needed" $ do
        updateSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
|]
        stockTake <- runDB $ getBy (UniqueSB "ST16NV00399X" 1)
        liftIO $ ((,) <$> stocktakeStockId <*> stocktakeQuantity) . entityVal <$> stockTake `shouldBe` Just ("t-shirt-black", 120)

      it "Updates the stocktake value" $ do
        saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
|]
        stockTakeOrig <- runDB $ getBy (UniqueSB "ST16NV00399X" 1)
        liftIO $ ((,) <$> stocktakeStockId <*> stocktakeQuantity) . entityVal <$> stockTakeOrig
          `shouldBe` Just ("t-shirt-black", 120)
        updateSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,red,17,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
|]
        stockTake <- runDB $ getBy (UniqueSB "ST16NV00399X" 1)
        liftIO $ ((,) <$> stocktakeStockId <*> stocktakeQuantity) . entityVal <$> stockTake `shouldBe` Just ("t-shirt-red", 17)
              
      it "doesn't update the adjustment key" $ do
        saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
|]
        Just (Entity oldKey old) <- runDB $ getBy (UniqueSB "ST16NV00399X" 1)
        -- create document
        now <- liftIO getCurrentTime
        let adj = StockAdjustment "test" now Pending (toSqlKey 1)
        adjId <- runDB $ insert adj
  
        runDB $ update oldKey [StocktakeAdjustment =. Just adjId]
         
        -- update
        updateSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,red,17,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
|]
        Just (Entity _ new) <- runDB $ getBy (UniqueSB "ST16NV00399X" 1)
        liftIO $ (stocktakeAdjustment new) `shouldBe` Just adjId
  
    describe "validation" $ do
      it "detects incorrect barcode on the first line" $ do
          postSTSheet 422 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-  shirt,black,120,shelf-1,ST16NV00399Z,34,20,17,2016/11/10,Jack
|]  
          htmlAllContain "table td.stocktakeBarcode .parsing-error" "ST16NV00399Z"


      it "detects incorrect barcode" $ do
          postSTSheet 422 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
t-shirt,red,120,shelf-1,ST16NV00400X,34,20,17,2016/11/10,Jack
|]  
          htmlAllContain "table td.stocktakeBarcode .parsing-error" "ST16NV00400X"



      it "detects incorrect partial barcode" $ do
          postSTSheet 422 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-  shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
t-shirt,red,120,shelf-1,400X,34,20,17,2016/11/10,Jack
|]  
          htmlAllContain "table td.stocktakeBarcode .parsing-error" "400X"



      it "detects incorrect barcode sequence" $ do
          postSTSheet 422 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-  shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
t-  shirt,red,120,shelf-1,,34,20,17,2016/11/10,Jack
t-  shirt,red,120,shelf-1,00400E,34,20,17,2016/11/10,Jack
|]  
          htmlAnyContain "table td.stocktakeBarcode. .parsing-error" "ST16NV00399X"

      it "check barcode sequence ends" $ do
        postSTSheet 422 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
t-shirt,red,120,shelf-1,,34,20,17,2016/11/10,
|]


          
      it "checks same barcode not twice in a row" $ do
        postSTSheet 422 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
t-shirt,red,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
|]


          
      it "checks everything is the same for group" $ do
          postSTSheet 422 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
t-shirt,black,120,shelf-2,-,34,20,17,2016/11/10,Jack
|]  
          htmlAnyContain "table td.stocktakeLocation. .parsing-error" "shelf-2"

      it "expands shelves" $ do
          postSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,black,120,shelf-[12],ST16NV00399X,34,20,17,2016/11/10,Jack
|]  
          htmlAnyContain "table td.stocktakeLocation" "shelf-1|shelf-2"

      it "checks shelves belongs to the same location " $ do
          postSTSheet 422 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
t-shirt,black,120,shelf-[123],ST16NV00399X,34,20,17,2016/11/10,Jack
|]  
          htmlAnyContain "table td.stocktakeLocation " "find location with name"

