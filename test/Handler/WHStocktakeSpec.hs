{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes#-}
module Handler.WHStocktakeSpec where

import TestImport
import Text.Shakespeare.Text (st)
import ModelField
import Yesod.Auth (requireAuthId)
import Database.Persist.Sql (toSqlKey)
import qualified Data.List as List

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
    byLabel "encoding" (tshow 1)
    byLabel "stylecomplete" (tshow 1)
    byLabel "displaymode" (tshow 1)
    case overrideM of
      Nothing -> do
        addToken_ "form#upload-form"
        fileByLabel "upload" path "text/plain"
        addPostParam "f6" "no"
        
      Just over -> do
        sheet <- liftIO $ readFile path
        let key = computeDocumentKey  sheet
        liftIO  $ writeFile ("/tmp/" <> unpack key) sheet
        addToken_ "form#upload-form"
        addPostParam "f1" ("Just "<> key)
        addPostParam "f2" (tshow $ Just (path))
        addPostParam "f7" $ if over then "yes" else "no"

  statusIs status

boxtakeLengthShouldBe expected = do
  boxtakes <- runDB (selectList [] [])
  liftIO $ length (boxtakes :: [Entity Boxtake]) `shouldBe` expected

stocktakeLengthShouldBe expected = do
  stocktakes <- runDB (selectList [] [])
  liftIO $ length (stocktakes :: [Entity Stocktake]) `shouldBe` expected

lengthShouldBe filter expected = do
  l <- runDB $ count filter
  liftIO $ l `shouldBe` expected

boxtakeStatusShouldBe barcode'statusz = do
  let (barcodes, status) = List.unzip barcode'statusz

  realStatusz <- forM barcodes $ \barcode ->  do
    res <- runDB (selectList [BoxtakeBarcode ==. barcode] [])
    return $ (barcode, case res of
      [Entity _ box ] -> Just (boxtakeActive box)
      _ -> Nothing
      )

  liftIO $ realStatusz `shouldBe` zip barcodes (map Just status)

      
stocktakeStatusShouldBe barcode'statusz = do
  let (barcodes, status) = List.unzip barcode'statusz

  realStatusz <- forM barcodes $ \barcode ->  do
    res <- runDB (selectList [StocktakeBarcode ==. barcode] [])
    return $ (barcode, case res of
      [Entity _ stocktake ] -> Just (stocktakeActive stocktake)
      _ -> Nothing
      )

  liftIO $ realStatusz `shouldBe` zip barcodes (map Just status)

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
  -- uploadSTSheet WHStocktakeSaveR status path (Just False)

findBarcodes getBarcode = do
  entities <- runDB $ selectList [] []
  return $ map (stocktakeBarcode . entityVal) entities

appSpec = withAppWipe BypassAuth $ do
  describe "@Stocktake upload stocktake" $ do
    describe "upload page" $ do
      it "proposes to upload a file" $ do
        logAsAdmin 
        get (WarehouseR WHStocktakeValidateR)
        statusIs 200

        bodyContains "upload"

    describe "upload" $ do
      it "parses correctly a correct file" $ do
        logAsAdmin
        postSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
|]

      it "saves stocktakes" $ do
        saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
|]
        barcodes <- findBarcodes stocktakeBarcode
        let types = barcodes :: [Text]
        liftIO $ barcodes `shouldBe`  ["ST16NV00399X"]

      it "saves boxtakes" $ do
        saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
|]
        barcodes <- findBarcodes boxtakeBarcode
        let types = barcodes :: [Text]
        liftIO $ barcodes `shouldBe`  ["ST16NV00399X"]

      it "doesn't save twice the same file" $ do
        saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
|]
        saveSTSheet 422 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
|]
        bodyContains "Document has already been uploaded"
      
--       it "saves twice" $ do
--         saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
-- t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
-- |]
--         saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
-- t-shirt,black,120,shelf-2,ST16NV00399X,34,20,17,2016/11/10,Jack,
-- |]
--         barcodes <- findBarcodes boxtakeBarcode
--         let types = barcodes :: [Text]
--         liftIO $ barcodes `shouldBe`  ["ST16NV00399X"]

    describe "process blanks" $ do
      it "fills barcode prefix" $ do
        postSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
t-shirt,red,120,shelf-1,400E,34,20,17,2016/11/10,Jack,
|]
        mapM (htmlAnyContain "table td.stocktakeBarcode")
             ["ST16NV00399X","ST16NV00400E"]


      it "highlights guessed valued" $ do
        postSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
t-shirt,red,120,shelf-1,400E,34,20,17,2016/11/10,,
|]
        htmlAnyContain "table td.stocktakeOperator span.guessed-value" "Jack"
      
          
      it "fills barcode sequence" $ do
        postSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
t-shirt,red,120,shelf-1,,34,20,17,2016/11/10,Jack,
t-shirt,red,120,shelf-1,00401L,34,20,17,2016/11/10,Jack,
|]
        mapM (htmlAnyContain "table td.stocktakeBarcode")
             ["ST16NV00399X","ST16NV00400E","ST16NV00401L"]


      it "fills everything else" $ do 
        postSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
,red,120,shelf-1,400E,,,,,,
|]
        htmlAllContain "table td.stocktakeStyle" "t-shirt"
        htmlCount "table td.stocktakeStyle" 2

        htmlAllContain "table td.stocktakeOperator" "Jack"
        htmlCount "table td.stocktakeOperator" 2



      it "groups mixed colours with the same barcode" $ do
        postSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
t-shirt,red,120,shelf-1,-,,,,,Jack,
|]
        htmlAllContain "table td.stocktakeBarcode" "ST16NV00399X"
        htmlCount "table td.stocktakeBarcode" 2

      it "skips blank lines" $ do
        (const pending)
--         postSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator
-- t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack
-- ,,,,,,,,,,
-- t-shirt,red,120,shelf-1,400E,,,,,Jack
-- |]
--         htmlCount "table. td.stocktakeBarcode" 2

    describe "overriding existing stocktake" $ do
      it "Creates a new stocktake item if needed" $ do
        updateSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
|]
        stockTake <- runDB $ getBy (UniqueSB "ST16NV00399X" 1)
        liftIO $ ((,) <$> stocktakeStockId <*> stocktakeQuantity) . entityVal <$> stockTake `shouldBe` Just ("t-shirt-black", 120)

      it "Updates the stocktake value" $ do
        saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
|]
        stockTakeOrig <- runDB $ getBy (UniqueSB "ST16NV00399X" 1)
        liftIO $ ((,) <$> stocktakeStockId <*> stocktakeQuantity) . entityVal <$> stockTakeOrig
          `shouldBe` Just ("t-shirt-black", 120)
        updateSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,red,17,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
|]
        stockTake <- runDB $ getBy (UniqueSB "ST16NV00399X" 1)
        liftIO $ ((,) <$> stocktakeStockId <*> stocktakeQuantity) . entityVal <$> stockTake `shouldBe` Just ("t-shirt-red", 17)
              
      it "doesn't update the adjustment key" $ do
        saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
|]
        Just (Entity oldKey old) <- runDB $ getBy (UniqueSB "ST16NV00399X" 1)
        -- create document
        now <- liftIO getCurrentTime
        let adj = StockAdjustment "test" now Pending (toSqlKey 1)
        adjId <- runDB $ insert adj
  
        runDB $ update oldKey [StocktakeAdjustment =. Just adjId]
         
        -- update
        updateSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,red,17,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
|]
        Just (Entity _ new) <- runDB $ getBy (UniqueSB "ST16NV00399X" 1)
        liftIO $ (stocktakeAdjustment new) `shouldBe` Just adjId
  
    describe "validation" $ do
      it "detects incorrect barcode on the first line" $ do
          postSTSheet 422 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-  shirt,black,120,shelf-1,ST16NV00399Z,34,20,17,2016/11/10,Jack,
|]  
          htmlAllContain "table td.stocktakeBarcode .parsing-error" "ST16NV00399Z"


      it "detects incorrect barcode" $ do
          postSTSheet 422 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
t-shirt,red,120,shelf-1,ST16NV00400X,34,20,17,2016/11/10,Jack,
|]  
          htmlAllContain "table td.stocktakeBarcode .parsing-error" "ST16NV00400X"



      it "detects incorrect partial barcode" $ do
          postSTSheet 422 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-  shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
t-shirt,red,120,shelf-1,400X,34,20,17,2016/11/10,Jack,
|]  
          htmlAllContain "table td.stocktakeBarcode .parsing-error" "400X"



      it "detects incorrect barcode sequence" $ do
          postSTSheet 422 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-  shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
t-  shirt,red,120,shelf-1,,34,20,17,2016/11/10,Jack,
t-  shirt,red,120,shelf-1,00400E,34,20,17,2016/11/10,Jack,
|]  
          htmlAnyContain "table td.stocktakeBarcode. .parsing-error" "ST16NV00399X"

      it "check barcode sequence ends" $ do
        postSTSheet 422 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
t-shirt,red,120,shelf-1,,34,20,17,2016/11/10,,
|]


          
      it "checks same barcode not twice in a row" $ do
        postSTSheet 422 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
t-shirt,red,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
|]


          
      it "checks everything is the same for group" $ do
          postSTSheet 422 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
t-shirt,black,120,shelf-2,-,34,20,17,2016/11/10,Jack,
|]  
          htmlAnyContain "table td.stocktakeLocation. .parsing-error" "shelf-2"

      it "expands shelves" $ do
          postSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-[12],ST16NV00399X,34,20,17,2016/11/10,Jack,
|]  
          htmlAnyContain "table td.stocktakeLocation" "shelf-1|shelf-2"

      it "checks shelves belongs to the same location " $ do
          postSTSheet 422 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-[123],ST16NV00399X,34,20,17,2016/11/10,Jack,
|]  
          htmlAnyContain "table td.stocktakeLocation " "find location with name"
  describe "null quantity " $ do
    it "accepts missing barcode" $ do
          saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,0,,,,,,2016/11/10,Jack,
|]  
          (zerotakes, boxtakes) <- runDB $ liftM2 (,)  (selectList[] []) (selectList [] [])
          liftIO $ do
              length (zerotakes ::[Entity Stocktake])  `shouldBe` 1
              length (boxtakes :: [Entity Boxtake]) `shouldBe` 0

    it "accepts 0 after boxes" $ do
        postSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
t-shirt,red,0,,,,,,,Jack,
|]
        htmlAllContain "table td.stocktakeStyle" "t-shirt"
        htmlCount "table td.stocktakeStyle" 2
        htmlAnyContain "table td.stocktakeQuantity" "0"

    it "breaks barcode sequences" $ do
          postSTSheet 422 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
t-shirt,red,120,shelf-1,,34,20,17,2016/11/10,Jack,
t-shirt,red,0,,,,,,2016/11/10,Jack,
|]  

    it "accepts 0 quantity in grouped box" $ do
        postSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
t-shirt,red,0,,,,,,,Jack,
t-shirt,black,120,shelf-1,ST16NV00400E,34,20,17,2016/11/10,Jack,
|]
        htmlAnyContain "table td.stocktakeBarcode" "ST16NV00399X"
        htmlCount "table td.stocktakeBarcode" 3

    it "use previous data if needed" $ do
        postSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
,red,0,,,,,,,,
|]
        htmlAllContain "table td.stocktakeStyle" "t-shirt"
        htmlCount "table td.stocktakeStyle" 2

    describe "quick test" $ do
      it "doesn't generate boxes" $ do
        postSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,,0,,,,2017/11/10,Jack,
|]
        boxtakeLengthShouldBe 0 

      it "doesn't invalidate previous boxes" $ do
        saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
,red,0,,,,,,,,
|]
        boxtakeLengthShouldBe 1
        postSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,,0,,,,2017/11/10,Jack,
|]
        lengthShouldBe [BoxtakeActive ==. True] 1 

      it "does invalidate previous stocktakes" $ do
        saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
,red,0,,,,,,,,
|]
        stocktakeLengthShouldBe 2 
        saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,,0,,,,2017/11/10,Jack,
|]
        stocktakeLengthShouldBe 3 
        lengthShouldBe [StocktakeActive ==. True] 2 

      it "reuse previous style, operator and date" $ do
        saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,,0,,,,2017/11/10,Jack,
,red,120,,0,,,,,,
|]
        stocktakeLengthShouldBe 2 

    describe "barcode lookup" $ do
      it "find content from barcode" $ do
        saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
,red,120,,-,,,,2016/11/10,Jack,
|]
        postSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,,,shelf-2,ST16NV00399X,,,,2016/11/10,Jack,
|]

        mapM (htmlAnyContain "table td.stocktakeColour") ["red", "black"]


      it "updates boxetakes" $ do
        saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
,red,120,,-,,,,2016/11/10,Jack,
|]
        saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,,,shelf-2,ST16NV00399X,,,,2016/11/10,Jack,
|]
        lengthShouldBe [BoxtakeLocation ==. "shelf-2"] 1

      it "updates boxtake history" $ do
        saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
,red,120,,-,,,,2016/11/10,Jack,
|]

        saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,,,shelf-2,ST16NV00399X,,,,2017/05/19,Jack,
|]
        boxM <- runDB $ getBy (UniqueBB "ST16NV00399X")
        liftIO $ (boxtakeLocationHistory . entityVal)
                <$> boxM `shouldBe` Just [( fromGregorian 2016 11 10
                                          , "shelf-1"
                                          )]
      it "updates stocktakes history" $ do
        saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
,red,120,,-,,,,2016/11/10,Jack,
|]

        saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt,,,shelf-2,ST16NV00399X,,,,2017/05/19,Jack,
|]
        Just (Entity jackK _) <- runDB $ selectFirst [OperatorNickname ==. "Jack"] []
        stockM <- runDB $ getBy (UniqueSB "ST16NV00399X" 1)
        liftIO $ (stocktakeHistory . entityVal)
                <$> stockM `shouldBe` Just [( fromGregorian 2016 11 10
                                            , jackK
                                            )]
              

    describe "invalidates previous take" $ do
      -- we used a different t-shirt code so that specs can be ran in parallel
      context "full take" $ do
        it "does invalidate previous boxes" $ do
          saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt#1,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
|]
          saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt#1,black,120,shelf-1,ST16NV00400E,34,20,17,2017/05/19,Jack,
|]
          boxtakeStatusShouldBe [ ("ST16NV00399X", False) -- unchanged
                                , ("ST16NV00400E", True) -- in the past
                                ]

          -- lengthShouldBe [BoxtakeDescription ==. Just "t-shirt#1-black", BoxtakeActive ==. False] 1

        it "does invalidate previous stocktakes" $ do
          saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt#2,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
|]
          saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt#2,black,120,shelf-1,ST16NV00400E,34,20,17,2017/05/19,Jack,
|]
          stocktakeStatusShouldBe [ ("ST16NV00399X", False) -- unchanged
                                , ("ST16NV00400E", True) -- in the past
                                ]

      context "zero take" $ do
        it "does invalidate previous boxes" $ do
          saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt#3,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
|]
          saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt#3,black,0,,,,,,2017/05/19,Jack,
|]

          -- lengthShouldBe [BoxtakeDescription ==. Just "t-shirt#3-black", BoxtakeActive ==. False] 1
          boxtakeStatusShouldBe [ ("ST16NV00399X", False) -- changed
                                ]

        it "does invalidate previous stocktakes" $ do
          saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt#4,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
|]
          saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt#4,black,0,,,,,,2017/05/19,Jack,
|]
          stocktakeStatusShouldBe [ ("ST16NV00399X", False) -- changed
                                ]
          stocktakeLengthShouldBe 2
          -- lengthShouldBe [StocktakeStockId ==. "t-shirt#4-black", StocktakeActive ==. False] 1

    describe "Does not invalidate old takes" $ do
    -- stocktakes older than exisiting should invalidates anything
      context "full take" $ do
        it "does NOT invalidate previous boxes" $ do
          saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt#11,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
|]
          saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt#11,black,120,shelf-1,ST16NV00400E,34,20,17,2015/05/19,Jack,
|]
          boxtakeStatusShouldBe [ ("ST16NV00399X", True) -- unchanged
                                , ("ST16NV00400E", False) -- in the past
                                ]

        it "does NOT invalidate previous stocktakes" $ do
          saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt#12,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
|]
          saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt#12,black,120,shelf-1,ST16NV00400E,34,20,17,2015/05/19,Jack,
|]
          stocktakeStatusShouldBe [ ("ST16NV00399X", True)
                                  , ("ST16NV00400E", False)
                                  ]


      context "zero take" $ do
        it "does NOT invalidate previous boxes" $ do
          saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt#13,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
|]
          saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt#13,black,0,,,,,,2015/05/19,Jack,
|]

          boxtakeStatusShouldBe [ ("ST16NV00399X", True) -- unchanged
                                ]

        it "does NOT invalidate previous stocktakes" $ do
          saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt#14,black,120,shelf-1,ST16NV00399X,34,20,17,2016/11/10,Jack,
|]
          saveSTSheet 200 [st|Style,Colour,Quantity,Location,Barcode Number,Length,Width,Height,Date Checked,Operator,Comment
t-shirt#14,black,0,,,,,,2015/05/19,Jack,
|]
          stocktakeStatusShouldBe [ ("ST16NV00399X", True) -- unchanged
                                ]
          stocktakeLengthShouldBe 1 -- no takes
