{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes#-}
module Handler.WHPackingListSpec (spec) where

import TestImport
import Text.Shakespeare.Text (st)
import Network.HTTP.Types.Status
import Data.List (dropWhileEnd)
import Handler.WH.PackingList
import qualified Data.Map as Map
import Handler.CsvUtils

spec :: Spec
spec = parallel pureSpec >> appSpec

data Mode = Save | Validate deriving (Eq, Show)
uploadPLSheet mode status sheet = do
  let route = WarehouseR WHPackingListR
  logAsAdmin 
  get route
  statusIs 200

  request $ do
    setMethod "POST"
    setUrl route
    addToken_ "form#upload-form"
    byLabelExact "order ref" "order-16017"
    byLabelExact "invoice ref" "INV-16107&16137A.xls"
    byLabelExact "container" "C1"
    byLabelExact "vessel" "Britanny Ferry/Bretagne"
    byLabelExact "batch" "A"
    byLabelExact "spreadsheet" sheet
    byLabelExact "departure" "2016-12-01"
    byLabelExact "arriving" "2017-01-15"
    addPostParam "action" (tshow mode)
    
    -- byLabelExact "encoding" (tshow 1)
  -- printBody

  statusIs (fromEnum status)

validatePLSheet status sheet = do
  uploadPLSheet Validate status sheet
  
savePLSheet = uploadPLSheet Save

updateDetails action status cart = do
  (Just (Entity key _)) <- runDB $ selectFirst [] [Desc PackingListId]
  let [PersistInt64 plId] =  keyToValues key
  logAsAdmin
  get (WarehouseR $ WHPackingListViewR plId (Just EditDetails))
  statusIs 200

  let route = WarehouseR $ WHPackingListEditDetailsR plId
  request $ do
    setMethod "POST"
    setUrl route
    addToken_ "form#edit-details"
    byLabelExact "cart" cart
    addPostParam "action" (tshow action)
  statusIs (fromEnum status)

replaceDetails = updateDetails Replace
insertDetails = updateDetails Insert
deleteDetails = updateDetails Delete

viewDeliverTab status = do
  -- create an operator if needed
  oCount <- runDB $ count [OperatorActive ==. True]
  when ( oCount == 0 ) $ do
       runDB $ insertUnique $ Operator "John" "Smith" "Jack" True
       return ()
  (Just (Entity key _)) <- runDB $ selectFirst [] [Desc PackingListId]
  let [PersistInt64 plId] =  keyToValues key
  logAsAdmin 
  get (WarehouseR $ WHPackingListViewR plId (Just Deliver))
  statusIs status
  return plId

deliver status cart = do
  plId <- viewDeliverTab 200

  let route = WarehouseR $ WHPackingListDeliverR plId
  request $ do
    setMethod "POST"
    setUrl route
    addToken_ "form#deliver-details"
    byLabelExact "cart" cart
    byLabelExact "operator" "1"
    byLabelExact "date" "2017-03-11"
    byLabelExact "location" "Default"

  statusIs (fromEnum status)

appSpec = withAppWipe BypassAuth $ do
  describe "upload packing list" $ do
    describe "#valid" $ do
      it "validates simple boxes" $ do
        validatePLSheet 200 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
Cardigan,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0,B
|]

      it "validates mixed box" $ do
        validatePLSheet 200 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
Cardigan,Red,12,,,,,1,,,X,,X,,,,,,,,A
T-shirt,Black,24,13,,13,1,36,36,79,X,45,X,38,0.14,0.3,1.51,0.135,0.3,6.04,B
|]
      it "many orders " $ do
        validatePLSheet 200 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
Container C2,,,,,,,,,,,,,,,,,,,,A
Cardigan,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0,A
|]
  
    describe "#save" $ do
      it "saves" $ do
        savePLSheet created201 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
CardiganForSave,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0,A
|]
        pls <- runDB $ selectList [] []
        liftIO $ do
          length pls `shouldBe` 1
          let [(Entity _ pl)] = pls
          packingListDeparture pl `shouldBe` (Just (fromGregorian 2016 12 1))
          packingListArriving pl `shouldBe` (Just (fromGregorian 2017 1 15))

        c <- runDB $ count [PackingListDetailStyle ==. "CardiganForSave"]
        liftIO $ c `shouldBe` 4 -- 4 boxes

      it "saves only once" $ do
        savePLSheet created201 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
Cardigan,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0,A
|]
        savePLSheet expectationFailed417 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
Cardigan,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0,A
|]

    describe "#invalid" $ do
      it "detects missing columns" $ do
        validatePLSheet badRequest400 [st|Style No .,missing,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
Cardigan,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0,A
|]
      it "detects wrong total quantity" $ do
        validatePLSheet badRequest400 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Black,24,13,,16,4,6,20,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
|]
        bodyContains "Total quantity doesn"

      it "detects wrong number of cartons" $ do
        validatePLSheet badRequest400 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Black,24,13,,16,5,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
|]
        bodyContains "Number of carton doesn"

      it "detects unfinished (EOF)" $ do
        validatePLSheet badRequest400 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
Cardigan,Red,12,,,,,1,,,X,,X,,,,,,,,A
|]
        bodyContains "Box not closed"

      it "detects unfinished (End of Container)" $ do
        validatePLSheet badRequest400 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
Cardigan,Red,12,,,,,1,,,X,,X,,,,,,,,A
Container C2,,,,,,,,,,,,,,,,,,,,A
|]
        bodyContains "Box not closed"
      it "detects different length within group" $ do
        validatePLSheet badRequest400 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
Cardigan,Red,12,,,,,1,,70,X,,X,,,,,,,,A
T-shirt,Black,24,13,,13,1,36,36,79,X,45,X,38,0.14,0.3,1.51,0.135,0.3,6.04,A
|]
        bodyContains "Box Dimension should be the same within a box"

      it "detects different width within group" $ do
        validatePLSheet badRequest400 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
Cardigan,Red,12,,,,,1,,,X,100,X,,,,,,,,A
T-shirt,Black,24,13,,13,1,36,36,79,X,45,X,38,0.14,0.3,1.51,0.135,0.3,6.04,A
|]
        bodyContains "Box Dimension should be the same within a box"

      it "detects different height within group" $ do
        validatePLSheet badRequest400 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
Cardigan,Red,12,,,,,1,,,X,,X,70,,,,,,,A
T-shirt,Black,24,13,,13,1,36,36,79,X,45,X,38,0.14,0.3,1.51,0.135,0.3,6.04,A
|]
        bodyContains "Box Dimension should be the same within a box"

      it "detects wrong volume" $ do
        validatePLSheet badRequest400 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.24,0.3,1.51,0.54,1.2,6.04,A
|]
        bodyContains "Volume doesn"
      it "detects wrong total volume" $ do
        validatePLSheet badRequest400 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.4,1.2,6.0,A
|]
        bodyContains "Total volume doesn"

      it "detects wrong total weight" $ do
        validatePLSheet badRequest400 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,120,6.04,A
|]
        bodyContains "Total weight doesn"
  

  describe "@editpl updating packing list details" $ do
    describe "#valid" $ do
      describe "#replaces" $ do
        it "replaces all details with the new ones" $ do
          savePLSheet created201 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
CardiganForSave,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0,A
|]
          replaceDetails ok200 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
CardiganForUpdate,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0,A
|]
          details <- runDB $ selectList [] [Asc PackingListDetailId]
          liftIO $ map (packingListDetailStyle . entityVal) details `shouldBe` (replicate 4 "T-shirt") ++  (replicate 4 "CardiganForUpdate" )

        it "replace with mixed box" $ do
          savePLSheet created201 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
CardiganForSave,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0,A
|]
          replaceDetails 200 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Red,12,,,,,1,,,X,,X,,,,,,,,
T-shirt,Black,24,13,,13,1,36,36,79,X,45,X,38,0.14,0.3,1.51,0.135,0.3,6.04,A
|]

          details <- runDB $ selectList [] [Asc PackingListDetailId]
          liftIO $ map (packingListDetailContent .entityVal) details `shouldBe` [Map.fromList [("Red", 12), ("Black", 24)]]

      describe "#insert" $ do
        it "keeps previous details" $ do
          savePLSheet created201 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
CardiganForSave,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0,A
|]
          insertDetails ok200 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Red,24,23,,26,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
|]
          details <- runDB $ selectList [] [Asc PackingListDetailId]
          liftIO $ length details `shouldBe` 12
       
        it "uses default order reference" $ do
          savePLSheet created201 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
Container C1,,,,,,,,,,,,,,,,,,,,A
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
Container C2,,,,,,,,,,,,,,,,,,,,A
CardiganForSave,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0,A
|]
          insertDetails ok200 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Red,24,23,,26,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
|]
          details <- runDB $ selectList [PackingListDetailReference ==. "Container C1"] []
          liftIO $ length details `shouldBe` 8

        it "uses row order reference" $ do
          savePLSheet created201 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
Container C1,,,,,,,,,,,,,,,,,,,,A
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
Container C2,,,,,,,,,,,,,,,,,,,,A
CardiganForSave,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0,A
|]
          insertDetails ok200 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
Container C2,,,,,,,,,,,,,,,,,,,,A
T-shirt,Red,24,23,,26,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
|]
          details <- runDB $ selectList [PackingListDetailReference ==. "Container C2"] []
          liftIO $ length details `shouldBe` 8

      it "deletes expected details" $ do
          savePLSheet created201 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
Container C1,,,,,,,,,,,,,,,,,,,,A
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
Container C2,,,,,,,,,,,,,,,,,,,,A
CardiganForSave,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0,A
|]
          deleteDetails ok200 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Red,24,23,,26,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
|]
          details <- runDB $ selectList [PackingListDetailReference ==. "Container C2"] []
          liftIO $ length details `shouldBe` 4

  describe "@deliver #deliver" $ do
    it "delivers what's on the cart" $ do
      savePLSheet created201 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
CardiganForSave,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0,A
|]
      deliver ok200 "1,T-shirt\n2,T-shirt"
      delivereds <- runDB $ selectList [PackingListDetailDelivered ==. True] []
      liftIO $ length delivereds `shouldBe` 2

    it "undelivers what's not on the cart" $ do
      savePLSheet created201 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
CardiganForSave,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0,A
|]
      deliver ok200 "1,T-shirt\n2,T-shirt\n"
      delivereds <- runDB $ selectList [PackingListDetailDelivered ==. True] []
      liftIO $ length delivereds `shouldBe` 2

      deliver ok200 "-1,T-shirt\n"
      undelivereds <- runDB $ selectList [PackingListDetailDelivered ==. False] []
      liftIO $ length undelivereds `shouldBe` 7

    it "creates the correstponding boxtake" $ do
      savePLSheet created201 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
CardiganForSave,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0,A
|]
      deliver ok200 "1,T-shirt\n2,T-shirt\n"
      boxtakes <- runDB $ selectList [] [Asc BoxtakeId]
      liftIO $ length boxtakes `shouldBe` 2
  
    it "removes the correstponding boxtake" $ do
      savePLSheet created201 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
CardiganForSave,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0,A
|]
      deliver ok200 "1,T-shirt\n2,T-shirt\n"
      deliver ok200 "-1,T-shirt\n"
      boxtakes <- runDB $ selectList [] [Asc BoxtakeId]
      liftIO $ length boxtakes `shouldBe` 1
  
    it "update denorm field accordingly" $ do
      savePLSheet created201 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
CardiganForSave,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0,A
|]
      deliver ok200 "1,T-shirt\n2,T-shirt\n"
      Just (Entity _ pl) <- runDB $ selectFirst [] [Asc PackingListId]
      liftIO $ packingListBoxesToDeliver_d pl `shouldBe` 6
    describe "#prefilled cart" $ do
      it "contains all undelivered items" $ do
        savePLSheet created201 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
CardiganForSave,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0,A
|]
        viewDeliverTab 200
        htmlAllContain "#deliver-details" "1,T-shirt"

      it "contains delivered items but commented" $ do
        savePLSheet created201 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)",Batch
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04,A
CardiganForSave,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0,A
|]
        deliver ok200 "1,T-shirt\n2,T-shirt\n"
        viewDeliverTab 200
        htmlAllContain "#deliver-details" "-- -1,T-shirt"
  
shouldGenerate content expectation =
  let result = contentToMarks content
  in (dropWhileEnd (=="") result) `shouldBe` expectation


pureSpec = do
  describe "@pure @parallel #stickers from PL" $ do
    it "generates 12" $ do
      [("BLK", 12)] `shouldGenerate` ["BLK", "○", "○"]
    it "generates two lines without name duplication" $ do
      [("BLK", 24)] `shouldGenerate` [ "BLK", "○", "○", "○"
                                     , ""   , "○"
                                     ]
    it "generates two lines with different names " $ do
      [("BLK", 12), ("RED", 12)] `shouldGenerate` [ "BLK", "○", "○", ""
                                                  , "RED", "○", "○"
                                                  ]
    it "generates colour in alphabetical order" $ do
      [("RED", 12), ("BLK", 12)] `shouldGenerate` [ "BLK", "○", "○", ""
                                                  , "RED", "○", "○"
                                                  ]
    it "more lines colour in alphabetical order" $ do
      [("RED", 12), ("BLK", 6), ("NAY", 24)] `shouldGenerate` [ "BLK", "○",    "", ""
                                                              , "NAY", "○", "○", "○"
                                                              , ""   , "○", ""   , ""
                                                              , "RED", "○", "○"
                                                              ]
    it "displays 1 properly" $ do
      [("BLK", 1)] `shouldGenerate` ["BLK", "∅"]
  describe "@pure @parallel @deliver parses deliver cart" $ do
    it "parses ids to deliver" $ do
      let cart = "1,comment\n"
          result = parseDeliverList cart
      result `shouldBe` ParsingCorrect ((map PackingListDetailKey [1]), [])

    it "parses ids to deliver" $ do
      let cart = "-1,comment\n"
          result = parseDeliverList cart
      result `shouldBe` ParsingCorrect ([], (map PackingListDetailKey [1]))

    it "skips comments" $ do
      let cart = "--1,comment\n"
          result = parseDeliverList cart
      result `shouldBe` ParsingCorrect ([], [])


      
      
      
      
