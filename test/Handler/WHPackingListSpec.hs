{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes#-}
module Handler.WHPackingListSpec where

import TestImport
import Text.Shakespeare.Text (st)
import Yesod.Auth (requireAuthId)
import Database.Persist.Sql (toSqlKey)
import Network.HTTP.Types.Status
import Data.List (dropWhileEnd)
import Handler.WH.PackingList

spec :: Spec
spec = pureSpec >> appSpec

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
    byLabel "order ref" "order-16017"
    byLabel "invoice ref" "INV-16107&16137A.xls"
    byLabel "container" "C1"
    byLabel "vessel" "Britanny Ferry/Bretagne"
    byLabel "spreadsheet" sheet
    byLabel "departure" "2016-12-01"
    byLabel "arriving" "2017-01-15"
    addPostParam "action" (tshow mode)
    
    -- byLabel "encoding" (tshow 1)
  -- printBody

  statusIs (fromEnum status)

validatePLSheet status sheet = do
  uploadPLSheet Validate status sheet
  
savePLSheet = uploadPLSheet Save

appSpec = withAppWipe BypassAuth $ do
  describe "upload packing list" $ do
    describe "#valid" $ do
      it "validates simple boxes" $ do
        validatePLSheet 200 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04
Cardigan,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0
|]

      it "validates mixed box" $ do
        validatePLSheet 200 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
Cardigan,Red,12,,,,,1,,,X,,X,,,,,,,
T-shirt,Black,24,13,,13,1,36,36,79,X,45,X,38,0.14,0.3,1.51,0.135,0.3,6.04
|]
      it "many orders " $ do
        validatePLSheet 200 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04
Container C2,,,,,,,,,,,,,,,,,,,
Cardigan,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0|]
  
    describe "#save" $ do
      it "saves" $ do
        savePLSheet created201 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04
CardiganForSave,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0
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
        savePLSheet created201 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04
Cardigan,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0
|]
        savePLSheet expectationFailed417 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04
Cardigan,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0
|]

    describe "#invalid" $ do
      it "detects missing columns" $ do
        validatePLSheet badRequest400 [st|Style No .,missing,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04
Cardigan,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0
|]
      it "detects wrong total quantity" $ do
        validatePLSheet badRequest400 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
T-shirt,Black,24,13,,16,4,6,20,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04
|]
        bodyContains "Total quantity doesn"

      it "detects wrong number of cartons" $ do
        validatePLSheet badRequest400 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
T-shirt,Black,24,13,,16,5,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04
|]
        bodyContains "Number of carton doesn"

      it "detects unfinished (EOF)" $ do
        validatePLSheet badRequest400 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
Cardigan,Red,12,,,,,1,,,X,,X,,,,,,,
|]
        bodyContains "Box not closed"

      it "detects unfinished (End of Container)" $ do
        validatePLSheet badRequest400 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
Cardigan,Red,12,,,,,1,,,X,,X,,,,,,,
Container C2,,,,,,,,,,,,,,,,,,,
|]
        bodyContains "Box not closed"
      it "detects different length within group" $ do
        validatePLSheet badRequest400 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
Cardigan,Red,12,,,,,1,,70,X,,X,,,,,,,
T-shirt,Black,24,13,,13,1,36,36,79,X,45,X,38,0.14,0.3,1.51,0.135,0.3,6.04
|]
        bodyContains "Box Dimension should be the same within a box"

      it "detects different width within group" $ do
        validatePLSheet badRequest400 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
Cardigan,Red,12,,,,,1,,,X,100,X,,,,,,,
T-shirt,Black,24,13,,13,1,36,36,79,X,45,X,38,0.14,0.3,1.51,0.135,0.3,6.04
|]
        bodyContains "Box Dimension should be the same within a box"

      it "detects different height within group" $ do
        validatePLSheet badRequest400 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
Cardigan,Red,12,,,,,1,,,X,,X,70,,,,,,
T-shirt,Black,24,13,,13,1,36,36,79,X,45,X,38,0.14,0.3,1.51,0.135,0.3,6.04
|]
        bodyContains "Box Dimension should be the same within a box"

      it "detects wrong volume" $ do
        validatePLSheet badRequest400 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.24,0.3,1.51,0.54,1.2,6.04
|]
        bodyContains "Volume doesn"
      it "detects wrong total volume" $ do
        validatePLSheet badRequest400 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.4,1.2,6.0
|]
        bodyContains "Total volume doesn"

      it "detects wrong total weight" $ do
        validatePLSheet badRequest400 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,120,6.04
|]
        bodyContains "Total weight doesn"
  
shouldGenerate content expectation =
  let result = contentToMarks content

  in (dropWhileEnd (=="") result) `shouldBe` expectation



pureSpec = do
  describe "@pure #stickers from PL" $ do
    it "generates 12" $ do
      [("BLK", 12)] `shouldGenerate` ["BLK", "⃝", "⃝"]
    it "generates two lines without name duplication" $ do
      [("BLK", 24)] `shouldGenerate` [ "BLK", "⃝", "⃝", "⃝"
                                     , ""   , "⃝"
                                     ]
    it "generates two lines with different names " $ do
      [("BLK", 12), ("RED", 12)] `shouldGenerate` [ "BLK", "⃝", "⃝", ""
                                                  , "RED", "⃝", "⃝"
                                                  ]
    it "generates colour in alphabetical order" $ do
      [("RED", 12), ("BLK", 12)] `shouldGenerate` [ "BLK", "⃝", "⃝", ""
                                                  , "RED", "⃝", "⃝"
                                                  ]
    it "more lines colour in alphabetical order" $ do
      [("RED", 12), ("BLK", 6), ("NAY", 24)] `shouldGenerate` [ "BLK", "⃝",    "", ""
                                                              , "NAY", "⃝", "⃝", "⃝"
                                                              , ""   , "⃝", ""   , ""
                                                              , "RED", "⃝", "⃝"
                                                              ]
      
      
      
      
