{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes#-}
module Handler.WHPackingListSpec where

import TestImport
import Text.Shakespeare.Text (st)
import Yesod.Auth (requireAuthId)
import Database.Persist.Sql (toSqlKey)

spec :: Spec
spec = appSpec

uploadPLSheet status sheet = do
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
    
    -- byLabel "encoding" (tshow 1)
  -- printBody

  statusIs status

validatePLSheet status sheet = do
  uploadPLSheet status sheet
  
appSpec = withAppWipe BypassAuth $ do
  describe "@current upload packing list" $ do
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
      it "saves" (const pending)
    describe "#invalid" $ do
      it "detects missing columns" $ do
        validatePLSheet 422 [st|Style No .,missing,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04
Cardigan,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0
|]
      it "detects wrong total quantity" $ do
        validatePLSheet 422 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
T-shirt,Black,24,13,,16,4,6,20,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04
|]
        bodyContains "Total quantity doesn"

      it "detects wrong number of cartons" $ do
        validatePLSheet 422 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
T-shirt,Black,24,13,,16,5,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04
|]
        bodyContains "Number of carton doesn"

      it "detects unfinished" $ do
        validatePLSheet 422 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
Cardigan,Red,12,,,,,1,,,X,,X,,,,,,,
|]
      it "detects different length within group" $ do
        validatePLSheet 422 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
Cardigan,Red,12,,,,,1,,70,X,,X,,,,,,,
T-shirt,Black,24,13,,13,1,36,36,79,X,45,X,38,0.14,0.3,1.51,0.135,0.3,6.04
|]
        bodyContains "Box Dimension should be the same within a box"

      it "detects different width within group" $ do
        validatePLSheet 422 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
Cardigan,Red,12,,,,,1,,,X,100,X,,,,,,,
T-shirt,Black,24,13,,13,1,36,36,79,X,45,X,38,0.14,0.3,1.51,0.135,0.3,6.04
|]
        bodyContains "Box Dimension should be the same within a box"

      it "detects different height within group" $ do
        validatePLSheet 422 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
Cardigan,Red,12,,,,,1,,,X,,X,70,,,,,,
T-shirt,Black,24,13,,13,1,36,36,79,X,45,X,38,0.14,0.3,1.51,0.135,0.3,6.04
|]
        bodyContains "Box Dimension should be the same within a box"

      it "detects wrong volume" $ do
        validatePLSheet 422 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.24,0.3,1.51,0.54,1.2,6.04
|]
        bodyContains "Volume doesn"
      it "@fail detects wrong total volume" $ do
        validatePLSheet 422 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.4,1.2,6.0
|]
        bodyContains "Total volume doesn"

      it "detects wrong total weight" $ do
        validatePLSheet 422 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,120,6.04
|]
        bodyContains "Total weight doesn"
  
