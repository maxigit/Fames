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
  statusIs status

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
T-shirt,Black,24,13,,13,1,36,36,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04
Cardigan,Red,12,,,,,1,,,X,,X,,0.12,0.4,1.9,0.49,1.6,0|]
      it "many orders " $ do
        validatePLSheet 200 [st|Style No .,Color,QTY,C/NO,,last,CTN,QTY/CTN,TQTY,Length,,Width,,Height,CBM/CTN,N.W./CTN,G.W./CTN,TV,N.W,"G.W.(KGS)"
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04
Container C2,,,,,,,,,,,,,,,,,,,
Cardigan,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0|]
  
    describe "#save" $ do
    describe "#invalid" $ do
      it "detects missing columns" $ do
        validatePLSheet 400 [st|Style No .,color,QTY,C/NO,,,CTN,QTY/CTN,QTY," MEAS/CTN. (L*W*H cm)",,,,,CBM/CTN,N. W./CTN,G.W./CTN,CBM,"N.W. (KGS)","G.W. (KGS)"
T-shirt,Black,24,13,,16,4,6,24,79,X,45,X,38,0.14,0.3,1.51,0.54,1.2,6.04
Cardigan,Red,24,1124,,1127,4,6,24,41,X,39,X,76,0.12,0.4,1.9,0.49,1.6,0|]

      it "detects wrong number of cartons" (const pending)
      it "detects groups with wrong number of cartons" (const pending)
      it "detects unfinished" (const pending)
      it "detects wrong volume" (const pending)
      it "detects wrong total volume" (const pending)
      it "detects wrong total weight" (const pending)
      it "detects group with many dimensions" (const pending)
      
  
