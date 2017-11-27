module Handler.WHBoxtake.UploadSpec where

import TestImport
import Handler.WH.Boxtake.Upload
import Data.List(mapAccumL)
import Database.Persist.MySQL
spec :: Spec
spec = parallel pureSpec


docKey :: Key DocumentKey
docKey = DocumentKeyKey' (SqlBackendKey 1)
o1 :: Operator
o1 = Operator "John" "Doe" "Jack" True
oid1 :: Key Operator
oid1 =  OperatorKey 1
b1 :: Boxtake
b1 = Boxtake (Just "B1") "stoctake" 10 20 30 "ST16OC00181N" "E00.01/1" d1 True (oid1) docKey []
bid1 :: Key Boxtake
bid1 = BoxtakeKey 1
d1 :: Day
d1 = fromGregorian 2015 10 01 

start = (Nothing, Nothing, Nothing, Nothing)
or1 = OperatorRow oe1
oe1 = Entity oid1 o1
lr1 = LocationRow "E00.01/1"
lr2 = LocationRow "E00.02/1"
dr1 = DateRow d1
br1 = BoxRow be1
be1 = Entity bid1 b1


shouldMake rows exp =
  let (result, newRows) = mapAccumL makeRow start rows
      r' = [ ((map toError) row) |(row) <- newRows]
      toError (Left _) = "Error"
      toError (Right _) = "Right"

  in (result, r') `shouldBe` exp


pureSpec = do
  describe "@pure @parallel #makeRow" $ do
    it "new operator .... open" $ do
      [or1] `shouldMake` ((Nothing, Just oe1, Nothing, Nothing)
                         , [Nothing])
    it "new location .. open" $ do
      [or1,lr1] `shouldMake` ((Nothing, Just oe1, Just "E00.01/1", Nothing)
                         , [Nothing, Nothing])
    it "all 3 ... close" $ do
      [or1,lr1,dr1] `shouldMake` ((Just d1, Just oe1, Just "E00.01/1", Nothing)
                         , [Nothing, Nothing, Nothing])
    it "barcode on open section : error" $ do
      [or1,lr1,br1] `shouldMake` ((Nothing, Just oe1, Just "E00.01/1", Nothing)
                         , [Nothing, Nothing, Just "Error"])
    it "barcodes2 on open section ... close but keep section" $ do
      [or1,lr1,br1,br1] `shouldMake` ((Nothing, Just oe1, Just "E00.01/1", Nothing)
                         , [Nothing, Nothing, Just "Error", Just "Error"])
    context "bug" $ do
      let step0 = [or1,lr1,br1,dr1]
          state1 =  (Just d1, Just oe1, Just "E00.01/1", Nothing)
          rows1 =  [Nothing, Nothing, Just "Error", Nothing]
          extraStep = [br1]
          extraRow = [Just "Right"]
          state2 = (Just d1, Just oe1, Just "E00.01/1", Just be1)
          step1 = step0  <> extraStep
          rows2 = rows1 <> extraRow
      it "close section after error." $ do
            step0 `shouldMake` (state1, rows1)
        -- [or1,lr1,br1,dr1] `shouldMake` ((Just d1, Just oe1, Just "E00.01/1", Nothing)
        --                    , [Nothing, Nothing, Just "Error", Nothing])
      it "intermediate " $ do
        -- last step before 2 previous test
        -- let (state, rows) = mapAccumR makeRow (Just d1, Just oe1, Just "E00.01/1", Nothing) [br1]
        -- state `shouldBe` (Just d1, Just oe1, Just "E00.01/1", Just be1)
        let (state,rows) = mapAccumL makeRow state1 extraStep
        state `shouldBe` state2

      it "close section after error. process barcode" $ do
        -- [or1,lr1,br1,dr1,br1] `shouldMake` ((Just d1, Just oe1, Just "E00.01/1", Just be1)
        --                   , [Nothing, Nothing, Just "Error", Nothing, Just "Right"])
            step1 `shouldMake` (state2, rows2)
        -- [or1,lr1,br1,dr1] `shouldMake` ((Just d1, Just oe1, Just "E00.01/1", Nothing)
      
    it "open new on operator" $ do
      [or1,lr1,br1,dr1,br1, or1] `shouldMake` ((Nothing , Just oe1, Nothing, Nothing)
                         , [Nothing, Nothing, Just "Error", Nothing, Just "Right", Nothing])
    it "open new on date" $ do
      [or1,lr1,br1,dr1,br1, dr1] `shouldMake` ((Just d1 , Nothing, Nothing, Nothing)
                         , [Nothing, Nothing, Just "Error", Nothing, Just "Right", Nothing])
    it "continue on location new on date" $ do
      [or1,lr1,br1,dr1,br1, lr2] `shouldMake` ((Just d1 , Just oe1 , Just "E00.02/1", Just be1)
                         , [Nothing, Nothing, Just "Error", Nothing, Just "Right", Nothing])

