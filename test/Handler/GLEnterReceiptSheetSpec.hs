module Handler.GLEnterReceiptSheetSpec (spec) where

import TestImport

import Handler.GLEnterReceiptSheet
import Data.Csv (decode, HasHeader(NoHeader))
spec :: Spec
spec = pureSpec -- >> appSpec

appSpec :: Spec
appSpec = withApp $ do

    describe "getGLEnterReceiptSheetR" $ do
        error "Spec not implemented: getGLEnterReceiptSheetR"


    describe "postGLEnterReceiptSheetR" $ do
        error "Spec not implemented: postGLEnterReceiptSheetR"


pureSpec :: Spec
pureSpec = do
  describe "Parshing csv" $ do
    context "should parse dates" $ do
      sequence_ [
        let expectedDate = fromGregorian 2015 01 02
        in it ("like '" <> show str) $ do
            assertField str expectedDate 
            | str  <- ["2015/01/02" 
                        , "02/01/2015"
                        , "02/01/15"
                        , "02 Jan 2015"
                        , "02 Jan 15"
                        , "02-Jan-15"
                        , "2015 Jan 02"
                        , "2015-Jan-02"
                        , "2015-Jan-02"
                        , "2015-01-02"
                        , "2015/01/02"
                        ]
            ] 
    context "should parse amounts" $ do
      it "without currency" $ do
        assertField "23.45" (23.45 :: Amount')
      it "without currency and spaces" $ do
        assertField " 23.45 " (23.45 :: Amount')
      it "with currency" $ do
        assertField "£23.45" (23.45 :: Amount')
      it "with currency and spaces" $ do
        assertField " £23.45 " (23.45 :: Amount')
      it "with currency and spaces between the currency" $ do
        assertField "£ 23.45 " (23.45 :: Amount')
    

assertField str value  = decode NoHeader (str <> ",") `shouldBe` Right (fromList [(value, ())])
