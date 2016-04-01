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
        -- we use 2011 so that 11 is also a valid month
        let expectedDate = fromGregorian 2011 01 02
        in it ("like '" <> show str) $ do
            assertField str expectedDate 
            | str  <- ["2011/01/02" 
                        , "02/01/2011"
                        , "02/01/11"
                        , "02 Jan 2011"
                        , "02 Jan 11"
                        , "02-Jan-11"
                        , "2011 Jan 02"
                        , "2011-Jan-02"
                        , "2011-Jan-02"
                        , "2011-01-02"
                        , "Wed 02 Jan 2011"
                        ]
            ] 
    context "should parse amounts" $ do
      it "without currency" $ do
        assertUtf8Field "23.45" (23.45 :: Amount')
      it "without currency and spaces" $ do
        assertUtf8Field " 23.45 " (23.45 :: Amount')
      it "with currency" $ do
        assertUtf8Field "£23.45" (23.45 :: Amount')
      it "with currency and spaces" $ do
        assertUtf8Field " £23.45 " (23.45 :: Amount')
      it "with currency and spaces between the currency" $ do
        assertUtf8Field "£ 23.45 " (23.45 :: Amount')
      it "negative with currency and spaces between the currency" $ do
        assertUtf8Field " - £ 23.45 " (-23.45 :: Amount')
    

assertField str value  = decode NoHeader (str <> ",") `shouldBe` Right (fromList [(value, ())])
-- we need  to encode bystestring in UTF8 because the IsString instance for bytestring
-- doesn't encode £ to two words but only one.
assertUtf8Field text value = assertField (encodeUtf8 text) value
