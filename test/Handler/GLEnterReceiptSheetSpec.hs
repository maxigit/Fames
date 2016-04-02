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
    

  describe "validateRawRow" $ do
    let validRawRow = RawReceiptRow rnot rnot rnot rnot
                                 rnot rnot rnot rnot
                                 rnot rnot rnot rnot
        rnot = Right Nothing
        rjust = Right . Just
        emptyRow = ReceiptRow  Nothing Nothing Nothing
                               Nothing Nothing Nothing
                               Nothing
        date = fromGregorian 2015 11 27
        emptyHeader = HeaderRow date "" "" Nothing 0
                               Nothing Nothing Nothing
                               Nothing Nothing Nothing
                               Nothing
        
    it "finds header" $ do
      let raw = validRawRow { rrDate = rjust date
                            , rrCompany = rjust "company"
                            , rrBankAccount = rjust "bank"
                            , rrTotalAmount = rjust 99
                            }
      validateRawRow raw `shouldBe` Right (emptyHeader { rCompany = "company"
                                                    , rBankAccount = "bank"
                                                    , rTotalAmount = 99
                                                    }
                                       )
    it "finds invalid header" $ do
      let raw = validRawRow { rrDate = rjust date
                            , rrCompany = rjust "company"
                            , rrTotalAmount = rjust 99
                            }
      validateRawRow raw `shouldBe` Left (raw, InvalidReceiptHeader)
    it "finds normal row" $ do
      let raw = validRawRow { rrItem= rjust  "item"
                            , rrItemPrice = rjust 12.50
                            }
      validateRawRow raw `shouldBe` Right (emptyRow { rItemPrice = Just 12.50
                                                    , rItem = Just "item"
                                                    }
                                           )
    it "find row without price" $ do
      let raw = validRawRow { rrItem = rjust  "item"
                            }
      validateRawRow raw `shouldBe` Left (raw, PriceMissing)


assertField str value  = decode NoHeader (str <> ",") `shouldBe` Right (fromList [(value, ())])
-- we need  to encode bystestring in UTF8 because the IsString instance for bytestring
-- doesn't encode £ to two words but only one.
assertUtf8Field text value = assertField (encodeUtf8 text) value
