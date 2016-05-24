{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes#-}
module Handler.GLEnterReceiptSheetSpec (spec) where

import TestImport

import Handler.GLEnterReceiptSheet
import Data.Csv (decode, HasHeader(NoHeader))
import Text.Shakespeare.Text (st)

spec :: Spec
spec = pureSpec >> storiesSpec >> appSpec

postReceiptSheet status sheet = do
  get GLEnterReceiptSheetR
  statusIs 200

  request $ do
    setMethod "POST"
    setUrl GLEnterReceiptSheetR
    addToken_ "form#text-form " --"" "#text-form"
    byLabel "Sheet name" "test 1"
    byLabel "Receipts" sheet

  statusIs status

appSpec :: Spec
appSpec = withApp $ do
    describe "getGLEnterReceiptSheetR" $ do
	it "proposes to upload a file" $ do
	  get GLEnterReceiptSheetR
          statusIs 200
   
   	  bodyContains "upload" -- to transform

    describe "postGLEnterReceiptSheetR" $ do
        it "parses correctly amounts with pound sign" $ do
	  get GLEnterReceiptSheetR
          statusIs 200

          request $ do
            setMethod "POST"
            setUrl GLEnterReceiptSheetR
            addToken_ "form#text-form " --"" "#text-form"
            byLabel "Sheet name" "test 1"
            byLabel "Receipts" [st|date,counterparty,bank account,total,gl account,amount,tax rate
2015/01/02,stapples,B1,180,7501,£100,20%
|]

          statusIs 200
          htmlAnyContain "#receipt1 .amount" "100.00"
        it "displays correctly a receipt with multiple lines" $ do
	  get GLEnterReceiptSheetR
          statusIs 200
          -- we try here the text area form instead of file  because it's
          -- easier to setup and read

          request $ do
            setMethod "POST"
            setUrl GLEnterReceiptSheetR
            addToken_ "form#text-form " --"" "#text-form"
            byLabel "Sheet name" "test 1"
            byLabel "Receipts" [st|date,counterparty,bank account,total,gl account,amount,tax rate
2015/01/02,stapples,B1,180,7501,100,20%
,,,,8000,50,20%
|]

          statusIs 200

          htmlAnyContain "#receipt1 .amount" "100.00"
          htmlAnyContain "#receipt1 .glAccount" "7501"
          htmlAnyContain "#receipt1-2 .amount" "50.00"
          htmlAnyContain "#receipt1-2 .glAccount" "8000"

        it "displays correctly a spreadsheet with multiple receipts" $ do
	  get GLEnterReceiptSheetR
          statusIs 200
          -- we try here the text area form instead of file  because it's
          -- easier to setup and read

          request $ do
            setMethod "POST"
            setUrl GLEnterReceiptSheetR
            addToken_ "form#text-form " --"" "#text-form"
            byLabel "Sheet name" "test 2"
            byLabel "Receipts" [st|date,counterparty,bank account,total,gl account,amount,tax rate
2015/01/02,stapples,B1,120,7501,100,20%
2015/01/02,stapples,B1,60,8000,50,20%|]
        
          statusIs 200
          htmlAnyContain "#receipt1-1 .amount" "100.00"
          htmlAnyContain "#receipt1-1 .glAccount" "7501"
          htmlAnyContain "#receipt2-1 .amount" "50.00"
          htmlAnyContain "#receipt2-1 .glAccount" "8000"

        it "displays Unprocessable Entity the spreadsheet is not total valid" $ do
          return pending
	  get GLEnterReceiptSheetR
          statusIs 200
          -- we try here the text area form instead of file  because it's
          -- easier to setup and read

          request $ do
            setMethod "POST"
            setUrl GLEnterReceiptSheetR
            addToken_ "form#text-form " --"" "#text-form"
            byLabel "Sheet name" "test 2"
            byLabel "Receipts" [st|date,counterparty,bank account,total,gl account,amount,tax rate
2015/01/02,,B1,120,7501,100,20%|]
        

          statusIs 422

        it "displays correctly a spreadsheet with multiple receipts" $ do
	  get GLEnterReceiptSheetR
          statusIs 200
          -- we try here the text area form instead of file  because it's
          -- easier to setup and read

          request $ do
            setMethod "POST"
            setUrl GLEnterReceiptSheetR
            addToken_ "form#text-form " --"" "#text-form"
            byLabel "Sheet name" "test 2"
            byLabel "Receipts" [st|date,counterparty,bank account,total,gl account,amount,tax rate
2015/01/02,stapples,B1,120,7501,100,20%
2015/01/02,stapples,B1,60,8000,50,20%|]
        
          statusIs 200
          htmlAnyContain "#receipt1 .amount" "100.00"
          htmlAnyContain "#receipt1 .glAccount" "7501"
          htmlAnyContain "#receipt2 .amount" "50.00"
          htmlAnyContain "#receipt2 .glAccount" "8000"

        it "displays an error if a header line is not fulled" $ do
          return pending
	  get GLEnterReceiptSheetR
          statusIs 200
          -- we try here the text area form instead of file  because it's
          -- easier to setup and read

          request $ do
            setMethod "POST"
            setUrl GLEnterReceiptSheetR
            addToken_ "form#text-form " --"" "#text-form"
            byLabel "Sheet name" "test 2"
            byLabel "Receipts" [st|date,counterparty,bank account,total,gl account,amount,tax rate
2015/01/02,,B1,120,7501,100,20%|]
        

          statusIs 422 -- @todo wrong
          bodyContains "Counterparty is missing"

        it "display receipt header" (const pending)
        it "needs at least one header" (const pending)
       


storiesSpec :: Spec 
storiesSpec =  withApp $ do
  describe "upload a file without error" $ do
    {-
      The user upload a spreadsheet.
      The result of the conversion to receipt events is displayed
      as well as the corresponding row of the spreadsheet
      The user can then validate it and save it
    
      - For that we need one page proposing to upload a file
        - getGLEnterReceiptSheetR
      - another one displaying the result
        - postGLEnterReceiptSheetR
      - another one display errors
        - postGLEnterReceiptSheetR
 
    -}
    it "story to write" (const pending)
pureSpec :: Spec
pureSpec = do
{-
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
-}
    context "should parse amounts" $ do
      it "without currency" $ do
        assertUtf8Field "23.45" (23.45 :: Currency)
      it "without currency and spaces" $ do
        assertUtf8Field " 23.45 " (23.45 :: Currency)
      it "with currency" $ do
        assertUtf8Field "£23.45" (23.45 :: Currency)
      it "negative with currency" $ do
        assertUtf8Field "-£23.45" (-23.45 :: Currency)
      it "negative with currency" $ do
        assertUtf8Field "£-23.45" (-23.45 :: Currency)

 {-
      it "with currency and spaces" $ do
        assertUtf8Field " £23.45 " (23.45 :: Currency)
      it "with currency and spaces between the currency" $ do
        assertUtf8Field "£ 23.45 " (23.45 :: Currency)
      it "negative with currency and spaces between the currency" $ do
        assertUtf8Field " - £ 23.45 " (-23.45 :: Currency)
    

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


-}
assertField str value  = decode NoHeader (str <> ",") `shouldBe` Right (fromList [(value, ())])
-- we need  to encode bystestring in UTF8 because the IsString instance for bytestring
-- doesn't encode £ to two words but only one.
assertUtf8Field text value = assertField (encodeUtf8 text) value

