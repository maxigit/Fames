{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes#-}
module Handler.GLEnterReceiptSheetSpec (spec) where

import TestImport

import Handler.CsvUtils
import Data.Csv (decode, HasHeader(NoHeader))

spec :: Spec
spec = describe "@GLReceipt" $ pureSpec

postReceiptSheet status sheet = do
  get (GLR GLEnterReceiptSheetR)
  statusIs 200

  request $ do
    setMethod "POST"
    setUrl (GLR GLEnterReceiptSheetR)
    addToken_ "form#text-form " --"" "#text-form"
    byLabelExact "Sheet name" "test 1"
    byLabelExact "Receipts" sheet

  -- printBody
  statusIs status

uploadReceiptSheet status path = do
  get (GLR GLEnterReceiptSheetR)
  statusIs 200

  request $ do
    setMethod "POST"
    setUrl (GLR GLEnterReceiptSheetR)
    addToken_ "form#upload-form "
    fileByLabelExact "upload" ("test/Handler/GLEnterReceiptSheetSpec/" ++ path) "text/plain"
    byLabelExact "encoding" (tshow $ 1) -- fromEnum encoding)

  -- printBody
  statusIs status

pureSpec :: Spec
pureSpec = do
  describe "@pure @parallel" $ do
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

 
assertField str value  = decode NoHeader (str <> ",") `shouldBe` Right (fromList [(value, ())])
-- we need  to encode bystestring in UTF8 because the IsString instance for bytestring
-- doesn't encode £ to two words but only one.
assertUtf8Field text value = assertField (encodeUtf8 text) value

