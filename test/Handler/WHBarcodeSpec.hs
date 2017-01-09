module Handler.WHBarcodeSpec where

import TestImport
import Handler.WH.Barcode

spec :: Spec
spec = pureSpec >> appSpec


pureSpec =  return ()

appSpec = withAppWipe BypassAuth $ do
  describe "@Barcodes" $ do
    context "Utils" $ do
        it "create new prefix in the database sequence" $ do
          barcodes <- runDB $ generateBarcodes "TS" (Just $ fromGregorian 2016 11 02) 100
          liftIO $ do
            headEx barcodes `shouldBe` "TS16NV00001V"
            lastEx barcodes `shouldBe` "TS16NV00100M"

        it "start from the last used barcodes" $ do
          runDB $ generateBarcodes "TS" (Just $ fromGregorian 2016 11 02) 100
          barcodes <- runDB $ generateBarcodes "TS" (Just $ fromGregorian 2016 11 02) 100
          liftIO $ do
            headEx barcodes `shouldBe` "TS16NV00101T"
            lastEx barcodes `shouldBe` "TS16NV00200K"

        it "rolls to the next date" $ do
          runDB $ generateBarcodes "TS" (Just $ fromGregorian 2016 12 02) 99950
          barcodes <- runDB $ generateBarcodes "TS" (Just $ fromGregorian 2016 12 02) 100
          liftIO $ do
            headEx barcodes `shouldBe` "TS16DE99951L"
            lastEx barcodes `shouldBe` "TS17JA00051H"

        it "rolls over the next next month" $ do
          runDB $ generateBarcodes "TS" (Just $ fromGregorian 2016 11 02) 99950
          runDB $ generateBarcodes "TS" (Just $ fromGregorian 2016 12 02) 99999
          barcodes <- runDB $ generateBarcodes "TS" (Just $ fromGregorian 2016 11 02) 100
          liftIO $ do
            headEx barcodes `shouldBe` "TS16NV99951L"
            lastEx barcodes `shouldBe` "TS17JA00051H"
