module Handler.WHBarcodeSpec (spec) where

import TestImport
import Handler.WH.Barcode

spec :: Spec
spec = pureSpec >> appSpec


pureSpec =  return ()

appSpec = withAppWipe BypassAuth $ do
  describe "@Barcodes" $ do
    parallel $ context "Utils" $ do
        it "create new prefix in the database sequence" $ do
          barcodes <- runDB $ generateBarcodes "TA" (Just $ fromGregorian 2016 11 02) 100
          liftIO $ do
            headEx barcodes `shouldBe` "TA16NV00001R"
            lastEx barcodes `shouldBe` "TA16NV00100I"

        it "start from the last used barcodes" $ do
          runDB $ generateBarcodes "TB" (Just $ fromGregorian 2016 11 02) 100
          barcodes <- runDB $ generateBarcodes "TB" (Just $ fromGregorian 2016 11 02) 100
          liftIO $ do
            headEx barcodes `shouldBe` "TB16NV00101V"
            lastEx barcodes `shouldBe` "TB16NV00200M"

        it "rolls to the next date" $ do
          runDB $ generateBarcodes "SC" (Just $ fromGregorian 2016 12 02) 99950
          barcodes <- runDB $ generateBarcodes "SC" (Just $ fromGregorian 2016 12 02) 100
          liftIO $ do
            headEx barcodes `shouldBe` "SC16DE99951L"
            lastEx barcodes `shouldBe` "SC17JA00051H"

        it "rolls over the next next month" $ do
          runDB $ generateBarcodes "RD" (Just $ fromGregorian 2016 11 02) 99950
          runDB $ generateBarcodes "RD" (Just $ fromGregorian 2016 12 02) 99999
          barcodes <- runDB $ generateBarcodes "RD" (Just $ fromGregorian 2016 11 02) 100
          liftIO $ do
            headEx barcodes `shouldBe` "RD16NV99951J"
            lastEx barcodes `shouldBe` "RD17JA00051F"
