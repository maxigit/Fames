module WH.BarcodeSpec where

import TestImport
import WH.Barcode

spec :: Spec
spec = pureSpec >> appSpec


pureSpec =  return ()

appSpec = withAppWipe BypassAuth $ do
  describe "#Barcodes" $ do
    it "create new prefix in the database sequence" $ do
      barcodes <- runDB $ generateBarcodes "TS" (Just $ fromGregorian 2016 11 02) 100
      liftIO $ do
        headEx barcodes `shouldBe` "TS16DE000001A"
        lastEx barcodes `shouldBe` "TS16DE000100A"

    it "start from the last used barcodes" $ do
      runDB $ generateBarcodes "TS" (Just $ fromGregorian 2016 11 02) 100
      barcodes <- runDB $ generateBarcodes "TS" (Just $ fromGregorian 2016 11 02) 100
      liftIO $ do
        headEx barcodes `shouldBe` "TS16DE000101A"
        lastEx barcodes `shouldBe` "TS16DE000200A"

    it "rolls to the next date " $ do
      runDB $ generateBarcodes "TS" (Just $ fromGregorian 2016 11 02) 99950
      barcodes <- runDB $ generateBarcodes "TS" (Just $ fromGregorian 2016 11 02) 100
      liftIO $ do
        headEx barcodes `shouldBe` "TS16DE999951A"
        lastEx barcodes `shouldBe` "TS17JA000050A"
        

