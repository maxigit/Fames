module WH.BarcodeSpec where

import TestImport
import WH.Barcode

spec :: Spec
spec = pureSpec

pureSpec = do
  describe "@expandLocation" $ do
    it "expands character set" $ do
       expandLocation "[AC]" `shouldBe` ["A", "C"]
    it "expands ranges" $ do
       expandLocation "[A-C]" `shouldBe` ["A", "B", "C"]
    it "expand many times" $ do
       expandLocation "A0[123].[12]" `shouldBe` ["A01.1", "A02.1", "A03.1", "A01.2", "A02.2", "A03.2"]



       
