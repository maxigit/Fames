module GL.ReceiptProcessingSpec (spec) where

import TestImport
import qualified GL.FA as FA
import GL.Receipt

spec :: Spec

spec = describe "#Receipt translator" $ do
  it "Generates a simple payment with tax type" $ do
    let receipt =  Receipt [ReceiptItem 7501 100 (TaxType 0.20 2200)]
    translate receipt `shouldBe`
      FA.Payment
        [ FA.PaymentItem 7501 100.00 Nothing Nothing Nothing Nothing
        , FA.PaymentItem 2200 20.00 (Just 100) Nothing Nothing Nothing
        ]
  it "Aggregates similar tax types" $ do
    let receipt =  Receipt [ReceiptItem 7501 100 r20 , ReceiptItem 8000 50 r20]
        r20 = TaxType (20/100) 2200
    translate receipt `shouldBe`
      FA.Payment
        [ FA.PaymentItem 7501 100.00 Nothing Nothing Nothing Nothing
        , FA.PaymentItem 8000 50.00 Nothing Nothing Nothing Nothing
        , FA.PaymentItem 2200 30.00 (Just 150) Nothing Nothing Nothing
        ]

  it "" $ pendingWith "Add company and all info needed by FA"
  it "Generates a simple payment without tax type" $ do
    pending
