module GL.ReceiptProcessingSpec (spec) where

import TestImport
import qualified GL.FA as FA
import GL.Receipt

spec :: Spec

spec = describe "#Receipt translator" $ do
  it "Generates a simple payment with VAT" $ do
    let receipt =  Receipt 7501 100 (ratePercent 20) --  {amount = 100.00} 
    translate receipt `shouldBe`
      FA.Payment
        [ FA.PaymentItem 7501 100.00 Nothing Nothing Nothing Nothing
        , FA.PaymentItem 2200 20.00 (Just 100) Nothing Nothing Nothing
        ]
    

