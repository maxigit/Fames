{-# LANGUAGE OverloadedStrings #-}
module GL.ReceiptProcessingSpec (spec) where

import TestImport
import qualified GL.FA as FA
import GL.Receipt
import Data.Yaml(decodeEither')

spec :: Spec
spec = paymentSpecs >> jsonSpecs

decodeEither = first show . decodeEither'
paymentSpecs = parallel $ describe "@parallel @Receipt #Receipt translator" $ do
  it "Generates a simple payment with tax type" $ do
    let receipt =  Receipt "" "" "" 0  [ReceiptItem 7501 100 (TaxType 0.20 2200)]
    translate receipt `shouldBe`
      FA.Payment
        [ FA.PaymentItem 7501 100.00 Nothing Nothing Nothing Nothing
        , FA.PaymentItem 2200 20.00 (Just 100) Nothing Nothing Nothing
        ]
  it "Aggregates similar tax types" $ do
    let receipt =  Receipt "" "" "" 0 [ReceiptItem 7501 100 r20 , ReceiptItem 8000 50 r20]
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


jsonSpecs = parallel $ describe "@parallel @Receipt #parseJSON" $ do
  let rightR x = Right (x :: ReceiptTemplate)
  it "parses counterparty setter " $ do
     decodeEither "counterparty: A" `shouldBe` rightR (CounterpartySetter "A")
  it "parses bank account setter " $ do
     decodeEither "bank: B" `shouldBe` rightR (BankAccountSetter "B")
  it "parses multiple setter as hash in Alpha order" $ do
     decodeEither "counterparty: A\nbank: B" `shouldBe` (rightR $ CompoundTemplate [BankAccountSetter "B", CounterpartySetter "A"])
  it "parses multiple setter as hash in alpha order" $ do
     decodeEither "bank: B\ncounterparty: A" `shouldBe` (rightR $ CompoundTemplate [BankAccountSetter "B", CounterpartySetter "A"])
  it "parses multiple setter as list" $ do
     decodeEither "- counterparty: A\n- bank: B" `shouldBe` (rightR $ CompoundTemplate [CounterpartySetter "A", BankAccountSetter "B"])

  -- it "parses VAT" $ do
  --   -- decodeEither' "vat:\n  rate: 20\n  tax: VAT 20%" `shouldBe` Right (ItemVATDeducer $ FA.TaxRef 0.20 "VAT 20%")
  --   decodeEither' "vat:\n  rate: 20\n  tax: VAT 20%" `shouldBe` 
  --     (rightR $ CompoundTemplate [CounterpartySetter "A", BankAccountSetter "B"])

