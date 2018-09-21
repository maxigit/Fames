{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes#-}
module Legacy.BankReconciliateSpec where
import Prelude
import BankReconciliate

import Test.Hspec
import Text.Shakespeare.Text (lbt)
import           Data.Text.Lazy.Encoding (encodeUtf8)

import Text.Parsec(parse)
import Data.Time

parseLT p i = parse p "" (encodeUtf8 i)
spec = pureSpec
pureSpec = santanderSpec
santanderSpec = describe "@Santander" $ do
   describe "parser" $ do
     it "parses transaction" $ do
        let input = [lbt|
            |Date: 20/09/2018
            |Description: BANK GIRO CREDIT, New York
            |Amount: 39.50 	
            |Balance: 337.97 
            |]

        parseLT parseSTransaction input `shouldBe`
           Right (STransaction
                   (fromGregorian 2018 09 20)
                   "BANK GIRO CREDIT, New York"
                   39.50	
                   337.97 
                 )


