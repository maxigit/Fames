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
     it "parses date1" $ do
       readTimeE ["%e/%m/%Y"] "20/09/2017" `shouldBe` Right (fromGregorian 2017 09 20)
     it "parses date2" $ do
       readTimeE ["%e/%m/%Y"] "01/09/2017" `shouldBe` Right (fromGregorian 2017 09 01)
     it "parses date3" $ do
       readTimeE ["%Y-%m-%d"] "2017-09-20" `shouldBe` Right (fromGregorian 2017 09 20)
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
     it "parses statement" $ do

       let input = [lbt|
                       |From: 20/03/2018 to 20/09/2018
                       |							
                       |Account: XXXX XXXX XXXX 1234
                       |						
                       |Date: 20/09/2018
                       |Description: BANK GIRO CREDIT , New York                                            
                       |Amount: 139.50 	
                       |Balance: 337.97 
                       |						
                       |Date: 19/09/2018
                       |Description: DUALITY 150910, Boston                                            
                       |Amount: 773.00 	
                       |Balance: 198.47 
                       |]
           trans = [ STransaction
                         (fromGregorian 2018 09 20)
                         "BANK GIRO CREDIT , New York"
                         139.5	
                         337.97 
                   , STransaction
                         (fromGregorian 2018 09 19)
                         "DUALITY 150910, Boston"
                         773.00
                         198.47
                  ]
       parseLT parseSStatement input `shouldBe`
               Right (SStatement
                       (fromGregorian 2018 03 20)
                       (fromGregorian 2018 09 20)
                       "XXXX XXXX XXXX 1234"
                       trans
                     )

