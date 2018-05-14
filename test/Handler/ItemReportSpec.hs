{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ItemReportSpec where

import TestImport
import Handler.Items.Reports.Common
import Items.Types

import qualified Data.Map as Map

spec :: Spec
spec = parallel pureSpec


pureSpec :: Spec
pureSpec = describe "@Report @parallel @pure" $ do
  describe "sortAndLimit" $ do
    let nmap2 = nmapFromList (Just "Level1")
                             [(mkNMapKey (PersistText key), tranQP QPSales (mkQPrice Inward q p))
                             | (key, q, p) <- [ ("A", 1, 4.5)
                                              , ("B", 7, 5)
                                              , ("C", 2, 6)
                                              ]
                             ]
    it "should aggregate bests with the same number levels " $ do
       let expected = nmapFromList (Just "Level1")
                             [ (NMapKey (Just $ PersistInt64 1) (PersistText "B"), tranQP QPSales (mkQPrice Inward 7 5))
                             , (NMapKey (Just $ PersistInt64 2) (PersistText "Res-2"), tranQP QPSales (QPrice Inward 3 16.5 (MinMax 4.5 6) ))
                             ]
           top1 = ColumnRupture (Just (Column "sku" (const (mkNMapKey . PersistText . tkSku))))
                                (TraceParams QPSales (Identifiable ("Down", [TraceParam (qpAmount Outward, undefined, undefined)]))) 
                                (Just RMResidual)
                                (Just 1)
       
       sortAndLimit [top1] nmap2  `shouldBe` expected 

         
