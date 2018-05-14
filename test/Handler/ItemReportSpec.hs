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
    let nmap1 = nmapFromList (Just "Level1")
                             [(mkNMapKey (PersistText key), tranQP QPSales (mkQPrice Inward q p))
                             | (key, q, p) <- [ ("A", 1, 4.5)
                                              , ("B", 7, 5) -- top 1
                                              , ("C", 2, 6)
                                              ]
                             ]
    let nmap1' = nmapFromList (Just "Level1")
                             [(mkNMapKey (PersistText key), tranQP QPSales (mkQPrice Inward q p))
                             | (key, q, p) <- [ ("A", 3, 5) -- top 1
                                              , ("C", 5, 2)
                                              , ("D", 2, 6)
                                              ]
                             ]
        nmap2 = NMap [Just "Level0", Just "Level1"] ( Map.fromList [ (mkNMapKey (PersistText key), m)
                                                                  | (key, m) <- [("X", nmap1), ("Y", nmap1')]
                                                                  ]
                                                   )
        top1 = ColumnRupture (Just (Column "sku" (const (mkNMapKey . PersistText . tkSku))))
                                (TraceParams QPSales (Identifiable ("Down", [TraceParam (qpAmount Outward, undefined, undefined)]))) 
                                (Just RMResidual)
                                (Just 1)
        expected1 = nmapFromList (Just "Level1")
                             [ (NMapKey (Just $ PersistInt64 1) (PersistText "B"), tranQP QPSales (mkQPrice Inward 7 5))
                             , (NMapKey (Just $ PersistInt64 2) (PersistText "Res-2"), tranQP QPSales (QPrice Inward 3 16.5 (MinMax 4.5 6) ))
                             ]
        expected1' = nmapFromList (Just "Level1")
                             [ (NMapKey (Just $ PersistInt64 1) (PersistText "A"), tranQP QPSales (mkQPrice Inward 3 5))
                             , (NMapKey (Just $ PersistInt64 2) (PersistText "Res-2"), tranQP QPSales (QPrice Inward 7 22 (MinMax 2 6) ))
                             ]
    it "should aggregate bests with the same number levels (1-level) " $ do
       sortAndLimit [top1] nmap1  `shouldBe` expected1 
    it "should aggregate bests with the same number levels " $ do
       sortAndLimit [top1] nmap1'  `shouldBe` expected1' 
    it "should aggregate bests with the same number levels (2-level) " $ do
      pendingWith "Bug "
      -- at the moment, limit and sorting is made recursively, meaning sub group
      -- are sorted and limited before their parent. This makes it non idemptotent.
      -- are sortAndLimit actually call it twice, one on the subgroup and once for the "margin"
      -- the margin of the children might end up being sorted and appear first

       -- let expected = NMap [Just "Level0", Just "Level1"]
       --                     ( Map.fromList [ (NMapKey (Just $ PersistInt64 1) (PersistText "X"), expected1) 
       --                                    , (NMapKey (Just $ PersistInt64 2) (PersistText "Res-2"), expected1')
       --                                    ]
       --                     )
       
       -- -- nmapToList  (sortAndLimit [top1,top1] nmap2)  `shouldBe` [] --  expected 
       -- (sortAndLimit [top1,top1] nmap2)  `shouldBe` expected 

         
