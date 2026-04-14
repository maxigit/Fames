{-# OPTIONS_GHC  -Wno-deprecations #-}
import Criterion.Main
-- import Criterion.Types
import BenchImport

import FA as FA hiding (unUserKey)
import Handler.Items.Category.Cache
import qualified Handler.Dashboard  as D
import qualified Handler.WH.Boxtake.Adjustment as BA

main :: IO ()
main = do
  let frozenDay = fromGregorian 2026 04 13
  app <- benchmarkFoundationWith [] id
  appWithCategory <- benchmarkFoundationWith ["../bench-data/category.yml"] id
  
  adjP_ <- h app BA.defaultAdjustmentParamH
  let adjP = adjP_ { BA.aDate = Just frozenDay }

  defaultMain
   [ bgroup "category" [ bench "load style category " $ nfIO . h appWithCategory $ categoryFinderCached "style" >>= (\f -> return $ traceShowId $ f (FA.StockMasterKey "Z"))
                       ]
   , bgroup "report" [ bench (unpack report) $ nfIO . h app  $ (D.reportDiv frozenDay report  >> return ())
                     | report <- ["top100ItemYear", "salesSlidingYearFull", "salesCurrentMonthFull20", "salesSlidingYearFull20" ]
                     ]
   , bgroup "boxes" [ bench "adjustment" $ nfIO . h app $ ( runDB $ runConduit ( BA.loadAdjustementInfo adjP .| sinkNull ))
                    ]
   ]

  
