{-# OPTIONS_GHC  -Wno-deprecations #-}
import Criterion.Main
-- import Criterion.Types
import BenchImport

import FA as FA hiding (unUserKey)
import Handler.Items.Category.Cache
import qualified Handler.Dashboard  as D
import qualified Handler.WH.Boxtake.Adjustment as BA
import qualified Handler.Items.Reports.NewForecast as NF
import qualified Handler.Items.Reports.Forecast as F

main :: IO ()
main = do
  let frozenDay = fromGregorian 2026 04 13
  -- let frozenDayPreviousYear = fromGregorian 2025 04 13
  app <- benchmarkFoundationWith [] id
  appWithCategory <- benchmarkFoundationWith ["../bench-data/category.yml"] id
  
  adjP_ <- h app BA.defaultAdjustmentParamH
  let adjP = adjP_ { BA.aDate = Just frozenDay
                   , BA.aStyleFilter = Just (LikeFilter "MX%") -- return nothing
                   }

  defaultMain
   [ bgroup "category" [ bench "load style category " $ nfIO . h appWithCategory $ categoryFinderCached "style" >>= (\f -> return $ traceShowId $ f (FA.StockMasterKey "Z"))
                       ]
   , bgroup "report" [ bench (unpack report) $ nfIO . h app  $ (D.reportDiv frozenDay report  >> return ())
                     | report <- ["top100ItemYear", "salesSlidingYearFull", "salesCurrentMonthFull20", "salesSlidingYearFull20" ]
                     ]
   , bgroup "boxes" [ bench "NOTHING adjustment" $ nfIO . h app $ ( runDB $ runConduit ( BA.loadAdjustementInfo adjP .| sinkNull ))
                    ]
   , bgroup "forecast" [ bench ""  $ nfIO . h app $ do
                                   let fparam = NF.defaultForecastParam -- { -- NF.fpStartDate = Just frozenDayPreviousYear }
                                   (_,summary) <- NF.getPlotForecastError fparam
                                                                        F.SkuGroup
                                                                        (fromGregorian 2025 02 18)
                                                                        "2025-02-18 Repeat"
                                   return $ show summary

                       ] 
   ]

  
