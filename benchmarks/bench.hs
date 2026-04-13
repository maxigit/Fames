{-# OPTIONS_GHC  -Wno-deprecations #-}
import Criterion.Main
-- import Criterion.Types
import BenchImport

import FA as FA hiding (unUserKey)
import Handler.Items.Category.Cache
import qualified Handler.Dashboard  as D

main :: IO ()
main = do
  let frozenDay = fromGregorian 2026 04 13
  handler <- benchHandlerWithH [] id
  handlerWithCategory <- benchHandlerWithH ["../bench-data/category.yml"] id
  let nfH = nfIO . handler 
  defaultMain
   [ bgroup "category" [ bench "load style category " $ nfIO . handlerWithCategory $ categoryFinderCached "style" >>= (\f -> return $ traceShowId $ f (FA.StockMasterKey "Z"))
                       ]
   , bgroup "report" [ bench (unpack report) $ nfH $ (D.reportDiv frozenDay report  >> return ())
                     | report <- ["top100ItemYear", "salesSlidingYearFull", "salesCurrentMonthFull20", "salesSlidingYearFull20" ]
                     ]
   ]
