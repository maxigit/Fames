{-# LANGUAGE OverloadedLabels, OverloadedRecordDot, TypeOperators #-}
module Handler.Items.Forecast.Model
where

import Import
import Items.Types
import Handler.Items.Sources
import Handler.Items.Common
import Measure -- as M
import qualified Data.Map as Map
import GL.Utils
import qualified Database.Esqueleto.Experimental as E
import qualified Data.Conduit.List as C
import FA
import Data.Text (strip)
import Data.Coerce(coerce)

-- * Type
data ForecastModel
     = Naive 
     -- | IM -- independant margins
     -- | CategorySplitter Category
     deriving (Show, Read, Eq)
     
-- * Common
estimateSkuSpeedFromDir :: Day -> FilePath -> Handler (Either Text (Vector (Sku, YearlyQuantity)))
estimateSkuSpeedFromDir forecastDay forecastDir = do
    content' <- readFileUtf8 $ forecastDir </> "model.hs"
    let content = strip content'
    case readMay content of
       Nothing -> return $ Left $ "can't parse :\n" <> tshow content --  "No model.hs file present in " <> tshow forecastDir
       Just model -> do
             estimation <- evaluateModel forecastDay model
             return $ Right estimation


-- * Model implementation

data ForecastData = ForecastData 
   { mdSales :: Map (Day, Day) (Vector (Sku, Quantity))
   }
   deriving Show 

evaluateModel :: Day -> ForecastModel -> Handler (Vector (Sku, YearlyQuantity))
evaluateModel forecastDay model = do
   datas <- loadModelData forecastDay model
   return $ estimateModel model datas


loadModelData :: Day -> ForecastModel -> Handler ForecastData
loadModelData forecastDay model = do
   mdSales <- loadSales forecastDay model
   return ForecastData{..}
   
   
loadSales :: Day -> ForecastModel -> Handler (Map (Day, Day) (Vector (Sku, Quantity)))
loadSales forecastDay model = do
    stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
    case modelToSalesRange forecastDay model of
       Nothing -> return mempty
       Just (start, end) -> do
            let query = do
                          tables <- itemSalesTables stockLike  emptyStockFilter True
                          let trans = E.getTable @DebtorTran tables
                              details = E.getTable @DebtorTransDetail tables
                          -- E.groupBy trans.tranDate
                          E.groupBy details.stockId -- trans.tranDate
                          -- E.orderBy [ E.asc trans.tranDate ]
                          -- return (trans.tranDate, salesDetailQuantity)
                          E.where_  $ (trans.tranDate E.>=. E.val start)
                                    E.&&. (trans.tranDate E.<=. E.val end)
                                   
                          return (details.stockId, E.sum_ $ salesDetailQuantity tables)
            salesv <- runDB $ runConduit $ E.selectSource query
                                         .| C.mapMaybe (\(E.Value sku, E.Value qtym)  -> fmap ((Sku sku,) . Measure) qtym)
                                         .| conduitVector 1000
                                         .| sinkList
            return $ Map.singleton (start, end) (mconcat salesv)

         
          
          
  
   
modelToSalesRange :: Day -> ForecastModel -> Maybe (Day, Day)
modelToSalesRange forecastDay model = let
  end = calculateDate (AddDays (-1)) forecastDay
  start = calculateDate (AddYears (-1)) forecastDay
  in case model of
       Naive -> Just (start, end)
       
       
estimateModel :: ForecastModel -> ForecastData -> Vector (Sku, YearlyQuantity)
estimateModel Naive mdata = estimateNaive mdata

estimateNaive :: ForecastData -> Vector (Sku, YearlyQuantity)
estimateNaive mdata = 
   case toList (mdSales mdata) of
       s'q:_ -> coerce s'q
       _ -> mempty
       

