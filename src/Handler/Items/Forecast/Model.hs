{-# LANGUAGE OverloadedLabels, OverloadedRecordDot, TypeOperators #-}
{-# LANGUAGE TypeAbstractions #-}
module Handler.Items.Forecast.Model
where

import Import
import Items.Types
import Handler.Items.Sources
import qualified Handler.Items.Forecast.Model.Easy as Easy
import Handler.Items.Common
import Measure -- as M
-- import qualified Data.Map as Map
import GL.Utils
import qualified Database.Esqueleto.Experimental as E
import qualified Data.Conduit.List as C
import qualified Data.Foldable as F
import FA
import Data.Text (strip)
-- import Data.Coerce(coerce)
import qualified Data.NoDF as N
import qualified Data.Vector.Sized as S
import Data.NoDF hiding(Vector) -- (Wix(..), pattern Z3)
import Data.Time.Calendar

-- * Type
data ForecastModel
     = Naive { fmFrom, fmTo :: Day, fmDuration :: Maybe Int }
     -- | IM -- independant margins
     -- | CategorySplitter Category
     deriving (Show, Eq)

-- * Common
estimateSkuSpeedFromDir :: Day -> FilePath -> Handler (Either Text (Vector (Sku, YearlyQuantity)))
estimateSkuSpeedFromDir forecastDay forecastDir = do
    content' <- readFileUtf8 $ forecastDir </> "model.hs"
    let content = strip content'
    case readMay content of
       Nothing -> return $ Left $ "can't parse :\n" <> tshow content --  "No model.hs file present in " <> tshow forecastDir
       Just easy -> do
             estimation <- evaluateModel (modelFromEasy forecastDay easy)
             return $ Right estimation


-- * Model implementation

data LoadedData = LoadedData 
   { mdSales :: Vector (Day, Sku, Quantity)
   }
   deriving Show 

-- | Loaded data with some common computation already computer
-- Serves as sort of cache for filtering and grouping data needed
-- by different sub-models.
data ForecastData where
   ForecastData :: forall ( n :: Nat) . 
                   { fdQuantities__n :: S.Vector n Quantity
                   , fdDays__n :: N.Vector n Day
                   , fdSku__n :: N.Vector n Sku
                   , fdDays :: Map (Day, Day) (Wix Maybe n)
                   }
                   -> ForecastData
   
evaluateModel :: ForecastModel -> Handler (Vector (Sku, YearlyQuantity))
evaluateModel model = do
   loaded <- loadModelData model
   let datas = prepareData model loaded

   return $ estimateModel model datas


loadModelData :: ForecastModel -> Handler LoadedData
loadModelData model = do
   mdSales <- loadSales model
   return LoadedData{..}
   
   
loadSales :: ForecastModel -> Handler (Vector (Day, Sku, Quantity))
loadSales model = do
    stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
    case modelToSalesRange model of
       Nothing -> return mempty
       Just (start, end) -> do
            let query = do
                          tables <- itemSalesTables stockLike  emptyStockFilter True
                          let trans = E.getTable @DebtorTran tables
                              details = E.getTable @DebtorTransDetail tables
                          E.groupBy trans.tranDate
                          E.groupBy details.stockId 
                          -- E.orderBy [ E.asc trans.tranDate, E.asc details.stockId ]
                          E.where_  $ (trans.tranDate E.>=. E.val start)
                                    E.&&. (trans.tranDate E.<=. E.val end)
                          E.where_ $ E.notExists $ do
                                     cat <- E.from (E.table @CustomerCategory)
                                     E.where_ (E.just cat.customerId E.==. trans.debtorNo
                                              E.&&. cat.category  E.==. E.val "clearance"
                                              E.&&. cat.value E.==. E.val "Yes"
                                              )

                                   
                          return (trans.tranDate, details.stockId, E.sum_ $ salesDetailQuantity tables)
            salesv <- runDB $ runConduit $ E.selectSource query
                                         .| C.mapMaybe (\(E.Value day, E.Value sku, E.Value qtym)  -> fmap ((day, Sku sku,) . Measure) qtym)
                                         .| conduitVector 1000
                                         .| sinkList
            mapM traceShowM $ toList salesv
            return $ mconcat salesv

         
modelFromEasy :: Day -> Easy.Model -> ForecastModel
modelFromEasy forecastDay model = 
   traceShowId $ case model of
     Easy.Naive -> let to = calculateDate (AddDays $ -1) forecastDay
                       from = calculateDate (AddYears $ -1) forecastDay
                   in Naive from to Nothing
     Easy.PreviousYear n -> let to = calculateDate (AddDays $ -1) forecastDay
                                from = calculateDate (AddYears $ -n) forecastDay
                            in Naive from to (Just $ fromIntegral n)
          
          
  
   
modelToSalesRanges :: ForecastModel -> [ (Day, Day) ]
modelToSalesRanges model = let
  in case model of
       Naive from to _ -> [ (from, to) ]
modelToSalesRange :: ForecastModel -> Maybe (Day, Day)
modelToSalesRange model =
    case modelToSalesRanges model of
       [] -> Nothing
       ranges -> Just ( minimumEx $ map fst ranges
                      , maximumEx $ map snd ranges
                      )
       

       
 -- ==================================================
 --     PREPARE
 -- ==================================================

prepareData :: ForecastModel -> LoadedData -> ForecastData
prepareData model LoadedData{..} 
  | SomeSized (Z3 fdDays__n fdSku__n fdQuantities__n) <- mdSales
  = let fdDays = mapFromList [ (range, filterX (\d -> from <= d && d <= to) fdDays__n)
                             | range@(from, to) <- modelToSalesRanges model
                             ]
    in ForecastData{..}
prepareData _ _ = error "exhaustive pattern"
    
       
estimateModel :: ForecastModel -> ForecastData -> Vector (Sku, YearlyQuantity)
estimateModel Naive{..} fdata = estimateNaive fmFrom fmTo duration fdata
   where duration = maybe (fromIntegral (diffDays fmTo fmFrom) / 365)
                          fromIntegral
                          fmDuration
   
                              
    

estimateNaive :: Day -> Day -> Double -> ForecastData -> Vector (Sku, YearlyQuantity)
estimateNaive from to years ForecastData{..} = 
   case lookup (from, to) fdDays of
        Just (Wix dNnDD) | skus__d <- windex dNnDD @> fdSku__n
                         , quantities__d <- windex dNnDD @> fdQuantities__n
                         , Wal @_ @_ @s dSsDD <- groupV skus__d
                         -> let skus__sku = walues dSsDD @=> skus__d
                                qty__sku = F.sum <$> walues dSsDD @>$ quantities__d
                                yearFraction = S.replicate $ Measure years :: N.Vector s Years
                            in fromSized $ Z2 skus__sku (qty__sku ^/ yearFraction)
        _ -> mempty
       

