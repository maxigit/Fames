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
import Data.List(nub)
import Data.Coerce(coerce)
import qualified Data.NoDF as N
import Data.NoDF.Fold1(headm, head1, pattern Fold1, Fold1(..))
import qualified Data.Vector.Sized as S
import Data.NoDF hiding(Vector, index) -- (Wix(..), pattern Z3)
import Data.Time.Calendar
-- import Data.Finite
import Data.Type.Equality ((:~:)(..))

-- * Type
data ForecastModel
     = Naive { fmFrom, fmTo :: Day, fmDuration :: Maybe Int }
     -- | IM -- independant margins
     | CategorySplitter { fmCategory:: CategoryName 
                        , fmCategoryModel :: Map CategoryValue ForecastModel
                        , fmDefaultModel :: ForecastModel
                        }
     | Combination (Vector1 YearlyQuantity -> YearlyQuantity) [ForecastModel]
     | MonoOperation (Double -> Double) ForecastModel
     | NullModel
     -- deriving (Show, Eq)

newtype CategoryName = CategoryName { unCategoryName :: Text }  deriving (Show, Eq, Ord)
newtype CategoryValue = CategoryValue { unCategoryValue :: Text }  deriving (Show, Eq, Ord)

modelFromEasy :: Day -> Easy.Model -> ForecastModel
modelFromEasy forecastDay model = 
   case model of
     Easy.Naive -> let to = calculateDate (AddDays $ -1) forecastDay
                       from = calculateDate (AddYears $ -1) forecastDay
                   in Naive from to (Just 1)
     Easy.PreviousYears n -> let to = calculateDate (AddDays $ -1) forecastDay
                                 from = calculateDate (AddYears $ -n) forecastDay
                            in Naive from to (Just $ fromIntegral n)
     Easy.Previous from to durm -> Naive (day from) (day to) durm
     Easy.ForeachCategory catName defModel ->   CategorySplitter (CategoryName catName) mempty $ go defModel
     Easy.CategoryCase catName cat'models defModel -> CategorySplitter (CategoryName catName)
                                                                        (mapFromList [(CategoryValue cat, go model)
                                                                                     | (cat, model) <- cat'models
                                                                                     ]
                                                                        )
                                                                        (go defModel)
     Easy.FilterCategory catName categories model -> CategorySplitter (CategoryName catName)
                                                              (mapFromList $ [(CategoryValue cat, go model) | cat <- categories ])
                                                              NullModel
     Easy.ExcludeCategory catName categories model -> CategorySplitter (CategoryName catName)
                                                              (mapFromList $ [(CategoryValue cat, NullModel) | cat <- categories ])
                                                              (go model)
     Easy.Sum models -> Combination F.sum (map go models)
     Easy.Max models -> Combination F.maximum (map go models)
     Easy.Min models -> Combination F.minimum (map go models)
     Easy.Median models -> Combination (coerce . median . coerce) (map go models)
     Easy.Avg models -> let n = length models 
                            weight = 1 / fromIntegral n
                        in Combination F.sum $ map (MonoOperation (*weight) . go)  models
     Easy.Scale weight model -> MonoOperation (*weight) (go model)
     Easy.Cap cap model -> MonoOperation (min cap) (go model)
     Easy.Null -> NullModel
   where go = modelFromEasy forecastDay
         median :: Vector1 Double -> Double
         median (Fold1 v) = let sorted = sort v
                    in case length v of 
                            n | odd n -> indexEx sorted (n `div` 2) -- ex 3 -> 1   : 0 [1] 2
                            n -> let half = n `div` 2   --- 4 -> 2    0 [1 2] 3 
                                 in (indexEx sorted half + indexEx sorted (half-1)) / 2
         day d = case d of 
                 Easy.EasyDay d -> d
                 Easy.EasyCalc calc -> calculateDate calc forecastDay
          
          
  
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
   { ldData :: Vector (Day, Sku, Quantity)
   , ldCategories :: Vector (Sku, CategoryName, CategoryValue)
   }
   deriving Show 

-- | Loaded data with some common computation already computer
-- Serves as sort of cache for filtering and grouping data needed
-- by different sub-models.
data ForecastData where
   ForecastData :: forall ( n :: Nat) (sku::Nat) . (KnownNat n, KnownNat sku) => 
                   { fdQuantities__n :: S.Vector n Quantity
                   , fdDays__n :: N.Vector n Day
                   , fdSku__n :: N.Vector n Sku
                   , fdDays :: Map (Day, Day) (Wix Maybe n)
                   , fdSku__nSsNN :: N.WectorFF Vector1 n sku n
                   , fdCategoryMap :: Map CategoryName (N.Vector sku (Maybe CategoryValue))
                   }
                   -> ForecastData

-- unGroupForecastData :: WectorFF Maybe x__n v__g x__n -> ForecastData -> Vector v__g ForecastData  
-- unGroupForecastData nGgNN fdata = fmap (flip narrowForecastData fdata) (invertGroup nGnNN)


narrowForecastData :: KnownNat v__n => Wix Maybe v__n -> ForecastData -> ForecastData
narrowForecastData w@(Wix sNnSS) ForecastData{..}  
   | Wector _sN nSS <- sNnSS
   , Just Refl <-  sameNat (S.length' fdSku__n ) (S.length' nSS)
   , daysMap <- fmap (intersectWix  w) fdDays
   = ForecastData{fdDays = daysMap,..}
--    , quantities__s <- sN @> fdQuantities__n
--    , days__s <- sN @> fdDays__n
--    , sku__s <- sN @> fdSku__n
--    , daysMap <- fmap (filterX  w) fdDays
--    , sku_nSsSS' <- error "todo"
--    = ForecastData quantities__s
--                   days__s
--                   sku__s
--                   daysMap
--                   sku_nSsSS'
--                   mempty
--      

narrowForecastData _ _ = error "exhaustive pattern"

   
evaluateModel :: ForecastModel -> Handler (Vector (Sku, YearlyQuantity))
evaluateModel model = do
   loaded <- loadModelData model
   let datas = prepareData model loaded

   return $ estimateModel model datas

-- * Loading sales

loadModelData :: ForecastModel -> Handler LoadedData
loadModelData model = do
   ldData <- loadSales model
   ldCategories <- loadCategories model
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
            return $ mconcat salesv

         
   
modelToSalesRanges :: ForecastModel -> [ (Day, Day) ]
modelToSalesRanges model = let
  in case model of
       Naive from to _ -> [ (from, to) ]
       CategorySplitter _  modelMap defModel -> concatMap modelToSalesRanges (defModel : toList modelMap)
       Combination _ models -> concatMap modelToSalesRanges models
       MonoOperation _ model -> modelToSalesRanges model
       NullModel -> []

modelToSalesRange :: ForecastModel -> Maybe (Day, Day)
modelToSalesRange model =
    case modelToSalesRanges model of
       [] -> Nothing
       ranges -> Just ( minimumEx $ map fst ranges
                      , maximumEx $ map snd ranges
                      )
       

-- * Loading categories
loadCategories :: ForecastModel -> Handler (Vector (Sku, CategoryName, CategoryValue))
loadCategories model = do
  let categories = modelToCategories model
      query = do
               cat <- E.from $ E.table @ItemCategory
               E.where_ $ cat.category `E.in_` (E.valList $ coerce categories)
               return (cat.stockId , cat.category, cat.value)
  catvs <- runDB $ runConduit $ E.selectSource query
                             .| mapC ( \(E.Value s, E.Value n, E.Value v) -> (Sku s, CategoryName n, CategoryValue v) )
                             .| conduitVector 1000
                             .| sinkList
  return $ mconcat catvs





--
modelToCategories :: ForecastModel -> [CategoryName ]
modelToCategories model =
  case model of
    Naive{..} -> []
    CategorySplitter cat modelMap defModel -> nub $ sort $ cat : concatMap modelToCategories (defModel : toList modelMap)
    Combination _ models -> nub $ sort $ concatMap modelToCategories models
    MonoOperation _ model -> modelToCategories model
    NullModel -> []

       
 -- ==================================================
 --     PREPARE
 -- ==================================================

prepareData :: ForecastModel -> LoadedData -> ForecastData
prepareData model LoadedData{..} 
  | SomeSized (Z3 fdDays__n fdSku__n fdQuantities__n) <- ldData
  , fdDays <- mapFromList [ (range, filterX (\d -> from <= d && d <= to) fdDays__n)
                          | range@(from, to) <- modelToSalesRanges model
                          ]
  ------------------
  , JSpineV skuSpine fdSku__nSsNN <- makeJoinSpineV fdSku__n
  , SomeSized (Z3 sku__c category__c value__c) <- ldCategories
  , categoryMap <- pivotWithSpine (JoinSpine skuSpine fdSku__nSsNN) sku__c category__c 
  , fdCategoryMap <- fmap ((@>$ value__c) . fmap headm)
                          categoryMap
  -- , fdSku_nSsNN <- N.groupV fdSku__n 
  = ForecastData{..}
prepareData _ _ = error "exhaustive pattern"
    
       
estimateModel :: ForecastModel -> ForecastData -> Vector (Sku, YearlyQuantity)
estimateModel Naive{..} fdata = estimateNaive fmFrom fmTo duration fdata
   where duration = maybe (fromIntegral (diffDays fmTo fmFrom) / 365)
                          fromIntegral
                          fmDuration
estimateModel CategorySplitter{..} fd@ForecastData{..} =
  case lookup fmCategory fdCategoryMap of
       Just categorym__sku | categorym__n <- windex fdSku__nSsNN  @> categorym__sku
                           , Wal nCcNN <- groupV categorym__n
                           , fdv__c <- fmap (flip narrowForecastData fd) (invertGroup nCcNN)
                           ->  mconcat [ estimateModel model groupFd 
                                       | i__c <- S.toList $ S.generate id
                                       , let groupFd = S.index fdv__c i__c
                                       , let catn = S.index (walues nCcNN) i__c
                                       , let catm = head1 $ catn @> categorym__n
                                       , let model = fromMaybe fmDefaultModel $  catm >>= flip lookup fmCategoryModel
                                       ]
       Nothing -> mempty
   
estimateModel NullModel _ = mempty
estimateModel (Combination agg models) fdata
    | SomeSized (Z2 sku__n qty__n) <- concatMap (flip estimateModel fdata) models 
    , Wal nSsNN <- groupV sku__n
    , sku__s <- walues nSsNN @=> sku__n
    , qty__s <- agg <$> walues nSsNN @>$ qty__n
    = fromSized $ Z2 sku__s qty__s
   
estimateModel (Combination _ _ ) _ = error "exhaustive pattern"
                              
estimateModel (MonoOperation f model) fdata =
   let sku'qty = estimateModel model fdata
   in fmap (fmap (fmap f)) sku'qty

    

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
       

