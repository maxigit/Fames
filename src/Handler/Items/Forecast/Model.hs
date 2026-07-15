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
import Text.Printf(printf)
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy as LT
import Data.List(nub)
import Data.Coerce(coerce)
import qualified Data.NoDF as N
import Data.NoDF.Fold1(headm, head1, pattern Fold1, Fold1(..))
import qualified Data.Vector.Sized as S
import Data.NoDF hiding(Vector, index) -- (Wix(..), pattern Z3)
import Data.Time.Calendar
-- import Data.Finite
import Data.Type.Equality ((:~:)(..))
import Data.Foldable1(intercalate1)

-- * Type
data ForecastModel
     = Naive { fmFrom, fmTo :: Day, fmDuration :: Maybe Int }
     -- | IM -- independant margins
     | CategorySplitter { fmCategory:: CategoryName 
                        , fmCategoryModel :: Map CategoryValue ForecastModel
                        , fmDefaultModel :: ForecastModel
                        }
     | Combination (Vector1 YearlyQuantity -> YearlyQuantity) Text [ForecastModel] -- zip models
     | MonoOperation (Double -> Double) Text ForecastModel
     | IndependantMargins ForecastModel
     | Hierachical { fmCategories :: [CategoryName]
                   -- , fmSimple :: Bool 
                   , fmTopModel :: ForecastModel
                   , fmBaseModel :: ForecastModel 
                   }
       -- ^ Computes forecast using model and then scale it so that each categories product add up to the sum of top model forecast
     | Aggregate (Vector YearlyQuantity -> YearlyQuantity) Text ForecastModel -- broadcast one value to all others
     | NullModel
     -- deriving (Show, Eq)

instance Show ForecastModel where
   show (Naive from to prev) = unwords ["Naive " , show from , show to, show prev ]
   show (CategorySplitter cat models def) = unwords [ "CategorySplititer", show cat, show models, show def]
   show (Combination _ ann models) = unwords ["Combination", unpack ann, show models  ]
   show (MonoOperation _ ann model) = unwords ["MonoOperation", unpack ann, show model ]
   show (IndependantMargins model) = unwords ["IndependantMargins", show model ]
   show (Aggregate _ ann model) = unwords ["Aggregate", unpack ann, show model ]
   show (Hierachical cats top base) = unwords ["Hierachical ", "(", show top, ")", show cats, "(", show base, ")" ]
   show NullModel = "NullModel"
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
     Easy.Sum models -> Combination F.sum "SUM" (map go models)
     Easy.Max models -> Combination F.maximum "MAX" (map go models)
     Easy.Min models -> Combination F.minimum "MIN" (map go models)
     Easy.Median models -> Combination (coerce . median . coerce) "MEDIAN" (map go models)
     Easy.Avg models -> let n = length models 
                            weight = 1 / fromIntegral n
                        in Combination F.sum "SUM(avg)" $ map (go . Easy.Scale weight)  models
     Easy.Scale weight model -> MonoOperation (*weight) (pack $ printf "Scale %0.2f *" weight) (go model)
     Easy.AtMost cap model -> MonoOperation (min cap) (pack $ printf "AtMost %0.2f &" cap) (go model)
     Easy.AtLeast floor_ model -> MonoOperation (max floor_) (pack $ printf "AtLeast %0.2f &" floor_) (go model)
     Easy.SetTo value model -> MonoOperation (const value) (pack $ printf "SetTo %0.2f &" value) (go model)
     Easy.IM model -> IndependantMargins (go model)
     Easy.HM model -> go $ Easy.ScaleBy ["style"] model (Easy.ForeachCategory "colour" $ Easy.Total model)
     Easy.Total model -> Aggregate F.sum "SUM" (go model)
     Easy.Mean model -> Aggregate (\v -> let l = fromIntegral (F.length v)
                                         in fmap (/l) (F.sum v))
                                  "MEAN" (go model)
     Easy.ScaleBy cats top base -> Hierachical (map CategoryName cats) (go top) (go base)
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
estimateSkuSpeedFromDir :: Day -> FilePath -> Handler (Either Text (Vector (Sku, YearlyQuantity, Text)))
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

   
evaluateModel :: ForecastModel -> Handler (Vector (Sku, YearlyQuantity, Text))
evaluateModel model = do
   loaded <- loadModelData model
   let datas = prepareData model loaded

   return $ fmap (\(sku, qty, comment) -> (sku, qty, LT.toStrict $ LTB.toLazyText comment)) $ estimateModel model datas

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
                          E.where_ $ details.stockId =%/. (RegexFilter "^[MC]")  -- to match actualSalesSources
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
       Combination _ _ models -> concatMap modelToSalesRanges models
       MonoOperation _ _ model -> modelToSalesRanges model
       NullModel -> []
       IndependantMargins model -> modelToSalesRanges model
       Aggregate _ _ model -> modelToSalesRanges model
       Hierachical _ top base -> concatMap modelToSalesRanges  [top, base]

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
    Combination _ _ models -> nub $ sort $ concatMap modelToCategories models
    MonoOperation _ _ model -> modelToCategories model
    NullModel -> []
    IndependantMargins model -> map CategoryName ["style", "colour"] ++ modelToCategories model
    Aggregate _ _ model -> modelToCategories model
    Hierachical cats top base -> cats <> concatMap modelToCategories [top, base]

       
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
    
       
estimateModel :: ForecastModel -> ForecastData -> Vector (Sku, YearlyQuantity, TextBuilder)
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
estimateModel (Combination agg aggName models) fdata
    | SomeSized (Z3 sku__n qty__n comment__n) <- concatMap (flip estimateModel fdata) models 
    , Wal nSsNN <- groupV sku__n
    , sku__s <- walues nSsNN @=> sku__n
    , qty__s <- agg <$> walues nSsNN @>$ qty__n
    , comment__s <- fmap (\nn -> mconcat $ LTB.fromText aggName : ":"
                                         : [ intercalate1 (LTB.singleton ' ' )
                                           (fmap (\n -> LTB.fromString "("
                                                       <> fromMeasure (S.index qty__n n)
                                                       <> "={" <> S.index comment__n n <> "}"
                                                ) nn
                                           )
                                           ]
                              
        
                         ) 
                         (walues nSsNN)

    = fromSized $ Z3 sku__s qty__s comment__s
   
-- estimateModel (Combination _ _ _ ) _ = error "exhaustive pattern"
                              
estimateModel (MonoOperation f name model ) fdata 
   | SomeSized (Z3 sku qty comment) <- estimateModel model fdata
   = fromSized (Z3 sku (fmap f <$> qty) (S.zipWith annotate qty comment))
   where annotate q c = LTB.fromText name <> " " <> fromMeasure q <> "=(" <> c <> ")"

    
estimateModel (IndependantMargins model) fdata@ForecastData{..} 
    | SomeSized (Z3 sku__e qty__e __comment__e) <- estimateModel model fdata
    , JoinSpineV skuSpine__e_k <- makeJoinSpineV sku__e -- k are unique skus found from estimateModel. TODO estimateModel should only return uninque sku
    , let sku__sku = walues fdSku__nSsNN @=> fdSku__n
    , eKkSS <- rejoin skuSpine__e_k sku__sku
    -- join with style, qty
    , Just style__sku <- lookup (CategoryName "style") fdCategoryMap --
    , stylevm_e <- wbroadcast eKkSS @>$ style__sku -- might be null
    , style__e <- join . headm <$> stylevm_e
    , Wal eTtEE <- groupV style__e
    , qty__t <- F.sum <$> walues eTtEE @>$ qty__e
    -- k -> e -> t  :: k -> 
    -- , styleQty__k <- F.sum <$> walues (jsGrouping skuSpine__e_k) @>$ (windex eTtEE @> qty__t)
    -- join with var, qty
    , Just var__sku <- lookup (CategoryName "colour") fdCategoryMap --
    , varvm_e <- wbroadcast eKkSS @>$ var__sku -- might be null
    , var__e <- join . headm <$> varvm_e
    , Wal eVvEE <- groupV var__e
    , qty__v <- F.sum <$> walues eVvEE @>$ qty__e
    -- k -> e -> t  :: k -> 
    -- , varQty__k <- F.sum <$> walues (jsGrouping skuSpine__e_k) @>$ (windex eVvEE @> qty__v)
    , let total = F.sum qty__e
    , im__e <- S.generate \e -> S.index qty__t (S.index (windex eTtEE) e)
                             *^ S.index qty__v (S.index (windex eVvEE) e)
                             ^/ total
    , com__e <- S.generate \e -> LTB.fromText "IM: "
                                 <> fromMeasure (S.index qty__t (S.index (windex eTtEE) e))
                                 <> LTB.fromText ("=" <> maybe "Style" unCategoryValue (S.index style__e e) <>  " * ")
                                 <> fromMeasure (S.index qty__v (S.index (windex eVvEE) e))
                                 <> LTB.fromText ("=" <> maybe "Colour" unCategoryValue (S.index var__e e) <> " ")
                                 <> fromMeasure total
                                 <> "=Total"
    = fromSized (Z3 sku__e im__e com__e)
-- estimateModel (IndependantMargins _) _ = error "exhaustive pattern"
estimateModel (Aggregate agg ann model) fdata
    | SomeSized (Z3 sku qty0 comment0) <- estimateModel model fdata
    , let qty = S.replicate $ agg $ fromSized qty0
          comment = fmap (\c -> "AGG" <> LTB.fromText ann <> "): [" <> c <> "]") comment0
    = fromSized (Z3 sku qty comment)
estimateModel (Hierachical cats top base) fdata@ForecastData{..}
   | SomeSized top__t <- estimateModel top fdata
   , Z3 sku__t qty__t __comment__t <- top__t
   , SomeSized base__b <- estimateModel base fdata
   , Z3 sku__b __qty__b __comment__b <- base__b
   -- for each group defined by the categorsie
   -- we need to collect the base , sum up the top and scale so that SUM of base' = SUM top
   -- we use as a spine the categorie-value combination
   , let sku__sku = walues fdSku__nSsNN @=> fdSku__n
         cats__sku = S.generate \sku -> [ lookup catname fdCategoryMap >>= flip S.index sku
                                        | catname <- cats
                                        ]
   -------------------- join top
   , JoinSpineV skuSpine__sku__s@(JoinSpine _s1 __skuSsSkuz) <- makeJoinSpineV sku__sku
   , Wal skuCcSkus <- groupV cats__sku -- group sku by cat values
     -- we need to group t (and b) by C so tCcTT and bCcBB (
   , skuSsTT <- rejoin skuSpine__sku__s sku__t
     -- get for each cats the sum
   , cTT <- (foldMap unFold1) <$> walues skuCcSkus @>~ wbroadcast skuSsTT 
   , topQty__c <- F.sum <$> cTT @>$ qty__t
   -------------------- join base
   , skuSsBB <- rejoin skuSpine__sku__s sku__b
   , cBB <- (foldMap unFold1) <$> walues skuCcSkus @>~ wbroadcast skuSsBB
   -- , baseQty__c <- F.sum <$> cBB @>$ qty__b
   -- for each category comb, explan skus 
   -- , xx <- S.zipWith3 (\bs topQty baseQty -> let sku = bs @> sku__b
   --                                               -- qty = fmap (*adjust) <$> bs @> qty__b 
   --                                               qty = bs @> qty__b 
   --                                               adjust = topQty / baseQty
   --                                               c = "(" <> fromMeasure topQty <> "/" <> fromMeasure baseQty <> "*"
   --                                               comment = (<> c)  <$> bs @> comment__b
   --                                           in ( sku, qty, comment)
   --              
   --                    )
   --                    cBB
   --                    baseQty__c
   --                    topQty__c
   -- , tCcTT <- rejoin catsSpine__c_sku sku__t
   = F.foldMap (\(bs, to) -> scaleTo to (bs @> base__b)) $ Z2 cBB topQty__c







estimateModel model  _  = error $ "exthaustive pattern for " <> show model

scaleTo :: YearlyQuantity -> Vector (Sku, YearlyQuantity, TextBuilder) -> Vector (Sku, YearlyQuantity, TextBuilder)
scaleTo to v = let
   (sku, qty, comment0) = unzip3 v
   total = F.sum qty
   weight = to ^/ total
   comment = zipWith (\q c -> "(" <> fromMeasure to <> "*" <> fromMeasure (q ^/ total) <> "=(" <> fromMeasure q  <> " / " <> fromMeasure total <> ")"
                 <> ": " <> c
                  ) qty
                  comment0
   in if total == 0
      then mempty 
      else zip3 sku ((^* weight) <$> qty) comment
 
estimateNaive :: Day -> Day -> Double -> ForecastData -> Vector (Sku, YearlyQuantity, TextBuilder)
estimateNaive from to years ForecastData{..} = 
   case lookup (from, to) fdDays of
        Just (Wix dNnDD) | skus__d <- windex dNnDD @> fdSku__n
                         , quantities__d <- windex dNnDD @> fdQuantities__n
                         , Wal @_ @_ @s dSsDD <- groupV skus__d
                         -> let skus__sku = walues dSsDD @=> skus__d
                                qty__sku = F.sum <$> walues dSsDD @>$ quantities__d
                                yearFraction = S.replicate $ Measure years :: N.Vector s Years
                                comment__sku = S.replicate ( "Naive <"  <> fromString (show from) <> ">--<" <> fromString (show to) 
                                                         <> "> (*" <> fromMeasure (Measure years) <>")"
                                                    ) 
                            in fromSized $ Z3 skus__sku (qty__sku ^/ yearFraction) comment__sku
        _ -> mempty
       



fromMeasure :: Measure m -> TextBuilder
fromMeasure x = fromString $ (printf "%0.2f") (measured x)
