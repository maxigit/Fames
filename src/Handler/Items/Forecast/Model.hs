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
import qualified Data.Map as Map
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
import Data.NoDF.Fold1(headm, head1, pattern Fold1, Fold1(..), pattern AscU, AscU(unAscU))
import qualified Data.Vector.Sized as S
import qualified Data.Vector as V
import Data.Finite
import Data.NoDF hiding(Vector, index) -- (Wix(..), pattern Z3)
import Data.Time.Calendar
-- import Data.Finite
import Data.Type.Equality ((:~:)(..))
import Data.Foldable1(intercalate1)
import System.Directory(doesFileExist)

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
     | ReComment (TextBuilder -> TextBuilder) Text ForecastModel 
     | InjectCategory CategoryName [CategoryValue] -- ^ Inject all the sku of the given category with a forecast of 0
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
   show (ReComment _ ann model) = unwords ["ReComment", unpack ann, show model ]
   show (InjectCategory cat values) = unwords ["InjectCategory", show cat, show values ]
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
     Easy.NoveltyFromFuture years -> let future = calculateDateChain [AddYears years, AddDays (-1)] forecastDay
                                     in  ReComment (const "Novelty") "Novelty"
                                       $ MonoOperation (const 0) "0" $ Naive forecastDay future (Just years)
     Easy.InjectCategory cat -> InjectCategory (CategoryName cat) []
     Easy.InjectCategoryValue cat value -> InjectCategory (CategoryName cat) [CategoryValue value]
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
   { ldSales :: Vector (Day, Sku, Quantity)
   , ldOrders :: Vector (Day, Sku, Quantity)
   , ldCategories :: Vector (Sku, CategoryName, CategoryValue)
   }
   deriving Show 

data Source = Sales Day Day
            | InjectedCategory CategoryName [CategoryValue]
     deriving (Show, Eq, Ord)

-- | Loaded data with some common computation already computer
-- Serves as sort of cache for filtering and grouping data needed
-- by different sub-models.
data ForecastData where
   ForecastData :: forall ( n :: Nat) (sku::Nat) . (KnownNat n, KnownNat sku) => 
                   { fdQuantities__n :: S.Vector n Quantity
                   , fdDays__n :: N.Vector n Day
                   , fdSku__n :: N.Vector n Sku
                   , fdSku__sku :: AscU (N.Vector sku) Sku
                   , fdDays :: Map (Day, Day) (Wix Maybe n)
                   --  ^^^^ n : sales
                   , fdManualMap :: Map (CategoryName, [CategoryValue]) (Wix Maybe sku)
                   --  ^^^ manual
                   , fdSku__nSsNN :: N.WectorFF Vector n sku n
                   , fdCategoryMap :: Map CategoryName (N.Vector sku (Maybe CategoryValue))
                   }
                   -> ForecastData

-- unGroupForecastData :: WectorFF Maybe x__n v__g x__n -> ForecastData -> Vector v__g ForecastData  
-- unGroupForecastData nGgNN fdata = fmap (flip narrowForecastData fdata) (invertGroup nGnNN)


narrowForecastData :: KnownNat v__sku => Wix Maybe v__sku -> ForecastData -> ForecastData
narrowForecastData w@(Wix cSsCC) ForecastData{..}  
   | Wector _cS sCC <- cSsCC
   , Just Refl <-  sameNat (S.length' (unAscU fdSku__sku) ) (S.length' sCC)
   , let wn = selectX (isJust <$> windex fdSku__nSsNN  @> sCC)
   , daysMap <- fmap (intersectWix  wn ) fdDays
   , manualMap <- fmap (intersectWix w) fdManualMap
   = ForecastData{fdDays = daysMap, fdManualMap=manualMap,..}
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
   ldSales <- loadSales model
   ldCategories <- loadCategories model
   ldOrders <- return mempty
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
       ReComment _ _ model -> modelToSalesRanges model
       InjectCategory _ _ -> []

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
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  let categories = modelToCategories model
      query = do
               cat <- E.from $ E.table @ItemCategory
               E.where_ $ cat.stockId  `E.like` E.val stockLike
               E.where_ $ cat.stockId =%/. (RegexFilter "^[MC]")  -- to match actualSalesSources  and loadSales
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
    ReComment _ _ model -> modelToCategories model
    InjectCategory cat _ -> [cat]

       
 -- ==================================================
 --     PREPARE
 -- ==================================================

prepareData :: ForecastModel -> LoadedData -> ForecastData
prepareData model LoadedData{..} 
  | SomeSized (Z3 fdDays__n fdSku__n fdQuantities__n) <- ldSales
  , SomeSized (Z3 _fdDays__o fdSku__o _fdQuantities__o) <- ldOrders
  , fdDays <- mapFromList [ (range, filterX (\d -> from <= d && d <= to) fdDays__n)
                          | range@(from, to) <- modelToSalesRanges model
                          ]
  -- Get unique skus for all different sources 
  -- get manual SKU from categoryMap
  , SomeSized (Z3 sku__c category__c value__c) <- ldCategories
  , PivV cKkCC categoryMapK <- pivotV sku__c category__c -- k = number of sku used by category map
  , let sku__k = walues cKkCC @=> sku__c
  , let manualKeySet = setFromList $ manualKeys model 
  , manualMapMaybek <- flip Map.fromSet manualKeySet \(cat, values) ->
                                       case lookup cat categoryMapK of
                                            Nothing -> -- category not found, nothing to lest
                                                      Nothing
                                            Just valuesk -> 
                                               let toKeep = case values of 
                                                              [] -> not . null 
                                                              _:_ -> maybe False (`elem` values) . headm 
                                               in Just $ filterX toKeep (valuesk @>$ value__c)
  , let manualMapk = Map.mapMaybe id manualMapMaybek
  ---------------------
  , let indexJust v = S.generate' (S.length' v) V.singleton 
        indexNothing v = S.replicate' (S.length' v) V.empty
  , SomeSized (Z3 allSku__all aN0__all
                              aK0__all) <- fromSized $ Z3 ( fdSku__n S.++ fdSku__o S.++ sku__k )
                                                          (indexJust fdSku__n S.++ indexNothing fdSku__o S.++ indexNothing sku__k)
                                                          (indexNothing fdSku__n S.++ indexNothing fdSku__o S.++ indexJust sku__k)
                      --                    ^^^^^      ^^
                      --                      |         |
                      --                      |         +-- trick to make sure sku vector and partial index have the same length (and shape)
                      --                      |                (swapping elements in addition would not typecheck)
                      --                      +------------ erase the length @n+@o+@m to a simple @a (otherwise nothing compiles)
  , JoinSpineV skuSpine__aSsAA <- makeJoinSpineV allSku__all
  , sku_aSsAA <- jsGrouping skuSpine__aSsAA
  , let nA__n =  S.generate' (S.length' fdSku__n)  (finite . getFinite)  
        nAaNN = Wector nA__n (aN0__all)
        nSsNN = composeW nAaNN (unFold1 <$> sku_aSsAA)
        n = fromIntegral $ S.length fdSku__n
        kA__k = S.generate' (S.length' sku__k)  (finite . (+n) . getFinite)
        _kAaKK = Wector kA__k aK0__all -- force type
  -- get unique all sku by using mkSpine and discard what is not needed
  , fdSku__sku <- jsSpine skuSpine__aSsAA
  ------------------
  -- , JSpineV skuSpine fdSku__nSsNN <- rejoin fdSku__n
  , fdSku__nSsNN <- nSsNN -- rejoin skuSpine fdSku__n
  ---------------------------------------
  -- we already have the categoryMap but we need to extend it to the big skuSpine
  , aSsKK <- rejoin skuSpine__aSsAA sku__k
  , categoryMap <- fmap (\cs__k  -> walues aSsKK  @>= cs__k @>$ value__c )
                          categoryMapK
  , Just fdCategoryMap <- traverse (traverse mkMaybe) categoryMap -- we know there is only one value per sku/category 
                                   -- so the result of the pivot should be 0 or 1, but no more.
                                   -- The Just there check at runtime that this is the case
  ------------------------------ 
  , fdManualMap <- fmap (\(Wix mKkM0) -> let  Wector mK kM0  = mKkM0
                                              Wector aS sKK = aSsKK
                                              Just sK0 = traverse mkMaybe sKK
                                              mS = mK @> kA__k @> aS
                                              sMM = sK0 @>= kM0
                                         in Wix (Wector mS sMM)
                        ) manualMapk
  = ForecastData{..}
prepareData _ _ = error "exhaustive pattern"
    

manualKeys :: ForecastModel -> [(CategoryName, [CategoryValue])]
manualKeys model0 = 
    case model0 of
      Naive _ _ _ -> []
      CategorySplitter _ modelMap  defModel -> go $  defModel : toList modelMap
      Combination _ _ models -> go models
      MonoOperation _ _ model -> go [model]
      IndependantMargins model -> go [model]
      Aggregate _ _ model -> go [model]
      Hierachical _ top base -> go [top, base]
      ReComment _ _ model -> go [model]
      NullModel -> []
      InjectCategory catName values -> [(catName, values)]
    where go = concatMap manualKeys


nullModel :: ForecastModel -> Bool
nullModel model0 =
    case model0 of
      Naive _ _ _ -> False
      CategorySplitter _ modelMap  defModel -> go $  defModel : toList modelMap
      Combination _ _ models -> go models
      MonoOperation _ _ model -> go [model]
      IndependantMargins model -> go [model]
      Aggregate _ _ model -> go [model]
      Hierachical _ top base -> go [top, base]
      ReComment _ _ model -> go [model]
      NullModel -> True
      InjectCategory catName values -> False
    where go = all nullModel
 -- ==================================================
 --     ESTIMATE
 -- ==================================================
       
estimateModel :: ForecastModel -> ForecastData -> Vector (Sku, YearlyQuantity, TextBuilder)
estimateModel Naive{..} fdata = estimateNaive fmFrom fmTo duration fdata
   where duration = maybe (fromIntegral (diffDays fmTo fmFrom) / 365)
                          fromIntegral
                          fmDuration
estimateModel CategorySplitter{..} fd@ForecastData{..} =
  case lookup fmCategory fdCategoryMap of
       Just categorym__sku | Wal sCcSS <- groupV categorym__sku
                           , fdv__c <- fmap (flip narrowForecastData fd) (invertGroup sCcSS)
                           ->  mconcat [ estimateModel model groupFd 
                                       | i__c <- S.toList $ S.generate id
                                       , let groupFd = S.index fdv__c i__c
                                       , let cats = S.index (walues sCcSS) i__c
                                       , let catm = head1 $ cats @> categorym__sku
                                       , let model = fromMaybe fmDefaultModel $  catm >>= flip lookup fmCategoryModel
                                       , not $ nullModel model
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
    , let AscU sku__sku = fdSku__sku
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
   | etop <- estimateModel top fdata
   = case length etop of
        0 -> mempty
        _ | SomeSized top__t <- etop
          , Z3 sku__t qty__t __comment__t <- top__t
          , SomeSized base__b <- estimateModel base fdata
          , Z3 sku__b __qty__b __comment__b <- base__b
          -- for each group defined by the categorsie
          -- we need to collect the base , sum up the top and scale so that SUM of base' = SUM top
          -- we use as a spine the categorie-value combination
          , let cats__sku = S.generate \sku -> [ lookup catname fdCategoryMap >>= flip S.index sku
                                               | catname <- cats
                                               ]
          -------------------- join top
          , skuSpine__sku__s@(JoinSpine _s1 __skuSsSkuz) <- mkSpine fdSku__sku
          , Wal skuCcSkus1 <- groupV cats__sku -- group sku by cat values
            -- we need to group t (and b) by C so tCcTT and bCcBB (
          , Wector _ skuTT <- rejoin skuSpine__sku__s sku__t
          , let cSkus = fmap unFold1 $ walues skuCcSkus1
            -- get for each cats the sum
          , cTT <- fold <$> cSkus @>$ skuTT 
          , topQty__c <- F.sum <$> cTT @>$ qty__t
          -------------------- join base
          , Wector _ skuBB <- rejoin skuSpine__sku__s sku__b
          -- , cBB <- (foldMap unFold1) <$> walues skuCcSkus1 @>~ wbroadcast skuSsBB
          , cBB <- fold <$> cSkus @>$ skuBB 
          -> F.foldMap (\(bs, to) -> scaleTo to (bs @> base__b)) $ Z2 cBB topQty__c
        _ -> error "unexpected happened "

estimateModel (ReComment recomment _ model) fdata 
    | SomeSized (Z3 sku__t qty__t comment__t) <- estimateModel model fdata
    = fromSized (Z3 sku__t qty__t (recomment <$> comment__t))





estimateModel (InjectCategory catName values) ForecastData{..}  =
      case lookup (catName, values) fdManualMap of
        Nothing -> mempty
        Just (Wix mSsMM) -> let AscU sku__sku = fdSku__sku
                            in fromSized (Z3 (windex mSsMM @> sku__sku)
                                             (S.replicate 0)
                                             (S.replicate $ "Inject " <> LTB.fromText (unCategoryName catName))
                                         )
                      
   


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


estimateCollectionProfile :: Day -> FilePath -> Handler (Sku -> Collection, Map Collection SeasonProfile)
estimateCollectionProfile forecastDay  forecastDir = do
   let path = forecastDir </> "profile.hs"
   exists <- liftIO $ doesFileExist path
   prof <- if exists
            then do
                 content' <- readFileUtf8 path
                 let content = strip content'
                 maybe (error $ "Can't read " <> show path) return $ readMay content
            else return $ Easy.DefaultProfile
   makeProfile forecastDay prof
       
makeProfile :: Day -> Easy.Profile -> Handler (Sku -> Collection, Map Collection SeasonProfile)
makeProfile _ Easy.DefaultProfile = do
       let flat = seasonProfile [] -- 10,0,0,0,0,10,10] -- $ 1 : repeat 0
           collection = Collection "model"
       return (const collection, singletonMap collection flat)

makeProfile forecastDay (Easy.PerCategoryYear catname years) = do
   let model = modelFromEasy forecastDay (Easy.ForeachCategory catname (Easy.PreviousYears years))
   LoadedData{..} <- loadModelData model
   return $ case () of
      _ | SomeSized (Z3 day__n sku__n  qty__n) <- ldSales
        , SomeSized (Z3 k _ profile__c) <- ldCategories
        , month__n <- fmap (\(YearMonthDay _ m _) -> m) day__n
        , JoinV nSsNN cSsCC <- joinV sku__n k 
        , profiles__n <- mkMaybe <$> windex nSsNN @> walues cSsCC @>$ profile__c
        , Just profilem__n <- sequence profiles__n
        , PivV nPpNN monthTo_pNN  <- pivotV profilem__n month__n
        , let collection sku = Collection $ maybe "" unCategoryValue $ lookup sku skuToProfile 
              skuToProfile :: Map Sku CategoryValue
              skuToProfile = mapFromList $ S.toList $ Z2 k profile__c 
              profileVector = imap (\pi nn1 -> let collectionm = Collection $ maybe "" unCategoryValue $ S.index profilem__n (head1 nn1)
                                                   Measure total = F.sum $ nn1  @> qty__n
                                                   profile = seasonProfile [ Measure (q / total)
                                                                           |  month <- [1..12]
                                                                           , let Measure q = case lookup month monthTo_pNN of 
                                                                                       Nothing -> 0
                                                                                       Just pNN -> F.sum $ S.index pNN pi @> qty__n

                                                                           ]
                                               in (collectionm, profile)
                                   )
                                   (walues nPpNN)
        -> ( collection
           , mapFromList $ S.toList profileVector
           )
      _ -> error "pattern should be exahustive"



