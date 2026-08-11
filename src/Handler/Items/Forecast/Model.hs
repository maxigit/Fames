{-# LANGUAGE OverloadedLabels, OverloadedRecordDot, TypeOperators #-}
{-# LANGUAGE TypeAbstractions #-}
module Handler.Items.Forecast.Model
where

import Import
import Items.Types
import Handler.Items.Sources
import qualified Handler.Items.Forecast.Model.Easy as Easy
import qualified Handler.Items.Forecast.Csv as Csv
import Handler.Items.Common
import Measure -- as M
import qualified Data.Map as Map
import GL.Utils
import qualified Database.Esqueleto.Experimental as E
import qualified Data.Conduit.List as C
import qualified Data.Foldable as F
import FA
import Data.Text (strip, breakOn)
import Text.Printf(printf)
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy as LT
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
     | PostCategorySplitter { fmCategory :: CategoryName
                            , fmCategoryModifier :: Map CategoryValue ModelModifier
                            , fmDefaultModifier :: ModelModifier
                            , fmDefaultModel :: ForecastModel
                            }
     | Combination (Vector1 Quantity -> Quantity) Text [ForecastModel] -- zip models
     | Modifier ModelModifier ForecastModel
     | IndependantMargins ForecastModel
     | Hierachical { fmCategories :: [CategoryName]
                   -- , fmSimple :: Bool 
                   , fmTopModel :: ForecastModel
                   , fmBaseModel :: ForecastModel 
                   , fmLimit :: Maybe ForecastModel
                   }
       -- ^ Computes forecast using model and then scale it so that each categories product add up to the sum of top model forecast
     | ReComment (TextBuilder -> TextBuilder) Text ForecastModel 
     | InjectCategory CategoryName [CategoryValue] -- ^ Inject all the sku of the given category with a forecast of 0
     | Reference Alias
     | With { fmAlias :: Alias
            , fmAliased, fmModel :: ForecastModel
            }
     | Read FilePath
     | Mask { fmModel, fmMask :: ForecastModel  }
     | Delete0 ForecastModel 
     | NullModel
     | Trend { fmA
             , fmB
             , fmModel :: ForecastModel
             , fmAlpha :: Double
             } -- Scale to  (B + (B-A) * alpha)/B)
     -- deriving (Show, Eq)

instance Show ForecastModel where
   show (Naive from to prev) = unwords ["Naive " , show from , show to, show prev ]
   show (CategorySplitter cat models def) = unwords [ "CategorySplititer", show cat, show models, show def]
   show (PostCategorySplitter cat mods def model) = unwords [ "PostCategorySplititer", show cat, show mods, show def, show model]
   show (Combination _ ann models) = unwords ["Combination", unpack ann, show models  ]
   show (Modifier mod model) = unwords ["Modifier", show mod , show model ]
   show (IndependantMargins model) = unwords ["IndependantMargins", show model ]
   show (Hierachical cats top base limitm) = unwords ["Hierachical ", "(", show top, ")", show cats, "(", show base, ")", "(", show limitm, ")" ]
   show (ReComment _ ann model) = unwords ["ReComment", unpack ann, show model ]
   show (InjectCategory cat values) = unwords ["InjectCategory", show cat, show values ]
   show (Reference ref) = unwords ["Refercence", show ref]
   show (With alias aliased model) = unwords ["With", show alias, show aliased , show model ]
   show (Read path) = unwords ["Read", path ]
   show (Mask model mask) = unwords ["Mask", show model, show mask ]
   show (Delete0 model) = unwords ["NonNull", show model ]
   show NullModel = "NullModel"
   show (Trend a b model alpha) = unwords ["Trend", show a, show b, show model, show alpha]
newtype CategoryName = CategoryName { unCategoryName :: Text }  deriving (Show, Eq, Ord)
newtype CategoryValue = CategoryValue { unCategoryValue :: Text }  deriving (Show, Eq, Ord)
newtype Alias = Alias { unAlias :: Text} deriving (Show, Eq, Ord)

data ModelModifier 
    = Reject
    | Id
    | ApplyMono (Double -> Double) Text
    | ApplyAggregate (Vector Quantity -> Quantity) Text

instance Show ModelModifier where
  show Reject = "Reject"
  show Id = "Id"
  show (ApplyMono _ ann) = "ApplyMono:" <> unpack ann
  show (ApplyAggregate _ ann) = "ApplyAggregate:" <> unpack ann


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
     Easy.AfterForeachCategory catName mod model -> PostCategorySplitter (CategoryName catName)
                                                                         mempty
                                                                         (modFromEasy mod)
                                                                         (go model)
     Easy.CategoryCase catName cat'models defModel -> CategorySplitter (CategoryName catName)
                                                                        (mapFromList [(CategoryValue cat, go model)
                                                                                     | (cat, model) <- cat'models
                                                                                     ]
                                                                        )
                                                                        (go defModel)
     Easy.PostCategoryCase catName cat'mods defMod model -> PostCategorySplitter (CategoryName catName)
                                                                                 (mapFromList [(CategoryValue cat, modFromEasy mod)
                                                                                              | (cat, mod) <- cat'mods
                                                                                              ]
                                                                                 )
                                                                                 (modFromEasy defMod)
                                                                                 (go model)
     Easy.FilterCategory catName categories model -> CategorySplitter (CategoryName catName)
                                                              (mapFromList $ [(CategoryValue cat, go model) | cat <- categories ])
                                                              NullModel
     Easy.ExcludeCategory catName categories model -> CategorySplitter (CategoryName catName)
                                                              (mapFromList $ [(CategoryValue cat, NullModel) | cat <- categories ])
                                                              (go model)
     Easy.Mod mod model -> Modifier (modFromEasy mod) (go model)
     Easy.Sum models -> Combination F.sum "SUM" (map go models)
     Easy.Max models -> Combination F.maximum "MAX" (map go models)
     Easy.Min models -> Combination F.minimum "MIN" (map go models)
     Easy.Median models -> Combination (coerce . median . coerce) "MEDIAN" (map go models)
     Easy.Avg models -> let n = length models 
                            weight = 1 / fromIntegral n
                        in Combination F.sum "SUM(avg)" $ map (go . Easy.Mod (Easy.Scale weight))  models
     Easy.AvgPresent models -> Combination mean "AVG_PRESENT" (map  go models)
     Easy.IM model -> IndependantMargins (go model)
     Easy.HM model -> go $ Easy.ScaleBy ["style"] model (Easy.AfterForeachCategory "colour" Easy.Total model)
     Easy.ScaleBy cats top base -> Hierachical (map CategoryName cats) (go top) (go base) Nothing
     Easy.LimitBy cats top base cap -> Hierachical (map CategoryName cats) (go top) (go base) (Just $ go cap)
     Easy.NoveltyFromFuture years -> let future = calculateDateChain [AddYears years, AddDays (-1)] forecastDay
                                     in  ReComment (const "Novelty") "Novelty"
                                       $ Modifier (ApplyMono (const 0) "0") $ Naive forecastDay future (Just years)
     Easy.InjectCategory cat -> InjectCategory (CategoryName cat) []
     Easy.InjectCategoryValue cat value -> InjectCategory (CategoryName cat) [CategoryValue value]
     Easy.Ref alias -> Reference (Alias alias)
     Easy.With aliases model -> foldr (\(alias, aliased) -> With (Alias alias) (go aliased)) 
                                      (go model)
                                      aliases
     Easy.Read path -> Read (unpack path)
     Easy.Mask model mask -> Mask (go model) (go mask)
     Easy.Delete0 model -> Delete0 (go model)
     Easy.DeleteIf mod model -> Mask (go model)
                                 $ go (Easy.Delete0
                                         (Easy.Mod (Easy.EQ 0
                                                            (Easy.SetTo 0)
                                                            (Easy.SetTo 1)
                                                   )
                                                   (Easy.Mod mod model)
                                         )
                                      )
     Easy.Trend a b alpha model -> Trend (go a) (go b) (go model) alpha
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
          
mean :: (Foldable t, Functor f, Fractional a, Eq a,  Num (f a)) => t (f a) -> f a
mean v = case fromIntegral (F.length v) of
             l | l == 0 ->  fmap (const 0) (F.sum v)
             l -> fmap (/l) (F.sum v)
          
modFromEasy :: Easy.Modifier -> ModelModifier
modFromEasy mod = 
  case mod of 
    Easy.Reject -> Reject
    Easy.Id -> Id
    Easy.Scale weight -> ApplyMono (*weight) (pack $ printf "Scale %0.2f *" weight)
    Easy.AtMost cap -> ApplyMono (min cap) (pack $ printf "AtMost %0.2f &" cap)
    Easy.AtLeast floor_ -> ApplyMono (max floor_) (pack $ printf "AtLeast %0.2f &" floor_)
    Easy.SetTo value -> ApplyMono (const value) (pack $ printf "SetTo %0.2f &" value)
    Easy.Total -> ApplyAggregate F.sum "SUM"
    Easy.Mean -> ApplyAggregate mean "MEAN"
    Easy.RoundUp step -> ApplyMono (roundTo ceiling step) (pack $ printf "Roundup %0.2f ~^" step)
    Easy.RoundDown step -> ApplyMono (roundTo floor step) (pack $ printf "Roundup %0.2f ~^" step)
    Easy.Round step -> ApplyMono (roundTo round step) (pack $ printf "Roundup %0.2f ~^" step)
    Easy.EQ val if_ else_ -> mkIf "EQ" (near val) if_ else_
    Easy.NEQ val if_ else_ -> mkIf "NeQ" (not . near val) if_ else_
    Easy.LT val if_ else_ -> mkIf "LT" (<val) if_ else_
    Easy.LTE val if_ else_ -> mkIf "LTE" (\x -> x < val || near x val) if_ else_
    Easy.GT val if_ else_ -> mkIf "GT" (>val) if_ else_
    Easy.GTE val if_ else_ -> mkIf "GTE" (\x -> x > val || near x val) if_ else_
  where mkIf ann test if_easy else_easy = let (if_, ifAnn) = fromMono if_easy
                                              (else_, elseAnn) = fromMono else_easy
                                      in ApplyMono (\x -> if test x 
                                                          then if_ x
                                                          else else_ x
                                                   )
                                                   ("IF" <> ann <> " THEN " <> ifAnn <> " ELSE " <> elseAnn)
        fromMono mod  = case modFromEasy mod of
                             Id -> (id, "Id")
                             ApplyMono ifF annF -> (ifF, annF)
                             _ -> error $ show mod <> " not a Mono"


  
roundTo :: (Double -> Int) -> Double -> Double -> Double
roundTo rounder step x = fromIntegral (rounder (x / step)) * step

near :: (Fractional a, Real a) => a -> a -> Bool
near x y = abs (x - y) < 1e-4

-- * Common
estimateSkuForecastFromDir :: Day -> FilePath -> Handler (Either Text (Vector (Sku, Quantity, Text)))
estimateSkuForecastFromDir forecastDay forecastDir = do
    content' <- readFileUtf8 $ forecastDir </> "model.hs"
    let content = strip $ removeComments content'
    case readMay content of
       Nothing -> return $ Left $ "can't parse :\n" <> content --  "No model.hs file present in " <> tshow forecastDir
       Just easy -> do
             estimation <- evaluateModel forecastDir (modelFromEasy forecastDay easy)
             return $ Right estimation


removeComments :: Text -> Text
removeComments t = let
   ls = lines t
   in unlines $ map stripComment ls
   where stripComment =  strip . fst . breakOn "--"
-- * Model implementation

data LoadedData = LoadedData 
   { ldSales :: Vector (Day, Sku, Quantity)
   , ldOrders :: Vector (Day, Sku, Quantity)
   , ldCategories :: Vector (Sku, CategoryName, CategoryValue)
   , ldForecasts :: Map FilePath (Vector (Sku, Quantity, Text)) -- ^ cache and manual csv
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
                   , fdAliasMap :: Map Alias (Vector (Sku, Quantity, TextBuilder))
                   -- ^ map alias and estimation, used by reference
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
   , let narrowForecast (SomeSized v ) | Z3 sku__v _qty__v _comment__v <- v
                                       , skuSpine <- mkSpine fdSku__sku
                                       , sSsVV <- rejoin skuSpine sku__v
                                       , vm  <- headm <$> windex cSsCC @> walues sSsVV @>$ v
                                       = V.catMaybes $ fromSized vm
         narrowForecast _ = error "exhaustive pattern"
   , fdAliasMap <- fmap narrowForecast fdAliasMap
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

   
evaluateModel :: FilePath -> ForecastModel -> Handler (Vector (Sku, Quantity, Text))
evaluateModel forecastDir model = do
   loaded <- loadModelData forecastDir model
   let datas = prepareData model loaded

   return $ fmap (\(sku, qty, comment) -> (sku, qty, LT.toStrict $ LTB.toLazyText comment)) $ estimateModel model datas

-- * Loading sales

loadModelData :: FilePath -> ForecastModel -> Handler LoadedData
loadModelData forecastDir model = do
   ldSales <- loadSales model
   ldCategories <- loadCategories model
   ldOrders <- return mempty
   ldForecasts <- loadForecasts forecastDir model
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
       PostCategorySplitter _ _ _ model -> modelToSalesRanges model
       Combination _ _ models -> concatMap modelToSalesRanges models
       Modifier _ model -> modelToSalesRanges model
       NullModel -> []
       IndependantMargins model -> modelToSalesRanges model
       Hierachical _ top base limitm -> concatMap modelToSalesRanges  $ [top, base] <>  toList limitm
       ReComment _ _ model -> modelToSalesRanges model
       Reference _ -> []
       With _ aliased model -> concatMap modelToSalesRanges [ aliased, model ]
       Read _ -> []
       Mask model mask -> concatMap modelToSalesRanges [model, mask ]
       Delete0 model -> modelToSalesRanges model
       InjectCategory _ _ -> []
       Trend a b model _ -> concatMap modelToSalesRanges [a, b , model ]

modelToSalesRange :: ForecastModel -> Maybe (Day, Day)
modelToSalesRange model =
    case modelToSalesRanges model of
       [] -> Nothing
       ranges -> Just ( minimumEx $ map fst ranges
                      , maximumEx $ map snd ranges
                      )
       
modelToInputFiles :: ForecastModel -> [FilePath]
modelToInputFiles model = let
  in ordNub case model of
      Naive _ _ _ -> []
      CategorySplitter _ modelMap defModel -> go $ defModel : toList modelMap
      PostCategorySplitter _ _ _ model -> modelToInputFiles model
      Combination _ _ models -> go models
      Modifier _ model -> go [ model ]
      NullModel -> []
      IndependantMargins model -> go [ model ]
      Hierachical _ top base limitm -> go $ [top, base ] <> toList limitm
      ReComment _ _ model -> go [ model ]
      Reference _ -> []
      With _ aliased model -> go [ aliased, model ]
      Read path -> [ path ]
      Mask model mask -> go [model, mask ]
      Delete0 model -> go [ model ]
      InjectCategory _ _ -> []
      Trend a b model _ -> go [a, b, model ]
      where go = concatMap modelToInputFiles


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
  ordNub case model of
    Naive{..} -> []
    CategorySplitter cat modelMap defModel -> cat : concatMap modelToCategories (defModel : toList modelMap)
    PostCategorySplitter cat _ _ model -> cat : modelToCategories model
    Combination _ _ models -> concatMap modelToCategories models
    Modifier _ model -> modelToCategories model
    NullModel -> []
    IndependantMargins model -> map CategoryName ["style", "colour"] ++ modelToCategories model
    Hierachical cats top base limitm -> cats <> concatMap modelToCategories ( [top, base] <> toList limitm )
    ReComment _ _ model -> modelToCategories model
    InjectCategory cat _ -> [cat]
    Reference _ -> []
    With _ aliased model -> concatMap modelToCategories [aliased, model ]
    Read _ -> []
    Mask model mask -> concatMap modelToCategories [model, mask ]
    Delete0 model -> modelToCategories model
    Trend a b model _ -> concatMap modelToCategories [a, b, model ]


-- * Load Csv
--

loadForecasts :: FilePath -> ForecastModel -> Handler (Map FilePath (Vector (Sku, Quantity, Text)))
loadForecasts filepath model = do
  let paths = modelToInputFiles model
  forecasts <- mapM (liftIO . Csv.readForecast . (filepath </>)) paths
  let forecastsWithComment = zipWith (\v path -> fmap (fmap (addComment path)) v)
                                    forecasts
                                    paths
      addComment p t = pack p <> ":" <> t
  return $ mapFromList $ zip paths forecastsWithComment
       
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
  , SomeSized sku__f <- V.concat [ skus
                                 |  v <- toList ldForecasts
                                 ,  let (skus, _, _ ) = V.unzip3 v
                                 ]
  ---------------------
  , let indexJust v = S.generate' (S.length' v) V.singleton 
        indexNothing v = S.replicate' (S.length' v) V.empty
  --      TODO ADD FORECAST FROM READ
  , SomeSized (Z4 allSku__all aN0__all
                              aK0__all
                              _aF0__all
              ) <- fromSized $ Z4 ( fdSku__n S.++ fdSku__o S.++ sku__k S.++ sku__f )
                                  (indexJust fdSku__n S.++ indexNothing fdSku__o S.++ indexNothing sku__k S.++ indexNothing sku__f)
                                  (indexNothing fdSku__n S.++ indexNothing fdSku__o S.++ indexJust sku__k S.++ indexNothing sku__f)
                                   (indexNothing fdSku__n S.++ indexNothing fdSku__o S.++ indexNothing sku__k S.++ indexJust sku__f)
              --    ^^^^^      ^^
              --      |         |
              --      |         +-- trick to make sure sku vector and partial index have the same length (and shape)
              --      |                (swapping elements in addition would not typecheck)
              --      +------------ erase the length @n+@o+@m to a simple @a (otherwise nothing compiles)
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
  , fdAliasMap <-  Map.fromDistinctAscList [ (Alias (pack path) , V.zip3 sku qty (fmap LTB.fromText comment) )
                                           | (path, v) <- mapToList ldForecasts
                                           , let (sku, qty, comment) = V.unzip3 v
                                           ]
  = ForecastData{..}
prepareData _ _ = error "exhaustive pattern"
    

manualKeys :: ForecastModel -> [(CategoryName, [CategoryValue])]
manualKeys model0 = 
    case model0 of
      Naive _ _ _ -> []
      CategorySplitter _ modelMap  defModel -> go $  defModel : toList modelMap
      PostCategorySplitter _ _ _ model -> go [model]
      Combination _ _ models -> go models
      Modifier _ model -> go [model]
      IndependantMargins model -> go [model]
      Hierachical _ top base limitm -> go $ [top, base] <> toList limitm
      ReComment _ _ model -> go [model]
      NullModel -> []
      InjectCategory catName values -> [(catName, values)]
      Reference _ -> []
      With _ aliased model -> go [ aliased, model ]
      Read _ -> []
      Mask model mask -> go [model, mask ]
      Delete0 model -> go [model ]
      Trend a b model _ -> go [a, b, model ]
    where go = concatMap manualKeys


nullModel :: ForecastModel -> Bool
nullModel model0 =
    case model0 of
      Naive _ _ _ -> False
      CategorySplitter _ modelMap  defModel -> go $  defModel : toList modelMap
      PostCategorySplitter _ _ _ model  -> go [ model]
      Combination _ _ models -> go models
      Modifier _ model -> go [model]
      IndependantMargins model -> go [model]
      Hierachical _ top base limitm -> go $ [top, base] <> toList limitm
      ReComment _ _ model -> go [model]
      NullModel -> True
      InjectCategory _ _ -> False
      Reference _ -> False
      With _ aliased model -> go [ aliased, model ]
      Read _ -> False
      Mask model mask -> go [model, mask ]
      Delete0 model -> go [model ]
      Trend a b model _ -> go [a, b, model ]

    where go = all nullModel
    
nullModifier :: ModelModifier -> Bool
nullModifier mod =
    case mod of
       Reject -> True
       Id -> False
       _ -> False

applyModifier :: ModelModifier -> Vector (Sku, Quantity, TextBuilder) -> Vector (Sku, Quantity, TextBuilder)
applyModifier Reject _ = mempty
applyModifier Id v = v
applyModifier (ApplyMono f ann) v 
   | SomeSized (Z3 sku qty comment) <- v
   = fromSized (Z3 sku (fmap f <$> qty) (S.zipWith annotate qty comment))
   where annotate q c = LTB.fromText ann <> " " <> fromMeasure q <> "=(" <> c <> ")"
applyModifier (ApplyAggregate agg ann) v 
    | SomeSized (Z3 sku qty0 comment0) <- v
    , let qty = S.replicate $ agg $ fromSized qty0
          comment = fmap (\c -> "AGG" <> LTB.fromText ann <> "): [" <> c <> "]") comment0
    = fromSized (Z3 sku qty comment)
applyModifier  _ _ = error "exhaustive pattern"

 -- ==================================================
 --     ESTIMATE
 -- ==================================================
       
estimateModel :: ForecastModel -> ForecastData -> Vector (Sku, Quantity, TextBuilder)
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
   
estimateModel PostCategorySplitter{..} fd@ForecastData{..}
   = case lookup fmCategory fdCategoryMap of
        Just categorym__sku | SomeSized est@(Z3 sku__n __qty__n __comment__n ) <- estimateModel fmDefaultModel fd
                            , JoinSpineV spine__n_k <- makeJoinSpineV sku__n
                            , nKkSS <- rejoin spine__n_k (unAscU fdSku__sku)
                            , Just nKkS0 <- traverse mkMaybe nKkSS
                            , catm__n <- wbroadcast nKkS0 @>= categorym__sku 
                            , Wal nCcNN <- groupV catm__n
                            -- process each category value , pull all the estimation from it and concat the resutls
                            -> mconcat [ applyModifier mod $ unFold1 nn1 @> est
                                       | i__c <- S.toList $ S.generate id -- indice of the curret category value
                                       , let nn1 = S.index (walues nCcNN) i__c 
                                       , let catm = S.index catm__n (head1 nn1)
                                             mod = fromMaybe fmDefaultModifier $ catm >>= flip lookup fmCategoryModifier
                                       , not $ nullModifier mod
                                       -- find the row belonging to that category, we need to broadcast the cat
                                       ]
        ___Nothing -> mempty

estimateModel NullModel _ = mempty
estimateModel (Combination agg aggName models) fdata =
    combineForecasts agg aggName (map (flip estimateModel fdata) models)
   
-- estimateModel (Combination _ _ _ ) _ = error "exhaustive pattern"
                              
estimateModel (Modifier mod model ) fdata = 
   applyModifier mod $ estimateModel model fdata

    
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

estimateModel (Hierachical cats top base limitm) fdata@ForecastData{..}
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
          , let scaler to = case limitm of
                             Nothing -> scaleTo to
                             Just cap -> let threshold = fmap (* 0.1) to 
                                             capForecast = estimateModel cap fdata
                                         in scaleWithLimit threshold to capForecast
          -> F.foldMap (\(bs, to) -> scaler to (bs @> base__b)) $ Z2 cBB topQty__c
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
                      
estimateModel (With alias aliased model) fd = 
    estimateModel model fd {fdAliasMap = insertMap alias (estimateModel aliased fd) (fdAliasMap fd)}

estimateModel (Reference alias) fd = 
  findWithDefault (error $ show alias <> " not found")  
                  alias
                  (fdAliasMap fd)
   
estimateModel (Read path) fd = estimateModel (Reference (Alias $ pack path)) fd
estimateModel (Mask model mask) fd 
    | SomeSized v@(Z3 sku__n _ _ ) <- estimateModel model fd
    , SomeSized (Z3 sku__m _ _) <- estimateModel mask fd
    , JoinV _nJjNN nJjMM <- joinV sku__n sku__m
    , let maybe__n = headm <$> wbroadcast nJjMM
    , JustX _ rNnRR <- catMaybesX maybe__n
    = fromSized $ windex rNnRR @> v

estimateModel (Delete0 model) fd 
    | SomeSized v@(Z3 _ qty__n _ ) <- estimateModel model fd
    , Wix sNnSS <- filterX (near 0) qty__n
    = fromSized $ windex sNnSS @> v

estimateModel (Trend ma mb model alpha) fd
   | SomeSized (Z3 _ qty__a _ ) <- estimateModel ma fd
   , SomeSized (Z3 _ qty__b _ ) <- estimateModel mb fd
   , SomeSized v@(Z3 _ qty__m _) <- estimateModel model fd
   , let Measure a = F.sum qty__a
         Measure b = F.sum qty__b
         c= b + (b - a) * alpha
         m = F.sum qty__m
         ratio = max 0 (c / b)
   = if near b 0 ||  near ratio  0 || ratio < 0
     then mempty
     else scaleTo (fmap (*ratio) m ) (fromSized v)

estimateModel model  _  = error $ "exthaustive pattern for " <> show model

combineForecasts :: (Vector1 Quantity -> Quantity) -> Text -> [Vector (Sku, Quantity, TextBuilder)] -> Vector (Sku, Quantity, TextBuilder)
combineForecasts agg aggName forecasts 
    | SomeSized (Z3 sku__n qty__n comment__n) <- concat forecasts
    , Wal nSsNN <- groupV sku__n
    , sku__s <- walues nSsNN @=> sku__n
    , qty__s <- agg <$> walues nSsNN @>$ qty__n
    , comment__s <- fmap (\nn -> mconcat $ LTB.fromText aggName : ":"
                                         : (intercalate1 (LTB.singleton ' ')
                                                         (fmap (\n -> fromMeasure (S.index qty__n n)
                                                               )
                                                               nn 
                                                         )
                                           )
                                         : " "
                                         : [ intercalate1 (LTB.singleton ' ' )
                                           (fmap (\n -> LTB.fromString "("
                                                       <> fromMeasure (S.index qty__n n)
                                                       <> "={" <> S.index comment__n n <> "}"
                                                       <> ")"
                                                ) nn
                                           )
                                           ]
                              
        
                         ) 
                         (walues nSsNN)

    = fromSized $ Z3 sku__s qty__s comment__s
    | otherwise  = error "exhaustive pattern"

scaleTo :: Quantity -> Vector (Sku, Quantity, TextBuilder) -> Vector (Sku, Quantity, TextBuilder)
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

-- | Scales so that the sum of forecasted quantity is equal to given total quantity but limit the qty for each sku to the capped quantity and redistribut the left over the remaining one.
-- For example let' say we have the following forecast
--    A : 10
--    B : 20
--    C:  10
-- and we want to scale (40) to 24. A+B = 40 so the normal scaling would result in
--    A :  6 
--    B : 12
--    C:   6
-- However we might have only 3 A left in stock and no plan to rebuy some. We can only supply 3 A and therefore redistribute
-- the 3 left over to B and C  (2 and 1) so the final forecast becomes
--    A :  3       = 3
--    B : 12+2     = 14
--    C:   6+1     = 7
--    
--    If something is not present in the cap vector it is expected to be 0.
scaleWithLimit :: Quantity -> Quantity -> Vector (Sku, Quantity, TextBuilder) -> Vector (Sku, Quantity, TextBuilder) -> Vector (Sku, Quantity, TextBuilder)
scaleWithLimit _ to _cap _v | to == 0 =  mempty
scaleWithLimit threshold to cap v = 
  case scaleTo to v of
     SomeSized scaled__n@(Z3 sku__n qty__n _ )
               | -- cap the scaled version to cap
                 Z4 capped__n __leftOver__n newcap__n comment__n <- capWith scaled__n cap
               , let totalScaled = F.sum capped__n
                     leftToScale = to - totalScaled
                     totalCapLeft = F.sum newcap__n
                     -- now we need to redistribute to leftover to things which are not capped
                     -- reusing leftOver would not work because it would not redistribute
                     -- but just try again the same sku with a different qty but a cap of 0
                     -- instead we need to keep the original forecast (which acts as weight)
                     -- but only for the sku which have a cap left
                     toRedistribute = S.zipWith (\q n -> if n == 0 then 0 else q)
                                              qty__n
                                              newcap__n
               -> if leftToScale <= threshold || totalCapLeft < 1 || totalScaled < 1
                  then fromSized (Z3 sku__n capped__n comment__n)
                  else -- scale left over
                       case scaleWithLimit threshold
                                           leftToScale
                                           (fromSized $ Z3 sku__n newcap__n comment__n)
                                           (fromSized $ Z3 sku__n toRedistribute comment__n)
                                           of
                            extra -> combineForecasts F.sum "+" [ fromSized $ Z3 sku__n capped__n comment__n
                                                                ,  extra
                                                                ]
     _ -> error "exhaustive pattern"
             

  
capWith :: KnownNat n => S.Vector n (Sku, Quantity, TextBuilder) -> Vector (Sku, Quantity, TextBuilder) -> S.Vector n (Quantity, Quantity, Quantity, TextBuilder)
--  -> Capped Forecast , Left to redistribut, New capp
capWith (Z3 sku__n qty__n comment__n) cap
    | SomeSized (Z3 sku__c qty__c comment__c) <- cap
    , JoinV nJjNN cJjCC <- joinV sku__n sku__c
    , let nCm = headm <$> (windex nJjNN @> walues cJjCC)
          capm__n =  nCm @>$ qty__c
          cap__n = fmap (fromMaybe 0) capm__n
          ccomment__n = fmap (fromMaybe "<null>") (nCm @>$ comment__c)
          capped__n = S.zipWith min qty__n cap__n
          left__n = qty__n - capped__n
          newcap__n = S.zipWith (\cap capped -> max 0 (cap - capped) )  cap__n capped__n
          newcomment__n = S.zipWith4 (\qty cap comment ccomment -> "LIMIT " <> fromMeasure qty <> " TO " <> fromMeasure cap
                                                           <> "[(" <> comment <> ") TO (" <> ccomment <> ")]" )
                                 qty__n
                                 cap__n
                                 comment__n
                                 ccomment__n
    = Z4 capped__n left__n newcap__n newcomment__n
capWith _ _ = error "exhaustive pattern"

estimateNaive :: Day -> Day -> Double -> ForecastData -> Vector (Sku, Quantity, TextBuilder)
estimateNaive from to years ForecastData{..} = 
   case lookup (from, to) fdDays of
        Just (Wix dNnDD) | skus__d <- windex dNnDD @> fdSku__n
                         , quantities__d <- windex dNnDD @> fdQuantities__n
                         , Wal @_ @_ @s dSsDD <- groupV skus__d
                         -> let skus__sku = walues dSsDD @=> skus__d
                                qty__sku = F.sum <$> walues dSsDD @>$ quantities__d
                                yearFraction = S.replicate $ Measure years :: N.Vector s Scalar
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
   makeProfile forecastDay forecastDir prof
       
makeProfile :: Day -> FilePath -> Easy.Profile -> Handler (Sku -> Collection, Map Collection SeasonProfile)
makeProfile _ _ Easy.DefaultProfile = do
       let flat = seasonProfile [] -- 10,0,0,0,0,10,10] -- $ 1 : repeat 0
           collection = Collection "model"
       return (const collection, singletonMap collection flat)

makeProfile forecastDay forecastDir (Easy.PerCategoryYear catname years) = do
   let model = modelFromEasy forecastDay (Easy.ForeachCategory catname (Easy.PreviousYears years))
   LoadedData{..} <- loadModelData forecastDir model
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



