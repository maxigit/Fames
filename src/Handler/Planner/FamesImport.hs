{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-} -- TODO remove
{-# OPTIONS_GHC -Wno-unused-do-bind #-} -- TODO remove
module Handler.Planner.FamesImport
( importFamesDispatch
) where
-- Import methods connecting Fames live database to the planner
import Import
import Planner.Types
import Planner.Internal
import WarehousePlanner.Report
import qualified Handler.WH.PackingList as PL
import qualified Handler.WH.Boxtake as Box
import qualified Handler.WH.Boxtake.Adjustment as Box
import qualified Handler.Planner.Exec as Exec
import qualified Handler.Items.Index as I
import qualified Handler.Items.Common as I
import qualified Items.Internal as I
import qualified Items.Types as I
import Database.Persist.Sql (toSqlKey)
import Data.Conduit.List (consume, sourceList)
import Data.Text(splitOn)
import Data.List (nubBy, nub)
import System.FilePath.Glob(globDir1, compile, match)
import Database.Persist.MySQL     (Single(..), rawSql)
import qualified Data.Map as Map
import GL.Utils
-- * Type 
-- data FamesImport
--   = ImportPackingList (Key PackingList) --  ^ import packing list
--   -- -| BoxTag
--   deriving (Eq, Show)

-- | Create a Route type for each type of import
-- This is a just a quick way of generate `parseRoute`
-- which helps us to convert a URI to data type
-- that we can dispatch on later
data FI
mkYesodSubData "FI" [parseRoutes|
/packingList/#Int64 FIPackingList
/activeBoxes FIActiveBoxes
/activeBoxes/live FIActiveBoxesLive
/activeBoxes/live/at/#Day FIActiveBoxesLiveAt
/activeBoxes/live/ago/#Int FIActiveBoxesLiveAgo
/boxStatus/active/#Text FIBoxStatusActive
/boxStatus/all/#Text FIBoxStatusAll
/boxStatus/live/#Text FIBoxStatusLive
/boxStatus/live/at/#Day/#Text FIBoxStatusLiveAt
/boxStatus/live/ago/#Int/#Text FIBoxStatusLiveAgo
/file/+[FilePath] FILocalFile
/plannerReport FIPlannerReport:
  /tags/+[FilePath] TagReport
  /boxes/+[FilePath] BoxReport
  /movesAndTags/+[FilePath] MATReport
  /orientations/+[FilePath] OrientationReport
  /clone/+[FilePath] CloneReport
  /delete/+[FilePath] DeleteReport
  /stocktake/+[FilePath] StocktakeReport
/variationStatus/active/#Text FIVariationStatusActive
/variationStatus/all/#Text FIVariationStatusAll
/cloneVariation/active/#Int/#Text/#Text FICloneVariationActive
/cloneVariation/all/#Int/#Text/#Text FICloneVariationAll
/stockStatus/active/#Text FIStockStatusActive
/stockStatus/all/#Text FIStockStatusAll
/colour/variations/#Text FIColourTransform
/category/#Text/+[Text] FICategory
/sales/#Day/#Day/#Text FISalesBetween
/salesWithKey/#Day/#Day/#Text/#Text FISalesWithKeyBetween
|]

__avoid_unused_warning_for_resourcesFI = resourcesFI 
-- *  Dispatcher 
importFamesDispatch :: Section -> Handler (Either Text [Section])
importFamesDispatch (Section ImportH (Right content) _) = do
  sectionss <- forM content $ \uri ->  do
    let (main:tags) = splitOn "#" uri
        pieces = splitOn "/" main
        ret :: Handler Section -> Handler (Either Text [Section])
        ret m = Right . (:[]) <$> m
        calcDate :: Int -> Handler Day
        calcDate days = do
          today <- todayH
          return $ calculateDate (AddDays $ -days) today
    case (parseRoute (pieces, []) <|> parseRoute (pieces ++ [""], [])) of
      --                                                  ^ similutate missing / to url which needs it
      Nothing -> return $ Left $ uri <> " is not a valid import"
      Just fi -> case fi of
          FIPackingList plId -> ret $ importPackingList (toSqlKey plId) tags
          FIActiveBoxes -> ret $ importActiveBoxtakes tags
          FIActiveBoxesLive -> ret $ importActiveBoxtakesLive Nothing tags
          FIActiveBoxesLiveAt date -> ret $ importActiveBoxtakesLive (Just date) tags
          FIActiveBoxesLiveAgo days -> calcDate days >>=  \date -> ret $ importActiveBoxtakesLive (Just date) tags
          FIBoxStatusActive prefix -> ret $ importBoxStatus ActiveBoxes prefix tags
          FIBoxStatusAll prefix -> ret $ importBoxStatus AllBoxes prefix tags
          FIBoxStatusLive prefix -> ret $ importBoxStatusLive Nothing AllBoxes prefix tags
          FIBoxStatusLiveAt date prefix -> ret $ importBoxStatusLive (Just date) AllBoxes prefix tags
          FIBoxStatusLiveAgo days prefix -> calcDate days >>= \date -> ret $ importBoxStatusLive (Just date) AllBoxes prefix tags
          FILocalFile path -> hxtoHe $ do
            sectionsX <- heToHx $ readLocalFiles (intercalate "/" path) tags
            ssx <- mapM (heToHx . importFamesDispatch) sectionsX
            return $ concat ssx
          FIPlannerReport report -> let
            reportParam = headMay tags
            in case report of
              TagReport path -> executeReport TagsH path reportParam
              BoxReport path -> executeReport (BoxesH tags) path reportParam
              MATReport path -> executeReport (MovesAndTagsH tags) path reportParam
              OrientationReport path -> executeReport OrientationsH path reportParam
              CloneReport path -> executeReport (ClonesH tags) path reportParam
              DeleteReport path -> executeReport DeletesH path reportParam
              StocktakeReport path -> executeReport (StocktakeH tags) path reportParam
          FIVariationStatusActive skus -> ret $ importVariationStatus ActiveBoxes skus
          FIVariationStatusAll skus -> ret $ importVariationStatus AllBoxes skus
          FICloneVariationActive qty skus toClone -> ret $ cloneVariationStatus ActiveBoxes qty skus toClone tags
          FICloneVariationAll qty skus toClone -> ret $ cloneVariationStatus AllBoxes qty skus toClone tags
          FIStockStatusActive skus -> ret $ importStockStatus ActiveBoxes skus
          FIStockStatusAll skus -> ret $ importStockStatus AllBoxes skus
          FIColourTransform prop -> ret $ importColourDefinitions prop
          FICategory skus categories -> ret $ importCategory skus categories
          FISalesBetween start end skus -> ret $ importSales start end skus Nothing
          FISalesWithKeyBetween start end skus for -> ret $ importSales start end skus (Just for)
  return $ (fmap concat) $  sequence sectionss
importFamesDispatch section = return $ Right [section]


-- * Importers 
-- | Imports undelivered boxes from packing list.
-- We only import undeliverd one because the delivered one are probablly in the planner somewhere
importPackingList :: Key PackingList -> [Text] -> Handler Section
importPackingList key tags =  runDB $ do
  packingList <- getJust key
  detailEs <- selectList [PackingListDetailPackingList ==. key, PackingListDetailDelivered ==. False]
                         [Asc PackingListDetailId]
  let texts = PL.toPlanner PL.WithDetails packingList detailEs
  return $ Section (BoxesH tags) (Right texts) ("** Import Packing List " <> tshow key)


-- | Create a STOCKTAKE sections
-- will all the boxes alives
importActiveBoxtakes :: [Text] -> Handler Section
importActiveBoxtakes tags = do
  today <- todayH
  let source = Box.boxSourceToCsv Box.plannerSource
  content <- (runDB $ runConduit $ source .| consume)
  return $ Section (StocktakeH tags) (Right content) ("* Stocktake from Fames DB [" <> tshow today <> "]")

  


-- | Generate a TAG files from barcode
-- with scan and status information.
-- The main difference with ActiveBoxtakes
-- is that it doesn't create boxes but only tags
data WhichBoxes = AllBoxes | ActiveBoxes deriving (Eq, Show)
importBoxStatus whichBoxes prefix a_tags = do
  today <- todayH
  operators <- allOperators
  let source = case whichBoxes of
        AllBoxes -> selectSource [] []
        ActiveBoxes -> Box.plannerSource  .| mapC fst
      -- prefix = fromMaybe "" a_prefix
      getTags (Entity _ Boxtake{..}) = "#barcode=" <> boxtakeBarcode <> "," <> (intercalate "#" tags)
              where
                tags = [ prefix <> "location="  <> boxtakeLocation
                       , prefix <> "status=" <> if boxtakeActive then "active" else "inactive"
                       , prefix <> "date=" <> tshow boxtakeDate
                       , prefix <> "reference=" <> boxtakeReference
                       , prefix <> "operator=" <> maybe (tshow $ unOperatorKey $ boxtakeOperator)
                                                        operatorNickname
                                                        (lookup boxtakeOperator operators)
                       ] <> a_tags
      header = "selector,tags"
  rows <- (runDB $ runConduit $ source .| mapC getTags .| consume)
  let content = header:rows
  return $ Section (TagsH) (Right content) ("* Tags from Fames DB [" <> tshow today <> "]")
  

-- | Read local files using glob pattern(s)
-- Uses the same directory as the planner
readLocalFiles :: FilePath -> [Text] -> Handler (Either Text [Section])
readLocalFiles pat excluded = do
  plannerDir <- appPlannerDir <$> getsYesod appSettings
  files <- liftIO $ globDir1 (compile pat) plannerDir
  let orgs = filter valid $ sort files
      exPats = map (compile . unpack . ("/**/" <>) ) excluded
      valid f =  all ($ f) $ fileValid : map (\p -> not . match p) exPats
  contents <- mapM readFile orgs
  let sectionss = traverse (parseScenarioFile . decodeUtf8)  contents
  return $ fmap concat sectionss

--  | Execute the report from another planner
executeReport :: HeaderType -> [FilePath] -> Maybe Text -> Handler (Either Text [Section])
executeReport headerType paths reportParamM = do
  let path = intercalate "/" paths
  plannerDir <- appPlannerDir <$> getsYesod appSettings
  let reportParam = fromMaybe "report" reportParamM
  scenarioE <- readScenarioFromPath importFamesDispatch (plannerDir </> path)
  today <- todayH
  content <- case scenarioE of
    Left _ ->  error $ "Scenario: " <> unpack path <> " doesn't exist"
    Right scenario -> do
      let report = generateGenericReport today reportParam
      rows <- Exec.renderReport scenario report 
      return rows
  -- add a dummy csv header if needed
  let addHeader content =
        if writeHeader headerType `elem` ["Boxes", "Moves", "Tags", "MovesAndTags"
                                         , "Orientations", "TransformTags", "Stocktake"
                                         ]
        then ("selector,action":content)
        else content
  return $ Right [Section headerType
                          (Right $ addHeader content)
                          ("* " <> writeHeader headerType
                            <> " from " <> pack path <>"[" <> tshow today <> "]"
                          )
                 ]
      

-- ** FA status 
-- | Create tags for each variations for FA and Website status
-- corresponding to the All statuses tabs in item report
importVariationStatus :: WhichBoxes -> Text -> Handler Section
importVariationStatus which skuLike = do
  cache <- I.fillIndexCache
  skuToStyleVar <- I.skuToStyleVarH
  let ?skuToStyleVar = skuToStyleVar
  itemGroups <- I.loadVariations cache indexParam  {I.ipMode = ItemFAStatusView }
                                                   {I.ipShowInactive = whichToActive which}
                                                   {I.ipSKU = Just $ LikeFilter skuLike }
  
  let rows  = [(style, var, runningStatus)
              | (__base, vars) <- itemGroups
              , (__varStatus, (I.ItemInfo style  var  info)) <- vars
              , Just (_, runningStatus) <- [I.faRunningStatus <$> I.impFAStatus info]
              ]
      content = [ style <> "#'" <> var <> ",fa-status=" <> tagFor status
                --           ^ the variaton is a special tag
                | (style, var, status) <- rows
                ]
      tagFor = drop 2 . toLower . tshow

  return $ Section TagsH (Right $ "selector,tags" : content) ("* FA status")
  
whichToActive which = case which of
  AllBoxes -> I.ShowAll
  _ -> I.ShowActive
-- | Clone boxes each variation and tag them with the FA status
cloneVariationStatus :: WhichBoxes -> Int -> Text -> Text -> [Text] -> Handler Section
cloneVariationStatus which qty skuLike toClonem tags = do
  let toClone = if null toClonem then "create-model" else toClonem
  cache <- I.fillIndexCache
  skuToStyleVar <- I.skuToStyleVarH
  let ?skuToStyleVar = skuToStyleVar
  itemGroups <- I.loadVariations cache indexParam  {I.ipMode = ItemFAStatusView }
                                                   {I.ipShowInactive = whichToActive which}
                                                   {I.ipSKU = Just $ LikeFilter skuLike }
  
  let rows  = [(style, var, runningStatus)
              | (__base, vars) <- itemGroups
              , (__varStatus, (I.ItemInfo style  var  info)) <- vars
              , Just (_, runningStatus) <- [I.faRunningStatus <$> I.impFAStatus info]
              ]
      content = [ style <> "#" <> toClone <> "," <> tshow qty <> "," <> var <> "#fa-status=" <> tagFor status 
                | (style, var, status) <- rows
                ]
      tagFor = drop 2 . toLower . tshow
  return $ Section (ClonesH tags) (Right $ "selector,qty,tags" : content) ("* Clone FA variations") 
  
-- | Create tags for each variations corresponding to the availability status (availiable low stock etc ...)
importStockStatus :: WhichBoxes -> Text -> Handler Section
importStockStatus which skus = do
  skuToStyleVar <- I.skuToStyleVarH
  let sql0 = "SELECT stock_id, state  FROM warehouse.fi_stock JOIN 0_stock_master using (stock_id) WHERE stock_id like ?"
      p = [ toPersistValue skus ]
      sql  = case which of
        AllBoxes -> sql0
        ActiveBoxes -> sql0 <> " AND inactive = 0"
  rows <- runDB $ rawSql sql p
  let content = map go rows
      go (Single (skuToStyleVar ->  (style, var)), Single status) = style <> "#'" <> var <> ",stock-status=" <> status
                                                                                         <> "#stock-status-colour=" <> colorFor status
                                                                                         <> "#stock-short-status=" <> short status
      colorFor status = case status of
        "available" -> "green"
        "low stock" -> "orange"
        "coming soon" -> "blue"
        "expected later" -> "black"
        _ -> "grey"
      short status = case status of
        "available" -> "AV"
        "low stock" -> "LS"
        "coming soon" -> "CS"
        "expected later" -> "EL"
        _ -> "SO"
  return $ Section TagsH (Right $ "selector,tags" : content) ("* Stock status")


indexParam :: I.IndexParam
indexParam = I.IndexParam{..} where
  ipSKU = Nothing
  ipCategory = Nothing
  ipCategoryFilter = Nothing
  ipVariationsF = Nothing
  ipVariationGroup = Nothing
  ipShowInactive = I.ShowAll
  ipShowExtra = False
  ipBases = mempty
  ipChecked = []
  ipColumns = []
  ipMode = ItemFAStatusView
  ipClearCache = False
  ipGLStatusFilter = Nothing
  ipSalesPriceStatusFilter = Nothing
  ipPurchasePriceStatusFilter = Nothing
  ipFAStatusFilter = Nothing
  ipWebStatusFilter = Nothing
  ipWebPriceStatusFilter = Nothing
  ipCategories = Nothing
  ipBaseVariation= Nothing

-- ** Live box status adjusted with realive QOH 
importBoxStatusLive :: Maybe Day -> WhichBoxes -> Text -> [Text] -> Handler Section
importBoxStatusLive todaym which prefix __tags = do
  summaries <- loadLiveSummaries todaym
  operators <- allOperators
  let header = "selector,tags"
  let content = header:rows
      rows = [  "#barcode=" <> boxtakeBarcode
               <> ","
               <> intercalate "#" [ prefix <> "live-status=" <> tshow (Box.boxStatus statusbox)
                                  , prefix <> "status=" <> if boxtakeActive then "active" else "inactive"
                                  , prefix <> "date=" <> tshow boxtakeDate
                                  , prefix <> "location=" <> tshow boxtakeLocation
                                  , prefix <> "reference=" <> boxtakeReference
                                  , prefix <> "operator=" <> maybe (tshow $ unOperatorKey $ boxtakeOperator)
                                                        operatorNickname
                                                        (lookup boxtakeOperator operators)
                                  ]
             | summary <- summaries
             , statusbox <- Box.ssBoxes summary
             , let (Entity _ Boxtake{..}, _) = Box.usedSubject statusbox
             , case which of
                 AllBoxes -> True
                 ActiveBoxes -> boxtakeActive
             ]

  return $ Section (TagsH) (Right content) ("* Tags from box status live")

loadLiveSummaries todaym = do
  defaultLocation <- appFADefaultLocation <$> getsYesod appSettings 
  let param =  Box.AdjustmentParam{..}
      aStyleFilter = Nothing
      aLocation = defaultLocation
      aSkipOk = False
      aShowDetails = True
      aStyleSummary =  False
      aDate = todaym
  infos <- runDB $ Box.loadAdjustementInfo  param
  let summaries = toList infos >>= Box.computeInfoSummary
  return summaries

  
importActiveBoxtakesLive :: Maybe Day -> [Text] -> Handler Section
importActiveBoxtakesLive todaym tags = do
  today <- maybe todayH return todaym
  skuToStyleVar <- I.skuToStyleVarH
  summaries <- loadLiveSummaries todaym
  let sameBarcode op (Entity _ a, _) (Entity _ b, _) = boxtakeBarcode a `op` boxtakeBarcode b
  let boxes = nubBy (sameBarcode (==)). sortBy (sameBarcode compare) $
              [ ( Entity boxKey boxtake  {boxtakeDescription= Just $ (fromMaybe ""  $ boxtakeDescription boxtake) <> extraTags}
                , map (snd . skuToStyleVar . stocktakeStockId . entityVal) stocktakes
                )
              | summary <- summaries
              , statusbox <- Box.ssBoxes summary
              , let (Entity boxKey boxtake, stocktakes) = Box.usedSubject statusbox
              , Box.boxStatus statusbox /= Box.BoxInactive
              , let tags = [ "#live-status=" <> tshow (Box.boxStatus statusbox)
                       ]
              , let extraTags = mconcat tags

              ]
      source  =  Box.boxSourceToCsv (sourceList boxes)
  content <- (runDB $ runConduit $ source .| consume)
  return $ Section (StocktakeH tags) (Right content) ("* Live Stocktake from Fames DB [" <> tshow today <> "]")
  


  
-- ** Website color 
-- | Transform colour name with RGB value
importColourDefinitions :: Text -> Handler Section
importColourDefinitions prop =  do
  let query = "SELECT field_colour_code_value, field_rgb_value FROM field_data_field_colour_code NATURAL JOIN field_data_field_rgb "
  col'rgbs <- runDCDB $ rawSql query []
  let content = map transform col'rgbs 
      transform (Single colour, Single rgb) = "#" <> prop <> "=" <> colour <> "," <> prop <> "=" <> clean rgb
      clean ('#':rgb) = clean rgb
      clean rgb = pack rgb

  mapM print content

  return $ Section (TagsH) (Right $ "selector,tag" : content) ("* Colour transform")

-- ** Category 
importCategory skus categories = do
  skuToStyleVar <- I.skuToStyleVarH
  cats <- runDB $ selectList ( filterE id ItemCategoryStockId (Just $ LikeFilter skus)
                             <> [ ItemCategoryCategory <-. categories
                                ]
                             )
                             [Asc ItemCategoryStockId ]
  -- for efficiency we group all the categories in one line
  -- so that the planner has only to find once each boxes and apply all the tags
  let grouped = Map.fromListWith (<>) [ (itemCategoryStockId, [(itemCategoryCategory, itemCategoryValue)])
                             | (Entity _ ItemCategory{..})  <- cats
                             ]
      content =  [ style <> "#'"  <> var <> "," <> (intercalate "#" (map mkTag cat'values))
                 | (sku, cat'values) <- Map.toList grouped
                 , let (style,var) = skuToStyleVar sku
                 ]
      mkTag (category, value) = "cat-" <> category <> "=" <> value
  return $ Section TagsH (Right $ "selector,tags" : content) ("* Categories " <> intercalate " " categories) 
-- ** Sales
-- | generates tags with the sales between the give date for each sku.
importSales :: Day -> Day -> Text -> Maybe Text  -> Handler _
importSales startDate endDate skus forStyle = do
  skuToStyleVar <- I.skuToStyleVarH
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  let sql = "SELECT stock_id, -sum(qty) sales  FROM 0_stock_moves WHERE tran_date BETWEEN ? AND ? "
         <> " AND stock_id like ? AND type in (10, 13)"
         <> " GROUP BY stock_id ORDER BY stock_id, sales DESC"
  let skuLike = if skus == "" then stockLike else skus

  raws <- runDB $ rawSql sql [ toPersistValue startDate, toPersistValue endDate
                             , toPersistValue skuLike
                             ]
  let _types = raws :: [(Single Text, Single Double)]
  let rows = [ (style, (var, qty))
             | (Single sku, Single qty) <- raws
             , let (style, var) = skuToStyleVar sku
             ] 
      maxRank = length rows + 1

  let content = [ key <> "#'" <> var <> ",fa-sales-rank=" <> tshow (rank :: Int )<> "#fa-sales=" <> tshow (round qty :: Int )
                | group <- groupBy (on (==) fst) rows
                , ((style, (var, qty)), rank) <- zip (sortOn (Down . snd . snd) group)  [1..]
                , let key = fromMaybe style forStyle
                ]
      reset = [ key <>",#fa-sales-rank=" <> tshow maxRank <> "#fa-sales=0"
              | style <- nub . sort $ map fst rows
              , let key = fromMaybe style forStyle
              ]

  return $ Section TagsH (Right $ "selector, tags" : (reset ++ content)) ( "* Sales ")
