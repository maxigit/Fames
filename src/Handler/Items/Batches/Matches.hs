{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Items.Batches.Matches where

import Import hiding((.:))
import qualified Data.Csv as Csv
import Data.List(scanl, nub)
import Handler.CsvUtils
import Handler.Items.Common
import Handler.Items.Category.Cache
import Data.Text(toTitle, splitOn)
import qualified FA as FA
import Data.Maybe(fromJust)
import Handler.Table
import Database.Persist.Sql(rawSql, Single(..))
import Data.Char(ord)
-- * Types

data MatchRow (s :: RowTypes) = MatchRow
  { source :: FieldTF s (ForRowT s Text Text (Entity Batch) (Entity Batch))
  , sourceColour :: FieldTF s Text
  , targetColour :: FieldTF s Text
  , target :: FieldTF s (ForRowT s Text Text (Entity Batch) (Entity Batch))
  , quality :: FieldTF s MatchQuality
  , comment :: FieldTF s (Maybe Text)
  } -- deriving Show

deriving instance Show (MatchRow 'RawT)

-- * Instance
-- | Parse one match per row
instance Csv.FromNamedRecord (MatchRow  'RawT) where
  parseNamedRecord  m = MatchRow
    <$> m .: "Source" 
    <*> m .: "SourceColour"
    <*> m .: "TargetColour"
    <*> m .: "Target"
    <*> m Csv..: "Quality"
    <*> m .: "Comment"
-- | Parse multiple matches , one column per batch (with multipe value)
instance Csv.FromNamedRecord ([MatchRow 'RawT]) where
  parseNamedRecord m = do
    -- get all columns
    let columns = keys m
        batchNames = filter (`notElem` ["SKU", "Batch", "Source", "SourceColour"]) columns

    -- check if a batch is given
    -- in that case we use the color of the sku
    -- but the given batch. We should also check
    -- that the given batch is correct
    batchM <- do
      if "Batch" `member` m
      then m .: "Batch"
      else return $ Left $ error "don't evaluate"
    sku <- m .: "SKU"
    let (source, sourceColour ) = case batchM of
                        (Right (Just batch)) -> (Right batch, sku) -- use sku as colour
                        _ -> (sku, Right Nothing)
    matchess <- forM batchNames $ \batchNameBS -> do
      col'qs <- m Csv..: batchNameBS
      batchName <- Csv.parseField batchNameBS
      return [ MatchRow source sourceColour
                        (col) (Right batchName)
                        (quality) (Right Nothing)
                        
             | (col, quality) <-  col'qs :: [(FieldForRaw Text, FieldForRaw MatchQuality)]
             ]
    return (concat matchess)
    
    
instance Csv.FromField (Either InvalidField (Maybe (ValidField MatchQuality))) where
  parseField field = do
    mt <- Csv.parseField field
    return $ case mt of
      Nothing -> Right Nothing
      Just t -> case readMatchQuality t of
        Nothing -> Left (ParsingError "MatchQuality" t)
        Just m -> Right (Just $ Provided m)

-- | Parses pairs colour quality, pairs are separated by , and colour quality as well if needed
-- Accepts format like
-- Black+++,Navy:Excellent
-- Black Excellent Navy Excellent
-- Black +++ Navy+++
instance Csv.FromField [(Either InvalidField (Maybe (ValidField Text)), Either InvalidField (Maybe (ValidField MatchQuality)))] where
  parseField field | null field = return []
  parseField field = do
    let parseQuality colour line = do
          let (qualityBS, rest) = break (`elem` (",;\t" :: ByteString)) line
          quality <- Csv.parseField qualityBS
          rets <- Csv.parseField (stripBS ",;\t " rest)
          return $ (colour, fromMaybe (Right (Just $ Provided Close)) quality) : rets
        (colourBS, rest) = break (`elem` (":+=," :: ByteString)) field
    colour <- Csv.parseField colourBS
    parseQuality (Right colour) (stripBS " :=" rest)


stripBS :: ByteString -> ByteString -> ByteString
stripBS seps = snd . span (`elem` seps)
readMatchQuality :: Text -> Maybe MatchQuality
readMatchQuality t = readMay (toTitle t) <|> case t of
  "+" -> Just Fair
  "-" -> Just Close
  "++" -> Just Good
  "+++" -> Just Excellent
  "---" -> Just Bad
  "===" -> Just Identical
  _ -> Nothing
  


instance Transformable (Entity Batch) Text where
  transform (Entity _ batch) = batchName batch

-- * Parsing
parseMatchRows :: Text -> ByteString -> Handler (ParsingResult (MatchRow 'RawT) [MatchRow 'ValidT])
parseMatchRows batchCategory bytes = do
  case parseMatchRowsGo bytes of
    Left result -> return result
    Right raw'filleds -> do
      raw'validEs  <- mapM (mapM $ validateRow batchCategory) raw'filleds
      -- check if everything is valid
      return $ case sequence (map snd raw'validEs) of
                 Right valids -> ParsingCorrect valids
                 Left _ -> InvalidData [] (lefts (map snd raw'validEs)) [ either id (const raw)  rawOrValid | (raw, rawOrValid) <- raw'validEs ]
      
        
parseMatchRowsGo bytes = do -- Either
      let columnMap = mapFromList $ map (,[]) ["Source", "SourceColour", "TargetColour", "Quality", "Comment"]
          columnMap' = mapFromList $ map (,[]) ["SKU"]
      raws <- ( parseSpreadsheet columnMap Nothing bytes <> (concat <$> parseSpreadsheet columnMap' Nothing bytes)) <|&> WrongHeader 
      let rawOrPartials = map (\raw -> sequenceMatchRow raw <|&> const raw) raws :: [Either (MatchRow 'RawT) (MatchRow 'PartialT)]
      partials <- sequence rawOrPartials <|&> const (InvalidFormat . lefts $ rawOrPartials)
      let filleds = case partials of
            [] -> []
            (row:rows) -> scanl fillFromPrevious row rows
      return $ zip raws filleds -- we return the raws row as well to be able to convert it back
      -- to raw in validation failed on other row
  

-- | sequence through fields functor
sequenceMatchRow MatchRow{..} = MatchRow <$> source <*> sourceColour
                                          <*> targetColour <*> target
                                          <*> quality <*> comment
                    
-- | Fill missing field from previous row if needed
fillFromPrevious :: MatchRow 'PartialT -> MatchRow 'PartialT -> MatchRow 'PartialT
fillFromPrevious p@(MatchRow source0 _sourceColour0 _targetColour0 target0 _quality0 _comment0)
                  r@(MatchRow source sourceColour targetColour target quality comment)
  = MatchRow (source <|> guess <$> source0 )
             sourceColour
             targetColour
             (target <|> guess <$> target0)
             quality
             comment

validateRow :: Text -> MatchRow 'PartialT -> Handler (Either (MatchRow 'RawT) (MatchRow 'ValidT))
validateRow batchCategory row@(MatchRow (Just sourcet) (sourceColourm)
                      (targetColourm) (Just targett)
                      (Just quality) comment
            )
  = do
  sourcem <- findBatch batchCategory (validValue sourcet) (validValue <$> sourceColourm)
  targetm <- findBatch batchCategory (validValue targett) (validValue <$> targetColourm)
  case (sourcem, targetm) of
    (Right (sourceBatch, sourceColour'), Right (targetBatch, targetColour')) -> let
      guessForSource :: a -> ValidField a
      guessForSource v = (\_ _ ->  v) <$> sourcet <*> (sequenceA sourceColourm) -- get the correct guessed status
      guessForTarget :: a -> ValidField a
      guessForTarget v = (\_ _ ->  v) <$> targett <*> (sequenceA targetColourm) -- get the correct guessed status
      source = guessForSource sourceBatch
      sourceColour = guessForSource sourceColour'
      target = guessForTarget targetBatch
      targetColour = guessForTarget targetColour'
      in return . Right $ MatchRow{..}
    _ -> return . Left $ let
                         sourceField = case sourcem of
                                           Left err -> Left (InvalidValueError err (validValue sourcet) )
                                           Right _ -> Right (transform sourcet)
                         targetField = case targetm of
                                           Left err -> Left (InvalidValueError err (validValue targett) )
                                           Right _ -> Right (transform targett)
                         in (validateRow' row)  { source = sourceField, target = targetField}

validateRow _ row = return $ Left (validateRow' row)

validateRow' :: MatchRow 'PartialT -> MatchRow 'RawT
validateRow' (MatchRow sourcem sourceColourm targetColourm targetm qualitym commentm) = 
  let source = validateNonEmpty "Source" $ Right sourcem
      sourceColour = validateNonEmpty "Source Colour" $ Right sourceColourm
      targetColour = validateNonEmpty "Target Colour" $ Right targetColourm
      target = validateNonEmpty "Target" $ Right targetm
      quality = validateNonEmpty "Quality" $ Right qualitym
      comment = Right commentm
  in MatchRow{..}

  
-- * Batch finding
-- Retrieve a batch given a batch name, a sku and a batch category
-- The batch category, allows to find the batch name given a sku.
-- Alternatively, if a sku is given as colour, we need to extract the colour from
findBatch :: Text -> Text -> Maybe Text -> Handler (Either Text (Entity Batch, Text))
findBatch batchCategory styleOrBatch (Just colourOrSku) = do
  -- try batch
  batchByName <- runDB $ getBy (UniqueBName styleOrBatch)
  case batchByName of
    Just batch -> return $ Right (batch, colourOrSku)
    Nothing -> do
      batchE <- findBatchForSku batchCategory (styleVarToSku styleOrBatch colourOrSku)
      case batchE of
        r@(Right _) -> return r
        l -> do -- try to extract the colour from the colourOrSku
          batchM <- findBatchForBatchSku batchCategory styleOrBatch colourOrSku
          return $ fromMaybe l batchM

findBatch batchCategory sku Nothing = findBatchForSku  batchCategory sku
findBatchForSku :: Text -> Text -> Handler (Either Text (Entity Batch, Text))
findBatchForSku batchCategory sku = do
  skuToStyleVar <- skuToStyleVarH
  catFinder <- categoryFinderCached
  categories <- categoriesH
  let (_, colour) = skuToStyleVar sku
  case (batchCategory `elem` categories,  colour) of
    (False, _ ) -> do
      setError("Batch category not set. Please contact your Administrator")
      return $  Left "Batch category not set"
    (True, "" ) -> do
      setError(toHtml $ sku <> " doesn't have a colour")
      return $ Left $ sku <> " doesn't have a colour"
    (True, _) -> do
      let batchm = catFinder batchCategory (FA.StockMasterKey sku)
      case batchm of
        Nothing -> return $ Left $ "no " <> batchCategory <> " value for " <> sku
        Just batch -> do
          batchByName <- runDB $ getBy (UniqueBName batch)
          return $  maybe (Left $ batch <> " is not a valid batch") (Right . (,colour)) batchByName
-- If the colour is a sku, find the associated colour and check that the
-- the input batch belongs to the batch category
findBatchForBatchSku :: Text -> Text -> Text -> Handler (Maybe (Either Text (Entity Batch, Text)))
findBatchForBatchSku batchCategory batchName sku =  do
  skuToStyleVar <- skuToStyleVarH
  let (_, colour) =  skuToStyleVar sku
  if null colour
  then return $ Nothing -- sku doesn't give a colour
  else do
     batchM  <- runDB $ getBy (UniqueBName batchName)
     case batchM of
        Nothing -> return . Just . Left $ batchName <> " is not a valid batch"
        Just batch -> do -- check the batch is authorized
            catFinder <- categoryFinderCached
            let batchNames = catFinder batchCategory (FA.StockMasterKey sku)
                validBatches = maybe [] (splitOn " | ")  batchNames
            if batchName `elem` validBatches
            then return . Just $ Right (batch, colour)
            else return . Just . Left $ batchName <> " doesn't belong current batches for "  <> sku

  


  
  

-- | convert a valid row to an original raw row, so that it can be displayed
unvalidateRow  :: MatchRow 'ValidT -> MatchRow 'RawT
unvalidateRow (MatchRow source sourceColour targetColour target quality comment) =
    MatchRow (transform (fmap transform source :: ValidField Text))
             (transform sourceColour)
             (transform targetColour)
             (transform (fmap transform target :: ValidField Text))
             (transform quality)
             (transform comment)

finalizeRow :: MatchRow 'ValidT -> MatchRow 'FinalT
finalizeRow (MatchRow source sourceColour targetColour target quality comment) =
    MatchRow (transform source)
             (transform sourceColour)
             (transform targetColour)
             (transform target)
             (transform quality)
             (transform comment)
finalRowToMatch :: Day -> Maybe OperatorId -> DocumentKeyId -> MatchRow 'FinalT -> BatchMatch
finalRowToMatch date operatorId docKey MatchRow{..} =
  BatchMatch (entityKey source)
              sourceColour
              (entityKey target)
              targetColour
              operatorId
              quality
              comment
              date
              docKey
              
-- * DB
loadBatchMatchesFor :: [Key Batch] -> SqlHandler [Entity BatchMatch]
loadBatchMatchesFor batchIds = do
  -- load everything with
  selectList [BatchMatchSource <-. batchIds, BatchMatchTarget <-. batchIds ] []

-- inverse source and target
reverseBatchMatch :: BatchMatch -> BatchMatch
reverseBatchMatch BatchMatch{..} = BatchMatch{ batchMatchSource=batchMatchTarget,batchMatchSourceColour=batchMatchTargetColour
                                             , batchMatchTarget=batchMatchSource, batchMatchTargetColour=batchMatchSourceColour,..}


-- * Match Table
-- | Assures all source are source and target are target regardless of the database order
newtype ForBuildTable = ForBuildTable [BatchMatch] 
buildTable :: ([(Text, MatchQuality)] -> Html) -- ^ qualitys rendered
           -> [Entity Batch] -- ^ rows
           -> [Entity Batch] -- ^ columns
           -> ForBuildTable -- ^ matches
           -> ( [ (Text, (Key Batch, Text) -> Maybe (Either Html PersistValue)) ] -- ^ COLUMNS : Column name, getter
              , ((Text, (Key Batch, Text) -> Maybe (Either Html PersistValue)) -> (Html, [Text])) -- ^ COLUMN NAME: from column (as above)
              , [ ((Text, (Key Batch, Text) -> Maybe (Either Html PersistValue)) -> Maybe (Html, [Text]), [Text]) ] -- ^ ROWS: column (as above) -> value (via getter) 
              )
buildTable renderColour'Qualitys rowBatches columnBatches (ForBuildTable matches) = let
  matchMap = groupAsMap (\BatchMatch{..} -> (batchMatchSource, batchMatchSourceColour, batchMatchTarget))
                        (\BatchMatch{..} -> [(batchMatchTargetColour, batchMatchQuality)])
                        matches

  source'colours = nub $ sort (map ((,) <$> batchMatchSource <*> batchMatchSourceColour ) matches)
  -- targets = nub $ sort (map batchMatchTarget matches)
  columns0 = [ ( fromMaybe batchName batchAlias -- Column name :: Text
               , \(source, colour) -> Left . renderColour'Qualitys <$> lookup (source, colour, batchId ) matchMap
                                  
               )
            | (Entity batchId Batch{..}) <- columnBatches
            ]
  -- columns = 
  columns = ("Style/Batch", \(source, _colour) -> lookup source batchMap <&> (Right . toPersistValue)) :
            ("Colour", \(_source, colour) -> Just (Right $ toPersistValue colour)) :
            columns0

  batchMap = groupAsMap entityKey  (\(Entity _ Batch{..}) -> fromMaybe batchName batchAlias ) (rowBatches <> columnBatches)
  rowsForTables = [ (colFn, [])
                  | source'colour <- source'colours
                  , let colFn (_colname,  getter) = getter source'colour <&> (,[]) . either id (toHtml . renderPersistValue)
                  ]
  in ( columns
     , \(col, _) -> (toHtml col, [])
     , rowsForTables
     )

buildTableForSku renderColour'Qualitys sku'batches columnBatches (ForBuildTable matches) = let
  matchMap = groupAsMap (\BatchMatch{..} -> (batchMatchSource, batchMatchSourceColour, batchMatchTarget))
                        (\BatchMatch{..} -> [(batchMatchTargetColour, batchMatchQuality)])
                        matches

  -- targets = nub $ sort (map batchMatchTarget matches)
  columns0 = [ ( fromMaybe batchName batchAlias -- Column name :: Text
               , \((style, colour), batch ) -> Left . renderColour'Qualitys <$> lookup (entityKey batch, colour, batchId ) matchMap
                                  
               )
            | (Entity batchId Batch{..}) <- columnBatches
            ]
  -- columns = 
  columns = ("Style", \((style, _var), _batch) -> Just (Right $ toPersistValue style) ) :
            ("Colour", \((_style, colour), _batch) -> Just (Right $ toPersistValue colour)) :
            ("Batch", \(_, Entity _ Batch{..}) -> Just (Right $ toPersistValue (fromMaybe batchName batchAlias))) :
            columns0

  -- batchMap = groupAsMap entityKey  (\(Entity _ Batch{..}) -> fromMaybe batchName batchAlias ) (columnBatches)
  rowsForTables = [ (colFn, [])
                  | sku'batch <- sortOn fst sku'batches
                  , let colFn (_colname,  getter) = getter sku'batch <&> (,[]) . either id (toHtml . renderPersistValue)
                  ]
  in ( columns
     , \(col, _) -> (toHtml col, [])
     , rowsForTables
     )
colour'QualityToHtml (colour, quality) = [shamlet|#{colour}
   <sup>#{tshow quality}|]
colour'QualitysToHtml = mconcat . map colour'QualityToHtml 

-- | Load batches from sku but return on modified batch for each sku
-- with the alias set to the style name
-- lookp the batch name be "inside" the actual batch category
loadSkuBatches :: Text -> FilterExpression -> SqlHandler [(Text, Key Batch)]
loadSkuBatches batchCategory filterE = do
  let sql = "SELECT stock_id, batch_id FROM fames_item_category_cache "
            <> " JOIN fames_batch ON (name RLIKE concat('(.*| )*', value, '( |.*)*')) "
            <> "WHERE category = ? AND stock_id " <> keyw <> " ?"
      (keyw, v )  = filterEKeyword filterE
  rows <- rawSql sql [toPersistValue batchCategory, toPersistValue v]
  return $ map (\(Single sku, Single batchId) -> (sku, batchId)) rows


skuToStyle''var'Batch :: (Text -> (Text, Text)) -- ^ Category
                      -> [Entity Batch] -- ^ available batches
                      -> (Text, Key Batch) -- ^ Sku, batch Id
                      -> ((Text, Text), Entity Batch)
skuToStyle''var'Batch skuToStyleVar batches = let
  -- split in two function to memoize the batchMap
  -- GHC might do it
  batchMap = mapFromList $ map (fanl entityKey) batches :: Map (Key Batch) (Entity Batch)
  in \(sku, batchId) -> case lookup batchId batchMap of
                        Nothing -> error $ "Batch " <> show batchId <> " not found."
                        Just batch -> (skuToStyleVar sku, batch)
                      
       
  
     

--   [ BatchMatch {batchCategory}
--   | ((style, var), batchId) <- style'var''batchId
--   , BatchMatch{..}  <- fromMaybe [] (lookup (batchId, var) matchMap)
--   ]







  
-- * Rendering
instance Renderable ([MatchRow 'RawT]) where
  render rows = [whamlet|
<table.table.table-hover>
  <tr>
    <th>Source
    <th>Source Colour
    <th>Target Colour
    <th>Target
    <th>Quality
    <th>Comment
  $forall row <- rows
    <tr>
      <td>^{render $ source row }
      <td>^{render $ sourceColour row }
      <td>^{render $ targetColour row }
      <td>^{render $ target row }
      <td>^{render $ fmap (fmap $ fmap tshow) (quality row) }
      <td>^{render $ comment row }
     |]
