{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Items.Batches.Matches where

import Import hiding((.:))
import qualified Data.Csv as Csv
import Data.List(scanl, nub)
import Handler.CsvUtils
import Handler.Items.Common
import Data.Text(toTitle)
import qualified FA
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
        batchNames = filter (`notElem` ["SKU", "Source", "SourceColour"]) columns

    sku <- m .: "SKU"
    matchess <- forM batchNames $ \batchNameBS -> do
      col'qs <- m Csv..: batchNameBS
      batchName <- Csv.parseField batchNameBS
      return [ MatchRow sku (Right Nothing)
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
  _ -> Nothing
  


instance Transformable (Entity Batch) Text where
  transform (Entity _ batch) = batchName batch

-- * Parsing
parseMatchRows :: ByteString -> Handler (ParsingResult (MatchRow 'RawT) [MatchRow 'ValidT])
parseMatchRows bytes = do
  let resultE = do -- Either
      let columnMap = mapFromList $ map (,[]) ["Source", "SourceColour", "TargetColour", "Quality", "Comment"]
          columnMap' = mapFromList $ map (,[]) ["SKU"]
      raws <- ( parseSpreadsheet columnMap Nothing bytes 
               <> (concat <$> parseSpreadsheet columnMap' Nothing bytes)
              )
             <|&> WrongHeader 
      let rawOrPartials = map (\raw -> sequenceMatchRow raw <|&> const raw) raws :: [Either (MatchRow 'RawT) (MatchRow 'PartialT)]
      partials <- sequence rawOrPartials <|&> const (InvalidFormat . lefts $ rawOrPartials)
      let filleds = case partials of
            [] -> []
            (row:rows) -> scanl fillFromPrevious row rows
      return $ zip raws filleds -- we return the raws row as well to be able to convert it back
      -- to raw in validation failed on other row
  case resultE of
    Left result -> return result
    Right raw'filleds -> do
      raw'validEs  <- mapM (mapM validateRow) raw'filleds
      -- check if everything is valid
      return $ case sequence (map snd raw'validEs) of
                 Right valids -> ParsingCorrect valids
                 Left _ -> InvalidData [] (lefts (map snd raw'validEs)) [ either id (const raw)  rawOrValid | (raw, rawOrValid) <- raw'validEs ]
      
        
  

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

validateRow :: MatchRow 'PartialT -> Handler (Either (MatchRow 'RawT) (MatchRow 'ValidT))
validateRow row@(MatchRow (Just sourcet) (sourceColourm)
                      (targetColourm) (Just targett)
                      (Just quality) comment
            )
  = do
  sourcem <- findBatch (validValue sourcet) (validValue <$> sourceColourm)
  targetm <- findBatch (validValue targett) (validValue <$> targetColourm)
  case (sourcem, targetm) of
    (Just (sourceBatch, sourceColour'), Just (targetBatch, targetColour')) -> let
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
                                           Nothing -> Left (InvalidValueError "Batch not found" (validValue sourcet) )
                                           Just _ -> Right (transform sourcet)
                         targetField = case targetm of
                                           Nothing -> Left (InvalidValueError "Batch not found" (validValue targett) )
                                           Just _ -> Right (transform targett)
                         in (validateRow' row)  { source = sourceField, target = targetField}

validateRow row = return $ Left (validateRow' row)

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
findBatch :: Text -> Maybe Text -> Handler (Maybe (Entity Batch, Text))
findBatch styleOrBatch (Just colour) = do
  -- try batch
  batchByName <- runDB $ getBy (UniqueBName styleOrBatch)
  case batchByName of
    Just batch -> return $ Just (batch, colour)
    Nothing -> findBatchForSku (styleVarToSku styleOrBatch colour)
findBatch sku Nothing = findBatchForSku  sku
findBatchForSku :: Text -> Handler (Maybe (Entity Batch, Text))
findBatchForSku sku = do
  skuToStyleVar <- skuToStyleVarH
  catFinder <- categoryFinderCached
  categories <- categoriesH
  let batchS = "batch"
      (_, colour) = skuToStyleVar sku
  case (batchS `elem` categories,  colour) of
    (False, _ ) -> do
      setError("Batch category not set. Please contact your Administrator")
      return Nothing
    (True, "" ) -> do
      setError(toHtml $ sku <> " doesn't have a a colour")
      return Nothing
    (True, _) -> do
      let batchm = catFinder batchS (FA.StockMasterKey sku)
      case batchm of
        Nothing -> return Nothing
        Just batch -> do
          batchByName <- runDB $ getBy (UniqueBName batch)
          return $  batchByName <&> (,colour)
  

  
  

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
                  | sku'batch <- sku'batches
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
loadSkuBatches :: FilterExpression -> SqlHandler [(Text, Key Batch)]
loadSkuBatches filterE = do
  let sql = "SELECT stock_id, batch_id FROM fames_item_category_cache "
            <> " JOIN fames_batch ON (value = name) "
            <> "WHERE category = 'batch' AND stock_id " <> keyw <> "?"
      (keyw, v )  = filterEKeyword filterE
  rows <- rawSql sql [toPersistValue v]
  return $ map (\(Single sku, Single batchId) -> (sku, batchId)) rows


skuToStyle''var'Batch :: (Text -> (Text, Text)) -> [Entity Batch] -> (Text, Key Batch) -> ((Text, Text), Entity Batch)
skuToStyle''var'Batch skuToStyleVar batches = let
  -- split in two function to memoize the batchMap
  -- GHC might do it
  batchMap = mapFromList $ map (fanl entityKey) batches :: Map (Key Batch) (Entity Batch)
  in \(sku, batchId) -> case lookup batchId batchMap of
                        Nothing -> error $ "Batch " <> show batchId <> " not found."
                        Just batch -> (skuToStyleVar sku, batch)
                      
       
  
     

--   [ BatchMatch {batchS}
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
