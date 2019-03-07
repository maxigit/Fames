{-# LANGUAGE DataKinds #-}
module Handler.Items.Batches.Matches where

import Import hiding((.:))
import qualified Data.Csv as Csv
import Data.List(scanl)
import Handler.CsvUtils
import Handler.Items.Common
import Data.Text(toTitle)
import qualified FA

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
instance Csv.FromNamedRecord (MatchRow  'RawT) where
  parseNamedRecord  m = MatchRow
    <$> m .: "Source" 
    <*> m .: "SourceColour"
    <*> m .: "TargetColour"
    <*> m .: "Target"
    <*> m Csv..: "Quality"
    <*> m .: "Comment"
    
instance Csv.FromField (Either InvalidField (Maybe (ValidField MatchQuality))) where
  parseField field = do
    mt <- Csv.parseField field
    return $ case mt of
      Nothing -> Right Nothing
      Just t -> case readMatchQuality t of
        Nothing -> Left (ParsingError "MatchQuality" t)
        Just m -> Right (Just $ Provided m)


readMatchQuality :: Text -> Maybe MatchQuality
readMatchQuality t = readMay (toTitle t) <|> case t of
  "+" -> Just Fair
  "-" -> Just Closest
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
      raws <- parseSpreadsheet columnMap Nothing bytes <|&> WrongHeader
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
