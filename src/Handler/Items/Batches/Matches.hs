{-# LANGUAGE DataKinds #-}
module Handler.Items.Batches.Matches where

import Import hiding((.:))
import qualified Data.Csv as Csv
import Data.List(scanl)
import Handler.CsvUtils
import Data.Text(toTitle)

-- * Types

data MatchRow (s :: RowTypes) = MatchRow
  { source :: FieldTF s Text
  , sourceColour :: FieldTF s Text
  , targetColour :: FieldTF s Text
  , target :: FieldTF s Text
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
  

-- * Parsing
parseMatchRows :: ByteString -> SqlHandler (ParsingResult (MatchRow 'RawT) [MatchRow 'ValidT])
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
                 Left _ -> InvalidData [] [] [ either id (const raw)  rawOrValid | (raw, rawOrValid) <- raw'validEs ]
      
        
  

-- | sequence through fields functor
sequenceMatchRow MatchRow{..} = MatchRow <$> source <*> sourceColour
                                          <*> targetColour <*> target
                                          <*> quality <*> comment
                    
-- | Fill missing field from previous row if needed
fillFromPrevious :: MatchRow 'PartialT -> MatchRow 'PartialT -> MatchRow 'PartialT
fillFromPrevious (MatchRow source0 _sourceColour0 _targetColour0 target0 _quality0 _comment0)
                  (MatchRow source sourceColour targetColour target quality comment)
  = MatchRow (source <|> source0 )
             sourceColour
             targetColour
             (target <|> target0)
             quality
             comment

validateRow :: MatchRow 'PartialT -> SqlHandler (Either (MatchRow 'RawT) (MatchRow 'ValidT))
validateRow (MatchRow (Just source) (Just sourceColour)
                      (Just targetColour) (Just target)
                      (Just quality) comment
            )
  = do
  -- validation
  return . Right $ MatchRow{..}
validateRow (MatchRow sourcem sourceColourm targetColourm targetm qualitym commentm) = do
  let source = validateNonEmpty "Source" $ Right sourcem
      sourceColour = validateNonEmpty "Source Colour" $ Right sourceColourm
      targetColour = validateNonEmpty "Target Colour" $ Right targetColourm
      target = validateNonEmpty "Target" $ Right targetm
      quality = validateNonEmpty "Quality" $ Right qualitym
      comment = Right commentm
  return $ Left $ MatchRow{..}

  

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
