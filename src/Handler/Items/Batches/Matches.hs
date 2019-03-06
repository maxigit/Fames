{-# LANGUAGE DataKinds #-}
module Handler.Items.Batches.Matches where

import Import hiding((.:))
import qualified Data.Csv as Csv
import Handler.CsvUtils

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
readMatchQuality t = readMay t <|> case t of
  "+" -> Just Fair
  "-" -> Just Closest
  "++" -> Just Good
  "+++" -> Just Excellent
  _ -> Nothing
  

-- * Parsing
parseMatchRows :: ByteString -> ParsingResult (MatchRow 'RawT) [MatchRow 'FinalT]
parseMatchRows bytes = either id ParsingCorrect $ do
  let columnMap = mapFromList $ map (,[]) ["Source", "SourceColour", "TargetColour", "Quality", "Comment"]
  raws <- parseSpreadsheet columnMap Nothing bytes <|&> WrongHeader
  let rawsE = map (\raw -> sequenceMatchRow raw <|&> const raw) raws :: [Either (MatchRow 'RawT) (MatchRow 'PartialT)]
  rows <- sequence rawsE <|&> const (InvalidFormat . lefts $ rawsE)
  return []
  

-- | sequence
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
             (target <|> target)
             quality
             comment

validateRow :: MatchRow 'PartialT -> Handler (Either (MatchRow 'RawT) (MatchRow 'ValidT))
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

  
