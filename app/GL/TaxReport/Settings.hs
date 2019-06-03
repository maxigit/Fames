module GL.TaxReport.Settings where
import ClassyPrelude
import Data.Aeson.TH(deriveJSON, defaultOptions, fieldLabelModifier, sumEncoding, SumEncoding(..))
import Data.Aeson.Types
import GL.Payroll.Settings
import GL.TaxReport.Types
-- * Type
-- | Main settins to define a report.
-- The actual report name should be in the key map
data TaxReportSettings  = TaxReportSettings
  { startDate :: Day -- ^ starting period of the first report
  , nextPeriod :: DateCalculator -- ^ how to calculate the next start from the last start
  , referenceFormat :: Text -- ^ format to use with format time to create reference
  , rules :: Rule
  , boxes :: [TaxBox]
  }
  deriving (Eq, Read, Show)

-- * JSON
-- $(deriveJSON defaultOptions ''TaxReport.Bucket)

-- use then key of an object as the boxname and nested fields as  field
instance FromJSON TaxBox where
  -- String use the name as the bucket
  parseJSON v = withText "taxbox as string" (\s -> return $ TaxBox s Nothing (TaxBoxGross s ) ) v
            <|> withObject "taxbox" parseObject v where
    parseObject o =  do
      case  mapToList o of
        [(tbName, fields)] -> do
          let parseFields o = do
                  tbDescription  <- o .:? "description"
                  -- allow simple rule or rule list
                  tbRule <- (o .: "rules") <|> (TaxBoxSum <$> o .: "rules")
                  return TaxBox{..}
              parseBucket s  = case s of
                "net" -> return  $ TaxBox tbName (Just $ tbName <> " net") (TaxBoxNet tbName )
                "tax" -> return  $ TaxBox tbName (Just $ tbName <> " tax") (TaxBoxTax tbName )
                "gross" -> return  $ TaxBox tbName (Just $ tbName <> " gross") (TaxBoxGross tbName )
                _ -> fail "Not a valid amount type"

          withText ("TaxBox bucket" <> show fields) parseBucket fields
            <|> withObject "TaxBox fields" parseFields fields
        _ -> fail $ "Too many keys for " <> show o
instance ToJSON TaxBox where
  toJSON TaxBox{..} = object [tbName .= object fields] where
      fields = catMaybes [ ("description" .=) <$> tbDescription 
                         , Just $ "rules" .= tbRule
                         ]
    
  
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField
                            , fieldLabelModifier = (fromMaybe <*> stripSuffix "Rule")
                                                 . (fromMaybe <*> stripPrefix "rule")
                            , constructorTagModifier = (fromMaybe <*> stripSuffix "Rule")
                                                       . (fromMaybe <*> stripPrefix "Rule")
                            } ''Rule)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField
                            , fieldLabelModifier = fromMaybe <*> stripPrefix "TaxBox"
                            , constructorTagModifier = fromMaybe <*> stripPrefix "TaxBox"
                            } ''TaxBoxRule)
$(deriveJSON defaultOptions ''TaxReportSettings)

