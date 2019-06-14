module GL.TaxReport.Settings where
import ClassyPrelude
import Data.Aeson.TH(deriveJSON, defaultOptions, fieldLabelModifier, sumEncoding, SumEncoding(..))
import Data.Aeson.Types
import GL.Payroll.Settings
import GL.TaxReport.Types
import Data.Text(strip)
-- * Type
-- | Main settins to define a report.
-- The actual report name should be in the key map
data TaxReportSettings  = TaxReportSettings
  { startDate :: Day -- ^ starting period of the first report
  , nextPeriod :: DateCalculator -- ^ how to calculate the next start from the last start
  , deadline :: Maybe DateCalculator -- ^ deadline to submit the report
  , referenceFormat :: Text -- ^ format to use with format time to create reference
  , rules :: Rule
  , boxes :: [TaxBox]
  , processor :: TaxProcessor
  }
  deriving (Eq, Read, Show)
data TaxProcessor
  = HMRCProcessor HMRCProcessorParameters 
  | ManualProcessor  ManualProcessorParameters-- ^ Don't submit anything but set the submitted date using the deadline or the submission date
  deriving (Eq, Read, Show)

data HMRCProcessorParameters = HMRCProcessorParameters
       { baseUrl :: Text
       , clientId :: Text
       , clientSecret :: Text
       , customerId :: Text
       }
  deriving (Eq, Read, Show)
data ManualProcessorParameters = ManualProcessorParameters
    { submissionDate :: Maybe DateCalculator -- ^ submission date calculated next period
    }
  deriving (Eq, Read, Show)

-- * JSON
-- $(deriveJSON defaultOptions ''TaxReport.Bucket)

-- use then key of an object as the boxname and nested fields as  field
instance FromJSON TaxBox where
  -- String use the name as the bucket
  parseJSON v = withText "taxbox as string" (\s -> return $ TaxBox s Nothing Nothing (TaxBoxGross s ) ) v
            <|> withObject "taxbox" parseObject v where
    parseObject o =  do
      case  mapToList o of
        [(tbName, fields)] -> do
          let parseFields o = do
                  tbDescription  <- o .:? "description"
                  shouldBe <- o .:? "shouldBe"
                  let tbShouldBe = case (take 3 . unpack . toLower) <$> (shouldBe :: Maybe Text) of
                        Just "neg" -> Just LT
                        Just "pos" -> Just GT
                        Just "nul" -> Just EQ
                        Just "0" -> Just EQ
                        _ -> Nothing
                  -- allow simple rule or rule list
                  tbRule <- (o .: "rules") <|> (TaxBoxSum <$> o .: "rules")
                  return TaxBox{tbRule=negateBucket tbRule,..}
              parseBucket (stripPrefix "-" -> Just bucket)  = do
                TaxBox{..} <- parseBucket bucket
                return TaxBox{tbRule=TaxBoxNegate tbRule, tbShouldBe= Just LT,..}
              parseBucket (stripPrefix "+" -> Just bucket)  = do
                TaxBox{..} <- parseBucket bucket
                return TaxBox{tbRule=tbRule, tbShouldBe = Just GT,..}
              parseBucket s  = case s of
                "net" -> return  $ TaxBox tbName (Just $ tbName <> " net") Nothing (TaxBoxNet tbName )
                "tax" -> return  $ TaxBox tbName (Just $ tbName <> " tax") Nothing (TaxBoxTax tbName )
                "gross" -> return  $ TaxBox tbName (Just $ tbName <> " gross") Nothing (TaxBoxGross tbName )
                _ -> fail "Not a valid amount type"

          withText ("TaxBox bucket" <> show fields) parseBucket fields
            <|> withObject "TaxBox fields" parseFields fields
        _ -> fail $ "Too many keys for " <> show o

-- | Negate bucket rule when name start with -
negateBucket :: TaxBoxRule -> TaxBoxRule
negateBucket rule = case rule of
  TaxBoxNet (stripPrefix "-" -> Just bucket) -> TaxBoxNegate $ TaxBoxNet (strip bucket) 
  TaxBoxTax (stripPrefix "-" -> Just bucket) -> TaxBoxNegate $ TaxBoxTax (strip bucket) 
  TaxBoxGross (stripPrefix "-" -> Just bucket) -> TaxBoxNegate $ TaxBoxGross (strip bucket) 
  TaxBoxTaxWith rate (stripPrefix "-" -> Just bucket) -> TaxBoxNegate $ TaxBoxTaxWith rate (strip bucket) 
  TaxBoxSum rules -> TaxBoxSum (negateBucket <$> rules)
  TaxBoxSub r1 r2 -> TaxBoxSub (negateBucket r1) (negateBucket r2)
  TaxBoxFloor rule -> TaxBoxFloor (negateBucket rule)
  TaxBoxCeil rule -> TaxBoxCeil (negateBucket rule) 
  TaxBoxRound rule -> TaxBoxRound (negateBucket rule)
  TaxBoxBanker rule -> TaxBoxBanker (negateBucket rule)
  other -> other


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
$(deriveJSON defaultOptions ''HMRCProcessorParameters)
$(deriveJSON defaultOptions ''ManualProcessorParameters)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField
                            , fieldLabelModifier = fromMaybe <*> stripPrefix "tp"
                            , constructorTagModifier = fromMaybe <*> stripSuffix "Processor"
                            } ''TaxProcessor)
$(deriveJSON defaultOptions ''TaxReportSettings)

