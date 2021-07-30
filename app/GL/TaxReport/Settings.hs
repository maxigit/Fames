{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module GL.TaxReport.Settings 
( TaxReportSettings(..)
, TaxProcessorSettings(..)
, HMRCProcessorParameters(..)
, ManualProcessorParameters(..)
, ECSLProcessorParameters(..)
)
where
import ClassyPrelude
import Data.Aeson.TH(deriveJSON, defaultOptions, fieldLabelModifier, sumEncoding, SumEncoding(..))
import Data.Aeson.Types
import GL.Payroll.Settings
import GL.TaxReport.Types
import Data.Text(strip)
import Util.Decimal
-- * Type
-- | Main settins to define a report.
-- The actual report name should be in the key map
data TaxReportSettings  = TaxReportSettings
  { startDate :: Day -- ^ starting period of the first report
  , nextPeriod :: DateCalculator -- ^ how to calculate the next start from the last start
  , deadline :: Maybe DateCalculator -- ^ deadline to submit the report
  , referenceFormat :: Text -- ^ format to use with format time to create reference
  , rules :: Rule
  , boxesRaw :: [TaxBox] -- ^ should only be called by processor
  , processor :: TaxProcessorSettings
  }
  deriving (Eq, Read, Show)
data TaxProcessorSettings
  = HMRCProcessor HMRCProcessorParameters 
  | ManualProcessor  ManualProcessorParameters-- ^ Don't submit anything but set the submitted date using the deadline or the submission date
  | ECSLProcessor ECSLProcessorParameters -- ^ European community VAT
  deriving (Eq, Read, Show)

data HMRCProcessorParameters = HMRCProcessorParameters
       { baseUrl :: Text
       , clientId :: Text
       , clientSecret :: Text
       , customerId :: Text
       , vatNumber :: Text
       , redirectURI :: Maybe Text -- ^ base url to redirect to
        -- # if DEVELOPMENT
       , obligationTestScenario :: Maybe Text -- ^ He
       , submitTestScenario :: Maybe Text -- ^ He
       -- # endif
       , govClientTimezone :: Text
       , govClientLocalIPs :: Text
       , govClientDeviceId :: Maybe Text
       , govVendorProductName :: Text
       }
  deriving (Eq, Read, Show)
data ManualProcessorParameters = ManualProcessorParameters
    { submissionDate :: Maybe DateCalculator -- ^ submission date calculated next period
    }
  deriving (Eq, Read, Show)
data ECSLProcessorParameters = ECSLProcessorParameters
  { outOfScope :: Maybe Bucket -- ^ Bucket used for tax out of ECSL scope
  , vatNumber :: Text
  , branch :: Text
  }
  deriving (Eq, Read, Show)

-- * JSON
-- $(deriveJSON defaultOptions ''TaxReport.Bucket)

-- use then key of an object as the boxname and nested fields as  field
instance FromJSON TaxBox where
  -- String use the name as the bucket
  parseJSON v = withText "taxbox as string" (\s -> return $ TaxBox s Nothing Nothing (TaxBoxGross s ) Nothing ) v
            <|> withObject "taxbox" parseObject v where
    parseObject o0 =  do
      case  mapToList o0 of
        [(l_tbName, fields)] -> do
          let parseFields o = do
                  tbDescription  <- o .:? "description"
                  shouldBe <- o .:? "shouldBe"
                  tbRound <- o .:? "rounding"
                  let tbShouldBe = case (take 3 . unpack . toLower) <$> (shouldBe :: Maybe Text) of
                        Just "neg" -> Just LT
                        Just "pos" -> Just GT
                        Just "nul" -> Just EQ
                        Just "0" -> Just EQ
                        _ -> Nothing
                  -- allow simple rule or rule list
                  tbRule <- (o .: "rules") <|> (TaxBoxSum <$> o .: "rules")
                  return TaxBox{tbRule=negateBucket tbRule,tbName=l_tbName,..}
              parseBucket (stripPrefix "-" -> Just bucket)  = do
                TaxBox{..} <- parseBucket bucket
                return TaxBox{tbRule=TaxBoxNegate tbRule, tbShouldBe= Just LT,tbRound=Nothing,..}
              parseBucket (stripPrefix "+" -> Just bucket)  = do
                TaxBox{..} <- parseBucket bucket
                return TaxBox{tbRule=tbRule, tbShouldBe = Just GT,..}
              parseBucket s  = case s of
                "net" -> return  $ TaxBox l_tbName (Just $ l_tbName <> " net") Nothing (TaxBoxNet l_tbName ) Nothing
                "tax" -> return  $ TaxBox l_tbName (Just $ l_tbName <> " tax") Nothing (TaxBoxTax l_tbName ) Nothing
                "gross" -> return  $ TaxBox l_tbName (Just $ l_tbName <> " gross") Nothing (TaxBoxGross l_tbName ) Nothing
                _ -> fail "Not a valid amount type"

          withText ("TaxBox bucket" <> show fields) parseBucket fields
            <|> withObject "TaxBox fields" parseFields fields
        _ -> fail $ "Too many keys for " <> show o0

-- | Negate bucket rule when name start with -
negateBucket :: TaxBoxRule -> TaxBoxRule
negateBucket rule0 = case rule0 of
  TaxBoxNet (stripPrefix "-" -> Just bucket) -> TaxBoxNegate $ TaxBoxNet (strip bucket) 
  TaxBoxTax (stripPrefix "-" -> Just bucket) -> TaxBoxNegate $ TaxBoxTax (strip bucket) 
  TaxBoxGross (stripPrefix "-" -> Just bucket) -> TaxBoxNegate $ TaxBoxGross (strip bucket) 
  TaxBoxTaxWith rate (stripPrefix "-" -> Just bucket) -> TaxBoxNegate $ TaxBoxTaxWith rate (strip bucket) 
  TaxBoxSum rules -> TaxBoxSum (negateBucket <$> rules)
  TaxBoxSub r1 r2 -> TaxBoxSub (negateBucket r1) (negateBucket r2)
  TaxBoxFloor rounding rule -> TaxBoxFloor rounding (negateBucket rule)
  TaxBoxCeil rounding rule -> TaxBoxCeil rounding (negateBucket rule) 
  TaxBoxRound rounding rule -> TaxBoxRound rounding (negateBucket rule)
  TaxBoxBanker rounding rule -> TaxBoxBanker rounding (negateBucket rule)
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
$(deriveJSON defaultOptions ''RoundingMethod)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField
                            , fieldLabelModifier = fromMaybe <*> stripPrefix "TaxBox"
                            , constructorTagModifier = fromMaybe <*> stripPrefix "TaxBox"
                            } ''TaxBoxRule)
$(deriveJSON defaultOptions ''HMRCProcessorParameters)
$(deriveJSON defaultOptions ''ManualProcessorParameters)
$(deriveJSON defaultOptions ''ECSLProcessorParameters)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField
                            , fieldLabelModifier = fromMaybe <*> stripPrefix "tp"
                            , constructorTagModifier = fromMaybe <*> stripSuffix "Processor"
                            } ''TaxProcessorSettings)
$(deriveJSON defaultOptions {fieldLabelModifier = (fromMaybe <*> stripSuffix "Raw")} ''TaxReportSettings)
