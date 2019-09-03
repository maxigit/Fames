{-# LANGUAGE DuplicateRecordFields #-}
module GL.TaxReport.Settings where
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
        -- # if DEVELOPMENT
       , obligationTestScenario :: Maybe Text -- ^ He
       , submitTestScenario :: Maybe Text -- ^ He
       -- # endif
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
    parseObject o =  do
      case  mapToList o of
        [(tbName, fields)] -> do
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
                  return TaxBox{tbRule=negateBucket tbRule,..}
              parseBucket (stripPrefix "-" -> Just bucket)  = do
                TaxBox{..} <- parseBucket bucket
                return TaxBox{tbRule=TaxBoxNegate tbRule, tbShouldBe= Just LT,tbRound=Nothing,..}
              parseBucket (stripPrefix "+" -> Just bucket)  = do
                TaxBox{..} <- parseBucket bucket
                return TaxBox{tbRule=tbRule, tbShouldBe = Just GT,..}
              parseBucket s  = case s of
                "net" -> return  $ TaxBox tbName (Just $ tbName <> " net") Nothing (TaxBoxNet tbName ) Nothing
                "tax" -> return  $ TaxBox tbName (Just $ tbName <> " tax") Nothing (TaxBoxTax tbName ) Nothing
                "gross" -> return  $ TaxBox tbName (Just $ tbName <> " gross") Nothing (TaxBoxGross tbName ) Nothing
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


-- * Function
-- | Each processor can alter or add its own boxes to a report
-- alterTaxReportSettings :: Text TaxReportSettings{..} = case processor
alterTaxReportSettings :: TaxReportSettings -> Either Text TaxReportSettings
alterTaxReportSettings settings@TaxReportSettings{..} = case processor of
  HMRCProcessor _ -> alterHMRCSettings settings
  _ -> Right settings

-- |  Add calculated boxes and set decimal number to each boxes
alterHMRCSettings :: TaxReportSettings -> Either Text TaxReportSettings
alterHMRCSettings settings@TaxReportSettings{..} = do
  let boxMap = mapFromList $ zip (map tbName boxesRaw) boxesRaw :: Map Text TaxBox
      findBox boxname = maybe (Left $ "Box " <> boxname <> " not present in HMRC report") Right
                      $ lookup boxname boxMap
  b1' <- findBox "B1"
  b2' <- findBox "B2"
  -- b3 calculated
  b4'<- findBox "B4"
  -- b5 calculated
  b6'<- findBox "B6" 
  b7'<- findBox "B7"
  b8'<- findBox "B8"
  b9'<- findBox "B9"

  let
    [b1,b2,b4] = [b {tbRound = Just $ Round 2} | b <- [b1', b2', b4']]
    b3 = TaxBox "B3" (Just "Total VAT due" )(Just GT) (TaxBoxSum $ map (TaxBoxRound 2 . tbRule) [b1, b2]) (Just $ Round 2)
    [b5'3, b5'4] = map (TaxBoxRound 2 . tbRule) [b3, b4]
    b5 = TaxBox "B5" (Just "Net to be payed to HMRC" ) Nothing (TaxBoxSub b5'3 b5'4 ) (Just $ Round 2)
    [b6,b7,b8,b9] = [b {tbRound = Just $ RoundDown 0} | b <- [b6',b7',b8',b9']]
  -- get all other boxes
  let newBoxes = [b1,b2,b3,b4,b5,b6,b7,b8,b9]
      newBoxesName = map tbName newBoxes
      others = filter ((`notElem` newBoxesName) . tbName) boxesRaw
  Right settings {boxesRaw = newBoxes <> others}






  
