{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- | Defines types for AppSettings
module GL.Payroll.Settings where

import ClassyPrelude
import Data.Aeson.TH(deriveJSON)
import Data.Aeson.Types
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap (toMapText)
import Control.Monad.Fail (MonadFail(..))

-- * Types 
-- ** For config 
data EmployeeSettings = EmployeeSettings
  { payrollId :: Int -- To add to 
  , hourlyRate :: Double
  , faSKU :: Text
  , dimension1 :: Maybe Int
  , dimension2 :: Maybe Int
  } deriving (Show , Eq, Ord)

-- | External party associated with deductions and costs
data PayrollExternalSettings = PayrollExternalSettings
  {  costPaymentSettings :: Maybe DACSettings
  ,  deductionsPaymentSettings :: Maybe DACSettings
  ,  costGlAccount:: Int
  } deriving (Show, Eq, Ord)

data DACSettings = DACSettings
  { paymentRef :: Maybe Text
  , paymentTerm :: DateCalculator
  , paymentSettings :: DACPaymentSettings
  } deriving (Show, Eq, Ord)

-- | dacs payments can be either entered in FA
-- as a normal payment (similar to employee)
-- and made to another supplier. In the later case
-- An invoice the corresponding of the correct amount must be ussied
-- as well as credit note to the main supplier.
data DACPaymentSettings
  = DACSupplierSettings
    { supplier :: Int
    , glAccount :: Int
    , psDimension1 :: Maybe Int
    , psDimension2 :: Maybe Int
    , memo :: Maybe Text
    }
  | DACPaymentSettings
    { bankAccount :: Int
    }
  deriving (Show, Eq, Ord)

data PayrollSettings = PayrollSettings
  { employees :: Map Text EmployeeSettings
  , firstTaxWeek :: Day -- ^ first day of the first week of the tax year
  , firstTaxMonth :: Day -- ^ first day of the first month of the taxk year
  , wagesSupplier :: Int
  , grnHolidayLocation :: Text
  , grnWorkLocation :: Text
  , wagesBankAccount :: Int
  , externals :: Map Text PayrollExternalSettings
  , views :: Map Text ([(Text,  [PayrollFormula]) ]
                      , [Text] -- employe order by name
                      )
  -- , formulas :: Map Text PayrollFormula
  } deriving (Show, Eq, Ord)


-- ** Date Calculator 
data DateCalculator
  = DayOfMonth { dayX :: Int, cutoff :: Int } -- day , cut off
  | AddDays Int
  | NextDayOfWeek { weekDay :: DayOfWeek, weekCutoff:: DayOfWeek } -- day, cut off
  | AddMonths Int
  | AddYears Int
  | AddWeeks Int
  | EndOfMonth 
  | EndOfWeek { weekDay :: DayOfWeek}
  | EndOfYear
  | BeginningOfMonth 
  | BeginningOfQuarter 
  | BeginningOfWeek { weekDay :: DayOfWeek}
  | BeginningOfYear
  | Chain [DateCalculator] -- apply  in order
  | Newest [DateCalculator]
  | Oldest [DateCalculator]
  | WeekDayCase (Map (Maybe DayOfWeek) DateCalculator)
  deriving (Show, Read, Eq, Ord)

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show, Read, Eq, Ord, Enum, Bounded)
          
predCyclic :: DayOfWeek -> DayOfWeek
predCyclic Monday = Sunday
predCyclic d = pred d

succCyclic :: DayOfWeek -> DayOfWeek
succCyclic Sunday = Monday
succCyclic d = succ d

-- ** Formula 
-- | Simple formula to 
data PayrollFormula = PFVariable Text
                    | PFValue Double
                    | PFNegVariable Text
  deriving (Show, Eq, Ord)

-- * JSON 
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField}''DayOfWeek)
instance ToJSONKey (Maybe DayOfWeek) where
  toJSONKey = toJSONKeyText myKey where
    myKey Nothing = "default"
    myKey (Just w ) = tshow w
instance FromJSONKey (Maybe DayOfWeek) where
  fromJSONKey = FromJSONKeyValue parseW  where
    parseW v = (Just <$> parseJSON  v) <|> return Nothing
       
instance {-# OVERLAPPING #-}  ToJSON (Text, [PayrollFormula]) where
  toJSON (var, []) = String var
  toJSON (var, [PFVariable var']) | var == var' = String var
  toJSON (var, vs) = toJSON [ (fromText var, vs) ]

instance {-# OVERLAPPING #-} FromJSON (Text, [PayrollFormula]) where
  parseJSON v = withText "variable" (\s -> return (s, [PFVariable s])) v
             <|> withObject ("named formula") (\o -> do
                 case mapToList $ toMapText o of
                        [(var, f)] -> (var,) <$> parseJSON f
                        _ -> fail "Named formula should have only one key value pair"
             ) v

instance {-# OVERLAPPING #-} ToJSON([(Text, [PayrollFormula])], [Text]) where
  toJSON (cols, []) = toJSON cols
  toJSON (cols, order) = object [ "order" .= order, "columns" .= cols ]
  
instance {-# OVERLAPPING #-} FromJSON ([(Text, [PayrollFormula])], [Text]) where
  parseJSON v = ((,[]) <$> parseJSON v)  -- withArray "column" (\xs -> return (xs, [])) v
              <|> withObject "with-order"  (\o -> do
                      order <- o .: "order" 
                      cols <- o .: "columns"
                      return (cols, order)
                      ) v

 
-- There valid code to instanciate FromJSON and ToJSON
-- but will result in (Text, [PayrollFormula]) to be valid
-- and therefore overlapp with ours
instance ToJSON PayrollFormula where
  toJSON (PFVariable var) = toJSON var
  toJSON (PFNegVariable var) = toJSON $ "-" <> var
  toJSON (PFValue val) = toJSON val

instance FromJSON PayrollFormula where
  parseJSON v = withScientific "constant" (\_ -> PFValue <$> parseJSON v) v --  return $ PFValue $ toRealFloat f) v
            <|> withText "value" (\val -> return $ case stripPrefix "-" val of
                                       Nothing -> PFVariable val
                                       Just negated -> PFNegVariable negated
                               ) v
  
$(deriveJSON defaultOptions ''EmployeeSettings)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField
                            , fieldLabelModifier = \d -> case d of
                                                           "dayX" -> "day"
                                                           _ -> d
                            } ''DateCalculator)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField}''DACPaymentSettings)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField} ''DACSettings)
$(deriveJSON defaultOptions ''PayrollExternalSettings)
$(deriveJSON defaultOptions ''PayrollSettings)

-- * Utils 
