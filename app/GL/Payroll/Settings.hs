{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- | Defines types for AppSettings
module GL.Payroll.Settings where

import ClassyPrelude
import Data.Aeson.TH(deriveJSON, defaultOptions, fieldLabelModifier, sumEncoding, SumEncoding(..))
import Data.Aeson.Types

-- * Types
-- ** For config
data EmployeeSettings = EmployeeSettings
  { payrollId :: Int -- To add to 
  , hourlyRate :: Double
  , faSKU :: Text
  , dimension1 :: Maybe Int
  , dimension2 :: Maybe Int
  } deriving (Show, Read , Eq, Ord)

-- | External party associated with deductions and costs
data PayrollExternalSettings = PayrollExternalSettings
  {  costPaymentSettings :: Maybe DACSettings
  ,  deductionsPaymentSettings :: Maybe DACSettings
  ,  costGlAccount:: Int
  } deriving (Show, Read, Eq, Ord)

data DACSettings = DACSettings
  { paymentRef :: Maybe Text
  , paymentTerm :: DateCalculator
  , paymentSettings :: DACPaymentSettings
  } deriving (Show, Read, Eq, Ord)

-- | dacs payments can be either entered in FA
-- as a normal payment (similar to employee)
-- and made to another supplier. In the later case
-- An invoice the corresponding of the correct amount must be ussied
-- as well as credit note to the main supplier.
data DACPaymentSettings
  = DACSupplierSettings
    { supplier :: Int
    , glAccount :: Int
    , dimension1 :: Maybe Int
    , dimension2 :: Maybe Int
    , memo :: Maybe Text
    }
  | DACPaymentSettings
    { bankAccount :: Int
    }
  deriving (Show, Read, Eq, Ord)

data PayrollSettings = PayrollSettings
  { employees :: Map Text EmployeeSettings
  , firstTaxWeek :: Day -- ^ first day of the first week of the tax year
  , firstTaxMonth :: Day -- ^ first day of the first month of the taxk year
  , wagesSupplier :: Int
  , grnHolidayLocation :: Text
  , grnWorkLocation :: Text
  , wagesBankAccount :: Int
  , externals :: Map Text PayrollExternalSettings
  , views :: Map Text [Text]
  } deriving (Show, Read, Eq, Ord)


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
          
predCyclic Monday = Sunday
predCyclic d = pred d

succCyclic Sunday = Monday
succCyclic d = succ d

-- * JSON
instance ToJSONKey (Maybe DayOfWeek) where
  toJSONKey = toJSONKeyText myKey where
    myKey Nothing = "default"
    myKey (Just w ) = tshow w
instance FromJSONKey (Maybe DayOfWeek) where
  fromJSONKey = FromJSONKeyValue parseW  where
    parseW v = (Just <$> parseJSON  v) <|> return Nothing
       
  
  
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField}''DayOfWeek)
$(deriveJSON defaultOptions ''EmployeeSettings)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField
                            , fieldLabelModifier = \d -> case d of
                                                           "dayX" -> "day"
                                                           _ -> d
                            } ''DateCalculator)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField} ''DACSettings)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField}''DACPaymentSettings)
$(deriveJSON defaultOptions ''PayrollExternalSettings)
$(deriveJSON defaultOptions ''PayrollSettings)

-- * Utils
