{-# LANGUAGE StandaloneDeriving #-}
-- | Defines types for AppSettings
module GL.Payroll.Settings where

import ClassyPrelude
import Data.Aeson.TH(deriveJSON, defaultOptions, sumEncoding, SumEncoding(..))

-- * Types
-- ** For config
data EmployeeSettings = EmployeeSettings
  { timesheet :: Text -- To add to 
  , faSKU :: Text
  , dimension1 :: Maybe Int
  , dimension2 :: Maybe Int
  } deriving (Show, Read , Eq, Ord)
  

-- | External party associated with deductions and costs
data PayrollExternalSettings = PayrollExternalSettings
  {  paymentSettings :: DACPaymentSettings
  ,  costGlAccount:: Int
  ,  paymentTerm :: DateCalculator
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
    }
  | DACPaymentSettings
    { bankAccount :: Int
    }
  deriving (Show, Read, Eq, Ord)

data PayrollSettings = PayrollSettings
  { employees :: Map Text EmployeeSettings
  , firstTaxWeek :: Day -- ^ first day of the first week of the tax year
  , firstTaxMonth :: Day -- ^ first day of the first month of the taxk year
  , grnSupplier :: Int
  , grnHolidayLocation :: Text
  , grnWorkLocation :: Text
  , wagesBankAccount :: Int
  , externals :: Map Text PayrollExternalSettings
  } deriving (Show, Read, Eq, Ord)

-- ** Date Calculator
data DateCalculator
  = DayOfMonth Int Int -- day , cut off
  | AddDays Int
  | NextDayOfWeek DayOfWeek DayOfWeek -- day, cut off
  | AddMonths Int
  deriving (Show, Read, Eq, Ord)

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show, Read, Eq, Ord, Enum, Bounded)
          
-- * JSON
$(deriveJSON defaultOptions ''DayOfWeek)
$(deriveJSON defaultOptions ''EmployeeSettings)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField} ''DateCalculator)
$(deriveJSON defaultOptions ''DACPaymentSettings)
$(deriveJSON defaultOptions ''PayrollExternalSettings)
$(deriveJSON defaultOptions ''PayrollSettings)

-- * Utils
