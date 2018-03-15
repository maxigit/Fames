{-# LANGUAGE StandaloneDeriving #-}
-- | Defines types for AppSettings
module GL.Payroll.Settings where

import ClassyPrelude
import Data.Aeson.TH(deriveJSON, defaultOptions)


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
  {  supplier :: Maybe Int
  ,  cost :: Maybe DACSettings
  ,  deduction :: Maybe DACSettings
  ,  glAccount :: Int
  } deriving (Show, Read, Eq, Ord)

data DACSettings = DACSettings
  { invoiceAccount :: Int
  , creditAccount :: Int
  } deriving (Show,Read, Eq, Ord)

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

-- * JSON
$(deriveJSON defaultOptions ''EmployeeSettings)
$(deriveJSON defaultOptions ''DACSettings)
$(deriveJSON defaultOptions ''PayrollExternalSettings)
$(deriveJSON defaultOptions ''PayrollSettings)

-- * Utils
