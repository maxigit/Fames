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
  } deriving (Show, Read)
  

data PayrollSettings = PayrollSettings
  { employees :: Map Text EmployeeSettings
  , firstTaxWeek :: Day -- ^ first day of the first week of the tax year
  , firstTaxMonth :: Day -- ^ first day of the first month of the taxk year
  , grnSupplier :: Int
  , grnHolidayLocation :: Text
  , grnWorkLocation :: Text
  } deriving (Show, Read)

-- * JSON
$(deriveJSON defaultOptions ''EmployeeSettings)
$(deriveJSON defaultOptions ''PayrollSettings)

-- * Utils
