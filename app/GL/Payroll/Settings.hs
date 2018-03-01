{-# LANGUAGE StandaloneDeriving #-}
-- | Defines types for AppSettings
module GL.Payroll.Settings where

import ClassyPrelude
import Data.Aeson.TH(deriveJSON, defaultOptions)


data EmployeeSettings = EmployeeSettings
  { timesheet :: Text -- To add to 
  } deriving (Show, Read)
  

data PayrollSettings = PayrollSettings
  { employees :: Map Text EmployeeSettings
  } deriving (Show, Read)

-- * JSON
$(deriveJSON defaultOptions ''EmployeeSettings)
$(deriveJSON defaultOptions ''PayrollSettings)
