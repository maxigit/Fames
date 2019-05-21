module GL.TaxReport.Settings where
import ClassyPrelude
import Data.Aeson.TH(deriveJSON, defaultOptions, fieldLabelModifier, sumEncoding, SumEncoding(..))
import Data.Aeson.Types
import GL.Payroll.Settings
-- * Type
-- | Main settins to define a report.
-- The actual report name should be in the key map
data TaxReportSettings  = TaxReportSettings
  { startDate :: Day -- ^ starting period of the first report
  , nextPeriod :: DateCalculator -- ^ how to calculate the next start from the last start
  , referenceFormat :: Text -- ^ format to use with format time to create reference
  -- , rules :: () -- 
  }
  deriving (Eq, Read, Show)

-- * JSON
$(deriveJSON defaultOptions ''TaxReportSettings)
