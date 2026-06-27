module Handler.Items.Forecast.Model
where

import Import
import Items.Types
import Measure as M
import qualified Data.Map as Map

-- * Type
data ForecastModel
     = Naive 
     -- | IM -- independant margins
     -- | CategorySplitter Category
     deriving (Show, Read, Eq)
     
-- * Common
estimateSkuSpeedFromDir :: FilePath -> Handler (Vector (Sku, YearlyQuantity))
estimateSkuSpeedFromDir forecastDir = do
    content <- readFileUtf8 $ forecastDir </> "model.hs"
    case readMay content of
       Nothing -> error $ "No model.hs file present in " <> show forecastDir
       Just model -> error $ show @ForecastModel model
    return mempty

-- * Model implementation

