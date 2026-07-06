module Handler.Items.Forecast.Model.Easy
where

import Import
import Handler.CsvUtils
import Text.Read(readPrec)

data Model 
     = Naive
     | PreviousYears Int
     | Previous EasyDay EasyDay (Maybe Int)
     | ForeachCategory Text Model -- ^ Apply the given model to each category separately
     | Null
     | FilterCategory Text [Text] Model
     | ExcludeCategory Text [Text] Model
     | CategoryCase Text [(Text, Model)] Model
     | Avg [Model]
     | Max [Model]
     | Min [Model]
     | Median [Model]
     | Sum [Model]
     | Scale Double Model
     | Cap Double Model
     deriving (Show, Read, Eq)
     
newtype EasyDay = EasyDay Day deriving (Show, Eq)


instance Read EasyDay where
   readPrec = do
       s <- readPrec
       AllFormatsDay d <- parseDay s
       return $ EasyDay d


