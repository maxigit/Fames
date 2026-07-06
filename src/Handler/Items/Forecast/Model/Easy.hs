module Handler.Items.Forecast.Model.Easy
where

import Import

data Model 
     = Naive
     | PreviousYear Int
     | ForeachCategory Text Model -- ^ Apply the given model to each category separately
     | Null
     -- | FilterCategory Text [Text] Model
     deriving (Show, Read, Eq)
