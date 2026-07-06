module Handler.Items.Forecast.Model.Easy
where

import Import

data Model 
     = Naive
     | PreviousYear Int
     | ForeachCategory Text Model -- ^ Apply the given model to each category separately
     | Null
     | FilterCategory Text [Text] Model
     | ExcludeCategory Text [Text] Model
     | CategoryCase Text [(Text, Model)] Model
     | Average [Model]
     | Scale Double Model
     deriving (Show, Read, Eq)
