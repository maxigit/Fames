module Handler.Items.Forecast.Model.Easy
where

import Import

data Model 
     = Naive
     | PreviousYear Int
     deriving (Show, Read, Eq)
