module Handler.Items.Reports.NewForecast  (
plotForecastError
, getPlotForecastError
) where

import Import
-- import Handler.Items.Reports.Common
import Handler.Items.Reports.Forecast
import Items.Types
-- import Data.Conduit.List (consume)
import qualified Data.Vector.Generic as V
import qualified Data.Conduit.Combinators as C
import Util.ForConduit



plotForecastError ::  UWeeklyAmount -> UWeeklyAmount -> Widget
plotForecastError actuals naives = do -- actuals naiveForecast previousForecast currentForecast = do
     --         1  0  0   2 0 1  3 0 0  0 4 0
   let y, y_under, y_over, x :: [Int]
       y =       [1,1,1, 2,3,4, 7,7,7,  7,11,11]
       y_under = [0,0,0, 1,1,2, 4,5,5,  6, 4,4]
       y_over =  [1,2,2, 3,3,4, 8,9,10,10,10,10]
       x = [1..  length actuals]
   [whamlet|
     The plot
     <div. id="forecast-error">
   |]
   toWidgetBody [julius|
      Plotly.plot("forecast-error" 
      , [{ 
         x: #{toJSON x}
         , y:#{toJSON actuals}
         , line: {shape: "spline", color: "black"}
         }
         ,
         { 
         x: #{toJSON x}
         , y:#{toJSON y_over}
         , fill: "tonexty"
         , line: {shape: "spline", color: "transparent"}
         }
         ,
         { 
         x: #{toJSON x}
         , y:#{toJSON naives}
         , mode: "lines"
         }
         ]
      );
      |]


getPlotForecastError :: Day -> Handler Widget
getPlotForecastError end = do
  let (start, salesSource) = loadYearOfActualCumulSalesByWeek end
      (_, naiveSource) = loadYearOfActualCumulSalesByWeek start
  runDB do
        Just salesByWeek <- runConduit $ salesSource
                            .| mapC snd
                            .|  C.foldl1 (V.zipWith (+))
        Just naive <- runConduit $ naiveSource
                            .| mapC snd
                            .|  C.foldl1 (V.zipWith (+))
        let plot = plotForecastError salesByWeek  naive
        return [whamlet|
          ^{plot}
        |]
        
        
errorOnConduit actuals naives = do
  joinOnWith 

computeAbsoluteError :: UWeeklyAmount  -> UWeeklyAmount -> (UWeeklyAmount, UWeeklyAmount)
computeAbsoluteError actuals forecast = (ups, downs) where
   ups = V.zipWith up actuals forecast
   downs = V.zipWith down actuals forecast


up, down :: Double -> Double -> Double
up a b = max (0, a -b )
down ab = max (0, b - a)
