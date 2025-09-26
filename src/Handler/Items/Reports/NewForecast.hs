module Handler.Items.Reports.NewForecast  (
plotForecastError
) where

import Import
-- import Handler.Items.Reports.Common
-- import Handler.Items.Reports.Forecast



plotForecastError ::  Widget
plotForecastError = do -- actuals naiveForecast previousForecast currentForecast = do
     --         1  0  0   2 0 1  3 0 0  0 4 0
   let y, y_under, y_over, x :: [Int]
       y =       [1,1,1, 2,3,4, 7,7,7,  7,11,11]
       y_under = [0,0,0, 1,1,2, 4,5,5,  6, 4,4]
       y_over =  [1,2,2, 3,3,4, 8,9,10,10,10,10]
       x = [1..12]
   [whamlet|
     The plot
     <div. id="forecast-error">
   |]
   toWidgetBody [julius|
      Plotly.plot("test" 
      , [{ 
         x: #{toJSON x}
         , y:#{toJSON y_under}
         , line: {shape: "spline", color: "transparent"}
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
         , y:#{toJSON y}
         , mode: "lines"
         }
         ]
      );
      |]

