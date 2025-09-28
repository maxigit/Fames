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



plotForecastError ::  UWeeklyAmount -> UWeeklyAmount -> UWeeklyAmount -> UWeeklyAmount -> Widget
plotForecastError actuals naive naiveUps naiveDowns  = do -- actuals naiveForecast previousForecast currentForecast = do
     --         1  0  0   2 0 1  3 0 0  0 4 0
   let x = [1..  length actuals] :: [Int]
       naiveUpsY = actuals `vadd` naiveUps
       naiveDownsY = actuals `vsub` naiveDowns
   [whamlet|
     The plot
     <div. id="forecast-error">
   |]
   toWidgetBody [julius|
      Plotly.plot("forecast-error" 
      , [{ 
         x: #{toJSON x}
         , y:#{toJSON naiveDownsY}
         , line: {shape: "spline", color: "black"}
         }
         ,
         { 
         x: #{toJSON x}
         , y:#{toJSON naiveUpsY}
         , fill: "tonexty"
         , line: {shape: "spline", color: "transparent"}
         }
         ,
         { 
         x: #{toJSON x}
         , y:#{toJSON naive}
         , mode: "lines"
         }
         ,
         { 
         x: #{toJSON x}
         , y:#{toJSON actuals}
         , mode: "lines"
         }
         ]
      );
      |]


getPlotForecastError :: Day -> Handler Widget
getPlotForecastError end = do
  let (start, salesSource) = loadYearOfActualCumulSalesByWeek end
      (_, naiveSource) = loadYearOfActualCumulSalesByWeek start
      v0 = V.replicate 52 0
      joinWithZeros (ForMap sku theseab) = ForMap sku ab where
          ab = these (,v0) (v0,) (,) theseab
      addErrors (ForMap _ (salesV, naiveV)) = (salesV, naiveV, computeAbsoluteError salesV naiveV)
  runDB do
        Just (salesByWeek, naive, (ups, downs)) <- 
             runConduit $ 
             alignConduit salesSource naiveSource
                 .|mapC  joinWithZeros
                 .|mapC  addErrors
                 .| C.foldl1 \(salesA, naiveA, (upA, downA)) (salesB, naiveB, (upB, downB)) -> 
                          (salesA `vadd` salesB
                          , naiveA `vadd` naiveB
                          , (upA `vadd` upB, downA `vadd` downB)
                          )

        

        let plot = plotForecastError salesByWeek  naive ups downs
        return [whamlet|
          ^{plot}
        |]
        
        
computeAbsoluteError :: UWeeklyAmount  -> UWeeklyAmount -> (UWeeklyAmount, UWeeklyAmount)
computeAbsoluteError actuals forecast = (ups, downs) where
   ups = V.zipWith up actuals forecast
   downs = V.zipWith down actuals forecast


up, down :: Double -> Double -> Double
up a b = max 0 ( a -b )
down a b = max 0 ( b - a)

vadd, vsub  :: UWeeklyAmount -> UWeeklyAmount -> UWeeklyAmount
vadd = V.zipWith (+)
vsub = V.zipWith (-)
