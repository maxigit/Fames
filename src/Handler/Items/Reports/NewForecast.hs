module Handler.Items.Reports.NewForecast  (
plotForecastError
, getPlotForecastError
, vadd, vmul, vsub, vdiv
) where

import Import
-- import Handler.Items.Reports.Common
import Handler.Items.Reports.Forecast
import Items.Types
-- import Data.Conduit.List (consume)
import qualified Data.Vector.Generic as V
import qualified Data.Conduit.Combinators as C
import Util.ForConduit


data WithError = WithError { forecast, overError, underError :: UWeeklyAmount }
   deriving (Show)

plotForecastError ::  Text -> UWeeklyAmount -> WithError -> Widget
plotForecastError plotId actuals0 naiveF = do -- actuals naiveForecast previousForecast currentForecast = do
   let WithError naives0 naiveOvers0 naiveUnders0 = naiveF
   let x = [1..  length actuals] :: [Int]
       adjust v = V.map (\x -> 100 * x / maxActual) v
       maxActual = V.last actuals0
       naiveOversY = actuals `vadd` naiveOvers
       naiveUndersY = actuals `vsub` naiveUnders
       actuals = adjust actuals0
       naive = adjust naives0
       naiveOvers = adjust naiveOvers0
       naiveUnders = adjust naiveUnders0
   traceShowM ("==============================", plotId, naiveUnders)
   [whamlet|
     The plot
     <div. id="#{plotId}">
   |]
   toWidgetBody [julius|
      Plotly.plot(#{plotId} 
      , [{ 
         x: #{toJSON x}
         , y:#{toJSON naiveUndersY}
         , line: {shape: "spline", color: "transparent"}
         }
         ,
         { 
         x: #{toJSON x}
         , y:#{toJSON naiveOversY}
         , fill: "tonexty"
         , line: {shape: "spline", color: "transparent"}
         }
         ,
         { 
         x: #{toJSON x}
         , y:#{toJSON naive}
         , mode: "lines"
         , line: {dash: "dash", color: "black"}
         }
         ,
         { 
         x: #{toJSON x}
         , y:#{toJSON actuals}
         , mode: "lines"
         , line: {color: "green"}
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
      addErrors (ForMap _ (salesV, naiveV)) = (salesV, computeAbsoluteError (Actual salesV) naiveV)
  runDB do
        Just (salesByWeek, naive) <- 
             runConduit $ 
             alignConduit salesSource naiveSource
                 .|mapC  joinWithZeros
                 .|mapC  addErrors
                 .| C.foldl1 \(salesA, WithError naiveA  overA  underA) (salesB, WithError naiveB overB underB) -> 
                          (salesA `vadd` salesB
                          , WithError (naiveA `vadd` naiveB)
                                      (overA `vadd` overB)
                                      (underA `vadd` underB)
                          )

        

        let plot = plotForecastError ("forecast-" <> tshow end) salesByWeek  naive
        return [whamlet|
          ^{plot}
        |]
        
        
newtype Actual a = Actual a
computeAbsoluteError :: Actual UWeeklyAmount  -> UWeeklyAmount -> WithError
computeAbsoluteError (Actual actuals) forecast = WithError forecast overError underError where
   overError = V.zipWith over forecast actuals
   underError = V.zipWith under forecast actuals


over, under :: Double -> Double -> Double
over a b = max 0 ( a -b )
under a b = max 0 ( b - a)

vadd, vsub, vdiv, vmul  :: UWeeklyAmount -> UWeeklyAmount -> UWeeklyAmount
vadd = V.zipWith (+)
vsub = V.zipWith (-)
vdiv = V.zipWith (/)
vmul = V.zipWith (*)
