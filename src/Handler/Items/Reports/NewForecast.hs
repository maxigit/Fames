module Handler.Items.Reports.NewForecast  (
plotForecastError
, getPlotForecastError
, vadd, vmul, vsub, vdiv
, forecastPathToDay
) where

import Import
-- import Handler.Items.Reports.Common
import Handler.Items.Reports.Forecast
import Items.Types
-- import Data.Conduit.List (consume)
import qualified Data.Vector.Generic as V
import qualified Data.Conduit.Combinators as C
import Util.ForConduit
import GL.Utils
import Data.List(iterate)

-- import qualified Handler.Items.Index as I
-- import qualified Handler.Items.Common as I

data WithError = WithError { forecast, overError, underError :: UWeeklyQuantity }
   deriving (Show)

plotForecastError ::  Text -> Day -> Day -> UWeeklyQuantity -> WithError -> WithError -> Widget
plotForecastError plotId start today actuals0 naiveF forecastF = do -- actuals naiveForecast previousForecast currentForecast = do
   let WithError naives0 naiveOvers0 naiveUnders0 = naiveF
       WithError forecasts0 forecastOvers0 forecastUnders0 = forecastF
   let x = take 52 $ iterate (calculateDate (AddWeeks 1)) start-- [1..  length actuals] :: [Int]
       adjust v =  v -- V.map (\x -> 100 * x / maxActual) v
       -- maxActual = V.last actuals0
       naiveOversY = actuals `vadd` naiveOvers
       naiveUndersY = actuals `vsub` naiveUnders
       forecastOversY = actuals `vadd` forecastOvers
       forecastUndersY = actuals `vsub` forecastUnders

       actuals = adjust actuals0
       naive = adjust naives0
       naiveOvers = adjust naiveOvers0
       naiveUnders = adjust naiveUnders0
       forecast = adjust forecasts0
       forecastOvers = adjust forecastOvers0
       forecastUnders = adjust forecastUnders0
       
       todayBar = if start <= today && today <= (calculateDate (AddYears 1) start)
                  then [julius|
                    shapes : [{
                    type: 'line'
                    , x0: #{tshow today}, x1: #{tshow today}
                    , y0: 0 , y1: 1
                    , xref:"x", yref:"paper"
                    , mode: "lines"
                    , line: {dash: "dash", color: "lightgray"}
                    }
                    ]

                       |]
                  else [julius||]
   [whamlet|
     The plot
     <div. id="#{plotId}">
   |]
   toWidgetBody [julius|
      traces = [{  // naive under
         x: #{toJSON x}
         , y:#{toJSON naiveUndersY}
         , line: {shape: "spline", color: "transparent"}
         }
         ,
         {  // naive over
         x: #{toJSON x}
         , y:#{toJSON naiveOversY}
         , fill: "tonexty"
         , line: {shape: "spline", color: "transparent"}
         }
         ,
         {  // naive line
         x: #{toJSON x}
         , y:#{toJSON naive}
         , mode: "lines"
         , line: {dash: "dash", color: "black"}
         }
         ,
         { // forecast under
         x: #{toJSON x}
         , y:#{toJSON forecastUndersY}
         , line: {shape: "spline", color: "transparent"}
         }
         ,
         {  // forecast over
         x: #{toJSON x}
         , y:#{toJSON forecastOversY}
         , fill: "tonexty"
         , line: {shape: "spline", color: "transparent"}
         , fillcolor: "rgba(200,0,0,0.3)"
         }
         ,
         {  // forecast line
         x: #{toJSON x}
         , y:#{toJSON forecast}
         , mode: "lines"
         , line: {dash: "dot", color: "rgb(200,0,0)"}
         }
         ,
         {  // actual
         x: #{toJSON x}
         , y:#{toJSON actuals}
         , mode: "lines"
         , line: {color: "green"}
         } // today
         ];
         vbar = {^{todayBar} };
      Plotly.newPlot(#{plotId}, traces, vbar );
      |]


getPlotForecastError :: Day -> FilePath -> Handler Widget
getPlotForecastError day path = do
  today <- todayH
  settings <- getsYesod appSettings
  let (start, end, salesSource) = loadYearOfActualCumulSalesByWeek day
  skuMap <- loadYearOfForecastCumulByWeek day $ appForecastProfilesDir settings </> path
  let (_, _,  naiveSource) = loadYearOfActualCumulSalesByWeek (calculateDate (AddYears $ -1) start)
      v0 = V.replicate 52 0
      joinWithZeros (ForMap sku theseab) = ForMap sku ab where
          ab = these (,v0) (v0,) (,) theseab
      addErrors (ForMap _ (salesV, naiveV, forecastV)) = (salesV, computeAbsoluteError (Actual salesV) naiveV
                                                                , computeAbsoluteError (Actual salesV) forecastV)
      replaceNaive (ForMap sku (salesV, naive)) = ForMap sku (salesV, naive, forecast) where
         forecast =  findWithDefault v0 sku skuMap
  runDB do
        Just (salesByWeek, naive, forecast) <- 
             runConduit $ 
             alignConduit salesSource naiveSource
                 .|mapC  joinWithZeros
                 .|mapC replaceNaive
                 .|mapC  addErrors
                 .| C.foldl1 \(salesA, WithError naiveA  overA  underA, WithError fA oA uA)
                              (salesB, WithError naiveB overB underB, WithError fB oB uB)
                              -> 
                          (salesA `vadd` salesB
                          , WithError (naiveA `vadd` naiveB)
                                      (overA `vadd` overB)
                                      (underA `vadd` underB)
                          , WithError (fA `vadd` fB)
                                      (oA `vadd` oB)
                                      (uA `vadd` uB)
                          )

        

        let plot = plotForecastError ("forecast-" <> tshow end) start today salesByWeek  naive forecast
        return [whamlet|
          <div> #{path}
          ^{plot}
          <div>forecast for period : #{tshow $ start} - #{tshow $ end} 
        |]

forecastPathToDay :: FilePath -> Maybe Day
forecastPathToDay = readMay . take 10
        
newtype Actual a = Actual a
computeAbsoluteError :: Actual UWeeklyQuantity  -> UWeeklyQuantity -> WithError
computeAbsoluteError (Actual actuals) forecast = WithError forecast overError underError where
   overError = V.zipWith over forecast actuals
   underError = V.zipWith under forecast actuals


over, under :: Double -> Double -> Double
over a b = max 0 ( a -b )
under a b = max 0 ( b - a)

--  -- The forecast gives us quantity but we need to weight it by price (to get Amount vs Quantity)
--  -- reuse the price cache
--  cache <- I.fillIndexCache
--  skuToStyleVar <- I.skuToStyleVarH
--  base <- basePriceList
--  let ?skuToStyleVar = skuToStyleVar
--  itemGroups <- I.loadVariations cache I.indexParam  {I.ipMode = ItemPriceView }
--                                                     {I.ipShowInactive = I.ShowAll}
--                                                     {I.ipSKU = Just $ LikeFilter "M%"  } -- load everingy
--  let stylePriceMap = Map.fromList [ (iiStyle item, price)
--                                   | (item, _vars) <- itemGroups
--                                   , price <- maybeToList $ masterPrice base (iiInfo item)
--                                   ]
--      skuToPricem :: Sku -> Maybe Double
--      skuToPricem sku = let
--         (style,_) = skuToStyleVar sku
--         in lookup style stylePriceMap
--
--
