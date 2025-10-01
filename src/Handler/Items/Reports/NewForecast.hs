module Handler.Items.Reports.NewForecast  (
plotForecastError
, getPlotForecastError
, vadd, vmul, vsub, vdiv
, forecastPathToDay
, ForecastSummary(..)
, averageForecastSummary
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
import Data.Time.Calendar (diffDays)

-- import qualified Handler.Items.Index as I
-- import qualified Handler.Items.Common as I

data WithError = WithError { forecastCumul, overError, underError :: UWeeklyQuantity }
   deriving (Show)

data ForecastSummary = ForecastSummary { overPercent, underPercent, overallPercent, naivePercent, aes, trendPercent :: Double }
   deriving (Show)
   
plotForecastError ::  Text -> Day -> Day -> UWeeklyQuantity -> WithError -> WithError -> Widget
plotForecastError plotId start today actuals0 naiveF forecastF = do -- actuals naiveForecast previousForecast currentForecast = do
   let WithError naives0 naiveOvers0 naiveUnders0 = naiveF
       WithError forecasts0 forecastOvers0 forecastUnders0 = forecastF
   let x = take 52 $ iterate (calculateDate (AddWeeks 1)) start-- [1..  length actuals] :: [Int]
       xbefore = case takeWhile (<= today) x of
                      [] -> x
                      before -> before
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
       
       naiveErrorRel = V.map (*100) $ (naiveUnders `vadd` naiveOvers) `vdiv` actuals
       forecastErrorRel = V.map (*100) $ (forecastUnders `vadd` forecastOvers) `vdiv` actuals
       
       todayBar = if start <= today && today <= (calculateDate (AddYears 1) start)
                  then [julius|
                    {
                    type: 'line'
                    , x0: #{tshow today}, x1: #{tshow today}
                    , y0: 0 , y1: 1
                    , xref:"x", yref:"paper"
                    , mode: "lines"
                    , line: {dash: "dash", color: "lightgray"}
                    , layer: "below"
                    }
                       |]
                  else [julius||]
       jan = calculateDate (Chain [AddYears 1, BeginningOfYear]) start
       janBar =[julius|
                    {
                    type: 'line'
                    , x0: #{tshow jan}, x1: #{tshow jan}
                    , y0: 0 , y1: 1
                    , xref:"x", yref:"paper"
                    , mode: "lines"
                    , line: {dash: "dash", color: "lightgray"}
                    , layer: "below"
                    }
                       |]
   [whamlet|
     The plot
     <div. id="#{plotId}">
   |]
   toWidgetBody [julius|
      traces = [
         {
         x: #{toJSON xbefore}
         , y:#{toJSON naiveUndersY}
         , line: {shape: "spline", color: "transparent"}
           , yaxis: 'y'
           , showlegend: false
         }
         , {  // naive over
           x: #{toJSON xbefore}
           , y:#{toJSON naiveOversY}
           , fill: "tonexty"
           , line: {shape: "spline", color: "transparent"}
           , yaxis: 'y'
           , showlegend: false
         }
         , {  // naive line
           x: #{toJSON x}
           , y:#{toJSON naive}
           , mode: "lines"
           , line: {dash: "dash", color: "black"}
           , yaxis: 'y'
           , name: "Naive"
           }
         , { // forecast under
           x: #{toJSON xbefore}
           , y:#{toJSON forecastUndersY}
           , line: {shape: "spline", color: "transparent"}
           , yaxis: 'y'
           , showlegend: false
           }
         ,
         {  // forecast over
            x: #{toJSON xbefore}
            , y:#{toJSON forecastOversY}
            , fill: "tonexty"
            , line: {shape: "spline", color: "transparent"}
            , fillcolor: "rgba(200,0,0,0.3)"
           , yaxis: 'y'
           , showlegend: false
            }
         , {  // forecast line
           x: #{toJSON x}
           , y:#{toJSON forecast}
           , mode: "lines"
           , line: {dash: "dot", color: "rgb(200,0,0)"}
           , yaxis: 'y'
           , name: "Forecast"
           }
         , {  // actual
           x: #{toJSON xbefore}
           , y:#{toJSON actuals}
           , mode: "lines"
           , line: {color: "green"}
           , yaxis: 'y'
           , name: "Actual"
           } // today
         , { // naive relative
           x: #{toJSON xbefore}
           , y: #{toJSON naiveErrorRel}
           , mode: "lines"
           , line: {color: "black", dash:""}
           , yaxis : 'y2'
           , name: "Naive %Error"
           }
         , { // naive relative
           x: #{toJSON xbefore}
           ,y: #{toJSON forecastErrorRel}
           , mode: "lines"
           , line: {dash: "", color: "red"}
           , yaxis : 'y2'
           , name: "Forceast %Error"
           }
         ];
      Plotly.newPlot(#{plotId}
                    , traces
                    , {shapes: [^{todayBar},
                               ^{janBar},
                               { x0: 0
                               , x1: 1
                               , y0: 100
                               , y1: 100
                               , xref: 'paper', yref: 'y2'
                               , line: {dash :"dashdot", color: "lightgray"}
                               , layer: "below"
                               },
                               { x0: 0
                               , x1: 1
                               , y0: 50
                               , y1: 50
                               , xref: 'paper', yref: 'y2'
                               , line: {dash :"dashdot", color: "lightgray", width: 1}
                               , layer: "below"
                               },
                               { x0: 0
                               , x1: 1
                               , y0: 0
                               , y1: 0
                               , xref: 'paper', yref: 'y2'
                               , line: {dash :"dashdot", color: "lightgray", width: 1}
                               , layer: "below"
                               },
                               ]
                      , yaxis2: {side: "left", anchor: "x"}
                      , grid: {rows:2, columns:1 ,shared_xaxes:true}
                      }
                    )
      |]


getPlotForecastError :: Day -> FilePath -> Handler (Widget, ForecastSummary)
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
        salesM <- 
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
        case salesM of
             Just (salesByWeek, naive, forecast) ->  do
                let plot = plotForecastError ("forecast-" <> tshow end) start today salesByWeek  naive forecast
                return ([whamlet|
                          <div> #{path}
                          ^{plot}
                          <div>forecast for period : #{tshow $ start} - #{tshow $ end} 
                        |]
                       , let overPercent = percentFor $ overError forecast
                             underPercent = percentFor $ underError forecast
                             overallPercent = overPercent + underPercent
                             naivePercent = percentFor $ vadd (overError naive) (underError naive)
                             aes = overallPercent / naivePercent
                             percentFor v = lastGood v / lastGood salesByWeek
                             trendPercent = lastGood salesByWeek / lastGood (forecastCumul naive) -1
                             weeksToToday =  fromInteger $ case diffDays today start `div` 7 of
                                                          n | n < 0 -> 51
                                                          n ->  min n 51
                             lastGood v = v V.! weeksToToday
                                                 
                         in ForecastSummary{..} 
                       )
             Nothing -> return ([whamlet| no data for #{tshow day}/#{path} |], ForecastSummary 0 0 0 0 0 0)

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

averageForecastSummary :: [ForecastSummary] -> ForecastSummary
averageForecastSummary [] = ForecastSummary 0 0 0 0 0 0
averageForecastSummary sums = let 
    avg f = sum (map f sums ) / n 
    n = fromIntegral (length sums)
    in ForecastSummary (avg overPercent)
                       (avg underPercent)
                       (avg overallPercent)
                       (avg naivePercent)
                       (avg aes)
                       (avg trendPercent)

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
