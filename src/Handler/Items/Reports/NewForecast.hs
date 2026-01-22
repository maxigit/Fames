module Handler.Items.Reports.NewForecast  (
plotForecastError
, getPlotForecastError
, vadd, vmul, vsub, vdiv
, forecastPathToDay
, ForecastSummary(..)
, averageForecastSummary
, getMostOffenders
, totalError
, makeOffenderTable
, makeSummaryTable
) where

import Import
import Handler.Items.Reports.Common hiding(formatQuantity)
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

totalError :: WithError -> UWeeklyQuantity
totalError w = overError w `vadd` underError w

instance Semigroup WithError where 
  (WithError c o u) <> (WithError c' o' u') = WithError (c `vadd` c')
                                                        (o `vadd` o')
                                                        (u `vadd` u')
instance Monoid WithError where
   mempty = WithError v0 v0 v0 where v0 = V.replicate 52 0
      

data ForecastSummary = ForecastSummary { overPercent, underPercent, overallPercent, naivePercent, aes, trendPercent :: Double }
   deriving (Show)
   
data WeeklySalesWithForecastErrors = 
     WeeklySalesWithForecastErrors { wsSales :: UWeeklyQuantity
                                   , wsNaiveError :: WithError
                                   , wsForecast :: WithError
                                   }
     deriving (Show)
     
instance Semigroup WeeklySalesWithForecastErrors where
  (WeeklySalesWithForecastErrors sales naive forecast) <>  (WeeklySalesWithForecastErrors sales' naive' forecast') =
    WeeklySalesWithForecastErrors (sales `vadd` sales')
                                  (naive <> naive')
                                  (forecast <> forecast')
                                  
instance Monoid WeeklySalesWithForecastErrors where
    mempty = WeeklySalesWithForecastErrors (V.replicate 52 0)  mempty mempty


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

getForecastErrors :: Ord key => ForecastGrouper key -> Day -> FilePath -> Handler ((Day, Day), ConduitT () (ForMap key WeeklySalesWithForecastErrors) SqlHandler ())
getForecastErrors grouper day path = do 
  settings <- getsYesod appSettings
  let (start, end, salesSource) = loadYearOfActualCumulSalesByWeek grouper day
  skuMap <- loadYearOfForecastCumulByWeek grouper day $ appForecastProfilesDir settings </> path
  let (_, _,  naiveSource) = loadYearOfActualCumulSalesByWeek grouper (calculateDate (AddYears $ -1) start)
      v0 = V.replicate 52 0
      joinWithZeror (ForMap sku theseab) = ForMap sku ab where
          ab = these (,v0) (v0,) (,) theseab
      addErrors (ForMap sku (salesV, naiveV, forecastV)) =
                   ForMap sku $ WeeklySalesWithForecastErrors salesV
                                                              (computeAbsoluteError (Actual salesV) naiveV)
                                                              (computeAbsoluteError (Actual salesV) forecastV)
      addForecast (ForMap sku (salesV, naive)) = ForMap sku (salesV, naive, forecast) where
         forecast =  findWithDefault v0 sku skuMap
      conduit = alignConduit salesSource naiveSource
                         .|mapC  joinWithZeror
                         .|mapC addForecast
                         .|mapC  addErrors
  return ((start, end), conduit)


getPlotForecastError :: Ord k => ForecastGrouper k -> Day -> FilePath -> Handler (Widget, ForecastSummary)
getPlotForecastError grouper day path = do
   today <- todayH
   ((start, end), salesConduit) <- getForecastErrors grouper day path
   salesM <- runDB $ runConduit
                   $ salesConduit
                   .| mapC forMapValue
                   .| C.foldl1 (<>)
   let ftime = formatTime0 @Text "%a %d %b %Y"
   case salesM of
        Just (WeeklySalesWithForecastErrors salesByWeek naive forecast) ->  do
           let plot = plotForecastError ("forecast-" <> pack path <> grouperName ) start today salesByWeek  naive forecast
               grouperName = case grouper of
                              SkuGroup -> "sku"
                              CategoryGroup category -> category
                              CustomerGroup -> "customer"
           return ([whamlet|
                     <div> #{path}
                     ^{plot}
                     <div>forecast for period : #{ftime start} - #{ftime end} 
                   |]
                  , let overPercent = percentFor $ overError forecast
                        underPercent = percentFor $ underError forecast
                        overallPercent = overPercent + underPercent
                        naivePercent = percentFor $ vadd (overError naive) (underError naive)
                        aes = overallPercent / naivePercent
                        percentFor v = lastGood v / lastGood salesByWeek
                        trendPercent = lastGood salesByWeek / lastGood (forecastCumul naive) -1
                        weeksToToday =  weeksTo start today
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

-- | Computes the number of weeks from start to today if needed
-- This is the week when the actual sales stops if Today is in a given year
weeksTo start today =  fromInteger $ case diffDays today start `div` 7 of
                                        n | n < 0 -> 51
                                        n ->  min n 51

data OffenderSummary = OffenderSummary { osActual, osForecast, osNaive, osError :: Double }
   deriving (Show)

getMostOffenders :: Ord k => ForecastGrouper k -> Int -> Day -> FilePath -> Handler (Maybe ([(Text, OffenderSummary)], [(Text, OffenderSummary)]))
getMostOffenders grouper topN day path = do
  today <- todayH
  ((start, _end), salesConduit) <- getForecastErrors grouper  day path
  let mkTopOffenders (ForMap key weekly) = let offenderSummary = OffenderSummary{..}
                                               get f = lastGood $ f weekly
                                               osActual = get wsSales
                                               osForecast = get (forecastCumul . wsForecast)
                                               osNaive = get (forecastCumul . wsNaiveError)
                                               osError = osActual - osForecast
                                               sku = unForecastKey grouper key 
                                           in ([(sku, offenderSummary)],  [(sku, offenderSummary)])
      -- vvvv silly we should use a normal fold and  as A or B is always a singleton list
      keepOffenders (topsA, bottomsA) (topsB, bottomsB) = let 
              bots = sortOn (Down . osError .snd)  (bottomsA ++ bottomsB)
              tops = sortOn (osError .snd)  (topsA ++ topsB)
              in (take topN tops, take topN bots)
      lastGood v = v V.! weeksToToday
      weeksToToday = weeksTo start today

  runDB $ runConduit
                     $ salesConduit
                     .| mapC mkTopOffenders
                     .| C.foldl1 keepOffenders
                     
makeOffenderTable :: Text -> [(Text, OffenderSummary)] -> Widget
makeOffenderTable categoryName summaries =  do
   let errorP os = case osActual os of 
                      0 | osError os == 0 -> 0  
                      0 -> 100
                      actual -> abs(100 * (osError os  / actual))
   [whamlet|
     <table.table.table-border.table-hover.table-striped>
       <theader>
         <tr>
           <th.just-right> #{categoryName}
           <th.just-right> Forecast
           <th.just-right> Actual
           <th.just-right> Error
           <th.just-right> %
           <th.just-right> Naive (previous year)
       <tbody>
         $forall (category, os) <-  summaries
          <tr>
            <td> #{category}
            <td.just-right> #{formatQuantity $ osForecast os}
            <td.just-right> #{formatQuantity $ osActual os}
            <td.just-right> #{formatQuantity $ abs (osError os)}
            <td.just-right> #{formatPercentage $ errorP os}
            <td.just-right> #{formatQuantity $ osNaive os}
   |]

makeSummaryTable :: [(Day, Text, ForecastSummary)] -> Widget
makeSummaryTable  day'path'summarys  = do
  let toPercent x = formatPercentage (x*100)
      ssum = averageForecastSummary $ map (\(_,_,s) -> s) day'path'summarys
  [whamlet|
              <table.table.table-hover.table-striped>
                <thead>
                  <tr>
                    <th> Date
                    <th> Method
                    <th.just-right> Scaled Error
                    <th.just-right> % Under estimation
                    <th.just-right> % Over estimation
                    <th.just-right> % Error estimation
                    <th.just-right> % Error Naive
                    <th.just-right> % Trend
                <tbody>
                  $forall (day, path, summary) <- day'path'summarys
                    <tr>
                      <td>#{tshow day}
                      <td><a href=@{DashboardR $ DForecastDetailedR $ Just $ path}> #{path}
                      <th.just-right>#{toPercent $ aes summary}
                      <td.just-right>#{toPercent $ underPercent summary}
                      <td.just-right>#{toPercent $ overPercent summary}
                      <th.just-right>#{toPercent $ overallPercent summary}
                      <td.just-right>#{toPercent $ naivePercent summary}
                      <td.just-right.negative-bad.positive-good>#{toPercent $ trendPercent summary}
                <tfooter>
                  <tr>
                    <th> Average
                    <td>
                    <th.just-right>#{toPercent $ aes ssum}
                    <th.just-right>#{toPercent $ underPercent ssum}
                    <th.just-right>#{toPercent $ overPercent ssum}
                    <th.just-right>#{toPercent $ overallPercent ssum}
                    <th.just-right>#{toPercent $ naivePercent ssum}
                    <th.just-right.negative-bad.positive-good>#{toPercent $ trendPercent ssum}
    |]
