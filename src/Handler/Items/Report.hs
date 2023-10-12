{-# OPTIONS_GHC -Wno-unused-top-binds #-} -- TODO remove
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-} -- TODO remove
module Handler.Items.Report
( getItemsReportR
, postItemsReportR
, getItemsReport2R
, postItemsReport2R
, getItemsReport3R
, postItemsReport3R
, getItemsReportSalesForecastR
, getItemsReportSalesForecastByStyleR
, getItemsReportSalesForecastByColourR
, getItemsReportSalesForecastReviewR
, getItemsReportSalesForecastReviewScatterR
, getItemsReportSalesForecastStockR
, getItemsReportSalesForecastStock6R
) where

import Import
import Items.Types
import Handler.Items.Reports.Common
import Text.Blaze.Html.Renderer.Text(renderHtml)
import qualified Data.List as List
import GL.Utils

-- * Form 
reportForm :: [Column] -> Maybe ReportParam -> Html -> MForm Handler (FormResult ReportParam, Widget)
reportForm cols paramM extra = do
  today <- todayH
  deduceTax <- appReportDeduceTax <$> getsYesod appSettings 
  categories <- lift $ categoriesH
  forecastDirOptions <- lift getForecastDirOptions

  let colOptions = [(colName c,c) | c <- cols]
      categoryOptions = [(cat, cat) | cat <-categories ]
      inoutwardOptions = [ (ios <> " - " <> dates <> " - " <> qtys, (io, date, qty) ) 
                         | (ios, io) <- [("As Sales" :: Text, Outward), ("As Purchase", Inward)]
                         , (dates, date) <- [ ("Order Date", OOrderDate), ("Delivery Date", ODeliveryDate) ]
                         , (qtys, qty) <- [ ("Ordered Quantity", OOrderedQuantity), ("Left Quantity", OQuantityLeft)]
                         ] 
      forecastOptions = [ ("As Sales - Outward" :: Text, Outward )
                        , ("As Purchase - Inward", Inward)
                        ]
      salesOptions = [ ("Transaction only" :: Text, SSalesOnly )
                     , ("With Order info", SSalesAndOrderInfo)
                     ]
      poOptions = [ (dates <> " - " <> qtys, (date, qty) ) 
                  | (dates, date) <- [ ("Order Date" :: Text, OOrderDate), ("Delivery Date", ODeliveryDate) ]
                  , (qtys, qty) <- [ ("Ordered Quantity", OOrderedQuantity), ("Left Quantity", OQuantityLeft)]
                  ]
  (fFrom, vFrom) <- mopt dayField "from" (Just $ rpFrom =<< paramM )
  (fTo, vTo) <- mopt dayField "to" (Just $ rpTo =<< paramM)
  (fPeriod, vPeriod) <- mopt (selectFieldList periodOptions) "period" (Just $ rpPeriod' =<< paramM)
  (fPeriodN, vPeriodN) <- mopt intField "number" (Just $ rpNumberOfPeriods =<< paramM)
  (fCategoryToFilter, vCategoryToFilter) <- mopt (selectFieldList categoryOptions ) "category" (Just $ rpCategoryToFilter =<< paramM)
  (fCategoryFilter, vCategoryFilter) <- mopt filterEField  "filter" (Just $ rpCategoryFilter =<< paramM)
  (fStockFilter, vStockFilter) <- mopt filterEField  "sku" (Just $ rpStockFilter =<< paramM)
  (fPanel, wPanel) <- ruptureForm colOptions "Panel" (rpPanelRupture <$> paramM)
  (fBand, wBand) <- ruptureForm colOptions "Band" (rpBand <$> paramM)
  (fSerie, wSerie) <- ruptureForm colOptions "Series" (rpSerie <$> paramM)
  (fColRupture, vColRupture) <- ruptureForm colOptions  "Columns" (rpColumnRupture <$> paramM)
  (fTrace1, wTrace1) <- traceForm 1 False "" (rpDataParam <$> paramM)
  (fTrace2, wTrace2) <- traceForm 2 False " 2" (rpDataParam2 <$> paramM)
  (fTrace3, wTrace3) <- traceForm 3 False " 3"(rpDataParam3 <$> paramM)
  (fSales, vSales) <- mopt (selectFieldList salesOptions) "Sales" (rpLoadSalesAndInfo <$> paramM)
  -- (fOrderInfo, vOrderInfo) <- mreq checkBoxField "OrderInfo" (rpLoadOrderInfo <$> paramM)
  (fOrder, vOrder) <- mopt (selectFieldList inoutwardOptions) "Sales Order" (rpLoadSalesOrders <$> paramM)
  (fPurchases, vPurchases) <- mreq checkBoxField "Purchases" (rpLoadPurchases <$> paramM)
  (fPurchasesDateOffset, vPurchasesDateOffset) <- mopt intField "Purchases Date Offset" (rpPurchasesDateOffset  <$> paramM)
  (fPOrders, vPOrders) <- mopt (selectFieldList poOptions) "Purchase Orders" (rpLoadPurchaseOrders <$> paramM)
  (fAdjustment, vAdjustment) <- mreq checkBoxField "Adjustment" (rpLoadAdjustment <$> paramM)
  (fForecast, vForecast) <- mopt (selectFieldList forecastDirOptions) "Forecast Profile" (rpForecastDir <$> paramM)
  (fForecastInOut, vForecastInOut) <- mopt (selectFieldList forecastOptions) "Forecast Mode" (rpForecastInOut <$> paramM)
  (fForecastStart, vForecastStart) <- mopt dayField "start" (Just $ rpForecastStart =<< paramM )
  (fColourMode, vColourMode) <- mreq (selectField optionsEnum) "Chart Colour Mode" (rpColourMode <$> paramM)
  (fGroupTrace, vGroupTrace) <- mopt (selectField optionsEnum) "Trace Group Mode" (rpTraceGroupMode <$> paramM)
  let fields = [ Left $ mapM_ renderField [vFrom, vTo, vPeriod, vPeriodN]
               , Left $ mapM_ renderField [vStockFilter, vCategoryToFilter, vCategoryFilter]
               , Right ("panel-rupture" :: Text, [wPanel, wBand, wSerie , vColRupture ])
               , Right ("panel-trace", [ wTrace1, wTrace2, wTrace3 ])
               , Left $ mapM_ renderField [vSales, vOrder]
               , Left $ mapM_ renderField [vPurchases, vPurchasesDateOffset, vPOrders ]
               , Left $ mapM_ renderField [vAdjustment]
               , Left $ mapM_ renderField [vForecast, vForecastInOut, vForecastStart]
               , Left $ mapM_ renderField [vColourMode, vGroupTrace]
               ]
  let form = [whamlet|#{extra}|] >> mapM_ (either inline dragablePanel) fields >> dragAndDropW
      inline w = [whamlet| <div.well.well-sm.form-inline>^{w}|]
      dragablePanel (panelId, ws) =  [whamlet|
           <div.well style="background:white" id="#{panelId}">
             $forall w <- ws
               <div.well.well-sm.form-inline
                      style="cursor:move"
                      draggable="true"
                      ondragstart="startDrag('#{panelId}',event)"
                      ondragend="endDrag('#{panelId}',event)"
                      ondragover="dragOver('#{panelId}',event)"
                      ondragenter="dragEnter('#{panelId}',event)"
                      ondragLeave="dragLeave('#{panelId}',event)"
                      ondrop="drop('#{panelId}',event)"
               >
                  <span.glyphicon.glyphicon-move>
                  ^{w}
                                  |]
      dragAndDropW = $(widgetFile "Items/report-dnd")
      -- julius = $(juliusFileReload "/home/max/devel/mae/Fames/src/Handler/Items/Reports/dnd-form.julius")
      -- cassius = $(cassiusFileReload "/home/max/devel/mae/Fames/src/Handler/Items/Reports/dnd-form.cassius")
      report = mkReport today deduceTax fFrom  fTo
          fPeriod  fPeriodN
          fCategoryToFilter  fCategoryFilter
          fStockFilter  fPanel  fBand  fSerie
          fColRupture  fTrace1  fTrace2  fTrace3
          fSales  fOrder fPurchases fPurchasesDateOffset fPOrders  fAdjustment (liftA3 (,,) fForecast fForecastInOut fForecastStart) fColourMode fGroupTrace
  return (report , form)
 
  
allTraces traceN = 
         [ noneOption
         , ("QA-Sales", quantityAmountStyle traceN Outward)
         , ("CumulAmount (Out)" ,   [(qpAmount Outward, VAmount, cumulAmountStyle traceN, RunSum)] )
         , ("CumulAmount Backward (Out)" ,   [(qpAmount Outward, VAmount, cumulAmountStyle traceN, RunSumBack)] )
         , ("CumulAmount (In)" ,   [(qpAmount Inward, VAmount, cumulAmountStyle traceN, RunSum)] )
         , ("Stock (In)" ,   [(qpQty Inward, VQuantity , quantityStyle traceN `nameStyle` "Stock"
                         , RunSum)] )
         , amountOutOption traceN
         , amountInOption traceN
         , ("Amount (Bare)",   [(_qpAmount, VAmount, amountStyle traceN, RSNormal)])
         , ("CumulAmount (Bare)" ,   [(_qpAmount, VAmount, cumulAmountStyle traceN, RunSum)] )
         , ("Quantity (Out)",  [(qpQty Outward, VQuantity, quantityStyle traceN, RSNormal)])
         , ("Quantity (In)",   [(qpQty Inward, VQuantity, quantityStyle traceN, RSNormal)])
         , ("Quantity (Bare)", [(_qpQty, VQuantity,           quantityStyle traceN, RSNormal)] )
         , ("CumulQuantity (Out)" ,   [(qpQty Outward, VQuantity , quantityStyle traceN, RunSum)])
         , ("Avg Price",       [(qpAveragePrice, VPrice,   priceStyle, RSNormal)])
         , ("Min Price",       [(qpMinPrice, VPrice,       priceStyle, RSNormal)])
         , ("Max Price",       [(qpMaxPrice, VPrice,       priceStyle, RSNormal)])
         , ("Price Band",          pricesStyle)
         , ("QuantityWithCumul (Out)", [ (qpQty Outward, VQuantity, smoothStyle QuantityAxis, RSNormal)
                                      , (qpQty Outward, VQuantity , hvNoMarkerStyle QuantityAxis , RunSum)])
         , ("QuantityWithCumul (In)", [ (qpQty Inward, VQuantity, smoothStyle QuantityAxis, RSNormal)
                                      , (qpQty Inward, VQuantity , hvNoMarkerStyle QuantityAxis , RunSum)])
         ]
{-# NOINLINE mkReport #-}
mkReport today deduceTax fFrom  fTo
   fPeriod  fPeriodN
   fCategoryToFilter  fCategoryFilter
   fStockFilter  fPanel  fBand  fSerie
   fColRupture  fTrace1  fTrace2  fTrace3
   fSales  fOrder fPurchases fPurchasesDateOffset fPOrders  fAdjustment fForecast
   fColourMode fGroupTrace =
  ReportParam <$> pure today <*> pure deduceTax <*> fFrom <*> fTo
  <*> fPeriod <*> fPeriodN
  <*> fCategoryToFilter <*> fCategoryFilter
  <*> fStockFilter <*> fPanel <*> fBand <*> fSerie
  <*> fColRupture <*> fTrace1 <*> fTrace2 <*> fTrace3
  <*> fSales <*> fOrder
  <*> fPurchases <*> fPurchasesDateOffset <*> fPOrders
  <*> fAdjustment
  <*> fForecast
  <*> fColourMode <*> fGroupTrace
  -- noneOption, amountOutOption, amountInOption :: ToJSON a =>  (Text, [(QPrice -> Amount, a -> [(Text, Value)], RunSum)])
traceForm :: Int -> Bool -> String -> Maybe DataParams -> MForm Handler (FormResult DataParams, Widget)
traceForm traceN nomode suffix p = do
  let
    dataTypeOptions = optionsPairs [(drop 2 (tshow qtype), qtype) | qtype <- [minBound..maxBound]]
    dataValueOptions = map (\(t_, tps'runsum) -> (t_, Identifiable (t_, (map mkTraceParam tps'runsum))))
                           (allTraces traceN)
  (tType, vType) <-  mreq (selectField dataTypeOptions)
                           (fromString $ "type" <> suffix)
                           (dpDataType <$> p) 
  (tParam, vParam) <- mreq (selectFieldList dataValueOptions)
                                            (fromString $ "value" <> suffix)
                                            (dpDataTraceParams <$> p) 
  (tNormTarget, vNormTarget) <- mopt (selectField optionsEnum) (fromString $ "normTarget" <> suffix) (nmTarget <$$> dpDataNorm <$> p)
  (tNormMargin, vNormMargin) <- mopt (selectField optionsEnum) (fromString $ "normMargin" <> suffix) (nmMargin <$$> dpDataNorm <$> p)
  let form =  mapM_ renderField $ [vType, vParam ] <> if nomode then [] else [ vNormMargin, vNormTarget]
      norm = liftA2 NormalizeMode <$> tNormMargin <*> tNormTarget
  return (DataParams <$> tType <*> tParam <*> norm, form )

ruptureForm :: [(Text, Column)] -> String -> Maybe ColumnRupture -> MForm Handler (FormResult ColumnRupture, Widget)
ruptureForm colOptions title paramM = do
  (fColumn, vColumn) <- mopt (selectFieldList colOptions) (fromString title)  (cpColumn <$> paramM) 
  (fSortBy, wSortBy) <- traceForm 1 True ("" ) (cpSortBy <$> paramM)
  (fRankMode, vRankMode) <- mopt (selectField optionsEnum) ("" ) (cpRankMode <$> paramM)
  (fLimitTo, vLimitTo) <- mopt intField "Limit To" (Just $ cpLimitTo =<< paramM)
  (fReverse, vReverse) <- mreq boolField "reverse" (cpReverse <$> paramM)

  let form = mapM_ (either renderField id) [ Left vColumn
                                           , Right [whamlet|<div.form-group style="margin-bottom: 5px">
                                                                             <label style="width:100%; text-align:center">Sort By
                                                                             <div.form-group>^{wSortBy}
                                                                             |]
                                           , Left vRankMode
                                           , Left vLimitTo
                                           , Left vReverse
                                           ]
  return (ColumnRupture <$> fColumn <*>  fSortBy <*> fRankMode <*> fLimitTo <*> fReverse, form)
  

getForecastDirOptions :: Handler [(Text, FilePath)]
getForecastDirOptions = getSubdirOptions appForecastProfilesDir

-- * Handler 

{-# NOINLINE getItemsReportR #-}
getItemsReportR :: Maybe ReportMode -> Handler TypedContent
getItemsReportR mode = do
  actionM <- lookupGetParam "action"
  case actionM of
    Nothing -> getItemsReportR' mode
    Just _ -> postItemsReportR mode
getItemsReportR' mode = do
  today <- todayH
  deduceTax <- appReportDeduceTax <$> getsYesod appSettings 
  (_, (band, serie, timeColumn)) <- getColsWithDefault
  let -- emptyRupture = ColumnRupture Nothing emptyTrace Nothing Nothing False
      bestSales = DataParams QPSales (mkIdentifialParam $ amountInOption 1) Nothing
      sales = DataParams QPSales (mkIdentifialParam $ amountOutOption 1) Nothing
      past = calculateDate (AddMonths (-3)) today
      tomorrow = calculateDate (AddDays 1) today
  
      defaultReportParam = case mode of
        Just ReportChart -> ReportParam
                            today
                            deduceTax
                           (Just past) --  rpFrom :: Maybe Day
                           (Just tomorrow) --  rpTo :: Maybe Day
                           Nothing --  rpPeriod
                           Nothing --  rpNumberOfPeriods
                           Nothing -- rpToCategoryToFilter
                           Nothing -- rpToCategoryFilter
                           Nothing --  rpStockFilter :: Maybe FilterExpression
                           emptyRupture   -- rpPanelRupture :: ColumnRupture
                           (ColumnRupture (Just band)  bestSales (Just RMResidual) (Just 20) False)--  rpBand :: ColumnRupture
                           (ColumnRupture (Just serie)  bestSales (Just RMResidual) (Just 20) False)--  rpSerie :: ColumnRupture
                           (emptyRupture {cpColumn = Just timeColumn})
                           sales --  rpDataParam :: DataParams
                           emptyTrace --  rpDataParam2 :: DataParams
                           emptyTrace --  rpDataParam3 :: DataParams
                           (Just SSalesOnly) Nothing
                           True Nothing Nothing -- purchases
                           True (Nothing, Nothing, Nothing)
                           minBound Nothing
        _ -> ReportParam   today
                           deduceTax
                           (Just past) --  rpFrom :: Maybe Day
                           (Just tomorrow) --  rpTo :: Maybe Day
                           Nothing --  rpPeriod
                           Nothing --  rpNumberOfPeriods
                           Nothing -- rpToCategoryToFilter
                           Nothing -- rpToCategoryFilter
                           Nothing --  rpStockFilter :: Maybe FilterExpression
                           (ColumnRupture (Just band) bestSales (Just RMResidual) (Just 20) False)--  rpBand :: ColumnRupture
                           (ColumnRupture (Just serie) bestSales (Just RMResidual)  (Just 20) False)--  rpSerie :: ColumnRupture
                           emptyRupture   -- rpPanelRupture :: ColumnRupture
                           (emptyRupture {cpColumn = Just timeColumn}) --  rpColumnRupture :: Column
                           sales --  rpDataParam :: DataParams
                           emptyTrace --  rpDataParam2 :: DataParams
                           emptyTrace --  rpDataParam3 :: DataParams
                           (Just SSalesOnly) Nothing
                           True Nothing Nothing -- Purchases
                           True (Nothing, Nothing, Nothing)
                           minBound Nothing

  renderReportForm ItemsReportR mode (Just defaultReportParam) ok200 Nothing

{-# NOINLINE getItemsReport2R #-}
getItemsReport2R :: Maybe ReportMode -> Handler Html
getItemsReport2R _ = do
  redirect $ ItemsR (ItemsReportR (Just ReportChart))
  -- renderReportForm ItemsReport2R mode Nothing ok200 Nothing

getItemsReport3R mode = do 
  renderReportForm ItemsReport3R mode Nothing ok200 Nothing


salesForecastParamH :: Day -> FilePath -> Maybe Int -> Handler ReportParam
salesForecastParamH today forecastPath periodm = do
  catProfile <- appForecastCollectionCategory <$> getsYesod appSettings
  forecastDir <- appForecastProfilesDir <$> getsYesod appSettings
  return 
    (defaultReportParam today Nothing)
      { rpToday = today
      , rpFrom = Just from
      , rpTo = Just $ calculateDate (Chain [AddDays $ -1, AddYears 1]) from
      , rpPeriod' = fmap  (const PFSlidingYearFrom) periodm
      , rpNumberOfPeriods = periodm
      , rpSerie = ColumnRupture  (Just periodColumn) (DataParams QPSummary i0 Nothing) Nothing Nothing True
      , rpColumnRupture = ColumnRupture  (Just monthlyColumn) (DataParams QPSummary i0 Nothing) Nothing Nothing False
      , rpDataParam = DataParams QPSales (tr 1 "Quantity (Out)" [])  Nothing
      , rpDataParam2 = DataParams QPSales (tr 2 "CumulQuantity (Out)" [])  Nothing
      , rpLoadSalesAndInfo = Just SSalesOnly
      , rpLoadAdjustment = False
      , rpForecast = (Just $ forecastDir </> forecastPath , Just Outward, Just (calculateDate (AddDays 1) today))
      , rpCategoryToFilter = Just catProfile
      , rpCategoryFilter = Just $ LikeFilter "_%"
      }
  where from = calculateDate BeginningOfMonth today
i0 = Identifiable ("Column", [])
tr :: Int -> Text -> [Text] -> Identifiable [TraceParam]
tr n name names = Identifiable ( name
                               , concat [ map (mkTraceParam . updateName traceName) tp
                                        | ((_, tp), traceName) <- zip (filter ((== name) . fst) (allTraces n))
                                                                      (map Just names ++ repeat Nothing)
                                        ]
                               )
     where updateName (Just n) (a, b, c, d) = (a, b, c `nameStyle` n, d)
           updateName Nothing tp = tp
                   
{-# NOINLINE getItemsReportSalesForecastR #-}
getItemsReportSalesForecastR = do
  today <- todayH
  forecastPath <- appForecastDefaultProfile <$> getsYesod appSettings
  forecastProfileColumnM  <- getProfileColumnM
  param0 <- salesForecastParamH today forecastPath (Just 2)
  let param = param0
               { rpBand = ColumnRupture forecastProfileColumnM bestSalesTrace Nothing Nothing False
               }
  postItemsReportFor ItemsReportR (Just ReportChart) (Just param)
  

getItemsReportSalesForecastByStyleR = do
  today <- todayH
  forecastPath <- appForecastDefaultProfile <$> getsYesod appSettings
  forecastProfileColumnM  <- getProfileColumnM
  param0 <- salesForecastParamH today forecastPath (Just 2)
  let param = param0
               { rpPanelRupture = ColumnRupture forecastProfileColumnM bestSalesTrace Nothing Nothing False
               , rpBand = ColumnRupture (Just styleColumn) bestSalesTrace (Just RMResidualAvg) (Just 20) False
               }
  postItemsReportFor ItemsReportR (Just ReportChart) (Just param)

getItemsReportSalesForecastByColourR = do
  today <- todayH
  forecastPath <- appForecastDefaultProfile <$> getsYesod appSettings
  forecastProfileColumnM  <- getProfileColumnM
  param0 <- salesForecastParamH today forecastPath (Just 2)
  let param = param0
               { rpPanelRupture = ColumnRupture forecastProfileColumnM bestSalesTrace Nothing Nothing False
               , rpBand = ColumnRupture (Just variationColumn) bestSalesTrace (Just RMResidualAvg) (Just 20) False
               }
  postItemsReportFor ItemsReportR (Just ReportChart) (Just param)

getItemsReportSalesForecastReviewR = do
  today <- todayH
  forecastPath <- appForecastPreviousProfile <$> getsYesod appSettings
  forecastProfileColumnM  <- getProfileColumnM
  forecastStartM <- appForecastPreviousProfileStart <$> getsYesod appSettings
  let start = calculateDate BeginningOfMonth $ fromMaybe (calculateDate (AddYears $ -1) today) forecastStartM
  param0 <- salesForecastParamH start forecastPath Nothing
  let param = param0
               { rpPanelRupture = ColumnRupture forecastProfileColumnM bestSalesTrace Nothing Nothing False
               , rpBand = ColumnRupture (Just styleColumn) bestSalesTrace (Just RMResidualAvg) (Just 20) False
               , rpSerie = emptyRupture
               , rpDataParam = DataParams QPSales (tr 1 "QuantityWithCumul (Out)" ["Sales", "Sales"])  Nothing
               , rpDataParam2 = DataParams QPSalesForecast (tr 2 "QuantityWithCumul (In)" ["Forecast", "Forecast"])  Nothing
               , rpForecast = let (path, _, day) = rpForecast param0 in (path, Just Inward, day)
               }
  postItemsReportFor ItemsReportR (Just ReportChart) (Just param)

getItemsReportSalesForecastReviewScatterR = do
  today <- todayH
  forecastPath <- appForecastPreviousProfile <$> getsYesod appSettings
  forecastProfileColumnM  <- getProfileColumnM
  forecastStartM <- appForecastPreviousProfileStart <$> getsYesod appSettings
  let start = calculateDate BeginningOfMonth $ fromMaybe (calculateDate (AddYears $ -1) today) forecastStartM
  param0 <- salesForecastParamH start forecastPath Nothing
  let param = param0
               { rpPanelRupture = ColumnRupture forecastProfileColumnM bestSalesTrace Nothing Nothing False
               , rpBand = emptyRupture
               , rpSerie = ColumnRupture (Just styleColumn) bestSalesTrace Nothing (Just 200) False -- set colour
               , rpColumnRupture = ColumnRupture (Just skuColumn) bestSalesTrace Nothing (Just 200) False
               , rpDataParam = DataParams QPSales (tr 1 "QuantityWithCumul (Out)" ["Sales", "Sales"])  Nothing
               , rpDataParam2 = DataParams QPSalesForecast (tr 2 "QuantityWithCumul (In)" ["Forecast", "Forecast"])  Nothing
               }
  postItemsReportFor ItemsReportR (Just ReportScatter) (Just param)

getItemsReportSalesForecastStockR = do
  today <- todayH
  forecastPath <- appForecastDefaultProfile <$> getsYesod appSettings
  let start = calculateDate BeginningOfMonth today
  param0 <- salesForecastParamH start forecastPath Nothing
  let param = param0
               { rpPanelRupture = emptyRupture
               , rpFrom = Just $ calculateDate (AddYears $ -1) start -- 2 years
               , rpBand = ColumnRupture (Just skuColumn)
                                        (DataParams QPSummary (tr 2 "Stock (In)" []) Nothing)
                                        Nothing
                                        (Just 100)
                                        False 
               -- display first the item most needed, indeed most negative stock quantity*amount at the end
               , rpSerie = emptyRupture
               , rpDataParam = DataParams QPSales (tr 1 "Quantity (Out)" ["Sales & Forecast"])  Nothing
               , rpDataParam2 = DataParams QPSummary (tr 2 "Stock (In)" ["Stock"])  Nothing
               , rpDataParam3 = DataParams QPPurchase (tr 2 "Quantity (In)" ["Purchases"])  Nothing
               , rpLoadPurchases = True
               , rpLoadPurchaseOrders = Just (ODeliveryDate, OQuantityLeft)
               , rpLoadAdjustment = True
               }
  postItemsReportFor ItemsReportR (Just ReportChart) (Just param)

getItemsReportSalesForecastStock6R = do
  today <- todayH
  forecastPath <- appForecastDefaultProfile <$> getsYesod appSettings
  let start = calculateDate BeginningOfMonth today
  param0 <- salesForecastParamH start forecastPath Nothing
  let param = param0
               { rpPanelRupture = emptyRupture
               , rpFrom = Just $ calculateDate (AddMonths $ -6) start
               , rpTo = Just $ calculateDate (Chain [AddDays $ -1, AddMonths  6]) start 
               , rpBand = ColumnRupture (Just skuColumn)
                                        (DataParams QPSummary (tr 2 "Stock (In)" []) Nothing)
                                        Nothing
                                        (Just 100)
                                        False 
               -- display first the item most needed, indeed most negative stock quantity*amount at the end
               , rpSerie = emptyRupture
               , rpDataParam = DataParams QPSales (tr 1 "Quantity (Out)" ["Sales & Forecast"])  Nothing
               , rpDataParam2 = DataParams QPSummary (tr 2 "Stock (In)" ["Stock"])  Nothing
               , rpDataParam3 = DataParams QPPurchase (tr 2 "Quantity (In)" ["Purchases"])  Nothing
               , rpLoadPurchases = True
               , rpLoadPurchaseOrders = Just (ODeliveryDate, OQuantityLeft)
               , rpLoadAdjustment = True
               }
  postItemsReportFor ItemsReportR (Just ReportChart) (Just param)

getProfileColumnM = do
  catColumns <- categoryColumnsH
  catProfile <- appForecastCollectionCategory <$> getsYesod appSettings
  let colProfile =  "item:" ++ catProfile
  return $ headMay $ filter (\c -> colName c == colProfile) catColumns
  

runFormAndGetAction form = do
  fromPost@((resp, _), _) <- runFormPostNoToken form
  case resp of
    FormMissing -> (,) <$> runFormGet form <*> lookupGetParam "action"
    _ -> do
        action <- lookupPostParam "action"
        return (fromPost, action)


-- | Adjust if needed date param so that they can be dynamic
-- i.e. calculated on the fly for example to get last 3 months from today
-- It uses some special http field not part of the form.
-- They are meant to be used when storing url
-- for example to get report using the current day (and the last 3 months)
-- one needs to add to url
-- `calculate-from=[AddMonths (-3)]&calculate-to=[AddMonths 0]`
adjustParamDates :: ReportParam -> Handler ReportParam
adjustParamDates param = do
  newFrom <- update_ (rpFrom param) "calculate-from"
  newTo <- update_ (rpTo param) "calculate-to"
  return param {rpFrom = newFrom, rpTo = newTo }
  where update_ day field = do
          calc <- lookupGetParam field
          return $ case fmap readMay calc of
                     Just (Just cs@(_:_)) -> Just $ foldl'(flip ($)) (rpToday param) (map calculateDate cs)
                     Just Nothing -> error $ "Can't parse " ++ unpack field ++  " : " ++ show calc
                     _ -> day
          
  

-- | React on post. Actually used GET So should be removed
postItemsReportR mode = postItemsReportFor ItemsReportR mode Nothing
postItemsReportFor route mode paramM= do
  cols <- getCols
  (formRan, actionM) <- runFormAndGetAction (reportForm cols Nothing)
  let ((resp, _), _) = formRan
  param <- case (paramM, resp) of
                   (Just p, _) -> return p
                   (Nothing, FormMissing) -> error "form missing"
                   (Nothing, FormFailure a) -> error $ "Form failure : " ++ show a
                   (Nothing, FormSuccess param0) -> do
                     adjustParamDates param0
  let panel_ = rpPanelRupture param
      band = rpBand param
      serie = rpSerie param
      col = rpColumnRupture param
      -- for table, the exact meaning of the rupture doesn't matter
      tableGrouper = panel_ : filter (isJust . cpColumn) [band, serie]
      grouper = [ panel_, band, serie, col]
  case readMay =<< actionM of

    Just ReportCsv -> do
          let (grouper', formatter) = case fromMaybe ReportTable mode  of
                ReportTable ->  (tableGrouper, summaryToCsv QPMinMax)
                _ -> (grouper, tracesToCsv)
          result <- itemReportWithRank param (filter (isJust . cpColumn) grouper') (fmap snd) --  (fmap (fmap (summarize . map snd)))
          let source = yieldMany (map (<> "\n") (formatter param result))
          setAttachment . fromStrict $ "items-report-" <> (tshowM $ colName <$> (cpColumn $ rpPanelRupture param)) <> "-"
                                          <> (tshowM $ colName <$> (cpColumn $ rpBand param)) <> ".csv"
          respondSource "text/csv" (source .| mapC toFlushBuilder)
    Just ReportRaw -> do
         let grouper' = grouper <> map mkRupture (filter (`notElem` columnInGrouper) cols) -- all columns with grouper first
             columnInGrouper = mapMaybe cpColumn grouper
             mkRupture cols_ = emptyRupture  {cpColumn = Just cols_}
             formatter = summaryToCsv QPAvg
         result <- itemReportWithRank param (filter (isJust . cpColumn) grouper') (fmap snd) --  (fmap (fmap (summarize . map snd)))
         let source = yieldMany (map (<> "\n") (formatter param result))
         setAttachment . fromStrict $ "items-report-raw-" <> tshowM (rpFrom param) <> "-" 
                                                           <> tshowM (rpTo param) <> ".csv"
         respondSource "text/csv" (source .| mapC toFlushBuilder)
    _ -> do
          report <- case mode of
                Just ReportChart -> itemReportWithRank param grouper (chartProcessor param)
                Just ReportPivot -> itemReport param pivotProcessor
                Just ReportBubble -> itemReportWithRank param grouper (bubbleProcessor param)
                Just ReportScatter -> itemReportWithRank param grouper (scatterProcessor param)
                _ -> itemReportWithRank param tableGrouper (tableProcessor param)
                -- _                -> itemReportWithRank param grouper (pivotProcessorXXX param)
          renderReportForm route mode (Just param) ok200 (Just report)


{-# NOINLINE postItemsReport2R #-}
postItemsReport2R :: Maybe ReportMode -> Handler TypedContent
postItemsReport2R mode = postItemsReportFor ItemsReport2R  (mode <|> Just ReportCsv) Nothing

{-# NOINLINE postItemsReport3R #-}
postItemsReport3R :: Maybe ReportMode -> Handler TypedContent
postItemsReport3R mode = postItemsReportFor ItemsReport3R  mode Nothing
-- ** Renders 

renderReportForm :: (Maybe ReportMode -> ItemsR)
                 -> Maybe ReportMode
                 -> Maybe ReportParam
                 -> Status
                 -> Maybe Widget
                 -> Handler TypedContent
renderReportForm  route modeM paramM status resultM = do
  cols <- getCols
  (repForm, repEncType) <- generateFormGet' $ reportForm cols paramM
  let buttons = [(ReportCsv, "Export To Csv" :: Text), (ReportRaw, "Export Raw CSV")]
      navs = ([minBound..maxBound] :: [ReportMode]) List.\\ map fst buttons
      -- \^ We use a button instead for the CSV
      mode = fromMaybe ReportTable modeM
      navClass nav = if mode == nav then "active" else "" :: Html
      fay = $(fayFile "ItemsReport")
      -- in theory we only need plotly when display a chart
      -- but it is need to be loaded on the first page, otherwise
      -- Ajax call with plot won't work
      -- plotly = addScriptRemote "https://cdn.plot.ly/plotly-latest.min.js"
      widget = [whamlet|
    <form #items-report-form role=form method=get action="@{ItemsR (route modeM)}" enctype="#{repEncType}">
        $maybe result <- resultM
          <ul.nav.nav-tabs>
            $forall nav <- navs
              <li class=#{navClass nav}>
                $if nav == ReportCsv
                  <a href="@{ItemsR (ItemsReportR (Just nav))}"> #{splitSnake $ drop 6 $ tshow nav}
                $else
                  <a.view-mode href="#" data-url="@{ItemsR (ItemsReportR (Just nav))}"> #{splitSnake $ drop 6 $ tshow nav}
          <div#items-report-result>
            ^{result}
      <div.well>
        ^{repForm}
        <button.btn type="submit" name="action" value="Submit"> Submit
        $forall (btn, title) <- buttons
          <button.btn.btn-info type="submit" name="action" value="#{tshow btn}">#{title}
    <div.panel.panel-info>
      
      #{reportDoc}
                        |]
  cacheSeconds (23*3600)
  selectRep $ do
    provideRep $ do
      html <- sendResponseStatus status =<< defaultLayout (toWidget commonCss >> widget >> fay) 
      return (html :: Html)
    provideRep $ do -- Ajax. return result
      div_ <- widgetToPageContent (fromMaybe (return ()) resultM)
      html <- withUrlRenderer (pageBody div_)
      returnJson (renderHtml html)

reportDoc = [shamlet|
<div.panel-heading>
  <h3>
    <span.data-toggler.collapsed data-toggle=collapse data-target="#report-doc"> Report Doc
<div.panel-body>
  <div.pre.collapse id=report-doc>
      #{reportDoc'}
|]


reportDoc' = [shamlet|
      <p> Main input parameters can be swapped by using drag and drop.
      <p> The default is to ignore and leave in place the residual/margin parameters (Residal, limits etc...)
      <p> However holding <code>shift</code> whilst dropping will swap the full row of parameters.
      <p> Holding <code>ctrl</code> will copy the parameters instead of swapping.
      <p> In addition, dropping on a field, will only copy this field and the one of the right.
      <p> Holding <code>shift</code> will not copy the residual parameters.
      <table.table.table-striped.table-hover>
        <thead>
          <tr>
            <th scope="col" >&#xa0;
            <th scope="col" >Table
            <th scope="col" >Chart
            <th scope="col" >Pivot
            <th scope="col" >Bubble
            <th scope="col" >Scatter
        <tbody>
          <tr>
            <td >Panel
            <td >Panel
            <td >Panel
            <td >Panel
            <td >Panel
            <td >Panell

          <tr>
            <td >Band
            <td >Row
            <td >Band
            <td >Band
            <td >Band
            <td >Band

          <tr>
            <td >Serie
            <td >Subrow
            <td >Line
            <td >Row
            <td >Y/Row
            <td >Colour/Label

          <tr>
            <td >Column
            <td >N/A
            <td >X
            <td >X/Column
            <td >X/Column
            <td >Point/Label

          <tr>
            <td >Trace 1
            <td >N/A
            <td >Line
            <td >Z/Subrow
            <td >Z/Size
            <td >X

          <tr>
            <td >Trace 2
            <td >N/A
            <td >Line
            <td >Z/Subrow
            <td >Z/Colour
            <td >Y

          <tr>
            <td >Trace 3
            <td >N/A
            <td >Line
            <td >Z/Subrow
            <td >&#xa0;
            <td >Z/Size
|]
