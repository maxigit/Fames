{-# LANGUAGE OverloadedStrings #-}
module Handler.WH.StockAdjustment where

import Import
import Yesod.Form.Bootstrap3
import Database.Persist.MySQL
import qualified Data.List as List
import Data.Time (addDays)
import Data.Time.Calendar.WeekDate (toWeekDate)

import SharedStockAdjustment
import qualified FA as FA

-- 3 sections
-- displays all, or each in a row
-- give a title, modify textcart to check title correct
-- Found : to move from LOST to DEF
-- Lost : DEF -> LOST
-- NEW : to creete

-- dodgy only | all | no dodgy
-- dodgy = one with stocktake = 

-- If the stocktake has been done the same day (around?) as a pick
-- we are unsure if the stocktake has been done before or after the pick

-- DONE filter style
-- DONE add all items
-- TODO add all container
-- TODO button save
-- TODO save  as csv -> compatible with old system
-- TODO parse
-- TODO check product exists
data Unsure = Sure | Unsure | All deriving (Eq, Read, Show, Enum, Bounded)


data CartMode = Lost | Found | New deriving (Eq, Read, Show, Enum)

-- If a stocktake represent a full style, all variations not present
-- should be lost .
data StyleMode = LooseMissing | Partial deriving (Eq, Read, Show, Enum)

data SortMode = SortByStyle | SortByQuantity deriving (Eq, Read, Show, Enum, Bounded)
data FormParam = FormParam
  { style :: Maybe Text
  , download :: Bool
  , minQty :: Maybe Int
  , maxQty :: Maybe Int
  , sortMode :: SortMode
  , unsure :: Unsure
  , modulo :: Maybe Int
  , comment :: Maybe Text
  } deriving (Eq, Read, Show)


data FormMode = Save | View deriving (Eq, Read, Show)
paramForm mode = renderBootstrap3 BootstrapBasicForm  form
  where form = FormParam
            <$> aopt textField "style" Nothing
            <*> areq boolField "download" (Just False)
            <*> aopt intField "min quantity" Nothing
            <*> aopt intField "max quantity" Nothing
            <*> areq (selectField optionsEnum)"sort by" Nothing
            <*> areq (selectField optionsEnum) "mode" (Just All)
            <*> aopt intField "modulo" Nothing
            <*>  (unTextarea <$$> case mode of
                   Save -> aopt textareaField "comment" Nothing
                   View -> pure Nothing)



getWHStockAdjustmentR :: Handler Html
getWHStockAdjustmentR = do
  renderStockAdjustment



renderStockAdjustment :: Handler Html
renderStockAdjustment = do
  (paramForm, encType) <- generateFormPost (paramForm View)
  pendings <- renderPending
  let response = [whamlet|
<div.panel.panel-info>
  <div.panel-heading>
    <h3> Pending Stock Adjustment
  <div.panel-body>
    ^{pendings}
<div.panel> 
  <div.panel-body>
    <form.well #stock-adjustement role=form method=post action=@{WarehouseR WHStockAdjustmentR} enctype=#{encType}>
      ^{paramForm}
      <button type="submit" name="submit" .btn.btn-default>Submit
|]

  defaultLayout response



postWHStockAdjustmentR :: Handler Html
postWHStockAdjustmentR = do
  action <- lookupPostParam "action"
  let mode = case action of
            Just "save" -> Save
            _ -> View
        

  ((resp, view), encType) <- runFormPost (paramForm Save)
  case resp of
    FormMissing -> error "Form missing"
    FormFailure a -> defaultLayout [whamlet|^{view}|]
    FormSuccess param -> do
      let (w,p) = case style param of
                  Just like  -> (" AND stock_id like ?", [PersistText like])
                  Nothing -> ("", [])
      let sql = "SELECT stock_id, COALESCE(SUM(quantity),0), MAX(date) "
                <> " FROM fames_stocktake  "
                <> " WHERE stock_adj_id IS NULL "
                <> " AND active = 1 "
                <> w
                <> " GROUP BY stock_id "

      stocktakes <- runDB $ rawSql sql p
      results <- catMaybes <$> mapM qohFor stocktakes
      let withDiff = [(abs (quantityTake0 (main pre) - quantityAt (main pre)), pre) |  pre <- results]
          f  (q, pre) = (maybe True (q >=) (minQty param))
                   &&  (maybe True (q <=) (maxQty param))
                   && let isUnsure = not . null $  concatMap (\loc -> movesAt $ loc pre) [main, lost]
                      in case unsure param of
                        All -> True
                        Unsure -> isUnsure
                        Sure -> not isUnsure

          filtered = filter f withDiff
      let


          rows = map snd $ case sortMode param of
            SortByStyle -> filtered
            SortByQuantity -> sortBy (comparing (\(q,pre) -> (Down q, sku pre ) ))  filtered
          

      -- use conduit ? TODO
      let classFor' qty qoh = case compare qty qoh of
            GT -> "success"
            EQ -> ""
            LT -> "danger" :: Text
          preToOriginal pre = (OriginalQuantities qtake (qoh-before) qlost (modulo param), before) where
            m = main pre
            qtake = quantityTake0 m
            qoh = quantityAt m
            qlost = quantityNow (lost pre)
            day = takeDate pre
            -- Move picked before the stock take have been taken into account
            -- in the QOH. We need to remove them to get the quantity excluding ALL moves
            before = sum [ movePickedQty move
                         | move <- movesAt m
                         , moveDate move <= day
                         ]
          -- | We want to pass the original quantities (without move) to the data- in html
          -- however the badges needs to be computed as if
          -- the "before" select boxes needs have been selected
          toOrigAndBadges pre = (orig, computeBadges orig {qoh = qoh orig + before}, before)
            where (orig, before) = preToOriginal pre
          classesFor [] = "" :: Text
          classesFor _ = "unsure danger" :: Text
      
      response <- case mode of
            Save -> saveStockAdj (comment param) rows
            View -> do
              let fay = $(fayFile "WHStockAdjustment")
              return $ fay <> [whamlet|
<div>
  <table.table.table-border.table-hover>
    <tr>
      <th> <input type="checkbox" id="stock-adj-active-all" checked>
      <th> Style
      <th> Stocktake
      <th> Date
      <th> QOH FA
      <th> lost
      <th> Last move
    $forall pre <- rows
      $with ((qties, badges, before), mainMoves, lostMoves) <- (toOrigAndBadges pre, (movesAt $ main pre), movesAt $ lost pre)
        <tr class="#{classesFor mainMoves}"
            id="#{sku pre}-row"
            data-sku="#{sku pre}"
            data-hidden="true"
            >
          <td.active><input type="checkbox" name="active-#{sku pre}" checked>
          <td.style>#{sku pre}
          <td.quantity data-original=#{qtake qties}>#{qtake qties}
            ^{badgeSpan (bMissing badges) (Just "#d9534f") "missing"}
            ^{badgeSpan (bMissingMod badges) (Just "#cccccc") "missing-mod"}
          <td.date>#{tshow $ (takeDate pre)}
          <td.qoh data-original=#{qoh qties}>
            <span.qoh>#{qoh qties + before}
            ^{badgeSpan (bFoundMod badges) (Just "#cccccc") "found-mod"}
            ^{badgeSpan (bFound badges) (Just "#29abe0") "found"}
            ^{badgeSpan (bNew badges) Nothing "new" }
          <td.lost data-original=#{qlost qties}>#{qlost qties}
          <td.last_move>#{fromMaybe "" (tshow <$> (lastMove pre))}
          $forall move <- mainMoves
            $with before <- moveDate move <= takeDate pre
              $with after <- not before
                <tr :before:.bg-info class="move sku-#{sku pre}" style="display:none">
                  <td> <select name="#{sku pre}">
                      <option :before:selected value="#{movePickedQty move}" >Before
                      <option :after:selected value="0">After
                  <td> #{movePickedQty move} #{moveCustomerName move}
                  <td> #{tshow $ moveDate move}
                  <td>
                  <td> #{(fromMaybe "" $ moveOperatorName move)}
                  <td>
|]
      defaultLayout [whamlet|
<form.well #stock-adjustment role=form method=post action=@{WarehouseR WHStockAdjustmentR} enctype=#{encType}>
  ^{view}
  <button type="submit" name="action" value="submit" .btn.btn-primary>Submit
  <button type="submit" name="action" value="save" .btn.btn-danger>Save
  ^{response}
|]


-- | Temporary data holding stock adjustment information (to display)
data LocationInfo = LocationInfo
  { location :: !Text
  , quantityTake :: (Maybe Int) -- quantity from stock take 
  , quantityAt :: Int -- quantity on hand at date
  , quantityNow :: !Int -- quantity on hand NOW. To avoid negative stock
  , date :: Maybe Day
  , movesAt :: [MoveInfo] -- any picking have been done within the "UNsure" date range
  } deriving (Eq, Read, Show)

quantityTake0 = fromMaybe 0 . quantityTake

noInfo locInfo = (quantityTake locInfo == Nothing) && (quantityAt locInfo == 0)

data PreAdjust = PreAdjust
  { sku :: !Text
  , main :: !LocationInfo
  , lost :: !LocationInfo
  , takeDate :: !Day
  } deriving (Eq, Read, Show)

adjustInfos :: PreAdjust -> [LocationInfo]
adjustInfos adj  = [main, lost] <*> [adj]

data MoveInfo = MoveInfo
 { moveDate :: !Day
 , moveCustomerName :: !Text
 , moveOperatorName :: Maybe Text
 , movePickedQty :: !Int
 } deriving (Eq, Read, Show)

lastMove pre = max (date (main pre)) (date (lost pre)) 
-- | Returns the qoh at the date of the stocktake.
-- return also the date of the last move (the one corresponding to given quantity).
-- Needed to detect if the stocktake is sure or not.
qohFor :: (Single Text, Single Int,  Single Day) -> Handler (Maybe PreAdjust)
qohFor r@(Single sku, Single qtake, Single date) = do
  adj <-  PreAdjust
         <$> pure sku
         <*> quantitiesFor "DEF" r
         <*> quantitiesFor "LOST" r
         <*> pure date
  return $ if all noInfo (adjustInfos adj)
              then Nothing
              else Just adj

-- | Retrive the quantities available for the given location at the given date
quantitiesFor :: Text -> (Single Text, Single Int, Single Day) -> Handler LocationInfo 
quantitiesFor loc (Single sku, Single take, Single date) = do
  let (minDate, maxDate) = unsureRange date
      sql = "SELECT SUM(qty) qoh"
            <> ", SUM(qty) qoh_at"
            <> ", MAX(tran_date)  "
            <> "FROM 0_stock_moves "
            <> "WHERE stock_id = ? "
            <> "AND tran_date <= ? "
            <> "AND loc_code = ?"

  -- extract all relevant moves within the unsure range
  let sqlForMoves = "SELECT moves.tran_date, COALESCE(debtor.name, '<>'), COALESCE(GROUP_CONCAT(operator.name), 'nop'), SUM(moves.qty) "
                   <> " FROM 0_stock_moves moves "
                   <> " LEFT JOIN 0_debtor_trans USING(trans_no, type)"
                   <> " LEFT JOIN 0_debtors_master debtor USING(debtor_no)"
                   <> " LEFT JOIN mop.action ON (orderId = order_ AND sku = stock_id AND typeId = 1)"
                   <> " LEFT JOIN mop.session ON (groupId = actionGroupId)"
                   <> " LEFT JOIN mop.operator ON (operatorId = operator.id)"
                   <> " WHERE stock_id = ?"
                   <> " AND moves.tran_date between ? AND ?"
                   <> " AND loc_code = ?"
                   <> " GROUP BY trans_no, type"
                   <> " ORDER BY moves.tran_date, moves.trans_id " :: Text
      toMove (Single date, Single debtor, Single operators, Single qty) = MoveInfo date debtor operators qty
                    
  (results, moves) <- runDB $ do
    results <- rawSql sql [PersistText sku, PersistDay maxDate, PersistText loc]
    moves <- rawSql sqlForMoves [PersistText sku, PersistDay minDate, PersistDay maxDate, PersistText loc]
    return (results , moves)


  traceShowM(sqlForMoves, moves)
  return $ case results of
    [] -> LocationInfo loc Nothing 0 0 Nothing []
    [(Single qoh, Single at, Single last)] -> LocationInfo loc (Just take) (fromMaybe 0 at) (fromMaybe 0 qoh) (last) (map toMove moves)


-- | Computes the date range when a stock take can be unsure or not.
-- A stocktake is unsure, if an item has been picked at the day as the stock take.
-- In that case, we can't guess if the stock take has been done before or after the picking.
-- In other word, does the stocktake take the picking into account or not ?
-- Things get ever more complicated as items can be picked a day but only processed in the system
-- the next working day. An item can have picked before a stocktake but only be seens as picked
-- on the day after. In that case, the stocktake , take the picked quantity into account even though
-- the stock moves only occurs the day after.
-- In reverse, a item can be picked the day before but only processed on the stocktake day.
-- In that case, we shouldn't take the move into account.
-- UnsureRange, computes the previous and next working day.
unsureRange :: Day -> (Day, Day)
unsureRange day = (previousWorkingDay day, nextWorkingDay day)

nextWorkingDay :: Day -> Day
nextWorkingDay day = headEx . filter (isWorkingDay) . drop 1 $  List.iterate (addDays 1) day
  
previousWorkingDay :: Day -> Day
previousWorkingDay day = headEx . filter (isWorkingDay)  . drop 1 $ List.iterate (addDays (-1)) day
  
isWorkingDay :: Day -> Bool
isWorkingDay day = let (_, _, weekDay) = toWeekDate day
  in weekDay <= 5



computeAdj :: StockAdjustmentId -> PreAdjust -> [StockAdjustmentDetail]
computeAdj key pre =
  let mainTake = quantityTake0 (main pre)
      mainAt   = quantityAt  (main pre)
      mainNow   = quantityNow  (main pre)
      lostTake = quantityTake0 (lost pre)
      lostAt   = quantityAt  (lost pre)
      lostNow   = quantityNow  (lost pre)

      lostLoc = Just . FA.LocationKey . location . lost $ pre 
      mainLoc = Just . FA.LocationKey . location . main $ pre 
      stock_id = sku pre

      adjs = if mainTake > mainAt
              then -- found
                let found = mainTake - mainAt
                    fromLost = min lostNow found
                    new = max 0 (found - fromLost)

                in [ StockAdjustmentDetail key stock_id fromLost lostLoc mainLoc  -- found
                    , StockAdjustmentDetail key stock_id new Nothing mainLoc
                    ]
              else
               let lostQuantity = mainAt - mainTake
                   -- we don't want stock adjustment to result in negative stock
                   toAdjust = min lostQuantity mainNow
                   
               in  [ StockAdjustmentDetail key stock_id toAdjust mainLoc lostLoc ]
  in filter ((/= 0) . stockAdjustmentDetailQuantity) adjs




saveStockAdj :: Maybe Text -> [PreAdjust] -> Handler (Widget)
saveStockAdj comment pres = do
  now  <- liftIO getCurrentTime
  userId <- requireAuthId
  runDB $ do
    let adj = StockAdjustment (fromMaybe "" comment) now Pending userId
    adjKey <- insert adj

    let adjs = concatMap (computeAdj adjKey) pres
    insertMany_ adjs

    -- update stock take
    mapM_ (\pre -> updateWhere [ StocktakeStockId ==. (sku pre)
                               , StocktakeAdjustment ==. Nothing
                               ]
                               [StocktakeAdjustment =. Just adjKey])
                               pres
    setSuccess "Stock adjustment saved"
    return ""

-- | Displays a list of pending adjustment
renderPending :: Handler Widget
renderPending = do
  pendings <- runDB $ selectList [StockAdjustmentStatus ==. Pending] [] -- [LimitTo 10]
  return [whamlet|
<table.table.table-hover>
  <tr>
    <th> Id
    <th> Date
    <th> Comment
    <th> Statement
  $forall (Entity k adj) <- pendings
    <tr>
      <td> <a href=@{WarehouseR (WHStockAdjustmentViewR (unSqlBackendKey $ unStockAdjustmentKey k) )}>
       ##{tshow $ unSqlBackendKey $ unStockAdjustmentKey k}
      <td> #{tshow $ stockAdjustmentDate adj}
      <td> #{take 50 $ stockAdjustmentComment adj}
      <td> #{tshow $ stockAdjustmentStatus adj}
|]


getWHStockAdjustmentViewR :: Int64 -> Handler Html
getWHStockAdjustmentViewR key = do
  let adjKey  = (SqlBackendKey key)
      mainLoc = Just (FA.LocationKey "DEF")
      lostLoc = Just (FA.LocationKey "LOST")

  
  (details, adj) <- runDB $ do
    adj <- get (StockAdjustmentKey adjKey)

    entities <- selectList [StockAdjustmentDetailAdjustment ==. StockAdjustmentKey adjKey] []
    return (map entityVal entities, adj)
  
  let lost = filter ((== mainLoc) . stockAdjustmentDetailFrom ) details
  let found = filter ((== lostLoc) . stockAdjustmentDetailFrom ) details
  let new = filter ((== Nothing) . stockAdjustmentDetailFrom ) details

  let renderDetails :: Text -> Text -> [StockAdjustmentDetail] -> Widget
      renderDetails title class_ details = [whamlet|
<div.panel. class="panel-#{class_}">
  <div.panel-heading>
    <h3> #{title}
  <div.panel-body>
    <p.well>
      $forall d <- details
       #{stockAdjustmentDetailStockId d} #{stockAdjustmentDetailQuantity d} <br>
                          |]

  defaultLayout $ do
    [whamlet|
$maybe a <- adj                                                  
  <div>
    #{tshow $ stockAdjustmentDate a}
    #{tshow $ stockAdjustmentStatus a}
    <div.well> #{stockAdjustmentComment a}
            |]
    renderDetails "Found" "success"  found
    renderDetails "Lost" "danger"  lost
    renderDetails "New" "warning"  new
  

badgeSpan :: Int -> Maybe String -> String -> Widget
badgeSpan qty bgM klass = do
  let style = case badgeWidth qty of
        Nothing -> "display:none"
        Just w ->  "width:" ++ show w ++ "em"
      bg = case bgM of
             Nothing -> ""
             Just col ->  "background-color:"++col++";"
  [whamlet|<span.badge class=#{klass} style="#{style}; #{bg}">#{qty}|]
