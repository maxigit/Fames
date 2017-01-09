{-# LANGUAGE OverloadedStrings #-}
module Handler.WH.StockAdjustment where

import Import
import Yesod.Form.Bootstrap3
import Database.Persist.MySQL

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
      -- only get the styles which one of the variations has been stock taken.
      let stockTakenStyle = " (active = 1 OR active IS NULL AND stock_adj_id IS NULL) "
      let sql = "SELECT stock_id, COALESCE(SUM(quantity),0), MAX(date) \
               \ FROM 0_stock_master \
               \ LEFT JOIN fames_stocktake USING (stock_id) \
               \ WHERE " <> stockTakenStyle <>
               " AND LEFT(stock_id,8) IN ( SELECT DISTINCT LEFT(stock_id, 8)  \
               \                           FROM fames_stocktake WHERE " <> stockTakenStyle <> ") \
               \ AND stock_adj_id IS NULL "
               <> w <> "\
               \ GROUP BY stock_id "

      stocktakes <- runDB $ rawSql sql p
      traceShowM sql
      traceShowM stocktakes
      results <- catMaybes <$> mapM qohFor stocktakes
      let withDiff = [(abs (quantityTake0 (main pre) - quantityAt (main pre)), pre) |  pre <- results]
          f  (q, pre) = (maybe True (q >=) (minQty param))
                   &&  (maybe True (q <=) (maxQty param))
                   && let isUnsure = date (main pre) == date (lost pre)
                      in case unsure param of
                        All -> True
                        Unsure -> isUnsure
                        Sure -> not isUnsure

          filtered = filter f withDiff

          rows = map snd $ case sortMode param of
            SortByStyle -> filtered
            SortByQuantity -> sortBy (comparing (\(q,pre) -> (Down q, sku pre ) ))  filtered
          

      -- use conduit ? TODO
      let classFor qty qoh = case compare qty qoh of
            GT -> "success"
            EQ -> ""
            LT -> "danger" :: Text
          split qty qoh lost = let new = qty -qoh
                                   fromLost = min lost (qty -qoh)
                               in (fromLost, new -fromLost) :: (Int, Int)
      
      response <- case mode of
            Save -> saveStockAdj (comment param) rows
            View -> return [whamlet|
<div>
  <table.table.table-border.table-hover>
    <tr>
      <th> Style
      <th> Stocktake
      <th> Date
      <th> QOH FA
      <th> lost
      <th> Last move
    $forall pre <- rows
      $with (qty, qoh, lostq) <- (quantityTake0 (main pre), quantityAt (main pre), quantityNow (lost pre))
        <tr class="#{classFor qty qoh}">
          <td.style>#{sku pre}
          <td.quantity>#{qty}
            $if qoh > qty
              <span.badge style="width:#{min (succ qoh - qty) 9}em; background-color:#d9534f">#{qoh - qty}
          <td.date>#{tshow $ date (main pre)}
          <td.qoh>#{qoh}
            $if qty > qoh
              $with (fromLost, new) <- split qty qoh lostq
                $if fromLost > 0
                  <span.badge style="width:#{min (succ fromLost) 9}em; background-color:#29abe0">#{fromLost}
                $if new > 0
                  <span.badge style="width:#{min (succ new) 9}em;">#{new}

          <td.lost>#{quantityNow (lost pre)}
          <td.last_move>#{tshow $ date $ main pre}
|]
      defaultLayout [whamlet|
<form.well #stock-adjustement role=form method=post action=@{WarehouseR WHStockAdjustmentR} enctype=#{encType}>
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
  , date :: !Day
  } deriving (Eq, Read, Show)

quantityTake0 = fromMaybe 0 . quantityTake

noInfo locInfo = all (==0) [quantityTake0 locInfo, quantityAt locInfo ]

data PreAdjust = PreAdjust
  { sku :: !Text
  , main :: !LocationInfo
  , lost :: !LocationInfo
  } deriving (Eq, Read, Show)

adjustInfos :: PreAdjust -> [LocationInfo]
adjustInfos adj  = [main, lost] <*> [adj]
-- | Returns the qoh at the date of the stocktake.
-- return also the date of the last move (the one corresponding to given quantity).
-- Needed to detect if the stocktake is sure or not.
qohFor :: (Single Text, Single Int,  Single (Maybe Day)) -> Handler (Maybe PreAdjust)
qohFor r@(Single sku, Single qtake, Single dateM) = do
  adj <-  PreAdjust
         <$> pure sku
         <*> quantitiesFor "DEF" r
         <*> quantitiesFor "LOST" r
  return $ if all noInfo (adjustInfos adj)
              then Nothing
              else Just adj

-- | Retrive the quantities available for the given location at the given date if provided.
quantitiesFor :: Text -> (Single Text, Single Int, Single (Maybe Day)) -> Handler LocationInfo 
quantitiesFor loc (Single sku, Single take, Single dateM) = do
  today <- utctDay <$> liftIO getCurrentTime
  let (old, params) = case dateM of
        Nothing -> ("TRUE" , [])
        Just date -> ("tran_date<= ?", [PersistDay date, PersistDay date])
  let sql = "SELECT SUM(qty) qoh\
            \, SUM( IF("<>old<>",qty,0) ) qoh_at\
            \, MAX( IF("<>old<>",tran_date,NULL) ) \
            \FROM 0_stock_moves \
            \WHERE stock_id = ? \
            \AND loc_code = ?"

  results <- runDB $ rawSql sql (params <> [PersistText sku, PersistText loc])

  return $ case results of
    [] -> LocationInfo loc Nothing 0 0 today
    [(Single qoh, Single at, Single last)] -> LocationInfo loc (Just take) (fromMaybe 0 at) (fromMaybe 0 qoh) (fromMaybe today last)


  
  



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
  
