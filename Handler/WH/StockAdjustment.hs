{-# LANGUAGE OverloadedStrings #-}
module Handler.WH.StockAdjustment where

import Import
import Yesod.Form.Bootstrap3
import Database.Persist.MySQL

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
  } deriving (Eq, Read, Show)


paramForm = renderBootstrap3 BootstrapBasicForm  form
  where form = FormParam
            <$> aopt textField "style" Nothing
            <*> areq boolField "download" (Just False)
            <*> aopt intField "min quantity" (Just (Just 1))
            <*> aopt intField "max quantity" Nothing
            <*> areq (selectField optionsEnum)"sort by" Nothing
            <*> areq (selectField optionsEnum) "mode" (Just All)



getWHStockAdjustmentR :: Handler Html
getWHStockAdjustmentR = do
  renderStockAdjustment



renderStockAdjustment :: Handler Html
renderStockAdjustment = do
  (paramForm, encType) <- generateFormPost paramForm
  let response = [whamlet|
<form.well #stock-adjustement role=form method=post action=@{WarehouseR WHStockAdjustmentR} enctype=#{encType}>
  ^{paramForm}
  <button type="submit" .btn.btn-primary>Submit
|]

  defaultLayout response


postWHStockAdjustmentR :: Handler Html
postWHStockAdjustmentR = do
  ((resp, view), encType) <- runFormPost paramForm
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
               \                           FROM fames_stocktake WHERE " <> stockTakenStyle <> ") "
               <> w <> "\
               \ GROUP BY stock_id "

      stocktakes <- runDB $ rawSql sql p
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
      let response = [whamlet|
<form #stock-adjustement role=form method=post action=@{WarehouseR WHStockAdjustmentR} enctype=#{encType}>
  ^{view}
  <button type="submit" .btn.btn-primary>Submit
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
          <td>#{sku pre}
          <td>#{qty}
            $if qoh > qty
              <span.badge style="#{min (succ qoh - qty) 9}em; background-color:#d9534f">#{qoh - qty}
          <td>#{tshow $ date (main pre)}
          <td>#{qoh}
            $if qty > qoh
              $with (fromLost, new) <- split qty qoh lostq
                $if fromLost > 0
                  <span.badge style="width:#{min (succ fromLost) 9}em; background-color:#29abe0">#{fromLost}
                $if new > 0
                  <span.badge style="width:#{min (succ new) 9}em;">#{new}

          <td>#{quantityNow (lost pre)}
          <td>#{tshow $ date $ main pre}
|]
      defaultLayout response


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

  print sql
  results <- runDB $ rawSql sql (params <> [PersistText sku, PersistText loc])

  return $ case results of
    [] -> LocationInfo loc Nothing 0 0 today
    [(Single qoh, Single at, Single last)] -> LocationInfo loc (Just take) (fromMaybe 0 at) (fromMaybe 0 qoh) (fromMaybe today last)


  
  




