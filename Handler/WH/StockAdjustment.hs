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
<form #stock-adjustement role=form method=post action=@{WarehouseR WHStockAdjustmentR} enctype=#{encType}>
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
      let withDiff = [(abs (qoh - qty), row) | row@(st, qty, stDate, qoh, lastMove) <- results]
          f  (q,(st, qty, stDate, qoh, lastMove)) = (maybe True (q >=) (minQty param))
                   &&  (maybe True (q <=) (maxQty param))
                   && case unsure param of
                        All -> True
                        Unsure -> stDate == lastMove
                        Sure -> stDate /= lastMove

          filtered = filter f withDiff

          rows = map snd $ case sortMode param of
            SortByStyle -> filtered
            SortByQuantity -> sortBy (comparing (\(q,(st,_,_,_,_)) -> (Down q, st) ))  filtered
          

      -- use conduit ? TODO
      let classFor qty qoh = case compare qty qoh of
            GT -> "success"
            EQ -> ""
            LT -> "danger" :: Text
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
      <th> Last move
    $forall (st, qty, stDate, qoh, lastMove) <- rows
      <tr class="#{classFor qty qoh}">
        <td>#{st}
        <td>#{qty}
          $if qoh > qty
            <span.badge style="#{min (succ qoh - qty) 9}em; background-color:#d9534f">#{qoh - qty}
        <td>#{tshow $ stDate}
        <td>#{qoh}
          $if qty > qoh
            <span.badge style="width:#{min (succ qty - qoh) 9}em;">#{qty - qoh}
        <td>#{tshow $ lastMove}
|]
      defaultLayout response


-- | Temporary data holding stock adjustment information (to display)
data LocationInfo = LocationInfo
  { location :: !Text
  , quantityTake :: !Int -- quantity from stock take 
  , qoh :: !Int -- quantity on hand at date
  , now :: !Int -- quantity on hand NOW. To avoid negative stock
  , date :: !Day
  } deriving (Eq, Read, Show)

data PreAdjust = PreAdjust
  { sku :: !Text
  , main :: !LocationInfo
  , lost :: !LocationInfo
  } deriving (Eq, Read, Show)

-- | Returns the qoh at the date of the stocktake.
-- return also the date of the last move (the one corresponding to given quantity).
-- Needed to detect if the stocktake is sure or not.

qohFor :: (Single Text, Single Int,  Single (Maybe Day)) -> Handler (Maybe (Text, Int, Day, Int, Day))
qohFor r@(Single stockId, Single qty, Single stDateM) = do

  let (w,p) = case stDateM of
        Just stDate -> ("  AND tran_date <= ? ", [PersistDay stDate])
        Nothing -> ("", [])
  let sql = "SELECT SUM(qty), MAX(tran_date)\
            \ FROM 0_stock_moves \
            \ WHERE stock_id =? " <> w <> "\
            \ AND loc_code = 'DEF'"

  result <- runDB $ rawSql sql ([ PersistText stockId] <> p)
  
  case (result, stDateM) of 
    ([(Single qoh, Single last)], Just stDate) -> return $ Just (stockId, qty, stDate
                                          , fromMaybe 0 qoh , fromMaybe stDate last)
    -- no stock take but qoh
    ([(Single (Just 0), _)], Nothing) -> return Nothing 
    ([(Single qoh, Single (Just last))], Nothing) -> return $ Just (stockId, qty, last
                                          , fromMaybe 0 qoh , last)
    ([], Just stDate) -> return . Just $ (stockId, qty, stDate , 0 , stDate)
    -- shouldn't be needed
    ([], Nothing) -> return Nothing 
    ([(Single Nothing, _)], Nothing) -> return Nothing 
    other -> error (show other)



  
  




