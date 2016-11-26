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

data Unsure = Sure | Unsure | All deriving (Eq, Read, Show, Enum, Bounded)


data CartMode = Lost | Found | New deriving (Eq, Read, Show, Enum)

-- If a stocktake represent a full style, all variations not present
-- should be lost .
data StyleMode = LooseMissing | Partial deriving (Eq, Read, Show, Enum)

data FormParam = FormParam
  { style :: Maybe Text
  , unsure :: Unsure
  } deriving (Eq, Read, Show)


paramForm = renderBootstrap3 BootstrapBasicForm  form
  where form = FormParam
            <$> aopt textField "style" Nothing
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
  ((resp, view), enctype) <- runFormPost paramForm
  case resp of
    FormMissing -> error "Form missing"
    FormFailure a -> defaultLayout [whamlet|^{view}|]
    FormSuccess param -> do
      let sql = "SELECT stock_id, SUM(quantity), MAX(date) \
               \ FROM fames_stocktake \
               \ WHERE active =1 \
               \ GROUP BY stock_id \
               \ LIMIT 5000 "

      stocktakes <- runDB $ rawSql sql []

      -- use conduit ? TODO
      xs <- mapM qohFor stocktakes
      let classFor qty qoh = case compare qty qoh of
            GT -> "success"
            EQ -> ""
            LT -> "danger" :: Text
      let response = [whamlet|
<div>
  <table.table.table-border.table-hover>
    <tr>
      <th> Style
      <th> Stocktake
      <th> Date
      <th> Quantity on Hand
      <th> Last move
    $forall (st, qty, stDate, qoh, lastMove) <- xs
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


-- | Returns the qoh at the date of the stocktake.
-- return also the date of the last move (the one corresponding to given quantity).
-- Needed to detect if the stocktake is sure or not.

qohFor :: (Single Text, Single Int,  Single Day) -> Handler (Text, Int, Day, Int, Day)
qohFor r@(Single stockId, Single qty, Single stDate) = do
  print r

  let sql = "SELECT SUM(qty), MAX(tran_date)\
            \ FROM 0_stock_moves \
            \ WHERE stock_id =? \
            \ AND tran_date <= ? \
            \ AND loc_code = 'DEF'"

  result <- runDB $ rawSql sql [ PersistText stockId
                                , PersistDay stDate
                                ]
  case result of 
    [(Single qoh, Single last)] -> return (stockId, qty, stDate
                                          , fromMaybe 0 qoh , fromMaybe stDate last)
    [] -> return (stockId, qty, stDate , 0 , stDate)
    other -> error (show other)


  
  




