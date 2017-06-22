{-# LANGUAGE OverloadedStrings #-}
module Handler.Items.History where

import Import
import Handler.Table
import Yesod.Form.Bootstrap3
import qualified FA as FA
import Database.Persist.MySQL(unSqlBackendKey)
import Data.List (mapAccumL)

data FormParams = FormParams
  { pStart :: Maybe Day
  , pEnd :: Maybe Day
  , pLocation :: Maybe Text
  }

getItemsHistoryR :: Text -> Handler Html
getItemsHistoryR sku = do
  history <- loadHistory sku
  let tableW = historyToTable history
  defaultLayout tableW


-- data ItemEvent = ItemEvent deriving Show
data ItemEvent = ItemEvent
 { ieOperator :: Maybe Operator
 , ieEvent :: Either FA.StockMove (Entity Stocktake)
 , ieQoh :: Double
 }

 
loadHistory :: Text -> Handler [ItemEvent]
loadHistory sku = do
  (moves, takes) <- runDB $ liftA2 (,) (loadMoves sku) (loadTakes sku)
  return $ makeEvents moves takes


historyToTable :: [ItemEvent] -> Widget
historyToTable events = let
  columns = ["Type", "#", "Reference", "Location", "Date", "In", "Out", "Quantity On Hand", "Operator"]
  colDisplay col = (toHtml col, [])
  rows = [ (valueFor event, [])
         | event <- events
         ]
  in displayTable columns colDisplay rows
  
  

valueFor :: ItemEvent -> Text -> Maybe (Html, [Text])
valueFor (ItemEvent opM (Left (FA.StockMove{..})) qoh)  col = case col of
  "Type" -> Just ("not implemented", ["bg-danger"]) -- _xxx (stockMoveType)
  "#" -> Just (toHtml (tshow stockMoveTransNo), [])
  "Reference" -> Just (toHtml (stockMoveReference), [])
  "Location" -> Just (toHtml (stockMoveLocCode), [])
  "Date" -> Just (toHtml (tshow stockMoveTranDate), [])
  "In" -> if stockMoveQty > 0 then Just (toHtml $ tshow stockMoveQty, []) else Nothing
  "Out" -> if stockMoveQty < 0 then Just (toHtml $ tshow (-stockMoveQty), []) else Nothing
  "Quantity On Hand" -> Just (toHtml qoh, [])
  "Operator" -> Just ("Need operator", ["bg-danger"])

valueFor (ItemEvent opM (Right (Entity key (Stocktake{..}))) qoh) col = let
  diff = qoh - fromIntegral stocktakeQuantity
  found = max 0 diff
  lost = max 0 (-diff)
  in case col of
  "Type" -> Just ("Stocktake", [])
  "#" -> Just (toHtml . unSqlBackendKey . unStocktakeKey $ key, [])
  "Reference" -> Just (toHtml (stocktakeBarcode), [])
  "Location" -> Just (toHtml . FA.unLocationKey $ stocktakeFaLocation, [])
  "Date" -> Just (toHtml (tshow $ stocktakeDate), [])
  "In" -> if found > 0 then Just (toHtml found, []) else Nothing
  "Out" -> if lost > 0 then Just (toHtml lost, []) else Nothing
  "Quantity On Hand" -> Just (toHtml stocktakeQuantity, []) 
  "Operator" -> Just ("Need operator", ["bg-danger"])



makeEvents moves takes = let
  moves' = [ ((FA.stockMoveTranDate move, fromIntegral $ FA.unStockMoveKey key), Left move)
           | (Entity key move) <- moves
           ]
  takes' = [ ((stocktakeDate $ entityVal take , unSqlBackendKey . unStocktakeKey $ entityKey take), Right take)
           | take <- takes
           ]
  lines =  map snd $ sortBy (comparing fst) (moves' ++ takes')
  events = snd $  mapAccumL accumEvent 0 lines 
  accumEvent qoh e@(Left move) = (newQoh, ItemEvent Nothing e newQoh) where
    newQoh = qoh + FA.stockMoveQty move
  accumEvent qoh e@(Right take ) = (qoh, ItemEvent Nothing e qoh) where
    newQoh = fromIntegral .  stocktakeQuantity . entityVal $ take
    
  in events



loadMoves sku = selectList [FA.StockMoveStockId ==. sku] [Asc FA.StockMoveTranDate, Asc FA.StockMoveId]
loadTakes sku = selectList [StocktakeStockId ==. sku] [Asc StocktakeDate]



  
