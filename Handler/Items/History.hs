{-# LANGUAGE OverloadedStrings #-}
module Handler.Items.History where

import Import
import Handler.Table
import Yesod.Form.Bootstrap3
import qualified FA as FA
import Database.Persist.MySQL(unSqlBackendKey)
import Data.List (mapAccumL)
import Text.Blaze.Html (ToMarkup)

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
  columns = ["Type", "#", "Reference", "Location", "Date", "In", "Quantity On Hand", "Out", "Operator"]
  -- columns = ["Type", "#", "Reference", "Location", "Date", "InOut", "Operator"]
  colDisplay col = (toHtml col, [])
  rows = [ (valueFor event, [])
         | event <- events
         ]
  in displayTable columns colDisplay rows
  
  

valueFor :: ItemEvent -> Text -> Maybe (Html, [Text])
valueFor ie "InOut" = let
  fromM = fromMaybe ("", [])
  (in_, _) = fromM (valueFor ie "In")
  (out, _) = fromM (valueFor ie "Out")
  (qoh, k) = fromM (valueFor ie "Quantity On Hand")
  in Just (in_ >> qoh >> out, k)
valueFor (ItemEvent opM (Left (FA.StockMove{..})) qoh)  col = case col of
  "Type" -> Just (showTransType $ toEnum stockMoveType, [])
  "#" -> Just (toHtml (tshow stockMoveTransNo), [])
  "Reference" -> Just (toHtml (stockMoveReference), [])
  "Location" -> Just (toHtml (stockMoveLocCode), [])
  "Date" -> Just (toHtml (tshow stockMoveTranDate), [])
  "In" -> Just (badgeSpan' inBadge stockMoveQty "", [])
  "Out" -> Just (badgeSpan' outBadge (-stockMoveQty) "", [])
  "Quantity On Hand" -> Just ((badgeSpan' qohBadge (qoh - (max 0 stockMoveQty))  ""
                              >> (badgeSpan' negBadge ((max 0 stockMoveQty) - qoh)  "")
                              ), [])
  "Operator" -> Just ("Need operator", ["bg-danger"])

valueFor (ItemEvent opM (Right (Entity key (Stocktake{..}))) qoh) col = let
  diff = qoh - fromIntegral stocktakeQuantity
  lost = max 0 diff
  found = max 0 (-diff)
  in case col of
  "Type" -> Just ("Stocktake", [])
  "#" -> Just (toHtml . unSqlBackendKey . unStocktakeKey $ key, [])
  "Reference" -> Just (toHtml (stocktakeBarcode), [])
  "Location" -> Just (toHtml . FA.unLocationKey $ stocktakeFaLocation, [])
  "Date" -> Just (toHtml (tshow $ stocktakeDate), [])
  "In" -> Just (badgeSpan' inBadge found "", [])
  "Out" -> Just (badgeSpan' outBadge lost "", [])
  "Quantity On Hand" -> Just (badgeSpan' qohBadge (fromIntegral stocktakeQuantity) "", [])
  "Operator" -> Just ("Need operator", ["bg-danger"])


-- badgeSpan' :: (Num a, Ord a, ToMarkup a)  => a -> Maybe String -> String -> Html
badgeSpan' :: Maybe String -> Double -> String -> Html
badgeSpan' bgM qty klass =
  badgeSpan badgeWidth qty bgM klass

qohBadge = Just "#29abe0"
inBadge = Nothing
outBadge = Just "#d9534f"
negBadge = Just "#000000"



badgeWidth q | q <= 0 = Nothing
             | otherwise = Just . (max 2) . floor $ (min (q / 6) 24) 

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



loadMoves sku = selectList [FA.StockMoveStockId ==. sku, FA.StockMoveLocCode ==. "DEF"] [Asc FA.StockMoveTranDate, Asc FA.StockMoveId]
loadTakes sku = selectList [StocktakeStockId ==. sku] [Asc StocktakeDate]



  
