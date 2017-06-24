{-# LANGUAGE OverloadedStrings #-}
module Handler.Items.History where

import Import
import Handler.Table
import Yesod.Form.Bootstrap3
import qualified FA as FA
import Database.Persist.MySQL(unSqlBackendKey)
import Data.List (mapAccumL)
import Text.Blaze.Html (ToMarkup)
import qualified Data.Map as Map
import Data.Time(addDays)

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
 , ieEvent :: Either FA.StockMove Adjustment
 , ieQoh :: Double
 , ieStocktake :: Maybe Double
 , ieMod :: Double -- ^ quantity added to topup partial stock take 
 }

-- | Local type which contains a group of stocktake
-- and the corresponding adjustment detail if any
data Adjustment = Adjustment
  { aAdj :: [StockAdjustmentDetail]
  , aTakes :: [Entity Stocktake]
  }
 
loadHistory :: Text -> Handler [ItemEvent]
loadHistory sku = do
  (moves, takes) <- runDB $ liftA2 (,) (loadMoves sku) (loadTakes sku)
  return $ makeEvents moves takes


historyToTable :: [ItemEvent] -> Widget
historyToTable events = let
  columns = ["Type", "#", "Reference", "Location", "Date", "InOut", "Quantity On Hand", "Stocktake", "Operator"]
  -- columns = ["Type", "#", "Reference", "Location", "Date", "InOut", "Operator"]
  colDisplay col = (toHtml col, [])
  rows = [ (valueFor event, [])
         | event <- events
         ]
  in displayTable columns colDisplay rows
  
  

valueFor :: ItemEvent -> Text -> Maybe (Html, [Text])
valueFor ie "InOutQ" = let
  fromM = fromMaybe ("", [])
  (in_, _) = fromM (valueFor ie "In")
  (out, _) = fromM (valueFor ie "Out")
  (qoh, k) = fromM (valueFor ie "Quantity On Hand")
  in Just (in_ >> qoh >> out, k)
valueFor ie "InOut" = let
  fromM = fromMaybe ("", [])
  (in_, k) = fromM (valueFor ie "In")
  (out, _) = fromM (valueFor ie "Out")
  in Just (in_ >> out, k)
valueFor (ItemEvent opM _ qoh stake mod) "Quantity On Hand" =
  let found = (fromMaybe 0 stake + mod - qoh)
  in Just (toHtml (formatQuantity qoh)
           >> badgeSpan' modBadge (- mod) ""
           >> badgeSpan' inBadge found "", [])
valueFor (ItemEvent opM ie qoh (Just stake) mod) "Stocktake" =
  let lost = (qoh - stake - mod)
      -- on a stocktake columen display a colour either or good or bad if the stocktake is correct
      stake' = case ie of
        Left _ -> toHtml (formatQuantity stake)
        Right _ -> badgeSpan' (if lost == 0 then qohBadge else negBadge) stake ""
     
  in Just (stake' >> badgeSpan' modBadge mod "" >>  badgeSpan' outBadge lost "", [])
valueFor (ItemEvent _ _ _ Nothing _) "Stocktake" = Nothing
valueFor (ItemEvent opM (Left (FA.StockMove{..})) qoh stake _)  col = case col of
  "Type" -> Just (showTransType $ toEnum stockMoveType, [])
  "#" -> Just (toHtml (tshow stockMoveTransNo), [])
  "Reference" -> Just (toHtml (stockMoveReference), [])
  "Location" -> Just (toHtml (stockMoveLocCode), [])
  "Date" -> Just (toHtml (tshow stockMoveTranDate), [])
  "In" -> Just (badgeSpan' inBadge stockMoveQty "", [])
  "Out" -> Just (badgeSpan' outBadge (-stockMoveQty) "", [])
  "Operator" -> Just ("Need operator", ["bg-danger"])

valueFor (ItemEvent opM (Right adj ) qoh stake _) col = let
  Stocktake{..} = entityVal . headEx $ aTakes adj
  diff = 0 -- qoh - fromIntegral stocktakeQuantity
  lost = max 0 diff
  found = max 0 (-diff)
  in case col of
  "Type" -> Just ("Stocktake", [])
  "#" -> (\a -> (toHtml . unSqlBackendKey . unStockAdjustmentKey $ stockAdjustmentDetailAdjustment a, [])) <$> headMay (aAdj adj)
  "Reference" -> Just (toHtml (stocktakeBarcode), [])
  "Location" -> Just (toHtml . FA.unLocationKey $ stocktakeFaLocation, [])
  "Date" -> Just (toHtml (tshow $ stocktakeDate), [])
  "In" -> Just (badgeSpan' inBadge found "", [])
  "Out" -> Just (badgeSpan' outBadge lost "", [])
  "Operator" -> Just ("Need operator", ["bg-danger"])


-- badgeSpan' :: (Num a, Ord a, ToMarkup a)  => a -> Maybe String -> String -> Html
badgeSpan' :: Maybe String -> Double -> String -> Html
badgeSpan' bgM qty klass =
  badgeSpan badgeWidth qty bgM klass

qohBadge = Just "#29abe0"
inBadge = Nothing
outBadge = Just "#d9534f"
negBadge = Just "#000000"
modBadge = Just "#cccccc"



badgeWidth q | q <= 0 = Nothing
             | otherwise = Just . (max 2) . floor $ (min q  12) 

makeEvents moves takes = let
  moves' = [ ((FA.stockMoveTranDate move, 0, fromIntegral $ FA.unStockMoveKey key), Left move)
           | (Entity key move) <- moves
           ]
  takes' = [ ((date, 1, (undefined . stocktakeAdjustment  . entityVal $ headEx ts)), Right a)
           | a@(Adjustment adj ts) <- takes
           , let date = maximumEx $ map (stocktakeDate . entityVal) ts
           ]
  lines =  map snd $ sortBy (comparing fst) (moves' ++ takes')
  events = snd $  mapAccumL accumEvent (0, Nothing) lines 
  accumEvent (qoh, stake) e@(Left move) = ((newQoh, newTake), ItemEvent Nothing e newQoh newTake 0) where
    newQoh = qoh + FA.stockMoveQty move
    -- We update accordinglinyg the expected stocktake 
    -- However, if the new qoh matches the (old) expected stocktake
    -- There is no point to carry on tracking the difference and we reset it.
    newTake = if stake == Just newQoh
              then Nothing
              else (+FA.stockMoveQty move) <$> stake
  accumEvent (qoh, _) e@(Right takes ) =
    ((qoh, (+mod) <$> newTake), ItemEvent Nothing e qoh newTake mod) where
    newTake0 = fromIntegral . sum $ map (stocktakeQuantity . entityVal) (aTakes takes)
    newTake = Just newTake0
    mod = case aAdj takes of
            [] -> 0
            details -> let
                  lost = sum  [ (if stockAdjustmentDetailFrom adj == Just (FA.LocationKey "DEF")
                                 then 1
                                 else -1) * stockAdjustmentDetailQuantity adj
                              | adj <- details
                              ]


                  -- lost = qoh - stocktake - mod
                  in qoh - newTake0 - fromIntegral lost
    
  in events



-- loadMoves :: Text -> ReaderT backend m [Entity FA.StockMove]
loadMoves sku = selectList [ FA.StockMoveStockId ==. sku
                           , FA.StockMoveLocCode ==. "DEF"
                           , FA.StockMoveQty !=. 0
                           ]
                           [Asc FA.StockMoveTranDate, Asc FA.StockMoveId]

-- loadTakes :: Text -> ReaderT backend m [Adjustment]
loadTakes sku = do
  takes <- selectList [StocktakeStockId ==. sku] [Asc StocktakeDate]
  -- join manual with the adjustment if any
  details <- selectList [StockAdjustmentDetailStockId ==. sku] [Asc StockAdjustmentDetailId]
  let takesByKey = Map.fromListWith (++) [(stocktakeAdjustment (entityVal take), [take]) | take <- takes]
      detailsByKey = Map.fromListWith (++) [(stockAdjustmentDetailAdjustment d, [d]) | (Entity _ d) <- details]

  return [Adjustment ( concat $ k >>= flip Map.lookup detailsByKey) ts | (k, ts) <- Map.toList takesByKey]





  
