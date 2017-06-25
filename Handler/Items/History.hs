{-# LANGUAGE OverloadedStrings #-}
module Handler.Items.History where

import Import
import Handler.Table
import Yesod.Form.Bootstrap3
import qualified FA as FA
import Database.Persist.MySQL(unSqlBackendKey, rawSql, Single(..))
import Data.List (mapAccumL, cycle)
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
  let tableW = do
        toWidget [cassius|
td.qoh
   text-align: center
tr
  span.total
     display: inline
  div.total
     display: block
  .split
     display: none
tr:hover
  .total
     display: none
  span.split
     display: inline
  div.split
     display: block
                |]
        historyToTable history
  defaultLayout tableW


-- data ItemEvent = ItemEvent deriving Show
data ItemEvent = ItemEvent
 { ieOperator :: Maybe Operator
 , ieEvent :: Either Move Adjustment
 , ieQoh :: Double
 , ieStocktake :: Maybe Double
 , ieMod :: Double -- ^ quantity added to topup partial stock take 
 }

-- | Local type which contains a group of stocktake
-- and the corresponding adjustment detail if any
data Adjustment = Adjustment
  { aAdj :: [StockAdjustmentDetail]
  , aTakes :: [(Entity Stocktake, Text)]
  }

data Move = Move
  { tMove :: FA.StockMove
  , tFrom :: Maybe Text -- ^ From location
  , tInfo :: Text
  , tAdjId :: Maybe Int
  }
 
loadHistory :: Text -> Handler [ItemEvent]
loadHistory sku = do
  (moves, takes) <- runDB $ liftA2 (,) (loadMoves sku) (loadTakes sku)
  return $ makeEvents moves takes


historyToTable :: [ItemEvent] -> Widget
historyToTable events = let
  columns = ["Type", "#", "Reference", "Location", "Date", "Info","InOut", "Quantity On Hand", "Stocktake", "Operator"]
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
valueFor (ItemEvent opM ie qoh stake mod) "Quantity On Hand" =
  let found = (fromMaybe 0 stake + mod - qoh)
  -- if a stock adjustment result in matching the stocktake  : badge
  in case ie of
       (Left move) | (toEnum . FA.stockMoveType $ tMove move) `elem` [ST_LOCTRANSFER, ST_INVADJUST]
                      && found == 0
                      && mod == 0 -> Just (badgeSpan' okBadge qoh "", [])
       _ -> Just ( -- toHtml (formatQuantity qoh)
                  badgeSpan' qohBadge qoh ""
                  >> badgeSpan' modBadge (- mod) ""
                  >> badgeSpan' inBadge found "", ["qoh"])
valueFor (ItemEvent opM ie qoh (Just stake) mod) "Stocktake" =
  let lost = (qoh - stake - mod)
      stake' = case ie of
        Left _ -> if stake == qoh
                  then [shamlet|<span style="color:#29abe0;"> #{formatQuantity stake}|]
                  else toHtml (formatQuantity stake)
        Right adj -> do
          let bg = if lost == 0 then okBadge else negBadge
          -- total
          badgeSpan' bg stake "total"
          -- splits 
          mapM_ (\t -> let badge = badgeSpan' bg (fromIntegral . stocktakeQuantity . entityVal . fst $ t) ""
                       in [shamlet|<div.split>#{badge}|]) (aTakes adj)
  in Just (stake' >> badgeSpan' modBadge mod "" >>  badgeSpan' outBadge lost "", [])
valueFor (ItemEvent _ _ _ Nothing _) "Stocktake" = Nothing
valueFor (ItemEvent opM (Left (Move FA.StockMove{..} _ info _)) qoh stake _)  col = case col of
  "Type" -> Just (showTransType $ toEnum stockMoveType, [])
  "#" -> Just (toHtml (tshow stockMoveTransNo), [])
  "Reference" -> Just (toHtml (stockMoveReference), [])
  "Location" -> Just (toHtml (stockMoveLocCode), [])
  "Date" -> Just (toHtml (tshow stockMoveTranDate), [])
  "In" -> let bg = if toEnum stockMoveType == ST_LOCTRANSFER -- found
                   then okBadge
                   else inBadge
          in Just (badgeSpan' bg stockMoveQty "", [])
  "Out" -> Just (badgeSpan' outBadge (-stockMoveQty) "", [])
  "Operator" -> Just ("Need operator", ["bg-danger"])
  "Info" -> Just (toHtml info, [])

valueFor (ItemEvent opM (Right adj ) qoh stake _) col = let
  inlineAll f = inlineAll' (map (f . entityVal . fst) (aTakes adj))
  inlineAll' values = let
    v'ks = case values of
      [x] -> [(x,"")]
      (x:xs) -> [(x, "" :: Text), ("...", "total")] ++ zip xs (cycle ["split"])
    in mapM_ (\(v, k) -> [shamlet|<div class="#{k}">#{v}|]) v'ks
            
  diff = 0 -- qoh - fromIntegral stocktakeQuantity
  lost = max 0 diff
  found = max 0 (-diff)
  in case col of
  "Type" -> Just ("Stocktake", [])
  "#" -> (\a -> (toHtml . unSqlBackendKey . unStockAdjustmentKey $ stockAdjustmentDetailAdjustment a, [])) <$> headMay (aAdj adj)
  "Reference" -> Just (inlineAll stocktakeBarcode, [])
  "Location" -> Just (inlineAll  (FA.unLocationKey . stocktakeFaLocation), [])
  "Date" -> Just (inlineAll (tshow . stocktakeDate), [])
  "In" -> Just (badgeSpan' inBadge found "", [])
  "Out" -> Just (badgeSpan' outBadge lost "", [])
  "Operator" -> Just (inlineAll' (map snd $ aTakes adj), [])
  "Info" -> case mapMaybe (stocktakeComment . entityVal . fst) (aTakes adj) of
                  [] -> Nothing
                  _ ->  Just (inlineAll (\e -> fromMaybe "" $ decodeHtmlEntities <$> stocktakeComment e), [])



-- badgeSpan' :: (Num a, Ord a, ToMarkup a)  => a -> Maybe String -> String -> Html
badgeSpan' :: Maybe String -> Double -> String -> Html
badgeSpan' bgM qty klass =
  badgeSpan badgeWidth qty bgM klass

okBadge = Just "#29abe0"
qohBadge = Just "#ccccff"
inBadge = Nothing
outBadge = Just "#d9534f"
negBadge = Just "#000000"
modBadge = Just "#cccccc"



badgeWidth q | q <= 0 = Nothing
             | otherwise = Just . (max 2) . floor $ (min q  12) 

makeEvents :: [(Key FA.StockMove, Move)] -> [Adjustment] -> [ItemEvent]
makeEvents moves takes = let
  lines =  interleaveEvents moves takes
  events = snd $  mapAccumL accumEvent (0, Nothing) lines 
  accumEvent (qoh, stake) e@(Left move') = ((newQoh, newTake), ItemEvent Nothing e newQoh newTake 0) where
    move = tMove move'
    newQoh = qoh + FA.stockMoveQty move
    -- We update accordinglinyg the expected stocktake 
    -- However, if the new qoh matches the (old) expected stocktake
    -- There is no point to carry on tracking the difference and we reset it.
    newTake = case ( stake == Just newQoh
                   , toEnum (FA.stockMoveType move)
                     `elem` [ST_CUSTCREDIT, ST_CUSTDELIVERY, ST_SUPPRECEIVE, ST_SUPPCREDIT]
                   ) of
               (True, False ) ->  stake
               (False, False)  | FA.stockMoveQty move < 24 -> stake
               _ -> (+FA.stockMoveQty move) <$> stake
  accumEvent (qoh, _) e@(Right takes ) =
    ((qoh, (+mod) <$> newTake), ItemEvent Nothing e qoh newTake mod) where
    newTake0 = fromIntegral . sum $ map (stocktakeQuantity . entityVal . fst) (aTakes takes)
    newTake = Just newTake0
    mod = case aAdj takes of
            -- no adjustment can be there is no adj or
            -- the stocktake generated 0 adujstment. in that case
            -- this mean the stocktake is correct
            [] -> case stocktakeAdjustment (entityVal . fst . headEx $ aTakes takes) of
              Nothing -> 0
              Just _ ->  qoh - newTake0
              
            details -> let
                  lost = sum  [ (if stockAdjustmentDetailFrom adj == Just (FA.LocationKey "DEF")
                                 then 1
                                 else -1) * stockAdjustmentDetailQuantity adj
                              | adj <- details
                              ]


                  -- lost = qoh - stocktake - mod
                  in qoh - newTake0 - fromIntegral lost
    
  in events


-- | Moves coming from FrontAccounting and stocktakes from Fames
-- needs to be interleaved.  Normally, we just need to sort moves by date and by id
-- however, within the same day, adjustment needs to be move after the corresponding stocktakes.
interleaveEvents :: [(Key FA.StockMove, Move)] -> [Adjustment] -> [Either Move Adjustment]
interleaveEvents moves takes =  let
  moves' = [ (( FA.stockMoveTranDate (tMove move)
              , if isJust (tAdjId move)  then 3 else 2
              , fromIntegral $ FA.unStockMoveKey key), Left move)
           | (key, move) <- moves
           ]
  takes' = [ ((date, 1, (undefined . stocktakeAdjustment  . entityVal . fst $ headEx ts)), Right a)
           | a@(Adjustment adj ts) <- takes
           , let date = maximumEx $ map (stocktakeDate . entityVal . fst) ts
           ]
  in map snd $ sortBy (comparing fst) (moves' ++ takes')



loadMoves :: MonadIO m => Text -> ReaderT SqlBackend m [(Key FA.StockMove, Move)]
loadMoves sku = do
  let sql = "SELECT ??, COALESCE(br_name, supp_name), event_no FROM 0_stock_moves"
            <> supp <> customer <> adj
            <>" WHERE stock_id = ? AND loc_code = 'DEF' AND qty != 0 "
            <> " ORDER BY tran_date, trans_id "
      supp = " LEFT JOIN 0_suppliers ON (type in (" <> (inTypes [ST_SUPPRECEIVE, ST_SUPPCREDIT]) 
             <> "                         ) AND person_id = supplier_id) "
      customer = " LEFT JOIN 0_debtor_trans dt ON (dt.type = 0_stock_moves.type AND dt.trans_no = 0_stock_moves.trans_no AND 0_stock_moves.type in (" <> inTypes [ST_CUSTCREDIT, ST_CUSTDELIVERY] <> ") ) "
               <> " LEFT JOIN 0_cust_branch cust ON (cust.branch_code = dt.branch_code "
               <> "                              AND cust.debtor_no = dt.debtor_no)"
      adj =  " LEFT JOIN fames_transaction_map ON (fa_trans_no = 0_stock_moves.trans_no "
             <> " AND fa_trans_type = 0_stock_moves.type "
             <> "AND event_type = " <> tshow (fromEnum StockAdjustmentE) <> ")"
        
        
  -- traceShowM sql
  moves <- rawSql sql [PersistText sku]
  return [(key, Move move Nothing (fromMaybe "" (fmap decodeHtmlEntities info)) adj)
         | (Entity key move, Single info, Single adj) <- moves ]

loadTakes :: (MonadIO m) => Text -> ReaderT SqlBackend m [Adjustment]
loadTakes sku = do
  let sql = "SELECT ??, nickname FROM fames_stocktake "
            <> " JOIN fames_operator USING (operator_id) "
            <> " WHERE stock_id = ? ORDER BY date"
  takes <- rawSql sql [PersistText sku]
  -- join manual with the adjustment if any
  details <- selectList [StockAdjustmentDetailStockId ==. sku] [Asc StockAdjustmentDetailId]
  let takesByKey = Map.fromListWith (++) [(stocktakeAdjustment (entityVal take), [(take, operator)])
                                         | (take, Single operator) <- takes
                                         ]
      detailsByKey = Map.fromListWith (++) [(stockAdjustmentDetailAdjustment d, [d]) | (Entity _ d) <- details]

  return [Adjustment ( concat $ k >>= flip Map.lookup detailsByKey) ts | (k, ts) <- Map.toList takesByKey]





  
