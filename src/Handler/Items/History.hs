{-# LANGUAGE OverloadedStrings #-}
module Handler.Items.History where

import Import
import Handler.Table
import qualified FA as FA
import Database.Persist.MySQL(unSqlBackendKey, rawSql, Single(..))
import Data.List (mapAccumL, cycle)
import qualified Data.Map as Map
import Data.Text(splitOn)
import Text.Blaze(Markup, ToMarkup)

data FormParams = FormParams
  { pStart :: Maybe Day
  , pEnd :: Maybe Day
  , pLocation :: Maybe Text
  }

getItemsHistoryR :: Text -> Handler Html
getItemsHistoryR sku = do
  faUrl <- getsYesod (pack . appFAExternalURL . appSettings)
  renderUrl <- getUrlRenderParams
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
span.badge.Pickers
  background-color:#dfc
  color: black
span.badge.Packers
  background-color:#fdc
  color: black
                |]
        historyToTable (faUrl, renderUrl) history
  html <- defaultLayout tableW
  pushLinks sku (ItemsR (ItemsHistoryR sku)) []
  return html


-- data ItemEvent = ItemEvent deriving Show
data ItemEvent = ItemEvent
 { ieEvent :: Either Move Adjustment
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
  , tPickers :: Maybe Text
  , tPackers :: Maybe Text
  }
 
loadHistory :: Text -> Handler [ItemEvent]
loadHistory sku = do
  (moves, takes) <- runDB $ liftA2 (,) (loadMoves sku) (loadTakes sku)
  return . take 200 . reverse $ makeEvents moves takes


historyToTable :: (Text, Route App -> [(Text, Text)] -> Text) ->  [ItemEvent] -> Widget
historyToTable (faUrl, renderUrl) events = let
  columns = ["Type", "#", "Reference", "Location", "Date", "Info","InOut", "Quantity On Hand", "Stocktake", "Operator"]
  -- columns = ["Type", "#", "Reference", "Location", "Date", "InOut", "Operator"]
  colDisplay col = (toHtml col, [])
  rows = [ (valueFor (urlForFA faUrl, renderUrl) event, [])
         | event <- events
         ]
  in displayTable columns colDisplay rows
  
  
-- inlineAll' ::  [Text] -> Markup
inlineAll' :: (ToMarkup a, IsString a) => [a] -> Markup
inlineAll' = inlineAll'' "..." (\(v,k) -> [shamlet|<div class="#{k}">#{v}|])
inlineAll'' :: Applicative m => a -> ((a, Text) -> m ()) -> [a] -> m ()
inlineAll'' ellipsis render values = let
  v'ks = case values of
           [x] -> [(x,"")]
           (x:xs) -> [(x, "" :: Text), (ellipsis, "total")] ++ zip xs (cycle ["split"])
           [] -> []
  in mapM_ render v'ks

displayPkers :: Text -> Maybe Text -> Html
displayPkers _ Nothing = return ()
displayPkers title (Just pkers) = do
  let operators = splitOn "," pkers
      toBadge op = [shamlet|<span.badge class=#{title}>#{op}|]
  inlineAll' (map toBadge operators)
  
-- displayPkers title_ (Just pkers) = do
--   let operators = splitOn "," pkers
--       title = [shamlet|<span.badge class="#{title}">#{title}|]
--   case operators of
--     [op] -> toHtml ( title <> op)
--     _ -> inlineAll' (title: operators)
  

valueFor :: (FATransType -> Int -> Text, (Route App) -> [(Text, Text)] -> Text)
         -> ItemEvent -> Text -> Maybe (Html, [Text])
valueFor faUrl ie "InOutQ" = let
  fromM = fromMaybe ("", [])
  (in_, _) = fromM (valueFor faUrl ie "In")
  (out, _) = fromM (valueFor faUrl ie "Out")
  (qoh, k) = fromM (valueFor faUrl ie "Quantity On Hand")
  in Just (in_ >> qoh >> out, k)
valueFor faUrl ie "InOut" = let
  fromM = fromMaybe ("", [])
  (in_, k) = fromM (valueFor faUrl ie "In")
  (out, _) = fromM (valueFor faUrl ie "Out")
  in Just (in_ >> out, k)
valueFor _ (ItemEvent ie qoh stake mod_) "Quantity On Hand" =
  let found = (fromMaybe 0 stake + mod_ - qoh)
  -- if a stock adjustment result in matching the stocktake  : badge
  in case ie of
       (Left move) | (toEnum . FA.stockMoveType $ tMove move) `elem` [ST_LOCTRANSFER, ST_INVADJUST]
                      && found == 0
                      && mod_ == 0 -> Just (badgeSpan' okBadge qoh "", [])
       _ -> Just ( -- toHtml (formatQuantity qoh)
                  badgeSpan' qohBadge qoh ""
                  >> badgeSpan' modBadge (- mod_) ""
                  >> badgeSpan' inBadge found "", ["qoh"])
valueFor _ (ItemEvent ie qoh (Just stake) mod_) "Stocktake" =
  let lost = (qoh - stake - mod_)
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
  in Just (stake' >> badgeSpan' modBadge mod_ "" >>  badgeSpan' outBadge lost "", [])
valueFor _ (ItemEvent _ _ Nothing _) "Stocktake" = Nothing
valueFor (urlForFA', renderUrl) (ItemEvent (Left (Move FA.StockMove{..} _ info adjM pickers packers)) __qoh __stake _)  col = case col of
  "Type" -> Just (showTransType $ toEnum stockMoveType, [])
  "#" -> Just ([shamlet|<a href="#{urlForFA' (toEnum stockMoveType) stockMoveTransNo}" target=_blank>#{stockMoveTransNo}|], [])
  "Reference" -> Just (case adjM of
                         Nothing -> toHtml (stockMoveReference)
                         Just adj -> let adjId = fromIntegral adj
                                       in [hamlet|<a href="@{WarehouseR (WHStockAdjustmentViewR adjId)}" target=_blank>#{stockMoveReference}|] renderUrl
                                          , [])
  "Location" -> Just (toHtml (stockMoveLocCode), [])
  "Date" -> Just (toHtml (tshow stockMoveTranDate), [])
  "In" -> let bg = if toEnum stockMoveType == ST_LOCTRANSFER -- found
                   then okBadge
                   else inBadge
          in Just (badgeSpan' bg stockMoveQty "", [])
  "Out" -> Just (badgeSpan' outBadge (-stockMoveQty) "", [])
  "Operator" -> Just (displayPkers "Pickers" pickers >> displayPkers "Packers" packers, [])
  "Info" -> Just (toHtml info, [])
  _ -> Nothing

valueFor (_, renderUrl) (ItemEvent (Right adj ) __qoh __stake _) col = let
  inlineAll f = inlineAll' (map (f . entityVal . fst) (aTakes adj))
  diff = 0 -- qoh - fromIntegral stocktakeQuantity
  lost = max 0 diff
  found = max 0 (-diff)
  in case col of
  "Type" -> Just ("Stocktake", [])
  "#" -> ( \a -> let adj_ =  stockAdjustmentDetailAdjustment a
                     adjId =  unSqlBackendKey $ unStockAdjustmentKey adj_
                 in ([hamlet|<a href="@{WarehouseR (WHStockAdjustmentViewR adjId)}" target=_blank>#{adjId}|] renderUrl
                     , [])
         ) <$> headMay (aAdj adj)
  -- "#" -> Just ([hamlet|<a href="@{WarehouseR WHStocktakeR}?doc_key=#{unDocumentKeyKey . stocktakeDocumentKey . entityVal . fst}">|] renderUrl, [])
  "Reference" -> let barWithRef (Nothing, k) = [shamlet|<div class="#{k}">...|]
                     barWithRef (Just stock,k) =
                       let barcode = stocktakeBarcode stock
                           docKey = unSqlBackendKey $ unDocumentKeyKey (stocktakeDocumentKey stock)
                                        in [hamlet|
<div class="#{k}">
  <a href="@{WarehouseR WHStocktakeR}?doc_key=#{docKey}" target=_blank>##{barcode}|] renderUrl
                                            
                 in Just (inlineAll'' Nothing barWithRef (map (Just . entityVal .fst) (aTakes adj)) , [])
  "Location" -> Just (inlineAll  (FA.unLocationKey . stocktakeFaLocation), [])
  "Date" -> Just (inlineAll (tshow . stocktakeDate), [])
  "In" -> Just (badgeSpan' inBadge found "", [])
  "Out" -> Just (badgeSpan' outBadge lost "", [])
  "Operator" -> Just (inlineAll' (map snd $ aTakes adj), [])
  "Info" -> case mapMaybe (stocktakeComment . entityVal . fst) (aTakes adj) of
                  [] -> Nothing
                  _ ->  Just (inlineAll (\e -> fromMaybe "" $ decodeHtmlEntities <$> stocktakeComment e), [])
  _ -> Nothing



-- badgeSpan' :: (Num a, Ord a, ToMarkup a)  => a -> Maybe String -> String -> Html
badgeSpan' :: Maybe String -> Double -> String -> Html
badgeSpan' bgM qty klass =
  badgeSpan badgeWidth qty bgM klass

okBadge :: IsString a => Maybe a
okBadge = Just "#29abe0"
qohBadge :: IsString a => Maybe a
qohBadge = Just "#ccccff"
inBadge :: Maybe a
inBadge = Nothing
outBadge :: IsString a => Maybe a
outBadge = Just "#d9534f"
negBadge :: IsString a => Maybe a
negBadge = Just "#000000"
modBadge :: IsString a => Maybe a
modBadge = Just "#cccccc"



badgeWidth :: (Integral a, RealFrac t) => t -> Maybe a
badgeWidth q | q <= 0 = Nothing
             | otherwise = Just . (max 2) . floor $ (min q  12) 

makeEvents :: [(Key FA.StockMove, Move)] -> [Adjustment] -> [ItemEvent]
makeEvents moves takes = let
  lines_ =  interleaveEvents moves takes
  events = snd $  mapAccumL accumEvent (0, Nothing) lines_ 
  accumEvent (qoh, stake) e@(Left move') = ((newQoh, newTake), ItemEvent e newQoh newTake 0) where
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
  accumEvent (qoh, _) e@(Right takes0 ) =
    ((qoh, (+mod_) <$> newTake), ItemEvent e qoh newTake mod_) where
    newTake0 = fromIntegral . sum $ map (stocktakeQuantity . entityVal . fst) (aTakes takes0)
    newTake = Just newTake0
    mod_ = case aAdj takes0 of
            -- no adjustment can be there is no adj or
            -- the stocktake generated 0 adujstment. in that case
            -- this mean the stocktake is correct
            [] -> case stocktakeAdjustment (entityVal . fst . headEx $ aTakes takes0) of
              Nothing -> 0
              Just _ ->  qoh - newTake0
              
            details -> let
                  lost = sum  [ (if stockAdjustmentDetailFrom adj == Just (FA.LocationKey "DEF")
                                 then 1
                                 else -1) * stockAdjustmentDetailQuantity adj
                              | adj <- details
                              ]


                  -- lost = qoh - stocktake - mod_
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
  takes' = [ ((date, 1, (maybe 0 (fromIntegral . unStockAdjustmentKey) $ stocktakeAdjustment  . entityVal . fst $ headEx ts)), Right a)
           | a@(Adjustment __adj ts) <- takes
           , let date = maximumEx $ map (stocktakeDate . entityVal . fst) ts
           ]
  in map snd $ sortBy (comparing fst) (moves' ++ takes')



loadMoves :: MonadIO m => Text -> ReaderT SqlBackend m [(Key FA.StockMove, Move)]
loadMoves sku = do
  let sql = "SELECT ??, COALESCE(br_name, supp_name), event_no, pickers, packers FROM 0_stock_moves"
            <> supp <> customer <> adj
            <> "LEFT JOIN (" <> operators <> ") operators ON debtor_trans_no = 0_stock_moves.trans_no "
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
      operators = ""
                <> "select debtor_trans_no, GROUP_CONCAT(distinct IF(typeId =1, operator.name, NULL)) pickers "
                <> ", GROUP_CONCAT(distinct IF(typeId =2, operator.name, NULL)) packers "
                <> "from 0_debtor_trans_details "
                <> "join mop.action ON (0_debtor_trans_details.src_id = action.detailId)  "
                <> "join mop.session ON (session.groupId = action.actionGroupId) "
                <> "join mop.operator ON (session.operatorId = operator.id) "
                <> "where 1 "
                <> "and debtor_trans_type = 13 "
                <> "group by debtor_trans_no "
        
        
  moves <- rawSql sql [PersistText sku]
  return [(key, Move move Nothing (fromMaybe "" (fmap decodeHtmlEntities info)) adj0 picker packer)
         | (Entity key move, Single info, Single adj0, Single picker, Single packer) <- moves ]

loadTakes :: (MonadIO m) => Text -> ReaderT SqlBackend m [Adjustment]
loadTakes sku = do
  let sql = "SELECT ??, nickname FROM fames_stocktake "
            <> " JOIN fames_operator USING (operator_id) "
            <> " WHERE stock_id = ? ORDER BY date"
  takes <- rawSql sql [PersistText sku]
  -- join manual with the adjustment if any
  details <- selectList [StockAdjustmentDetailStockId ==. sku] [Asc StockAdjustmentDetailId]
  let takesByKey = Map.fromListWith (++) [(stocktakeAdjustment (entityVal take0), [(take0, operator)])
                                         | (take0, Single operator) <- takes
                                         ]
      detailsByKey = Map.fromListWith (++) [(stockAdjustmentDetailAdjustment d, [d]) | (Entity _ d) <- details]

  return [Adjustment ( concat $ k >>= flip Map.lookup detailsByKey) ts | (k, ts) <- Map.toList takesByKey]





  
