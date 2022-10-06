{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Handler.WH.StockAdjustment
( getWHStockAdjustmentR
, getWHStockAdjustmentViewR
, postWHStockAdjustmentR
, postWHStockAdjustmentToFAR
, postWHStockAdjustmentRejectR
, nextWorkingDay
, previousWorkingDay
) where

import Import
import Yesod.Form.Bootstrap3
import Database.Persist.MySQL
import qualified Data.List as List
import Data.Time (addDays)
import Data.Time.Calendar.WeekDate (toWeekDate)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as Text

import SharedStockAdjustment
import qualified FA as FA
import qualified WH.FA.Types as WFA
import qualified WH.FA.Curl as WFA
import Control.Monad.Except hiding (mapM_)

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
  , stocktakeDoc :: Maybe Int64 -- ^ stocktakes are groupeb by documentKey
  , comment :: Maybe Text
  , activeRows :: Set Text
  , quantityBefore :: Map Text Int -- ^ Quantity to substract from qoh
  , minDate :: Maybe Day -- ^ force date
  , maxDate :: Maybe Day -- ^ force date
  } deriving (Eq, Read, Show)


data FormMode = Save | Reject | View deriving (Eq, Read, Show)
paramForm mode = renderBootstrap3 BootstrapBasicForm  form
  where form = FormParam
            <$> aopt textField "style" Nothing
            <*> areq boolField "download" (Just False)
            <*> aopt intField "min quantity" Nothing
            <*> aopt intField "max quantity" Nothing
            <*> areq (selectField optionsEnum)"sort by" Nothing
            <*> areq (selectField optionsEnum) "mode" (Just All)
            <*> aopt intField "modulo" Nothing
            <*> aopt intField "Stocktake#" Nothing
            <*>  (unTextarea <$$> case mode of
                   Save -> aopt textareaField "comment" Nothing
                   Reject -> pure Nothing
                   View -> pure Nothing)
            <*> pure (Set.fromList [])
            <*> pure (Map.fromList [])
            <*> aopt dayField "min Date" (Nothing)
            <*> aopt dayField "max Date" (Nothing)

-- Holds different information relative to adjusment, lost, new etc ...
data Carts adj trans = Carts { cNew :: adj, cLost :: trans, cFound :: trans} deriving Show
type DetailCarts = Carts [StockAdjustmentDetail] [StockAdjustmentDetail]
-- | Adjusted cart details with cost price and real quantity
type DetailCarts' = Carts [(StockAdjustmentDetail, Double)] [(StockAdjustmentDetail, Int)]
type FACarts = Carts WFA.StockAdjustment WFA.LocationTransfer


getActiveRows :: [(Text, Text)] -> Set Text
getActiveRows params = 
  let prefixLength = 7 --  length ("active-" :: Text)
      skusToKeep = [drop prefixLength  sku | (sku, checked) <- params, checked == "on" ]
  in Set.fromList (skusToKeep)

-- | Create a map Sku -> quantities selected from the postParams
getQuantityBefore :: [(Text, Text)] -> Map Text Int
getQuantityBefore params = 
  -- The same sku can appears more than once with different quantities
  -- We need to sum those quantites ()
  let sku'qtys = mapMaybe (traverse readMay) params
  in Map.fromListWith (+) sku'qtys
{-# NOINLINE getWHStockAdjustmentR #-}
getWHStockAdjustmentR :: Handler Html
getWHStockAdjustmentR = do
  renderStockAdjustment



renderStockAdjustment :: Handler Html
renderStockAdjustment = do
  (paramForm', encType) <- generateFormPost (paramForm View)
  pendings <- renderPending
  lastAdjs <- renderLast 50
  lastTakes <- renderTakes 50
  let response = [whamlet|
<div.panel.panel-info>
  <div.panel-heading>
    <h3> Pending Stock Adjustments
  <div.panel-body>
    ^{pendings}
<div.panel.panel-primary>
  <div.panel-heading>
    <h3> Last 10 Stock AdjustmentS
  <div.panel-body>
    ^{lastAdjs}
<div.panel.panel-primary>
  <div.panel-heading>
    <h3> Last 10 Stocktakes
  <div.panel-body>
    ^{lastTakes}
<div.panel> 
  <div.panel-body>
    <form.well #stock-adjustement role=form method=post action=@{WarehouseR WHStockAdjustmentR} enctype=#{encType}>
      ^{paramForm'}
      <button type="submit" name="submit" .btn.btn-default>Submit
|]

  defaultLayout response



{-# NOINLINE postWHStockAdjustmentR #-}
postWHStockAdjustmentR :: Handler Html
postWHStockAdjustmentR = do
  AppSettings{..}  <- getsYesod appSettings
  action <- lookupPostParam "action"
  let mode = case action of
            Just "save" -> Save
            Just "reject" -> Reject
            _ -> View
        

  (pp, _) <- runRequestBody
  ((resp, view), encType) <- runFormPost (paramForm mode)
  case resp of
    FormMissing -> error "Form missing"
    FormFailure _ -> defaultLayout [whamlet|^{view}|]
    FormSuccess param0 -> do
      let activeRows = getActiveRows pp
          qBefore = getQuantityBefore pp
      let param = param0 {activeRows = activeRows, quantityBefore = qBefore}

      let (w,p) = unzip $ catMaybes
                  [ style param <&>  (\like -> (" AND stock_id like ?", PersistText like))
                  , stocktakeDoc param <&> (\key -> (" AND document_key_id = ?", PersistInt64 key))
                  ]
      let sql = "SELECT stock_id, COALESCE(SUM(quantity),0), MAX(date), GROUP_CONCAT(comment) "
                <> " FROM fames_stocktake  "
                <> " WHERE stock_adj_id IS NULL "
                <> " AND active = 1 "
                <> (concat w)
                <> " GROUP BY stock_id "

      stocktakes <- runDB $ rawSql sql p
      results <- catMaybes <$> mapM (qohFor appFADefaultLocation appFALostLocation (minDate param, maxDate param)) stocktakes
      let withDiff = [(abs (quantityTake0 (mainLocation pre) - quantityAt (mainLocation pre)), pre) |  pre <- results]
          f  (q, pre) = (maybe True (q >=) (minQty param))
                   &&  (maybe True (q <=) (maxQty param))
                   && let isUnsure = not . null $  concatMap (\loc -> movesAt $ loc pre) [mainLocation, lost]
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
      let _unused_classFor' qty qoh = case compare qty qoh of
            GT -> "success"
            EQ -> ""
            LT -> "danger" :: Text
          classesFor [] = "" :: Text
          classesFor _ = "unsure danger" :: Text
          toOrigAndBadges'  = toOrigAndBadges (modulo param)
      
      response <- case mode of
            Save -> saveStockAdj param rows
            Reject -> rejectTakes param rows
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
      <th> Comment
    $forall pre <- rows
      $with ((qties, badges, before), mainLocationMoves, __lostMoves) <- (toOrigAndBadges' pre, (movesAt $ mainLocation pre), movesAt $ lost pre)
        <tr class="#{classesFor mainLocationMoves}"
            id="#{encodedSku pre}-row"
            data-sku="#{encodedSku pre}"
            data-hidden="true"
            >
          <td.active><input type="checkbox" name="active-#{sku pre}" checked>
          <td.style><a href=@{route pre} target="_blank">#{sku pre}
          <td.quantity data-original=#{qtake qties}>#{qtake qties}
            ^{badgeSpan' (bMissing badges) (Just redBadgeBg) "missing"}
            ^{badgeSpan' (bMissingMod badges) (Just grayBadgeBg) "missing-mod"}
          <td.date>#{tshow $ (takeDate pre)}
          <td.qoh data-original=#{qoh qties}>
            <span.qoh>#{qoh qties + before}
            ^{badgeSpan' (bFoundMod badges) (Just grayBadgeBg ) "found-mod"}
            ^{badgeSpan' (bFound badges) (Just blueBadgeBg) "found"}
            ^{badgeSpan' (bNew badges) Nothing "new" }
          <td.lost data-original=#{qlost qties}>#{qlost qties}
          <td.last_move>#{fromMaybe "" (tshow <$> (lastMove pre))}
          <td.comment_move>#{fromMaybe "" (tshow <$> (preComment pre))}
          $forall (move, before) <- detectBefores (takeDate pre) (preComment pre) mainLocationMoves
              $with after <- not before
                <tr :before:.bg-info class="move sku-#{encodedSku pre}" style="display:none">
                  <td> <select name="#{sku pre}">
                      <option :before:selected value="#{movePickedQty move}" >Before
                      <option :after:selected value="0">After
                  <td>
                  <td> #{movePickedQty move}
                  <td> #{tshow $ moveDate move}
                  <td>
                  <td>
                  <td> #{(fromMaybe "" $ moveOperatorName move)}
                  <td> #{moveCustomerName move}
|]
      defaultLayout [whamlet|
<form.well #stock-adjustment role=form method=post action=@{WarehouseR WHStockAdjustmentR} enctype=#{encType}>
  ^{view}
  <button type="submit" name="action" value="submit" .btn.btn-primary>Submit
  <button type="submit" name="action" value="save" .btn.btn-danger>Save
  <button type="submit" name="action" value="reject" .btn.btn-warning>Reject
  ^{response}
|]

-- | Moves are "before" the stock take
-- if the date is before the take date and not after (in order)
-- the customer associated to the stocktake
--
--      AAAAABBBBCCC
--             ^
--             + customer
--  returns (B is today)
--      AAAAABB
--
--      AAAAABBBBCCC
--         ^
--         + customer
--  returns (B is today)
--      AAAA
--
--      AAAAABBBBCCC
--                ^
--                + customer
--  returns (B is today)
--      AAAAA     C
detectBefores :: Day -> Maybe Text -> [MoveInfo] -> [(MoveInfo, Bool)]
detectBefores takeDay commentm moves = zip moves (map (const True) befores ++ repeat False) where
  (beforeCustomers, others)  = List.break (isCustomer) moves
  isBefore move = takeDay >= moveDate move
  befores = filter isBefore beforeCustomers ++ take 1 others
  isCustomer move =
    case commentm of
      Nothing -> False
      Just withCust -> moveCustomerName move `Text.isInfixOf` withCust


-- | Escape sku to be JQuery friendly
encodedSku :: PreAdjust -> Text
encodedSku pre = Text.replace "/" "_div_"
               $ Text.replace "*" "_star_"
               $ Text.replace "?" "_qmark_"
               $ Text.replace "!" "_emark_"
               $  (sku pre)
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
  , mainLocation :: !LocationInfo
  , lost :: !LocationInfo
  , takeDate :: !Day
  , preComment :: Maybe Text
  } deriving (Eq, Read, Show)

adjustInfos :: PreAdjust -> [LocationInfo]
adjustInfos adj  = [mainLocation, lost] <*> [adj]

route :: PreAdjust -> Route App
route pre = ItemsR $ ItemsHistoryR (sku pre)

data MoveInfo = MoveInfo
 { moveDate :: !Day
 , moveCustomerName :: !Text
 , moveOperatorName :: Maybe Text
 , movePickedQty :: !Int
 } deriving (Eq, Read, Show)

lastMove :: PreAdjust -> Maybe Day
lastMove pre = max (date (mainLocation pre)) (date (lost pre)) 
-- | Returns the qoh at the date of the stocktake.
-- return also the date of the last move (the one corresponding to given quantity).
-- Needed to detect if the stocktake is sure or not.
qohFor :: Text -> Text -> (Maybe Day, Maybe Day) ->  (Single Text, Single Int,  Single Day, Single (Maybe Text)) -> Handler (Maybe PreAdjust)
qohFor mainLoc lostLoc dateRange r@(Single sku, Single __qtake, Single date, Single comment) = do
  adj <-  PreAdjust
         <$> pure sku
         <*> quantitiesFor mainLoc dateRange r
         <*> quantitiesFor lostLoc dateRange r
         <*> pure date
         <*> pure comment
  return $ if all noInfo (adjustInfos adj)
              then Nothing
              else Just adj

-- | Retrive the quantities available for the given location at the given date
quantitiesFor :: Text -> (Maybe Day, Maybe Day) -> (Single Text, Single Int, Single Day, Single (Maybe Text)) -> Handler LocationInfo 
quantitiesFor loc (minDateM, maxDateM) (Single sku, Single take_, Single date, Single __comment) = do
  -- force date range from URL
  let (minDate0, maxDate0) = unsureRange date
      sql = "SELECT SUM(qty) qoh"
            <> ", SUM(qty) qoh_at"
            <> ", MAX(tran_date)  "
            <> "FROM 0_stock_moves "
            <> "WHERE stock_id = ? "
            <> "AND tran_date <= ? "
            <> "AND loc_code = ?"
      minDate = minimum $ minDate0 `ncons` (toList minDateM)
      maxDate = maximum $ maxDate0 `ncons` (toList  maxDateM)

  -- extract all relevant moves within the unsure range
  let sqlForMoves = "SELECT moves.tran_date, COALESCE(debtor.debtor_ref, '<>'), COALESCE(operators, 'no op'), SUM(moves.qty) "
                   <> " FROM 0_stock_moves moves "
                   <> " LEFT JOIN 0_debtor_trans USING(trans_no, type)"
                   <> " LEFT JOIN 0_debtors_master debtor USING(debtor_no)"
                   <> " LEFT JOIN (" <> operators <> ") operators ON (orderId = order_ AND sku = stock_id )"
                   <> " WHERE stock_id = ?"
                   <> " AND moves.tran_date between ? AND ?"
                   <> " AND loc_code = ?"
                   <> " GROUP BY trans_no, type"
                   <> " ORDER BY moves.tran_date, moves.trans_id " :: Text
      
      -- we have to group operators in a separate query to avoid moves quantity to be counted many times
      operators = " SELECT orderId, sku, GROUP_CONCAT(distinct operator.name) as operators "
                   <> " FROM mop.action "
                   <> " JOIN mop.session ON (groupId = actionGroupId)"
                   <> " JOIN mop.operator ON (operatorId = operator.id)"
                   <> " WHERE typeId = 1"
                   <> " AND sku = ? "
                   <> " GROUP BY orderId, sku"
      toMove (Single date_, Single debtor, Single operators_, Single qty) = MoveInfo date_ debtor operators_ qty
                    
  (results, moves) <- runDB $ do
    results <- rawSql sql [PersistText sku, PersistDay maxDate, PersistText loc]
    moves <- rawSql sqlForMoves [PersistText sku, PersistText sku, PersistDay minDate, PersistDay maxDate, PersistText loc]
    return (results , moves)


  return $ case results of
    [] -> LocationInfo loc Nothing 0 0 Nothing []
    [(Single qoh, Single at, Single last_)] -> LocationInfo loc (Just take_) (fromMaybe 0 at) (fromMaybe 0 qoh) (last_) (map toMove moves)
    _ -> error "query should return 1 value at the most"


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



computeAdj :: (Map Text Int) -> Maybe Int -> StockAdjustmentId -> PreAdjust -> [StockAdjustmentDetail]
computeAdj qBefores modulo key pre =
  let qBefore = Map.findWithDefault 0 (sku pre) qBefores
      -- \^ quantity selected by the user
      (orig, _) = preToOriginal modulo pre
      BadgeQuantities{..} = computeBadges orig { qoh = qoh orig + qBefore }
      lostLoc = Just . FA.LocationKey . location . lost $ pre 
      mainLocationLoc = Just . FA.LocationKey . location . mainLocation $ pre 
      stock_id = sku pre

      adjs = [ StockAdjustmentDetail key stock_id qty fromTo toTo
             | (qty, fromTo, toTo) <- [ (bMissing, mainLocationLoc, lostLoc )
                                      , (bFound, lostLoc, mainLocationLoc)
                                      , (bNew, Nothing, mainLocationLoc)
                                      ]
            ]
  in filter ((/= 0) . stockAdjustmentDetailQuantity) adjs

saveStockAdj :: FormParam -> [PreAdjust] -> Handler (Widget)
saveStockAdj FormParam{..} pres' = do
  now  <- liftIO getCurrentTime
  userId <- requireAuthId
  -- we Only keep adjustment which are active
  -- we also need to adjust the quantity on hand
  -- according to the before/after quantities
  let pres = filter ((`elem` activeRows) . sku) pres'
  runDB $ do
    let adj = StockAdjustment (fromMaybe "" comment) now Pending userId
    adjKey <- insert adj

    let adjs = concatMap (computeAdj quantityBefore modulo adjKey) pres
    insertMany_ adjs

    -- update stock take
    mapM_ (\pre -> updateWhere [ StocktakeStockId ==. (sku pre)
                               , StocktakeActive ==. True
                               , StocktakeAdjustment ==. Nothing
                               ]
                               [StocktakeAdjustment =. Just adjKey])
                               pres
    setSuccess "Stock adjustment saved"
    let adjId = unSqlBackendKey (unStockAdjustmentKey adjKey)
    lift $ pushLinks ("View Adjustment #" <> tshow adjId)
                     (WarehouseR $ WHStockAdjustmentViewR adjId )
                     []
    return ""

-- | Delete stocktakes  because they are probably wrong.
-- We could inactive them instead, but then we won't know
-- the difference between a rejected stocktake and a old one.
rejectTakes :: FormParam -> [PreAdjust] -> Handler (Widget)
rejectTakes FormParam{..} pres' = do
  let pres = filter ((`elem` activeRows) . sku) pres'
   
      -- in case  document has been selected
      -- we need to only remove this one
      globalFilter = catMaybes [
        stocktakeDoc <&> (\key -> StocktakeDocumentKey ==. (DocumentKeyKey' $ SqlBackendKey key ) )
        ]
      
  _ <- runDB $
     (flip traverse) pres  $ \pre -> 
         deleteWhere ([StocktakeStockId ==. sku pre
                      , StocktakeActive ==. True
                      , StocktakeAdjustment ==. Nothing
                      ]
                      ++ globalFilter
                     )
  setSuccess "Stocktakes have been deleted sucessfuly "
  return ""
-- | Reject a stock adjusment, releasing the stocktakes to be processed again.
-- Depending of it's state
-- The stock adjustment can be deleted
-- or not.
-- Transaction in FA are left, which should not be a problem
-- has the next stock adjustment should take the new QOH into account.
rejectStockAdj :: Int64 -> Handler ()
rejectStockAdj adjId = do
  let key = StockAdjustmentKey (SqlBackendKey adjId)
  -- unset stock adjustment
  runDB $ do
    adj <- getJust key
    updateWhere [StocktakeAdjustment ==. Just key] [StocktakeAdjustment =. Nothing]
  -- delete stock adjustment if possible
    when (stockAdjustmentStatus adj == Pending ) $ do
      deleteWhere [StockAdjustmentDetailAdjustment ==. key]
      deleteWhere [StockAdjustmentId ==. key]

    setSuccess . toHtml $ "Adjustment #"<> tshow adjId <> " has been successfully rejected."

  
-- | Displays a list of pending adjustment
renderPending :: Handler Widget
renderPending =  do
  pendings <- runDB $ selectList [StockAdjustmentStatus ==. Pending] [] -- [LimitTo 10]
  renderAdjustments pendings

renderLast :: Int -> Handler Widget
renderLast limit =  do
  lasts <- runDB $ selectList [StockAdjustmentStatus !=. Pending] [LimitTo limit, Desc StockAdjustmentId]
  renderAdjustments (reverse lasts)

renderAdjustments :: [Entity StockAdjustment] -> Handler Widget
renderAdjustments adjustments0 = do
  let adjustments = sortOn (Down . stockAdjustmentDate . entityVal) adjustments0
  a'details <- zip adjustments <$> mapM (loadDetailSummary . entityKey) adjustments
  let qtyFor getter cart = sum $ map stockAdjustmentDetailQuantity $ getter cart
  return [whamlet|
<table *{datatable}>
  <thead>
    <tr>
      <th> Id
      <th> Date
      <th> Comment
      <th> Statement
      <th> Lost
      <th> Found
      <th> New
  $forall (Entity k adj, cart) <- a'details
    <tr>
      <td> <a target=_blank href=@{WarehouseR (WHStockAdjustmentViewR (unSqlBackendKey $ unStockAdjustmentKey k) )}>
       ##{tshow $ unSqlBackendKey $ unStockAdjustmentKey k}
      <td> #{tshow $ stockAdjustmentDate adj}
      <td> #{take 50 $ stockAdjustmentComment adj}
      <td> #{tshow $ stockAdjustmentStatus adj}
      <td> ^{badgeSpan' (qtyFor cLost cart) (Just redBadgeBg) "" }
      $# <td> ^{badgeSpan' (qtyFor cFound cart - qtyFor cNew cart) (Just blueBadgeBg) "" }
      <td> ^{badgeSpan' (qtyFor cFound cart) (Just blueBadgeBg) "" }
      <td> ^{badgeSpan' (qtyFor cNew cart) (Just greenBadgeBg) "" }
|]

loadDetailSummary :: Key StockAdjustment -> Handler DetailCarts
loadDetailSummary adjKey = do 
  AppSettings{..} <- getsYesod appSettings
  let [main,lost] = map FA.LocationKey  [appFADefaultLocation, appFALostLocation]
  entities <- runDB $ selectList [StockAdjustmentDetailAdjustment ==. adjKey] []
  return $ splitDetails (Just main) (Just lost) (map entityVal entities)

renderTakes :: Int -> Handler Widget
renderTakes limit = do
  rtakes <- runDB $ selectList [DocumentKeyType ==. "stocktake"] [LimitTo limit
                                                                 , Desc DocumentKeyProcessedAt
                                                                 , Desc DocumentKeyId]
  take'sums <- zip rtakes <$>  (runDB $ mapM (loadTakeSummary . entityKey) rtakes)
  return [whamlet|
<table *{datatable}>
  <thead>
    <tr>
      <th> Id
      <th> Date
      <th> Comment
      <th> Barcodes
      <th> Quick
      <th> MOP
  $forall (Entity k doc, (total, quick, mop)) <- take'sums
    <tr>
      <td> ##{tshow $ unSqlBackendKey $ unDocumentKeyKey k}
      <td> #{tshow $ documentKeyProcessedAt doc}
      <td> #{take 50 $ documentKeyComment doc}
      <td> ^{badgeSpan' total Nothing ""}
      <td> ^{badgeSpan' quick (Just redBadgeBg) ""}
      <td> ^{badgeSpan' mop (Just blueBadgeBg) ""}
|]

-- Count for a given stocktake the number of real stocktake (with barcode)
-- vs zerotakes
loadTakeSummary :: Key DocumentKey -> SqlHandler (Int, Int, Int)
loadTakeSummary docKey = do
  let sql =  " select count(*) "
          <> ", sum(if(barcode like 'ZT%' and quantity <> 0 and comment not like 'MOP%', 1, 0))"
          <> ", sum(if(comment like 'MOP%', 1, 0)) "
          <> " from fames_stocktake "
          <> " where document_key_id = ?  and `index` = 1  "
  results <- rawSql sql [toPersistValue docKey]
  return $ case results of 
    [(Single total, Single (fromMaybe 0 -> quick), Single (fromMaybe 0 -> mop))] -> (total -quick-mop, quick , mop)
    [] -> (0,0,0)
    (_:_) -> error $ "Query with a SUM shoudln't return more than one row" <> show results

        -- <a href="@{WarehouseR WHStockAdjustmentR}&stock_id=#{tshow . unSqlBackendKey $ unStockAdjustmentKey k}">

-- select * from fames_stocktake where document_key_Id = 1223

-- | Load an adjustment from its key. We assume, as the key as been provided
-- and should come from somewhere, that the adjusment exists
-- loadAdjustment :: Int64 -> Handler Maybe (([StockAdjustmentDetail], StockAdjustmenty))
loadAdjustment :: Int64 -> SqlHandler ([StockAdjustmentDetail], StockAdjustment)
loadAdjustment key = do
  let adjKey  = (SqlBackendKey key)
  adj <- getJust (StockAdjustmentKey adjKey)
  entities <- selectList [StockAdjustmentDetailAdjustment ==. StockAdjustmentKey adjKey] []
  return (map entityVal entities, adj)

loadAdjustmentTakes :: Int64 -> SqlHandler [Stocktake]
loadAdjustmentTakes key = do
  let adjKey  = StockAdjustmentKey (SqlBackendKey key)
  map entityVal <$>  selectList [StocktakeAdjustment ==. Just adjKey] [Asc StocktakeStockId]

  
{-# NOINLINE getWHStockAdjustmentViewR #-}
getWHStockAdjustmentViewR :: Int64 -> Handler Html
getWHStockAdjustmentViewR key = do
  setting <- appSettings <$> getYesod
  let mainLocationLoc = Just (FA.LocationKey $ appFADefaultLocation setting)
      lostLoc = Just (FA.LocationKey $ appFALostLocation setting)

  
  ((details, adj), takes) <-runDB $ do
    adjs <- loadAdjustment key
    takes <- loadAdjustmentTakes key
    return (adjs, takes)

  -- let date = utctDay $  stockAdjustmentDate adj
  date <- utctDay <$> liftIO getCurrentTime
  
  Carts{..} <- adjustCarts date $ splitDetails mainLocationLoc lostLoc details 

  let renderStockId stockId = [whamlet|<a href="@{route'}" target="_blank">#{stockId}|]
        where route' = ItemsR (ItemsHistoryR stockId)
      renderStockIdFor = renderStockId . stockAdjustmentDetailStockId 
  let renderDetails :: Text -> Text -> [(StockAdjustmentDetail, Int)] -> Widget
      renderDetails title class_ details' = [whamlet|
<div.panel. class="panel-#{class_}">
  <div.panel-heading>
    <h3> #{title}
  <div.panel-body>
    <p.well>
      $forall (d, qty) <- details'
       ^{renderStockIdFor d}
       #{qty} 
       $with oqty <- stockAdjustmentDetailQuantity d
         $if oqty /= qty
            -- Original qty:#{stockAdjustmentDetailQuantity d}
       <br>
                          |]
  let renderDetails' :: Text -> Text -> [(StockAdjustmentDetail, Double)] -> Widget
      renderDetails' title class_ details' = [whamlet|
<div.panel. class="panel-#{class_}">
  <div.panel-heading>
    <h3> #{title}
  <div.panel-body>
    <p.well>
      $forall (d, cost) <- details'
        ^{renderStockIdFor d}
        #{stockAdjustmentDetailQuantity d}
        #{dollar}#{tshow cost}
        <br>
                          |] where dollar = "$" :: Text

  let renderTakes' takes' = [whamlet|
<div.panel. class="panel-primary">
  <div.panel-heading>
    <h3> Stocktakes
  <div.panel-body>
    <p.well>
      $forall take <- takes'
        ^{renderStockId (stocktakeStockId take) }
        #{stocktakeQuantity take}
        #{tshow $ stocktakeDate take}
        ##{tshow $ unSqlBackendKey $ unDocumentKeyKey $ stocktakeDocumentKey take}
        <br>
                            |]

  let page = do 
        renderDetails "Found" "success"  cFound
        renderDetails "Lost" "danger"  cLost
        renderDetails' "New" "warning"  cNew
        renderTakes' takes
  
  defaultLayout $ do
    [whamlet|
<div>
  #{tshow $ stockAdjustmentDate adj}
  #{tshow $ stockAdjustmentStatus adj}
  <div.well> #{stockAdjustmentComment adj}
    ^{page}
    $if (stockAdjustmentStatus adj /= Process)
      <form role=form method=post action=@{WarehouseR $ WHStockAdjustmentToFAR (key )}>
        <button type="submit" .btn.btn-danger>Save To FrontAccounting
    <form role=form method=post action=@{WarehouseR $ WHStockAdjustmentRejectR (key )}>
      <button type="submit" .btn.btn-warning>Reject 
            |]
  
-- | Save the required adjustments/transfer to FrontAccounting.
-- Everything is done within the same transaction even though we can be posting more than
-- one transactions. if on fail, everything will be rollbacked even though previous transcation
-- in FA might persists. They might needs to be cleaned manually
{-# NOINLINE postWHStockAdjustmentToFAR #-}
postWHStockAdjustmentToFAR ::  Int64 -> Handler Html
postWHStockAdjustmentToFAR key = do
  setting <- appSettings <$> getYesod
  let mainLocationLoc = Just (FA.LocationKey $ appFADefaultLocation setting)
      lostLoc = Just (FA.LocationKey $ appFALostLocation setting)
      baseref = "FamesAdj#" <> tshow key
  date <- utctDay <$> liftIO getCurrentTime

  (details, adj) <- runDB $ loadAdjustment key
  when (stockAdjustmentStatus adj == Process) $ do
    setError "The adjustment has already been processed. It can't be processed twice."
    sendResponseStatus (toEnum 412) =<< getWHStockAdjustmentViewR key
  carts <- adjustCarts date $ splitDetails mainLocationLoc lostLoc details
  let Carts{..} = detailsToCartFA (appFADefaultLocation setting) (appFALostLocation setting)
                                  baseref date carts
  let connectInfo = WFA.FAConnectInfo (appFAURL setting) (appFAUser setting) (appFAPassword setting)
  err <- runExceptT  $ liftM3 (,,)
    (postStockAdjustmentToFA connectInfo cNew)
    (postLocationTransferToFA connectInfo cFound)
    (postLocationTransferToFA connectInfo cLost)
  case err of
    Left err1 -> setError (toHtml err1)  >> getWHStockAdjustmentViewR key
    Right (adjId, t1, t2  ) -> do
      runDB $ do
        update (StockAdjustmentKey $ SqlBackendKey key) [StockAdjustmentStatus =. Process]
        insertMany_ [ TransactionMap faType faId StockAdjustmentE (fromIntegral key) False
                    | (faId, faType) <- catMaybes [ (, ST_INVADJUST) <$> adjId
                                                  , (, ST_LOCTRANSFER) <$> t1
                                                  , (, ST_LOCTRANSFER) <$> t2
                                                  ]
                    ]
       
      setSuccess "Stock adjustments have been processed sucessfully"
      getWHStockAdjustmentR

  
{-# NOINLINE postWHStockAdjustmentRejectR #-}
postWHStockAdjustmentRejectR :: Int64 -> Handler Html
postWHStockAdjustmentRejectR adjId = do
  rejectStockAdj adjId
  renderStockAdjustment



badgeSpan' :: Int -> Maybe Text -> Text -> Widget
badgeSpan' qty bgM klass = toWidget $ badgeSpan badgeWidth qty bgM klass

preToOriginal modulo pre = (OriginalQuantities qtake (qoh-beforeAll) qlost modulo , before) where
  m = mainLocation pre
  qtake = quantityTake0 m
  qoh = quantityAt m
  qlost = quantityNow (lost pre)
  day = takeDate pre
    -- Move picked before the stock take have been taken into account
    -- in the QOH. We need to remove them to get the quantity excluding ALL moves
  move'befores = detectBefores day (preComment pre) (movesAt m)
  (befores, afters) = partition (\(_,bef) -> bef) move'befores
  sumQty m'b = sum (map movePickedQty $ fst $ unzip m'b)
  before = sumQty befores
  beforeAll = before + sumQty afters
  -- -| We want to pass the original quantities (without move) to the data- in html
  -- however the badges needs to be computed as if
  -- the "before" select boxes needs have been selected
toOrigAndBadges :: Maybe Int -> PreAdjust -> (OriginalQuantities, BadgeQuantities, Int)
toOrigAndBadges modulo pre = (orig, computeBadges orig {qoh = qoh orig + before}, before)
  where (orig, before) = preToOriginal modulo pre

-- * To FA 

splitDetails :: Maybe FA.LocationId -> Maybe FA.LocationId -> [StockAdjustmentDetail] -> DetailCarts
splitDetails mainLocationLoc lostLoc details = let
  cLost = filter ((== mainLocationLoc) . stockAdjustmentDetailFrom ) details
  cFound = filter ((== lostLoc) . stockAdjustmentDetailFrom ) details
  cNew = filter ((== Nothing) . stockAdjustmentDetailFrom ) details
  in Carts{..}
  
detailsToCartFA :: Text -> Text ->  Text -> Day -> DetailCarts' -> FACarts
detailsToCartFA mainLoc lostLoc ref date (Carts news losts founds) = let
  new = WFA.StockAdjustment (ref<> "-new")
                        mainLoc
                        date
                        [ WFA.StockAdjustmentDetail (stockAdjustmentDetailStockId d)
                                                    (fromIntegral $ qty)
                                                    cost
                        | (d, cost) <- news
                        , let qty = stockAdjustmentDetailQuantity d
                        , qty > 0
                        ]
                        WFA.PositiveAdjustment

  lost = WFA.LocationTransfer (ref<> "-lost")
                        mainLoc lostLoc
                        date
                        [ WFA.LocationTransferDetail (stockAdjustmentDetailStockId d)
                                                     qty
                        | (d, qty) <- losts
                        , qty > 0
                        ]
  found = WFA.LocationTransfer (ref<> "-found")
                        lostLoc mainLoc
                        date
                        [ WFA.LocationTransferDetail (stockAdjustmentDetailStockId d)
                                                     qty
                        | (d, qty) <- founds
                        , qty > 0
                        ]

  in Carts new lost found
  
-- | retrieve the cost price from FrontAccounting needed to generated a stock adjustment. 
-- We just lookup in the stock_master table. The standard cost in FA doesn't depends on the date ...
findCostPrice :: StockAdjustmentDetail -> Handler (StockAdjustmentDetail, Double)
findCostPrice detail = do
  infoE <- runDB $ get ( FA.StockMasterKey (stockAdjustmentDetailStockId detail))
  return $ case infoE of
             Just info -> (detail, FA.stockMasterMaterialCost info
                                   + FA.stockMasterLabourCost info
                                   + FA.stockMasterOverheadCost info)
             Nothing -> (detail, 0)

adjustCarts :: Day -> DetailCarts -> Handler DetailCarts'
adjustCarts date Carts{..} = do
  new <- mapM findCostPrice cNew
  lost <- mapM (findMaxQuantity date) cLost
  found <- mapM (findMaxQuantity date) cFound
  return $ Carts new lost found

-- | Cap quantity to transfer according to quantity on hand so that
-- we don't generate negative stock.
-- As transfer can be done in the past, we need to make sure
-- that it doesn't result in a negative stock (at the transfer date)
-- or today. This can happen if an  item has been found and delivered between the stocktake/transfer and the current day
findMaxQuantity :: Day -> StockAdjustmentDetail -> Handler (StockAdjustmentDetail, Int)
findMaxQuantity date detail = do
  case stockAdjustmentDetailFrom detail of
    Nothing -> return (detail, 0)
    Just loc -> do
      let sku = stockAdjustmentDetailStockId detail
          qty = stockAdjustmentDetailQuantity detail
      locInfo <- quantitiesFor (FA.unLocationKey loc) (Nothing, Nothing)  (Single sku, Single 0, Single date, Single Nothing)

      
      -- to generate negative quantities, we can move more than the qoh at date
      -- as well as qoh now. qoh after the moves would be qoh-adj and should be >= 0
      let qmax = minimum $ impureNonNull [qty , quantityAt locInfo, quantityNow locInfo]
      return (detail, qmax)

-- | Post a stock adjusmtent to FrontAccounting and update
-- Fames db accordingly.
-- postStockAdjustmentToFA :: WFA.StockAdjustment -> ExceptT Text IO (Maybe Int)
postStockAdjustmentToFA :: WFA.FAConnectInfo -> WFA.StockAdjustment -> ExceptT Text Handler (Maybe Int)
postStockAdjustmentToFA connectInfo adj = do
  if null (WFA.adjDetails adj)
    then return Nothing
    else do
      adjId <- ExceptT . liftIO $ WFA.postStockAdjustment connectInfo adj
      setSuccess . toHtml $ "Adjustment#" <> tshow adjId <> " has been created successfully"
      return (Just adjId)

  

-- | Post a location transfer to FrontAccounting and update
-- Fames db accordingly.
-- postLocationTransferToFA :: WFA.LocationTransfer -> ExceptT Text IO (Maybe Int)
postLocationTransferToFA :: WFA.FAConnectInfo -> WFA.LocationTransfer -> ExceptT Text Handler (Maybe Int)
postLocationTransferToFA connectInfo trans = do
  if null (WFA.ltrDetails trans)
    then return Nothing
    else do
      transId <- ExceptT . liftIO $ WFA.postLocationTransfer connectInfo trans
      setSuccess . toHtml $ "Transfer#" <> tshow transId <> " has been created successfully"
      return (Just transId)

