module Handler.WH.Boxtake.Adjustment 
( AdjustmentParam(..)
, BoxStatus(..)
, StyleInfoSummary(..)
, adjustmentCSS
, adjustmentJS
, boxStatus
, computeInfoSummary
, UseActiveStatus(..)
, defaultAdjustmentParamH
, displayBoxtakeAdjustments
, loadAdjustementInfo
, loadBoxForAdjustment
, processBoxtakeAdjustment
, processBoxtakeDeactivation
, usedSubject
)
where
import Import hiding(Planner, leftover)
import Database.Persist.MySQL -- (BackendKey(SqlBackendKey))
import Data.Align
import Handler.WH.Boxtake.Common
import Handler.Items.Common
import Data.List(mapAccumL, tails, inits)
import Lens.Micro.Extras (preview)
import Data.These.Lens
import Util.ForConduit
import qualified Data.Conduit.List as C

type BoxtakePlus = (Entity Boxtake , [Entity Stocktake])
type StocktakePlus = (Entity Stocktake, Key Boxtake)
-- * types 
-- | All information regarging a style, QOH, boxtakes stocktakes etc.
data StyleInfo = StyleInfo
   { siQoh :: Map Sku Double
   , siBoxtakes :: [BoxtakePlus]
   } deriving (Show)
instance Semigroup StyleInfo where
   (StyleInfo a b) <>  (StyleInfo a' b') = StyleInfo (unionWith (+) a a') (b <> b')
  
instance Monoid StyleInfo where
  mempty = StyleInfo mempty mempty

data UsedStatus e = Used Double e | Unused e
  deriving (Show,Eq,Ord)
usedSubject :: UsedStatus e -> e
usedSubject (Used _ e) = e 
usedSubject (Unused e) = e 
usedQuantity :: UsedStatus e -> Maybe Double
usedQuantity (Used q _) = Just q 
usedQuantity (Unused _) = Nothing 
isUsed :: UsedStatus e -> Bool
isUsed (Used _ _) = True
isUsed (Unused _) = False

-- | Result of computation
data StyleInfoSummary = StyleInfoSummary
   { ssSku :: Maybe Sku
   , ssQoh :: Double
   , ssQUsed :: Double
   , ssBoxes :: [(UsedStatus BoxtakePlus)] 
   } deriving Show
-- | What should happend to a box
data BoxStatus
  = BoxUsed --  ^ Active and used. nothing to do
  | BoxToActivate --  ^ used but inactive. Needs activation
  | BoxToDeactivate --  ^ unused but active. Need deactivation.
  | BoxInactive --  ^ don't display
  deriving (Eq, Ord, Show)
-- *  

data UseActiveStatus = IgnoreActiveStatus | UseActiveStatus 
     deriving (Show, Eq)

-- | Compute the box status of each boxes
computeInfoSummary :: UseActiveStatus -> StyleInfo -> [StyleInfoSummary]
computeInfoSummary useActiveStatus StyleInfo{..} =
  -- to do so, we consider that
  -- quantity in a box can only go down, ie the
  -- quantity left is smaller than the quantity from the last stocktake (inactive or not)
  -- To detect which boxes are still in used or unused, we need to "fill" The boxes
  -- starting with QOH quantity.
  -- Boxes more likely to be unused should be at the end.
  -- The problem is then equivalent to sort boxes by priority, fill them and
  -- see which one are used or not.
  -- The priority (to pick things , ie the reverse priority)
  -- is inactive box first, then date and finally location type (top shelf are not picked from) and qty
  -- boxes with the less quantity are more likely to be picked from first, especially if they are mixed colours
  -- However, the fact that a box can contains many colours, means than we can just process
  -- all boxes together but needs to sort priority for each colours independently
  let p's'bs = concatMap (computeStocktakePriority useActiveStatus) siBoxtakes
      stocktakesSorted = map snd $ sortOn fst p's'bs
      (_qohLeft, stocktakesWithQ ) = mapAccumL assignQuantityToStocktake siQoh stocktakesSorted
      usedBoxes = usedStocktakeToBoxes (map fst siBoxtakes) stocktakesWithQ
      -- used boxes may contain the same box many times
      -- if it contains many colours
      -- we need to group box by colours
      -- and find w
      boxBySku = groupAsMap fst (return . snd) usedBoxes :: Map Sku [UsedStatus BoxtakePlus]
      skuInfo = align boxBySku siQoh
  in map (uncurry boxInfoToSummary) $ mapToList skuInfo

boxInfoToSummary :: Sku -> These [UsedStatus BoxtakePlus]  Double -> StyleInfoSummary
boxInfoToSummary sku t = let
  qoh = fromMaybe 0 $ preview there t 
  boxes = fromMaybe [] $ preview here t
  qused = sum $ mapMaybe usedQuantity boxes
  in StyleInfoSummary (Just sku)  qoh qused boxes

  
   

  

type StocktakePrioriry = (Down Bool, Down Day, Down Int, Down (Text, Text), Down StocktakeId)
computeStocktakePriority :: UseActiveStatus -> BoxtakePlus -> [(StocktakePrioriry, StocktakePlus)]
computeStocktakePriority useActiveStatus (Entity bId Boxtake{..}, stocktakes) = do
  s@(Entity sId Stocktake{..}) <- stocktakes
  let priority = ( Down $ useActiveStatus == IgnoreActiveStatus || boxtakeActive --    || stocktakeActive
                 , Down boxtakeDate -- older more likely to be picked
                 --     ^ we use the boxtake date and not the stocktake
                 --       because the boxtakeDate can be more recent if the box has been scanned
                 --       without a full stocktake
                 , Down stocktakeQuantity -- The more, the less we pick from it
                 , Down $ locationPriority boxtakeLocation
                 , Down $ sId -- in case of same date, use last created first. Make sure
                 -- the order is stable, across time.
                 )
  return (priority, (s, bId) )

-- priority in which item are likely to be picked
-- Item are more like to be picked from a shelf with an alphabetically low
-- name (as it will be physically before)
-- However, this is not true for the level, which should be take into account
-- last, so that top shelves are not picked from
locationPriority :: Text -> (Text, Text)
locationPriority location = break (== '/') location

-- | Assign and use the quantity for a given stock take
assignQuantityToStocktake :: (Map Sku Double) -> StocktakePlus -> (Map Sku Double, UsedStatus StocktakePlus)
assignQuantityToStocktake qohs s@(Entity _ Stocktake{..}, __bId) =
  case lookup (Sku stocktakeStockId) qohs of
    Just q | let used = min q $ fromIntegral stocktakeQuantity 
           , let leftover = q - used
           , used /= 0
           -> (insertMap (Sku stocktakeStockId) leftover qohs, (Used used s))
    _ -> (qohs, (Unused s))


-- Reverse 
usedStocktakeToBoxes :: [Entity Boxtake] -> [UsedStatus StocktakePlus] -> [(Sku, UsedStatus BoxtakePlus)]
usedStocktakeToBoxes boxes stocktakes = let
  boxByBoxId = mapFromList $ map (fanl entityKey) boxes
  stockByBoxId = groupAsMap (snd  . usedSubject) (:[]) stocktakes
  boxStock = alignWith goUsedStocktakeToBoxes boxByBoxId stockByBoxId
  in join $ toList boxStock 

-- | Might return the same boxes many times
goUsedStocktakeToBoxes  :: These (Entity Boxtake) [UsedStatus StocktakePlus] -> [(Sku, UsedStatus BoxtakePlus)] -- UsedStatus BoxtakePlus
goUsedStocktakeToBoxes t = case t of
  This boxe -> unused boxe
  That __stocktakes -> error "Shouldn't happen" -- all stockake belong intialy to a box
  These boxe stocktakes -> case filter isUsed stocktakes of
                             [] -> unused boxe
                             useds -> do
                               -- scan all item and get what's i
                               (Used q splus: t_, i) <- zip <$> tails <*> inits $ useds
                               let s = fst splus
                               return ( Sku $ stocktakeStockId $ entityVal s
                                      , Used q (boxe, s: map ( fst. usedSubject) (i<>t_))
                                      )
  where unused b = [(Sku $ fromMaybe "" $ boxtakeDescription (entityVal b), Unused (b, []))]


aggregateInfoSummaries :: Sku -> [StyleInfoSummary] -> StyleInfoSummary
aggregateInfoSummaries sku ss = 
  StyleInfoSummary (Just sku)
                   (sum $ map ssQoh ss)
                   (sum $ map ssQUsed ss)
                   (sortOn boxStatus . join $ map ssBoxes ss)
                     

-- | Parameter needed to process boxtake adjustment
data AdjustmentParam = AdjustmentParam
  { aStyleFilter :: Maybe FilterExpression
  , aLocation :: Text
  , aSkipOk :: Bool -- ^ don't show things with nothing to do
  , aShowDetails :: Bool -- ^ don't show boxes. 
  , aUseBoxStatus :: Bool -- ^ use or ignore box active status
  , aStyleSummary :: Bool -- ^ display summary for styles vs variations
  , aDate :: Maybe Day -- ^ stock date
  } 

defaultAdjustmentParamH :: Handler AdjustmentParam
defaultAdjustmentParamH = do
  defaultLocation <- appFADefaultLocation <$> getsYesod appSettings 
  return $ AdjustmentParam Nothing defaultLocation True False True True Nothing
-- * Render 
-- * DB 
-- | Load all boxes needed to display and compute adjustment
--  style filter filter a style (not a particular variation)
-- because boxes can be boxtaken without specifying the variation
-- select active and inactive boxes
loadBoxForAdjustment :: AdjustmentParam -> SqlConduit () (ForMap Style [(Entity Boxtake, [Entity Stocktake])]) ()
loadBoxForAdjustment param = do
  let filter_ = filterE Just BoxtakeDescription (filterEAddWildcardRight <$> aStyleFilter param)
  skuToStyleVar <- lift $ lift skuToStyleVarH 
  let descrToStyle sku = let cleaned = fst  $ break (`elem` ("&*" :: [Char])) sku
                             (style, _) = skuToStyleVar $ Sku cleaned
                         in style
      getStyle = maybe (Style "") descrToStyle . boxtakeDescription . entityVal
                         
      boxtakeSource = selectSource filter_  [Asc BoxtakeDescription, Desc BoxtakeActive, Desc BoxtakeDate]

  boxtakeSource .| loadStocktakes'
                .| mapC unForMap
                .| C.groupOn1 (getStyle . fst)
                .| mapC \(x@(boxtake,_) , xs) -> ForMap (getStyle boxtake) (x: xs)


loadQohForAdjustment :: AdjustmentParam -> SqlConduit () (ForMap Style [(Sku, Double)]) ()
loadQohForAdjustment param = 
  let defaultLocation = aLocation param
      orderBy = " ORDER BY style, stock_id DESC"
      (sql, p) = case aDate param of
        Nothing -> 
          -- join denorm table with category: style
          let sql = " SELECT value as style, stock_id, quantity "
                 <> " FROM fames_item_category_cache "
                 <> " JOIN 0_denorm_qoh USING (stock_id)"
                 <> " WHERE loc_code = ? AND category = 'style' AND quantity != 0 "
              (w,p) = case filterEKeyword <$> aStyleFilter param of
                Nothing -> ("",  [] )
                Just (keyword, v) -> (" AND value " <> keyword, v)
           in (sql <> w, p) 
        Just today ->  
          -- use stock moves table, slower
          let sql = " SELECT value as style, stock_id, sum(qty) quantity "
                 <> " FROM fames_item_category_cache "
                 <> " JOIN 0_stock_moves USING (stock_id)"
                 <> " WHERE loc_code = ? AND category = 'style' "
              after = " AND tran_date <= ? "
                    <> " GROUP BY stock_id HAVING quantity != 0 "
              (w,p) = case filterEKeyword <$> aStyleFilter param of
                Nothing -> ("",  [] )
                Just (keyword, v) -> (" AND value " <> keyword, v)
          in (sql <> w <> after, p ++ [toPersistValue today] )
      convert :: (Single Text, Single (Text), Single Double) -> (Style, (Sku, Double))
      convert (Single style, Single sku, Single quantity) = (Style style, (Sku sku, quantity))
  in ( rawQuery (sql <> orderBy) (toPersistValue defaultLocation :p))
           .| mapC (either (error . unpack ) convert . rawSqlProcessRow)
           .| C.groupOn1 fst
           .| mapC \((style,x), st'xs) -> ForMap style (x: map snd st'xs)
  

loadAdjustementInfo :: AdjustmentParam -> SqlConduit () (ForMap Style StyleInfo) ()
loadAdjustementInfo param = do
  joinOnWith forMapKey forMapKey  mkInfo (loadBoxForAdjustment param) (loadQohForAdjustment param)
  where mkInfo (ForMap style boxes) qohs =
               let sku'qohs = concat [ sku'qoh | ForMap _ sku'qoh <- qohs ]
               in ForMap style $ StyleInfo (mapFromList sku'qohs) boxes
  

-- | Fetch boxtake and their status according to FA Stock
-- and display it so that it can be processed
displayBoxtakeAdjustments :: AdjustmentParam -> Handler Widget
displayBoxtakeAdjustments param@AdjustmentParam{..}  = do
  infos <- runDB $ runConduit $ loadAdjustementInfo  param .| mapC unForMap .| sinkList
  let summaries0 = if aStyleSummary
        then [ aggregateInfoSummaries (Sku style) (computeInfoSummary useBoxStatus styleInfo) | (Style style, styleInfo) <- infos ]
        else map snd infos >>= computeInfoSummary useBoxStatus
      useBoxStatus = if aUseBoxStatus then UseActiveStatus else IgnoreActiveStatus
      -- only keep nono zero style
      summaries = filter toDisplay summaries0
      toDisplay StyleInfoSummary{..} = if aSkipOk 
        then let -- check if there is problem
                    leftOver = ssQoh - ssQUsed 
                    boxStatuses = map boxStatus ssBoxes 
            in leftOver > 0 || any (`elem` [BoxToActivate, BoxToDeactivate]) boxStatuses
        else ssQoh /= 0 || not (null $ mapMaybe classForBox ssBoxes)
        -- create a link to drilldown 
      decorateSku (Sku sku) = case aStyleSummary of
             -- style => drilldown
             True -> [whamlet|
                             <a href="@{WarehouseR (WHBoxtakeAdjustmentForR sku aSkipOk aDate)}"
                                target=_blank
                             >#{sku}
                     |]
             -- sku => link to item stocktake history
             False -> let
               in [whamlet|
                   <a href="@?{(WarehouseR WHStocktakeR, [("stock_id", sku)])}" > #{sku}
                              |]
  return [whamlet|
  <table.table.table-border.table-hover>
    $forall s <- summaries
     ^{xxx param decorateSku (decorateQuantity param) s}
     |]

decorateQuantity :: AdjustmentParam -> Maybe Sku -> String -> Widget
decorateQuantity _ Nothing qw = toWidget $ toHtml qw
decorateQuantity AdjustmentParam{..} (Just (Sku sku)) qw =
  case aStyleSummary of
      -- style => stocktake to item history, allow to go the item history for the given sku
      True -> [whamlet|
                      <a href="@{WarehouseR (WHStocktakeHistoryStyleR Nothing sku)}"
                          target=_blank
                        >#{qw}
                      |]
      -- sku -- item history
      False -> [whamlet|
                        <a href="@{ItemsR (ItemsHistoryR sku)}"
                          target=_blank
                        >#{qw}
                      |]
xxx :: AdjustmentParam
    -> (Sku -> Widget)
    -> _ -- (Maybe Text -> _ -> Widget)
    -> StyleInfoSummary
    -> Widget
xxx AdjustmentParam{..} decorateSku _decorateQuantity StyleInfoSummary{..} =
  [whamlet|
    <tbody.summary-group :aShowDetails:.with-details>
      <tr.summary-row>
        $if aShowDetails
          <td.checkboxColumn><input type="checkbox" checked>
        <td.styleColumn colspan=2>^{maybe mempty decorateSku ssSku }
        <td.varQuantity>^{_decorateQuantity ssSku (formatQuantity ssQoh)}
           $with leftOver <- ssQoh - ssQUsed
              $if leftOver > 0
                <span.badge>#{formatQuantity leftOver}
        <td colspan=4>
          <div.status-summary>
            $forall statusBox <- sortOn boxStatus ssBoxes
                  ^{displayBoxQuantity statusBox}
      $if aShowDetails 
        ^{forM_ ssBoxes displayBoxRow}
|]


displayBoxQuantity :: UsedStatus BoxtakePlus -> Widget
displayBoxQuantity status = forM_ (classForBox status) $ \klass ->  do
  [whamlet|
    <span class="#{klass}">
      <span.badge>#{maybe "∅" formatQuantity (usedQuantity status)}
      |]

displayBoxRow :: UsedStatus BoxtakePlus -> Widget
displayBoxRow status = forM_ (classForBox status) $ \klass -> do
  let (Entity _bId box@Boxtake{..}, stocktakees) = usedSubject status
      multi = not . null $ drop 1 stocktakees 
  [whamlet|
  <tr.box-row class=#{klass} :multi:.multi>
    <td>#{checkBoxForRow status}
    <td>^{dimensionPicture 48 box}
    <td><a href="@{WarehouseR (WHBoxtakeDetailR boxtakeBarcode)}" target=_blank> #{boxtakeBarcode}
    <td.boxQuantity>
      ^{displayBoxQuantity status}
    <td.boxDescription>#{fromMaybe "" boxtakeDescription}
    <td>#{tshow boxtakeDate}
    <td>#{boxtakeLocation}
          |]
  
-- | Depending on the status of a box
-- We need to either activate or not.
checkBoxForRow :: UsedStatus BoxtakePlus -> Html
checkBoxForRow status = case boxStatus status of
  BoxToActivate -> [shamlet|
                             <span.badge>
                               <input type="checkbox" name="activate-#{boxId}" checked>
                           |]
  BoxToDeactivate -> -- only deactivate if the box is not shared
    case snd (usedSubject  status ) of
      [] -> [shamlet|<span.badge>
                        <input type="checkbox" name="deactivate-#{boxId}" checked>|]
      _ -> [shamlet|<span.badge>
                   <input type="checkbox" disabled>
      -- _ -> [shamlet|<span.glyphicon.glyphicon-ban-circle>|]
  BoxUsed -> [shamlet|
                             <span.badge>
                               <input type="checkbox" name="used-#{boxId}" checked>
                           |]
  _ -> ""
  where boxId = tshow . unSqlBackendKey . unBoxtakeKey . entityKey . fst  $ usedSubject status




boxStatus :: UsedStatus BoxtakePlus -> BoxStatus
boxStatus b = case (b, boxtakeActive . entityVal . fst $ usedSubject b) of
  (Used _ _, True) -> BoxUsed
  (Used _ _, False) -> BoxToActivate
  (Unused _, True) -> BoxToDeactivate
  (Unused _, False) -> BoxInactive
    
classForBox :: UsedStatus BoxtakePlus -> Maybe Text
classForBox b = case boxStatus b of
  BoxInactive -> Nothing
  status -> Just (tshow status)

-- * Adjustment 
-- | Inactivate and reactivate the given boxes
  
processBoxtakeAdjustment :: Handler ()
processBoxtakeAdjustment = do
  today <- todayH
  (pp, _) <- runRequestBody
  let toDeactivate = extractBoxIdFromParam "deactivate-" pp
  let toActivate = extractBoxIdFromParam "activate-" pp
  if null toDeactivate && null toActivate
    then setWarning "Nothing to activate or deactivate" 
    else runDB $ do
        forM_ toDeactivate (deactivateBoxtakeByKey today)
        forM_ toActivate (reactivateBoxtakeByKey today)
        setSuccess [shamlet|<p>#{length toDeactivate} boxtakes deactivated succsessfuly.
                            <p>#{length toActivate} boxtakes reactivated succsessfuly.
                           |]

extractBoxIdFromParam :: Text -> [(Text, Text)] -> [Key Boxtake]
extractBoxIdFromParam prefix pp =
  let bids = [ bId | (p, checked) <- pp, checked=="on", Just bId <- [readMay =<< stripPrefix prefix p] ] :: [Int64]
  in map (BoxtakeKey . SqlBackendKey) bids
-- * Deactivate
-- | Force deactivation of selected boxes.
-- Usefull to reactive newer ones if older boxes
-- are active by mistake
processBoxtakeDeactivation :: Handler ()
processBoxtakeDeactivation = do
  today <- todayH
  (pp, _) <- runRequestBody
  let toDeactivate = extractBoxIdFromParam "deactivate-" pp 
                   <> extractBoxIdFromParam "used-" pp
  runDB $ forM_ toDeactivate (deactivateBoxtakeByKey today)
  setSuccess [shamlet|<p>#{length toDeactivate} boxtakes deactivated succsessfuly.
                     |]

                            

-- * Css 
adjustmentCSS :: Widget
adjustmentCSS =toWidget [cassius|
.multi td.boxDescription
  color: #{redBadgeBg}
.badge.badge-danger
  background: #{redBadgeBg}
  a
    color: white
.BoxToDeactivate
  span.badge
    background: #{redBadgeBg}
.BoxToActivate
  span.badge
    background: #{blueBadgeBg}
.BoxUsed
  span.badge
    background: #{grayBadgeBg}
  
td.varQuantity .badge
    background: #{amberBadgeBg}
tbody.summary-group.with-details
  tr.box-row
    display: none
  &:hover
    tr.box-row
      display: table-row
      border-left: 1px solid #{blueBadgeBg}
    div.status-summary
      display: none
    tr:last-child
        border-bottom: 2px solid #{blueBadgeBg}
    tr:first-child
        border-top: 2px solid #{blueBadgeBg}
    tr.summary-row
      font-weight: 500
      background: #{paleBlue}
      border-left: 1px solid #{blueBadgeBg}
      border-bottom: 2px black solid
      td
        border-top: 1px black solid
td.checkboxColumn
  width:  5%
td.styleColumn
  width:  20%
td.varQuantity
  width:  10%
|]

adjustmentJS :: Widget
adjustmentJS = toWidget [julius|
$(document).ready(function () {
  // toggle all children
  $('tr.summary-row input[type=checkbox]').change(function() {
     var tbody = $(this).parents('tbody');
     var inputs = $(tbody).find('tr.box-row input[type=checkbox]');
     $(inputs).prop('checked', $(this).prop('checked'))
  })

  $('tr.box-row input[type=checkbox]').change(function() {
     // update parent
     var tbody = $(this).parents('tbody');
     var inputs = $(tbody).find('tr.box-row input[type=checkbox]');
     var checked = this.checked;
     // is any checked ?
     $.each(inputs, function(i,r) {
        if(r.checked) { checked = true; return !checked;}
        else {return true;}
     })
     $(tbody).find('tr.summary-row input[type=checkbox]').prop('checked',checked)
     // update toggle-all
     // uncheck if all summary are  unchecked
     var toggleAll = $('input#toggle-all');
     var form = $(this).parents('form');
     var summaries = $(form).find('tr.box-row input[type=checkbox]')
     checked = false;
     $.each(summaries, function(i,r) {
       if (r.checked) {checked = true; return !checked;}
       else {return true;}
     })
     toggleAll.prop('checked', checked)
   
  })
  // toggle everything
  $('input#toggle-all').change(function() {
     var form = $(this).parents('form');
     var inputs = $(form).find('tr input[type=checkbox]')
     $(inputs).prop('checked', this.checked);
  })
  }
)
|]
