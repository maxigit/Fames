module Handler.WH.Boxtake.Adjustment
where
import Import hiding(Planner)
import Database.Persist.Sql(Single(..), rawSql, unSqlBackendKey)
import qualified Data.Map as Map
import Data.Align
import Handler.WH.Boxtake.Common
import Handler.Items.Common
import Data.List(mapAccumL, tails, inits)
import Lens.Micro.Extras (preview)

type BoxtakePlus = (Entity Boxtake , [Entity Stocktake])
type StocktakePlus = (Entity Stocktake, Key Boxtake)
-- * types
-- | All information regarging a style, QOH, boxtakes stocktakes etc.
data StyleInfo = StyleInfo
   { siQoh :: Map Text Double
   , siBoxtakes :: [BoxtakePlus]
   } deriving (Show)
instance Semigroup StyleInfo where
   (StyleInfo a b) <>  (StyleInfo a' b') = StyleInfo (unionWith (+) a a') (b <> b')
  
instance Monoid StyleInfo where
  mempty = StyleInfo mempty mempty
  mappend = (<>)

data UsedStatus e = Used Double e | Unused e
  deriving (Show,Eq,Ord)
usedSubject :: UsedStatus e -> e
usedSubject (Used _ e) = e 
usedSubject (Unused e) = e 
usedQuantity :: UsedStatus e -> Maybe Double
usedQuantity (Used q _) = Just q 
usedQuantity (Unused e) = Nothing 
isUsed :: UsedStatus e -> Bool
isUsed (Used _ _) = True
isUsed (Unused _) = False

-- | Result of computation
data StyleInfoSummary = StyleInfoSummary
   { ssSku :: Maybe Text
   , ssQoh :: Double
   , ssQUsed :: Double
   , ssBoxes :: [(UsedStatus BoxtakePlus)] 
   }
-- | What should happend to a box
data BoxStatus
  = BoxUsed -- ^ Active and used. nothing to do
  | BoxToActivate -- ^ used but inactive. Needs activation
  | BoxToDeactivate -- ^ unused but active. Need deactivation.
  | BoxInactive -- ^ don't display
  deriving (Eq, Show)
-- * 

-- | Compute the box status of each boxes
computeInfoSummary :: StyleInfo -> [StyleInfoSummary]
computeInfoSummary StyleInfo{..} =
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
  let p's'bs = concatMap computeStocktakePriority siBoxtakes
      stocktakesSorted = map snd $ sortOn fst p's'bs
      (_qohLeft, stocktakesWithQ ) = mapAccumL assignQuantityToStocktake siQoh stocktakesSorted
      usedBoxes = usedStocktakeToBoxes (map fst siBoxtakes) stocktakesWithQ
      -- used boxes may contain the same box many times
      -- if it contains many colours
      -- we need to group box by colours
      -- and find w
      boxBySku = groupAsMap fst (return . snd) usedBoxes :: Map Text [UsedStatus BoxtakePlus]
      skuInfo = align boxBySku siQoh
  in map (uncurry boxInfoToSummary) $ mapToList skuInfo

boxInfoToSummary :: Text -> These [UsedStatus BoxtakePlus]  Double -> StyleInfoSummary
boxInfoToSummary style t = let
  qoh = fromMaybe 0 $ preview there t 
  boxes = fromMaybe [] $ preview here t
  qused = sum $ mapMaybe usedQuantity boxes
  in StyleInfoSummary (Just style)  qoh qused boxes

  
   

  

type StocktakePrioriry = (Down Bool, Down Day, Down Int, (Text, Text))
--  USE location
computeStocktakePriority :: BoxtakePlus -> [(StocktakePrioriry, StocktakePlus)]
computeStocktakePriority (Entity bId Boxtake{..}, stocktakes) = do
  s@(Entity _ Stocktake{..}) <- stocktakes
  let priority = ( Down $ boxtakeActive || stocktakeActive
                 , Down (max stocktakeDate boxtakeDate) -- older more likely to be picked
                 , Down stocktakeQuantity -- The more, the less we pick from it
                 , locationPriority boxtakeLocation
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
assignQuantityToStocktake :: (Map Text Double) -> StocktakePlus -> (Map Text Double, UsedStatus StocktakePlus)
assignQuantityToStocktake qohs s@(Entity _ Stocktake{..}, bId) =
  case lookup stocktakeStockId qohs of
    Just q | let used = min q $ fromIntegral stocktakeQuantity 
           , let leftover = q - used
           , used /= 0
           -> (insertMap stocktakeStockId leftover qohs, (Used used s))
    _ -> (qohs, (Unused s))


-- Reverse 
usedStocktakeToBoxes :: [Entity Boxtake] -> [UsedStatus StocktakePlus] -> [(Text, UsedStatus BoxtakePlus)]
usedStocktakeToBoxes boxes stocktakes = let
  boxByBoxId = mapFromList $ map (fanl entityKey) boxes
  stockByBoxId = groupAsMap (snd  . usedSubject) (:[]) stocktakes
  boxStock = alignWith goUsedStocktakeToBoxes boxByBoxId stockByBoxId
  in join $ toList boxStock 

-- | Might return the same boxes many times
goUsedStocktakeToBoxes  :: These (Entity Boxtake) [UsedStatus StocktakePlus] -> [(Text, UsedStatus BoxtakePlus)] -- UsedStatus BoxtakePlus
goUsedStocktakeToBoxes t = case t of
  This boxe -> unused boxe
  That stocktakes -> error "Shouldn't happen" -- all stockake belong intialy to a box
  These boxe stocktakes -> case filter isUsed stocktakes of
                             [] -> unused boxe
                             useds -> do
                               -- scan all item and get what's i
                               (Used q splus: t, i) <- zip <$> tails <*> inits $ useds
                               let s = fst splus
                               return ( stocktakeStockId $ entityVal s
                                      , Used q (boxe, s: map ( fst. usedSubject) (i<>t))
                                      )
  where unused b = [(fromMaybe "" $ boxtakeDescription (entityVal b), Unused (b, []))]



-- | Parameter needed to process boxtake adjustment
data AdjustmentParam = AdjustmentParam
  { aStyleFilter :: Maybe FilterExpression
  , aLocation :: Text
  , aSkipOk :: Bool -- ^ don't show things with nothing to do
  , aShowDetails :: Bool -- ^ don't show boxes. 
  , aStyleSummary :: Bool -- ^ display summary for styles vs variations
  } 

defaultAdjustmentParamH = do
  defaultLocation <- appFADefaultLocation <$> getsYesod appSettings 
  return $ AdjustmentParam Nothing defaultLocation True False True
-- * Render
-- * DB
-- | Load all boxes needed to display and compute adjustment
--  style filter filter a style (not a particular variation)
-- because boxes can be boxtaken without specifying the variation
-- select active and inactive boxes
loadBoxForAdjustment :: AdjustmentParam -> SqlHandler (Map Text [(Entity Boxtake, [Entity Stocktake])])
loadBoxForAdjustment param = do
  let filter = filterE Just BoxtakeDescription (filterEAddWildcardRight <$> aStyleFilter param)
  skuToStyleVar <- lift skuToStyleVarH 
  let descrToStyle sku = let cleaned = fromMaybe sku  (stripSuffix "*" sku)
                             (style, _) = skuToStyleVar cleaned
                         in style
                         
  boxtakes <- selectList filter  [Asc BoxtakeDescription, Desc BoxtakeActive, Desc BoxtakeDate]
  withStocktake <- loadStocktakes' boxtakes
  let key = maybe "" descrToStyle . boxtakeDescription . entityVal . fst
  return $ groupAscAsMap key (:[]) withStocktake


loadQohForAdjustment :: AdjustmentParam -> SqlHandler (Map Text [(Text, Double)])
loadQohForAdjustment param = do
  let defaultLocation = aLocation param
  -- join denorm table with category: style
  let sql = " SELECT value as style, stock_id, quantity "
         <> " FROM fames_item_category_cache "
         <> " JOIN 0_denorm_qoh USING (stock_id)"
         <> " WHERE loc_code = ? AND category = 'style' AND quantity != 0 "
      orderBy = " ORDER BY style, stock_id DESC"
      (w,p) = case filterEKeyword <$> aStyleFilter param of
        Nothing -> ("",  [] )
        Just (keyword, v) -> (" AND value " <> keyword <> " ? " , [toPersistValue v])
      convert :: (Single Text, Single (Text), Single Double) -> (Text, (Text, Double))
      convert (Single style, Single var, Single quantity) = (style, (var, quantity))
  raw <- rawSql (sql <>  w <> orderBy) (toPersistValue defaultLocation :p)
  return $ groupAscAsMap fst (return . snd) (map convert raw)
  

loadAdjustementInfo :: AdjustmentParam -> SqlHandler (Map Text StyleInfo)
loadAdjustementInfo param = do
  boxGroups <- loadBoxForAdjustment param
  qs <- loadQohForAdjustment param
  return $ malign (fmap (flip StyleInfo mempty . Map.fromAscList) qs)
                  (fmap (StyleInfo mempty) boxGroups)

  
  

-- | Fetch boxtake and their status according to FA Stock
-- and display it so that it can be processed
displayBoxtakeAdjustments :: AdjustmentParam -> Handler Widget
displayBoxtakeAdjustments param@AdjustmentParam{..}  = do
  infos <- runDB $ loadAdjustementInfo  param
  let summaries0 = toList infos >>= computeInfoSummary
      -- only keep nono zero style
      summaries = filter toDisplay summaries0
      toDisplay StyleInfoSummary{..} = if aSkipOk 
        then let -- check if there is problem
                    leftOver = ssQoh - ssQUsed 
                    boxStatuses = map boxStatus ssBoxes 
            in leftOver > 0 || any (/= BoxUsed) boxStatuses
        else ssQoh /= 0 || not (null $ mapMaybe classForBox ssBoxes)
  return [whamlet|
  <table.table.table-border.table-hover>
    $forall s <- summaries
     <tbody.summary-group :aShowDetails:.with-details>
      <tr.summary-row>
        $if aShowDetails
          <td><input type="checkbox" checked>
        <td colspan=2>#{fromMaybe "" $ ssSku s}
        <td.varQuantity>#{formatQuantity (ssQoh s)}
           $with leftOver <- ssQoh s - ssQUsed s
              $if leftOver > 0
                <span.badge>#{formatQuantity leftOver}
        <td colspan=4>
          <div.status-summary>
            $forall statusBox <- ssBoxes s
              ^{displayBoxQuantity statusBox}
      $if aShowDetails 
        ^{forM_ (ssBoxes s) displayBoxRow}
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
  
processBoxtakeAdjustment :: AdjustmentParam -> Handler ()
processBoxtakeAdjustment param = do
  today <- todayH
  (pp, _) <- runRequestBody
  let toDeactivate = [ bId | (p, checked) <- pp, checked=="on", Just bId <- [readMay =<< stripPrefix "deactivate-" p] ]
  let toActivate = [ bId | (p, checked) <- pp, checked=="on", Just bId <- [readMay =<< stripPrefix "activate-" p] ]
  if null toDeactivate && null toActivate
    then setWarning "Nothing to activate or deactivate" 
    else runDB $ do
        forM_ toDeactivate (deactivateBoxtakeByKey today)
        forM_ toActivate (reactivateBoxtake today)
        setSuccess [shamlet|<p>#{length toDeactivate} boxtakes deactivated succsessfuly.
                            <p>#{length toActivate} boxtakes reactivated succsessfuly.
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
|]

adjustmentJS :: Widget
adjustmentJS = toWidget [julius|
$(document).ready(function () {
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
   
  }
  )
  }
)
|]
