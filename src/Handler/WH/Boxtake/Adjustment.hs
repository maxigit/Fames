module Handler.WH.Boxtake.Adjustment
where
import Import hiding(Planner)
import Database.Persist.Sql(Single(..), rawSql)
import qualified Data.Map as Map
import Data.Align
import Handler.WH.Boxtake.Common
import Handler.Items.Common

-- * types
-- | All information regarging a style, QOH, boxtakes stocktakes etc.
data StyleInfo = StyleInfo
   { siQoh :: Map (Maybe Text) Double
   , siBoxtakes :: [(Entity Boxtake, [Entity Stocktake])]
   } deriving (Show)
instance Semigroup StyleInfo where
   (StyleInfo a b) <>  (StyleInfo a' b') = StyleInfo (unionWith (+) a a') (b <> b')
  
instance Monoid StyleInfo where
  mempty = StyleInfo mempty mempty
  mappend = (<>)

data BoxStatus = BoxStatus
  deriving (Show,Eq,Ord)

-- | Result of computation
data StyleInfoSummary = StyleInfoSummary
   { ssSku :: Maybe Text
   , ssQoh :: Double
   , ssBoxes :: [(BoxStatus, Entity Boxtake, Maybe Double )] 
   }
-- * 

perBox = 6 -- TODO change
computeInfoSummary :: StyleInfo -> [StyleInfoSummary]
computeInfoSummary StyleInfo{..} = do
  (var, qoh) <- Map.toList siQoh
  -- doesn't work for boxes with multiple 
  let boxWithQty = filter ((/=0) . fst) $ map (fanl (boxSkuContent var)) siBoxtakes
      status (q,(b, _)) = (BoxStatus, b, Just $ fromIntegral q)
  return $ StyleInfoSummary var qoh (map status boxWithQty)
      


boxSkuContent :: Maybe Text -> (t,  [Entity Stocktake]) -> Int
boxSkuContent Nothing _ = 0
boxSkuContent (Just sku) (_, stocktakes) = -- traceShow("BOx", sku, stocktakes)
  r where
  r = sum . map stocktakeQuantity $ filter ((sku ==) . stocktakeStockId) (map entityVal stocktakes)

  






  



  

  
-- | Parameter needed to process boxtake adjustment
data AdjustmentParam = AdjustmentParam
  { aStyleFilter :: Maybe FilterExpression
  , aLocation :: Text
  } 

defaultAdjustmentParamH = do
  defaultLocation <- appFADefaultLocation <$> getsYesod appSettings 
  return $ AdjustmentParam Nothing defaultLocation
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


loadQohForAdjustment :: AdjustmentParam -> SqlHandler (Map Text [(Maybe Text, Double)])
loadQohForAdjustment param = do
  let defaultLocation = aLocation param
  -- join denorm table with category: style
  let sql = " SELECT value as style, stock_id, quantity "
         <> " FROM fames_item_category_cache "
         <> " JOIN 0_denorm_qoh USING (stock_id)"
         <> " WHERE loc_code = ? AND category = 'style' AND quantity != 0 "
      orderBy = " ORDER BY style, stock_id"
      (w,p) = case filterEKeyword <$> aStyleFilter param of
        Nothing -> ("",  [] )
        Just (keyword, v) -> (" AND value " <> keyword <> " ? " , [toPersistValue v])
      convert :: (Single Text, Single (Maybe Text), Single Double) -> (Text, (Maybe Text, Double))
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
displayBoxtakeAdjustments param  = do
  infos <- runDB $ loadAdjustementInfo param
  let summaries = toList infos >>= computeInfoSummary
  return [whamlet|
  <table>
    $forall s <- summaries
      <tr>
        <td>#{fromMaybe "" $ ssSku s}
        <td>#{formatQuantity (ssQoh s)}
        <td>
          $forall (_, _, qM) <- ssBoxes s
           (#{maybe "" formatQuantity qM})
           <nbsp> 
                 |]
    
_renderStyleInfos style'info =
  return [whamlet|
  <table>
    $forall (style, info) <- style'info
      <tr>
        <td>#{style}
        <td>
          $forall (var, qoh) <- mapToList (siQoh info)
            #{fromMaybe "" var}:#{tshow qoh}
        <td>
          $forall (boxe, stocke) <- take 3 $ siBoxtakes info
            #{tshow $ entityVal boxe}
|]
