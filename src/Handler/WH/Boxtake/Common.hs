{-# LANGUAGE OverloadedStrings #-}
module Handler.WH.Boxtake.Common
( loadStocktakes
, loadStocktakes'
, dimensionPicture
, displayActive
, extractPosition
)
where

import Import
import WarehousePlanner.Type
import Util.ForConduit
import qualified Data.Conduit.List as C


loadStocktakes' :: SqlConduit (Entity Boxtake) (ForMap (Entity Boxtake) [Entity Stocktake]) ()
loadStocktakes' =  do
   em <- await
   case em of 
     Nothing -> return ()
     Just e@(Entity _ box) -> do
            stocktakes <- lift $ selectList [StocktakeBarcode ==. boxtakeBarcode box] []
            yield (ForMap e stocktakes)
            loadStocktakes'

loadStocktakes :: [Entity Boxtake] -> Handler [(Entity Boxtake, [Entity Stocktake]) ]
loadStocktakes boxtakes = do
               forMap <- runDB $ runConduit $ C.sourceList boxtakes .| loadStocktakes' .| sinkList
               return $ map unForMap forMap

displayActive :: Bool -> Text
displayActive act = if act then "Active" else "Inactive"
  
dimensionPicture :: Int -> Boxtake -> Widget
dimensionPicture width Boxtake{..} =  do
  let dimRoute = WarehouseR $ WHDimensionOuterR (round boxtakeLength) (round boxtakeWidth) (round boxtakeHeight)
  [whamlet|
      <a href="@{dimRoute}" ><img src=@?{(dimRoute , [("width", tshow width)])}>
         |]
         
{-# NOINLINE orientations #-}
orientations = mconcat $ ":" : map showOrientation' allOrientations
-- | Extract the position from a location
-- Eg E01.02/2:1:1:1 => Just (Just ':', 1:1:1)
extractPosition :: Text -> (Text, Maybe (Maybe Char, Text))
extractPosition location =
  case break (`elem` orientations) location of
        (loc, uncons -> Just (o, position)) -> (loc, Just (if o == ':' then Nothing else Just o, position))
        _                                   -> (location, Nothing)


