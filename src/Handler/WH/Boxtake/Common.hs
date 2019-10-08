{-# LANGUAGE OverloadedStrings #-}
module Handler.WH.Boxtake.Common
where

import Import


loadStocktakes' :: [Entity Boxtake] -> SqlHandler [(Entity Boxtake , [Entity Stocktake])]
loadStocktakes' boxtakes = 
  -- slow version
  forM boxtakes $ \e@(Entity _ box) -> do
     stocktakes <- selectList [StocktakeBarcode ==. boxtakeBarcode box] []
     return (e, stocktakes)

loadStocktakes :: [Entity Boxtake] -> Handler [(Entity Boxtake , [Entity Stocktake])]
loadStocktakes boxtakes = runDB $ loadStocktakes' boxtakes

displayActive :: Bool -> Text
displayActive act = if act then "Active" else "Inactive"
  
dimensionPicture :: Int -> Boxtake -> Widget
dimensionPicture width Boxtake{..} =  do
  let dimRoute = WarehouseR $ WHDimensionOuterR (round boxtakeLength) (round boxtakeWidth) (round boxtakeHeight)
  [whamlet|
      <a href="@{dimRoute}" ><img src=@?{(dimRoute , [("width", tshow width)])}>
         |]
