{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
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
