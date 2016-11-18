{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Handler.WH.Stocktake where

import Import

import Yesod.Form.Bootstrap3 
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)




getWHStocktakeR :: Handler Html
getWHStocktakeR = do
  return ""


postWHStocktakeR :: Handler Html
postWHStocktakeR = return ""

