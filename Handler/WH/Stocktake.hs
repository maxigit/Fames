{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Handler.WH.Stocktake where

import Import

import Yesod.Form.Bootstrap3 
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)


-- * Request

getWHStocktakeR :: Handler Html
getWHStocktakeR = do
  (uploadFileFormW, upEncType) <- generateFormPost uploadFileForm
  defaultLayout $ do
    setTitle "Enter Stocktake"
    [whamlet|
<h1> Upload a stocktake spreadsheet
  <form #upload-form role=form method=post action=@{GLEnterReceiptSheetR} enctype=#{upEncType}>
    ^{uploadFileFormW}
    <button type="sumbit" name="validate" .btn .btn-default>Validate
    <button type="submit" name="process" .btn .btn-default>Process
|]


postWHStocktakeR :: Handler Html
postWHStocktakeR = return ""

