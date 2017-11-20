{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Handler.WH.Boxtake
( getWHBoxtakeR
, postWHBoxtakeR
, getWHBoxtakeDetailR
) where

import Import
import Yesod.Form.Bootstrap3
import Handler.CsvUtils

-- * Handlers
getWHBoxtakeR :: Handler Html
getWHBoxtakeR = undefined

postWHBoxtakeR :: Handler Html
postWHBoxtakeR = undefined

getWHBoxtakeDetailR :: Text -> Handler Html
getWHBoxtakeDetailR barcode = undefined
