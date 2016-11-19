{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Handler.WH.Stocktake where

import Import

import Handler.CsvUtils

import Yesod.Form.Bootstrap3 
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)


-- * Requests

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
postWHStocktakeR = do
  ((fileResp, postFileW), enctype) <- runFormPost uploadFileForm
  case fileResp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show a
    FormSuccess (fileInfo, encoding) -> do
      spreadsheet <- readUploadUTF8 fileInfo encoding
      either id defaultLayout $ do
        -- expected results
        --   Everything is fine : [These Stocktake Boxtake]
        --   wrong header : 
        --   good header but invalid format
        --   parsable but invalid
        error "Not Implemented "
        
  

-- * Csv

data TakeRow

data ParsingResult
  = WrongHeader InvalidSpreadsheet
  | InvalidFormat -- some cells can't be parsed
  | InvalidData -- can be parsed but data are invalid
  | ParsingCorrect [These Stocktake Boxtake] -- Ok
  deriving Show

parseTakes  :: ByteString -> ParsingResult
parseTakes bytes = ParsingCorrect []
