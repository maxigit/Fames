{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.GLEnterReceiptSheet where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Data.Conduit.List (consume)
-- | Entry point to enter a receipts spreadsheet
-- The use should be able to :
--   - post a text
--   - upload a document
--   - use an existing document
--   - download a spreadsheet template

       

getGLEnterReceiptSheetR :: Handler Html
getGLEnterReceiptSheetR = do
  (postTextFormW, postEncType) <- generateFormPost postTextForm
  (uploadFileFormW, upEncType) <- generateFormPost uploadFileForm
  let widget =  [whamlet|
<h1>Enter a receipts spreadsheet
  <ul>
    <li> Enter text
         <form role=form method=post action=@{GLEnterReceiptSheetR} enctype=#{postEncType}>
            ^{postTextFormW}
            <button type="submit" .btn .btn-default>Process
    <li> Or Upload a document
         <form role=form method=post action=@{GLEnterReceiptSheetR} enctype=#{upEncType}>
           ^{uploadFileFormW}
            <button type="submit" .btn .btn-default>Upload
    <li> Or use an existing document
    Not implemented
    <li> Or download a spreadsheet template
    Not implemented
|]
  defaultLayout $ widget

postTextForm :: Form Text
postTextForm = renderBootstrap3 BootstrapBasicForm $ fmap unTextarea (areq textareaField (withSmallInput "receipts") Nothing)


uploadFileForm = renderBootstrap3 BootstrapBasicForm $ (areq fileField (withSmallInput "upload") Nothing )
postGLEnterReceiptSheetR :: Handler Html
postGLEnterReceiptSheetR = do
  body <- runRequestBody
  sheet <- lookupPostParam "f1"
  file <- lookupFile "f1"
  content <- case (sheet, file) of
                  (Just s, Nothing) -> return $ show s
                  (Nothing, Just fi) -> do
                    c <- fileSource fi $$  consume
                    return (show c)
                  (_, _ ) -> return "Something when wrong"

  defaultLayout $ [whamlet|
Everything looks fine.
<ul>
  <li> Keys #{show $ keys $ fst body}
  <li> Keys #{show $ keys $ snd body}
First part of the requestk#{content}
|]
