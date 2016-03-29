{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.GLEnterReceiptSheet where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
-- | Entry point to enter a receipts spreadsheet
-- The use should be able to :
--   - post a text
--   - upload a document
--   - use an existing document
--   - download a spreadsheet template

       

getGLEnterReceiptSheetR :: Handler Html
getGLEnterReceiptSheetR = do
  (formWidget, formEncType) <- generateFormPost postTextForm
  let widget =  [whamlet|
<h1>Enter a receipts spreadsheet
<ul>
  <li> Enter text
    <form role=form method=post action=@{GLEnterReceiptSheetR} enctype=#{formEncType}>
      ^{formWidget}
      <button type="submit" .btn .btn-default>Process
  <li> Or Upload a document
  Not implemented
  <li> Or use an existing document
  Not implemented
  <li> Or download a spreadsheet template
  Not implemented
|]
  defaultLayout $ widget

postTextForm :: Form Text
postTextForm = renderBootstrap3 BootstrapBasicForm $ fmap unTextarea (areq textareaField (withSmallInput "receipts") Nothing)

postGLEnterReceiptSheetR :: Handler Html
postGLEnterReceiptSheetR = error "Not yet implemented: postGLEnterReceiptSheetR"
