module Handler.Items.Index where

import Import
import Yesod.Form.Bootstrap3

getItemsIndexR :: Handler Html
getItemsIndexR = renderIndex ok200


renderIndex :: Status -> Handler Html
renderIndex status = do
  let widget = "To do"
  sendResponseStatus status =<< defaultLayout widget




