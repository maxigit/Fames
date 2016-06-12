module Handler.Login where

import Import

-- | Login page
getLoginR' :: Handler Html
getLoginR' = do
  defaultLayout $ [whamlet|
<h1> Login
in construction.
|]

postLoginR' :: Handler Html
postLoginR' = undefined
