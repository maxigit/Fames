module Handler.Administrator where

import Import

-- | Page to test administrator authentication.
-- might be empty
getAdministratorR :: Handler Html
getAdministratorR = do
  defaultLayout $ [whamlet|
<h1> Administrator
You are logged as Administrator !
|]
