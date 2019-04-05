module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Text.Julius (RawJS (..))

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    mains <- mainLinks
    links <- mapM (\(title, route, _ ) -> do
                      subLinks <- sideLinksWithAuth (Just route)
                      return (title, route, subLinks)
                  ) mains
    let ix'links = zip [1..] links
    defaultLayout $ do
        setTitle "Welcome To Fames!"
        $(widgetFile "homepage")
        $(fayFile "Home")

postHomeR :: Handler Html
postHomeR = getHomeR
