-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Import

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

favicon= $(embedFile "config/favicon.ico")
faviconDOrange= $(embedFile "config/favicon-dorange.ico")
faviconPurple= $(embedFile "config/favicon-purple.ico")
getFaviconR :: Handler TypedContent
getFaviconR = do
  faviconName <- appFavicon <$> getsYesod appSettings
  cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
  return $ TypedContent "image/x-icon"
         $ toContent (case faviconName of
                        "Orange" -> faviconDOrange
                        "Purple" -> faviconPurple
                        _ -> favicon
                     )

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")
