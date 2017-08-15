module Handler.Administrator where

import Import
import WH.FA.Curl (testFAConnection)
import WH.FA.Types (FAConnectInfo(..))
import Data.Yaml.Pretty(encodePretty, defConfig)
import Data.Dynamic (dynTypeRep)
import qualified Data.Map as Map


-- | Page to test administrator authentication.
-- might be empty
getAIndexR :: Handler Html
getAIndexR = do
  args <- getArgs
  settings <- appSettings <$>  getYesod
  -- we don't show the settings for security reason
  -- however we can dump it to the console
  putStrLn $ decodeUtf8 $ encodePretty defConfig settings
  setSuccess "The configuration has been dumped to the log."
  defaultLayout $ [whamlet|
<h1> Administrator
You are logged as Administrator !
<ul>
  <li><a href="@{AdministratorR ATestFAR}"> Test FrontAccounting Connection
<div.panel>
  <div.panel-heading.panel-primary>
    <h3> Arguments
  <div.panel-body>
    <ul>
      $forall arg <- args
        <li> #{arg}
|]


-- | Test the connection with Front Accounting
getATestFAR :: Handler Html
getATestFAR = do
  setting <- appSettings <$> getYesod
  let connectInfo = FAConnectInfo (appFAURL setting) (appFAUser setting) (appFAPassword setting)
  resp <- liftIO $ testFAConnection connectInfo
  _ <- case resp of
    Left err -> do
      setError (toHtml $ "Couldn't connect to FrontAccounting" <> err)
    Right () -> do
      setSuccess "Connection successful"
  defaultLayout $ [whamlet|
<h1>FrontAccounting connection test
Trying to connect to URL #{tshow $ appFAURL setting} with user #{tshow $ appFAUser setting}
|]
  


-- | Displays status of the cache
getACacheR :: Handler Html
getACacheR = do
  cvar <- getsYesod appCache
  now <- liftIO $ getCurrentTime
  cache <- readMVar cvar
  let expired t = t < now
  -- sort by expiry date desc
  info <- mapM (\km ->traverse readMVar km) (Map.toList cache) 
  let sorted = sortBy (comparing (\(k, (_, t)) -> (Down t,k))) info
      extra _ = ""
  defaultLayout $ [whamlet|
<h1>Cache
<h2>Expiry Cache
<table.table.table-striped,table-hover>
  <td>
    <theader>Time
    <theader>Key
    <theader>Value
    <theader>Extra
  $forall (k, (dyn, t)) <- sorted
    $with exp <- expired t
      <tr :exp:.txt-muted>
        <td>#{tshow $ t}
        <td>#{tshow k}
        <td>#{tshow $ dynTypeRep dyn}
        <td>#{tshow $ extra dyn}
<form action=@{AdministratorR ACacheR} method=post>
  <button.btn.btn-danger type="submit">Clear 
                          |]

-- | Clear the cache
postACacheR :: Handler Html
postACacheR = do
  clearAppCache
  getACacheR
