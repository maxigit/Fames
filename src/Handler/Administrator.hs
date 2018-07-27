module Handler.Administrator where

import Import
import WH.FA.Curl (testFAConnection)
import WH.FA.Types (FAConnectInfo(..))
import Data.Yaml.Pretty(encodePretty, defConfig)
import Data.Dynamic (dynTypeRep, Dynamic, fromDynamic)
import Util.Cache
import qualified Data.Map as Map
import Items.Types
import Text.Pretty.Simple
import Development.GitRev
import Database.Persist.MySQL(unSqlBackendKey, Single(..))
import FA as FA hiding (unUserKey)


-- | Page to test administrator authentication.
-- might be empty
getAIndexR :: Handler Html
getAIndexR = do
  args <- getArgs
  settings <- appSettings <$>  getYesod
  let gitbranch = $gitBranch :: Text
      gitcommitDate = $gitCommitDate :: Text
      -- gitdescribe = $gitDescribe :: Text
      githash = $gitHash :: Text
      gitdirty = $gitDirtyTracked :: Bool
      gitdirtyStar = if gitdirty then "*" else "" :: Text
  -- we don't show the settings for security reason
  -- however we can dump it to the console
  putStrLn $ decodeUtf8 $ encodePretty defConfig settings
  setSuccess "The configuration has been dumped to the log."
  defaultLayout $ [whamlet|
<h1> Administrator
You are logged as Administrator !
<ul>
  <li>
    <a href="@{AdministratorR ATestFAR}">
      Test FrontAccounting Connection
<div.panel.panel-info>
  <div.panel-heading>
    <h3> executables arguments
  <div.panel-body>
    <ul>
      $forall arg <- args
        <li> #{arg}
<div.panel.panel-info>
  <div.panel-heading>
    <h3> build version
  <table.table.panel-body>
    <tr>
       <td> Branch
       <td> #{gitbranch}
    <tr>
       <td> Commit
       <td>
         <span :gitdirty:.text-danger :gitdirty:.bg-danger> #{githash}#{gitdirtyStar}
    <tr>
       <td> Dirty
       <td :gitdirty:.text-danger :(not gitdirty):.text-success> #{tshow gitdirty}
    <tr>
       <td> Commit Date
       <td> #{gitcommitDate}

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
  let expired t = t > now
  -- sort by expiry date desc
  info <- mapM (\km ->traverse readMVar km) (Map.toList cache) 
  let blockerH :: Delayed Handler a -> MVar DAction
      blockerH = blocker
  let sorted = sortBy (comparing (\(k, (_, t)) -> (Down t,k))) info
      extra :: Dynamic -> UTCTime -> Handler Html
      extra dyn t = case castToDelayed blockerH dyn of
        Nothing -> return ""
        Just block -> do
            mvarM <- liftIO $ tryReadMVar (block)
            let status = case (mvarM, expired t) of
                            (Nothing, True) -> [shamlet|<span.label.label-info>Waiting|]
                            (Nothing, False) -> [shamlet|<span.label.label-danger>Waiting|]
                            (Just DCancel, _) -> [shamlet|<span.label.label-warning>Cancelled|]
                            (Just DStart, True) -> [shamlet|<span.label.label-success>Started|]
                            (Just DStart, _) -> [shamlet|<span.label.label-warning>Started|]
            return $ status
      getExtra (k, (dyn, t)) = do
        x <- extra dyn t
        return (k, dyn, t, x)
  withExtra <- mapM getExtra sorted
  defaultLayout $ do
    toWidgetHead [shamlet|<meta http-equiv="refresh" content="1">|]
    [whamlet|
<h1>Cache
<h2>Expiry Cache
<table.table.table-striped.table-hover>
  <tr>
    <th>Time
    <th>Key
    <th>Value
    <th>Extra
  $forall (k, dyn, t, ex) <- withExtra
    $with exp <- expired t
      <tr :exp:.txt-muted>
        <td>#{tshow $ t}
        <td>#{k}
        <td>#{tshow $ dynTypeRep dyn}
        <td>#{ex}
<form action=@{AdministratorR ACacheR} method=post>
  <button.btn.btn-danger type="submit">Clear 
                          |]

-- | Clear the cache
postACacheR :: Handler Html
postACacheR = do
  clearAppCache
  getACacheR

getAResetCategoryCacheR :: Handler Html
getAResetCategoryCacheR = do
  refreshCategoryCache True
  setSuccess ("Category cache successfully refreshed")
  getAIndexR

getAResetCustomerCategoryCacheR :: Handler Html
getAResetCustomerCategoryCacheR = do
  refreshCustomerCategoryCache True
  setSuccess ("Customer Category cache successfully refreshed")
  getAIndexR
  
-- | Displays all customer, their info and computed category
getACustomerCategoryR :: Handler Html
getACustomerCategoryR = do
  infos <- loadDebtorsMasterRuleInfos
  cats <- customerCategoriesH
  finder <- customerCategoryFinderCached
  defaultLayout $ do
    [whamlet|
   <h2> Customers
   <table.table.table-border.table-hover.table-striped>
     <tr>
       <th>Id
       <th>Name
       <th>Note
       <th>Tax Code
       <th>Currency
       <th>Dimension 1
       <th>Dimension 2
       $forall cat <- cats
        <th>#{cat}
     $forall (key, info, dims) <- infos
       <tr>
        <td>##{tshow $ FA.unDebtorsMasterKey key}
        $with (Single name, Single note, Single tax, Single currency ) <- info
          <td>#{name}
          <td>#{note}
          <td>#{tax}
          <td>#{currency}
        $with (Single dim1, Single dim2) <- dims
          <td>#{tshowM dim1}
          <td>#{tshowM dim2}
        $forall cat <- cats
          <td>#{fromMaybe "" $ finder cat key}
    |]


-- * Masquerade
masquerade = "masquerade-user" :: Text
getAMasqueradeR :: Handler Html
getAMasqueradeR = do
  defaultLayout $ do
    [whamlet|
  <h2> Masquerade as
     <form action=@{AdministratorR AMasqueradeR} method=post>
       <input type=text name="#{masquerade}">
       <button.btn.btn-danger type="Submit">Submit
            |]
  

postAMasqueradeR :: Handler Html
postAMasqueradeR = do
  userM <- lookupPostParam masquerade
  case userM of
    Nothing -> deleteSession masquerade
    Just user -> do
      traceShowM ("Masquerading to " <> user)
      setSession masquerade user
      role <- currentRole
      $(logWarn) . toStrict $ pShow ("NEW ROLE", role)
  getAMasqueradeR

