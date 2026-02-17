module Handler.Administrator where

import Import
import WH.FA.Curl (testFAConnection)
import WH.FA.Types (FAConnectInfo(..))
import Data.Yaml.Pretty(encodePretty, defConfig)
import Data.Dynamic (dynTypeRep, Dynamic)
import Util.Cache
import qualified Data.Map as Map
import Text.Pretty.Simple
import Development.GitRev
import FA as FA hiding (unUserKey)
import Database.Persist.MySQL     (Single(..), rawSql)
import Handler.Items.Category(getItemsCategoryTermsR)
import qualified Handler.Items.Category.Cache as Cache
import GL.TaxReport.Settings
import Formatting(format, bytes, fixed, (%))
import Unsafe.Coerce(unsafeCoerce)
import Data.Time(NominalDiffTime)

-- | Page to test administrator authentication.
-- might be empty
{-# NOINLINE getAIndexR #-}
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
  putStrLn $ decodeUtf8 $ encodePretty defConfig settings { appCategoryRules = mempty
                                                          , appCustomerCategoryRules =  mempty
                                                          , appOrderCategoryRules =  mempty
                                                          , appDeliveryCategoryRules =  mempty
                                                          , appVariations = mempty
                                                          , appVariationGroups = mempty
                                                          , appStockLocations = mempty
                                                          , appBankStatements = mempty
                                                          , appTaxReportSettings = mempty
                                                          , appReceiptTemplates = mempty
                                                          , appDPDSettings = Nothing
                                                          }
  setSuccess "The configuration has been dumped to the log."
  defaultLayout $ [whamlet|
<h1> Administrator
You are logged as Administrator !
<ul>
  <li>
    <a href="@{AdministratorR ATestFAR}">
      Test FrontAccounting Connection
  ^{taxReportChecks settings}
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
{-# NOINLINE getATestFAR #-}
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
{-# NOINLINE getACacheR #-}
getACacheR :: Handler Html
getACacheR = do
  cvar <- getsYesod appCache
  now <- liftIO $ getCurrentTime
  cache <- toCacheMap <$> readMVar cvar
  let expired t = t < now
  -- sort by expiry date desc
  info <- mapM (\km ->traverse readMVar km) (Map.toList cache) 
  let blockerH :: Delayed Maybe () -> Delayed Maybe ()
      blockerH = id
  let sorted = sortBy (comparing (\(k, (_, t)) -> (Down t,k))) info
      extra :: Dynamic -> UTCTime -> Handler (Html, (Html, Html, Word, NominalDiffTime))
      extra dyn t = case castToDelayed blockerH dyn of
        Nothing -> return ("", ([shamlet|<td><span.label.label-error>Bad CAST"|]
                               , [shamlet|<td>|]
                               , 0
                               , 0 
                               )
                          )
        Just delayed -> do
            status <- statusDelayed $ delayed
            info <- case status of 
                         Finished -> do
                            -- finished so we can get the value
                            einfo <- getDelayed $ unsafeCoerce delayed
                            return $ case snd einfo of 
                               Left err -> let e = err :: String
                                           in ( [shamlet|<td><span.label.laber-danger>#{e}|]
                                              , [shamlet|<td>|]
                                              , 0
                                              , 0
                                              )
                               Right (size, d) -> let (_,duration) = (size, d) :: (Word, NominalDiffTime)
                                                         in ( [shamlet|<td data-order="#{size}">#{formatSize size}|]
                                                            , [shamlet|<td data-order="#{dropEnd 1 (tshow duration)}">#{tshow duration}|]
                                                            , size
                                                            , duration
                                                            )
                         OnError ex -> do
                            let errMsg = show ex
                            return ( [shamlet|<td><span.label.laber-danger>#{errMsg}|]
                                   , [shamlet|<td>|]
                                   , 0
                                   , 0
                                   )
                         _ -> do
                              return ( [shamlet|<td>|]
                                     , [shamlet|<td>|]
                                     , 0
                                     , 0
                                     )
            return ( case (status, expired t) of
                      (s, True) -> [shamlet|<span.label.label-danger>#{show s}|]
                      (Waiting, False) -> [shamlet|<span.label.info-info>Waiting|]
                      (InProgress, False) -> [shamlet|<span.label.label-warning>In Progress|]
                      (OnError _, _) -> [shamlet|<span.label.label-danger>Error|]
                      (Finished, False) -> [shamlet|<span.label.label-success>Ready|]
                   , info
                   )
      getExtra (k, (dyn, t)) = do
        (x, (sizeHtml, durationHtml, size, duration)) <- extra dyn t
        return ((k, dyn, t, sizeHtml, durationHtml, x)
               , (size, duration)
               )
      formatSize s = format (bytes $ fixed 1% " ") s
  (unzip -> (withExtra, size'durations)) <- mapM getExtra sorted
  let (sizes, durations) = unzip size'durations
  defaultLayout $ do
    toWidgetHead [shamlet|<meta http-equiv="refresh" content="30">|]
    [whamlet|
<h1>Cache
<h2>Expiry Cache
<div>
     <p>Total size: #{formatSize (sum sizes)}
     <p>Total duration: #{tshow (sum durations)}

<table *{datatable}>
  <thead>
    <tr>
      <th>Time
      <th>Key
      <th>Value
      <th>Size
      <th>Duration
      <th>Extra
      <th>
  $forall (k, dyn, t, tdsize, duration, ex) <- withExtra
    $with exp <- expired t
      <tr :exp:.txt-muted>
        <td>#{tshow $ t}
        <td>#{k}
        <td>#{tshow $ dynTypeRep dyn}
        ^{tdsize}
        ^{duration}
        <td>#{ex}
        <td>
          <form action=@{AdministratorR $ ACachePurgeKeyR $ pack k} method=POST>
            <button.btn.btn-danger type=submit>Purge
<form action=@{AdministratorR ACacheR} method=post>
  <button.btn.btn-danger type="submit">Clear 
                          |]

-- * Clear the cache 
{-# NOINLINE postACacheR #-}
postACacheR :: Handler Html
postACacheR = do
  clearAppCache
  getACacheR

{-# NOINLINE postACachePurgeKeyR #-}
postACachePurgeKeyR :: Text -> Handler Html
postACachePurgeKeyR key = do
  cache <- getsYesod appCache
  purgeKey' cache $ unpack key
  getACacheR

-- | Only there so that refresh doesn't crash
-- but returns something
getACachePurgeKeyR :: Text -> Handler Html
getACachePurgeKeyR _ = getACacheR
-- ** Item Categories 
{-# NOINLINE getAResetCategoryCacheR #-}
getAResetCategoryCacheR :: Handler Html
getAResetCategoryCacheR = do
  catm <- lookupGetParam "category"
  stockFilterM <- lookupGetParam "stockFilter"
  Cache.refreshCategoryFor catm (fromString . unpack <$> stockFilterM)
  setSuccess ("Category cache successfully refreshed")
  case catm of
    Nothing -> do
      -- clear all category keys
      categories <- categoriesH
      mapM (purgeCacheKey . Cache.categoryCacheKey) categories
      getAIndexR
    Just cat -> do
      purgeCacheKey $ Cache.categoryCacheKey cat
      getItemsCategoryTermsR cat

-- ** Customer Categories 
{-# NOINLINE getAResetCustomerCategoryCacheR #-}
getAResetCustomerCategoryCacheR :: Handler Html
getAResetCustomerCategoryCacheR = do
  Cache.refreshCustomerCategoryCache True
  setSuccess ("Customer Category cache successfully refreshed")
  getACustomerCategoryR
  
-- | Displays all customer, their info and computed category
{-# NOINLINE getACustomerCategoryR #-}
getACustomerCategoryR :: Handler Html
getACustomerCategoryR = do
  infos <- Cache.loadDebtorsMasterRuleInfos
  cats <- customerCategoriesH
  finder <- Cache.customerCategoryFinderCached
  defaultLayout $ do
    [whamlet|
   <h2> Customers
   <table *{datatable} data-paging=false>
     <thead>
      <tr>
        <th>Id
        <th>Name
        <th>Note
        <th>Tax Code
        <th>Currency
        <th>Dimension 1
        <th>Dimension 2
        <th>First Order Date
        <th>First Order Ref
        <th>Last Order Date
        <th>Last Order Ref
        $forall cat <- cats
          <th>#{cat}
     $forall (key, info, dims, firstOrder, lastOrder) <- infos
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
        $forall (Single day, Single ref) <- [firstOrder, lastOrder]
          <td>#{tshowM day}
          <td>#{fromMaybe "" ref}
        $forall cat <- cats
          <td>#{fromMaybe "" $ finder cat key}
    |]

-- ** Order Categories 
-- | Recomputes the categories for ALL orders
-- usefull when a new category is created
{-# NOINLINE getAResetOrderCategoryCacheR #-}
getAResetOrderCategoryCacheR :: Handler Html
getAResetOrderCategoryCacheR = do
  Cache.refreshOrderCategoryCache
  setSuccess ("Order category cache sucessfully refreshed")
  getAOrderCategoryR

-- | Computes category only for the new orders (limited by a number)
-- can be called 
{-# NOINLINE getAComputeNewOrderCategoryCacheR #-}
getAComputeNewOrderCategoryCacheR :: Handler Html
getAComputeNewOrderCategoryCacheR = do
  Cache.refreshNewOrderCategoryCache
  setSuccess ("New Order category cache sucessfully computed")
  getAOrderCategoryR

-- | Displays the status of order categories 
-- How many category have been computed, and the last order computed
getAOrderCategoryR  :: Handler Html
getAOrderCategoryR = do
  sumW <- displayOrderCategorySummary
  pendingW <- displayPendingOrderCategory
  defaultLayout [whamlet|
<div.panel.panel-info>
  <div.panel-heading>
    <h3> Summary
  <div.panel-body>
    ^{sumW}
<div.panel.panel-info>
  <div.panel-heading>
    <h3> Pending
  <div.panel-body>
    ^{pendingW}
<div.well>
  <a href=@{AdministratorR AResetOrderCategoryCacheR}>Reset Order Categories
  <a href=@{AdministratorR AComputeNewOrderCategoryCacheR}>Compute New Order Categories
|]

displayOrderCategorySummary :: Handler Widget
displayOrderCategorySummary = do
  let sql = "SELECT category, count(*) orderId, count(distinct value) FROM fames_order_category_cache GROUP BY category ORDER BY category"
  rows <- runDB $ rawSql sql []
  let _types = rows :: [(Single Text, Single Int, Single Int)]
  return [whamlet|
<table.table.table-border.table-hover.table-striped>
  <tr>
    <th>Category
    <th>Values #
    <th> Order #
  $forall (Single category, Single orderN, Single valueN) <- rows
    <tr>
      <td>#{category}
      <td>#{valueN}
      <td>#{orderN}
|]

-- find how many order have not been categoried
displayPendingOrderCategory :: Handler Widget
displayPendingOrderCategory = runDB $ do
   [Single orderNb] <- rawSql "SELECT count(*) from 0_sales_orders where trans_type = ? "  [toPersistValue ST_SALESORDER]
   [Single catNb] <- rawSql "SELECT count(distinct order_id) from fames_order_category_cache" []
   let leftOver = orderNb - catNb :: Int
   when (leftOver > 0) $ do
     setWarning (toHtml $ "There are " <> tshow leftOver <> " orders left to categorize")
   return [whamlet|
    <p>Categories order #{catNb}/#{orderNb}.
                  |]

  

-- * Masquerade 
masquerade :: Text
masquerade = "masquerade-user"
{-# NOINLINE getAMasqueradeR #-}
getAMasqueradeR :: Handler Html
getAMasqueradeR = do
  defaultLayout $ do
    [whamlet|
  <h2> Masquerade as
     <form action=@{AdministratorR AMasqueradeR} method=post>
       <input type=text name="#{masquerade}">
       <button.btn.btn-danger type="Submit">Submit
            |]

{-# NOINLINE postAMasqueradeR #-}
postAMasqueradeR :: Handler Html
postAMasqueradeR = do
  userM <- lookupPostParam masquerade
  case userM of
    Nothing -> deleteSession masquerade
    Just user -> do
      logInfoN ("Masquerading to " <> user)
      setSession masquerade user
      role <- currentRole
      $(logWarn) . toStrict $ pShow ("NEW ROLE", role)
  getAMasqueradeR


-- * Tax report 
-- List all report which needs check fraud previons
taxReportChecks :: AppSettings -> Widget
taxReportChecks settings = let
  reports = [ reportType
            | (reportType, repSettings) <- Map.toList (appTaxReportSettings settings)
            -- only display HMRCProcessor
            , HMRCProcessor _  <- [processor repSettings]
            ]
  in [whamlet|
      $forall reportType <- reports
        <li>
         <a href=@{GLR $ GLTaxReportValidateFraudPreventionHeadersR reportType}>#{reportType} : Validate Fraud Headers
      |]

