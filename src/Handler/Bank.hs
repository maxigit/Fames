module Handler.Bank 
( getGLBankR
, getGLBankDetailsR
) where


import Import
import qualified BankReconciliate as B
import BankReconciliate()
import Database.Persist.MySQL     (MySQLConf (..))
import System.Directory
import Data.List (scanl)


getGLBankR  :: Handler Html
getGLBankR = do
  today <- utctDay <$> liftIO getCurrentTime
  dbConf <- appDatabaseConf <$> getsYesod appSettings
  faURL <- getsYesod (pack . appFAExternalURL . appSettings)
  settings <- getsYesod (appBankStatements . appSettings)
  panels <- forM (sortOn (bsPosition . snd) $ mapToList settings) (displayStatementInPanel today dbConf faURL)
  defaultLayout (mconcat panels)


displayStatementInPanel :: Day -> _DB -> Text -> (Text, BankStatementSettings) -> Handler Widget
displayStatementInPanel today dbConf faURL (title, BankStatementSettings{..})= do
  let options = B.Options{..}
      hsbcFiles = unpack bsStatementGlob
      faCredential = myConnInfo dbConf
      statementFiles = unpack bsDailyGlob
      output = ""
      startDate = bsStartDate
      endDate = Nothing -- Just today
      faMode = B.BankAccountId (bsBankAccount)
      aggregateMode = B.BEST

  
  (stransz, banks) <- lift $ withCurrentDirectory bsPath (B.main options)
  -- we sort by date
  let sortTrans = sortOn (liftA3 (,,) (Down . B._sDate) (Down . B._sDayPos) (Down . B._sAmount))
      sorted = sortTrans stransz
      panelId = "bank-"  <> title
      ok = null sorted
      lastBanks = take 10 $ sortTrans banks
      lastW = renderTransactions Nothing faURL lastBanks (Just "Total") (const False)
      tableW = renderTransactions Nothing faURL sorted (Just "Total") ((B.FA ==) . B._sSource)
  return [whamlet|
    <div.panel :ok:.panel-success:.panel-danger>
      <div.panel-heading data-toggle="collapse" data-target="##{panelId}">
        <a href="@{GLR (GLBankDetailsR title)}">
          <h2>#{title}
      <div.panel-body.collapse :ok:.out:.in id="#{panelId}">
        $if ok   
           <p> Everything is fine
        $else
           <h3> Discrepencies
           ^{tableW}
        <h3> Last 10
        ^{lastW}
                     |]

linkToFA
  :: (FATransType -> Int -> Text) -> B.STrans -> Html
linkToFA urlForFA' trans = case (readMay (B._sType trans), B._sNumber trans) of
  (Just t, Just no) | B._sSource trans == B.FA ->
           let eType = toEnum t
               trans = showTransType eType :: Text
           in [shamlet|
     <a href="#{urlForFA' (eType) no}" target=_blank>
       #{trans}
       |]
  _ -> toHtml (B._sType trans)

renderTransactions :: Maybe B.Amount -> Text -> [B.STrans] -> Maybe Text -> (B.STrans -> Bool) -> Widget
renderTransactions runBalance faURL sorted totalTitle danger = 
      let (ins, outs) = partition (> 0) amounts
          amounts = map B._sAmount sorted
          inTotal = sum ins
          outTotal = sum outs
          total = inTotal + outTotal
          balances = case runBalance of
            Nothing -> repeat Nothing
            Just bal0 -> map Just $ scanl (+) bal0 amounts
         
      in [whamlet| 
        <table.table.table-hover.table-border.table-striped>
          <tr>
            <th>Paid Out
            <th>Paid In
            <th>Source
            <th>Date
            $maybe _ <- runBalance
              <th> Balance
            <th>Type
            <th>Description
            <th>Number
            <th>Object
          $forall (trans, balanceM) <- zip sorted balances
            $with isDanger <- danger trans
              <tr :isDanger:.text-danger :isDanger:.bg-danger>
                  $if B._sAmount trans > 0
                    <td>
                    <td>#{tshow $  B._sAmount trans}
                  $else
                    <td>#{tshow $ negate  $    B._sAmount trans}
                    <td>
                  <td>#{tshow $ B._sSource trans}
                  <td>#{tshow $ B._sDate trans}
                  $maybe  bal <- balanceM
                     <td>#{tshow bal}
                  <td>^{linkToFA (urlForFA faURL) trans}
                  <td>#{B._sDescription trans}
                  <td>#{maybe "-" tshow $ B._sNumber trans}
                  <td>#{fromMaybe "-" (B._sObject trans)}
          <tr>
            $maybe totalTitle <-  totalTitle
              <th>#{tshow $ negate outTotal}
              <th>#{tshow inTotal}
              <th> #{totalTitle}
              <th> #{tshow total}
              $maybe bal <- runBalance
                <th> #{tshow $ bal + total}
              <th>
              <th>
              <th>
              <th>
              |]

getGLBankDetailsR :: Text -> Handler Html
getGLBankDetailsR account = do
  allSettings <- getsYesod (appBankStatements . appSettings)
  case lookup account allSettings of
    Nothing -> error $ "Bank Account " <> unpack account <> " not found!"
    Just settings -> defaultLayout =<< displayDetailsInPanel account settings 

displayDetailsInPanel :: Text -> BankStatementSettings -> Handler Widget
displayDetailsInPanel account BankStatementSettings{..} = do
  today <- utctDay <$> liftIO getCurrentTime
  dbConf <- appDatabaseConf <$> getsYesod appSettings
  faURL <- getsYesod (pack . appFAExternalURL . appSettings)
  let options = B.Options{..}
      hsbcFiles = unpack bsStatementGlob
      faCredential = myConnInfo dbConf
      statementFiles = unpack bsDailyGlob
      output = ""
      startDate = bsStartDate
      endDate = Nothing -- Just today
      faMode = B.BankAccountId (bsBankAccount)
      aggregateMode = B.ALL
  
  (stransz, banks) <- lift $ withCurrentDirectory bsPath (B.main options)
  let tableW = renderTransactions (Just 0) faURL stransz (Just "Total") ((B.FA ==) . B._sSource)
      ok = null stransz
      panelId = "bank-"  <> account
  return [whamlet|
    <div.panel :ok:.panel-success:.panel-danger>
      <div.panel-heading data-toggle="collapse" data-target="##{panelId}">
          <h2>#{account}
      <div.panel-body.collapse :ok:.out:.in id="#{panelId}">
           <h3> By amount
           ^{tableW}
                     |]
