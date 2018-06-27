module Handler.Bank 
( getGLBankR
, postGLBankR
) where


import Import
import qualified BankReconciliate as B
import BankReconciliate()
import Database.Persist.MySQL     (MySQLConf (..))
import System.Directory


getGLBankR  :: Handler Html
getGLBankR = do
  today <- utctDay <$> liftIO getCurrentTime
  dbConf <- appDatabaseConf <$> getsYesod appSettings
  faURL <- getsYesod (pack . appFAExternalURL . appSettings)
  settings <- getsYesod (appBankStatements . appSettings)
  panels <- forM (sortOn (bsPosition . snd) $ mapToList settings) (displayStatementInPanel today dbConf faURL)
  defaultLayout (mconcat panels)


postGLBankR :: Handler Html
postGLBankR = getGLBankR


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
  let sortTrans = sortOn (liftA3 (,,) (Down . B._sDate) (B._sDayPos) (Down . B._sAmount))
      sorted = sortTrans stransz
      panelId = "bank-"  <> title
      ok = null sorted
      lastBanks = take 10 $ sortTrans banks
      lastW = renderTransactions faURL lastBanks False (const False)
      tableW = renderTransactions faURL sorted True ((B.FA ==) . B._sSource)
  return [whamlet|
    <div.panel :ok:.panel-success:.panel-danger>
      <div.panel-heading data-toggle="collapse" data-target="##{panelId}">
        <h2>#{title}
      <div.panel-body.collapse :ok:.out:.in id="##{panelId}">
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

renderTransactions :: Text -> [B.STrans] -> Bool -> (B.STrans -> Bool) -> Widget
renderTransactions faURL sorted displayTotal danger = 
      let total = sum (map B._sAmount sorted)
      in [whamlet| 
        <table.table.table-hover.table-border.table-striped>
          <tr>
            <th>Paid Out
            <th>Paid In
            <th>Source
            <th>Date
            <th>Type
            <th>Description
            <th>Number
            <th>Object
          $forall trans <- sorted
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
                  <td>^{linkToFA (urlForFA faURL) trans}
                  <td>#{B._sDescription trans}
                  <td>#{maybe "-" tshow $ B._sNumber trans}
                  <td>#{fromMaybe "-" (B._sObject trans)}
          <tr>
            $if displayTotal
              $if total > 0
                <th>
                <th>#{tshow total}
              $else
                <th>#{tshow $ negate total}
                <th>
              <th><= Difference
              <th>
              <th>
              <th>
              <th>
              <th>
              |]
