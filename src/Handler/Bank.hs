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
getGLBankR = defaultLayout [whamlet|
<form method=POST action="@{GLR GLBankR}">
  <button.btn.btn-primary type=submit>Submint
|]




postGLBankR :: Handler Html
postGLBankR = do
  today <- utctDay <$> liftIO getCurrentTime
  dbConf <- appDatabaseConf <$> getsYesod appSettings
  faURL <- getsYesod (pack . appFAExternalURL . appSettings)
  -- get mysql connect info
  let options = B.Options{..}
      hsbcFiles ="HSBC*.csv"
      faCredential = myConnInfo dbConf
      statementFiles = "statement*.csv"
      output = ""
      startDate = Just (fromGregorian 2015 11 16 )
      endDate = Nothing -- Just today
      faMode = B.BankAccountId 1
      aggregateMode = B.BEST

      statmentPath = "/home/max/mae/warehouse/BankReconciliate"

  
  
  stransz <- lift $ withCurrentDirectory statmentPath (B.main options)
  -- we sort by date
  let sorted = sortOn (liftA3 (,,) (Down . B._sDate) (Down . B._sAmount) (B._sDayPos)) stransz
      total = sum (map B._sAmount stransz)
  defaultLayout [whamlet|
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
         $with isFa <- B._sSource trans == B.FA
          <tr :isFa:.text-danger :isFa:.bg-danger>
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
