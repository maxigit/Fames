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
  -- get mysql connect info
  let options = B.Options{..}
      hsbcFiles ="HSBC*.csv"
      faCredential = myConnInfo dbConf
      statementFiles = "statement*.csv"
      output = ""
      startDate = Just (fromGregorian 2015 11 16 )
      endDate = Just today
      faMode = B.BankAccountId 1
      aggregateMode = B.TAIL

      statmentPath = "/home/max/mae/warehouse/BankReconciliate"

  
  
  stransz <- lift $ withCurrentDirectory statmentPath (B.main options)
  -- we sort by date
  let sorted = sortOn ((Down . B._sAmount)) stransz
  defaultLayout [whamlet|
    <table.table.table-hover.table-border.table-striped>
      <tr>
        <th>Amount
        <th>Source
        <th>Date
        <th>Type
        <th>Description
        <th>Number
        <th>Object
       $forall trans <- sorted
         $with isFa <- B._sSource trans == B.FA
          <tr :isFa:.text-danger.bg-danger>
              <td>#{tshow $  B._sAmount trans}
              <td>#{tshow $ B._sSource trans}
              <td>#{tshow $ B._sDate trans}
              <td>#{B._sType trans}
              <td>#{B._sDescription trans}
              <td>#{maybe "-" tshow $ B._sNumber trans}
              <td>#{fromMaybe "" $ B._sObject trans}
                        |]


      
