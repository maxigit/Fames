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
      aggregateMode = B.BEST

      statmentPath = "/home/max/mae/warehouse/BankReconciliate"

  
  
  report <- lift $ withCurrentDirectory statmentPath (B.main options)
  defaultLayout $ toWidget $ toHtmlWithBreak (toStrict $ decodeUtf8 report)


      
