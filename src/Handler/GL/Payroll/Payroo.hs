module Handler.GL.Payroll.Payroo
(
readPayroo
) where

import Import

import GL.Payroll.Timesheet hiding (payrollId)
import GL.Payroll.Settings
import Handler.CsvUtils
import qualified Data.Csv as Csv
-- import qualified Data.Csv.Conversion as Csv
import Control.Monad.Fail(fail)

import qualified Data.ByteString.Char8 as BS
import GL.Payroll.Parser (lockA)
import qualified Data.Map as Map

data PayrooRecord = PayrooRecord
     { worksNo :: Maybe Int
     , employeeName :: Text
     , taxableGross
     , netPay
     , employeePAYE
     , employeeNetNIC
     , employeePension
     , totalGrossPay
     , employerNetNIC
     , employerPension
     , totalNotionalPayment
     , totalEmployerCost :: Thousands Double
     , departmentName
     , costCenterName
     , branchName :: Text
  } deriving (Show )
  
columnNames = [ ( "worksNo"            , [ "Works\nNo." ] )
              , ("employeeName"        , [ "Employee Name" ] )
              , ("taxableGross"        , [ "Taxable\nGross" ] )
              , ("netPay"              , [ "Net Pay" ] )
              , ("employeePAYE"        , [ "Employee\nPAYE" ] )
              , ("employeeNetNIC"      , [ "Employee\nNet NIC" ] )
              , ("employeePension"     , [ "Employee\nPension" ] )
              , ("totalGrossPay"       , [ "Total\nGross Pay" ] )
              , ("employerNetNIC"      , [ "Employer\nNet NIC" ] )
              , ("employerPension"     , [ "Employer\nPension" ] )
              , ("totalNotionalPayment", [ "Total\nNotional\nPayment" ] )
              , ("totalEmployerCost"   , [ "Total\nEmployer\nCost" ] )
              , ("departmentName"      , [ "Department\nName" ] )
              , ("costCenterName"      , [ "Cost Center\nName" ] )
              , ("branchName"          , [ "Branch\nName" ] )
              ]
columnNameMap = buildColumnMap columnNames

instance Csv.FromNamedRecord PayrooRecord where
   parseNamedRecord m = let
     [no, employee, tax, net, paye, niE, penE, total, niR, penR, totalNP, totalCost, dept, costCenter, branch] = map fst columnNames
     parse :: Csv.FromField a => Csv.NamedRecord -> String -> Csv.Parser a
     parse v key = parseMulti columnNameMap v key
                   >>= either (fail . unpack . invalidFieldError)
                              pure
     in pure PayrooRecord
             <*> m `parse` no 
             <*> m `parse` employee
             <*> m `parse` tax
             <*> m `parse` net
             <*> m `parse` paye
             <*> m `parse` niE
             <*> m `parse` penE
             <*> m `parse` total
             <*> m `parse` niR
             <*> m `parse` penR
             <*> m `parse` totalCost
             <*> m `parse` totalNP
             <*> m `parse` dept
             <*> m `parse` costCenter
             <*> m `parse` branch
      
-- | 
readPayroo :: (Map Text EmployeeSettings) -> ByteString -> ParsingResult ByteString [DeductionAndCost  (String, Text)]
readPayroo employeesMap content = let 
     empMap = mapFromList [  (payrollId e, nickName)
                          | (nickName, e) <- Map.toList employeesMap
                          ]
     -- first we need to strip the first 21 lines
     -- the file should have at lise 21 for the header and 1 line for the footer
     ls = map (BS.filter (/='\r')) $ BS.lines content
     in case drop 5 ls of 
        employesAndTotal@(header :  _) {- | "\"Works"  `BS.isPrefixOf` header -} -> either id id do
                             employees <- parseSpreadsheet columnNameMap
                                                         Nothing
                                                         (BS.unlines $ employesAndTotal)
                                          <|&> \inv -> WrongHeader inv
                             dacs <- traverse (makeDACs empMap) employees 
                                     <|&> \e -> InvalidData [e] [] []
                             Right $ ParsingCorrect $ concat dacs


        [] -> WrongHeader  $ InvalidSpreadsheet "Wrong heador or data" [] [] []
        (header: others) -> InvalidData  ["Wrong header or data"] [header] others
        
makeDACs :: Map Int Text -> PayrooRecord -> Either Text [ DeductionAndCost (String, Text) ]
makeDACs empMap PayrooRecord{..} =
         case lookup (fromMaybe 0 worksNo) empMap of
           Just e -> let go = dac e
                     in Right $ catMaybes [ go "PAYE" employeePAYE 0
                                          , go "NI" employeeNetNIC employerNetNIC
                                          , go "NEST" employeePension employerPension
                                          ]
           Nothing | employeeName == "Grand Total" -> Right []
           Nothing -> Left $ "Employee " <> employeeName <> " #" <> tshow worksNo <> " is not configured"
  where dac em key ee er = 
               case (lockA $ unThousands ee, lockA $ unThousands er) of 
                 (_, _) | zero ee , zero er -> Nothing
                 (_, ler) | zero ee -> Just $ DeductionAndCost (key , em) (That ler)
                 (lee, _ )| zero er  -> Just $ DeductionAndCost (key , em) (This lee)
                 (lee, ler) -> Just $ DeductionAndCost (key , em) (These lee ler)
        zero x = abs (unThousands x) < 1e-6


     

