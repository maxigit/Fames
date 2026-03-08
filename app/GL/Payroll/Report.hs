{-# LANGUAGE ImplicitParams #-}
module GL.Payroll.Report where

import Prelude
import Lens.Micro
import Lens.Micro.Extras
import           Data.Map(Map)
import qualified Data.Map as Map
import GL.Payroll.Timesheet as TS
import           Data.Time ( Day
                           , addDays
                           , formatTime
                           )
import qualified GL.Payroll.Nest as Nest
import GL.Payroll.Settings (PayrollFormula(..))
import Data.Time.Format    (defaultTimeLocale)
import Data.These
import Data.Align
import Locker
import Data.Text (Text, unpack, pack)
import System.FilePath((</>))
import Data.Maybe(mapMaybe, fromMaybe)
-- * Formatting 
-- ** Display 
-- Custom show
class Display a where
    display :: a -> String

instance Display Employee where
    display  e = e ^. nickName

instance Display PayrooEmployee where
    display  e = e ^. nickName

instance Display Day where
    display d = formatTime local "%A %d %b %Y" d where
        local = defaultTimeLocale

instance {-# OVERLAPPING  #-} Display String where
  display c = c
-- *** Misc 
instance (Display a, Display b) => Display (a,b) where
    display (a,b) = display a
                    ++ "\t" ++ display b

instance (Display a, Display b, Display c) => Display (a,b, c) where
    display (a,b,c) = display a
                    ++ "\t" ++ display b
                    ++ "\t" ++ display c

instance (Display a) => Display [a] where
    display as = unlines $ map display as
instance (Display p, Display e) => Display (Timesheet p e) where
    display ts = display (ts^.shifts) <> "\n" <> display  (ts^.deductionAndCosts)

instance Display ShiftType where
  display st = show st

-- *** Deduction and Costs 
instance Display k =>  Display (Shift k) where
    display s = displayShift show show s

displayShift :: (HasShift s k, Display k) =>
                (Amount -> [Char]) -> (Duration -> [Char]) -> s -> [Char]
displayShift amountF durationF s = display (s ^. shiftKey)
                    ++  "\t" ++ (durationF $ s ^. duration)
                    ++ "\t@" ++ (amountF $ s ^. hourlyRate)
                    ++ "\t= " ++ (amountF $ s ^. cost)



-- *** Deduction and Costs 
instance Display k => Display (DeductionAndCost k) where
  display = displayDAC show show 

displayDAC :: (HasDeductionAndCost s a, Display a) =>
              (Amount -> [Char]) -> (Amount -> [Char]) -> s -> [Char]
displayDAC amountF durationF dac = "@" ++ display (dac ^. dacKey) ++ " "
                 ++ maybe "" durationF _duration
                 ++ "^"
                 ++ maybe "" amountF _cost where
                        (_duration, _cost) = these (\a -> (Just a, Nothing))
                                                 (\b -> (Nothing, Just b))
                                                 (\a b -> (Just a , Just b))
                                                 (dac ^. dacDac)


groupShiftsBy :: (Ord b) => (a-> b) -> [Shift a] -> [Shift b]
groupShiftsBy f ss = let
    ss' = [(f $ s^.shiftKey, s {_shiftKey = () } ) | s <- ss]
    groups = Map.fromListWith (<>) ss'
    in [s { _shiftKey = k } | (k,s) <- Map.toList groups]

groupDacsBy :: (Ord b) => (a-> b) -> [DeductionAndCost a] -> [DeductionAndCost b]
groupDacsBy f ss = let
    ss' = [(f $ s^.dacKey, s {_dacKey = () } ) | s <- ss]
    groups = Map.fromListWith (<>) ss'
    in [s { _dacKey = k } | (k,s) <- Map.toList groups]

instance Display () where
  display _st = ""
    
groupBy :: Ord k =>  (a->k) -> [a] -> Map k [a]
groupBy key as =
    Map.fromListWith (++)
                     (map ((,) <$> key <*> (:[])) as)
        
    
    
-- **  Textcart 
newtype Sku = Sku { sku :: String } deriving (Eq, Ord, Show)
newtype Textcart = Textcart (Day, ShiftType, [Shift Sku])
textcarts :: ShiftType -> Timesheet p Sku -> [Textcart]
textcarts st ts = let 
    _filtered = filter ((== st) . view shiftType) (ts ^. shifts)
    byDays = groupBy (^.day) _filtered
    in [Textcart (d, st, groupShiftsBy (view _1) ss) | (d,ss) <- Map.toList byDays]

instance Display Textcart where
   display (Textcart (d, st, ss)) = unlines $
       [  replicate 50 '-'
       , "-- " ++ display  d
       , "$deliverydate=" ++ formatDate  d
       , "$supplierreference=" ++ formatDate d
       , "$supplier=Wages"
       , "$location=" ++ renderShiftType st

       ]
       ++ map renderShift ss
       where renderShift  s = (sku $ s ^. shiftKey) -- sku
                          ++ "\t +" ++ show (s ^. duration) -- quantity
                          ++ "\t $" ++ show (s ^. hourlyRate) -- price
             renderShiftType Work = "DEF"
             renderShiftType Holiday = "LOST"
             formatDate = formatTime defaultTimeLocale "%Y/%m/%d"
-- ***
-- | Write a textcart into a file
-- The name depend on the date
writeTextcart :: String -> Textcart -> IO ()
writeTextcart dir cart@(Textcart (d, st, __ss)) = do
    -- createDirectory dir
    let path = (show st) ++ "-" ++ formatTime defaultTimeLocale "%F-%A.txt" d
    putStrLn $  "saving Texcart : " ++ path
    writeFile (dir </>  path) (display cart)

-- ** PAYROO 
-- 
payroo :: ( ?viewPayrollAmountPermissions :: (Text -> Granted)
          , ?viewPayrollDurationPermissions :: (Text -> Granted)
          )
       => Timesheet p PayrooEmployee -> [String]
payroo ts = 
    [ "Pay Period End Date,,,,,,,,"
    , (formatDay . period) (ts ^. periodStart) ++ ",,,,,,,,"
    , "Employer / Client / Branch Reference,Employee's Works Number,Employee's Name,Item code,Item Name,Item Indicator,Quantity,Rate,Payslip Message"
    ]++ map formatShift (groupShiftsBy (pure (,)
                                        <*> (view payrooEmployee)
                                        <*> (view shiftType)
                                       )
                                       (filter ((== Work) . view shiftType) (ts ^. shifts))
                        )

period :: Day -> Day
period = addDays 6

formatDay :: Day -> String
formatDay = formatTime defaultTimeLocale "%d/%m/%y"

formatShift :: ( ?viewPayrollAmountPermissions :: (Text -> Granted)
               , ?viewPayrollDurationPermissions :: (Text -> Granted)
               )
            => Shift (PayrooEmployee, ShiftType) -> String
formatShift s = "Employer" -- Employer / Client / Branch Reference
                ++ "," ++ (show $ s ^. payrollId) -- Employee's Works Number
                ++ "," ++ (s ^. firstName) -- Employee's Name
                       ++ " " ++ s^.surname
                ++ ",BASIC" -- Item code
                ++ ",BASIC PAY" -- Item Name
                ++ ",P" -- Item Indicator
                ++ "," ++ either (const "") show (unlock ?viewPayrollDurationPermissions (s^.duration)) -- Quantity
                ++ "," ++ either (const "") show (unlock ?viewPayrollAmountPermissions (s^.hourlyRate))-- Rate
                ++ "," -- Payslip Message"

writePayroo :: (?viewPayrollAmountPermissions::Text -> Granted,
                 ?viewPayrollDurationPermissions::Text -> Granted) =>
               FilePath -> Timesheet p PayrooEmployee -> IO ()
writePayroo dir ts = do
    let path = formatTime defaultTimeLocale "%F" (ts ^. periodStart)
           ++ "-"
           ++ formatTime defaultTimeLocale "%F" (period (ts ^. periodStart))
           ++ ".csv"

    writeFile (dir </> path) (unlines (payroo ts))

-- ** NEST
-- 
nest :: ( ?viewPayrollAmountPermissions :: (Text -> Granted)
          , ?viewPayrollDurationPermissions :: (Text -> Granted)
          )
       => Text -> Text -> [(Text, [PayrollFormula])] -> Timesheet Text (Text, Int) -> [String]
nest employerRef paymentSource formulas ts = let
  --                           ^^^^^^^^  
  --                              |
  --                              +--- contains Qualified Earning. In theory we just need this one
  header =  Nest.Header (unpack employerRef)
                        (periodEnd ts)
                        (unpack paymentSource)
                        (ts ^. frequency)
                        (ts ^. periodStart)
  summaries :: [EmployeeSummary Text (Text, Int)]
  summaries = map (tweakSummary formulas) $ paymentSummary ts
  details = mapMaybe mkDetail summaries
  mkDetail emp = let employee = Map.findWithDefault 0 "NEST(e)" (_deductions emp)
                     employer = Map.findWithDefault 0 "NEST(r)" (_costs emp)
                     qualifiedEarnings = Map.findWithDefault 0 "Qualified Earnings (M)" (_netDeductions emp)
  
                 in case (employee, employer) of 
                         (0,0) -> Nothing
                         _ -> Just $ Nest.Detail (unpack $ emp ^. sumEmployee . _1) -- surname
                                                 (show $ emp ^. sumEmployee . _2 )
                                                 (unlockAmount qualifiedEarnings)
                                                 (unlockAmount employee)
                                                 (unlockAmount employer)
  unlockAmount = either (error . show) id . unlock ?viewPayrollAmountPermissions
  in Nest.makeCsv header details

-- * Payment summary 
paymentSummary :: (Ord e, Ord p) => Timesheet p e -> [EmployeeSummary p e]
paymentSummary timesheet_ = let
  -- group shifts and dacs per operators
  employeeTotal' = groupBy (^.shiftKey._1) (timesheet_ ^. shifts)
  -- group shifts w
  employeeTotal'' = fmap (groupBy _shiftKey . groupShiftsBy (^._3)) employeeTotal'
  employeeTotal = fmap (fmap (map $ fmap (const ()))) employeeTotal'' -- replace ShifKey with ()
  employeeDACS' = groupBy (^.dacKey._2)  (timesheet_ ^. deductionAndCosts)
  employeeDACS = fmap (fmap (fmap fst)) employeeDACS'
  employeeMap = align employeeTotal employeeDACS
  in map mkSummary (Map.toList employeeMap)
  
mkSummary :: Ord p => (e, These (Map ShiftType [Shift ()]) [DeductionAndCost p]) -> EmployeeSummary p e
mkSummary (emp, These shiftMap dacs) = let
  gross_ = sum $ concatMap (map _cost) (Map.elems shiftMap)
  net_ = gross_ - deduction_
  final = net_  - netDeduction
  totalCost_ = gross_ + cost_
  netDeductions_ = Map.empty
  mkMap getter  = Map.fromList [(payee, amount)
                               | dac <- dacs
                               , Just amount <- return (dac ^? getter)
                               , let payee = dac ^. dacKey
                               ]
  deductions_ = mkMap dacDeduction
  costs_ = mkMap dacCost
  deduction_ = sum (Map.elems deductions_)
  cost_ = sum (Map.elems costs_)
  netDeduction = sum (Map.elems netDeductions_)
  hours = fmap (sum . (map _duration)) shiftMap
  in EmployeeSummary emp final totalCost_ net_ gross_ deductions_ netDeductions_ costs_ hours
mkSummary (emp, This s) = mkSummary (emp , These s [])
mkSummary (emp, That dacs) = mkSummary (emp , These Map.empty dacs)

-- * Formula
-- Add virtual columns corresponding to the give formulas
tweakSummary :: [(Text, [PayrollFormula])] -> TS.EmployeeSummary Text e -> TS.EmployeeSummary Text e
tweakSummary formulas emp =  let
  val118 = (\x -> x - 118) <$> emp ^. TS.gross -- unside a locker
  val512 = (\x -> x - 512) <$> emp ^. TS.gross -- unside a locker
  deducs = emp ^. TS.netDeductions
  suffixKeys suffix = Map.mapKeysMonotonic (<> suffix) 
  emp' = emp & TS.deductions %~ (suffixKeys "(e)")
                 & TS.costs %~ (suffixKeys "(r)")
  in emp' & TS.netDeductions .~ deducs <> Map.fromList ([ ("Do Not Use Earnings (W)", val118)
                                                       , ("Qualified Earnings (M)", val512)
                                                       ] <> 
                                                       [ (name, evalFormulas emp' formula)
                                                       | (name, formula) <- formulas
                                                       , [PFVariable name] /= formula
                                                       , [] /= formula
                                                       -- \^ we need to filter normal field
                                                       -- but only keep calculated formula
                                                       ] 
                                                     )

evalFormulas :: TS.EmployeeSummary Text e -> [PayrollFormula] -> TS.Amount
evalFormulas emp = sum . map (fromMaybe 0 . evalFormula emp) 
evalFormula :: TS.EmployeeSummary _Text e -> PayrollFormula -> Maybe TS.Amount
evalFormula  emp formula = let
   values = TS._deductions emp
             <> TS._netDeductions emp
             <> TS._costs emp
             <> Map.fromList ([ ("Gross",  TS._gross emp)
                             , ("Net", TS._net emp)
                             , ("To Pay", TS._finalPayment emp)
                             , ("Total Cost", TS._totalCost emp)
                             ]
                             <> [( pack $ show k, h)
                                | (k,h)  <- Map.toList (TS._totalHours emp)
                                ]
                             )
   in case formula of
       PFVariable var -> Map.lookup var values
       PFNegVariable var -> fmap (fmap negate) $ Map.lookup var values
       PFValue v -> return (pure v)
