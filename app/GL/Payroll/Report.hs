module GL.Payroll.Report where

import Prelude
import Lens.Micro
import Lens.Micro.Extras
import Control.Applicative
import           Data.Map(Map)
import qualified Data.Map as Map
import GL.Payroll.Timesheet
import           Data.Time ( Day
                           , LocalTime
                           , TimeOfDay
                           , makeTimeOfDayValid
                           , addDays
                           , timeOfDayToTime
                           , formatTime
                           )
import Data.Time.Format    ( defaultTimeLocale, wDays)
import Data.Semigroup
import Data.List.NonEmpty(nonEmpty)
import Data.Semigroup.Generator(reduceWith1)
import Data.Semigroup.Reducer(Reducer(..))
import Data.Ord(comparing)
import Data.Function(on)
import Data.These
import Data.Align


import System.Directory(createDirectory)
import System.FilePath((</>))
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
    display s = display (s ^. shiftKey)
                    ++  "\t" ++ show (s ^. duration)
                    ++ "\t@" ++ show (s ^. hourlyRate)
                    ++ "\t= " ++ show (s ^. cost)

-- *** Deduction and Costs
instance Display k => Display (DeductionAndCost k) where
  display dac = "@" ++ display (dac ^. dacKey) ++ " "
                 ++ maybe "" show duration
                 ++ "^"
                 ++ maybe "" show cost where
                        (duration, cost) = these (\a -> (Just a, Nothing))
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
  display st = ""
    
groupBy :: Ord k =>  (a->k) -> [a] -> Map k [a]
groupBy key as =
    Map.fromListWith (++)
                     (map ((,) <$> key <*> (:[])) as)
        
    
    
-- **  Textcart
newtype Sku = Sku { sku :: String } deriving (Eq, Ord, Read, Show)
newtype Textcart = Textcart (Day, ShiftType, [Shift Sku])
textcarts :: ShiftType -> Timesheet p Sku -> [Textcart]
textcarts st ts = let 
    filtered = filter ((== st) . view shiftType) (ts ^. shifts)
    byDays = groupBy (^.day) filtered
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
writeTextcart dir cart@(Textcart (d, st, ss)) = do
    -- createDirectory dir
    let path = (show st) ++ "-" ++ formatTime defaultTimeLocale "%F-%A.txt" d
    putStrLn $  "saving Texcart : " ++ path
    writeFile (dir </>  path) (display cart)

-- ** PAYROO
-- 
payroo :: Timesheet p PayrooEmployee -> [String]
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

formatDay = formatTime defaultTimeLocale "%d/%m/%y"

formatShift ::  Shift (PayrooEmployee, ShiftType) -> String
formatShift s = "Employer" -- Employer / Client / Branch Reference
                ++ "," ++ (show $ s ^. payrollId) -- Employee's Works Number
                ++ "," ++ (s ^. firstName) -- Employee's Name
                       ++ " " ++ s^.surname
                ++ ",BASIC" -- Item code
                ++ ",BASIC PAY" -- Item Name
                ++ ",P" -- Item Indicator
                ++ "," ++ show (s^.duration) -- Quantity
                ++ "," ++ show (s^.hourlyRate)-- Rate
                ++ "," -- Payslip Message"

writePayroo dir ts = do
    let path = formatTime defaultTimeLocale "%F" (ts ^. periodStart)
           ++ "-"
           ++ formatTime defaultTimeLocale "%F" (period (ts ^. periodStart))
           ++ ".csv"

    writeFile (dir </> path) (unlines (payroo ts))

-- * Payment summary
paymentSummary :: (Ord e, Ord p) => Timesheet p e -> [EmployeeSummary p e]
paymentSummary timesheet = let
  -- group shifts and dacs per operators
  employeeTotal = Map.fromList $ map (\s -> (s^.shiftKey, s)) (groupShiftsBy (^._1)  (timesheet ^. shifts))
  employeeDACS' = groupBy (^.dacKey._2)  (timesheet ^. deductionAndCosts)
  employeeDACS = fmap (fmap (fmap fst)) employeeDACS'
  employeeMap = align employeeTotal employeeDACS
  in map mkSummary (Map.toList employeeMap)
  
mkSummary :: Ord p => (e, These (Shift e) [DeductionAndCost p]) -> EmployeeSummary p e
mkSummary (emp, These s dacs) = let
  gross = s ^. cost
  net = gross - deduction
  final = net  - netDeduction
  totalCost = gross + cost_
  netDeductions = Map.empty
  mkMap getter  = Map.fromList [(payee, amount)
                               | dac <- dacs
                               , Just amount <- return (dac ^? getter)
                               , let payee = dac ^. dacKey
                               ]
  deductions = mkMap dacDeduction
  costs = mkMap dacCost
  deduction = sum (Map.elems deductions)
  cost_ = sum (Map.elems costs)
  netDeduction = sum (Map.elems netDeductions)
  in EmployeeSummary emp final totalCost net gross deductions netDeductions costs
mkSummary (emp, This s) = mkSummary (emp , These s [])
mkSummary (emp, That dacs) = mkSummary (emp , These s dacs) where
  s = Shift emp Nothing 0 0
