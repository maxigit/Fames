{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses , FunctionalDependencies , FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | This script read a "fast" time sheet format
-- and convert it FA textcart, PAYROO timesheet.csv
-- and generate report.
module GL.Payroll.Timesheet  where

import Prelude

import           Data.Time ( Day
                           , TimeOfDay
                           , addDays
                           , toGregorian
                           , addGregorianMonthsClip
                           )
import Data.Time.Calendar.WeekDate (toWeekDate)




import Data.Maybe

import Lens.Micro
import Lens.Micro.TH
import Data.Semigroup
import Data.List.NonEmpty(nonEmpty)
import Data.Semigroup.Generator(reduceWith1)



import Text.Printf(printf)
import Data.These
import Data.These.Lens
import Data.Bifunctor (bimap)
import qualified Data.Map as Map
import Data.Map(Map)
import Locker
import Data.Text (Text)

import GL.Utils

-- * Type alias 
type Amount = Locker Text Double
type Duration = Locker Text Double
type Hour = Double

-- * Data 
-- ** Employee 
-- | An employee.
data Employee = Employee
    { _nickName  :: String --  ^ use to designate an Employee. Must be unique
    , _defaultHourlyRate :: Maybe Amount
    } deriving (Show, Eq, Ord)

makeClassy ''Employee

-- *** Payroo 
-- | Add Payroo information to employee *inherits* from Employee
data PayrooEmployee = PayrooEmployee
    { _firstName :: String
    , _surname   :: String
    , _payrollId :: Int
    , _payrooEmployee' :: Employee
    } deriving (Show, Eq, Ord)
makeClassy ''PayrooEmployee
instance HasEmployee PayrooEmployee where
  employee = payrooEmployee'

-- ** Shift 
-- | The main data is a shift, ie a continous amount of time worked
-- The key represent a way to identify a shift (Employe/Employe,Day) ...
data ShiftType = Work | Holiday deriving (Show, Eq, Ord, Bounded, Enum)
makeClassy ''ShiftType
data Shift k = Shift
    { _shiftKey :: k
    , _startTime :: Maybe TimeOfDay
    , _duration :: Duration
    , _cost :: Amount
    } deriving (Show, Eq, Functor, Foldable, Traversable)

makeClassy ''Shift
instance HasEmployee k => HasEmployee (Shift k) where
    employee =  shiftKey . employee


instance HasEmployee e => HasEmployee (e, a) where
    employee = _1.employee

instance HasEmployee e => HasEmployee (e, a, b) where
    employee = _1.employee


instance HasPayrooEmployee k => HasPayrooEmployee (Shift k) where
    payrooEmployee =  shiftKey . payrooEmployee


instance HasPayrooEmployee e => HasPayrooEmployee (e, a) where
    payrooEmployee = _1.payrooEmployee

instance HasPayrooEmployee e => HasPayrooEmployee (e, a, b) where
    payrooEmployee = _1.payrooEmployee

makeClassy ''Day


instance HasDay d => HasDay (a, d)  where
    day = _2.day

instance HasDay d => HasDay (a, d, c)  where
    day = _2.day

instance HasDay k => HasDay (Shift k) where
    day = shiftKey.day

instance HasShiftType st => HasShiftType (a, st) where
  shiftType = _2.shiftType

instance HasShiftType st => HasShiftType (a, d, st) where
  shiftType = _3.shiftType

instance HasShiftType k => HasShiftType (Shift k) where
  shiftType = shiftKey.shiftType

hourlyRate :: HasShift s k => Getting r s Amount
hourlyRate = to $ (/) <$> (^.cost) <*> (^.duration)

-- | Deduction and costs
data DeductionAndCost key = DeductionAndCost
  { _dacKey :: key
  , _dacDac :: These Amount Amount
  } deriving (Eq, Show, Functor, Foldable, Traversable)
makeClassy ''DeductionAndCost

dacDeduction, dacCost :: (HasDeductionAndCost c key, Applicative f) => (Amount -> f Amount) -> c -> f c
dacDeduction = dacDac . here
dacCost = dacDac . there
dacTotal :: HasDeductionAndCost s key => s -> Amount
dacTotal dac =  go dacDeduction +  go dacCost where go f = fromMaybe 0 (dac ^? f)

data PayrollFrequency = Weekly | Monthly deriving (Eq, Show, Read, Enum, Bounded, Ord)
-- | A Timesheet. A functor over employee : allows
-- to easily modify the information relative to an employee
-- like replacing an operatorId by an Operator
data Timesheet p e = Timesheet
    { _shifts :: [Shift (e, Day, ShiftType)]
    , _periodStart :: Day
    , _frequency :: PayrollFrequency
    , _deductionAndCosts :: [DeductionAndCost (p, e)]
    }
    deriving (Show, Eq, Functor, Foldable, Traversable)

makeClassy ''Timesheet
traversePayee :: Monad m => (p -> m p' ) -> Timesheet p e -> m (Timesheet p' e)
traversePayee f ts = do
  let dacs = _deductionAndCosts ts
      f' dac =  do
        let (p, e) = _dacKey dac
        p' <- f p
        return $ dac { _dacKey = (p', e) }
  dacs' <- traverse f' dacs
  return $ ts { _deductionAndCosts = dacs'}

mapPayee :: (p -> p') -> Timesheet p e -> Timesheet p' e
mapPayee f ts =  ts { _deductionAndCosts = map f' (_deductionAndCosts ts) }
  where f' dac = let
          (p, e) = _dacKey dac
          p' = f p
          in dac { _dacKey = (p', e) }

newTimesheet :: PayrollFrequency -> Day -> Timesheet p e
newTimesheet l_frequency l_day = Timesheet [] l_day l_frequency []

-- ** Period 
-- ** EmployeeSummary 
-- Summary of payment, deductions and cost for a given Employee
data EmployeeSummary p e = EmployeeSummary
  { _sumEmployee :: e 
  , _finalPayment :: Amount  -- NET minus AFTER net deductions
  , _totalCost :: Amount  -- Gross + costs
  , _net :: Amount -- gross minus normal deductions
  , _gross :: Amount
  , _deductions :: Map p Amount -- dedk
  , _netDeductions :: Map p Amount
  , _costs :: Map p Amount
  , _totalHours :: Map ShiftType Duration
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
makeClassy ''EmployeeSummary

  

data Period = Period
  { _periodFrequency :: PayrollFrequency
  , _pStart :: Start
  } deriving (Show, Eq, Ord)
makeLenses ''Period

-- * Period Calculator 
-- | End of a period
periodEnd :: Timesheet p e -> Day
periodEnd ts = let
  l_day = _periodStart ts
  in case _frequency ts of
       Weekly -> addDays 6 l_day
       Monthly -> addDays (-1) (addGregorianMonthsClip 1 l_day )

-- | Adjust period year so it includes the given date
adjustPeriodYearFor :: Day -> Period -> Period
adjustPeriodYearFor l_day period = let
  newStart = adjustTaxYear (period ^. pStart) l_day
  in period & pStart .~ Start newStart


periodNameFor :: Day -> Period ->  String
periodNameFor l_day period = let
  (format, n) = case _periodFrequency period of
    Weekly -> ("%02d", snd $ weekNumber (_pStart period ) l_day )
    Monthly ->  ("M%02d", snd $  monthNumber (_pStart period ) l_day)
  in printf format n

refFormatter :: (Integer -> String -> String)
             -> Period
             -> Day
             -> String
refFormatter formatter period l_day = let
  period' = adjustPeriodYearFor l_day period
  Start start =  period' ^. pStart
  (year, _, _) = toGregorian start
  in formatter year (periodNameFor l_day period')

-- | Only there for testing reason
-- Allows to resave transactions even if they already exists
-- in front accountin
referencePrefix :: String
referencePrefix = ""
shortRef :: Period -> Day -> String
shortRef = refFormatter go where
  go year name = printf "%s%02d%s" referencePrefix (year `mod` 100) name

longRef :: Period -> Day -> String
longRef = refFormatter go where
  go year name = printf "%s%d/%s" referencePrefix year name


dayRef :: Period -> Day -> String
dayRef period l_day = let
  (_, _, weekDay) = toWeekDate l_day
  (_, _, monthDay) = toGregorian l_day
  dayS = case weekDay of
          1 -> "Mon" :: String
          2 -> "Tue"
          3 -> "Wed"
          4 -> "Thu"
          5 -> "Fri"
          6 -> "Sat"
          7 -> "Sun"
          _ -> error "Week l_day > 7"
  in case _periodFrequency period of
       Weekly -> dayS
       Monthly -> printf "%s-%02d" dayS monthDay


-- * Summarize 
-- ** Group by Key 
instance Semigroup k =>  Semigroup (Shift k) where
    a <> b = Shift
        (a^.shiftKey <> b^.shiftKey)
        (reduceWith1 getMin <$> nonEmpty ([a,b] ^.. each.startTime._Just))
        (a^.duration + b^.duration)
        (a^.cost + b^.cost)

instance Semigroup k => Semigroup (DeductionAndCost k) where
  a <> b = DeductionAndCost
                    (a ^. dacKey <> b ^. dacKey)
                    (bimap getSum getSum $ bimap Sum Sum (a ^. dacDac) <> bimap Sum Sum (b ^. dacDac))

instance (Ord p, Semigroup e) => Semigroup (EmployeeSummary p e) where
  a <> b = EmployeeSummary
              (a ^. sumEmployee <> b ^. sumEmployee )
              (a ^. finalPayment + b ^. finalPayment )
              (a ^. totalCost + b ^. totalCost )
              (a ^. net + b ^. net )
              (a ^. gross + b ^. gross )
              (Map.unionWith (+) (a ^. deductions) (b ^. deductions))
              (Map.unionWith (+) (a ^. netDeductions) (b ^. netDeductions))
              (Map.unionWith (+) (a ^. costs) (b ^. costs))
              (Map.unionWith (+) (a ^. totalHours) (b ^. totalHours))

-- * Locking 


filterTimesheet :: (Shift (e, Day, ShiftType) -> Bool) --  ^ Shift filter
                -> (DeductionAndCost (p, e) -> Bool) --  ^ DAC filter
                -> Timesheet p e
                -> Maybe (Timesheet p e) --  ^ returns nothing if everything has been filtered
filterTimesheet sFilter iFilter ts = 
  case  ( filter sFilter (_shifts ts)
        , filter iFilter (_deductionAndCosts ts)
        ) of
    ([], []) -> Nothing
    (ss, dacs) -> Just ts {_shifts = ss, _deductionAndCosts = dacs}
