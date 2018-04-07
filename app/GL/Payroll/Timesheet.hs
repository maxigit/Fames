{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses , FunctionalDependencies , FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | This script read a "fast" time sheet format
-- and convert it FA textcart, PAYROO timesheet.csv
-- and generate report.
module GL.Payroll.Timesheet  where

import Prelude
import           Data.Decimal
import           Data.Time ( Day
                           , LocalTime
                           , TimeOfDay
                           , makeTimeOfDayValid
                           , addDays
                           , timeOfDayToTime
                           , formatTime
                           ,fromGregorian
                           , toGregorian
                           , diffDays
                           , addGregorianMonthsClip
                           )
import Data.Time.Calendar.WeekDate (toWeekDate)
import qualified Data.Time as Time
import           Control.Applicative

import qualified Text.Parsec as P
import Data.Maybe

import Lens.Micro hiding(index)
import Lens.Micro.TH
import Data.Semigroup
import Data.List.NonEmpty(nonEmpty)
import Data.Semigroup.Generator(reduceWith1)
import Data.Semigroup.Reducer(Reducer(..))
import Data.Ord(comparing)
import Data.Function(on)
import Text.Printf(printf)
import Data.These
import Data.Bifunctor (bimap)
import qualified Data.Map as Map
import Data.Map(Map)
import Locker
import Data.Text (Text)

-- * Type alias
type Amount = Locker [Text] Double
type Duration = Double
type Hour = Double
-- * Data
-- ** Employee
-- | An employee.
data Employee = Employee
    { _nickName  :: String -- ^ use to designate an Employee. Must be unique
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

-- hourlyRate :: Getter (Shift k) Amount
hourlyRate = to $ (/) <$> (^.cost) <*> (pure . (^.duration))

-- | Deduction and costs
data DeductionAndCost key = DeductionAndCost
  { _dacKey :: key
  , _dacDac :: These Amount Amount
  } deriving (Eq, Read, Show, Functor, Foldable, Traversable)
makeClassy ''DeductionAndCost

dacDeduction = dacDac . here
dacCost = dacDac . there
dacTotal dac =  go dacDeduction +  go dacCost where go f = fromMaybe 0 (dac ^? f)

data PayrollFrequency = Weekly | Monthly deriving (Eq, Read, Show, Enum, Bounded, Ord)
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
newTimesheet frequency day = Timesheet [] day frequency []

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

  

-- | Argument type to not mixup adjustTAxYear arguments
newtype Start = Start Day -- to mixup adjustTaxYear
  deriving (Show, Eq, Ord)
data Period = Period
  { _periodFrequency :: PayrollFrequency
  , _pStart :: Start
  } deriving (Show, Eq, Ord)
makeLenses ''Period

-- * Period Calculator
-- Adjust the start tax year to be in the
-- same tax year as the given day
adjustTaxYear :: Start -> Day -> Day
adjustTaxYear (Start taxStart) start = let
  (_taxYear, taxMonth, taxDay) = toGregorian taxStart
  (startYear, startMonth, startDay) = toGregorian start
  newYear = if (startMonth, startDay) >= (taxMonth, taxDay)
            then startYear
            else startYear - 1
  in fromGregorian newYear taxMonth taxDay

weekNumber :: Start -> Day -> (Integer, Int)
weekNumber taxStart start = let
  -- first we need to adjust
  adjusted = adjustTaxYear taxStart start
  days = diffDays start adjusted
  in (toYear adjusted , fromIntegral $ days  `div` 7 +1)

monthNumber :: Start -> Day -> (Integer, Int)
monthNumber taxStart start = let
  (taxYear, taxMonth, taxDay) = toGregorian (adjustTaxYear taxStart start)
  (_startYear, startMonth, startDay) = toGregorian start
  deltaMonth =  startMonth - taxMonth
  plusOneIf (y,d) = if startDay < taxDay
            then (y,d)
            else (y,d+1)
  adjust12 (y,d) = if d < 1
               then (y, d +12)
               else (y,d)
  in adjust12 $ plusOneIf (taxYear, deltaMonth)

toYear :: Day -> Integer
toYear d = y where (y, _, _ ) = toGregorian d
-- | End of a period
periodEnd :: Timesheet p e -> Day
periodEnd ts = let
  day = _periodStart ts
  in case _frequency ts of
       Weekly -> addDays 6 day
       Monthly -> addDays (-1) (addGregorianMonthsClip 1 day )

-- | Adjust period year so it includes the given date
adjustPeriodYearFor :: Day -> Period -> Period
adjustPeriodYearFor day period = let
  newStart = adjustTaxYear (period ^. pStart) day
  in period & pStart .~ Start newStart


periodNameFor :: Day -> Period ->  String
periodNameFor day period = let
  (format, n) = case _periodFrequency period of
    Weekly -> ("%02d", snd $ weekNumber (_pStart period ) day )
    Monthly ->  ("M%02d", snd $  monthNumber (_pStart period ) day)
  in printf format n

refFormatter :: (Integer -> String -> String)
             -> Period
             -> Day
             -> String
refFormatter formatter period day = let
  period' = adjustPeriodYearFor day period
  Start start =  period' ^. pStart
  (year, _, _) = toGregorian start
  in formatter year (periodNameFor day period')

-- | Only there for testing reason
-- Allows to resave transactions even if they already exists
-- in front accountin
referencePrefix = "" :: String
shortRef :: Period -> Day -> String
shortRef = refFormatter go where
  go year name = printf "%s%02d%s" referencePrefix (year `mod` 100) name

longRef :: Period -> Day -> String
longRef = refFormatter go where
  go year name = printf "%s%d/%s" referencePrefix year name


dayRef period day = let
  (_, _, weekDay) = toWeekDate day
  (_, _, monthDay) = toGregorian day
  dayS = case weekDay of
          1 -> "Mon" :: String
          2 -> "Tue"
          3 -> "Wed"
          4 -> "Thu"
          5 -> "Fri"
          6 -> "Sat"
          7 -> "Sun"
          _ -> error "Week day > 7"
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
