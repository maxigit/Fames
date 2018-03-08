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
                           )
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

-- * Type alias
type Amount = Double
type Duration = Double
type Hour = Double
-- * Data
-- | An employee.
data Employee = Employee
    { _nickName  :: String -- ^ use to designate an Employee. Must be unique
    , _defaultHourlyRate :: Maybe Amount
    } deriving (Show, Eq, Ord)

makeClassy ''Employee

-- ** Payroo
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
hourlyRate = to $ (/) <$> (^.cost) <*> (^.duration)

data PayrollFrequency = Weekly | Monthly deriving (Eq, Read, Show, Enum, Bounded, Ord)
-- | A Timesheet. A functor over employee : allows
-- to easily modify the information relative to an employee
data Timesheet e = Timesheet
    { _shifts :: [Shift (e, Day, ShiftType)]
    , _periodStart :: Day
    , _frequency :: PayrollFrequency
    }
    deriving (Show, Eq, Functor, Foldable, Traversable)

makeClassy ''Timesheet
    

newTimesheet :: PayrollFrequency -> Day -> Timesheet e
newTimesheet frequency day = Timesheet [] day frequency

-- * Period Calculator
-- Adjust the start tax year to be in the
-- same tax year as the given day
adjustTaxYear :: Day -> Day -> Day
adjustTaxYear taxStart start = let
  (taxYear, taxMonth, taxDay) = toGregorian taxStart
  (startYear, startMonth, startDay) = toGregorian start
  newYear = if (startMonth, startDay) >= (taxMonth, taxDay)
            then startYear
            else startYear - 1
  in fromGregorian newYear taxMonth taxDay

weekNumber :: Day -> Day -> (Integer, Int)
weekNumber taxStart start = let
  -- first we need to adjust 
  adjusted = adjustTaxYear taxStart start
  days = diffDays start adjusted
  in (toYear adjusted , fromIntegral $ days  `div` 7 +1)

monthNumber :: Day -> Day -> (Integer, Int)
monthNumber taxStart start = let
  (taxYear, taxMonth, taxDay) = toGregorian (adjustTaxYear taxStart start)
  (startYear, startMonth, startDay) = toGregorian start
  in (taxYear, startMonth - taxMonth + 1)

toYear :: Day -> Integer
toYear d = y where (y, _, _ ) = toGregorian d

-- * Summarize
-- ** Group by Key
instance Semigroup k =>  Semigroup (Shift k) where
    a <> b = Shift 
        (a^.shiftKey <> b^.shiftKey)
        (reduceWith1 getMin <$> nonEmpty ([a,b] ^.. each.startTime._Just))
        (a^.duration + b^.duration)
        (a^.cost + b^.cost)
