{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses , FunctionalDependencies , FlexibleInstances #-}
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
type Amount = Decimal
type Duration = Decimal
type Hour = Decimal
-- * Data
-- | An employee.
data Employee = Employee
    { _firstName :: String
    , _surname   :: String
    , _nickName  :: String -- ^ use to designate an Employee. Must be unique
    , _defaultHourlyRate :: Maybe Amount
    , _payrollId :: Int
    } deriving (Show, Eq, Ord)

sku :: Employee -> String
sku e = "NW-"++ _nickName e
makeClassy ''Employee

-- | The main data is a shift, ie a continous amount of time worked
-- The key represent a way to identify a shift (Employe/Employe,Day) ...
data ShiftType = Work | Holiday deriving (Show, Eq, Ord)
makeClassy ''ShiftType
data Shift k = Shift
    { _shiftKey :: k
    , _startTime :: Maybe TimeOfDay
    , _duration :: Duration
    , _cost :: Amount
    } deriving (Show, Eq)


makeClassy ''Shift
instance HasEmployee k => HasEmployee (Shift k) where
    employee =  shiftKey . employee

type ShiftDE = Shift (Employee, Day, ShiftType)

instance HasEmployee e => HasEmployee (e, a) where
    employee = _1.employee

instance HasEmployee e => HasEmployee (e, a, b) where
    employee = _1.employee

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

-- |
data Timesheet = Timesheet
    { _shifts :: [ShiftDE]
    , _weekStart :: Day
    }
    deriving (Show, Eq)

makeClassy ''Timesheet

newTimesheet :: Day -> Timesheet
newTimesheet day = Timesheet [] day

-- * Summarize
-- ** Group by Key
instance Semigroup k =>  Semigroup (Shift k) where
    a <> b = Shift 
        (a^.shiftKey <> b^.shiftKey)
        (reduceWith1 getMin <$> nonEmpty ([a,b] ^.. each.startTime._Just))
        (a^.duration + b^.duration)
        (a^.cost + b^.cost)

