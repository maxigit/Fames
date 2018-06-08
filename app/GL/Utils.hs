module GL.Utils where
import ClassyPrelude
import GL.Payroll.Settings
import Data.Time.Calendar(addDays, addGregorianMonthsClip, gregorianMonthLength)
import           Data.Time ( Day
                           , LocalTime
                           , TimeOfDay
                           , makeTimeOfDayValid
                           -- , addDays
                           , timeOfDayToTime
                           , formatTime
                           , fromGregorian
                           , toGregorian
                           , diffDays
                           -- , addGregorianMonthsClip
                           )

-- * Date calculator
calculateDate :: DateCalculator -> Day -> Day
calculateDate (DayOfMonth target cutoff) day = let
  (y, m, d) = toGregorian day
  new' = fromGregorian y m target
  new = if cutoff > target
        then addGregorianMonthsClip 1 new'
        else new'
  in if d >= cutoff -- next month
     then addGregorianMonthsClip 1 new
     else new
calculateDate (AddDays n) day = addDays (fromIntegral n) day
calculateDate (AddMonths n) day = addGregorianMonthsClip (fromIntegral n) day
calculateDate (NextDayOfWeek target cutoff) day = let
  d = dayOfWeek day
  -- to compare the cut off we need to use the target day as the begining of the week
  -- ie, cutoff of Sunday, for Wednesday day, means Monday should go to next week
  -- even so Monday is < Sunday
  offset = (fromEnum target - fromEnum d) `mod` 7 -- in our example 3 - 1 = 2
  cutoffOffset = (fromEnum target - fromEnum cutoff) `mod` 7 -- but sundays is 3 - 7 = 3
  -- monday is 
  -- sunday: offset = 3 - 7 = 3, cutoffO = 3 - 7 = 3, 3 <= 3 ? => YES final offset = 10 Ok
  -- Thurday: offset = 3 - 4 = 6, cutoffO = 3 - 7 = 0, 6 < 3 ? NO => final offset = 6 Ok
  in if offset <= cutoffOffset
     then addDays (fromIntegral $ offset + 7) day
     else addDays (fromIntegral $ offset) day
calculateDate EndOfMonth day = let
  (y, m, _) = toGregorian day
  in fromGregorian y m (gregorianMonthLength y m)
calculateDate (EndOfWeek target) day = nextWeekDay target day
calculateDate EndOfYear day = fromGregorian year 12 31 where (year,_,_) = toGregorian day


dayOfWeek :: Day -> DayOfWeek
dayOfWeek day = toEnum . (`mod` 7) $ fromEnum day + 2

-- | Find the first day of week prior the given  date
-- Return the actual date if the possible
previousWeekDay :: DayOfWeek -> Day -> Day
previousWeekDay weekDay day = calculateDate (NextDayOfWeek weekDay weekDay) (addDays (-7) day)

-- | Find the first day of week after the given  date
-- Return the actual date if it matches
nextWeekDay :: DayOfWeek -> Day -> Day
nextWeekDay  weekDay day = calculateDate (NextDayOfWeek weekDay weekDay) (addDays (-1) day)
  

-- * Helper (not using calculateDate)
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
-- | beginning of month period starting at given day
-- Ex : 5 (07 Mar) -> 5 Mar
--    5 (03 Mar) -> 5 Feb
previousMonthStartingAt :: Int -> Day -> Day
previousMonthStartingAt d day = addGregorianMonthsClip (-1) $  calculateDate (DayOfMonth d d) day

-- * Date Folding
-- | Used to project a given a day into a given period. Useful for comparing or charting
-- sales for different year.
-- example 2017/03/01 for year starting on 2018/08/01 will be projected to 2018/03/01 (day) period_start 2017/01/01
  
-- | Argument type to not mixup adjustTAxYear arguments
newtype Start = Start Day -- to mixup adjustTaxYear
  deriving (Show, Eq, Ord)
data PeriodFolding
  = FoldYearly Day  -- starting day
  -- PreviousPeriod -- use the length of the start and end period
  | FoldMonthly
  | FoldWeekly
  deriving Show



-- compute the period and the new date within the current period
foldTime :: PeriodFolding -> Day -> (Day, Start)
foldTime (FoldYearly yearStart) day = let
  (periodYear, periodMonth, periodDay) = toGregorian yearStart
  (dayYear, dayMonth, dayDay) = toGregorian day
  newPeriodYear = if (dayMonth, dayDay) >= (periodMonth, periodDay)
            then dayYear
            else dayYear - 1
  newDayYear = periodYear+ (newPeriodYear - dayYear) -- adjustment
  in ( fromGregorian newDayYear dayMonth dayDay
     , Start (fromGregorian newPeriodYear periodMonth periodDay)
     )

