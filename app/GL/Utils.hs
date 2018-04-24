module GL.Utils where
import ClassyPrelude
import GL.Payroll.Settings
import Data.Time.Calendar(addDays, addGregorianMonthsClip)

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
  

-- | beginning of month period starting at given day
-- Ex : 5 (07 Mar) -> 5 Mar
--    5 (03 Mar) -> 5 Feb
previousMonthStartingAt :: Int -> Day -> Day
previousMonthStartingAt d day = addGregorianMonthsClip (-1) $  calculateDate (DayOfMonth d d) day