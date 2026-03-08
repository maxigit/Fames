module GL.Payroll.Nest
( Header(..)
, Detail(..)
, makeCsv
) where

import Prelude

import Data.List (intercalate)
import qualified GL.Payroll.Timesheet as TS
import Data.Time.Format    (defaultTimeLocale)
import Data.Time(Day, formatTime)
import Text.Printf(printf)

data Header = Header
  { employerRef :: String
  , earningsPeriodEnd :: Day
  , paymentSource :: String
  -- , paymentDueDate :: Day
  , frequency :: TS.PayrollFrequency
  , earningsPeriodStart :: Day
  -- , totalMembers :: Int
  }

data Detail = Detail
  { surname :: String
  , alternativeId :: String
  , earnings :: Double
  , employee :: Double
  , employer :: Double
  }

formatDate d = formatTime defaultTimeLocale "%Y-%m-%d" d

headerRow :: Header -> String
headerRow h =
  intercalate ","
    [ "H"                                 -- 1 it is a HEADER record (as opposed to the next line, CS
    , employerRef h                       -- 2
    , "CS"                                -- 3
    , formatDate $ earningsPeriodEnd h    -- 4
    , paymentSource h                     -- 5
    , "" -- formatDate $ paymentDueDate h -- 6
    , case frequency h of                 -- 7
           TS.Monthly -> "Monthly"
           freq -> error $ show freq <> " not supported in NEST yet."
    , ""                                     -- 8
    , ""                                     -- 9
    , formatDate $ earningsPeriodStart h     -- 10
    , "Y"                                    -- 11 Any missing employee seen marked as no contributions due
    ]

money :: Double -> String
money = printf "%.2f"

detailRow :: Detail -> String
detailRow d =
  intercalate ","
    [ "D"                     -- 1
    , surname d               -- 2
    , ""                      -- 3
    , alternativeId d           -- 4
    , money (earnings d)      -- 5
    , ""                      -- 6
    , money (employer d)      -- 7
    , money (employee d)      -- 8
    , reason                  -- 9
    , ""                      -- 10
    , ""                      -- 11
    , ""                      -- 12
    , ""                      -- 13
    , ""                      -- 14
    , ""                      -- 15
    , ""                      -- 16
    , ""                      -- 17
    , ""                      -- 18
    , ""                      -- 19
    , ""                      -- 20
    , ""                      -- 21
    ]
    where reason = if abs (employer d) + abs (employee d ) <1e-2 -- 0
                   then "5" -- member as insufficient earnings
                   else ""


trailerRow :: [Detail] -> String
trailerRow ms =
  intercalate ","
    [ "T"
    , show (length ms)
    , "4"
    ]


makeCsv :: Header -> [Detail] -> [String]
makeCsv header details =
      [headerRow header]
        ++ map detailRow details
        ++ [trailerRow details]
