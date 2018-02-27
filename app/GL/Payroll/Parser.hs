{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
-- * FastTimesheet Parser
-- The fast timesheet format allows
-- to enter quickly a timesheet.
-- For that, each day can be written a line and the parser
-- will detect the correct field, regardless of their order, depending on their *shape*.

-- | Parsing state. Keep default values which can be used over different lines.
module GL.Payroll.Parser where
import Prelude
import Lens.Micro hiding(index)
import Lens.Micro.TH
import Control.Applicative
import Data.List(foldl')
import Data.Maybe
import           Data.Decimal
import Text.Regex.TDFA ((=~))
import GL.Payroll.Timesheet
import           Data.Time ( Day
                           , LocalTime
                           , TimeOfDay
                           , makeTimeOfDayValid
                           , addDays
                           , timeOfDayToTime
                           , formatTime
                           )
import qualified Data.Time as Time
import Data.Time.Format    ( defaultTimeLocale, wDays)
import qualified Data.Map as Map
import Data.Map(Map)

data Current = Current
        { _currentEmployee :: Maybe Employee
        , _currentDay :: Maybe Day
        , _currentHourlyRate :: Maybe Amount
        , _currentTimesheet :: Timesheet
        , _currentEmployeeMap :: Map String Employee
        } deriving (Show)

makeClassy ''Current

initCurrent weekDay = Current Nothing Nothing Nothing  (newTimesheet weekDay) (Map.empty)
currentWeekStart =  currentTimesheet . weekStart 

setEmploye :: Employee -> Current -> Current
setEmploye emp u = u & currentEmployee ?~  emp
                     & currentDay ?~ u ^. currentWeekStart

-- find an existing employee by nickName
-- andd assign it as the current employee
-- current date to the beginning of the week
setEmployeByNickname :: String -> Current -> Maybe Current
setEmployeByNickname name u = do
    e <- Map.lookup name (u^.currentEmployeeMap)
    return $ setEmploye e u

-- | Add an Employee to Current and check all the payrollId are unique
addNewEmployee :: Employee -> Current -> Maybe Current
addNewEmployee emp u =  do
    if isJust (Map.lookup (emp^.nickName) (u^.currentEmployeeMap))
    then (error $ "Employee " ++ emp^.nickName ++ " already exists" )
    else if emp^.payrollId `elem` (u^..currentEmployeeMap.traversed.payrollId) 
         then error $ "Payroll id " ++ (show $ emp^.payrollId ) ++ " already used"
         else Just $  u & currentEmployeeMap %~
                              Map.insert (emp^.nickName) emp
                        & setEmploye emp




-- Create a new shift and add it at the end of the timeshhet
-- if all requireid information are present
addShift :: Duration -> Maybe TimeOfDay ->  Current -> Maybe Current
addShift = addShift' Work
addShift' :: ShiftType -> Duration -> Maybe TimeOfDay ->  Current -> Maybe Current
addShift' shiftType duration start u = do
    emp <- u ^. currentEmployee
    rate <- emp ^. defaultHourlyRate <|> u ^. currentHourlyRate
    day <- u ^. currentDay
    let s = Shift (emp, day, shiftType) start duration (duration*rate)
        ts = u ^. currentTimesheet


    return $  u & currentTimesheet. shifts %~ (++ [s])

findWeekDay :: Current -> WeekDay -> Day
findWeekDay u wday = case dayToWeekDay sday of
    Nothing -> error ("can't find day of the week" ++ show (formatTime defaultTimeLocale "%a" sday))
    Just wstart -> let offset = (index wday - index wstart) `mod` 7
                   in  addDays (fromIntegral offset) (u ^. currentWeekStart)
    where sday = u ^. currentWeekStart 

                    


skipDay :: Current -> Current
skipDay u = u & currentDay.mapped %~ addDays 1

-- Go back to the same day as the previous shift
-- Needs to be the same employe
backDay :: Current -> Maybe Current
backDay u = do
    e <- u ^. currentEmployee
    s <- u ^? lastShift
    if e == s ^. employee 
    then return $ u & currentDay ?~ s ^. day 
    else return u


-- lastShift :: Current -> Maybe Shift
lastShift = currentTimesheet.shifts._last
    
-- ** Days
data WeekDay = WeekDay { shortName :: String
                       , fullName :: String
                       , index :: Int
                       } deriving (Show, Eq)

weekDays :: [WeekDay]
weekDays = [WeekDay short full i | ((full, short),i)  <- zip (wDays defaultTimeLocale) [1..] ]

weekDaysAL = [(shortName d ,d) | d <- weekDays ] 
        ++ [(fullName d ,d) | d <- weekDays ] 

parseWeekDay :: String -> Maybe WeekDay
parseWeekDay s = lookup s weekDaysAL

dayToWeekDay :: Day -> Maybe WeekDay
dayToWeekDay day = parseWeekDay dayName where
    dayName = formatTime defaultTimeLocale "%a" day
-- * Tokenizer


data Token = NameT String
           | PayrollIdT Int
           | RateT Amount
           | DayT Day
           | DurationT ShiftType Duration
           | RangeT TimeOfDay TimeOfDay
           | SkipT
           | PipeT
           | WeekDayT WeekDay
           deriving (Show, Eq)

token :: String -> Token
token s = case mapMaybe match cases of
            [] -> error $ "Can't parse " ++ s
            (h:_) -> h
        where cases = [ ("[a-zA-Z]\\S*", 
                        \(name, _) -> case parseWeekDay name of
                                        Nothing -> NameT name
                                        Just w -> WeekDayT w
                                        )
                      , ( hhmm ++ "-" ++ hhmm,
                           \(_,[hh,mm,hh',mm']) ->
                                fromMaybe (error $ "Invalid Time format for " ++ show s) 
                                (do RangeT
                                 <$> (makeTimeOfDayValid
                                                (read hh)
                                                (read mm)
                                                0)
                                 <*> (makeTimeOfDayValid
                                                (read hh')
                                                (read mm')
                                                0)
                                                )
                        )
                      , ("(\\d{1,2})h(\\d{2})", \(_, [hh, mm]) -> DurationT Work (roundTo 2 (read hh + read mm / 60)))
                      , ("!(\\S+)", (\(DurationT _ d) -> DurationT Holiday d) . token . head . snd)
                      , ("#(\\d+)", PayrollIdT . read . head. snd)
                      , ("\\d+(.\\d+)?", DurationT Work . read .fst)
                      , ("\\$(\\d+(.\\d+)?)", RateT . read . head. snd)
                      , ( "(\\d{4})([/-])(\\d{2})\\2(\\d{2})"
                            , \(_, [y,_,m,d]) -> DayT (Time.fromGregorian 
                                                     (read y)
                                                     (read m)
                                                     (read d))
                        )
                      , ("_", const SkipT)
                      , ("\\|", const PipeT)

                      ]
              hhmm = "(\\d{1,2}):(\\d{2})"

              match (r,f) = case s =~ ("^"++r++"$") of
                    (_,[],_,_) -> Nothing
                    (p,m,s,captures) -> let types = (p, s) :: (String, String)
                                            in Just (f (m,captures))

data Parser a = Parser

-- | Read a csv and produce a timesheet
-- It will also insert employee definition if provided
readFastTimesheet :: Maybe String -> String -> IO Timesheet
readFastTimesheet epath path = do
    content <- readFile path
    content' <- case (epath, lines content) of
        (Nothing, contentLined) -> return contentLined
        (Just e, (header:body)) -> do
            employees <- readFile e
            return $ header:lines employees++body
        _ -> error "Employe without body"
    return $ parseFastTimesheet content'


parseFastTimesheet :: [String] -> Timesheet
parseFastTimesheet lines = let
    tokenss = map token . tokeninize <$> lines :: [[Token]]
    go :: Current -> [[Token]] -> Timesheet
    go current tss = (^.currentTimesheet) $ foldl' processLine current tss
    processLine :: Current -> [Token] -> Current
    processLine u toks = case toks of
        -- set current user and reset currentDay
        [NameT name] -> fromMaybe (error $ "Employee "
                                            ++ name 
                                            ++ " doesn't exist"
                                   )
                                   (setEmployeByNickname name u)
        [NameT name, PayrollIdT pid] -> 
            addNewEmployee' name "" name Nothing pid
        [NameT name, PayrollIdT pid, RateT rate] -> 
            addNewEmployee' name "" name (Just rate) pid
        [ NameT alias , NameT firstname , NameT surname
            , PayrollIdT pid ] -> 
            addNewEmployee' alias firstname surname Nothing pid
        [ NameT alias , NameT firstname , NameT surname
            , PayrollIdT pid, RateT rate ] -> 
            addNewEmployee' alias firstname surname (Just rate) pid
        -- set current hourly rate
        [RateT rate] -> u & currentHourlyRate ?~ rate
        -- set current date
        [DayT day] -> u & currentDay ?~ day
        [] -> u
        _  -> let u' = foldl' processShift u toks
              -- only update some attributes of global states
              -- only update the current day if the user has changed
              in u & currentTimesheet.shifts
                     .~ u'^.currentTimesheet.shifts
                   & currentDay
                     .~ ( if u^.currentEmployee  == u'^.currentEmployee
                          then u'
                          else u
                         ) ^. currentDay
                   & currentEmployee .~ u'^.currentEmployee

        where addNewEmployee' alias firstname surname rate pid =
                fromMaybe (error "Should't happend")
                          (addNewEmployee (Employee firstname
                                               surname
                                               alias
                                               rate
                                               pid)
                                               u)
    processShift :: Current -> Token -> Current
    processShift u t =  case t of
        (NameT name) -> fromMaybe (error $ "Employee "
                                            ++ name 
                                            ++ " doesn't exist"
                                   )
                                   (setEmployeByNickname name u)
        (DayT day )  -> u & currentDay ?~ day
        (RateT rate) -> u & currentHourlyRate ?~ rate
                          & currentEmployee.mapped %~ (\e -> e & defaultHourlyRate ?~ rate ) 
                           -- override employe hourly rate temporatily
        (DurationT stype dur) -> fromMaybe (error $ "can't add shift : " ++ show (u,t) ) 
                            (skipDay <$> addShift' stype dur Nothing u)
        (RangeT t1 t2) -> fromMaybe (error $ "can't add shift : " ++ show (u,t) ) 
                            (skipDay <$> addShift (diffTime t2 t1) (Just t1) u)
        (WeekDayT wday) -> u & currentDay ?~  (findWeekDay u wday)
        SkipT  -> skipDay u
        -- Goback to the day of the previous shift
        -- allow 2 shifts to be in the same day ex : 4|14:00-18:00 => 8.00
        PipeT  ->  fromMaybe u $ backDay u
        PayrollIdT _ -> error "unexpected payroll Id"
    in case tokenss of
        ([DayT weekDay]:tokenss') -> go (initCurrent weekDay) tokenss'
        otherwise -> error $ "File should start with the week start date"

-- | Split a line into usefull tokens.
-- Basically split on spaces but also deals with '-' and '|'
tokeninize :: String -> [String]
tokeninize s = filter (not.null) $ go [] [] (stripL s) where
    go words word (c:s)
        -- break on |
        | c == '|' = go (words ++ [word]++[[c]]) "" (s)
        -- don't break on - and remove trailing space and stick
        -- it to the previous word
        | c == '-' =  let (words', word') = if null word
                                            then (init words, last words)
                                            else (words, word)
                      in go words' (word'++[c]) (stripL s)
        | c == ' ' || c == '\t' = go (words ++ [word ]) []           (stripL s)
        | otherwise = go words              (word++[c])  s
    go words word [] = words ++ [word]
    stripL [] = []
    stripL (c:cs) | c == ' ' || c == '\t' = stripL cs
                  | otherwise = (c:cs)

    

diffTime :: TimeOfDay -> TimeOfDay -> Duration
diffTime a b  = (realFracToDecimal 2 $ (timeOfDayToTime a - timeOfDayToTime b) / 3600)
