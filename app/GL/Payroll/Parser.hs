{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
-- The fast timesheet format allows
-- to enter quickly a timesheet.
-- For that, each day can be written a line and the parser
-- will detect the correct field, regardless of their order, depending on their *shape*.

-- | Parsing state. Keep default values which can be used over different lines_.
module GL.Payroll.Parser where
import Prelude hiding(read)
import Lens.Micro
import Lens.Micro.TH
import Control.Applicative
import Control.Monad (foldM)
import Data.Maybe
import Text.Regex.TDFA ((=~))
import GL.Payroll.Timesheet
import           Data.Time ( Day
                           , TimeOfDay
                           , makeTimeOfDayValid
                           , addDays
                           , timeOfDayToTime
                           , formatTime
                           )
import qualified Data.Time as Time
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Format    ( defaultTimeLocale, wDays)
import qualified Data.Map as Map
import Data.Map(Map)
import Text.Read (readMaybe)
import Data.Align (align)
import Locker

data Current = Current
        { _currentEmployee :: Maybe PayrooEmployee
        , _currentDay :: Maybe Day
        , _currentHourlyRate :: Maybe Amount
        , _currentTimesheet :: Timesheet String PayrooEmployee
        , _currentEmployeeMap :: Map String PayrooEmployee
        , _currentExternal :: Maybe String
        } deriving (Show)

makeClassy ''Current

initCurrent :: PayrollFrequency -> Day -> Current
initCurrent period start = Current Nothing Nothing Nothing  (newTimesheet period start) (Map.empty) Nothing

currentWeekStart :: (HasCurrent c, Functor f) => (Day -> f Day) -> c -> f c
currentWeekStart =  currentTimesheet . periodStart 

setEmploye :: PayrooEmployee -> Current -> Current
setEmploye emp u = u & currentEmployee ?~  emp
                     & currentDay ?~ u ^. currentWeekStart

-- find an existing employee by nickName
-- andd assign it as the current employee
-- current date to the beginning of the week
setEmployeByNickname :: String -> Current -> Maybe Current
setEmployeByNickname name u = do
    e <- Map.lookup name (u^.currentEmployeeMap)
    return $ setEmploye e u

-- | Add an PayrooEmployee to Current and check all the payrollId are unique
addNewEmployee :: PayrooEmployee -> Current -> Maybe Current
addNewEmployee emp u =  do
    if isJust (Map.lookup (emp^.nickName) (u^.currentEmployeeMap))
    then (error $ "PayrooEmployee " ++ emp^.nickName ++ " already exists" )
    else if emp^.payrollId `elem` (u^..currentEmployeeMap.traversed.payrollId) 
         then error $ "Payroll id " ++ (show $ emp^.payrollId ) ++ " already used"
         else Just $  u & currentEmployeeMap %~
                              Map.insert (emp^.nickName) emp
                        & setEmploye emp




-- Create a new shift and add it at the end of the timesheet
-- if all requireid information are present
-- we need to round the duration so that duration AND *hourly rate is a multiple * 0.01
--  we do a complicated formula :
addShift :: Duration -> Maybe TimeOfDay ->  Current -> Maybe Current
addShift = addShift' Work
addShift' :: ShiftType -> Duration -> Maybe TimeOfDay ->  Current -> Maybe Current
addShift' shiftType_ duration_ start u = do
    emp <- u ^. currentEmployee
    rate <- emp ^. defaultHourlyRate <|> u ^. currentHourlyRate
    let adjustedDuration = roundDuration (unsafeUnlock rate) <$> duration_
        cost_ = rate * adjustedDuration
    day_ <- u ^. currentDay
    let s = Shift (emp, day_, shiftType_) start adjustedDuration cost_
    return $  u & currentTimesheet . shifts %~ (++ [s])

roundDuration :: (RealFrac a1, RealFrac a2) => a1 -> a2 -> a2
roundDuration rate duration_ = let
  -- let take a rate of 7.48
  pences = floor (rate * 100) `mod` 100 -- 48
  -- we need to find x so, that x*.48  is multiple of 1
  -- gcd 100 48 = 4
  m = gcd 100 pences -- gcd 4 we need to round at .25
  -- 0.25 * 0.48 = 0.12. every multiple of 0.25 will give a whole number of pences
  -- worth case scenario, gcd = 1
  m' = fromIntegral m
  -- we have to round up so that the employee is not loosing out
  in fromIntegral (ceiling (duration_ * m')) / m'


-- | Find the given week day (Mon, Sun) within the current week (in Week mode)
-- | or the following the current day in Month mode
findWeekDay :: Current -> WeekDay -> Day
findWeekDay u wday = case dayToWeekDay sday of
    Nothing -> error ("can't find day of the week" ++ show (formatTime defaultTimeLocale "%a" sday))
    Just wstart -> let offset = (index wday - index wstart) `mod` 7
                   in  addDays (fromIntegral offset) sday
    where sday = case u ^. currentTimesheet . frequency of
            Weekly -> u ^. currentWeekStart -- next day from start
            Monthly -> fromMaybe (u^.currentWeekStart) (u ^. currentDay) -- next day from current one

                    


skipDay :: Current -> Current
skipDay u = u & currentDay.mapped %~ skip where
  skip d = addDays (case toWeekDate d of
    (_,_,5) -> 3 -- Friday -> Sunday
    _ -> 1
    ) d

-- Go back to the same day as the previous shift
-- Needs to be the same employe
backDay :: Current -> Maybe Current
backDay u = do
    e <- u ^. currentEmployee
    s <- u ^? lastShift
    if e == s ^. payrooEmployee 
    then return $ u & currentDay ?~ s ^. day 
    else return u


-- Lens or Prism ?  return a maybe
-- Current -> Maybe (Shift ...)
lastShift :: (HasCurrent c, Applicative f) =>
             (Shift (PayrooEmployee, Day, ShiftType)
          -> f (Shift (PayrooEmployee, Day, ShiftType)))
          -> c -> f c
lastShift = currentTimesheet.shifts._last
    
addDeductionAndCost :: Current -> (Maybe Amount) -> (Maybe  Amount) -> Maybe Current
addDeductionAndCost u costM deductionM = do
  e <- u ^. currentEmployee
  ext <- u ^. currentExternal
  cad <- align costM deductionM
  return $ u & currentTimesheet . deductionAndCosts %~ (++ [ DeductionAndCost (ext, e) cad])
  
-- ** Days 
data WeekDay = WeekDay { shortName :: String
                       , fullName :: String
                       , index :: Int
                       } deriving (Show, Eq)

weekDays :: [WeekDay]
weekDays = [WeekDay short full i | ((full, short),i)  <- zip (wDays defaultTimeLocale) [1..] ]

weekDaysAL :: [(String, WeekDay)]
weekDaysAL = [(shortName d ,d) | d <- weekDays ] 
        ++ [(fullName d ,d) | d <- weekDays ] 

parseWeekDay :: String -> Maybe WeekDay
parseWeekDay s = lookup s weekDaysAL

dayToWeekDay :: Day -> Maybe WeekDay
dayToWeekDay day_ = parseWeekDay dayName where
    dayName = formatTime defaultTimeLocale "%a" day_
-- * Tokenizer 


data Token = NameT String
           | PayrollIdT Int
           | RateT Amount
           | DayT Day
           | DurationT ShiftType Duration
           | RangeT TimeOfDay TimeOfDay
           | SkipT
           | FrequencyT PayrollFrequency 
           | PipeT
           | MultiplierT Double ShiftType Duration
           | WeekDayT WeekDay
           | DeductionAndCostT (Maybe Amount) (Maybe Amount)
           | ExternalT String
           deriving (Show, Eq)

read :: Read a => String -> String -> a
read msg s = fromMaybe (error $ "can't read " ++ show s ++ " to " ++ msg ) $ readMaybe s
readLockD :: String -> String -> Duration
readLockD msg s = lockD (read msg s)
readLockA :: String -> String -> Amount
readLockA msg s = lockA (read msg s)
lockA :: Double -> Amount
lockA = lock ["Payroll/internal/amount"]
lockD :: Double -> Duration 
lockD = lock ["Payroll/internal/duration"]
token :: String -> Either String Token
token s = case mapMaybe match cases of
            [] -> Left $ "Can't tokenize `" ++ show s ++ "`"
            (h:_) -> h
        where cases = [ ("[[:alpha:]][[:alnum:]]*", 
                        \(name, _) -> Right $ case (parseWeekDay name, readMaybe name) of
                                                (_, Just frequency_) -> FrequencyT frequency_
                                                (Just w, _) -> WeekDayT w
                                                (Nothing, Nothing) -> NameT name
                                        )
                      , ( hhmm ++ "-" ++ hhmm,
                           \(_,[hh,mm,hh',mm']) ->
                                maybe (Left $ "Invalid Time format for " ++ show s) Right
                                (do RangeT
                                 <$> (makeTimeOfDayValid
                                                (read "hh" hh)
                                                (read "mm" mm)
                                                0)
                                 <*> (makeTimeOfDayValid
                                                (read "hh'" hh')
                                                (read "mm'" mm')
                                                0)
                                                )
                        )
                      , ("([0-9]+(.[0-9+])?)x([^[:space:]]+)", \(_, [mul, _, leftover]) -> do -- Either
                                              subtoken <- token leftover
                                              case subtoken of
                                                    DurationT typ duration_ -> Right $ MultiplierT (read "mul" mul) typ duration_
                                                    _ -> Left $ s ++ " is not a valid duration_"
                        )
                      , ("([0-9]{1,2})h([0-9]{2})", \(_, [hh, mm]) -> Right $ DurationT Work ((readLockD "Dur:hh" hh + readLockD "Dur:mm" mm / 60)))
                      , ("!([^[:space:]]+)", (\(_, groups) -> do -- Either
                                              subtokens <- mapM token groups 
                                              case subtokens of
                                                   [DurationT _ duration_] -> Right (DurationT Holiday duration_)
                                                   _ -> Left $ s ++ " is not a valid duration_"
                                             ))
                      , ("#([0-9]+)", Right . PayrollIdT . read "payrollId" . head. snd)
                      , ("[0-9]+(.[0-9]+)?", Right . DurationT Work . readLockD "duration_" .fst)
                      , ("\\$([0-9]+(.[0-9]+)?)", Right . RateT . readLockA "rate" . head. snd)
                      , ( "([0-9]{4})/([0-9]{2})/([0-9]{2})"
                            , \(_, [y,m,d]) -> Right $ DayT (Time.fromGregorian 
                                                     (read "year/" y)
                                                     (read "mont/" m)
                                                     (read "day" d))
                        )
                      , ( "([0-9]{4})-([0-9]{2})-([0-9]{2})"
                            , \(_, [y,m,d]) -> Right $ DayT (Time.fromGregorian 
                                                     (read "year-"y)
                                                     (read "month-" m)
                                                     (read "day-" d))
                        )
                      , ("_", \_ -> Right SkipT)
                      , ("\\|", \_ -> Right PipeT)
                      , ("@([[:alpha:]][[:alnum:]]*)" , \(_, [name]) -> Right $ ExternalT name )
                      , ( "(" ++ amount ++ ")?\\^(" ++ amount ++ ")?" , \(_, [deduction, _dec,  cost_, _dec']) -> Right $ DeductionAndCostT (lockA <$> readMaybe deduction) (lockA <$> readMaybe cost_) )
                      ]
              amount = "-?[0-9]+(.[0-9]+)?"
              hhmm = "([0-9]{1,2}):([0-9]{2})"

              match (r,f) = case s =~ ("^"++r++"$") of
                    (_,[],_,_) -> Nothing
                    (p,m,s',captures) -> let _types = (p, s') :: (String, String)
                                            in Just (f (m,captures))

data Parser a = Parser

-- | Read a csv and produce a timesheet
-- It will also insert employee definition if provided
readFastTimesheet :: Maybe String -> String -> IO (Timesheet String PayrooEmployee)
readFastTimesheet epath path = do
    content <- readFile path
    content' <- case (epath, lines content) of
        (Nothing, contentLined) -> return contentLined
        (Just e, (header:body)) -> do
            employees <- readFile e
            return $ header : lines employees ++ body
        _ -> error "Employe without body"
    case parseFastTimesheet content' of
      Left e -> error e
      Right ts -> return ts
      


parseFastTimesheet :: [String] -> Either String (Timesheet String PayrooEmployee)
parseFastTimesheet lines_ = do -- Either
    tokenss <- mapM (mapM token . tokeninize) lines_ -- :: [[Either Token]]
    let go :: Current -> [[Token]] -> Either String (Timesheet String PayrooEmployee)
        go current_ tss = (^.currentTimesheet) <$> foldM processLine current_ tss
    case tokenss of
        ([DayT start]:tokenss') -> go (initCurrent Weekly start) tokenss'
        ([FrequencyT frequency_, DayT start]:tokenss') -> go (initCurrent frequency_ start) tokenss'
        _ -> Left "File should start with the week start date"

-- | Process a full line of tokens.
-- One line should correspond to only one operators.
-- Some parst of the states, are reset at the end of line.
processLine :: Current -> [Token] -> Either String Current
processLine u toks = case toks of
        -- set current user and reset currentDay
        [NameT name] -> maybe (Left $ "Employee "
                                    ++ name 
                                    ++ " doesn't exist"
                              )
                              Right
                              (setEmployeByNickname name u)
        [NameT name, PayrollIdT pid] -> 
            addNewEmployee' name name "" Nothing pid
        [NameT name, PayrollIdT pid, RateT rate] -> 
            addNewEmployee' name name "" (Just rate) pid
        [ NameT alias , NameT firstname , NameT surname_
            , PayrollIdT pid ] -> 
            addNewEmployee' alias firstname surname_ Nothing pid
        [ NameT alias , NameT firstname , NameT surname_
            , PayrollIdT pid, RateT rate ] -> 
            addNewEmployee' alias firstname surname_ (Just rate) pid
        -- set current hourly rate
        [RateT rate] -> Right $ u & currentHourlyRate ?~ rate
        -- set current date
        [DayT day_] -> Right $ u & currentDay ?~ day_
        [] -> Right u
        _  -> do
              u' <- foldM processShift u toks
              -- only update some attributes of global states
              -- only update the current day_ if the user has changed
              Right $ u & currentTimesheet.shifts
                             .~ u'^.currentTimesheet.shifts
                        & currentDay
                             .~ ( if u^.currentEmployee  == u'^.currentEmployee
                                  then u'
                                  else u
                                 ) ^. currentDay
                        & currentEmployee .~ u'^.currentEmployee
                        & currentTimesheet.deductionAndCosts .~ u' ^. currentTimesheet.deductionAndCosts

        where addNewEmployee' alias firstname surname_ rate pid =
                maybe (Left $ "Couldn't create new employee" ++ show (alias, firstname, surname_))
                      Right
                      ( addNewEmployee (PayrooEmployee firstname
                                               surname_
                                               pid
                                               (Employee alias rate)
                                      )
                        u
                      )

-- | update the current state accordingly to the given token
processShift :: Current -> Token -> Either String Current
processShift u t =  case t of
        (NameT name) -> maybe (Left $ "PayrooEmployee "
                                    ++ name 
                                    ++ " doesn't exist"
                              )
                              Right 
                              (setEmployeByNickname name u)
        (DayT day_ )  -> Right $ u & currentDay ?~ day_
        (RateT rate) -> Right $ u & currentHourlyRate ?~ rate
                          & currentEmployee.mapped %~ (\e -> e & defaultHourlyRate ?~ rate ) 
                           -- override employe hourly rate temporatily
        (DurationT stype dur) -> maybe (Left $ "can't add shift : " ++ show (u,t) ) Right
                            (skipDay <$> addShift' stype dur Nothing u)
        (RangeT t1 t2) -> maybe (Left $ "can't add shift : " ++ show (u,t) ) Right
                            (skipDay <$> addShift (diffTime t2 t1) (Just t1) u)
        (WeekDayT wday) -> Right $ u & currentDay ?~  (findWeekDay u wday)
        SkipT  -> Right $ skipDay u
        -- Goback to the day of the previous shift
        -- allow 2 shifts to be in the same day ex : 4|14:00-18:00 => 8.00
        PipeT  ->  Right $ fromMaybe u $ backDay u
        PayrollIdT _ -> Left "unexpected payroll Id"
        ExternalT name -> Right $ u & currentExternal ?~ name
        DeductionAndCostT deductionM costM  -> maybe (Left "Can't assign Deduction and Costs if current employee and external are not set")
                                       Right
                                       (addDeductionAndCost u deductionM costM)
        -- many days, apply it many times
        MultiplierT n stype dur | n <= 0 -> Right u
                                | n < 1 -> processShift u (DurationT stype (pure n*dur))
                                | otherwise -> do
                                    u' <- processShift u (DurationT stype dur)
                                    processShift u' ((MultiplierT (n-1) stype dur))
        FrequencyT freq -> Right $ u & currentTimesheet . frequency .~ freq

-- | Split a line into usefull tokens.
-- Basically split on spaces but also deals with '-' and '|'
tokeninize :: String -> [String]
tokeninize s0 = filter (not.null) $ go [] [] (stripL s0) where
    go words_ word (c:s)
        -- break on |
        | c == '|' = go (words_ ++ [word]++[[c]]) "" (s)
        | c == ' ' || c == '\t' = go (words_ ++ [word ]) []           (stripL s)
        | otherwise = go words_              (word++[c])  s
    go words_ word [] = words_ ++ [word]
    stripL [] = []
    stripL (c:cs) | c == ' ' || c == '\t' = stripL cs
                  | otherwise = (c:cs)

    

diffTime :: TimeOfDay -> TimeOfDay -> Duration
diffTime a b  = realToFrac ((timeOfDayToTime a - timeOfDayToTime b) / 3600)
