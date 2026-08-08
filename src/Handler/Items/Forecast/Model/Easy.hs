module Handler.Items.Forecast.Model.Easy
where

import Import
import Handler.CsvUtils
import Text.Read(readPrec, (+++))
import GL.Payroll.Settings (DateCalculator)

data Model 
     = Naive
     | PreviousYears Int
     | Previous EasyDay EasyDay (Maybe Int)
     | ForeachCategory Text Model -- ^ Apply the given model to each category separately
     | AfterForeachCategory Text Modifier Model -- ^ Apply the given model to each category separately
     | Null
     | FilterCategory Text [Text] Model
     | ExcludeCategory Text [Text] Model
     | CategoryCase Text [(Text, Model)] Model
     | PostCategoryCase Text [(Text, Modifier)] Modifier Model
     | Avg [Model]
     | AvgPresent [Model]
     | Max [Model]
     | Min [Model]
     | Median [Model]
     | Mod Modifier Model
     | Sum [Model]
     | IM Model -- real independent margin
     | HM Model -- used IM but not independant as we scale by style
     | ScaleBy [Text] Model Model
     | LimitBy [Text] Model Model Model
     | Mask Model Model
     | Delete0 Model
     | DeleteIf Modifier Model
     | NoveltyFromFuture Int -- inject SKU with 0 forecast from future years
     | InjectCategory Text
     | InjectCategoryValue Text Text
     | Ref Text
     | With [(Text, Model)] Model
     | Read Text
     deriving (Show, Read, Eq)
     
data Modifier
     = Reject
     | Id
     | Scale Double
     | AtMost Double
     | AtLeast Double
     | SetTo Double
     | Total
     | RoundUp Double
     | RoundDown Double
     | Round Double
     | Mean
     | EQ Double Modifier Modifier
     | NEQ Double Modifier Modifier
     | LT Double Modifier Modifier
     | LTE Double Modifier Modifier
     | GT Double Modifier Modifier
     | GTE Double Modifier Modifier
     deriving (Show, Read, Eq)

data EasyDay = EasyDay Day
                | EasyCalc DateCalculator 
                deriving (Show, Eq)


instance Read EasyDay where
   readPrec = readEasy +++ readCalculator
     where readEasy = do
                    s <- readPrec
                    AllFormatsDay d <- parseDay s
                    return $ EasyDay d
           readCalculator = EasyCalc <$> readPrec



data Profile
     = DefaultProfile
     | PerCategoryYear Text Int
     deriving (Show, Read, Eq)

