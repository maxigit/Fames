module Handler.WH.Legacy.Box where

import Prelude
import Data.List.Split (splitOn)
import Data.List (sortBy, partition)
import Data.Function (on)
import Control.Monad (forM, forM_)
import System.Environment (getArgs)
import Numeric (showFFloat)
import qualified Data.Map as Map
import Data.Map(Map)
import Control.Monad.State --  (execWriter)
import Text.Printf (printf)

data Dimension = Dimension {
    bLength ::  Double -- X
    ,bWidth :: Double -- Y
    ,bHeigth :: Double  -- Z
    } deriving Show

rotate :: Dimension -> Dimension
rotate (Dimension l w h) = Dimension h w l

tiltForward :: Dimension -> Dimension
tiltForward (Dimension l w h) = Dimension l h w

area :: Dimension -> Double
area (Dimension l w h) = l*h

(//) :: Dimension -> Dimension -> Int
(Dimension l w h) // (Dimension l' w' h') = (fdiv l l') * (fdiv h h') where
    fdiv x y = floor (x/y)

class Dimensionable a where
    dimension :: a -> Dimension

type Style = String

data Box = Box {
    boxDimension :: Dimension
    ,style :: Style
    ,number :: Int
    ,quantity :: Int
    } deriving Show

totalQuantity :: Box -> Int
totalQuantity box = number box * quantity box
instance Dimensionable Box where
    dimension box = boxDimension box

tiltBoxForward :: Box -> Box
tiltBoxForward box = box {boxDimension = (tiltForward.boxDimension) box}
readBoxes :: String -> IO [Box]
readBoxes filename = do
    file <- readFile filename
    return $ map lineToBox (tail $ lines file) 

lineToBox :: String -> Box
lineToBox line = let
    splits = splitOn "," line
    in case splits of
        [style, n, quantity, l, w, h] ->  Box (Dimension (read l) (read w) (read h)) style (read n) (read quantity)
        otherwise -> error $ "line '"++line++"' not well formatted"
----------------------------------------------------------------------
data Bay = Bay {
        bayName :: String,
        bayDimension :: Dimension
    } deriving Show

instance Dimensionable Bay where
    dimension bay = bayDimension bay

bays :: [Bay]
bays = [
        Bay "7B" $ Dimension 270 0 145
        ,Bay "A72" $ Dimension 180 0 145
        ,Bay "U6" $ Dimension 184 0 150
        ,Bay "Gav" $ Dimension 320 0 90
    ]

----------------------------------------------------------------------
data Orientation = Flat | Turned deriving (Show, Eq)
data Bulk = Bulk {
        orientation :: Orientation
        ,perBay :: Int
        ,bulk :: Double
    } deriving (Show, Eq)

    
instance Ord Bulk where
    compare a b = compare (bulk a) (bulk b)

bestOrientation :: Box -> Bay -> Bulk
bestOrientation box bay = let 
    boxDim = dimension box
    bayDim = dimension bay
    areaRatio = area boxDim / (area bayDim)
    normalNumber = bayDim // boxDim
    turnedNumber = bayDim // (rotate boxDim)
    in if (turnedNumber > normalNumber)
        then Bulk Turned turnedNumber ((fromIntegral turnedNumber)*areaRatio) 
        else Bulk Flat normalNumber  ((fromIntegral normalNumber)*areaRatio)

bestBay :: [Bay] -> Box -> [(Bay, Bulk)]
bestBay bays box = reverse $ sortBy (compare `on` snd) $ zip bays $ map (bestOrientation box) bays where

----------------------------------------------------------------------
----------------------------------------------------------------------
report :: Box -> IO ()
report box =
    let bulks = bestBay bays box
    in do 
        putStr $ (style box) ++","++(show $ number box)
        mapM_ (reportBulk (number box)) (bestBay bays box)
        putStrLn ""

reportBulk n (bay, bulk_) = do
    putStr $ ","
        ++(bayName bay)
        ++","
        ++(show $ orientation bulk_)
        ++","
        ++(show $ perBay bulk_)
        ++" [" 
        ++(show $ n `divp` (perBay bulk_))
        ++"]," 
        ++(showFFloat (Just 0)  (100*(bulk bulk_)) "%")
    where divp _ 0 = 0
          divp p q = (p+q-1) `div` q

main' = do
    args <- getArgs
    case args of
        [filename] ->  do
            boxes' <- readBoxes filename
            let boxes = map tiltBoxForward boxes'
            
            mapM_ report boxes
        otherwise -> error "please supply an file as argument"

main = do
    args <- getArgs
    case args of 
        [filename] -> do
            boxes <- readBoxes filename
            mapM_ printSlice (findSlices boxes)
        
        otherwise -> error "please supply a file as argument"

-- | Find how to unload a container by "slices"
-- ie columns of a similar slices
type Slice = ( Style
             , Int -- ^ width in box
             , Int -- ^ Height in box
             , Double -- ^ width
             , Double -- ^ Depth
             , Double -- ^ cumulative Widt
             )
findSlices :: [Box] -> [Slice]
findSlices boxes = let  
    byStyle = foldr insertWith (Map.empty) boxes
    insertWith box m = Map.insertWith append (style box) box m
    -- add boxes and keep the dimensions of the most numerous one
    append b b' | totalQuantity b < totalQuantity b' = append b' b
                | otherwise = b { number = number b
                                        + (totalQuantity b' `divup` quantity b)
                                }

    maxHeigh = 300
    maxLength = 200
    toSlice b = let d = boxDimension b
                    h = floor $ maxHeigh / bHeigth d
                    l = floor $ maxLength / bLength d
                    w = number b `divup` (h*l)
                    l' = number b `divup` (h*w)
                in (style b, w, h, bWidth d * fromIntegral w/100, fromIntegral l'*bLength d)
    preSlices = map toSlice $ Map.elems byStyle
    -- splitNarrow
    (narrow, wide) = partition (\(_,_,_,_,l)  -> l< 120) preSlices
    -- we need slices to be sorteb by
    [narrowS, wideS]= map (sortBy (compare `on` priority))
                          [narrow, wide]
    priority (s, _, _, _, _) = drop 5 s
    
    run sorted =  evalState (mapM (\(s, n, nh, w, l) -> do
                                acc <- get
                                let acc' = resetif 8 (acc+w+0.1)
                                
                                put acc'
                                return $ (s, n, nh, w, l, acc)
                        ) sorted
                    ) 0
    in concatMap run [narrowS++ wideS]
    

resetif threshold v = if v >= threshold 
                         then 0
                         else v
                
printSlice :: Slice -> IO ()
printSlice (st, n, nh, w, d ,cw) = printf "%-15s x %2d  |  %5.2f - %5.2f (%2.0f) \n"
                                      st n cw (cw + w) d
                                                 
        
        
divup p q = (p + q-1) `div` q
