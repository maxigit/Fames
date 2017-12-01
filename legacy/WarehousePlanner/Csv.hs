-- | Miscelaneous functions to read and write shelves and boxes from csv
module WarehousePlanner.Csv where

import WarehousePlanner.Base
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as Vec
import Control.Monad
import Data.List.Split (splitOn)
import Data.List(nub, union, groupBy, sortBy)
import Data.Char(toLower)
import Data.Ord(comparing)
import Prelude
import Text.Read(readMaybe)
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Text.ParserCombinators.Parsec.Token as P
import Control.Applicative
import Data.Maybe(maybeToList)

import Debug.Trace
readShelves :: String -> IO (WH [Shelf s] s)
readShelves filename = do
    csvData <- BL.readFile filename

    case Csv.decode  Csv.HasHeader csvData of
        Left err ->  do putStrLn err; return (return [])
        Right (rows) -> return $ do
            let v = Vec.forM rows $ \(name, first, last, description, l, w, h) ->
                        let dim = Dimension l w h
                            dim' = Dimension l (w+10) h
                            last' = maybe first id last
                            types = (first, description) :: (Int, String)
                        in forM [first..last'] $ \i ->
                                        let shelfName = name ++ "." ++ show i
                                        in newShelf shelfName Nothing dim dim' DefaultOrientation ColumnFirst

            concat `fmap` (Vec.toList `fmap` v)

--  | Read shelves and allow formula between different shelves
--  example A,,100,200,100,
--          B,,100,250-{A},{A}
--  by default we use the same field as the current one of a refered object.
--  In the previous example 250-{A} means "width of A"
--  Reference must be between bracket
--  Reference can also do basic subsitution with the current shelf name.
--  example for A123
--  {B%} -> B123
--  {B_+-} -> B132
readShelves2 :: BoxOrientator -> String -> IO (WH [Shelf s] s)
readShelves2 defaultOrientator filename = do
    csvData <- BL.readFile filename

    case Csv.decode  Csv.HasHeader csvData of
        Left err ->  do putStrLn err; return (return [])
        Right (rows) -> return $ do
            v <- Vec.forM rows $ \(name, description, l, w, h, shelfType) ->
                        let dim = (,,) l w h
                            dim' = (,,) l w h
                            types = description :: String
                            (shelfO, fillStrat) = case map toLower shelfType of
                                "deadzone" ->  (AddOrientations [] [up, rotatedUp ], RowFirst)
                                "shelf" -> (ForceOrientations [tiltedForward, tiltedFR], ColumnFirst)
                                _ -> (defaultOrientator, ColumnFirst)

                            name'tagS = expand name
                        in mapM (\(n, tag) -> newShelfWithFormula
                                    (dimToFormula n dim)
                                    (dimToFormula n dim')
                                    defaultOrientator
                                    fillStrat
                                    n
                                    tag
                                    ) name'tagS

            return $ concat (Vec.toList v)

-- | Expand chars between bracket to all possible string
-- example:
--    A[135] => A1 A3 A5
-- if a tag is present at the end
-- will be added to the last element
-- example:
--   A[123#top] => A1 A2 A3#top
expand :: String -> [(String, Maybe String)]
expand name = let
  (fix, vars) = break (=='[') name
  in case vars of
    "" -> [extractTag fix]
    ('[':vars) -> case break (==']') vars of
        (_,"") -> error $ "unbalanced brackets in " ++ name
        (elements'tag, rest) -> do
              -- check if there is any tag at the end
              let (elements, tag) = extractTag elements'tag
                  n  = length elements
              (e,i) <- zip elements [1..n]
              (expanded, exTag) <- expand (drop 1 rest)
              return (fix++e:expanded, exTag <|> if i == n then tag else Nothing)
    _ -> error "Should not happen" -- We've been breaking on [



data Expr = AddE Expr Expr
          | SubE Expr Expr
          | MulE Expr Expr
          | DivE Expr Expr
          | ValE Double
          | RefE String (Dimension -> Double)

parseExpr :: (Dimension -> Double) -> String -> Expr
parseExpr defaultAccessor s =  case P.parse (parseExpr' defaultAccessor <* P.eof) s s of
  Left err -> error (show err)
  Right  exp -> exp

parseExpr' :: (Dimension -> Double) -> P.Parser Expr
parseExpr' accessor = (P.try (parseOp accessor))
                    <|> parseTerminalExpr accessor

parseTerminalExpr :: (Dimension -> Double) -> P.Parser Expr
parseTerminalExpr accessor = parseVal <|> parseRef accessor
parseVal :: P.Parser Expr
parseVal = do
      n <- P.many1 P.digit
      f <- P.option "" ((:) <$> P.char '.' <*> P.many1 P.digit)
      let s =  n ++ f
      return $ case readMaybe s of
                  Nothing -> error $ "Can't parse [" ++ s ++ "]"
                  Just v -> ValE v

parseOp accessor = do
  e1 <- parseTerminalExpr accessor
  P.spaces
  op <- P.oneOf "+-*/"
  P.spaces
  e2 <- parseExpr' accessor
  let c = case op of
          '+' -> AddE
          '-' -> SubE
          '*' -> MulE
          '/' -> DivE
          _ -> error "should not happen"


  return $ c e1 e2

parseRef :: (Dimension -> Double) -> P.Parser Expr
parseRef accessor = do
  P.char '{'
  ref <- P.many1 (P.alphaNum <|> P.oneOf ".+-%_\\")
  acc <- P.option accessor $ P.char ':' *> parseAccessor
  P.char '}'
  return $ RefE ref acc

parseAccessor = P.choice $ map  (\(s ,a) -> P.string s >> return a)
                [ ("length", dLength)
                , ("width", dWidth  )
                , ("height", dHeight)
                ]


evalOperator :: String -> (Double -> Double -> Double) -> Expr -> Expr -> WH Double s
evalOperator shelfName op e1 e2 = do
    v1 <- evalExpr shelfName e1
    v2 <- evalExpr shelfName e2
    return (v1 `op` v2)

evalExpr :: String -> Expr -> WH Double s
evalExpr shelfName (AddE e1 e2) = evalOperator shelfName (+) e1 e2
evalExpr shelfName (SubE e1 e2) = evalOperator shelfName (-) e1 e2
evalExpr shelfName (MulE e1 e2) = evalOperator shelfName (*) e1 e2
evalExpr shelfName (DivE e1 e2) = evalOperator shelfName (/) e1 e2


evalExpr _ (ValE v) = return v

evalExpr shelfName (RefE ref accessor) = do
  let refName = transformRef shelfName ref
  ids <- findShelfByName refName
  shelf <- case ids of
              [] -> error $ "Can't find shelf " ++ refName ++ " when evaluating formula"
              [id] ->  findShelf id
              _ -> error $ "Find multiple shelves for " ++ shelfName ++ "when evaluating formula."
  return $ (accessor.minDim) shelf


transformRef :: String -> String -> String
transformRef  [] ref = ref
transformRef origin "%" = origin
transformRef os ('\\':c:cs) = c:transformRef os cs
transformRef (o:os) (c:cs) = case c of
  '_' -> o:transformRef os cs
  '+' -> (succ o):transformRef os cs
  '-' -> (pred o):transformRef os cs
  _ -> c:transformRef os cs

transformRef _ [] = []

-- transformRef os cs = error $ "Non-exhaustive patterns catch "
--    ++ "\n\t[" ++ os ++ "]\n\t[" ++ cs  ++ "]"

dimToFormula :: String -> (String, String, String) -> WH Dimension s
dimToFormula name (ls, ws, hs) = do
  l <- eval dLength ls
  w <- eval dWidth ws
  h <- eval dHeight hs
  return $ Dimension l w h
  where eval :: (Dimension -> Double) -> String -> WH Double s
        eval accessor s = evalExpr name (parseExpr accessor s)


-- | Create a new shelf using formula
newShelfWithFormula :: (WH Dimension s) -> (WH Dimension s) -> BoxOrientator -> FillingStrategy -> String -> Maybe String ->  WH (Shelf s) s

newShelfWithFormula dimW dimW' boxo strategy name tag = do
  dim <- dimW
  dim' <- dimW'
  newShelf name tag dim dim' boxo strategy


readBoxes :: [Orientation] -> (String -> (String, String)) -> String -> IO (WH [Box s] s)
readBoxes boxOrientations splitter filename = do
    csvData <- BL.readFile filename

    case Csv.decode  Csv.HasHeader csvData of
        Left err ->  do putStrLn err; return (return [])
        Right (rows) -> return $ do
            let v = Vec.forM rows $ \(style', qty, l, w, h) -> do
                        let dim = Dimension l w h
                            types = qty :: Int
                            (name, tagM) = extractTag style'
                            (style, content) = splitter name
                        s0 <- incomingShelf

                        forM [1..qty] $   \i -> newBox style content dim (head boxOrientations) s0 boxOrientations ("new" : maybeToList tagM)

            concat `fmap` (Vec.toList `fmap` v)


-- | Split a file so
-- a|b|c d|e f   => [[[a,b,c] [d,e] [f]]]
-- Line starting with < will be reversed
-- < a|b|c => c b a
-- > stays as normal (just there so on can disable and reenable easily << by transforming then to >>
readLayout :: String -> IO [[[ String ]]]
readLayout filename = do
    content <- readFile filename

    return $ map processLine (filter (not . comment) $ lines content)
    where processLine ('<':line) = reverse (processLine line)
          processLine ('>':line) = processLine line
          processLine line = map (splitOn "|")  (words line)
          comment ('#':_) = True -- line starting with #
          comment [] = True -- or empty line
          comment _ = False

readWarehouse :: String -> IO (WH (ShelfGroup s) s)
readWarehouse filename = buildWarehouse `fmap` readLayout filename


-- | read a file assigning styles to locations
-- returns left boxes
readMoves :: String -> IO ( WH [Box s] s)
readMoves filename = do
    csvData <- BL.readFile filename
    case Csv.decode  Csv.HasHeader csvData of
        Left err ->  do putStrLn err; return (return [])
        Right (rows) -> return $ do
            let v = Vec.forM rows $ \ (style, location) -> do
                        let locations = splitOn "|" location
                        boxes <- findBoxByStyleAndShelfNames style
                        shelvesS <- mapM findShelfByName locations
                        aroundArrangement moveBoxes boxes ((nub.concat) shelvesS)

            concat `fmap` (Vec.toList `fmap` v)

-- | read a file assigning tags to styles
-- returns left boxes
readTags :: String -> IO ( WH [Box s] s)
readTags filename = do
    csvData <- BL.readFile filename
    case Csv.decode  Csv.HasHeader csvData of
        Left err ->  do putStrLn err; return (return [])
        Right (rows) -> return $ do
            let v = Vec.forM rows $ \ (style, tag) -> do
                        let tags = splitOn "#" tag
                        boxes <- findBoxByStyleAndShelfNames style
                        shelvesS <- mapM findShelfByName tags
                        mapM (updateBoxTags tags) boxes

            concat `fmap` (Vec.toList `fmap` v)
readOrientations :: [Orientation] -> String -> [Orientation]
readOrientations def os = case os of
    [] -> []
    '*':_ -> allOrientations -- all
    '%':os' -> def `union` readOrientations def os'  -- def
    o:os' -> [readOrientation o] `union` readOrientations def os'

-- | Read box dimension on their location
readStockTake :: [Orientation] -> (String -> (String, String)) -> String -> IO (WH ([Box s], [String]) s)
readStockTake newBoxOrientations splitStyle filename = do
    csvData <- BL.readFile filename

    case Csv.decode  Csv.HasHeader csvData of
        Left err ->  do putStrLn err; return (return ([], []))
        Right (rowsV) -> return $ do
            -- we get bigger box first : -l*w*h
            let rows = [ ((qty, content),  (-(l*w*h), shelf, style', tagM, l,w,h, if null os then "%" else os))
                       | (shelf, style, qty, l, w, h, os)
                       <- Vec.toList (rowsV ::  Vec.Vector (String, String, Int, Double, Double, Double, String))
                       , let (name, tagM) = extractTag style
                       , let (style', content) = splitStyle name
                       ]
            -- groups similar
                groups = groupBy (\a b -> snd a == snd b)
                       $ sortBy (comparing snd) rows


            v <- forM groups $ \rows@((_, (_,shelf, style, tagM, l, w, h, os)):_) -> do
                        s0 <- defaultShelf
                        let dim = Dimension l w h
                            boxOrs = readOrientations newBoxOrientations os
                        boxesS <- forM rows $ \((qty, content),_) ->
                          forM [1..qty] $   \i -> do
                            newBox style
                                    content
                                    dim
                                    (head boxOrs)
                                   s0
                                   boxOrs -- create box in the "ERROR self)
                                   ("take" : maybeToList tagM)
                        let boxes = concat boxesS
                        shelves <- (mapM findShelf) =<< findShelfByName shelf
                        leftOvers <- moveBoxes boxes shelves

                        let errs = if not (null leftOvers)
                                      then map (\b -> "ERROR: box " ++ show b ++ " doesn't fit in " ++ shelf) leftOvers
                                      else []

                        -- detect if any error occurs
                        -- shelfIds <- findShelvesByBoxes boxes
                        -- let s0Id = shelfId s0
                        --     numberOfError = length (filter (== s0Id) shelfIds)
                        --     error = if numberOfError > 0
                        --             then ["ERROR: " ++ show numberOfError ++ " boxes don't fit in shelf for row " ++ show rows ++ ". Found "
                        --           ++ show (length shelves) ++ " shelves :  " ++ show shelves
                        --      ]
                                    -- else []
                        return (boxes, errs)
            let (boxes, errors) = unzip (v)

            return (concat boxes, concat errors)

