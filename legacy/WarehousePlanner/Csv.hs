-- | Miscelaneous functions to read and write shelves and boxes from csv
module WarehousePlanner.Csv where

import WarehousePlanner.Base
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as Vec
import Control.Monad
import Data.List.Split (splitOn)
import Data.List(nub, union, groupBy, sortBy)
import Data.Char(toLower, isDigit)
import Data.Ord(comparing)
import Prelude
import Text.Read(readMaybe)
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Text.ParserCombinators.Parsec.Token as P
import Control.Applicative
import Data.Maybe(maybeToList, catMaybes, fromMaybe)
import Control.Monad.State
import Text.Regex.TDFA ((=~))
import qualified Text.Regex as Rg
import qualified Text.Regex.Base.RegexLike as Rg
import qualified Data.Set as Set

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
                            _types = (first, description) :: (Int, String)
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
                            _types = description :: String
                            (_shelfO, fillStrat) = case map toLower shelfType of
                                "deadzone" ->  (AddOrientations [] [up, rotatedUp ], RowFirst)
                                "shelf" -> (ForceOrientations [tiltedForward, tiltedFR], ColumnFirst)
                                _ -> (defaultOrientator, ColumnFirst)

                            name'tagS = expand =<< splitOn "|" name
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

-- | Read a csv described a list of box with no location
-- boxes are put in the default location and tagged with the new tag
readBoxes :: [Orientation] -> (String -> (String, String)) -> String -> IO (WH [Box s] s)
readBoxes boxOrientations splitter filename = do
    csvData <- BL.readFile filename

    case Csv.decode  Csv.HasHeader csvData of
        Left err ->  do putStrLn err; return (return [])
        Right rows -> return $ do
            let v = Vec.forM rows $ \(style', qty, l, w, h) -> do
                        let dim = Dimension l w h
                            _types = qty :: Int
                            (name, tags) = extractTags style'
                            (style, content) = splitter name
                        s0 <- incomingShelf

                        forM [1..qty] $   \i -> newBox style content dim (head boxOrientations) s0 boxOrientations ("new" : tags)

            concat `fmap` (Vec.toList `fmap` v)

-- | Read a csv file cloning existing boxes
-- This can be used to create ghosts, ie fake boxes
-- used to reserve some space. 
readClones :: String -> IO (WH [Box s] s)
readClones filename = do
    csvData <- BL.readFile filename
    case Csv.decode  Csv.HasHeader csvData of
        Left err ->  do putStrLn err; return (return [])
        Right rows -> return $ do  -- IO
          cloness <- forM (Vec.toList rows) $ \(selector, qty, content'tags) -> do -- WH
                let (content0, tags) = extractTags content'tags
                    content = if null content0 then Nothing else Just content0
                s0 <- incomingShelf
                
                boxIds <- findBoxByStyleAndShelfNames selector
                boxes <- mapM findBox boxIds
                let box'qtys =  [(box, q) | box <- boxes , q <- [1..qty :: Int]] -- cross product
                forM box'qtys  $ \(box, i) -> do
                    box <- newBox (boxStyle box)
                            (fromMaybe (boxContent box) content) -- use provided content if possible
                            (_boxDim box)
                            (orientation box)
                            s0
                            (boxBoxOrientations box)
                            (Set.toList (boxTags box))
                    updateBoxTags tags box
          return $ concat cloness

readDeletes :: String -> IO (WH [Box s] s)
readDeletes filename = do
  content <- readFile filename
  return $ do -- IO
      boxess <- forM (lines content) $ \selector -> do -- WH
        boxes <- findBoxByStyleAndShelfNames selector
        return [] -- deleteBoxes boxes
      return (concat boxess)

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
readMoves = readMovesAndTagsWith (\(style, location) -> processMovesAndTags (style, Nothing, Just location))


readMovesAndTagsWith :: Csv.FromRecord r => (r -> WH [Box s] s) -> String -> IO (WH [Box s] s)
readMovesAndTagsWith  rowProcessor filename = do
    csvData <- BL.readFile filename
    case Csv.decode  Csv.HasHeader csvData of
        Left err ->  putStrLn err >> return (return [])
        Right (rows) -> return $ do
          v <- Vec.forM rows rowProcessor
          return $ concat (Vec.toList v)

processMovesAndTags :: (String, Maybe [Char], Maybe [Char]) -> WH [Box s] s
processMovesAndTags (style, tagM, locationM) = do
  boxes0 <- findBoxByStyleAndShelfNames style
  forM_ locationM $ \location' -> do
    let (location, exitMode) = case location' of
                                  ('^':loc) -> (loc, ExitOnTop)
                                  _ -> (location', ExitLeft)
    let locations = splitOn "|" location
    shelvesS <- mapM findShelfByName locations
    aroundArrangement (moveBoxes exitMode) boxes0 ((nub.concat) shelvesS)
  boxes <- mapM findBox boxes0
  case tagM of
    Nothing -> return boxes
    Just tag  -> do
      let tags = splitOn "#" tag
      mapM (updateBoxTags tags) boxes

-- | read a file assigning tags to styles
-- returns left boxes
readTags :: String -> IO ( WH [Box s] s)
readTags = readMovesAndTagsWith (\(style, tag) -> processMovesAndTags (style, Just tag, Nothing))

-- | Read tags or moves. Normally we could consider
-- that by default, we have a location, unless we start with '#'
-- and the it's a tag. However, location can have a tag. They need
-- then to be prefixed by /
-- #tag
-- location
-- /#taggedLocation
-- /location
-- #tag/location
-- location,tag
readMovesAndTags :: String -> IO (WH [Box s] s)
readMovesAndTags = readMovesAndTagsWith go where
  go (style, tag'location) =
    let (tagM, locM) = splitTagsAndLocation tag'location
    in processMovesAndTags (style, tagM, locM)

splitTagsAndLocation tag'locations
   -- | (tag, _:location@(_:_)) <- break (=='/') tag'locations = (just tag, just location)
   | (location , _:tag@(_:_)) <- break (=='#') tag'locations = (just tag, just location)
   | otherwise = (Nothing, Just tag'locations)
   where just [] = Nothing
         just s = Just s
    


readOrientations :: [Orientation] -> String -> [Orientation]
readOrientations def os = case os of
    [] -> []
    '*':_ -> allOrientations -- all
    '%':os' -> def `union` readOrientations def os'  -- def
    o:os' -> [readOrientation o] `union` readOrientations def os'

-- * Read transform tags
-- | Temporary type to read a regex using Cassava
-- Usefull so that regex validation is done when reading the file
type RegexOrFn s =  Either Rg.Regex (Box s -> WH Rg.Regex s)
instance Csv.FromField (Either Rg.Regex (Box s -> WH Rg.Regex s)) where
  parseField s = do
    r <- Csv.parseField s
    case expandAttribute' r of
      Nothing -> Left <$> Rg.makeRegexM r
      Just f -> return . Right $ \box -> do
              r'  <- expandAttribute box r
              Rg.makeRegexM r'

-- | Read transform tags
readTransformTags :: String -> IO (WH [Box s] s)
readTransformTags = readMovesAndTagsWith (\(style, tagPat, tagSub) -> transformTags style tagPat tagSub)

 -- | Apply {transformTagsFor} to the matching boxes
transformTags :: String -> RegexOrFn s -> String -> WH [Box s] s
transformTags style tagPattern tagSub = do
  boxes0 <- findBoxByStyleAndShelfNames style
  boxes <- mapM findBox boxes0
  catMaybes <$> mapM (transformTagsFor tagPattern tagSub) boxes
  
-- | Regex tags substitutions. Each tags is matched and applied separately
-- The tag is not removed. To do so add a `#-\0` at the end
transformTagsFor :: RegexOrFn s -> String -> Box s -> WH (Maybe (Box s)) s
transformTagsFor tagPat' tagSub box = do
  tagPat <- either return ($ box) tagPat'
  let tags0 = boxTags box
      tags = concatMap (splitOn "#" . (\t -> Rg.subRegex tagPat t tagSub)) (Set.toList tags0)
  if Set.toList tags0 == tags
  then return Nothing
  else Just <$> updateBoxTags tags box

-- | Read box dimension on their location
readStockTake :: [Orientation] -> (String -> (String, String)) -> String -> IO (WH ([Box s], [String]) s)
readStockTake newBoxOrientations splitStyle filename = do
    csvData <- BL.readFile filename
    case Csv.decode  Csv.HasHeader csvData of
        Left err ->  do putStrLn err; return (return ([], []))
        Right (rowsV) -> return $ do
            -- we get bigger box first : -l*w*h
            let rows = [ ((qty, content, tags),  (-(l*w*h), shelf, style', l,w,h, if null os then "%" else os))
                       | (shelf, style, qty, l, w, h, os)
                       <- Vec.toList (rowsV ::  Vec.Vector (String, String, Int, Double, Double, Double, String))
                       , let (name, tags) = extractTags style
                       , let (style', content) = splitStyle name
                       ]
            -- groups similar
                groups = groupBy (\a b -> snd a == snd b)
                       $ sortBy (comparing snd) rows

            v <- forM groups $ \rows@((_, (_,shelf, style, l, w, h, os)):_) -> do
                        s0 <- defaultShelf
                        let dim = Dimension l w h
                            boxOrs = readOrientations newBoxOrientations os
                        boxesS <- forM rows $ \((qty, content, tags),_) ->
                          forM [1..qty] $   \i -> do
                            newBox style
                                    content
                                    dim
                                    (head boxOrs)
                                   s0
                                   boxOrs -- create box in the "ERROR self)
                                   ("take" : tags)
                        let boxes = concat boxesS
                        shelves <- (mapM findShelf) =<< findShelfByName shelf
                        leftOvers <- moveBoxes ExitLeft boxes shelves

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


-- * read orientation rules
readOrientationRules :: [Orientation] -> String -> IO (Box s -> Shelf s -> Maybe [(Orientation, Int, Int)])
readOrientationRules defOrs filename = do
    csvData <- BL.readFile filename
    case Csv.decode  Csv.HasHeader csvData of
        Left err ->  do putStrLn err; return (\s b -> Nothing)
        Right (rows) -> do
            let rules = fmap (\ (boxSelectors, orientations) ->
                        let (style, boxTagM, _, location, locationTagM) = splitBoxSelector boxSelectors
                            validate shelf box = let
                              boxPatterns = patternToMatchers style
                              locPatterns = patternToMatchers location
                              ors = parseOrientationRule defOrs orientations
                              (on, off) = maybe ([],[]) tagToOnOff boxTagM

                              in if and [ orTrue $ boxPatterns <*> [boxStyle box]
                                        , orTrue $ locPatterns <*> [shelfName shelf]
                                        , filterShelfByTag locationTagM shelf
                                        , filterBoxByTag on off box
                                        ]
                                  then Just ors
                                  else Nothing
                         in validate
                        ) (Vec.toList rows)
                fn box shelf = case catMaybes $ [rule shelf box | rule <- rules ] of
                                  [] -> Nothing
                                  (result:_) -> Just result
            return fn
                           

setOrientationRules :: [Orientation] -> String -> IO (WH () s)
setOrientationRules defOrs filename = do
  fn <- readOrientationRules defOrs filename

  return $ do
    old <- gets boxOrientations
    let new box shelf = case fn box shelf of
          Nothing ->  old box shelf
          Just or -> or

    wh <- get
    put wh {boxOrientations = new}
    return ()
                              

-- orientationFromTag defOrs box shelf = let
--   fromTags = do -- []
--     tag <- boxTags box
--     parseOrientationRule tag
--   in
--   case fromTags of
--     [] -> map (,9) defOrs
--     _ -> fromTags

  
-- | Orientation rules follow this syntax
-- [min : ][max] [or1] [or2] ...
-- example:
-- 9 -- max 9
-- 1:9 -- min 1
parseOrientationRule:: [Orientation] -> String -> [(Orientation, Int, Int)]
parseOrientationRule defOrs cs = let
  (ns, cs') = span (isDigit) cs
  n0 = read ns
  (nM, cs'') = case cs' of
                  (':':s) -> let (ns', leftover) = span (isDigit) s
                             in (Just ( readMaybe ns' :: Maybe Int), leftover)
                  _ -> (Nothing, cs')
  -- if only one number, use it as the maximum
  (min_, max_) = case nM of
    Nothing -> (0, n0)
    Just Nothing -> (n0, n0)
    Just (Just n) -> (n0, n)

  ors = case cs'' of
    [] -> defOrs
    s -> readOrientations defOrs s
  in [(o, min_, max_) | o <- ors ]
  
