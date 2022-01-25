{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Miscelaneous functions to read and write shelves and boxes from csv
module WarehousePlanner.Csv where

import WarehousePlanner.Base
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as Vec
import Control.Monad hiding(mapM_,foldM)
-- import Data.List.Split (splitOn)
import qualified Data.List as List
import Data.Char(isDigit,ord,chr)
import ClassyPrelude hiding(readFile)
import Text.Read(readMaybe)
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as P
import Control.Monad.State hiding(fix,mapM_,foldM)
import qualified Text.Regex as Rg
import qualified Text.Regex.Base.RegexLike as Rg
import Data.Text(splitOn)
import Data.Text.IO(readFile)

-- | Dimension info to construct a Shelf
data ShelfDimension = ShelfDimension
  { sMinD :: Dimension
  , sMaxD :: Dimension
  , sBottomOffset :: Double -- 
  }
  deriving (Show)

-- | Offset ("altitude") of the top of a shelf
sTopOffset :: ShelfDimension -> Double
sTopOffset s = dHeight (sMaxD s) + sBottomOffset s

shelfDimension :: Shelf s -> ShelfDimension
shelfDimension shelf = ShelfDimension (minDim  shelf) (maxDim shelf) (bottomOffset  shelf)
readShelves :: FilePath-> IO (WH [Shelf s] s)
readShelves filename = do
    csvData <- BL.readFile filename

    case Csv.decode  Csv.HasHeader csvData of
        Left err ->  error $ "File:" <> filename <> " " <>  err -- putStrLn err >> return (return [])
        Right (rows) -> return $ do
            let v = Vec.forM rows $ \(name, first_, last_, description, l, w, h) ->
                        let dim = Dimension l w h
                            dim' = Dimension l (w+10) h
                            last' = maybe first_ id last_
                            _types = (first_, description) :: (Int, Text)
                        in forM [first_..last'] $ \i ->
                                        let shelfName = name <> "." <> tshow i
                                        in newShelf shelfName mempty dim dim' 0 DefaultOrientation ColumnFirst

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
readShelves2 :: BoxOrientator -> FilePath-> IO (WH [Shelf s] s)
readShelves2 defaultOrientator filename = do
    csvData <- BL.readFile filename
    -- Call Csv.decode but add default value to bottom if not present in header
    let decode = asum
          [ Csv.decode Csv.HasHeader csvData
          , fmap (Vec.map (\(name, description, l, w, h, shelfType) ->
                   (name, description, l, w, h, shelfType, "0"))
                 )
                 (Csv.decode Csv.HasHeader csvData)
          ]

    case decode of 
        Left err ->  error $ "File:" <> filename <> " " <>  err -- putStrLn err >> return (return [])
        Right (rows) -> return $ do
            v <- Vec.forM rows $ \(name, description, l, w, h, shelfType, bottom) -> do
                        let dim = (,,) l w h
                            dim' = (,,) l w h
                            _types = description :: Text
                            (_shelfO, fillStrat) = case toLower (shelfType :: Text) of
                                "deadzone" ->  (AddOrientations [] [up, rotatedUp ], RowFirst)
                                "shelf" -> (ForceOrientations [tiltedForward, tiltedFR], ColumnFirst)
                                _ -> (defaultOrientator, ColumnFirst)

                        (name'tagS, go) <-
                            if toLower shelfType == "update"
                            then do
                              let (BoxSelector boxSel shelfSel _) = parseBoxSelector name
                                  selector = ShelfSelector boxSel shelfSel
                              shelfIds <- findShelvesByBoxNameAndNames selector
                              shelves <- mapM findShelf shelfIds
                              let names  = map shelfName shelves
                              return ( map (,Nothing) names , updateShelfWithFormula)
                            else
                              return ( expand =<< splitOn "|" name
                                     , newShelfWithFormula
                                     )
                        mapM (\(n, tag) -> go
                                    (dimToFormula n dim)
                                    (dimToFormula n dim')
                                    (bottomToFormula n bottom)
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
expand :: Text -> [(Text, Maybe Text)]
expand name = let
  (fix, vars0) = break (=='[') name
  in case vars0 of
    "" -> [extractTag fix]
    (uncons -> Just ('[', vars)) -> case break (==']') vars of
        (_,"") -> error $ "unbalanced brackets in " ++ unpack name
        (elements'tag, rest) -> do
              -- check if there is any tag at the end
              let (elements, tag) = extractTag elements'tag
                  n  = length elements
              (e,i) <- zip (unpack elements) [1..n]
              (expanded, exTag) <- expand (drop 1 rest)
              let finalTag =
                    case catMaybes [if i == n then tag else Nothing, exTag] of
                      [] -> Nothing
                      tags -> Just $ intercalate "#" tags
                    
              return (fix <> cons e expanded , finalTag)
    _ -> error "Should not happen" -- We've been breaking on [



data Expr = AddE Expr Expr
          | SubE Expr Expr
          | MulE Expr Expr
          | DivE Expr Expr
          | ValE Double
          | RefE Text (ShelfDimension -> Double)


parseExpr :: (ShelfDimension -> Double) -> Text -> Expr
parseExpr defaultAccessor s =  case P.parse (parseExpr' defaultAccessor <* P.eof) (unpack s) s of
  Left err -> error (show err)
  Right  expr -> expr

parseExpr' :: (ShelfDimension -> Double) -> P.Parser Expr
parseExpr' accessor = (P.try (parseOp accessor))
                    <|> parseTerminalExpr accessor

parseTerminalExpr :: (ShelfDimension -> Double) -> P.Parser Expr
parseTerminalExpr accessor = parseVal <|> parseRef accessor
parseVal :: P.Parser Expr
parseVal = do
      n <- P.many1 P.digit
      f <- P.option "" ((:) <$> P.char '.' <*> P.many1 P.digit)
      let s =  n ++ f
      return $ case readMaybe s of
                  Nothing -> error $ "Can't parse [" ++ s ++ "]"
                  Just v -> ValE v

parseOp :: (ShelfDimension -> Double) -> P.ParsecT Text () Identity Expr
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

parseRef :: (ShelfDimension -> Double) -> P.Parser Expr
parseRef accessor = do
  _ <- P.char '{'
  ref <- P.many1 (P.alphaNum <|> P.oneOf ".+-%_\\")
  acc <- P.option accessor $ P.char ':' *> parseAccessor
  _ <- P.char '}'
  return $ RefE (pack ref) acc

parseAccessor :: P.Stream s m Char => P.ParsecT s u m (ShelfDimension -> Double)
parseAccessor = P.choice $ map  (\(s ,a) -> P.string s >> return a)
                [ ("length", dLength . sMinD)
                , ("width", dWidth   . sMinD)
                , ("height", dHeight . sMinD)
                , ("Length", dLength . sMaxD)
                , ("Width", dWidth   . sMaxD)
                , ("Height", dHeight . sMaxD)
                , ("bottom", sBottomOffset)
                , ("top", sTopOffset)
                ]


evalOperator :: Text -> (Double -> Double -> Double) -> Expr -> Expr -> WH Double s
evalOperator shelfName op e1 e2 = do
    v1 <- evalExpr shelfName e1
    v2 <- evalExpr shelfName e2
    return (v1 `op` v2)

evalExpr :: Text -> Expr -> WH Double s
evalExpr shelfName (AddE e1 e2) = evalOperator shelfName (+) e1 e2
evalExpr shelfName (SubE e1 e2) = evalOperator shelfName (-) e1 e2
evalExpr shelfName (MulE e1 e2) = evalOperator shelfName (*) e1 e2
evalExpr shelfName (DivE e1 e2) = evalOperator shelfName (/) e1 e2


evalExpr _ (ValE v) = return v

evalExpr shelfName (RefE ref accessor) = do
  let refName = transformRef shelfName ref
  ids <- findShelfBySelector (Selector (NameMatches [MatchFull refName]) [])
  shelf <- case ids of
              [] -> error $ "Can't find shelf " ++ unpack refName ++ " when evaluating formula"
              [id_] ->  findShelf id_
              _ -> error $ "Find multiple shelves for " ++ unpack shelfName ++ "when evaluating formula."
  return $ accessor . shelfDimension $ shelf


transformRef :: Text -> Text -> Text
transformRef a b = pack (transformRef' (unpack a) (unpack b))
transformRef'  :: String -> String -> String
transformRef'  "" ref = ref
transformRef' origin "%" = origin
transformRef' os ('\\':c:cs) = c:transformRef' os cs
-- symetry in the given range
-- ex [24] : 2 -> 4 3->3 4 -> 2
transformRef' (o:os) ('[':c:d:']':cs) = chr ni : transformRef' os cs where
  ci = ord(c)
  di = ord(d)
  oi = ord(o)
  ni = ci + di - oi
transformRef' (o:os) (c:cs) = case c of
  '_' -> o:transformRef' os cs
  '+' -> (succ o):transformRef' os cs
  '-' -> (pred o):transformRef' os cs
  _ -> c:transformRef' os cs
transformRef' _ [] = []

-- transformRef os cs = error $ "Non-exhaustive patterns catch "
--    ++ "\n\t[" ++ os ++ "]\n\t[" ++ cs  ++ "]"

dimToFormula :: Text -> (Text, Text, Text) -> WH Dimension s
dimToFormula name (ls, ws, hs) = do
  l <- eval (dLength . sMinD) ls
  w <- eval (dWidth . sMinD) ws
  h <- eval (dHeight . sMinD) hs
  return $ Dimension l w h
  where eval :: (ShelfDimension -> Double) -> Text -> WH Double s
        eval accessor s = evalExpr name (parseExpr accessor s)


bottomToFormula :: Text -> Text -> WH Double s
bottomToFormula name bs = evalExpr name  (parseExpr sTopOffset bs)
-- | Create a new shelf using formula
newShelfWithFormula :: (WH Dimension s) -> (WH Dimension s) -> (WH Double s) -> BoxOrientator -> FillingStrategy -> Text -> Maybe Text ->  WH (Shelf s) s
newShelfWithFormula dimW dimW' bottomW boxo strategy name tags = do
  dim <- dimW
  dim' <- dimW'
  bottom <- bottomW
  newShelf name tags dim dim' bottom boxo strategy

-- | Update an existing shelf
updateShelfWithFormula :: (WH Dimension s) -> (WH Dimension s) -> (WH Double s) -> BoxOrientator -> FillingStrategy -> Text -> Maybe Text ->  WH (Shelf s) s
updateShelfWithFormula dimW dimW' bottomW _boxo _strategy name _tags = do
  shelfIds <- findShelfBySelector (Selector (NameMatches [MatchFull name]) [])
  case shelfIds of
    [shelfId] -> do
        shelf <- findShelf shelfId
        dim <- dimW
        dim' <- dimW'
        bottom <- bottomW
        updateShelf (\s -> s { minDim = dim, maxDim = dim', bottomOffset = bottom }) shelf
    [] -> error $ "Shelf " <> unpack name <> " not found. Can't update it"
    _ -> error $ "To many shelves named " <> unpack name <> " not found. Can't update it"

-- | Read a csv described a list of box with no location
-- boxes are put in the default location and tagged with the new tag
readBoxes :: [Text] -> [Orientation] -> (Text -> (Text, Text)) -> FilePath -> IO (WH [Box s] s)
readBoxes defaultTags boxOrientations splitter filename = do
    csvData <- BL.readFile filename

    case Csv.decode  Csv.HasHeader csvData of
        Left err ->  error $ "File:" <> filename <> " " <>  err -- putStrLn err >> return (return [])
        Right rows -> return $ do
            let v = Vec.forM rows $ \(style', qty, l, w, h) -> do
                        let dim = Dimension l w h
                            _types = qty :: Int
                            (name, tags) = extractTags style'
                            (style, content) = splitter name
                        s0 <- incomingShelf

                        forM [1..qty] $   \_ -> newBox style content dim (headEx boxOrientations) s0 boxOrientations (defaultTags <> tags)

            concat `fmap` (Vec.toList `fmap` v)

-- | Read a csv file cloning existing boxes
-- This can be used to create ghosts, ie fake boxes
-- used to reserve some space. 
readClones :: [Text] -> FilePath-> IO (WH [Box s] s)
readClones defaultTags filename = do
    csvData <- BL.readFile filename
    case Csv.decode  Csv.HasHeader csvData of
        Left err ->  error $ "File:" <> filename <> " " <>  err -- putStrLn err >> return (return [])
        Right rows -> return $ do  -- IO
          cloness <- forM (Vec.toList rows) $ \(selector, qty, content'tags) -> do -- WH
                let (content0, tags) = extractTags content'tags
                    content = if null content0 then Nothing else Just content0
                s0 <- incomingShelf
                
                boxIds <- findBoxByNameAndShelfNames selector
                boxes <- mapM findBox boxIds
                let box'qtys =  [(box, q) | box <- boxes , q <- [1..qty :: Int]] -- cross product
                forM box'qtys  $ \(box, _) -> do
                    newbox <- newBox (boxStyle box)
                            (fromMaybe (boxContent box) content) -- use provided content if possible
                            (_boxDim box)
                            (orientation box)
                            s0
                            (boxBoxOrientations box)
                            [] --  no tags, copy them later
                    updateBoxTags (map parseTagOperation $ defaultTags ++  tags)
                                  newbox  {boxTags = boxTags box} -- copy tags
          return $ concat cloness

readDeletes :: FilePath-> IO (WH [Box s] s)
readDeletes filename = do
  content <- readFile filename
  return $ do -- IO
      boxess <- forM (lines content) $ \selector -> do -- WH
        boxes <- findBoxByNameAndShelfNames (parseBoxSelector selector)
        deleteBoxes boxes
      return (concat boxess)

-- | Split a file so
-- a|b|c d|e f   => [[[a,b,c] [d,e] [f]]]
-- Line starting with < will be reversed
-- < a|b|c => c b a
-- > stays as normal (just there so on can disable and reenable easily << by transforming then to >>
readLayout :: FilePath -> IO [[[ Text ]]]
readLayout filename = do
    content <- readFile filename

    return $ map (processLine) (filter (not . comment) $ lines content)
    where processLine (uncons -> Just ('<', line)) = reverse (processLine line)
          processLine (uncons -> Just ('>', line)) = processLine line
          processLine line = map (splitOn "|")  (words line)
          comment (uncons -> Just ('#',_)) = True -- line starting with #
          comment "" = True -- or empty line
          comment _ = False

readWarehouse :: FilePath -> IO (WH (ShelfGroup s) s)
readWarehouse filename = buildWarehouse `fmap` readLayout filename


-- | read a file assigning styles to locations
-- returns left boxes
readMoves :: [Text] -> FilePath -> IO ( WH [Box s] s)
readMoves tags = readFromRecordWith (\(style, location) -> processMovesAndTags (style, tags, Just location))


readFromRecordWith :: Csv.FromRecord r => (r -> WH [a s] s) -> FilePath -> IO (WH [a s] s)
readFromRecordWith  rowProcessor filename = do
    csvData <- BL.readFile filename
    case Csv.decode  Csv.HasHeader csvData of
        Left err ->  error $ "File:" <> filename <> " " <>  err -- putStrLn err >> return (return [])
        Right (rows) -> return $ do
          v <- Vec.forM rows rowProcessor
          return $ concat (Vec.toList v)

-- | Move and tag boxes.
-- If a location (destination) is provided, only boxes which have been moved will be tagged -
-- leftovers (boxes not fitting ) will be untagged boxes which haven't
-- That way, a tag can either be set on succesfully moved boxed
-- or set on failure (by providing a negative tag).
-- This tagging/untagging ensure that after a move only the moved boxes
-- have the expecting tag (regardless of the previous tags on the boxes)
-- Locations separated by " " will processed in sucession with the leftover
-- so that ^A|B C|D is equivalent to
--     ^A|B#moved
--     #-moved,^C|D.
-- This normally doesn't change anything in ExiftLeft mode
-- but in ExitTop mode make sure the same set of shelves is reuse
-- before going to the left
-- example ^A|B|C|D will exit on top of B  and fill C (even though
-- there might be space left to create a new column in A)
-- \^A|B C|D will exit B to A  until A and B are full
processMovesAndTags :: (BoxSelector s, [Text], Maybe Text ) -> WH [Box s] s
processMovesAndTags (style, tags, locationM) = do
  boxes0 <- findBoxByNameAndShelfNames style
  boxes <- mapM findBox boxes0
  leftoverss <- forM locationM $ \location' -> do
    let (location, exitMode) = case uncons location' of
                                  Just ('^', loc) -> (loc, ExitOnTop)
                                  _ -> (location', ExitLeft)
    let locationss = map (splitOn "|") (splitOn " " location)
    -- reuse leftover of previous locations between " " same syntax as Layout
    foldM (\boxes locations -> do
               shelves <- findShelfBySelectors (map parseSelector locations)
               aroundArrangement (moveBoxes exitMode) boxes shelves
          ) boxes locationss
  case tags of
    [] -> return boxes
    _  -> do
      let tagOps = map parseTagOperation tags
          untagOps = negateTagOperations tagOps
      new <- mapM (updateBoxTags tagOps) boxes
      -- traceShowM("UNTAG", untagOps, length $ concat leftoverss)
      _ <- mapM (updateBoxTags untagOps) (concat leftoverss)
      return new

-- | read a file assigning tags to styles
-- returns left boxes
readTags :: FilePath -> IO ( WH [Box s] s)
readTags = readFromRecordWith (\(style, tag) -> processMovesAndTags (style, splitOn "#" tag, Nothing))

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
readMovesAndTags :: [Text] -> FilePath -> IO (WH [Box s] s)
readMovesAndTags tags0 = readFromRecordWith go where
  go (style, tag'location) =
    let (tags, locM) = splitTagsAndLocation tag'location
    in processMovesAndTags (style, tags0 <> tags, locM)

splitTagsAndLocation :: Text -> ([Text], Maybe Text)
splitTagsAndLocation tag'locations
   -- -| (tag, _:location@(_:_)) <- break (=='/') tag'locations = (just tag, just location)
   | (location , uncons -> Just (_,tag@(uncons -> Just _))) <- break (=='#') tag'locations = (splitOn "#" tag, just location)
   | otherwise = ([], Just tag'locations)
   where just "" = Nothing
         just s = Just s
    


readOrientations :: [Orientation] -> Text -> [Orientation]
readOrientations def os = case uncons os of
    Nothing -> []
    Just ('*', _) -> allOrientations -- all
    Just ('%', os') -> def `List.union` readOrientations def os'  -- def
    Just (o, os') -> [readOrientation o] `List.union` readOrientations def os'

-- * Read Shelf Tags 
readShelfTags :: FilePath -> IO (WH [Shelf s] s)
readShelfTags = readFromRecordWith go where
  go (selector, splitOn "#" -> tags) = do
    shelves <- findShelvesByBoxNameAndNames selector
    let tagOps = map parseTagOperation tags
    mapM (updateShelfTags tagOps) shelves
-- * Read transform tags 
-- | Temporary type to read a regex using Cassava
-- Usefull so that regex validation is done when reading the file
type RegexOrFn s =  Either Rg.Regex (Box s -> WH Rg.Regex s)
instance Csv.FromField (Either Rg.Regex (Box s -> WH Rg.Regex s)) where
  parseField s = do
    r <- Csv.parseField s
    case expandAttribute' r of
      Nothing -> Left <$> Rg.makeRegexM (unpack r)
      Just _ -> return . Right $ \box -> do
              r'  <- expandAttribute box r
              Rg.makeRegexM (unpack r')

instance Csv.FromField (BoxSelector a) where
  parseField s = do
    x <- Csv.parseField s
    return $ parseBoxSelector x
    
instance Csv.FromField (ShelfSelector a) where
  parseField s = do
    x <- Csv.parseField s
    let (BoxSelector boxSel selfSel _ ) = parseBoxSelector x
    return (ShelfSelector boxSel selfSel)

instance Csv.FromField (Selector a) where
  parseField s = do
    x <- Csv.parseField s
    return $ parseSelector x

-- | Read transform tags
readTransformTags :: FilePath -> IO (WH [Box s] s)
readTransformTags = readFromRecordWith (\(style, tagPat, tagSub) -> transformTags style tagPat tagSub)

 -- -| Apply {transformTagsFor} to the matching boxes
transformTags :: BoxSelector s -> RegexOrFn s -> Text -> WH [Box s] s
transformTags style tagPattern tagSub = do
  boxes0 <- findBoxByNameAndShelfNames style
  boxes <- mapM findBox boxes0
  catMaybes <$> mapM (transformTagsFor tagPattern tagSub) boxes
  
-- | Regex tags substitutions. Each tags is matched and applied separately
-- The tag is not removed. To do so add a `#-\0` at the end
transformTagsFor :: RegexOrFn s -> Text -> Box s -> WH (Maybe (Box s)) s
transformTagsFor tagPat' tagSub box = do
  tagPat <- either return ($ box) tagPat'
  let tagOps = map parseTagOperation $
               concatMap (splitOn "#" . (\t -> pack $ Rg.subRegex tagPat (unpack t) (unpack tagSub)))
               (getTagList box)
  Just <$> updateBoxTags tagOps box

-- | Read box dimension on their location
readStockTake :: [Text] -> [Orientation] -> (Text -> (Text, Text)) -> FilePath -> IO (WH ([Box s], [Text]) s)
readStockTake defaultTags newBoxOrientations splitStyle filename = do
    csvData <- BL.readFile filename
    case Csv.decode  Csv.HasHeader csvData of
        Left err ->  error $ "File:" <> filename <> " " <>  err -- putStrLn err >> return (return [])
        Right (rowsV) -> return $ do
            -- we get bigger box first : -l*w*h
            let rows0 = [ ((qty, content, tags),  (-(l*w*h), shelf, style', l,w,h, if null os then "%" else os)) | (shelf, style, qty, l, w, h, os)
                       <- Vec.toList (rowsV ::  Vec.Vector (Text, Text, Int, Double, Double, Double, Text))
                       , let (name, tags) = extractTags style
                       , let (style', content) = splitStyle name
                       ]
            -- groups similar
                groups = List.groupBy (\a b -> snd a == snd b)
                       $ List.sortBy (comparing snd) rows0

            v <- forM groups $ \rows@((_, (_,shelf, style, l, w, h, os)):_) -> do
                        s0 <- defaultShelf
                        let dim = Dimension l w h
                            boxOrs = readOrientations newBoxOrientations os
                        boxesS <- forM rows $ \((qty, content, tags),_) ->
                          forM [1..qty] $   \_ -> do
                            newBox style
                                    content
                                    dim
                                    (headEx boxOrs)
                                   s0
                                   boxOrs -- create box in the "ERROR self)
                                   (defaultTags ++ tags)
                        let boxes = concat boxesS
                        shelves <- (mapM findShelf) =<< findShelfBySelector (Selector (NameMatches [MatchFull shelf]) [])
                        leftOvers <- moveBoxes ExitLeft boxes shelves

                        let errs = if not (null leftOvers)
                                      then map (\b -> "ERROR: box " <> tshow b <> " doesn't fit in " <> shelf) leftOvers
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
readOrientationRules :: [Orientation] -> FilePath -> IO (Box s -> Shelf s -> Maybe [OrientationStrategy])
readOrientationRules defOrs filename = do
    csvData <- BL.readFile filename
    case Csv.decode  Csv.HasHeader csvData of
        Left err ->  error $ "File:" <> filename <> " " <>  err -- putStrLn err >> return (return [])
        Right (rows) -> do
            let rules = fmap (\(boxSelectors, orientations) ->
                        let
                            ors = parseOrientationRule defOrs orientations
                            (BoxSelector boxSel shelfSel _) = parseBoxSelector boxSelectors
                            validate shelf box = if applyNameSelector (nameSelector boxSel) boxStyle box
                                                    && applyTagSelectors (tagSelectors boxSel) boxTags box
                                                    && applyNameSelector (nameSelector shelfSel) shelfName shelf
                                                    && applyTagSelectors (tagSelectors shelfSel) shelfTag shelf
                                                 then Just ors
                                                 else Nothing
                        in validate
                        ) (Vec.toList rows)
                fn box shelf = case catMaybes $ [rule shelf box | rule <- rules ] of
                                  [] -> Nothing
                                  (result:_) -> Just result
            return fn
                           

setOrientationRules :: [Orientation] -> FilePath -> IO (WH () s)
setOrientationRules defOrs filename = do
  fn <- readOrientationRules defOrs filename

  return $ do
    old <- gets boxOrientations
    let new box shelf = case fn box shelf of
          Nothing ->  old box shelf
          Just or_ -> or_

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
-- [!] [min : ][max] [or1] [or2] ...
-- example:
-- 9 -- max 9
-- 1:9 -- min 1
-- ! don't use diagonal mode
parseOrientationRule:: [Orientation] -> Text -> [OrientationStrategy]
parseOrientationRule defOrs cs0 = let
  (diag,cs) = case uncons cs0 of
                Just ('!', s) -> (False, s)
                _ -> (True, cs0)

  (ns, cs') = span (isDigit) cs
  n0 = fromMaybe 1 $ readMay ns
  (nM, cs'') = case uncons cs' of
                  Just (':', s) -> let (ns', leftover) = span (isDigit) s
                             in (Just ( readMay ns' :: Maybe Int), leftover)
                  _ -> (Nothing, cs')
  -- if only one number, use it as the maximum
  (min_, max_) = case nM of
    Nothing -> (0, n0)
    Just Nothing -> (n0, n0)
    Just (Just n) -> (n0, n)

  ors = case cs'' of
    "" -> defOrs
    s -> readOrientations defOrs s
  in [(OrientationStrategy o  min_  max_ (rotateO o `elem` ors && diag)) | o <- ors ]
  
