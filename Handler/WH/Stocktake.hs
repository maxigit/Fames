{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Handler.WH.Stocktake
( getWHStocktakeR
, postWHStocktakeSaveR
, postWHStocktakeSaveR
, getWHStocktakeSaveR
, postWHStocktakeValidateR
, getWHStocktakeValidateR
, getWHStocktakeLocationR
) where

import Import hiding(length, (\\))
import Handler.CsvUtils hiding(FieldTF, RawT, PartialT, ValidT, FinalT)
import qualified Handler.CsvUtils as CU
import Handler.WH.Barcode
import WH.Barcode
import Data.List(scanl, init, length, head, (\\), nub)
import Data.Either
import System.Directory (doesFileExist)

import Database.Persist.Sql
import qualified Data.Csv as Csv
import Yesod.Form.Bootstrap3 
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import qualified System.FilePath.Glob as Glob
import qualified Data.Map.Strict as Map

import qualified FA as FA
import qualified Data.Set as Set

-- * Requests

data SavingMode = Validate | Save deriving (Eq, Read, Show)

getWHStocktakeR :: Handler Html
getWHStocktakeR = entityTableHandler (WarehouseR WHStocktakeR) ([] :: [Filter Stocktake])

getWHStocktakeValidateR :: Handler Html
getWHStocktakeValidateR = renderWHStocktake Validate Nothing 200 (setInfo "Enter Stocktake") (return ())

renderWHStocktake :: SavingMode -> Maybe FormParam -> Int -> Handler () -> Widget -> Handler Html
renderWHStocktake mode paramM status message pre = do
  let (action, button,btn) = case mode of
        Validate -> (WHStocktakeValidateR, "validate" :: Text, "primary" :: Text)
        Save -> (WHStocktakeSaveR, "save", "danger")
  (uploadFileFormW, upEncType) <- generateFormPost $ uploadForm mode paramM
  message
  sendResponseStatus (toEnum status) =<< defaultLayout [whamlet|
  <div>
    <p>
      <a href=@{WarehouseR WHStocktakeLocationR}>Available locations
  <div .pre> ^{pre}
  <div.well>
    <form #upload-form role=form method=post action=@{WarehouseR action} enctype=#{upEncType}>
      ^{uploadFileFormW}
      <button type="submit" name="#{button}" .btn class="btn-#{btn}">#{button}
|]


postWHStocktakeValidateR :: Handler Html
postWHStocktakeValidateR = processStocktakeSheet Validate


getWHStocktakeSaveR :: Handler Html
getWHStocktakeSaveR = renderWHStocktake Save Nothing 200 (setInfo "Enter Stocktake") (return ())

postWHStocktakeSaveR :: Handler Html
postWHStocktakeSaveR = processStocktakeSheet Save

-- | Which stocktake items to display.
-- Complete stocktake can be quite big so displaying in the browser can be skipped if not needed.
data DisplayMode = DisplayAll | DisplayMissingOnly | HideMissing
  deriving (Eq, Read, Show, Enum, Bounded)

-- | Whether a style is complete or not, i.e. we need to generate zerotake for variations
-- not present in the stocktake (only for style present)
data StyleComplete = StyleComplete | StyleIncomplete
  deriving (Eq, Read, Show, Enum, Bounded)

-- | Should be Either FileInfo (Text, Text)
data FormParam = FormParam
 { pFileInfo :: Maybe FileInfo
 , pFileKey :: Maybe Text
 , pFilePath :: Maybe Text
 , pEncoding :: Encoding
 , pComment :: Maybe Textarea
 , pStyleComplete :: StyleComplete
 , pDisplayMode :: DisplayMode
 , pOveridde :: Bool
 }

uploadForm mode paramM = 
  let form' Save = FormParam
                   <$> pure Nothing -- areq hiddenField "upload" (fmap pFileInfo paramM)
                   <*> areq hiddenField "key" (Just $ pFileKey =<< paramM)
                   <*> areq hiddenField "path" (Just $ pFilePath =<< paramM)
                   <*> areq (selectField optionsEnum ) "encoding" (fmap pEncoding paramM <|> Just UTF8)
                   <*> aopt textareaField "comment" (fmap pComment paramM)
                   <*> areq (selectField optionsEnum ) "stylecomplete" (fmap pStyleComplete paramM <|> Just StyleComplete)
                   <*> areq (selectField optionsEnum ) "displaymode" (fmap pDisplayMode paramM <|> Just DisplayAll)
                   <*> areq boolField "override" (fmap pOveridde paramM <|> Just False)
      form' Validate = FormParam
                   <$> (Just <$> areq fileField "upload" (pFileInfo =<< paramM))
                   <*> pure Nothing
                   <*> pure Nothing
                   <*> areq (selectField optionsEnum ) "encoding" (fmap pEncoding paramM <|> Just UTF8)
                   <*> aopt textareaField "comment" (fmap pComment paramM)
                   <*> areq (selectField optionsEnum ) "stylecomplete" (fmap pStyleComplete paramM <|> Just StyleComplete)
                   <*> areq (selectField optionsEnum ) "displaymode" (fmap pDisplayMode paramM <|> Just DisplayAll)
                   <*> areq boolField "override" (fmap pOveridde paramM <|> Just False)
  in renderBootstrap3 BootstrapBasicForm . form' $ mode

renderValidRows :: FormParam -> [ValidRow] -> Handler Widget
renderValidRows param rows = do
  missings <- case pStyleComplete param of
    StyleComplete -> ZeroST <$$> generateMissings rows
    StyleIncomplete -> return []
    
  return $ case pDisplayMode param of
          DisplayAll -> render rows >> render missings
          DisplayMissingOnly -> render missings
          HideMissing -> render rows
  
-- | Display or save the uploaded spreadsheet
-- At the moment saving a spreadsheet involves having to upload it twice.
-- Once for validation and once for processing. We could use a session or similar
-- to "save" the validation processing.
processStocktakeSheet :: SavingMode -> Handler Html 
processStocktakeSheet mode = do
  ((fileResp, postFileW), enctype) <- runFormPost (uploadForm mode Nothing)
  case fileResp of
    FormMissing -> error "form missing"
    FormFailure a -> error $ "Form failure : " ++ show (mode, a)
    FormSuccess param@(FormParam fileInfoM keyM pathM encoding commentM complete display override) -> do
      let tmp file = "/tmp" </> (unpack file)
      (spreadsheet, key, path) <- case (fileInfoM, keyM, pathM) of
        (_, Just key, Just path) -> do
          ss <- readFile (tmp key)
          return (ss, key, path)
        (Just fileInfo, _, _) -> do
          (ss, key) <- readUploadUTF8 fileInfo encoding
          let path = tmp key
          exist <- liftIO $ doesFileExist path
          unless exist $ do
            writeFile (tmp key) ss
          return (ss, key, fileName fileInfo)
          

      -- TODO move to reuse
      -- check if the document has already been uploaded
      -- and reject it.
      documentKey <- runDB $ getBy (UniqueSK key)
      forM documentKey $ \(Entity _ doc) -> do
        uploader <- runDB $ get (documentKeyUserId doc)
        let msg = [shamlet|Document has already been uploaded
$maybe u <- uploader
  as "#{documentKeyName doc}"
  by #{userIdent u}
  on the #{tshow $ documentKeyProcessedAt doc}.
|]
        case mode of
          Validate -> setWarning msg >> return ""
          Save -> renderWHStocktake Validate Nothing  422 (setError msg) (return ()) -- should exit

      locations <- appStockLocationsInverse . appSettings <$> getYesod
      skus <- getStockIds
    -- get all operators and generate a map name, firstname  -> operator, operator id
      operators <- runDB $ selectList [OperatorActive ==. True] [] 
      let operatorKeys = Map.fromListWith (++) $
                   [ (toLower $ operatorNickname op, [Operator' opId op] ) | op'@(Entity opId op) <- operators ]
                <> [ (toLower $ operatorFirstname op <> operatorSurname op, [Operator' opId op] )
                   | op'@(Entity opId op) <- operators
                   ] :: Map Text [Operator']
           -- we need to filter operators key with more than one solution
          pks = Map.map (Data.List.head)  (Map.filter (\ops -> Data.List.length ops ==1) operatorKeys)
          -- findOps = Map.lookup (Map.map (Data.List.head)  (Map.filter (\ops -> Data.List.length ops /=1) operatorKeys)) . toLower :: Text -> Entity Operator
          findOps = (flip Map.lookup) pks  . toLower . (filter (/= ' '))
          findLocs = validateLocation locations

      userId <- requireAuthId
      processedAt <- liftIO getCurrentTime

      let docKey = DocumentKey "stocktake" path (maybe "" unTextarea commentM) key (userId) processedAt
          newParam = param {pFileKey = Just key, pFilePath = Just path}

      renderParsingResult (renderWHStocktake mode ((Just newParam)) 422)
                          (process newParam docKey mode)
                          (parseTakes skus findOps findLocs spreadsheet)
  where process  param doc Validate rows = do
          widget <- renderValidRows param rows
          renderWHStocktake Save (Just param) 200 (setSuccess "Validation ok!") widget
        process param doc Save rows = do
          missings <- case pStyleComplete param of
            StyleComplete -> map ZeroST <$> generateMissings rows
            StyleIncomplete -> return []
          (runDB $ do
          let finals = zip [1..] (map finalizeRow $ rows <> missings)  :: [(Int, FinalRow)]
          -- separate between full stocktake and zero one.
          -- zero takes needs to be associated with a new barcodes
          -- and don't have box information.
              fulls = [(i, full) | (i, FinalFull full) <- finals]
              zeros = [ zero | (_, FinalZero zero) <- finals]
              override = pOveridde param

          -- rows can't be converted straight away to stocktakes and boxtakes
          -- if in case of many row for the same barcodes, stocktakes needs to be indexed
          -- and boxtake needs to keep only the first one.
          let groups = groupBy ((==) `on` (rowBarcode . snd))
                     . sortBy (comparing (rowBarcode . snd))
                     $ fulls :: [[(Int, FinalFullRow)]]
              
          keyId <- insert doc
          let stocktakes =  do 
                group <- groups
                zipWith ($)(mapMaybe (toStocktakeF keyId . snd) group) [1..]

              boxtakes = do
                group <- groups
                let descriptionF = case group of
                                      [row] -> id
                                      (_:_) -> (<> "*")
                take 1 $ mapMaybe (toBoxtake keyId descriptionF) group
          
          insertZeroTakes keyId zeros

          if override
            then 
              do
                forM_ stocktakes $ \s -> do
                  -- todo  not optimal
                  let unique = UniqueSB (stocktakeBarcode s) (stocktakeIndex s)
                  existM <- getBy  unique
                  case existM of
                    Nothing -> insert_ s
                    Just (Entity key old) -> update key [ StocktakeStockId =. stocktakeStockId s
                                            , StocktakeQuantity =. stocktakeQuantity s
                                            , StocktakeFaLocation =. stocktakeFaLocation s
                                            , StocktakeDate =. stocktakeDate s
                                            , StocktakeActive =. stocktakeActive s
                                            , StocktakeOperator =. stocktakeOperator s
                                            -- , StocktakeAdjustment =. stocktakeAdjustment old
                                            , StocktakeDocumentKey =. stocktakeDocumentKey s
                                            ]
                mapM (\b -> do
                  let unique = UniqueBB (boxtakeBarcode b)
                  existM <- getBy unique
                  case existM of
                    Nothing -> insert_ b
                    Just (Entity key _) -> update key [ BoxtakeDescription =. boxtakeDescription b
                                                      ,  BoxtakeReference =. boxtakeReference b
                                                      ,  BoxtakeLength =. boxtakeLength b
                                                      ,  BoxtakeWidth =. boxtakeWidth b
                                                      ,  BoxtakeHeight =. boxtakeHeight b
                                                      ,  BoxtakeBarcode =. boxtakeBarcode b
                                                      ,  BoxtakeLocation =. boxtakeLocation b
                                                      ,  BoxtakeDate =. boxtakeDate b
                                                      ,  BoxtakeActive =. boxtakeActive b
                                                      ,  BoxtakeOperator =. boxtakeOperator b
                                                      ,  BoxtakeDocumentKey =. boxtakeDocumentKey b
                                                      ]
                  -- print (b, existM)
                  -- upsert b []
                     ) boxtakes
                return ()
            else do
              insertMany_ stocktakes
              insertMany_ boxtakes

          ) >> renderWHStocktake mode
                                 Nothing
                                 200
                                 ( setSuccess (toHtml $ "Spreadsheet"
                                                       <> (fromMaybe "<path>" (pFilePath param))
                                                       <> " processed")
                                 )
                                 (return ())
       
     
insertZeroTakes :: MonadIO m => Key DocumentKey -> [FinalZeroRow] -> ReaderT SqlBackend m ()
-- insertZeroTakes _ [] = return ()
insertZeroTakes docId zeros = do
  let count = length zeros

      toStockTake barcode TakeRow{..} =
        Stocktake (rowStyle <> "-" <> rowColour)
                  0
                  barcode
                  1 -- index
                  (FA.LocationKey "LOST") -- location
                  rowDate
                  True -- active
                  (opId rowOperator)
                  Nothing
                  docId

  barcodes <- generateBarcodes ("ZT" :: Text) Nothing count
  let zerotakes = zipWith toStockTake barcodes zeros

  insertMany_ zerotakes

-- * Csv

-- | Wrapper around Operator
-- ** Temporary types for parsing
data Operator' = Operator' {opId :: OperatorId, op :: Operator} deriving (Eq, Show)
type OpFinder = Text -> Maybe Operator'


-- Result of the parsing of location
data Location' = Location' { faLocation :: FA.LocationId
                           , parsed :: Text
                           , expanded :: Text
                           } deriving (Eq, Read, Show)
type LocFinder = Text -> Maybe Location'

-- | a take row can hold stocktake and boxtake information
data TakeRow s = TakeRow
  { rowStyle :: FieldTF s Text Identity
  , rowColour :: FieldTF s Text Identity
  , rowQuantity :: FieldTF s (Known Int) Null
  , rowLocation :: FieldTF s Location' Null
  , rowBarcode :: FieldTF s Text  Null
  , rowLength :: FieldTF s Double  Null
  , rowWidth :: FieldTF s Double  Null
  , rowHeight :: FieldTF s Double  Null
  , rowDate :: FieldTF s Day  Identity
  , rowOperator :: FieldTF s Operator' Identity
  }
  

data TakeRowType = RawT | PartialT | FullT | ZeroT | FinalZeroT |  FinalT deriving (Eq, Read, Show)
type RawRow = TakeRow RawT -- Raw data. Contains if the original text value if necessary
type PartialRow = TakeRow PartialT -- Well formatted row. Can contains blank
type FullRow = TakeRow FullT -- Contains valid value with guessed/provided indicator
type ZeroRow = TakeRow ZeroT -- Zero take. No item founds, therefore no quantities, barcode, box dimensions, etc ...
type FinalFullRow = TakeRow FinalT -- Contains value with ornement
type FinalZeroRow = TakeRow FinalZeroT -- Contains value with ornement

type family  FieldTF (s :: TakeRowType) a z where
  FieldTF 'RawT a z = FieldForRaw a
  FieldTF 'PartialT a z = FieldForPartial a
  FieldTF 'FullT a z = FieldForValid a
  FieldTF 'ZeroT a z = FieldForValid (UnIdentity (z a))
  FieldTF 'FinalT a z = a
  FieldTF 'FinalZeroT a z = (UnIdentity (z a))

deriving instance Show RawRow
deriving instance Show PartialRow
deriving instance Show FullRow
deriving instance Show ZeroRow
deriving instance Show FinalFullRow
deriving instance Show FinalZeroRow

data ValidRow = FullST FullRow | ZeroST ZeroRow  deriving Show
data FinalRow = FinalFull FinalFullRow | FinalZero FinalZeroRow  deriving Show

-- | Validates if a raw row has been parsed properly. ie cell are well formatted
validateRaw :: OpFinder -> LocFinder ->  RawRow -> Either RawRow PartialRow
validateRaw ops locs raw= do
  -- preprocess  Operator
  -- Operator is special in a sense it can't be parsed without a map name -> operator
  -- So, when the spreadsheet is parsed, rowOperator is necessary `Left`.
  -- We need to check if the name means something, and if so replace it the operator
  -- However, if we are validating again a valid value, ops shouldn't be need.
  -- We just keep the right value
  let rowOperator' =
        case rowOperator raw of
          Right operator -> Right operator
          Left (ParsingError _ name) -> maybe (Left $ ParsingError ("Can't find operator with name " <> name) name)
                             (Right . Just .Provided)
                             (ops name)
          Left field -> Left field
  let rowLocation' =
        case rowLocation raw of
          Right location -> Right location
          Left (ParsingError _ name) -> maybe (Left $ ParsingError ("Can't find location with name " <> name) name)
                             (Right . Just .Provided)
                             (locs name)
          Left field -> Left field

  either (const $ Left raw {rowOperator = rowOperator', rowLocation = rowLocation'}) Right $ do
  rowStyle <- rowStyle raw
  rowColour <- rowColour raw
  rowQuantity <- rowQuantity raw
  rowLocation <- rowLocation'
  rowBarcode <- rowBarcode raw
  rowLength <- rowLength raw
  rowWidth <- rowWidth raw
  rowHeight <- rowHeight raw
  rowDate <- rowDate raw
  rowOperator <- rowOperator'

  Right TakeRow{..}

-- | Validates if a row is invalid or not. ie 
validateRows :: Set Text -> [PartialRow] -> Either [RawRow] [ValidRow]
validateRows _ [] = Left []
validateRows skus (row:rows) = do
  -- fill blank with previous value if possible
  -- All row needs to be validated. However, except the firs one
  -- we don't the check the barcode as it can be missing the prefix
  let filleds = scanl (fillFromPrevious skus)  (validateRow skus CheckBarcode row) rows :: [Either RawRow ValidRow]
      transformRow' r = case r of
        FullST full -> let p = transformRow full  
                       in transformRow (p :: PartialRow)
        ZeroST zero -> let p = transformRow zero  
                       in transformRow (p :: PartialRow)
  -- we need to check that the last barcode is not guessed
  
      errors = map (either id (transformRow')) filleds
      -- ignore zerotake barcode when determining if a sequence of barcode is correct or not.
      validBarcode (FullST row) = Just (rowBarcode row)
      validBarcode (ZeroST row) = Nothing 
               
  valids <- sequence  filleds <|&> const errors
  
  case (lastMay (mapMaybe validBarcode valids))  of
    (Just (Guessed barcode)) -> let
                           l = last (unsafeToMinLen errors) -- valids is not null, so errors isn't either
                           l' = l {rowBarcode = Left $ ParsingError "Last barcode of a sequence should be provided" barcode }
                      
                           in Left $ (init errors) ++ [l']
    _ -> Right valids


data ValidationMode = CheckBarcode | NoCheckBarcode deriving Eq

validateRow :: Set Text -> ValidationMode -> PartialRow -> Either RawRow ValidRow
validateRow skus validateMode row@(TakeRow (Just rowStyle) (Just rowColour) (Just (Provided (Known 0)))
                                  _ _ _ _ _ (Just rowDate) (Just rowOperator))
  = Right . ZeroST $ TakeRow{rowQuantity=(), rowLocation=(), rowBarcode=() 
                            , rowLength=(), rowWidth=(), rowHeight=()
                            , .. }

validateRow skus validateMode row@(TakeRow (Just rowStyle) (Just rowColour) (Just rowQuantity)
                    (Just rowLocation) (Just rowBarcode)
                    (Just rowLength) (Just rowWidth) ( Just rowHeight)
                    (Just rowDate) (Just rowOperator))
  =  case catMaybes [ if (isBarcodeValid (fromStrict $ validValue rowBarcode)
                         || validateMode == NoCheckBarcode)
                      then Nothing
                      else Just (\r -> r {rowBarcode=Left $ ParsingError "Invalid barcode" (validValue rowBarcode)})
                    , 
                        let sku = validValue rowStyle <> "-" <> validValue rowColour
                        in if null skus || sku `member` skus
                              then Nothing
                              else Just (\r -> r {rowColour=Left $ ParsingError "Invalid variation" sku})
                    ] of
       [] -> Right . FullST $ TakeRow{..}
       modifiers -> Left $ foldr ($) (transformRow row) modifiers

validateRow _ _ invalid =  Left $ transformRow invalid


fillFromPrevious :: Set Text -> Either RawRow ValidRow -> PartialRow -> Either RawRow ValidRow
fillFromPrevious skus (Left prev) partial =
  case validateRow skus CheckBarcode partial of
    Left _ ->  Left $ transformRow partial
    Right valid -> Right valid -- We don't need to check the sequence barcode as the row the previous row is invalid anyway

fillFromPrevious skus (Right (ZeroST previous)) partial =
  case validateRow skus CheckBarcode partial of
    Left _ ->  Left $ transformRow partial
    Right valid -> Right valid -- We don't need to check the sequence barcode as the row the previous row is invalid anyway
fillFromPrevious skus (Right (FullST previous)) partial
  -- | Right valid  <- validateRow CheckBarcode partial = Right valid
  --  ^ can't do that, as we need to check if a barcode sequence is correct

  | otherwise = let
      style    = rowStyle partial `fillValue` transform (rowStyle previous)
      colour   = rowColour partial `fillValue` transform (rowColour previous)
      quantity = rowQuantity partial `fillValue` transform (rowQuantity previous)
      location = rowLocation partial `fillValue` transform (rowLocation previous)
      barcode  = rowBarcode partial `fillBarcode` transform (rowBarcode previous)
      length   = rowLength partial `fillValue` transform (rowLength previous)
      width    = rowWidth partial `fillValue` transform (rowWidth previous)
      height   = rowHeight partial `fillValue` transform (rowHeight previous)
      date     = rowDate partial `fillValue` transform (rowDate previous)
      operator = rowOperator partial `fillValue` transform (rowOperator previous)

      raw = (TakeRow (Just <$> style) (Just <$> colour) (Just <$> quantity)
                    (Just <$> location) (Just <$> barcode)
                    (Just <$> length) (Just <$> width) (Just <$> height)
                    (Just <$> date) (Just <$> operator) :: RawRow)
      a /~ b = (guess <$> a) /=(guess <$> b)
      modifiers = case (rowBarcode partial, rowQuantity partial) of
      -- belongs to the previous box, we need to check that box information
        -- are identical (if set)
        (_, Just (Provided (Known 0))) -> []
        (Just (Provided "-" ), _) ->
          [ if location /~ Right (rowLocation previous)
            then \r -> r {rowLocation = Left $ ParsingError "Doesn't match original box"
                           (maybe "" (tshow . validValue) (rowLocation partial))
                         }
            else id
          , if length /~ Right (rowLength previous)
            then \r -> r {rowLength = Left $ ParsingError (tshow (length, rowLength previous)) -- "Doesn't match original box"
                           (maybe "" (tshow . validValue) (rowLength partial))
                         }
            else id
                         
          , if width /~ Right (rowWidth previous)
            then \r -> r {rowWidth = Left $ ParsingError "Doesn't match original box"
                           (maybe "" (tshow . validValue) (rowWidth partial))
                         }
            else id
          , if height /~ Right (rowHeight previous)
            then \r -> r {rowHeight = Left $ ParsingError "Doesn't match original box"
                           (maybe "" (tshow . validValue) (rowHeight partial))
                         }
            else id
          , if operator /~ Right (rowOperator previous)
            then \r -> r {rowOperator = Left $ ParsingError "Doesn't match original box"
                           (maybe "" (tshow . validValue) (rowOperator partial))
                         }
            else id
          ]
        _ -> []
     
        
      in validateRow skus CheckBarcode =<< validateRaw (const Nothing) (const Nothing)  (foldr ($) raw modifiers)

fillValue :: (Maybe (ValidField a)) -> Either InvalidField (ValidField a) -> Either  InvalidField (ValidField a)
fillValue (Just new) _ = Right new
fillValue Nothing old = guess <$> old

-- | Fill barcodes in a sequence (ie previous + 1)
-- However, we also need to check the last of a sequence is given and correct.

fillBarcode :: (Maybe (ValidField Text)) -> Either InvalidField (ValidField Text) -> Either InvalidField  (ValidField Text)
fillBarcode new prevE =
  case (new, prevE) of
    (Just  (Provided "-"), Right prev) -> Right prev -- use the same level of guessing than the previous one
  -- TODO Add datatype to deal with type of barcode
    (Just barcode, Right prev) -> do
      -- we need to check if it's valid or miss a prefix
      -- as well as if it's the end of a sequence
      -- and the same barcode is not used twice.
      -- in theory we should check if a barcode hasn't been used at all
      -- but in practice checking the last one should be enough
      
      let invalid = Left $ ParsingError "Invalid Barcode" (validValue barcode)
          splitBarcodeE s = maybe invalid  Right $ splitBarcode s

      -- even though the prefix has been set, we don't count it as guessed.
      (prefix, n, c) <- splitBarcodeE (fromStrict $ validValue barcode)
      (prefix0, n0, _) <- splitBarcodeE (fromStrict $ validValue prev)

      -- add prefix if missing
      let prefix' = if null prefix then prefix0 else prefix
          new = formatBarcode prefix' n
      -- check suffix is correct
      (_,_,newC) <- splitBarcodeE new

      case (newC == c, prev) of
        (False, _) -> Left $ ParsingError "Invalid Barcode" (validValue barcode)
        (_, Guessed _ ) | n /= n0+1 -> Left $ ParsingError "Barcode Sequence broken" (toStrict new)
        _ | n == n0 -> Left $ ParsingError "Barcode alread in use" (validValue barcode)
        _  -> Right $ Provided (toStrict new)

    (Just barcode, _)  -> Right (barcode)
    (Nothing,_) -> do -- Either
                 prev <- validValue <$> prevE
                 case nextBarcode $ fromStrict prev of
                   Nothing -> Left $ ParsingError ("Previous barcode invalid" <> prev) ""
                   Just barcode -> Right (Guessed $ toStrict barcode)
    
-- | Expand a location pattern to the matching locations
-- ex : ["AB" "AC" "BD"] "A?" -> AB AC
-- expandLocation :: [String] -> String -> [String]
-- expandLocation [] pat = [pat]
-- expandLocation locations pat = let
--   pat' = Glob.compile pat
--   in filter (Glob.match pat') locations

expandLocation :: Text -> [Text]
expandLocation name = let
  (fix, vars) = break (=='[') name
  in case stripPrefix "[" vars of
    Nothing -> [fix]
    (Just vars) -> case break (==']') vars of
        (_ ,rest) | null rest -> [] --  Left $ "unbalanced brackets in " <> name
        (elements, rest) -> do
              e <- toList elements
              expanded <- expandLocation (drop 1 rest)
              return $ (fix<> singleton e)<>expanded

validateLocation :: Map Text Text -> Text -> Maybe Location'
validateLocation locMap t =
  let locs = expandLocation t
  in case mapMaybe (flip Map.lookup locMap) locs of
  [] -> Nothing
  (fa:fas) -> -- check all shelves exists and belongs to the same FA location
    if null (filter (/= fa) fas)
    then Just $ Location' (FA.LocationKey fa) t (intercalate "|" locs)
    else Nothing
  

-- | Parses a csv of stocktakes. If a list of locations is given
-- parseTakes that the locations are valid (matches provided location using unix file globing)
parseTakes  :: Set Text -> OpFinder -> LocFinder -> ByteString -> ParsingResult RawRow [ValidRow]
parseTakes skus opf locf bytes = either id ParsingCorrect $ do
    
  raws <- parseSpreadsheet mempty Nothing bytes <|&> WrongHeader
  rows <- validateAll (validateRaw opf locf) raws <|&> InvalidFormat
  valids <- validateRows skus rows <|&> InvalidData 
  Right valids

  where -- validateAll :: (a-> Either b c) -> [a] -> Either [b] [c]
        validateAll validate rows = let
          validateds = map validate rows
          in case (lefts validateds, rights validateds) of
            ([], valids) -> Right valids
            (invalids,_) -> Left invalids

          -- a = (traverse validate rows)
          -- in a <|&> -- (const $ lefts ) (const $ map transformRow rows)

toStocktakeF :: DocumentKeyId -> FinalFullRow -> Maybe (Int -> Stocktake)
toStocktakeF docId TakeRow{..} = case rowQuantity of
  Unknown -> Nothing
  Known quantity -> Just $ \index -> Stocktake (rowStyle <> "-" <> rowColour)
                      quantity
                      rowBarcode
                      index
                      (faLocation rowLocation)
                      rowDate
                      True
                      (opId rowOperator)
                      Nothing
                      docId


toBoxtake :: DocumentKeyId -> (Text -> Text) -> (Int, FinalFullRow) -> Maybe Boxtake
toBoxtake docId descriptionFn (col, TakeRow{..}) =
  Just $ Boxtake (Just $ descriptionFn (rowStyle<>"-"<>rowColour))
                 (tshow col)
                 rowLength rowWidth rowHeight
                 rowBarcode (expanded rowLocation)
                 rowDate
                 True
                 (opId rowOperator)
                 docId

-- | Generates zerotake corresponding to variations which exits for a given style
-- but have not been stock taken
generateMissings :: [ValidRow] -> Handler [ZeroRow]
generateMissings rows = do
  let cmp row = let row' = transformValid row :: ZeroRow
                in (,) <$> (validValue . rowStyle)
                       <*> (Down . validValue . rowDate)
                       $ row'
  let groups = groupBy ((==) `on` (validValue . styleFor)) (sortBy (comparing cmp) rows)
  
  zeros <- mapM generateMissingsFor groups
  return $ concat zeros 
      

-- | All rows should belongs to the same style. The first one
-- being the one use to set the date and operator.
generateMissingsFor :: [ValidRow] -> Handler [ZeroRow]
generateMissingsFor [] = return []
generateMissingsFor rows@(row:_) = do
  let day = validValue . rowDate $ (transformValid row :: ZeroRow) -- max date, rows are sorted by date DESC
  -- find all items which have been in stock
  let sql = "SELECT stock_id FROM (SELECT stock_id, SUM(qty) quantity \
            \FROM 0_stock_moves \
            \WHERE LEFT(stock_id,8)  = ? \
            \ AND tran_date <= ? \
            \ AND loc_code = \"DEF\" \
            \GROUP BY stock_id \
            \HAVING quantity != 0 ) variations"
      style = validValue (styleFor row) :: Text
      getColour = drop 9

      

  variations <- runDB $ rawSql sql [PersistText style, PersistDay day]

  let usedVars = nub . sort $ map (validValue . rowColour . (\r -> transformValid r :: ZeroRow)) rows
  let vars = nub . sort $ (map (getColour . unSingle) variations)
  let missingVars = vars \\ usedVars
      row' = transformValid row :: ZeroRow

  -- traceShowM (length rows)
  -- traceShowM ("variations", vars
  --            , "takes", usedVars
  --            , "missing", missingVars
  --            )

  return $ [TakeRow  {rowQuantity=(), rowLocation=(), rowBarcode=() 
                     , rowLength=(), rowWidth=(), rowHeight=()
                     , rowStyle = Provided style
                     , rowColour = Guessed var
                     , rowDate = rowDate row'
                     , rowOperator = rowOperator row'
                     }
           | var <- missingVars
           ]


transformRow TakeRow{..} = TakeRow
  (transform rowStyle)
  (transform rowColour)
  (transform rowQuantity)
  (transform rowLocation)
  (transform rowBarcode)
  (transform rowLength)
  (transform rowWidth)
  (transform rowHeight)
  (transform rowDate)
  (transform rowOperator)

transformValid (FullST row) = transformRow row
transformValid (ZeroST row) = transformRow row

finalizeRow (FullST row) = FinalFull (transformRow row)
finalizeRow (ZeroST row) = FinalZero (transformRow row)

styleFor (FullST row) = rowStyle row
styleFor (ZeroST row) = rowStyle row

-- * Csv
instance Csv.FromNamedRecord RawRow where
  parseNamedRecord m = pure TakeRow
    <*> m `parse` "Style"
    <*> m `parse` "Colour"
    <*> m `parse` "Quantity"
    <*> m  Csv..: "Location"
    <*> m `parse` "Barcode Number"
    <*> m `parse` "Length"
    <*> m `parse` "Width"
    <*> m `parse` "Height"
    <*> (allFormatsDay <$$$$> m `parse` "Date Checked" )
    <*> m  Csv..: "Operator"
    where parse m colname = do
            -- parse as a text. To get the cell content
            t <- m Csv..: colname
            -- the real value to parse, can fail
            val <- m Csv..: colname
            return $ toError t val

instance Csv.FromField (Either InvalidField (Maybe (ValidField (Operator')))) where
  parseField bs = do
    t <- Csv.parseField bs
    let types = t :: Maybe Text 
    case t of
      Nothing -> return $ Right Nothing
      Just t' -> return $ Left (ParsingError "Operator doesn't exist" t')


instance Csv.FromField (Either InvalidField (Maybe (ValidField (Location')))) where
  parseField bs = do
    t <- Csv.parseField bs
    let types = t :: Maybe Text 
    case t of
      Nothing -> return $ Right Nothing
      Just t' -> return $ Left (ParsingError "Location doesn't exist" t')

-- * Rendering

renderRow TakeRow{..} = do
  [whamlet|
<td.stocktakeStyle>^{render rowStyle}
<td.stocktakeColour>^{render rowColour}
<td.stocktakeQuantity>^{render rowQuantity}
<td.stocktakeLocation>^{render rowLocation}
<td.stocktakeBarcode>^{render rowBarcode}
<td.stocktakeLength>^{render rowLength}
<td.stocktakeWidth>^{render rowWidth}
<td.stocktakeHeight>^{render rowHeight}
<td.stocktakeDate>^{render rowDate}
<td.stocktakeOperator>^{render rowOperator}
|]

instance Renderable RawRow where render = renderRow
instance Renderable FullRow where render = renderRow
instance Renderable ZeroRow where
  render row = let partial = transformRow row :: PartialRow
               in renderRow $ partial{rowQuantity= Just . Provided $ Known  0 }

instance Renderable [RawRow ]where render = renderRows classForRaw
instance Renderable [ValidRow] where render = renderRows (const ("" :: Text))
instance Renderable ValidRow where
  render (FullST row) = render row
  render (ZeroST row) = render row

instance Renderable Operator' where
  render (Operator' opId op) = render $ operatorNickname op

instance Renderable Location' where
  render loc = [whamlet|
<span.message. data-toggle="tooltip" title=#{expanded loc}>
  #{parsed loc} (#{FA.unLocationKey $ faLocation loc})
|]

classForRaw :: RawRow -> Text                                 
classForRaw raw = case validateRaw (const Nothing) (const Nothing) raw of
  Left _ -> "invalid bg-danger"
  Right row -> case validateRow mempty NoCheckBarcode row of
    Left _ -> "bg-warning"
    Right _ -> ""

renderRows classFor rows = do
  [whamlet|
<table.table.table-bordered.table-hover>
  <tr>
    <th>Style
    <th>Colour
    <th>Quantity
    <th>Location
    <th>Barcode
    <th>Length
    <th>Width
    <th>Height
    <th>Date
    <th>Operator
    $forall row  <- rows
      <tr class=#{classFor row}> ^{render row}
|]



-- | Displays the list of locations
getWHStocktakeLocationR :: Handler Html
getWHStocktakeLocationR = do
  locations <- appStockLocationsInverse . appSettings <$> getYesod
  defaultLayout [whamlet|
<h1> Stock locations
  <table.table.table-striped> 
    <tr>
      <th> Name
    $forall (shelf, location) <- Map.toList locations
      <tr>
        <td> #{location}: #{shelf}
|]

getStockIds :: Handler (Set Text)
getStockIds = do
  stockLike <- appFAStockLikeFilter . appSettings <$> getYesod
  let sql = " SELECT stock_id \
            \ FROM 0_stock_master \
            \ WHERE inactive = 0 \
            \ AND stock_id like ?"

  results <- runDB $ rawSql sql [PersistText stockLike]
  return $ Set.fromList (map unSingle results)
