{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Handler.WH.Stocktake
( getWHStocktakeR
, postWHStocktakeSaveR
, postWHStocktakeCollectMOPR
, getWHStocktakeSaveR
, postWHStocktakeValidateR
, getWHStocktakeValidateR
, getWHStocktakeLocationR
) where

import Import hiding(length, (\\), Null)
import Handler.Util
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
import qualified System.FilePath.Glob as Glob
import qualified Data.Map.Strict as Map

import qualified FA as FA
import qualified Data.Set as Set
import Text.Blaze(Markup)


-- * Types
-- | Validate spreadsheet, save , collect and save partial stocktakes generated from lost items
data SavingMode = Validate | Save | CollectMOP deriving (Eq, Read, Show)

-- | Which stocktake items to display.
-- Complete stocktake can be quite big so displaying in the browser can be skipped if not needed.
data DisplayMode = DisplayAll | DisplayMissingOnly | HideMissing
  deriving (Eq, Read, Show, Enum, Bounded)

-- | Whether a style is complete or not, i.e. we need to generate zerotake for variations
-- not present in the stocktake (only for style present)
-- StyleComplete: means that every items of a given style is complete.
-- This will reset all previous stocktake information relative to this style
-- and generate 0 takes for variations not present
-- StyleIncomplete: The stock stake is complete but only for each variation.
-- We reset previous information corresponding to a color but doens't create
-- 0 takes form missing variations
-- StyleAddition just add new boxes to the actual stock take.
-- Should be used ideally for delivery 

data StyleComplete = StyleComplete | StyleIncomplete | StyleAddition
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

-- * Requests
getWHStocktakeR :: Handler Html
getWHStocktakeR = do
  sId <- lookupGetParam "id"
  stockId <- lookupGetParam "stock_id"
  docId <- lookupGetParam "doc_key"
  let filter = catMaybes
        [ (\i -> StocktakeId ==. StocktakeKey (SqlBackendKey i)) <$> (sId >>= readMay)
        , (StocktakeStockId ==. ) <$> stockId
        , (\k -> StocktakeDocumentKey ==. DocumentKeyKey' (SqlBackendKey k) ) <$> (docId >>= readMay)
        ]
  entityTableHandler (WarehouseR WHStocktakeR) (filter :: [Filter Stocktake])

getWHStocktakeValidateR :: Handler Html
getWHStocktakeValidateR = do
  mop0 <- collectFromMOP
  (formW, encType) <- generateFormPost $ uploadForm CollectMOP Nothing
  let param0 = FormParam {pStyleComplete = StyleIncomplete, pDisplayMode = HideMissing }
  widget <- if null mop0
    then return $ return ()
    else do 
      table <- renderValidRows param0 mop0
      setWarning "Some items have been lost. Please collect them"
      return [whamlet|
<div.panel.panel-info >
  <div.panel-heading><h3> Lost Items
  <div.panel-body>
    <form #save-collect method=post action=@{WarehouseR WHStocktakeCollectMOPR} enctype=#{encType}>
      ^{table}
      ^{formW}
      <button type="submit" name="Collect" .btn class="btn-danger">Collect
|]
  renderWHStocktake Validate Nothing 200 (setInfo "Enter Stocktake") widget


postWHStocktakeValidateR :: Handler Html
postWHStocktakeValidateR = processStocktakeSheet Validate


getWHStocktakeSaveR :: Handler Html
getWHStocktakeSaveR = renderWHStocktake Save Nothing 200 (setInfo "Enter Stocktake") (return ())

postWHStocktakeSaveR :: Handler Html
postWHStocktakeSaveR = processStocktakeSheet Save

postWHStocktakeCollectMOPR :: Handler Html
postWHStocktakeCollectMOPR = processStocktakeSheet CollectMOP

-- * Form
uploadForm :: SavingMode -> Maybe FormParam -> Markup ->  _ (FormResult FormParam, Widget)
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
      form' CollectMOP = FormParam
                   <$> pure Nothing
                   <*> pure Nothing
                   <*> pure Nothing
                   <*> pure UTF8
                   <*> pure Nothing
                   <*> pure StyleIncomplete 
                   <*> pure HideMissing
                   <*> pure False
  in renderBootstrap3 BootstrapBasicForm . form' $ mode

-- * Rendering
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

renderValidRows :: FormParam -> [ValidRow] -> Handler Widget
renderValidRows param rows = do
  missings <- case pStyleComplete param of
    StyleComplete -> QuickST <$$> generateMissings rows
    _ -> return []

  unless (null missings) $ do
    setWarning (toHtml $ tshow (length missings) <> " variations are missing. Please check this is correct.")

  let missingW = if (null missings)
                 then ""
                 else [whamlet|
<div.panel.panel-warning>
  <div.panel-heading data-toggle="collapse" data-target="#st-missing-variations">
    <h3> Missings variations
    <p> Those variations are considered lost and will be added automatically to the stocktake.
  <div.panel-body #st-missing-variations>
    ^{render missings}  
|]
    
  return $ case pDisplayMode param of
          DisplayAll -> missingW >> render rows
          DisplayMissingOnly -> missingW
          HideMissing -> render rows
  
-- * Processing

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
        (_,_,_) -> do -- CollectMOP
          time <- liftIO getCurrentTime
          return ("", computeDocumentKey (encodeUtf8 (tshow time)), "collectMOP" )
          

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
        case (mode, pOveridde param) of
          (Save, False) -> renderWHStocktake Validate Nothing  422 (setError msg) (return ()) -- should exit
          _ -> setWarning msg >> return ""

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

      (parsed, finalizer) <- case mode of
                               CollectMOP -> do
                                 takes0 <- collectFromMOP
                                 return (ParsingCorrect takes0
                                        , mapM_ cleanupTopick takes0) -- processAt
                               _ -> return $ (parseTakes skus findOps findLocs spreadsheet, return ())

      takes <- lookupBarcodes parsed
      renderParsingResult (renderWHStocktake mode ((Just newParam)) 422)
                          (process newParam docKey mode finalizer)
                          (takes)
  where process  param doc Validate finalizer rows = do
          widget <- renderValidRows param rows
          renderWHStocktake Save (Just param) 200 (setSuccess "Validation ok!") widget
        process param doc mode finalizer rows | mode == Save || mode == CollectMOP = do
          missings <- case pStyleComplete param of
            StyleComplete -> map QuickST <$> generateMissings rows
            _ -> return []
          (runDB $ do
            let finals = zip [1..] (map finalizeRow $ rows <> missings)  :: [(Int, FinalRow)]
            -- separate between full stocktake, barcode lookup and quick one.
            -- quick takes needs to be associated with a new barcodes
            -- and don't have box information.
                fulls = [(i, full) | (i, FinalFull full) <- finals]
                quicks = [ quick | (_, FinalQuick quick) <- finals]
                fromBarcodes = [(i, lookedup) | (i, FinalBarcodeLookup lookedup) <- finals]
                override = pOveridde param

            -- rows can't be converted straight away to stocktakes and boxtakes
            -- if in case of many row for the same barcodes, stocktakes needs to be indexed
            -- and boxtake needs to keep only the first one.
            let groupByBarcode = groupBy ((==) `on` (rowBarcode . snd))
                      . sortBy (comparing ((,) <$> rowBarcode . snd <*> fst))
            let groups = groupByBarcode fulls :: [[(Int, FinalFullRow)]]

            keyId <- do
              existM <- getBy (UniqueSK $ documentKeyKey doc)
              case existM of
                Nothing ->  insert doc
                Just (Entity k _ ) -> replace k doc >> return k


            let stocktakes =  concatMap (groupToStocktakes keyId) groups
                boxtakes = do
                  group <- groups
                  let descriptionF = case group of
                                        [row] -> id
                                        (_:_) -> (<> "*")
                                        _ -> error "shouldn't happen"
                  take 1 $ mapMaybe (toBoxtake keyId descriptionF) group
          
            -- Unless we are doing "addition" .We assume that when doing a stock take
            -- ALL boxes of a given style are withing the stock take
            -- Therefore we can inactivated the previous for a given style
            -- However, for quick take which are not null 
            -- which corresponds to a partial check
            -- we don't invalid the boxes are they are probably still there.
            -- We only invalidate the other takes. This is wrong as well
            -- but if we consider a stocktake to be an event, when someone counted boxes
            -- everything is fine. We can have form example 6x4=24 on the previous stocktake
            -- and only count 5. This corresponds to a loss of 19 or -1 if we consider it modulo 6.
            -- Invalidating the other means when doing the stock adjustement that we will ose 19 (or 1)
            -- This shouldn't change anything anyway as the previous stocktake might be already part
            -- of a stock adjustment and therefore not taken into account anyway whilst creating the
            -- adjustment.
            -- However, we keep the boxes.
            -- Quick check with 0 are different. It means we haven't found any, therefore
            -- it is legitimate to also inacite the boxes. We know (or think) the boxes
            -- have disappeared.
            when (pStyleComplete param /= StyleAddition) (do
              let skusForFull = map stocktakeStockId stocktakes
                  (zeros, nonZeros)= partition (\r -> rowQuantity r == Known 0)  quicks
                  skusForZeros = zipWith makeSku (map rowStyle quicks) (map rowColour zeros)
                  skusForNonZeros = zipWith makeSku (map rowStyle quicks) (map rowColour nonZeros)
              invalidPreviousTakes (skusForFull ++ skusForZeros)
              invalidPreviousStocktakes  skusForNonZeros
              return ()
              )

            insertQuickTakes keyId quicks
            mapM_ (updateLookedUp keyId) (groupByBarcode fromBarcodes)

            if override
              then do
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
                                              , StocktakeHistory =. ( stocktakeDate s
                                                                    , stocktakeOperator s
                                                                    ) : stocktakeHistory old
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

            let key' = tshow $ unSqlBackendKey (unDocumentKeyKey keyId)
            lift $ do
              pushLinks ("View Stocktake #" <> key') (WarehouseR (WHStocktakeR)) [("doc_key", key' )]
              pushLinks ("Create Stocktake Adjustment") (WarehouseR (WHStockAdjustmentR)) []

            finalizer
           ) >> renderWHStocktake Validate
                                 Nothing
                                 200
                                 ( setSuccess (toHtml $ "Spreadsheet"
                                                       <> (fromMaybe "<path>" (pFilePath param))
                                                       <> " processed")
                                 )
                                 (return ())
       
quickPrefix = "ZT" :: Text
quickPrefix :: Text
     
insertQuickTakes :: MonadIO m => Key DocumentKey -> [FinalQuickRow] -> ReaderT SqlBackend m ()
-- insertQuickTakes _ [] = return ()
insertQuickTakes docId quicks = do
  let count = length quicks

      toStockTake barcode TakeRow{..} =
        Stocktake (makeSku rowStyle rowColour)
                  (qty)
                  barcode
                  1 -- index
                  (FA.LocationKey "LOST") -- location
                  rowDate
                  True -- active
                  (opId rowOperator)
                  Nothing
                  docId
                  []
                  rowComment
        where Known qty = rowQuantity

  barcodes <- generateBarcodes quickPrefix  Nothing count
  let quicktakes = zipWith toStockTake barcodes quicks

  insertMany_ quicktakes

isBarcodeQuick :: Text -> Bool
isBarcodeQuick = isPrefixOf quickPrefix
-- invalidPreviousStockTakes :: [Stocktake]
-- invalidPreviousStockTakes stocktakes = do
--   let skus = nub $ sort (map stocktakeStockId stocktakes)
--       invalid sku = updateWhere [StocktakeStockId ==. sku, StocktakeActive ==. True]
--                                      [StocktakeActive =. False ]
--   mapM_ invalid skus
   
invalidPreviousTakes :: MonadIO m => [Text] -> ReaderT SqlBackend m ()
invalidPreviousTakes skus0 = do
  let skus = nub $ sort skus0
      sql = "UPDATE fames_stocktake st " <>
            "LEFT JOIN fames_boxtake bt USING (barcode) " <>
            "SET st.active = 0, bt.active = 0 " <>
            "WHERE st.stock_id = ?"
  mapM_ (rawExecute sql) (map (return .toPersistValue) skus)
-- @TODO factorize with invalidPreviousTakes
invalidPreviousStocktakes :: MonadIO m => [Text] -> ReaderT SqlBackend m ()
invalidPreviousStocktakes skus0 = do
  let skus = nub $ sort skus0
      sql = "UPDATE fames_stocktake st " <>
            "SET st.active = 0 " <>
            "WHERE st.stock_id = ?"
  mapM_ (rawExecute sql) (map (return .toPersistValue) skus)

-- | when an untouched boxed is stocktaken
-- We can just scan it's barcode and assume it's content
-- is identical to when it's been scanned (and sealed) the last time
-- We just need to update the operator and date (and pop the old one to history)
updateLookedUp :: MonadIO m => DocumentKeyId -> [(Int, FinalFullRow)] -> ReaderT SqlBackend m ()
updateLookedUp docKey i'rows = do
  let s = snd (headEx i'rows)
  let barcode = rowBarcode s
  olds <- selectList [StocktakeBarcode ==. barcode] []
  case olds of
    [] -> -- doesn't exits we need to insert them instead updating
          -- we probably pulled the information from the packing list
         mapM insert_ (groupToStocktakes docKey i'rows)
          
    _ -> mapM (\(Entity key old) ->
            update key [ StocktakeOperator =. (opId $ rowOperator s)
                       , StocktakeDate =.  rowDate s
                       , StocktakeDocumentKey =. docKey
                       , StocktakeHistory =. ( stocktakeDate old
                                             , stocktakeOperator old
                                             ) : stocktakeHistory old
                       , StocktakeActive =. True
                       , StocktakeAdjustment =. Nothing
                       ]

        ) olds
  -- reactivate box if needed and update location history
  -- only if index = 0
  boxtakeM <- getBy (UniqueBB barcode)
  case boxtakeM of
     Nothing -> return ()
     Just (Entity key old) -> update key [ BoxtakeLocation =. expanded (rowLocation s)
                                         , BoxtakeDate =. rowDate s
                                         , BoxtakeOperator =. opId (rowOperator s)
                                         , BoxtakeDocumentKey =. docKey
                                         , BoxtakeLocationHistory =. (boxtakeDate old
                                                             , boxtakeLocation old
                                                             ) : boxtakeLocationHistory old
                                         , BoxtakeActive =. True
                                         ]

-- * Csv

-- | Wrapper around Operators
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
data TakeRow s = TakeRow --  Basic       QuickTake BarcodeLookup
  { rowStyle    :: FieldTF s Text        Identity Identity
  , rowColour   :: FieldTF s Text        Identity Null
  , rowQuantity :: FieldTF s (Known Int) Identity Null
  , rowLocation :: FieldTF s Location'   Null     Identity
  , rowBarcode  :: FieldTF s Text        Null     Identity
  , rowLength   :: FieldTF s Double      Null     Null
  , rowWidth    :: FieldTF s Double      Null     Null
  , rowHeight   :: FieldTF s Double      Null     Null
  , rowDate     :: FieldTF s Day         Identity Identity
  , rowOperator :: FieldTF s Operator'   Identity Identity
  , rowComment  :: FieldTF s (Maybe Text)   Identity Identity
  }
  

data TakeRowType = RawT | PartialT | FullT
  | QuickT | FinalQuickT
  |  BarcodeLookupT -- | FinalBarcodeLookupT
  | FinalT deriving (Eq, Read, Show)
type RawRow = TakeRow RawT -- Raw data. Contains if the original text value if necessary
type PartialRow = TakeRow PartialT -- Well formatted row. Can contains blank
type FullRow = TakeRow FullT -- Contains valid value with guessed/provided indicator
type QuickRow = TakeRow QuickT -- Quick take. No item founds, therefore no quantities, barcode, box dimensions, etc ...
type BarcodeLookupRow = TakeRow BarcodeLookupT -- Barcode lookup. Rescan an already scanned boxed
type FinalFullRow = TakeRow FinalT -- Contains value with ornement
type FinalQuickRow = TakeRow FinalQuickT -- Contains value with ornement
-- type FinalBarcodeLookupRow = TakeRow FinalBarcodeLookupT -- Barcode lookup. Rescan an already scanned boxed

type family  FieldTF (s :: TakeRowType) a z lk where
  FieldTF 'RawT a z lk = FieldForRaw a
  FieldTF 'PartialT a z lk = FieldForPartial a
  FieldTF 'FullT a z lk = FieldForValid a
  FieldTF 'QuickT a z lk = FieldForValid (UnIdentity (z a))
  FieldTF 'FinalQuickT a z lk = UnIdentity (z a)
  FieldTF 'BarcodeLookupT a z lk = FieldForValid (UnIdentity (lk a))
  -- FieldTF 'FinalBarcodeLookupT a z lk = UnIdentity (lk a)
  FieldTF 'FinalT a z lk = a

deriving instance Show RawRow
deriving instance Show PartialRow
deriving instance Show FullRow
deriving instance Show QuickRow
deriving instance Show FinalQuickRow
deriving instance Show FinalFullRow
deriving instance Show BarcodeLookupRow
-- deriving instance Show FinalBarcodeLookupRow

data ValidRow = FullST FullRow | QuickST QuickRow
              | BLookupST BarcodeLookupRow | BLookedupST FullRow   deriving Show
data FinalRow = FinalFull FinalFullRow
              | FinalQuick FinalQuickRow
              | FinalBarcodeLookup FinalFullRow
              deriving Show


data ValidationMode = CheckBarcode | NoCheckBarcode deriving Eq

-- ** Parsing
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
    rowComment <- rowComment raw

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
        QuickST quick -> let p = transformRow quick  
                       in transformRow (p :: PartialRow)
        BLookupST lookup -> let p = transformRow lookup  
                       in transformRow (p :: PartialRow)
        BLookedupST lookup -> error "Shouldn't happen"
  -- we need to check that the last barcode is not guessed
  
      errors = map (either id (transformRow')) filleds
      -- ignore quicktake barcode when determining if a sequence of barcode is correct or not.
      validBarcode (FullST row) = Just (rowBarcode row)
      validBarcode (QuickST row) = Nothing 
      validBarcode (BLookupST row) = Nothing 
      validBarcode (BLookedupST row) = error "Shouldn't happen"
               
  valids <- sequence  filleds <|&> const errors
  
  case (lastMay (mapMaybe validBarcode valids))  of
    (Just (Guessed barcode)) -> let
                           l = last (impureNonNull errors) -- valids is not null, so errors isn't either
                           l' = l {rowBarcode = Left $ ParsingError "Last barcode of a sequence should be provided" barcode }
                      
                           in Left $ (initEx errors) ++ [l']
    _ -> Right valids

-- | The big parsing method which detect depending on the available value
-- which type of row we are having.
validateRow :: Set Text -> ValidationMode -> PartialRow -> Either RawRow ValidRow
validateRow skus validateMode row@(TakeRow (Just rowStyle) (Just rowColour)
                                   (Just rowQuantity@(Provided (Known _)))
                                  Nothing (Just (Provided "0")) Nothing Nothing Nothing
                                   (Just rowDate) (Just rowOperator) rowComment)
  = Right . QuickST $ TakeRow{rowLocation=(), rowBarcode=() 
                            , rowLength=(), rowWidth=(), rowHeight=()
                            , .. }
validateRow skus validateMode row@(TakeRow (Just rowStyle) (Just rowColour)
                                   (Just rowQuantity@(Provided (Known 0)))
                                  _ _ _ _ _
                                   (Just rowDate) (Just rowOperator) rowComment)
  = Right . QuickST $ TakeRow{rowLocation=(), rowBarcode=() 
                            , rowLength=(), rowWidth=(), rowHeight=()
                            , .. }
validateRow skus validateMode row@(TakeRow (Just rowStyle) (Nothing) (Nothing)
                                   (Just rowLocation) (Just rowBarcode)
                                  _ _ _ (Just rowDate) (Just rowOperator) rowComment)
  = Right . BLookupST $ TakeRow{rowColour=(),rowQuantity=() 
                            , rowLength=(), rowWidth=(), rowHeight=()
                            , .. }

validateRow skus validateMode row@(TakeRow (Just rowStyle) (Just rowColour) (Just rowQuantity)
                    (Just rowLocation) (Just rowBarcode)
                    (Just rowLength) (Just rowWidth) ( Just rowHeight)
                    (Just rowDate) (Just rowOperator) rowComment)
  =  case catMaybes [ if (isBarcodeValid (fromStrict $ validValue rowBarcode)
                         || validateMode == NoCheckBarcode)
                      then Nothing
                      else Just (\r -> r {rowBarcode=Left $ ParsingError "Invalid barcode" (validValue rowBarcode)})
                    , 
                        let sku = makeSku (validValue rowStyle) (validValue rowColour)
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

fillFromPrevious skus (Right (QuickST previous)) partial =
  let 
      style    = rowStyle partial `fillValue` transform (rowStyle previous)
      colour = rowColour partial
      quantity = rowQuantity partial
      location = rowLocation partial
      barcode  = rowBarcode partial
      length   = rowLength partial
      width    = rowWidth partial
      height   = rowHeight partial
      date     = rowDate partial `fillValue` transform (rowDate previous)
      operator = rowOperator partial `fillValue` transform (rowOperator previous)
      comment  = rowComment partial
      raw = (TakeRow (Just <$> style) (Right colour) (Right quantity)
                    (Right location) (Right barcode)
                    (Right length) (Right width) (Right height)
                    (Just <$> date) (Just <$> operator) (Right comment) :: RawRow)
  in validateRow skus CheckBarcode =<< validateRaw (const Nothing) (const Nothing)  raw

fillFromPrevious skus (Right (BLookupST previous)) partial = let
      style    = rowStyle partial `fillValue` transform (rowStyle previous)
      colour = rowColour partial
      quantity = rowQuantity partial
      location = rowLocation partial `fillValue` transform (rowLocation previous)
      barcode  = rowBarcode partial
      length   = rowLength partial
      width    = rowWidth partial
      height   = rowHeight partial
      date     = rowDate partial `fillValue` transform (rowDate previous)
      operator = rowOperator partial `fillValue` transform (rowOperator previous)
      comment  = rowComment partial

      raw = (TakeRow (Just <$> style) (Right colour) (Right quantity)
                    (Just <$> location) (Right barcode)
                    (Right length) (Right width) (Right height)
                    (Just <$> date) (Just <$> operator) (Right comment):: RawRow)
      in validateRow skus CheckBarcode =<< validateRaw (const Nothing) (const Nothing)  raw

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
      comment  = rowComment partial

      raw = (TakeRow (Just <$> style) (Just <$> colour) (Just <$> quantity)
                    (Just <$> location) (Just <$> barcode)
                    (Just <$> length) (Just <$> width) (Just <$> height)
                    (Just <$> date) (Just <$> operator) (Right comment) :: RawRow)
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

fillFromPrevious _ (Right (BLookedupST _)) _ = error "Shouldn't happen"

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

-- | Int wrapper to prevent accidental call to toStocktakeF
-- index should start at 1 and being consecutive within a
-- row group (ie same box)
newtype StIndex = StIndex Int deriving (Eq, Show)
toStocktakeF :: DocumentKeyId -> FinalFullRow -> Maybe (StIndex -> Stocktake)
toStocktakeF docId TakeRow{..} = case rowQuantity of
  Unknown -> Nothing
  Known quantity -> Just $ \(StIndex index) -> Stocktake (makeSku rowStyle rowColour)
                      quantity
                      rowBarcode
                      index
                      (faLocation rowLocation)
                      rowDate
                      True
                      (opId rowOperator)
                      Nothing
                      docId
                      []
                      rowComment


toBoxtake :: DocumentKeyId -> (Text -> Text) -> (Int, FinalFullRow) -> Maybe Boxtake
toBoxtake docId descriptionFn (col, TakeRow{..}) =
  Just $ Boxtake (Just $ descriptionFn (makeSku rowStyle rowColour))
                 (tshow col)
                 rowLength rowWidth rowHeight
                 rowBarcode (expanded rowLocation)
                 rowDate
                 True
                 (opId rowOperator)
                 docId
                 []

groupToStocktakes :: DocumentKeyId -> [(Int, FinalFullRow)] -> [Stocktake]
groupToStocktakes docId group = let
  stockFns = mapMaybe (toStocktakeF docId . snd) group
  in zipWith ($) stockFns (map StIndex [1..])

-- | Generates quicktake corresponding to variations which exits for a given style
-- but have not been stock taken
generateMissings :: [ValidRow] -> Handler [QuickRow]
generateMissings rows = do
  let cmp row = let row' = transformValid row :: QuickRow
                in (,) <$> (validValue . rowStyle)
                       <*> (Down . validValue . rowDate)
                       $ row'
  let groups = groupBy ((==) `on` (validValue . styleFor)) (sortBy (comparing cmp) rows)
  
  quicks <- mapM generateMissingsFor groups
  return $ concat quicks 
      

-- | All rows should belongs to the same style. The first one
-- being the one use to set the date and operator.
generateMissingsFor :: [ValidRow] -> Handler [QuickRow]
generateMissingsFor [] = return []
generateMissingsFor rows@(row:_) = do
  let day = validValue . rowDate $ (transformValid row :: QuickRow) -- max date, rows are sorted by date DESC
  -- find all items which have been in stock
  let sql = "SELECT stock_id FROM (SELECT stock_id, SUM(qty) quantity "
            <> "FROM 0_stock_moves "
            <> "WHERE LEFT(stock_id,8)  = ? "
            <> " AND tran_date <= ? "
            <> " AND loc_code = \"DEF\" "
            <> "GROUP BY stock_id "
            <> "HAVING quantity != 0 ) variations"
      style = validValue (styleFor row) :: Text
      getColour = drop 9

      

  variations <- runDB $ rawSql sql [PersistText style, PersistDay day]

  let usedVars = nub . sort $ map (validValue . rowColour . (\r -> transformValid r :: QuickRow)) rows
  let vars = nub . sort $ (map (getColour . unSingle) variations)
  let missingVars = vars \\ usedVars
      row' = transformValid row :: QuickRow

  --            , "takes", usedVars
  --            , "missing", missingVars
  --            )

  return $ [TakeRow  {rowQuantity = Guessed (Known 0), rowLocation=(), rowBarcode=() 
                     , rowLength=(), rowWidth=(), rowHeight=()
                     , rowStyle = Provided style
                     , rowColour = Guessed var
                     , rowDate = rowDate row'
                     , rowOperator = rowOperator row'
                     , rowComment = Just $ Guessed "Missing: auto-generated."
                     }
           | var <- missingVars
           ]

-- 
lookupBarcodes :: ParsingResult RawRow [ValidRow] -> Handler (ParsingResult RawRow [ValidRow])
lookupBarcodes (ParsingCorrect rows) = do
  finalizeds <- runDB $ mapM lookupBarcode rows
  case sequence finalizeds of
    Right valids -> return (ParsingCorrect (concat valids))
    Left invalids -> return (InvalidData (concatMap (either return
                                                            (map transformValid')
                                                    )
                                                    finalizeds
                                         )
                            )
    
  
lookupBarcodes result = return result

lookupBarcode :: MonadIO m => ValidRow -> ReaderT SqlBackend m (Either RawRow [ValidRow])
lookupBarcode valid@(BLookupST row@TakeRow{..}) = do
  let barcode = validValue rowBarcode
      style = validValue rowStyle
      raw = transformValid' valid
      checkStyle st0 sts = if any (isPrefixOf style) (st0:sts)
                          then Nothing
                          else Just $ raw {rowStyle = Left ( InvalidValueError ( "Actual style doesn't match content: "
                                                                                 <> st0
                                                                                 <> ")"
                                                                               ) style
                                                           )
                                          }
  stocktakes <- selectList [StocktakeBarcode ==. barcode] []
  variations <- case stocktakes of
    [] -> lookupPackingListDetail checkStyle barcode
    (Entity _ st0:es) -> return $ case checkStyle (stocktakeStockId st0) (map (stocktakeStockId . entityVal) stocktakes) of
                                       Nothing -> Right  [(drop 1 colour, stocktakeQuantity st)
                                                         | (Entity key st) <- stocktakes  
                                                         , let Just colour = stripPrefix style (stocktakeStockId st)
                                                         ]
                                       Just err -> Left err

  boxtakeM <- getBy (UniqueBB $ barcode)

  return $ case (variations, boxtakeM) of
             (Right vars@(_:_), Just (Entity _ boxtake)) -> 
                   Right [ BLookedupST (TakeRow
                                       (rowStyle)
                                       (Guessed $ colour) -- remove dash @todo unify and centralize style color , split <-> merge
                                       (Guessed . Known $ quantity)
                                       rowLocation
                                       rowBarcode
                                       (Guessed $ boxtakeLength boxtake)
                                       (Guessed $ boxtakeWidth boxtake)
                                       (Guessed $ boxtakeHeight boxtake)
                                       rowDate 
                                       rowOperator 
                                       rowComment
                                       )
                         | (colour, quantity) <- vars
                         ]
             (Left err, _) -> Left err
             _ -> Left raw {rowBarcode = Left (InvalidValueError "the given barcode doesn't exist" barcode)}
  
lookupBarcode row = return . return $ return row

deriving instance Show PackingListDetail
-- | Fetch box content information from a packing list 
lookupPackingListDetail :: (MonadIO m) => (Text -> [t] -> Maybe a) -> Text -> ReaderT SqlBackend m (Either a [(Text, Int)])
lookupPackingListDetail checkStyle barcode = do
  detailE <- getBy (UniquePLB barcode)
  return $ case detailE of
    Just (Entity _ detail) -> case checkStyle (packingListDetailStyle detail) [] of
                                 Nothing -> Right [ ( colour , quantity)
                                                  | (colour, quantity) <- Map.toList (packingListDetailContent detail)
                                                  ]
                                 Just err -> Left err
    _ -> Right []
-- 
-- * Transform instance Related

-- transformRow :: TakeRow t -> TakeRow s
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
  (transform rowComment)

transformValid :: ValidRow -> QuickRow
transformValid (FullST row) = transformRow row
transformValid (QuickST row) = transformRow row
transformValid (BLookedupST row) = transformRow row
transformValid row = error ("transformValid:" ++ show row)

transformValid' :: ValidRow -> RawRow
transformValid' (FullST row) = transformRow row
transformValid' (BLookedupST row) = transformRow row
transformValid' (QuickST TakeRow{..}) = TakeRow
              (transform rowStyle)
              RNothing
              RNothing
              RNothing
              RNothing
              RNothing
              RNothing
              RNothing
              (transform rowDate)
              (transform rowOperator)
              (transform rowComment)

transformValid' (BLookupST TakeRow{..}) = TakeRow
              (transform rowStyle)
              RNothing
              RNothing
              (transform rowLocation)
              (transform rowBarcode)
              RNothing
              RNothing
              RNothing
              (transform rowDate)
              (transform rowOperator)
              (transform rowComment)

finalizeRow :: ValidRow -> FinalRow
finalizeRow (FullST row) = FinalFull (transformRow row)
finalizeRow (QuickST row) = FinalQuick (transformRow row)
finalizeRow (BLookupST row) = error "Shouldn't happend"
finalizeRow (BLookedupST row) = FinalBarcodeLookup (transformRow row)

styleFor :: ValidRow -> ValidField Text
styleFor (FullST row) = rowStyle row
styleFor (QuickST row) = rowStyle row
styleFor (BLookupST row) = rowStyle row
styleFor (BLookedupST row) = rowStyle row

makeSku :: Text -> Text -> Text
makeSku style colour = style <> "-" <> colour
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
    <*> m  `parse` "Comment"
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

-- * Generate partial stocktake from lost items in MOP
-- | It seems easier to generate the result of parsing a spreading
-- and passing to the process function which will take care of generating
-- fake barcodes and invalidates everything required
-- rather than generate Stocktake and refactore the code to make it work on Stocktake
-- We use if available the operator and date corresponding to picking action in MOP
-- MOP Stocktake can comes from two different sources. They can correspond to lost item
-- Which haven't been picked or dispatched (0 takes) or by operator checking manually
-- the stock value. In that case quantities generally only corresponds to the quantity in box
-- and have been entered through the MOP tracking page.
collectFromMOP :: Handler [ValidRow]
collectFromMOP = do
  -- query might incorrect if an order details has been picked multiple time
  -- the system will probably found the last session
  let action = "SELECT * from mop.action "
               <> "JOIN (SELECT max(id) id "
               <> "      FROM mop.action "
               <> "      WHERE typeId = 1 GROUP BY detailId"
               <> "      ) maxaction USING (id) "
  let sql = "SELECT 0_topick.base, 0_topick.variation, MAX(IF(type = 'lost', 0, 0_topick.quantity)) "
            <> " , CONCAT(0_topick.location, ' (', IF(type='lost', MAX(0_topick.quantity), 'partial'), ')') "
            <> " , op.operator_id, GROUP_CONCAT(nickname), MAX(session.date)"
            <> " FROM 0_topick "
            <> " LEFT JOIN (" <> action <> ") action ON (detailId = detail_id AND typeId = 1)" -- picking only
            <> " LEFT JOIN mop.session ON (groupId = actionGroupId)"
            <> " LEFT JOIN mop.operator mop ON (operatorId = mop.id)"
            <> " LEFT JOIN fames_operator op ON (op.nickname = mop.name)" 
            <> " WHERE `type` IN ('lost', 'stocktake')"
            <> " GROUP BY base, variation "
  losts <- runDB $ rawSql sql []
  -- let types = losts :: [(Text, Text, Int, Maybe Day, Maybe Text)]
  if null losts
    then return []
    else do
      (Entity opId operator) <- firstOperator
      today <- utctDay <$> liftIO getCurrentTime
      return $ map (mopToFinalRow (Operator' opId operator) today) losts

mopToFinalRow :: Operator' -> Day
              -> (Single Text, Single Text, Single Int, Single Text
                 ,Single (Maybe (Key Operator)), Single (Maybe Text)
                 , Single (Maybe Day))  -> ValidRow
mopToFinalRow user day (Single style, Single variation , Single quantity, Single comment
                       , Single mOpId, Single mOpName,  Single mday) = let
  rowStyle = Provided style
  rowColour = Provided variation
  rowQuantity = Provided $ Known quantity 
  rowLocation = ()
  rowBarcode = ()
  rowLength = ()
  rowWidth = ()
  rowHeight = ()
  rowOperator = case mOpId of
                  Just opId -> Provided $ Operator' opId
                                                   (Operator {operatorNickname=fromMaybe "" mOpName
                                                             ,operatorFirstname=""
                                                             ,operatorSurname=""
                                                             ,operatorActive=True})
                  Nothing -> Guessed user
  rowDate = maybe (Guessed day) Provided mday
  rowComment = Just $ Provided ("MOP: " <> comment)
  in QuickST $ TakeRow {..}

  
cleanupTopick :: (MonadIO m) => ValidRow -> ReaderT SqlBackend m ()
cleanupTopick (QuickST TakeRow{..}) = do
  let sku = makeSku (validValue rowStyle) (validValue rowColour)
  deleteWhere [FA.TopickSku ==. Just sku, FA.TopickType ==. Just "lost"]
  deleteWhere [FA.TopickSku ==. Just sku, FA.TopickType ==. Just "stocktake"]

-- * Rendering

-- renderRow :: TakeRow _ -> Widget
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
<td.stocktakeComment>^{render rowComment}
|]

instance Renderable RawRow where render = renderRow
instance Renderable FullRow where render = renderRow
instance Renderable QuickRow where
  render row = let partial = transformRow row :: PartialRow
               in renderRow $ partial -- {rowQuantity= Just . Provided $ Known  0 }

instance Renderable [RawRow ]where render = renderRows classForRaw
instance Renderable [ValidRow] where render = renderRows (const ("" :: Text))
instance Renderable ValidRow where
  render (FullST row) = render row
  render (QuickST row) = render row
  render (BLookedupST row) = render row
  render (BLookupST row) = error "Shouldn't happen"

instance Renderable Operator' where
  render (Operator' opId op) = render $ operatorNickname op

instance Renderable Location' where
  render loc = [whamlet|
<span.message. data-toggle="tooltip" title=#{expanded loc}>
  #{parsed loc} (#{FA.unLocationKey $ faLocation loc})
|]

classForRaw :: RawRow -> Text                                 
classForRaw raw = ""
classForRaw raw = case validateRaw (const Nothing) (const Nothing) raw of
  Left _ -> "invalid bg-danger"
  Right row -> case validateRow mempty NoCheckBarcode row of
    Left _ -> "bg-warning"
    Right _ -> ""

-- renderRows :: (Renderable r) => (r -> _ -> [r] -> Widget)
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
    <th>Comment
    $forall row  <- rows
      <tr class=#{classFor row}> ^{render row}
|]



-- * Locations
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
  let sql = " SELECT stock_id "
            <> " FROM 0_stock_master "
            <> " WHERE inactive = 0 "
            <> " AND stock_id like ?"

  results <- runDB $ rawSql sql [PersistText stockLike]
  return $ Set.fromList (map unSingle results)
