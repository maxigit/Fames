{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
module Handler.GLEnterReceiptSheet where

import Import

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import qualified Data.Csv as Csv
import Data.Csv
import Data.Char (ord)
import Data.Time(parseTimeM)
import Text.Blaze.Html(ToMarkup(toMarkup))
import Text.Printf(printf)
import Data.Conduit.List (consume)
import qualified Data.List.Split  as S
-- | Entry point to enter a receipts spreadsheet
-- The use should be able to :
--   - post a text
--   - upload a document
--   - use an existing document
--   - download a spreadsheet template

       

getGLEnterReceiptSheetR :: Handler Html
getGLEnterReceiptSheetR = do
  (postTextFormW, postEncType) <- generateFormPost postTextForm
  (uploadFileFormW, upEncType) <- generateFormPost uploadFileForm
  let widget =  [whamlet|
<h1>Enter a receipts spreadsheet
         <form role=form method=post action=@{GLEnterReceiptSheetR} enctype=#{postEncType}>
            ^{postTextFormW}
            <button type="submit" .btn .btn-default>Process
    <li> Or Upload a document
    <form role=form method=post action=@{GLEnterReceiptSheetR} enctype=#{upEncType}>
            ^{uploadFileFormW}
            <button type="submit" .btn .btn-default>Process
    <li> Or use an existing document
    Not implemented
    <li> Or download a spreadsheet template
    Not implemented
|]
  defaultLayout $ widget

postTextForm :: Form (Text, Textarea)
postTextForm = renderBootstrap3 BootstrapBasicForm $ (,)
  <$> areq textField "Sheet name" Nothing
  <*> areq textareaField ("Receipts") Nothing


uploadFileForm = renderBootstrap3 BootstrapBasicForm $ (areq fileField ("upload") Nothing )
postGLEnterReceiptSheetR :: Handler Html
postGLEnterReceiptSheetR = do
  ((res, postW), enctype) <- runFormPost postTextForm
  ((upRes, upW), enctype) <- runFormPost uploadFileForm
  case (res, upRes) of
    (FormSuccess (title, text),_) -> processReceiptSheetR title (encodeUtf8  $ unTextarea text)
    (_, FormSuccess fileinfo) ->  do
      bytes <- runResourceT $ fileSource fileinfo $$ consume
      let title = fileName fileinfo
      processReceiptSheetR title (concat bytes)
    _ -> setError "Form empty" >> redirect GLEnterReceiptSheetR

   
type Amount = Double
-- Amount with currency sign
newtype Amount' = Amount' { unAmount' :: Amount} deriving (Read, Show, Eq, Ord, Num, Fractional)

instance Csv.FromField Amount' where
  parseField bs =
    case stripPrefix " " bs of
          Nothing -> case stripPrefix "-" bs of
                          Nothing -> let stripped = bs `fromMaybe` stripPrefix (encodeUtf8 "£") bs
                                         r =  Amount' <$> parseField stripped
                                     in  r

                          Just bs' -> map negate (parseField bs')
          Just bs' -> parseField bs'

-- Can be a real amount or a percentage. Percentage can still be multiplied
data TaxAmount =TaxAmount Amount | TaxRate Amount | TaxCode Text deriving (Read, Show, Eq, Ord)
-- normal numbers are treated as amount. Number starting with T represent a tax code.
instance Csv.FromField TaxAmount where
  parseField bs | Just bs' <- stripPrefix " " bs = parseField bs'
                | Just bs' <- stripPrefix "T" bs = TaxCode <$> parseField bs'
                | Just bs' <- stripPrefix "R" bs = TaxRate . (/100) <$> parseField bs'
                | Just bs' <- stripSuffix "%" bs = TaxRate . (/100) <$> parseField bs' 
                | otherwise = TaxAmount . unAmount' <$> parseField bs

    

-- Convenience functions to help decides of string-like types
s :: String -> String
s =  id

t :: Text -> Text  
t = id 

data RawReceiptRow = RawReceiptRow
  { rrDate :: Either Text (Maybe Day)
  , rrCompany :: Either Text (Maybe Text)
  , rrBankAccount :: Either Text (Maybe Text)
  , rrComment :: Either Text (Maybe Text)
  , rrTotalAmount :: Either Text (Maybe Amount')
  , rrItemPrice :: Either Text (Maybe Amount')
  , rrItemNet :: Either Text (Maybe Amount')
  , rrItemTaxAmount :: Either Text (Maybe TaxAmount)
  , rrItem :: Either Text (Maybe Text)
  , rrGlAccount :: Either Text (Maybe Text)
  , rrGLDimension1 :: Either Text (Maybe Text)
  , rrGLDimension2 :: Either Text (Maybe Text)
  } deriving (Show, Read, Eq, Ord)

instance Csv.FromField Day where
  parseField "" = empty
  parseField bs = parseField bs >>= \str ->  case  concat [parseTimeM True defaultTimeLocale f str | f <- formats] of
    [day] -> pure day
    (d:ds) -> error (show (d:ds))
    _ -> mzero
    where
      -- formats to try. Normally there shouldn't be any overlap bettween the different format.
      -- The 0 in %0Y is important. It guarantes that only 4 digits are accepted.
      -- without 11/01/01 will be parsed by %Y/%m/%d and %d/%m/%y
      formats = [ "%0Y/%m/%d"
                , "%d/%m/%0Y"
                , "%d/%m/%y"
                , "%0Y-%m-%d"
                , "%d %b %0Y"
                , "%d-%b-%0Y"
                , "%d %b %y"
                , "%d-%b-%y"
                , "%0Y %b %d"
                , "%0Y-%b-%d"
                , "%a %d %b %0Y"
                ]


instance ToMarkup Day where
  toMarkup day = toMarkup $ formatTime defaultTimeLocale "%a %d %b %Y" day
instance Csv.FromNamedRecord RawReceiptRow where
  parseNamedRecord m = pure RawReceiptRow
    <*> m `parse` "date"
    <*> m `parse` "company"
    <*> m `parse` "bank account"
    <*> m `parse` "comment"
    <*> m `parse` "total price"
    <*> m `parse` "item price"
    <*> m `parse` "item net"
    <*> m `parse` "item tax"
    <*> m `parse` "item"
    <*> m `parse` "gl account"
    <*> m `parse` "dimension 1"
    <*> m `parse` "dimension 2"
    where parse m  colname = do
            e <- m Csv..: colname
            return (case e of 
                    Left bs -> let types = bs :: ByteString in  Left (decodeUtf8 bs)
                    Right r -> Right r
                    )

processReceiptSheetR title text  = do
  case parseReceiptSheet text of
    Left err -> setError (fromString $ "Error encountered" ++ show err) >> redirect GLEnterReceiptSheetR
    Right csv -> case validateRawRows csv of
      Left _ ->  defaultLayout $ "<h1>Some rows are invalids</h1>" >> (renderRawReceiptRows csv)
      Right rows ->  let receipts = groupReceiptRows rows
        in case sequence receipts of
          Left _ -> defaultLayout $ renderReceipts receipts
          Right receipts ->  defaultLayout (processReceipts receipts)

processReceipts receipts = do
  let eventsOrReceipt = map (receiptToEvent defaultConfig) receipts
  mapM_ renderEvents (rights eventsOrReceipt)

renderRawReceiptRows :: (Foldable t, MonadIO m, MonadBaseControl IO m, MonadThrow m) => t RawReceiptRow -> WidgetT App m ()
renderRawReceiptRows rows = [whamlet|
<table .table .table-striped .table-hover>
  <tr>
    <th>date
    <th>company
    <th>bank account
    <th>comment
    <th>total price
    <th>item price
    <th>item net
    <th>item tax
    <th>item
    <th>gl account
    <th>dimension 1
    <th>dimension 2
  $forall row <- rows
    <tr class=#{rowStatus row} >
      ^{tdWithError $ rrDate row}
      ^{tdWithError $ rrCompany row}
      ^{tdWithError $ rrBankAccount row}
      ^{tdWithError $ rrComment row}
      ^{tdWithError $ map (formatF . unAmount') <$> rrTotalAmount row}
      ^{tdWithError $ map (formatF . unAmount') <$> rrItemPrice row}
      ^{tdWithError $ map (formatF . unAmount') <$> rrItemNet row}
      ^{tdWithError $ map (formatTax) <$> rrItemTaxAmount row}
      ^{tdWithError $ rrItem row}
      ^{tdWithError $ rrGlAccount row}
      ^{tdWithError $ rrGLDimension1 row}
      ^{tdWithError $ rrGLDimension2 row}
|]
    
tdWithError :: (ToMarkup a) => Either Text (Maybe a) -> Widget
tdWithError (Left e) = toWidget [hamlet|
<td>
  <span .text-danger .bg-danger>#{e}
|]
tdWithError (Right (Just v)) = toWidget [hamlet|
<td>
  $#<span .bg-success>#{v}
  <span>#{v}
|]
tdWithError (Right Nothing) = [whamlet|
<td>
 <  span .bg-warnig>
|]
  

formatTax (TaxAmount amount) = formatF amount
formatTax (TaxCode code) = "T" <> code
formatTax (TaxRate rate) = formatF (100*rate) <> "%"

rowStatus :: RawReceiptRow -> Text
rowStatus raw = case validateRawRow raw  of
  Left (_, ParseError) -> "danger"
  Left (_, _) -> "warning"
  _ ->  "" -- """succes"

-- formatF :: (Num a) => a -> Text
formatF :: Amount -> Text
formatF f = pack $ printf "%.02f" f

-- Transforms a text into a set of receipts
data RawRow 
-- validates that the text is a table with the correct columns
parseReceiptSheet :: ByteString -> Either Text [RawReceiptRow]
parseReceiptSheet bytes = case partitionEithers results  of
  (_, [(header, vector)]) -> Right (toList vector)
  (errs, []) -> Left $ pack ("PriceMissing format:" ++ show errs)
  (_,rights) -> Left "File ambiguous."
  where
    bytes' = fromStrict bytes
    results = map (`Csv.decodeByNameWith` bytes') seps
    seps = [Csv.DecodeOptions (fromIntegral (ord sep)) | sep <- ",;\t" ]


data ReceiptRow
  = HeaderRow
    { rDate :: Day
    , rCompany :: Text
    , rBankAccount :: Text
    , rComment :: (Maybe Text)
    , rTotalAmount :: Amount
    , rItemPrice :: (Maybe Amount)
    , rItemNet :: (Maybe Amount)
    , rItemTaxAmount :: (Maybe TaxAmount)
    , rItem :: (Maybe Text)
    , rGLAccount :: (Maybe Text)
    , rGLDimension1 :: (Maybe Text)
    , rGLDimension2 :: (Maybe Text)
    }
  | ReceiptRow
    { rItemPrice :: (Maybe Amount)
    , rItemNet :: (Maybe Amount)
    , rItemTaxAmount :: (Maybe TaxAmount)
    , rItem :: (Maybe Text)
    , rGLAccount :: (Maybe Text)
    , rGLDimension1 :: (Maybe Text)
    , rGLDimension2 :: (Maybe Text)
    }
  deriving (Show, Read, Eq, Ord)

data RawRowStatus = ParseError | PriceMissing | InvalidReceiptHeader deriving (Read, Show, Eq, Ord)
-- validates that a raw is valid
validateRawRow :: RawReceiptRow -> Either (RawReceiptRow, RawRowStatus) ReceiptRow
validateRawRow raw =
  let row = do
        ( date, company, bank, comment, total 
         , iPrice, iNet, iTax, item, account, dim1, dim2) <- pure (,,,,,,,,,,,)
          <*> rrDate raw
          <*> rrCompany raw
          <*> rrBankAccount raw
          <*> rrComment raw
          <*> (map unAmount' <$> rrTotalAmount raw)
          <*> (map unAmount' <$> rrItemPrice raw)
          <*> (map unAmount' <$> rrItemNet raw)
          <*> (rrItemTaxAmount raw)
          <*> rrItem raw
          <*> rrGlAccount raw
          <*> rrGLDimension1 raw
          <*> rrGLDimension2 raw

        let hasHeader = void date <|> void company <|> void bank <|> void total
            hasPrice = asum [void iPrice, void iNet, void iTax]
            rrow =  ReceiptRow iPrice iNet iTax item account dim1 dim2
        case ((date, company, bank, total), hasHeader, hasPrice) of 
              
              ((Just date', Just company', Just bank', Just total'),_,_) ->
                Right . Right $ HeaderRow date' company' bank' comment total'
                iPrice iNet iTax item account dim1 dim2
              (_, Just _, _ ) -> Right $ Left InvalidReceiptHeader
              (_, Nothing, Just _ ) -> Right $ Right rrow
              _ -> Right $ Left PriceMissing
            

  in case row of
      Left _ -> Left (raw, ParseError)
      Right (Left e) -> Left (raw, e)
      Right (Right r) -> Right r
 
validateRawRows :: [RawReceiptRow] -> Either [RawReceiptRow] [ReceiptRow]
validateRawRows raws = case traverse validateRawRow raws of
  Left _ -> Left raws
  Right  rows -> Right rows

data Receipt = Receipt
  { receiptDate :: Day
  , receiptCompany :: Text
  , receiptBankAccount :: Text
  , receiptComment :: (Maybe Text)
  , receiptTotalAmount :: Amount
  , receiptDefaultTaxAmount :: Maybe TaxAmount
  , receiptItems :: [ReceiptItem]
  } deriving (Show, Read, Eq, Ord)

data ReceiptItem = ReceiptItem
  { itemPrice :: Maybe Amount
  , itemNet :: Maybe Amount
  , itemTaxAmount :: Maybe TaxAmount
  , itemMemo :: Maybe Text
  , itemGLAccount :: Maybe Text
  , itemGLDimension1 :: Maybe Text
  , itemGLDimension2 :: Maybe Text
  } deriving (Show, Read, Eq, Ord)
-- groups rows into a set op receipt

-- starts an new group on each line having a company and a total
groupReceiptRows :: [ReceiptRow] -> [Either [ReceiptRow] Receipt]
groupReceiptRows  rows = let
  groups =  S.split (S.keepDelimsL $ S.whenElt  isHeaderRow) rows
  isHeaderRow (HeaderRow {}) = True
  isHeaderRow _ = False
  in map makeReceipt (drop 1 $ groups) -- @TODO remove

makeReceipt [] = Left []
makeReceipt (HeaderRow {..}: rows) = Right $
  Receipt rDate rCompany rBankAccount
          rComment rTotalAmount defaultTaxRate items
  where items = map makeItem $ (ReceiptRow rItemPrice rItemNet rItemTaxAmount rItem
                rGLAccount rGLDimension1 rGLDimension2) : rows
        defaultTaxRate | length items > 2 && isNothing rItemNet = rItemTaxAmount
                       | otherwise = Nothing
        makeItem (ReceiptRow {..}) =
          ReceiptItem rItemPrice rItemNet rItemTaxAmount rItem
                          rGLAccount rGLDimension1 rGLDimension2

        

     
renderReceipts receipts = [whamlet|
<ul .list-group>
  $forall  receiptE <- receipts
    <li .list-group-item .panel .panel-default>
      $case  receiptE
        $of Left row
          #{tshow row}
        $of Right receipt
          <table .table .panel-heading > 
            <tr .panel-primary .bg-info>
              <th>
              <th> #{receiptDate  receipt}
              <th> #{receiptCompany  receipt}
              <th> #{receiptBankAccount  receipt}
              <th> #{fromMaybe "" $ receiptComment  receipt}
              <th> #{formatF $ receiptTotalAmount  receipt}
          <table .table .panel .panel-body>
            $forall item <- receiptItems receipt
              <tr>
                <td> #{maybeToHtml $ formatF <$> itemPrice  item}
                <td> #{maybeToHtml $ formatF <$> itemNet  item}
                <td> #{maybeToHtml $ formatTax <$> itemTaxAmount  item}
                <td> #{maybeToHtml $ itemMemo  item}
                <td> #{maybeToHtml $ itemGLAccount  item}
                <td> #{maybeToHtml $ itemGLDimension1  item}
                <td> #{maybeToHtml $ itemGLDimension2  item}
|]


maybeToHtml Nothing = ""
maybeToHtml (Just v) = toHtml v

-- * Event relateds
  -- Keep the origin of a value, if it's been calculated, guessed etc ...
data GivenStatus = Guessed | Calculated | Given deriving (Read, Show, Eq, Ord)

data GValue  a = GValue { givenStatus :: GivenStatus , givenValue :: a }
     deriving (Read, Show, Eq, Functor)
guess a = a { givenStatus = Guessed }
given a = GValue Given a

instance Applicative (GValue) where
  pure x = GValue Given x
  (GValue fs fv) <*> (GValue as av)= GValue (min fs as) (fv av)


  
instance Num a => Num (GValue a) where
  fromInteger = GValue Given . fromInteger
  negate = map negate
  abs = map abs
  signum = map signum

  a + b  = map (+) a <*> b
  a * b  = map (*) a <*> b
  a - b  = map (-) a <*> b

instance Fractional a => Fractional (GValue a) where
  a / b = map (/) a <*> b
  recip = map recip
  fromRational = GValue Given . fromRational

instance Ord a => Ord (GValue a) where
  (<) = undefined
  

data Event = PaymentEvent
  { evDate :: Day
  , evCounterparty :: GValue Contreparty
  , evBankAccount :: GValue BankAccount
  , evComment :: Maybe Text
  , evPayItems :: [EventPaymentItem]
  } deriving (Read, Show, Eq, Ord)

data EventPaymentItem
  = TaxedPaymentItem { payTotal :: GValue Amount
                     , payNet :: GValue Amount
                     , payTax :: GValue Amount
                     , payGLAccount :: GValue GLAccount
                     , payGLDimension1 :: GValue (Maybe GLDimension)
                     , payGLDimension2 :: GValue (Maybe GLDimension)
                     , payGLTaxAccount :: GValue GLAccount
                     }
  | UntaxedPaymentItem { payTotal :: GValue Amount
                       , payGLAccount :: GValue GLAccount
                       , payGLDimension1 :: GValue (Maybe GLDimension)
                       , payGLDimension2 :: GValue (Maybe GLDimension)
                       }
  deriving (Read, Show, Eq, Ord)
data Contreparty = Customer | Supplier | Other Text  deriving (Read, Show, Eq, Ord)
data BankAccount = BankAccount deriving (Read, Show, Eq, Ord)
data GLAccount = GLAccount deriving (Read, Show, Eq, Ord)
data GLDimension = GLDimension deriving (Read, Show, Eq, Ord)


renderEvents events =  undefined
renderEvent event =  undefined
-- transforms a receipt into an Event
receiptToEvent :: Config -> Receipt -> Either Receipt (Receipt, Event)
receiptToEvent conf receipt@(Receipt {..})= maybe (Left receipt) Right $ do
  let defRate = fromMaybe (guess 20 )$ given . getTaxRate conf <$> receiptDefaultTaxAmount
      makeItems (ReceiptItem {..}) = do
        glAccount <- getGLAccount conf =<< itemGLAccount
        let glDimension1 = sequenceA $ getGLDimension conf =<< itemGLDimension1
            glDimension2 = sequenceA $ getGLDimension conf =<< itemGLDimension2
            glTaxAccount = guess $ pure GLAccount
        let (price, net, tax ) = calculateAmount defRate (itemPrice, itemNet, itemTaxAmount) 
        return $ TaxedPaymentItem price net tax  glAccount glDimension1 glDimension2 glTaxAccount
  items <- mapM makeItems (receiptItems)
  contreparty <-  getContreparty conf receiptCompany
  bank <-   getBankAccount conf receiptBankAccount
    
  let event = PaymentEvent receiptDate
                           contreparty
                           bank
                           receiptComment
                           items

    -- check that the sum of all items adds up with the overall total 
  return (receipt, event)

  

-- calculate missing amounts needs a default rate
calculateAmount defRate gnt = case gnt of
  (Just gross, Nothing, Nothing) -> let g = given gross
                                        t = taxFromGross g defRate
                                    in (g, g-t, t )
  (Just gross, Nothing, Just (TaxAmount tax)) -> let g = given gross
                                                     t = given tax
                                    in (g, g-t, t )
  (Just gross, Nothing, Just (TaxRate rate)) ->
    let g = given gross
        t = taxFromGross g (given rate)
    in (g, g-t, t)
  (Nothing, Just net, Nothing) -> let n = given net
                                      t = guess defRate
                                  in (n+t, n, t)
  (Nothing, Just net, Just (TaxAmount tax)) -> let n = given net
                                                   t = given tax
                                               in (n+t,n,t)
  (Nothing, Just net, Just (TaxRate rate)) ->
    let n = given net
        t = given rate  * n
    in (n+t, n, t) 
  _ -> error (show (defRate, gnt))
  where taxFromGross gross rate = gross / (1+rate)

  
  
-- * Config
-- Needs to be moved in App
data Config = Config
 {getTaxRate :: TaxAmount -> Amount
 , getBankAccount :: Text -> Maybe   (GValue BankAccount)
 , getGLAccount :: Text -> Maybe  (GValue GLAccount)
 , getGLDimension :: Text -> Maybe  (GValue GLDimension)
 , getContreparty :: Text -> Maybe  (GValue Contreparty)
 }

defaultConfig = let
  getTaxRate = undefined
  getBankAccount = const (Just . pure $ BankAccount)
  getGLAccount = const (Just . pure $ GLAccount)
  getGLDimension = const (Just . pure $ GLDimension)
  getContreparty = const (Just . pure $ Customer)

  in Config {..}
  
