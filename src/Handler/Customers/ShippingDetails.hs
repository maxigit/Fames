-- | fetch and update shipping details
-- from misc informations.
module Handler.Customers.ShippingDetails
( computeKey
, makeKey
, clearContact
, saveShippingDetails
, getShippingDetails
, DetailsKey(..)
, joinSpaces
, Match(..)
) where

import Import
import Data.List(nub)


newtype DetailsKey = DetailsKey { unDetailsKey :: Text }

computeKey :: ShippingDetails -> DetailsKey
computeKey ShippingDetails{..} = let
  address'contact = joinSpaces shippingDetailsPostCode
                  : fmap tshow shippingDetailsCountry
                  ?: shippingDetailsOrganisation
                  : shippingDetailsAddress1
                  : shippingDetailsAddress2
                  : shippingDetailsTown
                  : shippingDetailsCounty
                  : shippingDetailsContact
                  : fmap joinSpaces shippingDetailsTelephone
                  ?: shippingDetailsNotificationEmail
                  ?: fmap joinSpaces shippingDetailsNotificationText
                  ?: []
  in makeKey address'contact

      
joinSpaces :: Text -> Text
joinSpaces = mconcat . words  :: Text -> Text

makeKey :: [Text] -> DetailsKey
makeKey texts = let
  ws0 = concatMap words texts
  ws = filter (not . null) 
     . filter (/= "the")
     . map (filter (`notElem` (",.-'&/()[];:" :: String)))
     . map toLower
     $ ws0
  --ws = filter (not . null) $ map (filter (/=',')) ws0
  in DetailsKey $ unwords . nub $ sort ws

-- | Clear contact details so we can generate
-- a key looking for the address only
clearContact :: ShippingDetails -> ShippingDetails
clearContact ShippingDetails{..} =
  ShippingDetails{ shippingDetailsContact=""
                 , shippingDetailsTelephone= Nothing
                 , shippingDetailsNotificationEmail = Nothing
                 , shippingDetailsNotificationText = Nothing
                 , .. }

                      
-- | Save shipping details with different key if necessary
saveShippingDetails :: ShippingDetails -> SqlHandler ()
saveShippingDetails detail = do
  let keys = [ shippingDetailsKey detail
             , unDetailsKey $ computeKey detail -- in case set key is different
             , unDetailsKey $ computeKey $ clearContact detail
             ]
  mapM traceShowM keys
  forM_ (nub $ sort keys) $ \k -> do
    traceShowM ("SAVING", k)
    let d = detail { shippingDetailsKey = k }
    inDB <- getBy $ UniqueCB (shippingDetailsCourrier detail) k
    case inDB of
      Nothing -> insert_  d
      Just e | (traceShowId $ entityVal e) /= d -> replace (entityKey e)  d
      _ -> return ()

data Match = FullKeyMatch
           | AddressOnlyMatch -- Only address stripped of contact
           -- | FullContactMatch
           | TelephoneMatch
           | EmailMatch
     deriving (Eq, Ord, Show, Read, Bounded, Enum)
-- | Find the shipping details with the same key as the given one.
-- This allow to find a valid version of the details even if
-- fields are not field correctly (all information is there but not
-- in the correct field. Example country in county field etc ...
getShippingDetails :: [Match] -> ShippingDetails -> SqlHandler (Maybe (Match, Entity ShippingDetails))
getShippingDetails [] _ = return Nothing
getShippingDetails (m:ms) details = do
  found <- case m of
    FullKeyMatch -> getBy $ UniqueCB (shippingDetailsCourrier details) (unDetailsKey $ computeKey details)
    AddressOnlyMatch -> getBy $ UniqueCB (shippingDetailsCourrier details)
                                            (unDetailsKey . computeKey $ clearContact details)
    TelephoneMatch -> fmap join $ forM (shippingDetailsTelephone details) $ \tel ->
                         onlyOne <$> selectList [ShippingDetailsTelephone
                                                 ==. Just (joinSpaces tel)
                                                ] []
    EmailMatch -> fmap join $ forM (shippingDetailsNotificationEmail details) $ \email -> 
                         onlyOne <$> selectList [ShippingDetailsNotificationEmail
                                                ==. Just (joinSpaces email)
                                                ] []
  case found of
    Just e -> return $ Just (m, e)
    Nothing -> getShippingDetails ms details
  where
    -- returns something
    -- only if there is exactly one
    -- or are duplicates with different keys.
    -- Duplicates happens where saving the same details with different key
    -- If all duplicates contains the same information we should return a value
    onlyOne xs =
      case nub . sort $ map (\(Entity _ d) -> d { shippingDetailsKey = "" }) xs of
        [_] -> headMay xs
        _ -> Nothing





