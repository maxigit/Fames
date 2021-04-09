-- | fetch and update shipping details
-- from misc informations.
module Handler.Customers.ShippingDetails
( computeKey
, makeKey
, clearContact
, saveShippingDetails
, getShippingDetails
, DetailsKey(..)
) where

import Import
import Data.List(nub)


newtype DetailsKey = DetailsKey { unDetailsKey :: Text }

computeKey :: ShippingDetails -> DetailsKey
computeKey ShippingDetails{..} = let
  joinSpace = mconcat . words  :: Text -> Text
  address'contact = joinSpace shippingDetailsPostCode
                  : fmap tshow shippingDetailsCountry
                  ?: shippingDetailsOrganisation
                  : shippingDetailsAddress1
                  : shippingDetailsAddress2
                  : shippingDetailsTown
                  : shippingDetailsCounty
                  : shippingDetailsContact
                  : fmap joinSpace shippingDetailsTelephone
                  ?: shippingDetailsNotificationEmail
                  ?: fmap joinSpace shippingDetailsNotificationText
                  ?: []
  in makeKey address'contact

      

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
              -- , unDetailsKey $ computeKey $ clearContact detail
             ]
  forM_ (nub $ sort keys) $ \k -> do
    void $ upsert detail {shippingDetailsKey = k} []

-- | Find the shipping details with the same key as the given one.
-- This allow to find a valid version of the details even if
-- fields are not field correctly (all information is there but not
-- in the correct field. Example country in county field etc ...
getShippingDetails :: ShippingDetails -> SqlHandler (Maybe (Entity ShippingDetails))
getShippingDetails details =
  getBy $ UniqueCB (shippingDetailsCourrier details) (unDetailsKey $ computeKey details)




