{-# LANGUAGE OverloadedStrings #-}
module Handler.Customers.RoyalMail
( Delivery(..)
, makeRoyalMailSource
)
where 

import Import    hiding((.=))
import Data.Csv
import Handler.Customers.DPD hiding (Delivery, deliveryToFields)
import qualified Handler.Customers.DPD as DPD
import qualified Data.ByteString.Lazy as L


data Delivery = Delivery { dpdDelivery :: DPD.Delivery
                         , packageNumber :: Int 
                         }
-- data Delivery = Delivery
--   { addressLine1 :: Text
--   , addressLine2 :: Maybe Text
--   , city Text
--   , postCode ::  text
--   , fullName :: Text
--   }
--   deriving Show
  
instance ToNamedRecord Delivery where
  toNamedRecord = namedRecord . deliveryToFields

deliveryToFields Delivery{..} = let
   -- reorder for convienience
   firstFields = ["Delivery organisation/name"
                 , "Delivery address line1 (property/street)"
                 , "Delivery address line3 (City)"
                 , "Delivery post code"
                 , "Delivery contact name"
                 , "Delivery contact telephone number"
                 , "Delivery notification email"
                 , "Delivery notification SMS Number"
                 , "Delivery service code"
                 , "Delivery total weight (kg)"
                 ]
   fieldToExclude = [ "Delivery no of packages"
                    , "Generate customData"
                    ]
   (before, after) = partition ((`elem` firstFields) . fst)
                   . filter ((`notElem` fieldToExclude) . fst)
                   $ DPD.deliveryToFields dpdDelivery
   customs = [ "Package Number" .=  packageNumber
             , ("Email Notification", if null (notificationEmail  dpdDelivery)
                                      then "False"
                                      else "True"
               )
             , ( "SMS Notification",  if null (notificationSMSNumber dpdDelivery)
                                      then "False"
                                      else "True"
               )
             ]

   in before
      ++ customs
      ++ after
   
   
  
makeRoyalMailSource :: (Double -> [ Delivery ]) -> [ProductDetail] -> ConduitT () (L.ByteString) Handler ()
--  makeRoyalMailSource _ [] = error "Invoice without product details"
makeRoyalMailSource mkDelivery _details = do
 let deliveries = mkDelivery 0 -- (error "total cost not set ???")
     devHeader = header $ map fst $ deliveryToFields $ headEx deliveries
 yield $ encodeByName devHeader deliveries


    

