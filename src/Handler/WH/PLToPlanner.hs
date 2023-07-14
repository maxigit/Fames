module Handler.WH.PLToPlanner
(toPlanner
, WithDetails(..)
) where 
-- Move from Handler.WH.PacklingList to break include cycle.
import Import
import qualified Data.Map as Map

data WithDetails = WithDetails | NoDetail deriving (Eq, Show)

toPlanner :: WithDetails -> PackingList -> [Entity PackingListDetail] -> [Text]
toPlanner withDetails PackingList{..} details = let
  groups = Map.fromListWith (+) [ ( ( style detail
                                    , packingListDetailLength
                                    , packingListDetailWidth
                                    , packingListDetailHeight
                                    )
                                 , (1 :: Int)
                                 )
                               | (Entity _ detail@PackingListDetail{..}) <- details
                               ]
  style PackingListDetail{..} = packingListDetailStyle
                             <> (content $ Map.toList packingListDetailContent )
                             <> if withDetails == WithDetails
                                then ("#barcode=" <> packingListDetailBarcode )
                                     <>  (maybe "" ("#pl-vessel=" <>) packingListVessel)
                                     <>  (maybe "" ("#pl-batch=" <>) $ packingListDetailBatch <|> packingListBatch)
                                     <>  (maybe "" ("#pl-container=" <>) packingListContainer)
                                     <>  (maybe "" ("#pl-departure=" <>) $ fmap tshow packingListDeparture)
                                     <>  (maybe "" ("#pl-arriving=" <>) $ fmap tshow packingListArriving)
                                     <> ("#reference=" <> packingListDetailReference )
                                     <> ("#boxNumber=" <> tshow packingListDetailBoxNumber )
                                     <> case Map.toList packingListDetailContent of
                                         cols@(_:_:_) -> "#mixed" <> mconcat [ "#content" <> tshow i <> "=" <> col
                                                                             | ((col, __qty), i) <- zip cols [1..]
                                                                             ]
                                         _ -> ""
                                 else ""
  content [] = ""
  content cols = "-" <> intercalate "&" (map fst cols)
  header = "style,quantity,l,w,h"
  detailToText ((style, l, w, h),qty) = intercalate "," $
    [style , tshow qty ]
    <> map tshow [ l , w , h]
  in header : map detailToText (Map.toList groups) 

