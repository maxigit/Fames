module Handler.Planner.FamesImport
( importFamesDispatch
) where
-- Import methods connecting Fames live database to the planner
import Import
import Planner.Types
import qualified Handler.WH.PackingList as PL
import Database.Persist.Sql (toSqlKey)
-- * Type
data FamesImport
  = ImportPackingList (Key PackingList) -- ^ import packing list
  -- | BoxTag
  deriving (Eq, Show)

-- *  Dispatcher
importFamesDispatch :: Section -> Handler (Either Text [Section])
importFamesDispatch (Section ImportH (Right content) _) = do
  sections <- forM content $ \pl' ->  do
    case readMay pl' of
      Nothing -> return $ Left $ "importFamesDispatch: " <> tshow pl'  <> " is not  a valid packing list id"
      Just plId -> Right <$> importPackingList (toSqlKey plId) []
  return $ sequence sections
importFamesDispatch section = return $ Right [section]

-- * Importers
-- | Imports undelivered boxes from packing list.
-- We only import undeliverd one because the delivered one are probablly in the planner somewhere
importPackingList :: Key PackingList -> [Text] -> Handler Section
importPackingList key tags =  runDB $ do
  packingList <- getJust key
  detailEs <- selectList [PackingListDetailPackingList ==. key, PackingListDetailDelivered ==. False]
                         [Asc PackingListDetailId]
  let texts = PL.toPlanner PL.WithDetails packingList detailEs
  return $ Section (BoxesH tags) (Right texts) ("** Import Packing List " <> tshow key)
  
  

  
