module Model.DocumentKey where

import Foundation
import Import.NoFoundation
import qualified Crypto.Hash as Crypto
import Web.PathPieces

-- | TODO: Needs to be an enum and used as such in DocumentKey model
newtype DocumentType = DocumentType Text
newtype DocumentHash = DocumentHash {unDocumentHash :: Text } deriving (Show, Read)

instance PathPiece DocumentHash where
  fromPathPiece = DocumentHash <$$>  fromPathPiece
  toPathPiece = toPathPiece . unDocumentHash


computeDocumentKey :: ByteString -> DocumentHash
computeDocumentKey bs = let
  digest = Crypto.hash bs :: Crypto.Digest Crypto.SHA256
  in DocumentHash (tshow digest)
-- | Create a new document key in the DB
createDocumentKey ::  DocumentType -> DocumentHash -> Text -> Text -> SqlHandler (Key DocumentKey)
createDocumentKey (DocumentType docType) (DocumentHash key) reference comment = do
  userId <- lift requireAuthId
  processedAt <- liftIO getCurrentTime
  let documentKey = DocumentKey docType reference
                                comment
                                key userId processedAt
  insert documentKey



getDocumentKeyByHash :: DocumentHash -> SqlHandler (Maybe (Entity DocumentKey))
getDocumentKeyByHash (DocumentHash key) = getBy (UniqueSK key)
