{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Role where

import ClassyPrelude.Yesod
import Data.Text(init)
import qualified Data.Text as Text

-- * Types
-- | Read or Write request
data WriteRequest = ReadRequest | WriteRequest deriving (Eq, Read, Show, Ord)

type Permissions = Set (Text, WriteRequest)
type URL = Text

newtype RoleFor = RoleFor {roleFor :: (Maybe Text -> Role) }


instance Show RoleFor where
  show _ = "RoleFor"

-- | A role, can grant permissions.
data Role = Administrator
          | RoleGroup [Role]
          | RolePermission Permissions
          | RoleRoute (URL) WriteRequest
  deriving(Show, Read, Eq)

-- * Checking permissions
-- | Check if the given route is allowed, based on it path 
isRouteAllowed :: URL -> WriteRequest -> Role -> Bool
isRouteAllowed _ _ Administrator =  True
isRouteAllowed route r (RoleGroup roles) = or $ map (isRouteAllowed route r) roles
isRouteAllowed _ _ (RolePermission _) = False
isRouteAllowed route r (RoleRoute route' r') | route == route' =
  case (r,r') of
    (WriteRequest, ReadRequest) -> False
    (_, _) -> True


-- | Remove permissions which are granted
filterPermissions :: WriteRequest -> Set Text -> Role -> Set Text
filterPermissions _ _ Administrator = mempty
filterPermissions wreq perms (RoleGroup roles) = foldl' (filterPermissions wreq) perms roles
filterPermissions wreq perms (RolePermission grants) = perms \\ setFromList [g | (g, w) <- toList grants, w >= wreq]
filterPermissions _ perms (RoleRoute _ _) = perms

authorizeFromAttributes :: Role -> Set Text -> WriteRequest -> Bool
authorizeFromAttributes _ attrs _ | null attrs = False
authorizeFromAttributes role attrs wreq = null $ (filterPermissions wreq) attrs role

authorizeFromPath :: Role -> URL -> WriteRequest-> Bool
authorizeFromPath Administrator _ _ = True
authorizeFromPath (RoleGroup roles) route wreq = or [authorizeFromPath role route wreq | role <- roles]
authorizeFromPath (RolePermission _) route wreq = False
authorizeFromPath (RoleRoute route' wreq') route wreq = route == route' && wreq' >= wreq


-- * From JSON

instance FromJSON Role where
  parseJSON (String s) | s ==  "Administrator" = return Administrator
  parseJSON (Array os) = do
    roles <- mapM parseJSON (toList os)
    return $ RoleGroup roles

  parseJSON Null = return $ RoleGroup []
  parseJSON (String s) = case (Text.splitAt 1 s) of
    ("/", _) -> let (route, wreq) = isWriteReq s in return $ RoleRoute route wreq
    _ -> return . RolePermission . setFromList $ map (isWriteReq) (words s)
    where isWriteReq s | not (null s)  = if Text.last s == '+'
                                       then (init s, WriteRequest)
                                       else  (s, ReadRequest)
