{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Role where

import ClassyPrelude.Yesod
import qualified Data.Text as Text
import qualified Data.Set as Set
-- * Types
-- | Read or Write request
data WriteRequest = ReadRequest | WriteRequest deriving (Eq, Read, Show, Ord)

type Permissions = Set (Text, WriteRequest)
type URL = Text

newtype RoleFor = RoleFor {roleFor :: (Maybe Text -> Role) }


instance Show RoleFor where
  show _ = "RoleFor"

-- | A role, can grant permissions.
-- An object can require  many permissions.
-- To be authorized the object needs all the permissions to be grant from the role.
-- There is no distinction between a user and a role.
data Role = Administrator -- ^ As access to everything
          | RoleGroup [Role] -- ^ groups role
          | RolePermission Permissions -- ^ a set of permissions
          | RoleRoute (URL) WriteRequest -- ^ a url. Override everything else
  deriving(Show, Read, Eq)

-- * Checking permissions
-- | Check if the given route is allowed, based on it path .
-- Ignores any route attributes.

-- isRouteAllowed :: URL -> WriteRequest -> Role -> Bool
-- isRouteAllowed _ _ Administrator =  True
-- isRouteAllowed route r (RoleGroup roles) = or $ map (isRouteAllowed route r) roles
-- isRouteAllowed _ _ (RolePermission _) = False
-- isRouteAllowed route r (RoleRoute route' r') | route == route' =
--   case (r,r') of
--     (WriteRequest, ReadRequest) -> False
--     (_, _) -> True


-- | Remove permissions which are granted
filterPermissions :: WriteRequest -> Set Text -> Role -> Set Text
filterPermissions _ _ Administrator = mempty
filterPermissions wreq perms (RoleGroup roles) = foldl' (filterPermissions wreq) perms roles
filterPermissions wreq perms (RolePermission grants) = perms \\ setFromList [g | (g, w) <- toList grants, w >= wreq]
filterPermissions _ perms (RoleRoute _ _) = perms

authorizeFromAttributes :: Role -> Set Text -> WriteRequest -> Bool
authorizeFromAttributes _ attrs _ | null attrs = False
authorizeFromAttributes role attrs wreq = null $ (filterPermissions wreq) attrs' role
  where attrs'= Set.filter ('=' `notElem`) attrs -- remove key=value attributes

authorizeFromPath :: Role -> URL -> WriteRequest-> Bool
authorizeFromPath Administrator _ _ = True
authorizeFromPath (RoleGroup roles) route wreq = or [authorizeFromPath role route wreq | role <- roles]
authorizeFromPath (RolePermission _) _ _ = False
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
    where isWriteReq r | not (null r), Text.last r == '+' = (initEx r, WriteRequest)
                       | otherwise = (r, ReadRequest)
  parseJSON _ = error "Can't parse Role"
