{-# LANGUAGE DeriveGeneric #-}
module Planner.Types where

import ClassyPrelude
import Model.DocumentKey
import WarehousePlanner.Base
import GHC.Generics(Generic)


type Content = Either DocumentHash [Text]

data Section = Section
   { sectionType :: HeaderType
   , sectionContent :: Content
   , sectionTitle :: Text
   } deriving (Show, Read, Eq)

-- * Content Types
-- TODO needs refactoring. Now we are using orgmode drawer
-- for planner instructions. HeaderL should only be drawer
-- and org-mode drawer could be simple comment.
-- Also the DocumentHash probably needs to be removed.
-- It is not useful anymore now that we can save planner file on the server
-- (via Dropbox)
data TypedLine = CommentL 
              | HeaderL HeaderType Text
              | TextL Text
              | EndL
              | EndSectionL
              | HashL DocumentHash
              deriving (Show, Read, Eq, Ord)


data HeaderType
  = LayoutH
  | ShelvesH
  | InitialH
  | StocktakeH [Text]
  | BoxesH  [Text]
  | MovesH  [Text]
  | TagsH 
  | MovesAndTagsH [Text] -- not needed but to be compatible with Moves
  | OrientationsH 
  | TransformTagsH
  | ClonesH [Text]
  | DeletesH
  | TitleH
  deriving (Show, Read, Eq, Ord, Generic)

-- * Scenario
-- | Description of warehouse. Initial stage plus way modify it.
-- It is only a description, .i.e elements are either text or a sha of it.
data Scenario = Scenario
  { sInitialState :: Maybe DocumentHash
  , sSteps        :: [Step]
  , sLayout ::  Maybe DocumentHash
  } deriving (Read, Show)

instance Semigroup Scenario where
  _ <> sc@(Scenario (Just i') _ _)  = sc
  (Scenario i steps l) <> (Scenario i' steps' l') = Scenario i (steps <> steps') (l' <|> l)
instance Monoid Scenario where
  mempty = Scenario Nothing [] Nothing
  

-- | Text is the original line.
-- usefull to reconstruct the orignal file
data Step = Step HeaderType DocumentHash Text
          | SavingPoint
          deriving (Show, Read, Eq)



