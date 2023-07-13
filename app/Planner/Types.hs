{-# LANGUAGE DeriveGeneric #-}
module Planner.Types where

import ClassyPrelude
import Model.DocumentKey

type Content = Either DocumentHash [Text]

data Section = Section
   { sectionType :: HeaderType
   , sectionContent :: Content
   , sectionTitle :: Text
   } deriving (Show, Eq)

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
              deriving (Show, Eq, Ord)


data HeaderType
  = LayoutH
  | ShelvesH
  | InitialH
  | StocktakeH [Text]
  | BoxesH  [Text]
  | MovesH  [Text]
  | TagsH  [Text]
  | MovesAndTagsH [Text] -- not needed but to be compatible with Moves
  | ShelfTagsH
  | ShelfSplitH
  | ShelfJoinH
  | UpdateShelvesH
  | OrientationsH 
  | TransformTagsH [Text]
  | ClonesH [Text]
  | DeletesH
  | ImportH
  | TitleH
  | ColourMapH
  deriving (Show, Eq, Ord, Generic)

-- * Scenario 
-- | Description of warehouse. Initial stage plus way modify it.
-- It is only a description, .i.e elements are either text or a sha of it.
data Scenario = Scenario
  { sInitialState :: Maybe DocumentHash
  , sSteps        :: [Step]
  , sLayout ::  Maybe DocumentHash
  , sColourMap :: [DocumentHash]
  } deriving (Show)

instance Semigroup Scenario where
  _ <> sc@(Scenario (Just _i') _ _ _)  = sc
  (Scenario i steps l cmap) <> (Scenario _i' steps' l' cmap') = Scenario i (steps <> steps') (l' <|> l) (cmap <> cmap')
instance Monoid Scenario where
  mempty = Scenario Nothing [] Nothing mempty
  

-- | Text is the original line.
-- usefull to reconstruct the orignal file
data Step = Step HeaderType DocumentHash Text
          | SavingPoint
          deriving (Show, Eq)



