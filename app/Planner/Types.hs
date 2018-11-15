module Planner.Types where

import ClassyPrelude
import Model.DocumentKey
import WarehousePlanner.Base


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
              | HashL DocumentHash
              deriving (Show, Read, Eq, Ord)


data HeaderType
  = LayoutH
  | ShelvesH
  | InitialH
  | StocktakeH
  | BoxesH 
  | MovesH 
  | TagsH 
  | MovesAndTagsH
  | OrientationsH 
  | TransformTagsH
  | TitleH
  deriving (Show, Read, Eq, Ord)

-- * Scenario
-- | Description of warehouse. Initial stage plus way modify it.
-- It is only a description, .i.e elements are either text or a sha of it.
data Scenario = Scenario
  { sInitialState :: Maybe DocumentHash
  , sSteps        :: [Step]
  , sLayout ::  Maybe DocumentHash
  } deriving (Read, Show)

instance Monoid Scenario where
  mempty = Scenario Nothing [] Nothing
  _ `mappend` sc@(Scenario (Just i') _ _)  = sc
  (Scenario i steps l) `mappend` (Scenario i' steps' l') = Scenario i (steps <> steps') (l' <|> l)
  

-- | Text is the original line.
-- usefull to reconstruct the orignal file
data Step = Step HeaderType DocumentHash Text
          | SavingPoint
          deriving (Show, Read, Eq)



