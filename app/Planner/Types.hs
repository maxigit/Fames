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
data TypedLine = CommentL 
              | HeaderL HeaderType Text
              | TextL Text
              | EndL
              | HashL DocumentHash
              deriving (Show, Read, Eq, Ord)


data HeaderType = LayoutH | ShelvesH | InitialH | StocktakeH | BoxesH | MovesH | TagsH | OrientationsH | TitleH
  deriving (Show, Read, Eq, Ord)

-- * Scenario
-- | Description of warehouse. Initial stage plus way modify it.
-- It is only a description, .i.e elements are either text or a sha of it.
data Scenario = Scenario
  { sInitialState :: Maybe DocumentHash
  , sSteps        :: [Step]
  , sLayout ::  Maybe DocumentHash
  } deriving (Read, Show)

-- | Text is the original line.
-- usefull to reconstruct the orignal file
data Step = Step HeaderType DocumentHash Text deriving (Show, Read)



