module Planner.Types where

import ClassyPrelude
import Model.DocumentKey
import WarehousePlanner.Base


type Content = Either DocumentHash [Text]

data Section = Section
   { sectionType :: HeaderType
   , sectionContent :: Content
   } deriving (Show, Read, Eq)

-- * Content Types
data TypedLine = CommentL 
              | HeaderL HeaderType
              | TextL Text
              | EndL
              | HashL DocumentHash
              deriving (Show, Read, Eq, Ord)


data HeaderType = LayoutH | ShelvesH | InitialH | StocktakeH | BoxesH | MovesH | TagsH | OrientationsH
  deriving (Show, Read, Eq, Ord)

-- * Scenario
-- | Description of warehouse. Initial stage plus way modify it.
-- It is only a description, .i.e elements are either text or a sha of it.
data Scenario = Scenario
  { sInitialState :: Maybe DocumentHash
  , sSteps        :: [Step]
  , sLayout ::  Maybe DocumentHash
  } deriving (Read, Show)

data Step = Step HeaderType DocumentHash deriving (Show, Read)



