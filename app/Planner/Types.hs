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
-- | AST-ish of a parsed document
-- data Section = LayoutS Content -- describe the layout of the shelves. How to display them
--              | ShelvesS Content -- shelves description
--              | InitialS DocumentHash -- Initial state
--              | StocktakeS Content -- list of boxes and their location
--              | BoxesS Content -- list of boxes without location
--              | MovesS Content -- list of moves : boxes -> shelves
       
data TypedLine = CommentL 
              | HeaderL HeaderType
              | TextL Text
              | EndL
              | HashL DocumentHash
              deriving (Show, Read, Eq, Ord)


data HeaderType = LayoutH | ShelvesH | InitialH | StocktakeH | BoxesH | MovesH
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



