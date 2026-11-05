module Occam.Dependency (
    ObjectName,
    ObjectKind (..),
    DependencyType (..),
    ObjectAction (..),
    DependencyGraph (..),
    emptyGraph,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Occam.Core (StatementId)

type ObjectName = Text

data ObjectKind
    = TableObject
    | ColumnObject
    | IndexObject
    | ViewObject
    | FunctionObject
    deriving (Eq, Ord, Show)

data DependencyType
    = -- | Creates an object
      DefDep
    | -- | Reads an object
      UseDep
    | -- | Alters / Mutates an object
      ModDep
    deriving (Eq, Ord, Show)

data ObjectAction = ObjectAction
    { actionKind :: !ObjectKind
    , actionType :: !DependencyType
    , actionName :: !ObjectName
    }
    deriving (Eq, Ord, Show)

data DependencyGraph = DependencyGraph
    { graphStmts :: !(Set StatementId)
    , graphActions :: !(Map StatementId [ObjectAction])
    , graphEdges :: !(Map StatementId (Set StatementId)) -- stmt -> dependencies
    }
    deriving (Eq, Show)

emptyGraph :: DependencyGraph
emptyGraph =
    DependencyGraph
        { graphStmts = Set.empty
        , graphActions = Map.empty
        , graphEdges = Map.empty
        }
