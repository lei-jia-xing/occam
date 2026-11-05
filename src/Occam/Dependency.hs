module Occam.Dependency
    ( ObjectName
    , ObjectKind(..)
    , DependencyType(..)
    , ObjectAction(..)
    , DependencyGraph(..)
    , emptyGraph
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
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
    = DefDep   -- ^ Creates an object
    | UseDep   -- ^ Reads an object
    | ModDep   -- ^ Alters / Mutates an object
    deriving (Eq, Ord, Show)

data ObjectAction = ObjectAction
    { actionKind :: !ObjectKind
    , actionType :: !DependencyType
    , actionName :: !ObjectName
    } deriving (Eq, Ord, Show)

data DependencyGraph = DependencyGraph
    { graphStmts   :: !(Set StatementId)
    , graphActions :: !(Map StatementId [ObjectAction])
    , graphEdges   :: !(Map StatementId (Set StatementId)) -- stmt -> dependencies
    } deriving (Eq, Show)

emptyGraph :: DependencyGraph
emptyGraph = DependencyGraph
    { graphStmts   = Set.empty
    , graphActions = Map.empty
    , graphEdges   = Map.empty
    }
