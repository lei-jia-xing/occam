module Occam.Slicer (
    backwardSlice,
) where

import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Occam.Core (StatementId)
import Occam.Dependency (DependencyGraph (..))

-- | Computes the backward slice reachable from a target statement.
backwardSlice :: StatementId -> DependencyGraph -> Set StatementId
backwardSlice target DependencyGraph {..} = go (Set.singleton target) [target]
    where
        go visited [] = visited
        go visited (curr : queue) =
            let deps = Map.findWithDefault Set.empty curr graphEdges
                newNodes = Set.difference deps visited
                visited' = Set.union visited newNodes
             in go visited' (queue ++ Set.toList newNodes)
