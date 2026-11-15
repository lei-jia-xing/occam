module Occam.Slicer (
    backwardSlice,
    backwardSliceMany,
) where

import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Occam.Core (StatementId)
import Occam.Dependency (DependencyGraph (..))

-- | Computes the backward slice reachable from a target statement.
backwardSlice :: StatementId -> DependencyGraph -> Set StatementId
backwardSlice target = backwardSliceMany (Set.singleton target)

{- | Computes the union of the backward slices of several targets in a single
traversal, visiting each statement at most once.
-}
backwardSliceMany :: Set StatementId -> DependencyGraph -> Set StatementId
backwardSliceMany roots DependencyGraph {..} = go roots (Set.toList roots)
    where
        go !visited [] = visited
        go !visited (curr : rest) =
            let deps = Map.findWithDefault Set.empty curr graphEdges
                newNodes = deps `Set.difference` visited
             in if Set.null newNodes
                    then go visited rest
                    else go (visited `Set.union` newNodes) (Set.foldl' (flip (:)) rest newNodes)
