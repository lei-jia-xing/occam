module Occam.Slicer (
    backwardSlice,
    backwardSliceMany,
    backwardSliceFast,
    backwardSliceManyFast,
) where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Occam.Core (StatementId (..))
import Occam.Dependency (DependencyGraph (..))

-- | Computes the backward slice reachable from a target statement.
backwardSlice :: StatementId -> DependencyGraph -> Set StatementId
backwardSlice target = backwardSliceMany (Set.singleton target)

-- | Computes the backward slice reachable from a target statement ID directly as an 'IntSet'.
backwardSliceFast :: Int -> DependencyGraph -> IntSet
backwardSliceFast target = backwardSliceManyFast (IntSet.singleton target)

{- | Computes the union of the backward slices of several targets in a single
traversal, visiting each statement at most once.
-}
backwardSliceMany :: Set StatementId -> DependencyGraph -> Set StatementId
backwardSliceMany roots graph@DependencyGraph {..}
    | IntMap.null graphFastEdges = go roots (Set.toList roots)
    | otherwise =
        let !rootInts = IntSet.fromList (map unStatementId (Set.toList roots))
            !visitedInts = backwardSliceManyFast rootInts graph
         in Set.fromList (map StatementId (IntSet.toList visitedInts))
    where
        go !visited [] = visited
        go !visited (curr : rest) =
            let deps = Map.findWithDefault Set.empty curr graphEdges
                newNodes = deps `Set.difference` visited
             in if Set.null newNodes
                    then go visited rest
                    else go (visited `Set.union` newNodes) (Set.foldl' (flip (:)) rest newNodes)

{- | Computes the union of the backward slices of several integer statement targets
directly over 'graphFastEdges', visiting each statement at most once.
-}
backwardSliceManyFast :: IntSet -> DependencyGraph -> IntSet
backwardSliceManyFast roots DependencyGraph {..}
    | IntMap.null graphFastEdges =
        let legacyRoots = Set.fromList (map StatementId (IntSet.toList roots))
            goLegacy !visited [] = visited
            goLegacy !visited (curr : rest) =
                let deps = Map.findWithDefault Set.empty curr graphEdges
                    newNodes = deps `Set.difference` visited
                 in if Set.null newNodes
                        then goLegacy visited rest
                        else goLegacy (visited `Set.union` newNodes) (Set.foldl' (flip (:)) rest newNodes)
            legacyVisited = goLegacy legacyRoots (Set.toList legacyRoots)
         in IntSet.fromList (map unStatementId (Set.toList legacyVisited))
    | otherwise = goFast roots (IntSet.toList roots)
    where
        goFast !visited [] = visited
        goFast !visited (curr : rest) =
            let deps = IntMap.findWithDefault IntSet.empty curr graphFastEdges
                newNodes = deps `IntSet.difference` visited
             in if IntSet.null newNodes
                    then goFast visited rest
                    else goFast (visited `IntSet.union` newNodes) (IntSet.foldl' (flip (:)) rest newNodes)
