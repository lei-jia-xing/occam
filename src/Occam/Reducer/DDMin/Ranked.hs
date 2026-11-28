{- | Canonical byte-ranked lane scheduling and unwindowed execution for DDMin.
Lanes use estimated-byte ordering, bounded preference quota 4, and
weight-monotonic singleton resumption.
-}
module Occam.Reducer.DDMin.Ranked (
    buildRankedSchedule,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.HashSet qualified as HashSet
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Occam.Core.Oracle (OracleOutcome (..))
import Occam.Core.Types (Statement (..), StatementId (..))
import Occam.Metrics (
    FamilyTag (..),
    recordAcceptedBytesSaved,
 )
import Occam.Pool (
    poolJobCount,
    poolMetricsStore,
    reportCandidateCount,
    testOrderedTagged,
 )
import Occam.Reducer.Config (StatementLane (..))
import Occam.Reducer.DDMin.Core (
    DDMinEnv (..),
    DDMinOutcome,
    LaneState (..),
    SeedToken (..),
    classifyDDMinSeed,
    dedupIntSets,
    recordDDMinVerdict,
 )
import Occam.Reducer.Statement (statementIdsInt)

type ScheduledSeed = (StatementLane, FamilyTag, IntSet.IntSet)
type MixedBatchItem = (StatementLane, FamilyTag, [Statement], Int, ByteString)
type MixedBarrier = (StatementLane, FamilyTag, [Statement], Int)

mergeWeightedLanes :: [(Int, [a])] -> [a]
mergeWeightedLanes = go False []
    where
        go _ [] [] = []
        go progressed acc []
            | progressed = go False [] (reverse acc)
            | otherwise = []
        go progressed acc ((q, xs) : rest) = case splitAt (max 1 q) xs of
            ([], _) -> go progressed acc rest
            (taken, remaining) -> taken ++ go True ((q, remaining) : acc) rest

{- | Canonical ranked scheduler: estimated-byte lane ordering, bounded lane
preference quota 4, and weight-monotonic singleton resumption.
-}
buildRankedSchedule ::
    DDMinEnv ->
    LaneState ->
    [Statement] ->
    IntSet.IntSet ->
    IntMap.IntMap Int ->
    Int ->
    [[[Statement]]] ->
    [(Int, Bool, IO DDMinOutcome)]
buildRankedSchedule env laneState current currentInts weights currentLen partitions =
    [(2, True, tryScheduledStream env current currentInts currentLen scheduled)]
    where
        weightOf sid = IntMap.findWithDefault 0 sid weights
        retainedBytes = IntSet.foldl' (\total sid -> total + weightOf sid) 0
        rankLane lane tag seeds =
            let nonTrivial = dedupIntSets [seed | seed <- seeds, not (IntSet.null seed), seed /= currentInts]
                ordered = sortOn retainedBytes nonTrivial
             in [(lane, tag, seed) | seed <- ordered]
        groups = map statementIdsInt (concat partitions)
        complements =
            rankLane
                LaneComplement
                TagStatementComplement
                [currentInts `IntSet.difference` group | group <- groups, IntSet.size group > 1]
        -- Keep smallest subsets first: small subset byte strings remain
        -- invariant across deletions and hit exact-byte memoization.
        subsets = rankLane LaneSubset TagStatementSubset groups
        singletonIds = orderRankedSingletonIds laneState weightOf (IntSet.toList currentInts)
        singletons =
            [(LaneSingleton, TagStatementComplement, IntSet.delete sid currentInts) | sid <- singletonIds]
        laneQueue LaneComplement = complements
        laneQueue LaneSingleton = singletons
        laneQueue LaneSubset = subsets
        scheduled = interleaveRankedLanes laneState laneQueue

orderRankedSingletonIds :: LaneState -> (Int -> Int) -> [Int] -> [Int]
orderRankedSingletonIds laneState weightOf orderedSids = case lsResumeAnchor laneState of
    Just (StatementId anchorId, anchorWeight) ->
        let sorted = sortOn (\sid -> (Down (weightOf sid), sid)) orderedSids
            (before, after) =
                span (\sid -> (Down (weightOf sid), sid) <= (Down anchorWeight, anchorId)) sorted
         in after ++ before
    Nothing -> sortOn (Down . weightOf) orderedSids

interleaveRankedLanes :: LaneState -> (StatementLane -> [a]) -> [a]
interleaveRankedLanes = interleaveBoundedPreference

interleaveBoundedPreference :: LaneState -> (StatementLane -> [a]) -> [a]
interleaveBoundedPreference laneState laneQueue = case lsLastAcceptedLane laneState of
    Nothing ->
        -- Test both halves of the initial 2-way split before singletons/subsets,
        -- then interleave all three lanes fairly.
        let (headC, restC) = splitAt 2 (laneQueue LaneComplement)
         in headC ++ mergeWeightedLanes [(1, laneQueue LaneSingleton), (1, laneQueue LaneSubset), (1, restC)]
    Just LaneSingleton ->
        -- Weight-monotonic resumption probes the next singleton first; on a miss,
        -- yield immediately to multi-statement complements and subsets.
        mergeWeightedLanes
            [(1, laneQueue LaneSingleton), (1, laneQueue LaneComplement), (1, laneQueue LaneSubset)]
    Just winLane ->
        -- Coarse winning lanes (Complement / Subset) keep bounded cyclic preference
        -- while preserving guaranteed slots for the other two lanes.
        let winQuota = min 4 (1 + lsLaneStreak laneState)
            baseOrder = [LaneComplement, LaneSingleton, LaneSubset]
            others = [(1, laneQueue l) | l <- baseOrder, l /= winLane]
         in mergeWeightedLanes ((winQuota, laneQueue winLane) : others)

-- Unwindowed ranked stream supporting ordered concurrency (`jobs >= 1`).
tryScheduledStream ::
    DDMinEnv -> [Statement] -> IntSet.IntSet -> Int -> [ScheduledSeed] -> IO DDMinOutcome
tryScheduledStream env current currentInts currentLen = pull
    where
        jobs = poolJobCount (ddPool env)
        pull [] = pure Nothing
        pull seeds = collect jobs [] HashSet.empty seeds
        collect _ acc _ [] = testAndContinue (reverse acc) [] Nothing
        collect 0 acc _ rest = testAndContinue (reverse acc) rest Nothing
        collect n acc seen ((lane, tag, seedInts) : rest) = do
            token <- classifyDDMinSeed env Nothing tag current currentInts currentLen seedInts
            case token of
                Nothing -> collect n acc seen rest
                Just (SeedAccepted kept saved) ->
                    testAndContinue (reverse acc) rest (Just (lane, tag, kept, saved))
                Just (SeedFresh kept count sql)
                    | HashSet.member sql seen -> collect n acc seen rest
                    | otherwise ->
                        collect (n - 1) ((lane, tag, kept, count, sql) : acc) (HashSet.insert sql seen) rest
        testAndContinue batch rest barrier =
            resolveMixedBatch env currentLen batch barrier (pull rest)

resolveMixedBatch ::
    DDMinEnv -> Int -> [MixedBatchItem] -> Maybe MixedBarrier -> IO DDMinOutcome -> IO DDMinOutcome
resolveMixedBatch env currentLen batch barrier onMiss = do
    accepted <- testMixedBatch env currentLen batch
    case (accepted, barrier) of
        (Just result, _) -> pure (Just result)
        (Nothing, Just (lane, tag, kept, saved)) -> do
            recordAcceptedBytesSaved (poolMetricsStore (ddPool env)) tag saved
            pure (Just (lane, kept, saved))
        (Nothing, Nothing) -> onMiss

testMixedBatch :: DDMinEnv -> Int -> [MixedBatchItem] -> IO DDMinOutcome
testMixedBatch _ _ [] = pure Nothing
testMixedBatch env currentLen batch = do
    let pool = ddPool env
    mapM_ (\(_, _, _, count, sql) -> reportCandidateCount pool sql count) batch
    outcomes <- testOrderedTagged pool [(tag, sql) | (_, tag, _, _, sql) <- batch]
    let retired = zip batch outcomes
    mapM_ (\((_, _, _, _, sql), (outcome, _)) -> recordDDMinVerdict env sql outcome) retired
    case listToMaybe [candidate | (candidate, (outcome, _)) <- retired, outcome == OracleAccepted] of
        Just (lane, tag, kept, _, sql) -> do
            let saved = currentLen - BS.length sql
            recordAcceptedBytesSaved (poolMetricsStore pool) tag saved
            pure (Just (lane, kept, saved))
        Nothing -> pure Nothing
