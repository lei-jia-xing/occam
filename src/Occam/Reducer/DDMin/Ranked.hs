-- | Ranked lane scheduling and bounded projected-window execution for DDMin.
module Occam.Reducer.DDMin.Ranked (
    buildRankedSchedule,
) where

import Control.Monad (forM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.IORef (newIORef, readIORef)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (sortOn)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ord (Down (..))
import Occam.Core (
    OracleOutcome (..),
    Statement (..),
    StatementId (..),
 )
import Occam.Metrics (
    FamilyTag (..),
    recordAcceptedBytesSaved,
    recordGeneratedSkipped,
 )
import Occam.Pool (
    poolJobCount,
    poolMetricsStore,
    reportCandidateCount,
    testOrderedTagged,
 )
import Occam.Reducer.Config (
    LaneOrdering (..),
    LaneScheduling (..),
    ProjectionWindow (..),
    ProjectionWindowOrder (..),
    RankedScheduleConfig (..),
    SingletonResume (..),
    StatementLane (..),
 )
import Occam.Reducer.DDMin.Core (
    DDMinEnv (..),
    DDMinOutcome,
    Feedback (..),
    LaneState (..),
    SeedToken (..),
    classifyDDMinSeed,
    dedupIntSets,
    recordDDMinVerdict,
 )
import Occam.Reducer.Statement (statementIdsInt)

data WindowCandidate = WindowCandidate
    { wcLane :: !StatementLane
    , wcTag :: !FamilyTag
    , wcEstimatedSaved :: !Int
    , wcProjectedSaved :: !Int
    , wcToken :: !SeedToken
    }

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

orderWindowCandidates :: ProjectionWindowOrder -> [WindowCandidate] -> [WindowCandidate]
orderWindowCandidates WindowOrderPreserve items = items
orderWindowCandidates WindowOrderCrossEstimated items = sortOn (Down . wcEstimatedSaved) items
orderWindowCandidates WindowOrderCrossProjected items = sortOn (Down . wcProjectedSaved) items
orderWindowCandidates WindowOrderWithinLaneProjected items =
    refill (map wcLane items) sortedC sortedD sortedS
    where
        sortedFor lane = sortOn (Down . wcProjectedSaved) [c | c <- items, wcLane c == lane]
        sortedC = sortedFor LaneComplement
        sortedD = sortedFor LaneSingleton
        sortedS = sortedFor LaneSubset
        refill [] _ _ _ = []
        refill (LaneComplement : rest) (c : cs) ds ss = c : refill rest cs ds ss
        refill (LaneSingleton : rest) cs (d : ds) ss = d : refill rest cs ds ss
        refill (LaneSubset : rest) cs ds (s : ss) = s : refill rest cs ds ss
        refill (_ : rest) cs ds ss = refill rest cs ds ss

-- Unified ranked scheduler: orthogonal lane ordering, lane quota
-- scheduling, singleton resumption, and optional bounded projection window.
buildRankedSchedule ::
    DDMinEnv ->
    RankedScheduleConfig ->
    LaneState ->
    [Statement] ->
    IntSet.IntSet ->
    IntMap.IntMap Int ->
    Int ->
    [[[Statement]]] ->
    [(Int, Bool, IO DDMinOutcome)]
buildRankedSchedule env cfg laneState current currentInts weights currentLen partitions =
    case (ddClosureAware env, rankedProjectionWindow cfg) of
        (True, ProjectionWindow w order) ->
            [ (2, resumes, tryProjectedWindow env w order retainedBytes current currentInts currentLen scheduled)
            ]
        _ ->
            [(2, resumes, tryScheduledStream env current currentInts currentLen scheduled)]
    where
        resumes = rankedSingletonResume cfg /= ResumeNone
        weightOf sid = IntMap.findWithDefault 0 sid weights
        retainedBytes = IntSet.foldl' (\total sid -> total + weightOf sid) 0
        rankLane lane tag seeds =
            let nonTrivial = dedupIntSets [seed | seed <- seeds, not (IntSet.null seed), seed /= currentInts]
                ordered = case rankedLaneOrdering cfg of
                    LaneOrderPartition -> nonTrivial
                    LaneOrderEstimatedBytes -> sortOn retainedBytes nonTrivial
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
        singletonIds = orderRankedSingletonIds cfg laneState weightOf (IntSet.toList currentInts)
        singletons =
            [(LaneSingleton, TagStatementComplement, IntSet.delete sid currentInts) | sid <- singletonIds]
        laneQueue LaneComplement = complements
        laneQueue LaneSingleton = singletons
        laneQueue LaneSubset = subsets
        scheduled = interleaveRankedLanes cfg laneState resumes laneQueue

orderRankedSingletonIds :: RankedScheduleConfig -> LaneState -> (Int -> Int) -> [Int] -> [Int]
orderRankedSingletonIds cfg laneState weightOf orderedSids =
    case (rankedSingletonResume cfg, lsResumeAnchor laneState) of
        (ResumeCursorOnly, Just (StatementId anchorId, _)) ->
            let (before, after) = span (<= anchorId) orderedSids in after ++ before
        (ResumeWeightStableTies, Just (StatementId anchorId, _)) ->
            let (before, after) = span (<= anchorId) orderedSids
             in sortOn (Down . weightOf) (after ++ before)
        (ResumeWeightMonotonic, Just (StatementId anchorId, anchorWeight)) ->
            let sorted = sortOn (\sid -> (Down (weightOf sid), sid)) orderedSids
                (before, after) =
                    span (\sid -> (Down (weightOf sid), sid) <= (Down anchorWeight, anchorId)) sorted
             in after ++ before
        _ -> case rankedLaneOrdering cfg of
            LaneOrderPartition -> orderedSids
            LaneOrderEstimatedBytes -> sortOn (Down . weightOf) orderedSids

interleaveRankedLanes ::
    RankedScheduleConfig -> LaneState -> Bool -> (StatementLane -> [a]) -> [a]
interleaveRankedLanes cfg laneState resumes laneQueue = case rankedLaneScheduling cfg of
    LaneSerial -> complements ++ subsets ++ singletons
    LaneRoundRobin
        | resumes
        , Just _ <- lsResumeAnchor laneState ->
            mergeWeightedLanes [(1, singletons), (1, complements), (1, subsets)]
        | otherwise ->
            mergeWeightedLanes [(1, complements), (1, subsets), (1, singletons)]
    LaneBoundedPreference rawQuota -> interleaveBoundedPreference (max 1 rawQuota) laneState laneQueue
    where
        complements = laneQueue LaneComplement
        singletons = laneQueue LaneSingleton
        subsets = laneQueue LaneSubset

interleaveBoundedPreference :: Int -> LaneState -> (StatementLane -> [a]) -> [a]
interleaveBoundedPreference maxQuota laneState laneQueue = case lsLastAcceptedLane laneState of
    Nothing ->
        -- Test both halves of the initial 2-way split before singletons/subsets,
        -- then interleave all three lanes fairly.
        let (headC, restC) = splitAt (min 2 maxQuota) (laneQueue LaneComplement)
         in headC ++ mergeWeightedLanes [(1, laneQueue LaneSingleton), (1, laneQueue LaneSubset), (1, restC)]
    Just LaneSingleton ->
        -- Weight-monotonic resumption probes the next singleton first; on a miss,
        -- yield immediately to multi-statement complements and subsets.
        mergeWeightedLanes
            [(1, laneQueue LaneSingleton), (1, laneQueue LaneComplement), (1, laneQueue LaneSubset)]
    Just winLane ->
        -- Coarse winning lanes (Complement / Subset) keep bounded cyclic preference
        -- while preserving guaranteed slots for the other two lanes.
        let winQuota = min maxQuota (1 + lsLaneStreak laneState)
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

-- Bound speculative projection and rendering to a fixed window of W seeds.
-- Deduplicates projected IntSets before rendering and isolates window ordering.
tryProjectedWindow ::
    DDMinEnv ->
    Int ->
    ProjectionWindowOrder ->
    (IntSet.IntSet -> Int) ->
    [Statement] ->
    IntSet.IntSet ->
    Int ->
    [ScheduledSeed] ->
    IO DDMinOutcome
tryProjectedWindow env rawSize order retainedBytes current currentInts currentLen = search
    where
        wSize = max 1 rawSize
        jobs = poolJobCount (ddPool env)
        search [] = pure Nothing
        search seeds = do
            let (window, remaining) = splitAt wSize seeds
            seenProjected <- newIORef HashSet.empty
            classified <- forM window (classifyWindowSeed seenProjected)
            runWindow remaining (orderWindowCandidates order (catMaybes classified))
        classifyWindowSeed seenProjected (lane, tag, seed) = do
            token <- classifyDDMinSeed env (Just seenProjected) tag current currentInts currentLen seed
            pure (fmap (mkWindowCandidate lane tag seed) token)
        mkWindowCandidate lane tag seed cand =
            WindowCandidate
                { wcLane = lane
                , wcTag = tag
                , wcEstimatedSaved = currentLen - retainedBytes seed
                , wcProjectedSaved = seedSavedBytes currentLen cand
                , wcToken = cand
                }
        runWindow remaining [] = search remaining
        runWindow remaining items = collectWindow remaining jobs [] HashSet.empty items
        collectWindow remaining _ acc _ [] = testWindow remaining (reverse acc) [] Nothing
        collectWindow remaining 0 acc _ rest = testWindow remaining (reverse acc) rest Nothing
        collectWindow remaining n acc seen (cand : rest) = case wcToken cand of
            SeedAccepted kept saved ->
                testWindow remaining (reverse acc) rest (Just (wcLane cand, wcTag cand, kept, saved))
            SeedFresh kept count sql -> do
                tested <- readIORef (fbEvaluated (ddFeedback env))
                if HashMap.member sql tested || HashSet.member sql seen
                    then do
                        recordGeneratedSkipped (poolMetricsStore (ddPool env)) "projected-window-duplicate"
                        collectWindow remaining n acc seen rest
                    else
                        collectWindow
                            remaining
                            (n - 1)
                            ((wcLane cand, wcTag cand, kept, count, sql) : acc)
                            (HashSet.insert sql seen)
                            rest
        testWindow remaining batch rest barrier =
            resolveMixedBatch env currentLen batch barrier (runWindow remaining rest)

seedSavedBytes :: Int -> SeedToken -> Int
seedSavedBytes _ (SeedAccepted _ saved) = saved
seedSavedBytes currentLen (SeedFresh _ _ sql) = currentLen - BS.length sql

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
