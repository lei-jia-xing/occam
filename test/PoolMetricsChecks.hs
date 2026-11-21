module PoolMetricsChecks (tests) where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, readMVar, threadDelay)
import Control.Exception (SomeException, displayException, throwIO, try)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Occam.Core (OracleInconclusiveError (..), OracleOutcome (..))
import Occam.Facts (ParserEditCategory (..))
import Occam.Metrics (
    FamilyCounters (..),
    FamilyTag (..),
    MetricPolicy (..),
    MetricsSnapshot (..),
    MetricsStore,
    newMetricsStore,
    snapshotMetrics,
 )
import Occam.Oracle (OracleObservation (..), OracleObservationDetails (..))
import Occam.Pool (OraclePool, PoolConfig (..), newOraclePool, poolLiveWorkers, testOrderedTagged)
import System.Exit (ExitCode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
    testGroup
        "Pool cost attribution"
        [ testCase "tags attribute retired and physical counts" testTaggedAttribution
        , testCase "finished losers attribute completion" testLoserCounts
        , testCase "canceled work has bounded interaction wall" testCanceledWall
        , testCase "cache hits carry no physical cost" testCacheHits
        , testCase "inconclusive records family before throw" testInconclusiveFamily
        , testCase "failed work counts separately from cancels" testFailedWork
        , testCase "async cancel keeps the retired prefix" testCancelAfterHeadRetires
        ]

testStore :: Int -> IO MetricsStore
testStore jobs = newMetricsStore (MetricPolicy "test-policy" "off" jobs)

acceptDetails :: OracleObservationDetails
acceptDetails =
    OracleObservationDetails
        { detailedObservation = OracleFinished ExitSuccess
        , totalElapsedMicros = 0
        , resetElapsedMicros = 0
        , oracleElapsedMicros = Nothing
        }

rejectDetails :: OracleObservationDetails
rejectDetails = acceptDetails {detailedObservation = OracleFinished (ExitFailure 1)}

timeoutDetails :: OracleObservationDetails
timeoutDetails = acceptDetails {detailedObservation = OracleTimedOut}

testPoolWith ::
    Maybe MetricsStore ->
    Int ->
    Bool ->
    (ByteString -> IO OracleObservationDetails) ->
    (Int -> ByteString -> OracleOutcome -> Bool -> IO ()) ->
    IO OraclePool
testPoolWith store jobs cacheEnabled execute retire =
    newOraclePool
        PoolConfig
            { poolJobs = jobs
            , poolCacheEnabled = cacheEnabled
            , poolPermits = Nothing
            , poolPrecomputed = Map.empty
            , poolMetrics = store
            , poolExecute = execute
            , poolBill = const (pure ())
            , poolRetire = retire
            , poolReportCount = Nothing
            }

familyCounters :: MetricsSnapshot -> FamilyTag -> FamilyCounters
familyCounters snapshot tag = Map.findWithDefault emptyCounters tag (snapshotFamilies snapshot)
    where
        emptyCounters =
            FamilyCounters
                { physicalStarts = 0
                , retiredAccept = 0
                , retiredReject = 0
                , retiredInconclusive = 0
                , physicalCompletions = 0
                , completedUnselected = 0
                , completedUnselectedInteractionWallMicros = 0
                , canceled = 0
                , canceledInteractionWallMicros = 0
                , failed = 0
                , failedInteractionWallMicros = 0
                , candidateBuildMicros = 0
                , oracleMicros = Nothing
                , resetMicros = Nothing
                }

-- | Wait for an observed completion event.
awaitCompletions :: MetricsStore -> FamilyTag -> Int -> IO ()
awaitCompletions store tag want = go (10000 :: Int)
    where
        go 0 = assertFailure "timed out waiting for physical completions"
        go n = do
            snapshot <- snapshotMetrics store
            if physicalCompletions (familyCounters snapshot tag) >= want
                then pure ()
                else threadDelay 100 >> go (n - 1)

testTaggedAttribution :: IO ()
testTaggedAttribution = do
    store <- testStore 2
    started <- newEmptyMVar
    never <- newEmptyMVar
    let execute candidate =
            if candidate == "accept"
                then readMVar started >> pure acceptDetails
                else putMVar started () >> readMVar never >> pure rejectDetails
    pool <- testPoolWith (Just store) 2 False execute (\_ _ _ _ -> pure ())
    -- Lookahead starts and blocks before head accepts, settling as started-then-canceled.
    outcomes <-
        testOrderedTagged
            pool
            [(TagStatement, "accept"), (TagSyntax EditList "remove-values_row", "reject")]
    assertEqual "outcomes" [(OracleAccepted, False)] outcomes
    snapshot <- snapshotMetrics store
    let statement = familyCounters snapshot TagStatement
        list = familyCounters snapshot (TagSyntax EditList "remove-values_row")
    assertEqual "statement retired accept" 1 (retiredAccept statement)
    assertEqual "statement starts" 1 (physicalStarts statement)
    assertEqual "statement completions" 1 (physicalCompletions statement)
    assertEqual "lookahead starts" 1 (physicalStarts list)
    assertEqual "lookahead retired nothing" 0 (retiredAccept list + retiredReject list)
    assertEqual "lookahead canceled" 1 (canceled list)
    assertEqual "total starts" 2 (physicalStarts statement + physicalStarts list)

testLoserCounts :: IO ()
testLoserCounts = do
    store <- testStore 2
    let execute candidate =
            if candidate == "winner"
                then awaitCompletions store TagStatement 1 >> pure acceptDetails
                else pure rejectDetails
    pool <- testPoolWith (Just store) 2 False execute (\_ _ _ _ -> pure ())
    -- Winner awaits loser completion so loser settles as finished-but-unselected.
    outcomes <- testOrderedTagged pool [(TagStatement, "winner"), (TagStatement, "loser")]
    assertEqual "outcomes" [(OracleAccepted, False)] outcomes
    snapshot <- snapshotMetrics store
    let counters = familyCounters snapshot TagStatement
    assertEqual "one accept retired" 1 (retiredAccept counters)
    assertEqual "two starts" 2 (physicalStarts counters)
    assertEqual "two completions" 2 (physicalCompletions counters)
    assertEqual "one finished loser" 1 (completedUnselected counters)
    assertEqual "nothing canceled" 0 (canceled counters)

testCanceledWall :: IO ()
testCanceledWall = do
    store <- testStore 3
    startedB <- newEmptyMVar
    readyB <- newEmptyMVar
    startedC <- newEmptyMVar
    readyC <- newEmptyMVar
    never <- newEmptyMVar
    let loser started ready =
            putMVar started ()
                >> threadDelay 100000
                >> putMVar ready ()
                >> readMVar never
                >> pure rejectDetails
    let execute candidate = case candidate of
            "winner" -> readMVar readyB >> readMVar readyC >> pure acceptDetails
            "b" -> loser startedB readyB
            _ -> loser startedC readyC
    pool <- testPoolWith (Just store) 3 False execute (\_ _ _ _ -> pure ())
    -- Both losers block after starting; head accepts and cancels both.
    outcomes <-
        testOrderedTagged pool [(TagStatement, "winner"), (TagStatement, "b"), (TagStatement, "c")]
    assertEqual "outcomes" [(OracleAccepted, False)] outcomes
    snapshot <- snapshotMetrics store
    let counters = familyCounters snapshot TagStatement
    assertEqual "three starts" 3 (physicalStarts counters)
    assertEqual "one completion" 1 (physicalCompletions counters)
    assertEqual "one accept retired" 1 (retiredAccept counters)
    assertEqual "two canceled" 2 (canceled counters)
    assertEqual "no finished losers" 0 (completedUnselected counters)
    assertBool
        "canceled wall within generous scale bound"
        (canceledInteractionWallMicros counters < 10000000)

testCacheHits :: IO ()
testCacheHits = do
    store <- testStore 1
    pool <- testPoolWith (Just store) 1 True (const (pure acceptDetails)) (\_ _ _ _ -> pure ())
    first <- testOrderedTagged pool [(TagStatement, "x")]
    assertEqual "first executes" [(OracleAccepted, False)] first
    second <- testOrderedTagged pool [(TagStatement, "x")]
    assertEqual "second hits cache" [(OracleAccepted, True)] second
    snapshot <- snapshotMetrics store
    let counters = familyCounters snapshot TagStatement
    assertEqual "two retired accepts" 2 (retiredAccept counters)
    assertEqual "one physical start" 1 (physicalStarts counters)
    assertEqual "one physical completion" 1 (physicalCompletions counters)

testInconclusiveFamily :: IO ()
testInconclusiveFamily = do
    store <- testStore 1
    pool <- testPoolWith (Just store) 1 False (const (pure timeoutDetails)) (\_ _ _ _ -> pure ())
    result <-
        try (testOrderedTagged pool [(TagBundle "remove-column-bundle", "candidate")]) ::
            IO (Either OracleInconclusiveError [(OracleOutcome, Bool)])
    case result of
        Left OracleInconclusiveError -> pure ()
        Right _ -> fail "expected OracleInconclusiveError"
    snapshot <- snapshotMetrics store
    let counters = familyCounters snapshot (TagBundle "remove-column-bundle")
    assertEqual "inconclusive retired for the family" 1 (retiredInconclusive counters)
    assertEqual "physical start recorded" 1 (physicalStarts counters)
    assertEqual "physical completion recorded" 1 (physicalCompletions counters)
    assertEqual "no accept recorded" 0 (retiredAccept counters)
    assertEqual "no reject recorded" 0 (retiredReject counters)
    assertEqual "no losers" 0 (completedUnselected counters)
    assertEqual "nothing canceled" 0 (canceled counters)

-- | Drain barrier: waits until the observed cancel count lands.
awaitCanceled :: MetricsStore -> FamilyTag -> Int -> IO ()
awaitCanceled store tag want = go (10000 :: Int)
    where
        go 0 = assertFailure "timed out waiting for cancellation"
        go n = do
            snapshot <- snapshotMetrics store
            if canceled (familyCounters snapshot tag) >= want
                then pure ()
                else threadDelay 100 >> go (n - 1)

testCancelAfterHeadRetires :: IO ()
testCancelAfterHeadRetires = do
    store <- testStore 2
    startedNext <- newEmptyMVar
    headRetired <- newEmptyMVar
    never <- newEmptyMVar
    let execute candidate =
            if candidate == "head"
                then pure rejectDetails
                else putMVar startedNext () >> readMVar never >> pure rejectDetails
        retire index _ _ _ = when (index == 0) (putMVar headRetired ())
    pool <- testPoolWith (Just store) 2 False execute retire
    searchTid <- forkIO $ do
        _ <- testOrderedTagged pool [(TagStatement, "head"), (TagStatement, "next")]
        pure ()
    -- Cancel only after the head is retired (recorded plus prefix committed,
    -- callback fired after both) and the next task has started.
    readMVar headRetired
    readMVar startedNext
    killThread searchTid
    -- Drain: the canceled loser must settle before asserting.
    awaitCanceled store TagStatement 1
    snapshot <- snapshotMetrics store
    let counters = familyCounters snapshot TagStatement
    assertEqual "head retired reject" 1 (retiredReject counters)
    assertEqual "head not double-counted as loser" 0 (completedUnselected counters)
    assertEqual "next canceled" 1 (canceled counters)
    assertEqual "two starts" 2 (physicalStarts counters)
    live <- poolLiveWorkers pool
    assertEqual "workers drained" 0 live

testFailedWork :: IO ()
testFailedWork = do
    store <- testStore 1
    pool <- testPoolWith (Just store) 1 False (\_ -> throwIO (userError "boom")) (\_ _ _ _ -> pure ())
    result <-
        try (testOrderedTagged pool [(TagStatement, "boom")]) ::
            IO (Either SomeException [(OracleOutcome, Bool)])
    case result of
        Left err -> assertBool ("loud: " ++ displayException err) ("boom" `isInfixOf` displayException err)
        Right _ -> fail "worker exception became a false outcome"
    snapshot <- snapshotMetrics store
    let counters = familyCounters snapshot TagStatement
    assertEqual "failed recorded" 1 (failed counters)
    assertEqual "nothing canceled" 0 (canceled counters)
    assertEqual "one start" 1 (physicalStarts counters)
    assertEqual "no completion" 0 (physicalCompletions counters)
    assertBool
        "failed wall within generous scale bound"
        (failedInteractionWallMicros counters < 10000000)
