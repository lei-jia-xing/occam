module PoolChecks (tests) where

import Control.Concurrent (
    MVar,
    forkIO,
    modifyMVar_,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
    threadDelay,
 )
import Control.Exception (
    AsyncException (..),
    SomeException,
    bracket_,
    displayException,
    fromException,
    throwIO,
    try,
 )
import Data.ByteString (ByteString)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Occam.Cache (CacheStats (..))
import Occam.Core (OracleInconclusiveError, OracleOutcome (..))
import Occam.Oracle (OracleObservation (..), OracleObservationDetails)
import Occam.Pool (
    ExecutionBudgetReached (..),
    OraclePool,
    PoolConfig (..),
    closeOraclePool,
    newOraclePool,
    observationDetails,
    poolAttempts,
    poolCacheStats,
    poolCancelled,
    poolLiveWorkers,
    testOrdered,
 )
import System.Exit (ExitCode (..))
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
    testGroup
        "Ordered pool scheduler"
        [ testCase "earliest acceptance commits despite a later fast acceptance" $ do
            retiredRef <- newIORef []
            billedRef <- newIORef (0 :: Int)
            let execute candidate
                    | candidate == "A-bytes" = pure rejectDetails
                    | otherwise = pure acceptDetails
                bill _ = atomicModifyIORef' billedRef (\n -> (n + 1, ()))
                retire index candidate outcome hit =
                    modifyIORef' retiredRef ((index, candidate, outcome, hit) :)
            pool <- testPool 4 Nothing execute bill retire
            -- Pre-complete C so its acceptance is known before B even launches.
            _ <- testOrdered pool ["C-bytes"]
            writeIORef retiredRef []
            outcomes <- testOrdered pool ["A-bytes", "B-bytes", "C-bytes"]
            assertEqual
                "retirement must stop at the earliest acceptance"
                [OracleRejected, OracleAccepted]
                (map fst outcomes)
            retired <- reverse <$> readIORef retiredRef
            let accepts = [candidate | (_, candidate, outcome, _) <- retired, outcome == OracleAccepted]
            assertEqual "the committed acceptance is B, not the early-known C" ["B-bytes"] accepts
            stats <- poolCacheStats pool
            assertEqual "late duplicate served from cache" 1 (cacheHits stats)
            attempts <- poolAttempts pool
            assertEqual "physical starts across both windows" 3 attempts
        , testCase "racing later acceptance cannot overtake a gated earlier acceptance" $ do
            retiredRef <- newIORef []
            billedRef <- newIORef (0 :: Int)
            startedB <- newEmptyMVar
            gateB <- newEmptyMVar
            let execute candidate
                    | candidate == "A-bytes" = pure rejectDetails
                    | candidate == "B-bytes" = do
                        putMVar startedB ()
                        takeMVar gateB
                        pure acceptDetails
                    | otherwise = pure acceptDetails
                bill _ = atomicModifyIORef' billedRef (\n -> (n + 1, ()))
                retire index candidate outcome hit =
                    modifyIORef' retiredRef ((index, candidate, outcome, hit) :)
            pool <- testPool 4 Nothing execute bill retire
            resultVar <- newEmptyMVar
            _ <- forkIO (testOrdered pool ["A-bytes", "B-bytes", "C-bytes"] >>= putMVar resultVar)
            takeMVar startedB
            awaitBilled billedRef 2
            threadDelay 50000
            putMVar gateB ()
            outcomes <- takeMVar resultVar
            assertEqual "ordered prefix" [OracleRejected, OracleAccepted] (map fst outcomes)
            retired <- reverse <$> readIORef retiredRef
            let accepts = [candidate | (_, candidate, outcome, _) <- retired, outcome == OracleAccepted]
            assertEqual "the committed acceptance is B" ["B-bytes"] accepts
            attempts <- poolAttempts pool
            assertEqual "every window task started physically" 3 attempts
            billed <- readIORef billedRef
            assertEqual "completed unselected work stays billed" 3 billed
        , testCase "earlier inconclusive blocks a later acceptance" $ do
            retiredRef <- newIORef []
            let execute candidate
                    | candidate == "T-bytes" = pure (observationDetails OracleTimedOut)
                    | candidate == "C-bytes" = pure acceptDetails
                    | otherwise = pure rejectDetails
                retire index candidate outcome hit =
                    modifyIORef' retiredRef ((index, candidate, outcome, hit) :)
            pool <- testPool 4 Nothing execute (const (pure ())) retire
            result <-
                try (testOrdered pool ["A-bytes", "T-bytes", "C-bytes"]) ::
                    IO (Either OracleInconclusiveError [(OracleOutcome, Bool)])
            case result of
                Left _ -> pure ()
                Right _ -> assertFailure "inconclusive ordinal was skipped for a later acceptance"
            retired <- reverse <$> readIORef retiredRef
            assertBool
                "later acceptance committed despite earlier inconclusive"
                (all (\(_, _, outcome, _) -> outcome /= OracleAccepted) retired)
            attempts <- poolAttempts pool
            -- A and T must start; speculative candidate C may be cancelled before starting.
            assertBool ("too few physical starts: " ++ show attempts) (attempts >= 2)
            assertBool ("too many physical starts: " ++ show attempts) (attempts <= 3)
        , testCase "ordered reservation gives the head the sole permit" $ do
            let execute _ = pure acceptDetails
            pool <- testPool 4 (Just 1) execute (const (pure ())) (\_ _ _ _ -> pure ())
            outcomes <- testOrdered pool ["H-bytes", "X1-bytes", "X2-bytes", "X3-bytes"]
            assertEqual "head acceptance commits despite denied lookahead" [OracleAccepted] (map fst outcomes)
            attempts <- poolAttempts pool
            assertEqual "only the head started physically" 1 attempts
            denied <-
                try (testOrdered pool ["Y-bytes"]) ::
                    IO (Either ExecutionBudgetReached [(OracleOutcome, Bool)])
            case denied of
                Left _ -> pure ()
                Right _ -> assertFailure "exhausted budget admitted another physical start"
        , testCase "at most jobs physical executions run concurrently" $ do
            activeVar <- newMVar (0 :: Int, 0 :: Int)
            gate <- newEmptyMVar
            let execute _ =
                    bracket_
                        (modifyMVar_ activeVar (\(current, peak) -> let next = current + 1 in pure (next, max peak next)))
                        (modifyMVar_ activeVar (\(current, peak) -> pure (current - 1, peak)))
                        (readMVar gate >> pure rejectDetails)
            pool <- testPool 3 Nothing execute (const (pure ())) (\_ _ _ _ -> pure ())
            resultVar <- newEmptyMVar
            _ <- forkIO (testOrdered pool ["S1", "S2", "S3", "S4", "S5", "S6"] >>= putMVar resultVar)
            awaitActive activeVar 3
            (_, peak) <- readMVar activeVar
            assertEqual "all three slots ran concurrently" 3 peak
            putMVar gate ()
            outcomes <- takeMVar resultVar
            assertEqual "every candidate retired" 6 (length outcomes)
            assertBool "all reject" (all ((== OracleRejected) . fst) outcomes)
            attempts <- poolAttempts pool
            assertEqual "every candidate started physically" 6 attempts
            (_, finalPeak) <- readMVar activeVar
            assertBool "concurrency exceeded the job bound" (finalPeak <= 3)
        , testCase "worker exceptions surface loudly instead of hanging" $ do
            billedRef <- newIORef (0 :: Int)
            let execute candidate
                    | candidate == "boom" = throwIO (userError "boom-observation")
                    | otherwise = pure rejectDetails
                bill _ = atomicModifyIORef' billedRef (\n -> (n + 1, ()))
            pool <- testPool 2 Nothing execute bill (\_ _ _ _ -> pure ())
            result <-
                try (testOrdered pool ["ok", "boom"]) ::
                    IO (Either SomeException [(OracleOutcome, Bool)])
            case result of
                Left err ->
                    assertBool
                        ("worker error was altered: " ++ displayException err)
                        ("boom-observation" `isInfixOf` displayException err)
                Right _ -> assertFailure "worker exception became a false outcome"
            attempts <- poolAttempts pool
            assertEqual "both tasks started physically" 2 attempts
            billed <- readIORef billedRef
            assertEqual "only completed work is billed" 1 billed
            cancelled <- poolCancelled pool
            assertEqual "worker errors are failed work, not cancellations" 0 cancelled
        , testCase "early acceptance never forces a lazy infinite tail" $ do
            pool <- testPool 2 Nothing (const (pure acceptDetails)) (const (pure ())) (\_ _ _ _ -> pure ())
            let candidates = "accept-now" : repeat "never-forced"
            result <- timeout 10000000 (testOrdered pool candidates)
            case result of
                Nothing -> assertFailure "ordered window forced the unbounded tail"
                Just outcomes -> assertEqual "decision stops at the head" [OracleAccepted] (map fst outcomes)
        , testCase "concurrent windows serialize on the shared gate" $ do
            let execute _ = pure rejectDetails
            pool <- testPool 2 Nothing execute (const (pure ())) (\_ _ _ _ -> pure ())
            firstVar <- newEmptyMVar
            secondVar <- newEmptyMVar
            _ <- forkIO (testOrdered pool ["A1", "DUP"] >>= putMVar firstVar)
            _ <- forkIO (testOrdered pool ["B1", "DUP"] >>= putMVar secondVar)
            first <- takeMVar firstVar
            second <- takeMVar secondVar
            assertEqual "first window retired fully" 2 (length first)
            assertEqual "second window retired fully" 2 (length second)
            assertBool "windows misreported" (all ((== OracleRejected) . fst) (first ++ second))
            attempts <- poolAttempts pool
            assertEqual "shared duplicate executed once" 3 attempts
        , testCase "close cancels and joins owned workers" $ do
            started <- newEmptyMVar
            neverGate <- newEmptyMVar :: IO (MVar ())
            let execute _ = do
                    putMVar started ()
                    takeMVar neverGate
                    pure rejectDetails
            pool <- testPool 1 Nothing execute (const (pure ())) (\_ _ _ _ -> pure ())
            resultVar <- newEmptyMVar
            _ <- forkIO (try (testOrdered pool ["slow"]) >>= putMVar resultVar)
            takeMVar started
            closeOraclePool pool
            live <- poolLiveWorkers pool
            assertEqual "worker outlived close" 0 live
            outcome <- takeMVar resultVar
            case outcome of
                Left err -> case fromException err of
                    Just ThreadKilled -> pure ()
                    _ -> assertFailure ("close delivered the wrong exception: " ++ displayException err)
                Right _ -> assertFailure "killed window reported false outcomes"
            cancelled <- poolCancelled pool
            assertEqual "killed task was not accounted as cancelled" 1 cancelled
            attempts <- poolAttempts pool
            assertEqual "killed task started physically" 1 attempts
        ]

testPool ::
    Int ->
    Maybe Int ->
    (ByteString -> IO OracleObservationDetails) ->
    (OracleObservationDetails -> IO ()) ->
    (Int -> ByteString -> OracleOutcome -> Bool -> IO ()) ->
    IO OraclePool
testPool jobs permits execute bill retire =
    newOraclePool
        PoolConfig
            { poolJobs = jobs
            , poolCacheEnabled = True
            , poolPermits = permits
            , poolPrecomputed = Map.empty
            , poolMetrics = Nothing
            , poolExecute = execute
            , poolBill = bill
            , poolRetire = retire
            , poolReportCount = Nothing
            }

acceptDetails :: OracleObservationDetails
acceptDetails = observationDetails (OracleFinished ExitSuccess)

rejectDetails :: OracleObservationDetails
rejectDetails = observationDetails (OracleFinished (ExitFailure 1))

awaitBilled :: IORef Int -> Int -> IO ()
awaitBilled ref want = go (5000 :: Int)
    where
        go 0 = assertFailure "timed out waiting for billed observations"
        go n = do
            billed <- readIORef ref
            if billed >= want then pure () else threadDelay 1000 >> go (n - 1)

awaitActive :: MVar (Int, Int) -> Int -> IO ()
awaitActive activeVar want = go (5000 :: Int)
    where
        go 0 = assertFailure "timed out waiting for concurrent workers"
        go n = do
            (current, _) <- readMVar activeVar
            if current >= want then pure () else threadDelay 1000 >> go (n - 1)
