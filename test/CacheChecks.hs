module CacheChecks (tests) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (try)
import Control.Monad (forM_)
import Data.ByteString.Char8 qualified as BSC
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text qualified as T
import Occam.Cache (
    CacheAdmission (..),
    CacheMode (..),
    CacheStats (..),
    LookupResult (..),
    cacheCompleteInflight,
    cacheInstallInflight,
    cacheLookup,
    cacheRemoveInflight,
    cacheStats,
    newCache,
    observationAdmission,
 )
import Occam.Core (OracleInconclusiveError, OracleOutcome (..), oracleAccepts)
import Occam.Oracle (OracleObservation (..))
import Occam.Parser (SQLDialect (..), nativeOracleCacheKey)
import Occam.Pool (observationDetails, poolCacheStats, poolOracle, testOrdered)
import System.Exit (ExitCode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import TestSupport (boolObservation, testCountingPool)

tests :: TestTree
tests =
    testGroup
        "Phase 6: Candidate Memoization"
        [ testCase "ordered pool caches repeated queries and tracks hits/misses" $ do
            callCountRef <- newIORef (0 :: Int)
            let mockOracle sql = do
                    modifyIORef' callCountRef (+ 1)
                    pure (observationDetails (boolObservation (BSC.isInfixOf "CRASH" sql)))
            pool <- testCountingPool 1 mockOracle
            let oracle = poolOracle pool

            -- 1. Initial queries (all misses)
            r1 <- oracleAccepts oracle "SELECT * FROM t1 WHERE CRASH;"
            assertEqual "First query should return True" True r1
            r2 <- oracleAccepts oracle "SELECT 1;"
            assertEqual "Second query should return False" False r2

            calls1 <- readIORef callCountRef
            assertEqual "Underlying oracle should be called 2 times" 2 calls1

            stats1 <- poolCacheStats pool
            assertEqual "cacheHits should be 0" 0 (cacheHits stats1)
            assertEqual "cacheMisses should be 2" 2 (cacheMisses stats1)

            -- 2. Repeated queries (all hits)
            r3 <- oracleAccepts oracle "SELECT * FROM t1 WHERE CRASH;"
            assertEqual "Cached query 1 should return True" True r3
            r4 <- oracleAccepts oracle "SELECT 1;"
            assertEqual "Cached query 2 should return False" False r4
            r5 <- oracleAccepts oracle "SELECT * FROM t1 WHERE CRASH;"
            assertEqual "Cached query 1 (second time) should return True" True r5

            calls2 <- readIORef callCountRef
            assertEqual "Underlying oracle should NOT be re-invoked on cache hits" 2 calls2

            stats2 <- poolCacheStats pool
            assertEqual "cacheHits should be 3" 3 (cacheHits stats2)
            assertEqual "cacheMisses should remain 2" 2 (cacheMisses stats2)

            -- 3. Another new query (miss)
            r6 <- oracleAccepts oracle "SELECT 2;"
            assertEqual "New query should return False" False r6

            calls3 <- readIORef callCountRef
            assertEqual "Underlying oracle should be called for new query" 3 calls3

            stats3 <- poolCacheStats pool
            assertEqual "cacheHits should be 3" 3 (cacheHits stats3)
            assertEqual "cacheMisses should be 3" 3 (cacheMisses stats3)
        , testCase "inconclusive outcomes are not cached as authoritative" $ do
            callCountRef <- newIORef (0 :: Int)
            let outcomeOracle _ = do
                    modifyIORef' callCountRef (+ 1)
                    pure (observationDetails OracleTimedOut)
            pool <- testCountingPool 1 outcomeOracle
            let oracle = poolOracle pool
                expectInconclusive label = do
                    result <- try (oracle "SELECT 1;") :: IO (Either OracleInconclusiveError OracleOutcome)
                    case result of
                        Left _ -> pure ()
                        Right _ -> assertFailure ("inconclusive outcome was returned as definite: " ++ label)
            expectInconclusive "first"
            expectInconclusive "second"
            calls <- readIORef callCountRef
            assertEqual "inconclusive result is re-observed" 2 calls
            stats <- poolCacheStats pool
            assertEqual "inconclusive results are misses" 2 (cacheMisses stats)
            assertEqual "inconclusive results are not hits" 0 (cacheHits stats)
        , testCase "concurrent duplicate candidates single-flight one physical execution" $ do
            callCountRef <- newIORef (0 :: Int)
            let mockOracle _ = do
                    modifyIORef' callCountRef (+ 1)
                    pure (observationDetails (OracleFinished (ExitFailure 1)))
            pool <- testCountingPool 4 mockOracle
            outcomes <- testOrdered pool (replicate 4 "SELECT 1;")
            assertEqual "every duplicate retires" 4 (length outcomes)
            assertBool "shared verdict rejects" (all ((== OracleRejected) . fst) outcomes)
            calls <- readIORef callCountRef
            assertEqual "duplicates share one physical execution" 1 calls
            stats <- poolCacheStats pool
            assertEqual "one miss" 1 (cacheMisses stats)
            -- Duplicates join in-flight owner or hit completed cache without re-executing.
            assertEqual
                "every duplicate shared without re-executing"
                3
                (cacheHits stats + cacheJoins stats)
        , testCase "native cache keys preserve literal tokens and source-dependent SQL" $ do
            forM_ [SQLite, DuckDB] $ \dialect -> do
                let key = nativeOracleCacheKey dialect
                a <- key "SELECT 'a b' AS note, - 23 AS score;"
                assertBool "an explicitly named literal SELECT is canonicalizable" (isJust a)
                assertEqual "only ordinary inter-token whitespace is removed" a
                    =<< key "\nSELECT\t'a b' AS note,-23 AS score ; "
                b <- key "SELECT 'ab' AS note, -23 AS score;"
                assertBool "literal bytes remain distinct" (isJust b && a /= b)
                assertEqual "comments require exact bytes" Nothing =<< key "SELECT /* a */ 2 AS note;"
                assertEqual "derived column names require exact bytes" Nothing =<< key "SELECT 1 + 2;"
                assertEqual "source-reading functions require exact bytes" Nothing
                    =<< key "SELECT current_query() AS note;"
                withSemicolon <- key "SELECT 'a;''b' AS note ;"
                assertBool "quoted semicolon remains eligible" (isJust withSemicolon)
                assertEqual "quoted semicolon keeps exact token bytes" withSemicolon
                    =<< key "SELECT\t'a;''b' AS note;"
                assertEqual "multiple statements require exact bytes" Nothing
                    =<< key "SELECT 1 AS n; SELECT 2 AS n;"
        , testCase "direct cacheLookup and newCache behavior" $ do
            let precompKey = BSC.pack "SELECT 42;"
                precompObs = OracleFinished ExitSuccess
                precomputed = Map.singleton precompKey precompObs
            cache <- newCache precomputed

            -- Hit on precomputed with CacheEnabled records a hit
            r1 <- cacheLookup cache CacheEnabled precompKey
            assertEqual "precomputed hit enabled" (LookupHit precompObs) r1
            s1 <- cacheStats cache
            assertEqual "one hit" 1 (cacheHits s1)
            assertEqual "zero misses" 0 (cacheMisses s1)

            -- Hit on precomputed with CacheDisabled does NOT record a hit
            r2 <- cacheLookup cache CacheDisabled precompKey
            assertEqual "precomputed hit disabled" (LookupHit precompObs) r2
            s2 <- cacheStats cache
            assertEqual "hits unchanged" 1 (cacheHits s2)

            -- Fresh query with CacheDisabled records a miss
            let freshKey = BSC.pack "SELECT 99;"
            r3 <- cacheLookup cache CacheDisabled freshKey
            assertEqual "fresh query disabled" LookupMiss r3
            s3 <- cacheStats cache
            assertEqual "one miss" 1 (cacheMisses s3)

            -- Fresh query with CacheEnabled records a miss
            r4 <- cacheLookup cache CacheEnabled freshKey
            assertEqual "fresh query enabled" LookupMiss r4
            s4 <- cacheStats cache
            assertEqual "two misses" 2 (cacheMisses s4)
        , testCase "direct in-flight lifecycle, join reservation, and completion" $ do
            cache <- newCache Map.empty
            let candidate = BSC.pack "SELECT candidate;"
                obs = OracleFinished ExitSuccess

            -- 1. Install candidate in-flight for worker ordinal 10
            cacheInstallInflight cache candidate 10
            r1 <- cacheLookup cache CacheEnabled candidate
            assertEqual "in-flight query returns join" (LookupJoin 10) r1
            s1 <- cacheStats cache
            assertEqual "joins incremented" 1 (cacheJoins s1)

            -- 2. Wrong ordinal cannot complete the in-flight entry
            cacheCompleteInflight cache candidate 99 (Just obs)
            r2 <- cacheLookup cache CacheEnabled candidate
            assertEqual "still in-flight after mismatched owner complete" (LookupJoin 10) r2
            s2 <- cacheStats cache
            assertEqual "joins incremented again" 2 (cacheJoins s2)

            -- 3. Correct ordinal completes the in-flight entry
            cacheCompleteInflight cache candidate 10 (Just obs)
            r3 <- cacheLookup cache CacheEnabled candidate
            assertEqual "completed entry returns hit" (LookupHit obs) r3
            s3 <- cacheStats cache
            assertEqual "hits incremented" 1 (cacheHits s3)

            -- 4. Complete on non-inflight candidate does nothing
            let nonInflight = BSC.pack "SELECT never_inflight;"
            cacheCompleteInflight cache nonInflight 10 (Just obs)
            r4 <- cacheLookup cache CacheEnabled nonInflight
            assertEqual "non-inflight complete leaves candidate as miss" LookupMiss r4
        , testCase "direct cacheRemoveInflight cancels and evicts in-flight entry" $ do
            cache <- newCache Map.empty
            let candidate = BSC.pack "SELECT to_cancel;"
            cacheInstallInflight cache candidate 5
            r1 <- cacheLookup cache CacheEnabled candidate
            assertEqual "installed" (LookupJoin 5) r1
            cacheRemoveInflight cache candidate 5
            r2 <- cacheLookup cache CacheEnabled candidate
            assertEqual "evicted after cancellation" LookupMiss r2
        , testCase "observationAdmission filters valid results" $ do
            assertEqual "success is admitted" CacheAdmit (observationAdmission (OracleFinished ExitSuccess))
            assertEqual
                "exit failure is admitted"
                CacheAdmit
                (observationAdmission (OracleFinished (ExitFailure 1)))
            assertEqual "timeout is rejected" CacheReject (observationAdmission OracleTimedOut)
            assertEqual "reset failure is rejected" CacheReject (observationAdmission OracleResetFailed)
            assertEqual
                "io failure is rejected"
                CacheReject
                (observationAdmission (OracleIOFailed (T.pack "fail")))
        , testCase "concurrent HAMT cache inserts and lookups" $ do
            cache <- newCache Map.empty
            let workerCount = 8
                itemsPerWorker = 30
                obs = OracleFinished ExitSuccess
            doneVars <- sequence [newEmptyMVar | _ <- [1 .. workerCount]]
            forM_ (zip [0 .. workerCount - 1] doneVars) $ \(wId, doneVar) ->
                forkIO $ do
                    forM_ [0 .. itemsPerWorker - 1] $ \i -> do
                        let key = BSC.pack ("SELECT " ++ show wId ++ ", " ++ show i ++ ";")
                        cacheInstallInflight cache key wId
                        cacheCompleteInflight cache key wId (Just obs)
                        res <- cacheLookup cache CacheEnabled key
                        assertEqual "concurrent lookup hit" (LookupHit obs) res
                    putMVar doneVar ()
            forM_ doneVars takeMVar
            stats <- cacheStats cache
            assertEqual "all completed lookups hit" (workerCount * itemsPerWorker) (cacheHits stats)
        ]
