module ReductionChecks.StatementSearchChecks (checks) where

import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.IntSet qualified as IntSet
import Data.List (elemIndex, tails)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Occam.Core (
    BarrierScope (GlobalScope),
    OracleOutcome (..),
    ResultStatement (..),
    ScopedBarrier (barrierScope),
    SemanticFacts (factBarriers),
    StatementId (..),
 )
import Occam.Dependency (DependencyGraph (..), analyzeStatements)
import Occam.Facts (StatementUnit (..))
import Occam.Logger (Logger (..), defaultLogConfig, nullLogger)
import Occam.Oracle (OracleObservationDetails (..), observationOutcome)
import Occam.Oracle.Inline (newResultTraceTracker, prepareResultOracleWithTrace)
import Occam.Oracle.Sqlite (generateSqliteContract, sqliteContractOracle)
import Occam.Parser (ParsedSQL (..), SQLDialect (..), defaultParseOptions, parseSQL)
import Occam.Proposals (
    DeletionProposalGroup (..),
    GroupProposalPlan (..),
    extractDeletionProposalPlan,
    isNoncontiguousIn,
 )
import Occam.Reducer (
    LaneScheduling (..),
    ProjectionWindow (..),
    ProjectionWindowOrder (..),
    RankedScheduleConfig (..),
    ReductionConfig (..),
    ReductionResult (..),
    ReductionStage (..),
    SingletonResume (..),
    StageEvent (..),
    StatementSearchPolicy (..),
    closureDdmin,
    defaultRankedScheduleConfig,
    defaultReductionConfigFor,
    reduceSQLWithConfig,
    reduceSQLWithEvidence,
    reduceSQLWithResultSlicing,
    reduceSQLWithResultTrace,
    shouldActivateRankedFallback,
    statementDdminWithOrder,
    statementRanked,
 )
import ReductionChecks.Support (outcomePool, predicatePool)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

checks :: [TestTree]
checks =
    [ testCase "Same-named unused table is not a false dependency" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE needed (id INT);"
                    , "CREATE TABLE unrelated (id INT);"
                    , "SELECT needed.id FROM needed;"
                    ]
        result <- parseSQL (defaultParseOptions SQLite) sql
        case result of
            Left err -> assertFailure ("SQL parser transport error: " ++ show err)
            Right parsed -> do
                let graph = analyzeStatements (parsedInitialSession parsed) (parsedUnits parsed)
                    dependencies = Map.findWithDefault Set.empty (StatementId 3) (graphEdges graph)
                assertBool "query should depend on its referenced table" (Set.member (StatementId 1) dependencies)
                assertBool
                    "query must not depend on an unrelated table with the same column name"
                    (Set.notMember (StatementId 2) dependencies)
    , testCase "Configured SQL dialect changes dependency closure" $ do
        pool <- predicatePool (BSC.isInfixOf "SELECT id")
        let sql = BSC.pack "CREATE TABLE \"MiXeD\" (id INT);\nSELECT id FROM \"mixed\";\n"
            config dialect =
                (defaultReductionConfigFor dialect)
                    { reductionSlicing = False
                    , reductionClosure = True
                    , reductionIntraStatement = False
                    , reductionUnrestricted = False
                    }
        postgres <- reduceSQLWithConfig (config PostgreSQL) nullLogger pool sql
        sqlite <- reduceSQLWithConfig (config SQLite) nullLogger pool sql
        assertEqual
            "PostgreSQL quoted identifiers retain case"
            (BSC.pack "SELECT id FROM \"mixed\";\n")
            postgres
        assertEqual "SQLite folds quoted identifiers for closure" sql sqlite
    , testCase "An unchanged globally fenced slice tries unrestricted deletion before closure" $ do
        let sql = "CREATE TABLE noise (id INT); SELECT sum(1);"
            config = defaultReductionConfigFor SQLite
        pool <- predicatePool (BSC.isInfixOf "SELECT sum(1)")
        stages <- newIORef []
        let report (StageStarted stage) = modifyIORef' stages (stage :)
            report StageFinished {} = pure ()
        _ <- reduceSQLWithEvidence report config nullLogger pool sql
        started <- reverse <$> readIORef stages
        assertEqual
            "ordinary deletion should get the first search window after slicing"
            [StageSlicing, StageUnrestrictedFallback, StageClosureSearch]
            (take 3 started)
    , testCase "Result slicing tries the observed statement before other candidates" $ do
        let sql = "CREATE TABLE noise (x INT);SELECT 42 AS answer;CREATE TABLE tail (x INT);"
            expected = "SELECT 42 AS answer;"
            config = defaultReductionConfigFor SQLite
            locate bytes = pure (if bytes == sql then Just (ResultStatement 3 1) else Nothing)
        candidates <- newIORef []
        pool <- outcomePool $ \candidate -> do
            modifyIORef' candidates (candidate :)
            pure (if expected `BSC.isInfixOf` candidate then OracleAccepted else OracleRejected)
        _ <- reduceSQLWithResultSlicing locate (const (pure ())) config nullLogger pool sql
        attempted <- reverse <$> readIORef candidates
        assertEqual
            "the first candidate is the backward slice of the result statement"
            [sql, expected]
            (take 2 attempted)
    , testCase "A relaxed global slice requires oracle acceptance" $ do
        let sql = "CREATE TABLE noise (x INT);SELECT sum(1);"
            proposal = "SELECT sum(1);"
            config = defaultReductionConfigFor SQLite
            locate bytes = pure (if bytes == sql then Just (ResultStatement 2 1) else Nothing)
            run acceptsProposal = do
                candidates <- newIORef []
                pool <- outcomePool $ \candidate -> do
                    modifyIORef' candidates (candidate :)
                    pure $
                        if candidate == sql || (acceptsProposal && candidate == proposal)
                            then OracleAccepted
                            else OracleRejected
                ReductionResult result _ <-
                    reduceSQLWithResultSlicing locate (const (pure ())) config nullLogger pool sql
                attempted <- reverse <$> readIORef candidates
                assertEqual "relaxed slice is the first proposal" [sql, proposal] (take 2 attempted)
                pure result
        assertEqual "accepted proposal is used" proposal =<< run True
        assertEqual "rejected proposal is discarded" sql =<< run False
    , testCase "Rejected relaxed slice retries with opaque roots" $ do
        let primary = "SELECT sum(1);"
            retry = "SELECT sum(2);" <> primary
            sql = "CREATE TABLE noise (x INT);" <> retry
            config = defaultReductionConfigFor SQLite
            locate bytes = pure (if bytes == sql then Just (ResultStatement 3 2) else Nothing)
        attempts <- newIORef []
        pool <- outcomePool $ \candidate -> do
            modifyIORef' attempts (candidate :)
            pure (if candidate == sql || candidate == retry then OracleAccepted else OracleRejected)
        ReductionResult reduced _ <-
            reduceSQLWithResultSlicing locate (const (pure ())) config nullLogger pool sql
        tested <- reverse <$> readIORef attempts
        assertEqual "the retry retains the opaque producer" retry reduced
        assertEqual "both proposals are independently checked" [sql, primary, retry] (take 3 tested)
    , testCase "Prefix slice cannot displace a smaller accepted seed" $ do
        let create = "CREATE TABLE t(x INT);"
            noise = "CREATE TABLE noise(x INT);"
            earlier = "SELECT x FROM t;"
            seed = "SELECT 1000;"
            prefix = "SELECT 0;" <> create <> noise <> earlier
            sql = prefix <> seed
            config = defaultReductionConfigFor SQLite
            locate bytes = pure (if bytes == sql then Just (ResultStatement 5 4) else Nothing)
        attempts <- newIORef []
        pool <- outcomePool $ \candidate -> do
            modifyIORef' attempts (candidate :)
            pure
                (if candidate `elem` [sql, seed, prefix, create <> earlier] then OracleAccepted else OracleRejected)
        ReductionResult result _ <-
            reduceSQLWithResultSlicing locate (const (pure ())) config nullLogger pool sql
        tested <- readIORef attempts
        assertEqual "the smaller accepted seed survives" seed result
        assertBool "the prefix is observed" (prefix `elem` tested)
        assertBool
            "the larger slice is skipped even if it would accept"
            ((create <> earlier) `notElem` tested)
    , testCase "Accepted seed can dominate the entire prefix search" $ do
        let earlier = "CREATE TABLE noise(x INT);"
            seed = "SELECT 1;"
            sql = earlier <> seed
            config = defaultReductionConfigFor SQLite
            locate bytes = pure (if bytes == sql then Just (ResultStatement 2 1) else Nothing)
        attempts <- newIORef []
        pool <- outcomePool $ \candidate -> do
            modifyIORef' attempts (candidate :)
            pure (if candidate `elem` [sql, seed, earlier] then OracleAccepted else OracleRejected)
        ReductionResult result _ <-
            reduceSQLWithResultSlicing locate (const (pure ())) config nullLogger pool sql
        tested <- readIORef attempts
        assertEqual "the accepted seed survives" seed result
        assertBool "the prefix cannot improve it" (earlier `notElem` tested)
    , testCase "Unaligned result statement metadata follows the last-statement search" $ do
        let sql = "CREATE TABLE noise (x INT);SELECT 42 AS answer;CREATE TABLE tail (x INT);"
            expected = "SELECT 42 AS answer;"
            config = defaultReductionConfigFor SQLite
            run location = do
                candidates <- newIORef []
                pool <- outcomePool $ \candidate -> do
                    modifyIORef' candidates (candidate :)
                    pure (if expected `BSC.isInfixOf` candidate then OracleAccepted else OracleRejected)
                let locate bytes = pure (if bytes == sql then location else Nothing)
                ReductionResult reduced _ <-
                    reduceSQLWithResultSlicing locate (const (pure ())) config nullLogger pool sql
                trace <- reverse <$> readIORef candidates
                pure (reduced, trace)
        baseline@(reduced, trace) <- run Nothing
        assertEqual "the expected query survives" expected reduced
        assertEqual "the last statement is tested first" [sql, "CREATE TABLE tail (x INT);"] (take 2 trace)
        wrongCount <- run (Just (ResultStatement 4 1))
        assertEqual "an in-range ordinal with a mismatched count is ignored" baseline wrongCount
        wrongOrdinal <- run (Just (ResultStatement 3 3))
        assertEqual "an out-of-range ordinal is ignored" baseline wrongOrdinal
        negativeOrdinal <- run (Just (ResultStatement 3 (-1)))
        assertEqual "a negative ordinal is ignored" baseline negativeOrdinal
    , testCase "A global barrier discovered during graph construction prioritizes unrestricted search" $ do
        let sql =
                BSC.unlines
                    [ "CREATE SCHEMA a;"
                    , "CREATE SCHEMA b;"
                    , "CREATE TABLE a.t (x INT);"
                    , "CREATE TABLE b.t (x INT);"
                    , "DROP TABLE t;"
                    , "SELECT x FROM b.t;"
                    ]
            config = (defaultReductionConfigFor PostgreSQL) {reductionSlicing = False}
        parsed <- parseSQL (defaultParseOptions PostgreSQL) sql
        case parsed of
            Left err -> assertFailure (show err)
            Right result -> do
                let graph = analyzeStatements (parsedInitialSession result) (parsedUnits result)
                assertBool
                    "the parser emitted no global barrier"
                    (all (all ((/= GlobalScope) . barrierScope) . factBarriers) (graphFacts graph))
                assertBool "graph construction discovered a global barrier" (graphHasGlobalBarrier graph)
        pool <- predicatePool (BSC.isInfixOf "DROP TABLE t;")
        stages <- newIORef []
        let report (StageStarted stage) = modifyIORef' stages (stage :)
            report StageFinished {} = pure ()
        _ <- reduceSQLWithEvidence report config nullLogger pool sql
        started <- reverse <$> readIORef stages
        assertEqual
            "unrestricted search precedes closure"
            [StageUnrestrictedFallback, StageClosureSearch]
            (take 2 started)
    , testCase "Closure search prunes unneeded independent subgraphs" $ do
        pool <-
            predicatePool (\candidate -> BSC.isInfixOf "crash_tbl" candidate && BSC.isInfixOf "42" candidate)
        let sql =
                BSC.unlines
                    [ "CREATE TABLE unneeded_tbl (id INT);"
                    , "INSERT INTO unneeded_tbl VALUES (1);"
                    , "SELECT * FROM unneeded_tbl;"
                    , "CREATE TABLE crash_tbl (id INT);"
                    , "INSERT INTO crash_tbl VALUES (42);"
                    , "SELECT * FROM crash_tbl WHERE id = 42;"
                    ]
            config =
                (defaultReductionConfigFor PostgreSQL)
                    { reductionIntraStatement = False
                    , reductionUnrestricted = False
                    }
        reduced <- reduceSQLWithConfig config nullLogger pool sql
        assertBool "keeps crash table" (BSC.isInfixOf "crash_tbl" reduced)
        assertBool "keeps crash value" (BSC.isInfixOf "42" reduced)
        assertBool "eliminates unneeded subgraph" (not (BSC.isInfixOf "unneeded_tbl" reduced))
    , testCase "SQLite virtual table lowering enables reduction of unrelated tables" $ do
        pool <- predicatePool (BSC.isInfixOf "DROP TABLE vt1")
        let sql =
                BSC.unlines
                    [ "CREATE VIRTUAL TABLE vt0 USING fts5(c0);"
                    , "CREATE VIRTUAL TABLE vt1 USING fts4(c0);"
                    , "INSERT INTO vt1(c0) VALUES(0);"
                    , "BEGIN;"
                    , "UPDATE vt1 SET c0 = 0;"
                    , "INSERT INTO vt1(c0) VALUES (0), (0);"
                    , "UPDATE vt0 SET c0 = 0;"
                    , "INSERT INTO vt1(c0) VALUES (0);"
                    , "UPDATE vt1 SET c0 = 0;"
                    , "INSERT INTO vt1(vt1) VALUES('automerge=1');"
                    , "UPDATE vt1 SET c0 = 0;"
                    , "DROP TABLE vt1;"
                    ]
            config =
                (defaultReductionConfigFor SQLite)
                    { reductionIntraStatement = False
                    , reductionUnrestricted = False
                    }
        reduced <- reduceSQLWithConfig config nullLogger pool sql
        assertBool "keeps target vt1 drop" (BSC.isInfixOf "DROP TABLE vt1" reduced)
        assertBool "keeps vt1 definition" (BSC.isInfixOf "CREATE VIRTUAL TABLE vt1" reduced)
        assertBool "prunes unrelated vt0 virtual table completely" (not (BSC.isInfixOf "vt0" reduced))

        pool0 <- predicatePool (BSC.isInfixOf "UPDATE vt0")
        reduced0 <- reduceSQLWithConfig config nullLogger pool0 sql
        assertBool "keeps target vt0 update" (BSC.isInfixOf "UPDATE vt0" reduced0)
        assertBool "keeps vt0 definition" (BSC.isInfixOf "CREATE VIRTUAL TABLE vt0" reduced0)
        assertBool "prunes unrelated vt1 virtual table completely" (not (BSC.isInfixOf "vt1" reduced0))
    , testCase "Constraint stripping and deeper branch pruning simplify table definitions and expressions" $ do
        pool <-
            predicatePool
                (\candidate -> BSC.isInfixOf "42" candidate && BSC.isInfixOf "CREATE TABLE t" candidate)
        let sql =
                BSC.unlines
                    [ "CREATE TABLE t (id INT NOT NULL, name TEXT DEFAULT 'abc', PRIMARY KEY (id), FOREIGN KEY (id) REFERENCES p(id));"
                    , "SELECT CASE WHEN id = 1 THEN 42 ELSE 0 END, COALESCE(id, 0), id IS NOT NULL FROM t;"
                    ]
            config = (defaultReductionConfigFor SQLite) {reductionIntraStatement = True}
        reduced <- reduceSQLWithConfig config nullLogger pool sql
        assertBool "strips table constraint" (not (BSC.isInfixOf "PRIMARY KEY" reduced))
        assertBool "strips foreign key constraint" (not (BSC.isInfixOf "FOREIGN KEY" reduced))
        assertBool "strips column constraint" (not (BSC.isInfixOf "NOT NULL" reduced))
        assertBool "simplifies case to branch" (BSC.isInfixOf "42" reduced)
    , testCase "Rejected literal batch splits and finds a coupled reduction" $ do
        let original = "SELECT 1234, 1234, 9999, 9999;"
            allChanged = "SELECT 0, 0, 0, 0;"
            expected = "SELECT 0, 0, 9999, 9999;"
            cfg = (defaultReductionConfigFor SQLite) {reductionIntraStatement = True}
            accepted sql = sql == original || sql == expected
        control <- predicatePool accepted
        reducedControl <- reduceSQLWithConfig cfg nullLogger control original
        assertEqual "single edits cannot reach the accepted pair" original reducedControl
        candidates <- newIORef []
        treatment <- outcomePool $ \sql -> do
            modifyIORef' candidates (sql :)
            pure (if accepted sql then OracleAccepted else OracleRejected)
        reduced <- reduceSQLWithConfig (cfg {reductionLiteralBatches = True}) nullLogger treatment original
        assertEqual "batch split applies only the accepted pair" expected reduced
        attempted <- reverse <$> readIORef candidates
        let wholeIndex = elemIndex allChanged attempted
            halfIndex = elemIndex expected attempted
        assertBool
            "whole batch was tried before its accepted half"
            ( case (wholeIndex, halfIndex) of
                (Just i, Just j) -> i < j
                _ -> False
            )
    , testCase "Complement-first statement search still tries useful subsets" $ do
        let original = "SELECT 10;SELECT 20;SELECT 30;SELECT 40;SELECT 50;SELECT 60;SELECT 70;SELECT 80;"
            complement = "SELECT 30;SELECT 40;SELECT 50;SELECT 60;SELECT 70;SELECT 80;"
            expected = "SELECT 30;SELECT 40;"
            config =
                (defaultReductionConfigFor SQLite)
                    { reductionSlicing = False
                    , reductionClosure = False
                    , reductionStatementSearch = StatementComplementFirst
                    }
        attempts <- newIORef []
        pool <- outcomePool $ \sql -> do
            modifyIORef' attempts (sql :)
            pure (if sql == original || sql == expected then OracleAccepted else OracleRejected)
        reduced <- reduceSQLWithConfig config nullLogger pool original
        assertEqual "a subset is still selected" expected reduced
        tested <- reverse <$> readIORef attempts
        assertBool "the complement is tested before the successful subset" $
            case (elemIndex complement tested, elemIndex expected tested) of
                (Just complementOrdinal, Just subsetOrdinal) -> complementOrdinal < subsetOrdinal
                _ -> False
    , testCase "Closure search validates candidates from a nonclosed input set" $ do
        let prefix = "CREATE TABLE t(x INT);"
            dependent = "SELECT x FROM t;"
            independent = "SELECT 1;"
            sql = prefix <> dependent <> independent
        parsed <- parseSQL (defaultParseOptions SQLite) sql
        case parsed of
            Left err -> assertFailure ("SQL parser transport error: " ++ show err)
            Right input -> do
                let units = parsedUnits input
                    graph = analyzeStatements (parsedInitialSession input) units
                    supplied = map unitStatement (drop 1 units)
                attempts <- newIORef []
                pool <- outcomePool $ \candidate -> do
                    modifyIORef' attempts (candidate :)
                    pure OracleAccepted
                _ <- closureDdmin pool sql supplied graph
                tested <- readIORef attempts
                assertBool
                    "a candidate with no missing predecessors is tried"
                    ((prefix <> independent) `elem` tested)
                assertBool
                    "a candidate with a missing predecessor is skipped"
                    ((prefix <> dependent) `notElem` tested)
    , testCase "Byte-weighted singleton deletion resumes and wraps around" $ do
        let original = "SELECT 10;SELECT 20;SELECT 30;SELECT 40;SELECT 50;SELECT 60;SELECT 70;SELECT 80;"
            firstAccepted = "SELECT 10;SELECT 20;SELECT 30;SELECT 50;SELECT 60;SELECT 70;SELECT 80;"
            nextDeletion = "SELECT 10;SELECT 20;SELECT 30;SELECT 60;SELECT 70;SELECT 80;"
            afterWrap = "SELECT 20;SELECT 30;SELECT 50;SELECT 60;SELECT 70;SELECT 80;"
        attempts <- newIORef []
        pool <- outcomePool $ \sql -> do
            modifyIORef' attempts (sql :)
            pure (if sql `elem` [original, firstAccepted, afterWrap] then OracleAccepted else OracleRejected)
        reduced <- statementDdminWithOrder StatementByteWeighted SQLite pool original
        assertEqual "the earlier deletion is retried on the smaller script" afterWrap reduced
        tested <- reverse <$> readIORef attempts
        let afterFirst = drop 1 (dropWhile (/= firstAccepted) tested)
        assertEqual "search resumes after the removed statement" [nextDeletion] (take 1 afterFirst)
        assertBool "search wraps back to the first statement" (afterWrap `elem` afterFirst)
    , testCase "Ranked scheduling keeps a subset that complements and single deletions miss" $ do
        let original = "SELECT 10;SELECT 20;SELECT 30;SELECT 40;SELECT 50;SELECT 60;SELECT 70;SELECT 80;"
            suffix = "SELECT 70;SELECT 80;"
        pool <- predicatePool (\candidate -> candidate == original || candidate == suffix)
        reduced <- statementDdminWithOrder statementRanked SQLite pool original
        assertEqual "the ranked subset lane is still searched" suffix reduced
    , testCase "Bounded lane preference preserves subset lane opportunities" $ do
        let original = "SELECT 10;SELECT 20;SELECT 30;SELECT 40;SELECT 50;SELECT 60;SELECT 70;SELECT 80;"
            suffix = "SELECT 70;SELECT 80;"
            policy =
                StatementRankedWith
                    defaultRankedScheduleConfig
                        { rankedLaneScheduling = LaneBoundedPreference 4
                        , rankedSingletonResume = ResumeWeightMonotonic
                        }
        pool <- predicatePool (\candidate -> candidate == original || candidate == suffix)
        reduced <- statementDdminWithOrder policy SQLite pool original
        assertEqual "bounded lane preference still finds subset-only reductions" suffix reduced
    , testCase "Weight-monotonic singleton resumption avoids re-testing heavier required statements" $ do
        let req i = BSC.pack ("SELECT 'required-payload-statement-" ++ show i ++ "';")
            noise i = BSC.pack ("SELECT " ++ show i ++ ";")
            stmts = concat [[req i, noise i] | i <- [1 :: Int .. 6]]
            original = BSC.concat stmts
            required = [req i | i <- [1 :: Int .. 6]]
            expected = BSC.concat required
            accepts candidate = all (`BSC.isInfixOf` candidate) required
            runPolicy resumeMode = do
                calls <- newIORef (0 :: Int)
                pool <- outcomePool $ \candidate -> do
                    modifyIORef' calls (+ 1)
                    pure (if accepts candidate then OracleAccepted else OracleRejected)
                let policy =
                        StatementRankedWith
                            defaultRankedScheduleConfig
                                { rankedLaneScheduling = LaneBoundedPreference 4
                                , rankedSingletonResume = resumeMode
                                }
                reduced <- statementDdminWithOrder policy SQLite pool original
                count <- readIORef calls
                pure (reduced, count)
        (reducedStable, callsStable) <- runPolicy ResumeWeightStableTies
        (reducedMono, callsMono) <- runPolicy ResumeWeightMonotonic
        assertEqual "stable ties reaches expected SQL" expected reducedStable
        assertEqual "weight-monotonic reaches expected SQL" expected reducedMono
        assertBool
            ( "weight-monotonic resumption should use fewer probes on short noise (mono="
                ++ show callsMono
                ++ ", stable="
                ++ show callsStable
                ++ ")"
            )
            (callsMono < callsStable)
    , testCase "Within-lane projected window preserves complement-before-subset slot order" $ do
        let original =
                BSC.concat
                    [ "CREATE TABLE a(x INT);"
                    , "CREATE TABLE b(x INT);"
                    , "CREATE TABLE c(x INT);"
                    , "SELECT x FROM a;"
                    , "SELECT x FROM b;"
                    , "SELECT x FROM c;"
                    ]
            policy =
                StatementRankedWith
                    defaultRankedScheduleConfig
                        { rankedLaneScheduling = LaneBoundedPreference 4
                        , rankedProjectionWindow = ProjectionWindow 8 WindowOrderWithinLaneProjected
                        }
        parsed <- parseSQL (defaultParseOptions SQLite) original
        case parsed of
            Left err -> assertFailure ("SQL parser transport error: " ++ show err)
            Right input -> do
                let units = parsedUnits input
                testedRef <- newIORef []
                pool <- outcomePool $ \candidate -> do
                    modifyIORef' testedRef (candidate :)
                    pure (if candidate == original then OracleAccepted else OracleRejected)
                let config =
                        (defaultReductionConfigFor SQLite)
                            { reductionSlicing = False
                            , reductionUnrestricted = False
                            , reductionStatementSearch = policy
                            }
                _ <- reduceSQLWithConfig config nullLogger pool original
                tested <- drop 1 . reverse <$> readIORef testedRef
                assertEqual "expected six statements" 6 (length units)
                case tested of
                    [] -> assertFailure "closure search should probe at least one candidate"
                    firstProbe : _ ->
                        -- First probe should be a multi-statement complement, not a tiny 1-statement subset.
                        assertBool
                            ("expected complement before tiny subset, got: " ++ BSC.unpack firstProbe)
                            (BS.length firstProbe > BS.length "CREATE TABLE a(x INT);")
    , testCase "Ranked fallback activates only when slicing stalls with substantial work" $ do
        let cfg = defaultRankedScheduleConfig {rankedFallbackMinStatements = 8, rankedFallbackMaxShrinkPct = 10}
        assertBool
            "activates on strict stall with >= 8 statements"
            (shouldActivateRankedFallback cfg True 1000 1000 12)
        assertBool
            "activates on <= 10% shrink with >= 8 statements"
            (shouldActivateRankedFallback cfg True 1000 920 12)
        assertBool
            "skips when slicing removed > 10%"
            (not (shouldActivateRankedFallback cfg True 1000 500 12))
        assertBool
            "skips when remaining statements < threshold"
            (not (shouldActivateRankedFallback cfg True 1000 1000 4))
        assertBool
            "skips when slicing is disabled"
            (not (shouldActivateRankedFallback cfg False 1000 1000 20))
        let original =
                BSC.concat
                    [ "CREATE TABLE t(x INT);"
                    , "INSERT INTO t VALUES (1);"
                    , "INSERT INTO t VALUES (2);"
                    , "INSERT INTO t VALUES (3);"
                    , "INSERT INTO t VALUES (4);"
                    , "INSERT INTO t VALUES (5);"
                    , "INSERT INTO t VALUES (6);"
                    , "SELECT sum(x) FROM t;"
                    ]
            fallbackPolicy =
                StatementRankedFallback
                    defaultRankedScheduleConfig
                        { rankedLaneScheduling = LaneBoundedPreference 4
                        , rankedSingletonResume = ResumeWeightMonotonic
                        , rankedFallbackMinStatements = 8
                        , rankedFallbackMaxShrinkPct = 0
                        }
            config =
                (defaultReductionConfigFor SQLite)
                    { reductionStatementSearch = fallbackPolicy
                    }
        activations <- newIORef (0 :: Int)
        let captureLogger =
                Logger
                    defaultLogConfig
                    ( \_ _ msg _ ->
                        when (msg == "Ranked statement fallback activated") $
                            modifyIORef' activations (+ 1)
                    )
                    (pure ())
        pool <- predicatePool (== original)
        reduced <- reduceSQLWithConfig config captureLogger pool original
        assertEqual "irreducible script stays intact" original reduced
        actCount <- readIORef activations
        assertBool "ranked fallback activated after stalled slicing" (actCount >= 1)
    , testCase "Dependency group proposals extract noncontiguous scattered tables and tx wrappers" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE t(x INT);"
                    , "CREATE TABLE noise_a(k INT);"
                    , "BEGIN;"
                    , "INSERT INTO t VALUES (42);"
                    , "INSERT INTO noise_a VALUES (1);"
                    , "COMMIT;"
                    , "CREATE INDEX idx_noise_a ON noise_a(k);"
                    , "SELECT sum(x) AS answer FROM t;"
                    ]
        parsed <- parseSQL (defaultParseOptions SQLite) sql
        case parsed of
            Left err -> assertFailure ("parse failed: " ++ show err)
            Right input -> do
                let units = parsedUnits input
                    stmts = map unitStatement units
                    graph = analyzeStatements (parsedInitialSession input) units
                    plan =
                        extractDeletionProposalPlan
                            (parsedInitialSession input)
                            stmts
                            graph
                            (Just (ResultStatement 8 7))
                    weakSets = map proposalStatements (planWeakGroups plan)
                    allWrapperCandidates =
                        map proposalStatements (planWeakGroups plan ++ planTxWrappers plan)
                assertBool
                    "identifies noncontiguous set {2,5,7}"
                    (isNoncontiguousIn stmts (IntSet.fromList [2, 5, 7]))
                assertBool
                    "identifies contiguous set {3,4,5}"
                    (not (isNoncontiguousIn stmts (IntSet.fromList [3, 4, 5])))
                assertBool
                    ("expected scattered noise_a group {2,5,7} in weak groups: " ++ show weakSets)
                    (IntSet.fromList [2, 5, 7] `elem` weakSets)
                assertBool
                    ("expected noncontiguous BEGIN/COMMIT wrapper {3,6}: " ++ show allWrapperCandidates)
                    (IntSet.fromList [3, 6] `elem` allWrapperCandidates)
    , testCase "Noncontiguous tx-wrapper proposal removes BEGIN/COMMIT unreachable by contiguous ddmin" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE p(id INT PRIMARY KEY);"
                    , "CREATE TABLE c(pid INT REFERENCES p(id) ON DELETE CASCADE);"
                    , "BEGIN;"
                    , "INSERT INTO p VALUES (1), (2);"
                    , "INSERT INTO c VALUES (1), (2);"
                    , "COMMIT;"
                    , "PRAGMA foreign_keys = ON;"
                    , "DELETE FROM p WHERE id = 1;"
                    , "SELECT count(*) AS answer FROM c;"
                    ]
        contractRes <- generateSqliteContract sql
        case contractRes of
            Left err -> assertFailure ("contract failed: " ++ show err)
            Right contract -> do
                let runWith useGroups = do
                        pool <- outcomePool (sqliteContractOracle contract)
                        let cfg =
                                (defaultReductionConfigFor SQLite)
                                    { reductionGroupProposals = useGroups
                                    }
                        reduceSQLWithConfig cfg nullLogger pool sql
                withoutGroups <- runWith False
                withGroups <- runWith True
                assertBool "baseline contiguous search retains BEGIN" ("BEGIN;" `BSC.isInfixOf` withoutGroups)
                assertBool "group proposals remove BEGIN" (not ("BEGIN;" `BSC.isInfixOf` withGroups))
                assertBool "group proposals remove COMMIT" (not ("COMMIT;" `BSC.isInfixOf` withGroups))
                assertBool
                    "group proposals reach strictly smaller SQL"
                    (BS.length withGroups < BS.length withoutGroups)
                verdict <- sqliteContractOracle contract withGroups
                assertEqual "reduced SQL preserves native SQLite contract" OracleAccepted verdict
    , testCase "Rejected dependency group leaves its members eligible for contiguous search" $ do
        let sql =
                BSC.unlines
                    [ "CREATE TABLE t(x INT);"
                    , "CREATE TABLE aux(y INT);"
                    , "INSERT INTO t VALUES (40);"
                    , "INSERT INTO aux VALUES (2);"
                    , "CREATE INDEX idx_aux ON aux(y);"
                    , "SELECT sum(x) + (SELECT sum(y) FROM aux) AS answer FROM t;"
                    ]
        contractRes <- generateSqliteContract sql
        case contractRes of
            Left err -> assertFailure ("contract failed: " ++ show err)
            Right contract -> do
                pool <- outcomePool (sqliteContractOracle contract)
                let cfg =
                        (defaultReductionConfigFor SQLite)
                            { reductionGroupProposals = True
                            }
                reduced <- reduceSQLWithConfig cfg nullLogger pool sql
                assertBool
                    "dead index on required aux table is still deleted"
                    (not ("CREATE INDEX" `BSC.isInfixOf` reduced))
                verdict <- sqliteContractOracle contract reduced
                assertEqual "reduced SQL preserves native SQLite contract" OracleAccepted verdict
    , testCase "Relaxed slicing preserves pre-slice transaction block bodies for group proposals" $ do
        let txBlock =
                [ "BEGIN;"
                , "UPDATE t SET x = x + 1;"
                , "UPDATE t SET x = x - 1;"
                , "COMMIT;"
                ]
            sql =
                BSC.unlines
                    ( "CREATE TABLE t(x INT);"
                        : "INSERT INTO t VALUES (42);"
                        : "ANALYZE;"
                        : concat (replicate 16 txBlock)
                        ++ ["SELECT x AS answer FROM t;"]
                    )
        (remember, lookupTrace) <- newResultTraceTracker True
        prepared <- prepareResultOracleWithTrace remember SQLite 500000 sql
        (_, observer) <- either (assertFailure . show) pure prepared
        callsRef <- newIORef (0 :: Int)
        let decide cand = do
                modifyIORef' callsRef (+ 1)
                details <- observer (pure ()) cand
                pure (observationOutcome (detailedObservation details))
        pool <- outcomePool decide
        let cfg =
                (defaultReductionConfigFor SQLite)
                    { reductionGroupProposals = True
                    }
        ReductionResult out _ <-
            reduceSQLWithResultTrace lookupTrace (const (pure ())) cfg nullLogger pool sql
        calls <- readIORef callsRef
        assertBool "removes coupled no-op UPDATE pairs" (not ("UPDATE" `BSC.isInfixOf` out))
        assertBool "removes ANALYZE fence" (not ("ANALYZE" `BSC.isInfixOf` out))
        assertBool ("expected <= 12 oracle calls, got " ++ show calls) (calls <= 12)
    , testCase "Single-statement audit can miss a nonadjacent pair" $ do
        let statements = ["SELECT 10;", "SELECT 20;", "SELECT 30;", "SELECT 40;"]
            original = BSC.concat statements
            pairReduction = "SELECT 20;SELECT 40;"
            predicate sql = sql == original || sql == pairReduction
            config =
                (defaultReductionConfigFor SQLite)
                    { reductionSlicing = False
                    , reductionClosure = False
                    }
        pool <- predicatePool predicate
        reduced <- reduceSQLWithConfig config nullLogger pool original
        assertEqual "current partitions and single audit stop at the original" original reduced
        let acceptedPairs = [left <> right | left : rest <- tails statements, right <- rest, predicate (left <> right)]
        assertEqual
            "only the nonadjacent pair passes an exhaustive pair check"
            [pairReduction]
            acceptedPairs
    , testCase "Deferred VALUES chunk remains eligible after singleton rejections" $ do
        let original = "INSERT INTO t VALUES (1), (2), (3), (4), (5), (6), (7), (8);"
            singleton = "INSERT INTO t VALUES (2), (3), (4), (5), (6), (7), (8);"
            expected = "INSERT INTO t VALUES (1), (2), (5), (6), (7), (8);"
            config =
                (defaultReductionConfigFor SQLite)
                    { reductionIntraStatement = True
                    , reductionAdaptiveValues = True
                    }
        attempts <- newIORef []
        pool <- outcomePool $ \sql -> do
            modifyIORef' attempts (sql :)
            pure (if sql == original || sql == expected then OracleAccepted else OracleRejected)
        reduced <- reduceSQLWithConfig config nullLogger pool original
        assertEqual "only the fine chunk is accepted" expected reduced
        tested <- reverse <$> readIORef attempts
        assertBool "singletons are tested before the deferred fine chunk" $
            case (elemIndex singleton tested, elemIndex expected tested) of
                (Just singleOrdinal, Just chunkOrdinal) -> singleOrdinal < chunkOrdinal
                _ -> False
    ]
