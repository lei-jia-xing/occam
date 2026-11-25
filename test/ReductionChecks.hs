module ReductionChecks (tests) where

import Control.Exception (IOException, try)
import Control.Monad (forM_, when)
import Data.Aeson (Value (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntSet qualified as IntSet
import Data.List (elemIndex, tails)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text.Encoding qualified as TE
import Occam.Certificate (
    EarlyRejectCertifier (..),
    buildEarlyRejectCertifier,
    checkEarlyRejection,
 )
import Occam.Core (
    BarrierScope (GlobalScope),
    EarlyRejectReason (..),
    OracleInconclusiveError,
    OracleOutcome (..),
    ResultStatement (..),
    ResultTrace (..),
    ScopedBarrier (barrierScope),
    SemanticFacts (factBarriers),
    Statement (..),
    StatementId (..),
 )
import Occam.Dependency (DependencyGraph (..), analyzeStatements)
import Occam.Facts (
    ParserEdit (..),
    ParserEditCategory (..),
    StatementFacts (..),
    StatementUnit (..),
 )
import Occam.Logger (Logger (..), defaultLogConfig, nullLogger)
import Occam.Oracle (
    OracleObservation (..),
    OracleObservationDetails (..),
    observationOutcome,
 )
import Occam.Oracle.DuckDB (
    defaultDuckDBCandidateTimeoutMicros,
    duckdbContractOracle,
    duckdbContractOracleTimeout,
    generateDuckDBContractWithTrace,
    isDuckDBAvailable,
 )
import Occam.Oracle.Inline (newResultTraceTracker, prepareResultOracleWithTrace)
import Occam.Oracle.Sqlite (
    SqliteError (..),
    executeSqliteCandidateDetailedTimeout,
    generateSqliteContract,
    generateSqliteContractWithTrace,
    sqliteContractOracle,
 )
import Occam.Parser (
    ParseOptions (..),
    ParsedSQL (..),
    ParserError (..),
    ParserErrorKind (..),
    SQLDialect (..),
    countStatements,
    defaultParseOptions,
    parseSQL,
 )
import Occam.Pool (
    OraclePool,
    PoolConfig (..),
    newOraclePool,
    observationDetails,
 )
import Occam.Proposals (
    DeletionProposalGroup (..),
    GroupProposalPlan (..),
    extractDeletionProposalPlan,
    isNoncontiguousIn,
 )
import Occam.Reducer (
    ColumnMode (..),
    LaneScheduling (..),
    LiteralDeferralMode (..),
    ProjectionWindow (..),
    ProjectionWindowOrder (..),
    RankedScheduleConfig (..),
    ReductionConfig (..),
    ReductionResult (..),
    ReductionStage (..),
    SingletonResume (..),
    StageEvent (..),
    StatementSearchPolicy (..),
    applyParserEdits,
    closureDdmin,
    cutRemovedStatementInts,
    defaultRankedScheduleConfig,
    defaultReductionConfig,
    findInterestingPrefix,
    reduceSQL,
    reduceSQLWithConfig,
    reduceSQLWithEvidence,
    reduceSQLWithResultSlicing,
    reduceSQLWithResultTrace,
    selectIndependentEdits,
    shouldActivateRankedFallback,
    statementAudit,
    statementDdmin,
    statementDdminWithOrder,
    statementRanked,
 )
import System.Exit (ExitCode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
    testGroup
        "Reducer checks"
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
                    defaultReductionConfig
                        { reductionSlicing = False
                        , reductionClosure = True
                        , reductionIntraStatement = False
                        , reductionUnrestricted = False
                        , reductionDialect = dialect
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
                config = defaultReductionConfig {reductionDialect = SQLite}
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
                config = defaultReductionConfig {reductionDialect = SQLite}
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
                config = defaultReductionConfig {reductionDialect = SQLite}
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
                config = defaultReductionConfig {reductionDialect = SQLite}
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
                config = defaultReductionConfig {reductionDialect = SQLite}
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
                config = defaultReductionConfig {reductionDialect = SQLite}
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
                config = defaultReductionConfig {reductionDialect = SQLite}
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
                config = defaultReductionConfig {reductionDialect = PostgreSQL, reductionSlicing = False}
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
                    defaultReductionConfig
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
                    defaultReductionConfig
                        { reductionIntraStatement = False
                        , reductionUnrestricted = False
                        , reductionDialect = SQLite
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
                config = defaultReductionConfig {reductionDialect = GenericSQL, reductionIntraStatement = True}
            reduced <- reduceSQLWithConfig config nullLogger pool sql
            assertBool "strips table constraint" (not (BSC.isInfixOf "PRIMARY KEY" reduced))
            assertBool "strips foreign key constraint" (not (BSC.isInfixOf "FOREIGN KEY" reduced))
            assertBool "strips column constraint" (not (BSC.isInfixOf "NOT NULL" reduced))
            assertBool "simplifies case to branch" (BSC.isInfixOf "42" reduced)
        , testCase "Rejected literal batch splits and finds a coupled reduction" $ do
            let original = "SELECT 1234, 1234, 9999, 9999;"
                allChanged = "SELECT 0, 0, 0, 0;"
                expected = "SELECT 0, 0, 9999, 9999;"
                cfg = defaultReductionConfig {reductionDialect = SQLite, reductionIntraStatement = True}
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
                    defaultReductionConfig
                        { reductionDialect = SQLite
                        , reductionSlicing = False
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
                            defaultReductionConfig
                                { reductionDialect = SQLite
                                , reductionSlicing = False
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
                    defaultReductionConfig
                        { reductionDialect = SQLite
                        , reductionStatementSearch = fallbackPolicy
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
                                    defaultReductionConfig
                                        { reductionDialect = SQLite
                                        , reductionGroupProposals = useGroups
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
                            defaultReductionConfig
                                { reductionDialect = SQLite
                                , reductionGroupProposals = True
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
                    defaultReductionConfig
                        { reductionDialect = SQLite
                        , reductionGroupProposals = True
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
                    defaultReductionConfig
                        { reductionDialect = SQLite
                        , reductionSlicing = False
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
                    defaultReductionConfig
                        { reductionDialect = SQLite
                        , reductionIntraStatement = True
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
        , testCase "Refreshed syntax edits agree with full reparse across statement boundaries" $ do
            let cases =
                    [ (SQLite, "CREATE TABLE t(a INT NOT NULL);\r\n-- retained\r\nSELECT a, 123456 FROM t WHERE a > 0;")
                    ,
                        ( PostgreSQL
                        , "SET search_path TO public; CREATE TABLE t(a INT NOT NULL); SELECT a, 123456 FROM t WHERE a > 0;"
                        )
                    ,
                        ( DuckDB
                        , "CREATE MACRO identity_value(x) AS x; CREATE TABLE t(a INT NOT NULL); SELECT a, 123456 FROM t WHERE a > 0;"
                        )
                    ]
                run fullReparse dialect original = do
                    candidates <- newIORef []
                    pool <- outcomePool $ \candidate -> do
                        modifyIORef' candidates (candidate :)
                        let accepted = all (`BSC.isInfixOf` candidate) ["CREATE TABLE t(", "SELECT a", "FROM t"]
                        pure (if accepted then OracleAccepted else OracleRejected)
                    let config =
                            defaultReductionConfig
                                { reductionDialect = dialect
                                , reductionIntraStatement = True
                                , reductionLiteralBatches = True
                                , reductionAdaptiveValues = True
                                , reductionFullSyntaxReparse = fullReparse
                                }
                    reduced <- reduceSQLWithConfig config nullLogger pool original
                    trace <- reverse <$> readIORef candidates
                    pure (reduced, trace)
            mapM_
                ( \(dialect, original) -> do
                    incremental@(reduced, _) <- run False dialect original
                    full <- run True dialect original
                    assertBool (show dialect <> " applies an edit") (BS.length reduced < BS.length original)
                    assertEqual (show dialect <> " candidate bytes and endpoint") full incremental
                )
                cases
        , testCase "Trailing SELECT clause shifts preserve the full syntax-reparse candidate order" $ do
            let original =
                    "SELECT 123456 AS a, 6583 AS b WHERE 25 > 12 ORDER BY a, b LIMIT 10; SELECT 12345 AS c;"
                withoutLimit =
                    "SELECT 123456 AS a, 6583 AS b WHERE 25 > 12 ORDER BY a, b; SELECT 12345 AS c;"
                withoutOrder =
                    "SELECT 123456 AS a, 6583 AS b WHERE 25 > 12; SELECT 12345 AS c;"
                afterLiteralChange =
                    "SELECT 123456 AS a, 6583 AS b WHERE 25 > 12; SELECT 0 AS c;"
                accepted = [original, withoutLimit, withoutOrder, afterLiteralChange]
                run fullReparse = do
                    candidates <- newIORef []
                    pool <- outcomePool $ \candidate -> do
                        modifyIORef' candidates (candidate :)
                        pure (if candidate `elem` accepted then OracleAccepted else OracleRejected)
                    let cfg =
                            defaultReductionConfig
                                { reductionDialect = SQLite
                                , reductionIntraStatement = True
                                , reductionFullSyntaxReparse = fullReparse
                                }
                    reduced <- reduceSQLWithConfig cfg nullLogger pool original
                    trace <- reverse <$> readIORef candidates
                    pure (reduced, trace)
            fast@(endpoint, _) <- run False
            assertEqual "both trailing clauses and the later literal are removed" afterLiteralChange endpoint
            assertEqual "same ordered candidate bytes" fast =<< run True
        , testCase "applyParserEdits and selectIndependentEdits enforce disjoint non-gluing boundaries" $ do
            let sql = "CREATE TABLE t (a VARCHAR(20) NOT NULL, b BIGINT DEFAULT 1);"
                -- a: VARCHAR(20) [18..29) -> "INT", NOT NULL [29..38) -> ""
                -- b: BIGINT [42..48) -> "INT", DEFAULT 1 [48..58) -> ""
                eTypeA = ParserEdit 18 29 "INT" EditType "simplify-data_type" False False False False
                eTypeAText = ParserEdit 18 29 "TEXT" EditType "simplify-data_type" False False False False
                eConsA = ParserEdit 29 38 "" EditConstraint "remove-column_constraint" False False False False
                eTypeB = ParserEdit 42 48 "INT" EditType "simplify-data_type" False False False False
                eConsB = ParserEdit 48 58 "" EditConstraint "remove-column_constraint" False False False False
                gluedA = ParserEdit 18 23 "AA" EditExpression "test" False False False False
                gluedB = ParserEdit 23 29 "BB" EditExpression "test" False False False False
            assertEqual
                "applyParserEdits applies adjacent replacement + deletion cleanly"
                (Just "CREATE TABLE t (a INT, b INT);")
                (applyParserEdits sql [eConsB, eTypeA, eTypeB, eConsA])
            assertEqual
                "applyParserEdits rejects overlapping edits"
                Nothing
                (applyParserEdits sql [eTypeA, eTypeAText])
            assertEqual
                "applyParserEdits rejects gluing non-empty adjacent replacements"
                Nothing
                (applyParserEdits sql [gluedA, gluedB])
            assertEqual
                "applyParserEdits rejects out-of-bounds spans"
                Nothing
                (applyParserEdits sql [ParserEdit 0 999 "" EditClause "oob" False False False False])
            let selected = selectIndependentEdits [eTypeAText, eTypeA, gluedA, gluedB, eTypeB]
            assertEqual
                "selectIndependentEdits picks highest-savings non-overlapping non-gluing set"
                [eTypeA, eTypeB]
                selected
        , testCase "Independent AST edit batching cuts calls and subdivides rejected batches across dialects" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t1 (a VARCHAR(50) NOT NULL DEFAULT 'abc', b BIGINT NOT NULL, PRIMARY KEY (a));"
                        , "CREATE TABLE t2 (c VARCHAR(50) NOT NULL, d DECIMAL(10, 2) DEFAULT 0);"
                        , "SELECT a, c FROM t1 JOIN t2 ON t1.a = t2.c WHERE 1 = 1 ORDER BY a LIMIT 5;"
                        ]
                accepted sqlCandidate =
                    BSC.isInfixOf "PRIMARY KEY (a)" sqlCandidate
                        && BSC.isInfixOf "CREATE TABLE t1" sqlCandidate
                        && BSC.isInfixOf "CREATE TABLE t2" sqlCandidate
                        && BSC.isInfixOf "SELECT" sqlCandidate
                        && BSC.isInfixOf "WHERE 1 = 1" sqlCandidate
            mapM_
                ( \dialect -> do
                    controlCalls <- newIORef (0 :: Int)
                    controlPool <- outcomePool $ \c -> do
                        modifyIORef' controlCalls (+ 1)
                        pure (if accepted c then OracleAccepted else OracleRejected)
                    let controlCfg =
                            defaultReductionConfig
                                { reductionDialect = dialect
                                , reductionSlicing = False
                                , reductionClosure = False
                                , reductionUnrestricted = False
                                , reductionIntraStatement = True
                                }
                    reducedControl <- reduceSQLWithConfig controlCfg nullLogger controlPool sql
                    nControl <- readIORef controlCalls

                    batchCalls <- newIORef (0 :: Int)
                    batchPool <- outcomePool $ \c -> do
                        modifyIORef' batchCalls (+ 1)
                        pure (if accepted c then OracleAccepted else OracleRejected)
                    let batchCfg = controlCfg {reductionAstBatching = True}
                    reducedBatch <- reduceSQLWithConfig batchCfg nullLogger batchPool sql
                    nBatch <- readIORef batchCalls

                    assertBool (show dialect <> " preserves PRIMARY KEY") (BSC.isInfixOf "PRIMARY KEY (a)" reducedBatch)
                    assertBool (show dialect <> " removes NOT NULL") (not (BSC.isInfixOf "NOT NULL" reducedBatch))
                    assertBool (show dialect <> " removes DEFAULT") (not (BSC.isInfixOf "DEFAULT" reducedBatch))
                    assertBool (show dialect <> " removes ORDER BY") (not (BSC.isInfixOf "ORDER BY" reducedBatch))
                    assertBool (show dialect <> " removes LIMIT") (not (BSC.isInfixOf "LIMIT" reducedBatch))
                    assertBool
                        (show dialect <> " batching reduces oracle calls (" ++ show nBatch ++ " vs " ++ show nControl ++ ")")
                        (nBatch < nControl)
                    assertBool
                        (show dialect <> " batching shrinks at least as much as control")
                        (BS.length reducedBatch <= BS.length reducedControl)
                )
                [SQLite, PostgreSQL, MySQL, DuckDB]
        , testCase "ColumnAtomic refreshes bundles cleanly after EditList and AST batching in same statement" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (a INT, b VARCHAR(40) NOT NULL);"
                        , "INSERT INTO t(a, b) VALUES (1, 'keep'), (2, 'drop');"
                        , "SELECT a FROM t ORDER BY a;"
                        ]
                accepted candidate =
                    BSC.isInfixOf "CREATE TABLE t" candidate
                        && BSC.isInfixOf "INSERT INTO t" candidate
                        && BSC.isInfixOf "(1" candidate
                        && BSC.isInfixOf "SELECT a FROM t" candidate
            pool <- predicatePool accepted
            let cfg =
                    defaultReductionConfig
                        { reductionDialect = SQLite
                        , reductionSlicing = False
                        , reductionClosure = False
                        , reductionUnrestricted = False
                        , reductionIntraStatement = True
                        , reductionAstBatching = True
                        , reductionColumnMode = ColumnAtomic
                        }
            reduced <- reduceSQLWithConfig cfg nullLogger pool sql
            assertBool "accepted output remains valid" (accepted reduced)
            assertBool "unused column b removed by atomic bundle" (not (BSC.isInfixOf "'keep'" reduced))
            assertBool "redundant ORDER BY removed" (not (BSC.isInfixOf "ORDER BY" reduced))
            assertBool "unused row (2, 'drop') removed" (not (BSC.isInfixOf "'drop'" reduced))
        , testCase "Intra-statement rewrites reduce UPDATE assignments, WHERE clauses, and DELETE clauses" $ do
            pool <-
                predicatePool
                    ( \candidate ->
                        BSC.isInfixOf "UPDATE" candidate
                            && BSC.isInfixOf "target_col = 42" candidate
                            && BSC.isInfixOf "DELETE" candidate
                    )
            let sql =
                    BSC.unlines
                        [ "UPDATE t SET noise_col = 999, target_col = 42, other_noise = 888 WHERE dead_cond = 1;"
                        , "DELETE FROM t WHERE dead_delete_cond = 1;"
                        ]
                config = defaultReductionConfig {reductionDialect = GenericSQL, reductionIntraStatement = True}
            reduced <- reduceSQLWithConfig config nullLogger pool sql
            assertBool "keeps target assignment" (BSC.isInfixOf "target_col = 42" reduced)
            assertBool "removes noise_col assignment" (not (BSC.isInfixOf "noise_col" reduced))
            assertBool "removes other_noise assignment" (not (BSC.isInfixOf "other_noise" reduced))
            assertBool "removes dead update where clause" (not (BSC.isInfixOf "dead_cond" reduced))
            assertBool "removes dead delete where clause" (not (BSC.isInfixOf "dead_delete_cond" reduced))
            assertBool "keeps DELETE statement" (BSC.isInfixOf "DELETE FROM t;" reduced)
        , testCase "Intra-statement rewrites reduce compound queries across set operations" $ do
            pool <- predicatePool (BSC.isInfixOf "42")
            let sql =
                    "SELECT 999 UNION ALL (SELECT 888) UNION ALL SELECT 42;"
                config = defaultReductionConfig {reductionDialect = GenericSQL, reductionIntraStatement = True}
            reduced <- reduceSQLWithConfig config nullLogger pool sql
            assertBool "keeps target value" (BSC.isInfixOf "42" reduced)
            assertBool "removes noise branch 999" (not (BSC.isInfixOf "999" reduced))
            assertBool "removes noise branch 888" (not (BSC.isInfixOf "888" reduced))
            assertEqual "reduces to clean single query" "SELECT 42;" reduced
        , testCase "Intra-statement rewrites reduce DISTINCT, GROUP BY, ORDER BY, and window specifications" $ do
            pool <- predicatePool (BSC.isInfixOf "42")
            let sql =
                    "SELECT DISTINCT sum(42) OVER (PARTITION BY noise_part ORDER BY noise_ord ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING) FROM t GROUP BY a, noise_grp ORDER BY x, noise_order;"
                config = defaultReductionConfig {reductionDialect = GenericSQL, reductionIntraStatement = True}
            reduced <- reduceSQLWithConfig config nullLogger pool sql
            assertBool "keeps target value" (BSC.isInfixOf "42" reduced)
            assertBool "removes distinct modifier" (not (BSC.isInfixOf "DISTINCT" reduced))
            assertBool "removes noise partition" (not (BSC.isInfixOf "noise_part" reduced))
            assertBool "removes noise window order" (not (BSC.isInfixOf "noise_ord" reduced))
            assertBool "removes noise frame" (not (BSC.isInfixOf "ROWS BETWEEN" reduced))
            assertBool "removes noise group by item" (not (BSC.isInfixOf "noise_grp" reduced))
            assertBool "removes noise query order by item" (not (BSC.isInfixOf "noise_order" reduced))
        , testCase "Intra-statement rewrites unwrap CAST expressions" $ do
            pool <- predicatePool (BSC.isInfixOf "42")
            let sql = "SELECT CAST(42 AS VARCHAR(255));"
                config = defaultReductionConfig {reductionDialect = GenericSQL, reductionIntraStatement = True}
            reduced <- reduceSQLWithConfig config nullLogger pool sql
            assertEqual "unwraps cast to inner expression" "SELECT 42;" reduced
        , testCase "Intra-statement rewrites unwrap CASE expressions to matching branch" $ do
            pool <- predicatePool (BSC.isInfixOf "42")
            let sql = "SELECT CASE WHEN cond THEN 42 ELSE 99 END;"
                config = defaultReductionConfig {reductionDialect = GenericSQL, reductionIntraStatement = True}
            reduced <- reduceSQLWithConfig config nullLogger pool sql
            assertEqual "unwraps case expression to inner branch" "SELECT 42;" reduced
        , testCase "Interleaving statement audit and intra-statement rewrites reaches a global fixed point" $ do
            pool <-
                predicatePool (\candidate -> BSC.isInfixOf "main_tbl" candidate && BSC.isInfixOf "42" candidate)
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE helper (hid INT);"
                        , "INSERT INTO helper VALUES (999);"
                        , "CREATE TABLE main_tbl (id INT);"
                        , "INSERT INTO main_tbl VALUES (42);"
                        , "SELECT id FROM main_tbl WHERE id = 42 AND (SELECT COUNT(*) FROM helper) > 0;"
                        ]
                config = defaultReductionConfig {reductionDialect = GenericSQL, reductionIntraStatement = True}
            reduced <- reduceSQLWithConfig config nullLogger pool sql
            assertBool "keeps main table" (BSC.isInfixOf "main_tbl" reduced)
            assertBool "keeps target value" (BSC.isInfixOf "42" reduced)
            assertBool "eliminates helper table via interleaved audit" (not (BSC.isInfixOf "helper" reduced))
            assertBool "eliminates helper insert via interleaved audit" (not (BSC.isInfixOf "999" reduced))
        , testCase "Statement audit reaches a fixed point, permits empty output, and preserves bytes" $ do
            let sql = BSC.unlines ["SELECT 101;", "SELECT 202;", "SELECT 303;"]
                accepted = BSC.pack "SELECT 101;\nSELECT 303;\n"
                final = BSC.pack "SELECT 303;\n"
            pool <-
                predicatePool (\candidate -> candidate == sql || candidate == accepted || candidate == final)
            fixedPoint <- statementAudit GenericSQL pool sql
            assertEqual "a statement becomes removable only after an earlier deletion" final fixedPoint
            emptyPool <- predicatePool (const True)
            empty <- statementAudit GenericSQL emptyPool "SELECT 7;"
            assertEqual "the empty candidate is not prohibited" BS.empty empty
            keepPool <- predicatePool (BSC.isInfixOf "SELECT 11")
            let bytes = BSC.pack "SELECT 11;\r\nSELECT 22;\n"
                firstStatement = BSC.pack "SELECT 11;\r\n"
            byteResult <- statementDdmin GenericSQL keepPool bytes
            assertEqual "retained source bytes, including CRLF, are unchanged" firstStatement byteResult
        , testCase "statement audit distinguishes identical statements with different trailing comments" $ do
            let kept = "SELECT 1; -- A\n"
                input = kept <> "SELECT 1; -- B\n"
            forM_ [1, 4] $ \jobs -> do
                pool <- jobsPool jobs (BS.isInfixOf "-- A")
                reduced <- statementAudit SQLite pool input
                assertEqual "audit finds the interesting deletion" kept reduced
        , testCase "statement deletion removes exactly its own terminator and separator" $ do
            pool <-
                predicatePool
                    (\candidate -> BSC.isInfixOf "SELECT 2" candidate && BSC.isInfixOf "SELECT 3" candidate)
            let sql = BSC.pack "SELECT 1;\r\n\r\nSELECT 2;\r\nSELECT 3;\r\n"
            reduced <- statementDdmin GenericSQL pool sql
            assertEqual
                "the removed statement takes its CRLF and blank separator with it"
                (BSC.pack "SELECT 2;\r\nSELECT 3;\r\n")
                reduced
        , testCase "CTE removal preserves a required sibling and untouched source bytes" $ do
            let prefix = TE.encodeUtf8 "-- café\n"
                kept = "kept AS (SELECT 42 AS value)"
                query = "SELECT value FROM kept;\r\n"
                sql = prefix <> "WITH unused AS (SELECT 1), " <> kept <> " " <> query
            pool <- predicatePool (\candidate -> BS.isInfixOf kept candidate && BS.isInfixOf query candidate)
            reduced <- syntaxOnly SQLite pool sql
            assertBool "the unnecessary CTE is removed" (not (BSC.isInfixOf "unused" reduced))
            assertBool "the required CTE is unchanged" (BS.isInfixOf kept reduced)
            assertBool "the Unicode prefix is untouched" (BS.isPrefixOf prefix reduced)
            assertBool "the query and CRLF ending are untouched" (BS.isSuffixOf query reduced)
            secondPool <- predicatePool (BSC.isInfixOf "SELECT 42;")
            withoutCTEs <-
                syntaxOnly
                    SQLite
                    secondPool
                    "WITH unused AS (SELECT 1) SELECT 42;"
            assertBool
                "the final unnecessary CTE and WITH keyword can disappear"
                (not (BSC.isInfixOf "WITH" withoutCTEs))
        , testCase "VALUES row reduction excludes the INSERT target column list" $ do
            pool <-
                predicatePool
                    (\candidate -> BSC.isInfixOf "t(a,b)" candidate && BSC.isInfixOf "(42,43)" candidate)
            let sql = "INSERT INTO t(a,b) VALUES (1,2),(42,43);"
            reduced <- syntaxOnly SQLite pool sql
            assertEqual
                "only the irrelevant VALUES row and its comma are removed"
                "INSERT INTO t(a,b) VALUES (42,43);"
                reduced
        , testCase "VALUES chunk deletion removes contiguous rows with exact separators" $ do
            pool <- predicatePool (BSC.isInfixOf "(42)")
            let prefix = TE.encodeUtf8 "-- café\n"
                sql = prefix <> "INSERT INTO t VALUES (1),(2),(3),(42);\r\n"
                expected = prefix <> "INSERT INTO t VALUES (42);\r\n"
            reduced <- syntaxOnly SQLite pool sql
            assertEqual
                "contiguous chunk deletions and singles leave exact separators and untouched bytes"
                expected
                reduced
        , testCase "unused projection terms are removed inside a subquery" $ do
            pool <-
                predicatePool
                    (\candidate -> BSC.isInfixOf "SELECT a FROM" candidate && BSC.isInfixOf "42 AS a" candidate)
            let sql = "SELECT a FROM (SELECT 42 AS a,99 AS b);\r\n"
            reduced <- syntaxOnly SQLite pool sql
            assertEqual
                "the unused projection and comma disappear without reformatting"
                "SELECT a FROM (SELECT 42 AS a);\r\n"
                reduced
        , testCase "OFFSET can disappear while a required LIMIT remains" $ do
            pool <-
                predicatePool
                    (\candidate -> BSC.isInfixOf "SELECT a FROM t" candidate && BSC.isInfixOf "LIMIT 1" candidate)
            let sql = "SELECT a FROM t LIMIT 1 OFFSET 0;\r\n"
            reduced <- syntaxOnly SQLite pool sql
            assertEqual
                "LIMIT and the original ending survive OFFSET removal"
                "SELECT a FROM t LIMIT 1;\r\n"
                reduced
        , testCase "literal scalar subqueries collapse using their exact source span" $ do
            pool <- predicatePool (BSC.isInfixOf "42")
            let prefix = TE.encodeUtf8 "-- café\n"
                sql = prefix <> "SELECT (SELECT 42);\r\n"
            reduced <- syntaxOnly SQLite pool sql
            assertEqual "outer Unicode and CRLF bytes remain untouched" (prefix <> "SELECT 42;\r\n") reduced
            tableQuery <- syntaxOnly SQLite pool "SELECT * FROM (SELECT 42);"
            assertBool
                "table subqueries are not replaced by scalar literals"
                (not (BSC.isInfixOf "FROM 42" tableQuery))
        , testCase "SQL search retries an FK-blocked parent row after child reduction" $ do
            let prefix = TE.encodeUtf8 "-- café\n"
                script parentValues childValues =
                    prefix
                        <> BSC.pack
                            ( "PRAGMA foreign_keys = ON;\r\n"
                                ++ "CREATE TABLE parent (id INTEGER PRIMARY KEY);\r\n"
                                ++ "CREATE TABLE child (pid INTEGER REFERENCES parent(id));\r\n"
                                ++ "INSERT INTO parent VALUES "
                                ++ parentValues
                                ++ ";\r\n"
                                ++ "INSERT INTO child VALUES "
                                ++ childValues
                                ++ ";\r\n"
                                ++ "SELECT p.id FROM parent p JOIN child c ON c.pid = p.id WHERE p.id = 1;\r\n"
                            )
                initial = script "(1), (2)" "(1), (2)"
                blockedParent = script "(1)" "(1), (2)"
                afterChild = script "(1), (2)" "(1)"
                expected = script "(1)" "(1)"
                -- A deterministic external predicate accepts these three complete valid scripts.
                accepted = Set.fromList [initial, afterChild, expected]
            observations <- newIORef ([] :: [(ByteString, Bool)])
            pool <- outcomePool $ \candidate -> do
                let wasAccepted = Set.member candidate accepted
                modifyIORef' observations ((candidate, wasAccepted) :)
                pure (if wasAccepted then OracleAccepted else OracleRejected)
            reduced <- syntaxOnly SQLite pool initial
            seen <- reverse <$> readIORef observations
            let observedSQL = map fst seen
            assertEqual "the later parent deletion is accepted after its child" expected reduced
            assertEqual "the parent deletion is initially rejected" (Just False) (lookup blockedParent seen)
            assertEqual "the child deletion is accepted" (Just True) (lookup afterChild seen)
            assertEqual "the enabled parent deletion is accepted" (Just True) (lookup expected seen)
            let positions =
                    ( elemIndex blockedParent observedSQL
                    , elemIndex afterChild observedSQL
                    , elemIndex expected observedSQL
                    )
            case positions of
                (Just blockedAt, Just childAt, Just parentAt) -> do
                    assertBool "the blocked parent candidate comes before child removal" (blockedAt < childAt)
                    assertBool "child removal precedes the newly enabled parent removal" (childAt < parentAt)
                _ -> assertFailure "the oracle did not observe the three complete-script states"
        , testCase "Statement surgery preserves compound SQLite triggers and UTF-8 offsets" $ do
            let prefix = TE.encodeUtf8 "-- café\n"
                unused = BSC.pack "CREATE TABLE unused (id INT);\n"
                trigger =
                    TE.encodeUtf8
                        "CREATE TRIGGER tr AFTER INSERT ON t BEGIN UPDATE t SET x = CASE WHEN x = 'a;b' THEN 'c;d' ELSE x END; INSERT INTO audit VALUES ('x;y'); END;"
                target = TE.encodeUtf8 "SELECT 'semi;colon';\r\n"
                sql = prefix <> unused <> trigger <> "\r\n" <> target <> BSC.pack "SELECT 9;\n"
                expected = prefix <> trigger <> "\r\n" <> target
            pool <-
                predicatePool (\candidate -> BS.isInfixOf trigger candidate && BS.isInfixOf target candidate)
            statementCount <- countOrFail SQLite sql
            assertEqual "table, trigger, target, and following SELECT are four source units" 4 statementCount
            reduced <- statementDdmin SQLite pool sql
            assertEqual "the trigger is one source statement and retained bytes are untouched" expected reduced
        , testCase "Statement counting follows each selected SQL dialect" $ do
            sqliteCount <- countOrFail SQLite "SELECT /*; block */ 'a;b'; -- line; comment\nSELECT [g;h];"
            postgresCount <-
                countOrFail PostgreSQL "SELECT $tag$semi;colon$tag$; -- line; comment\nSELECT E'a\\';semi';"
            mysqlCount <- countOrFail MySQL "SELECT `g;h`; -- line; comment\nSELECT 'a;b';"
            assertEqual "SQLite quoted/comment semicolons stay within units" 2 sqliteCount
            assertEqual "PostgreSQL dollar and escaped strings stay within units" 2 postgresCount
            assertEqual "MySQL backticks and strings stay within units" 2 mysqlCount
            duckdbCount <- countOrFail DuckDB "SELECT [1, 2, 3]; -- comment\nSELECT * FROM range(5);"
            clickhouseCount <- countOrFail ClickHouse "SELECT number FROM numbers(10); -- comment\nSELECT 1;"
            oracleCount <- countOrFail Oracle "SELECT 1 FROM dual; -- comment\nSELECT 2 FROM dual;"
            ansiCount <- countOrFail Ansi "SELECT 1; -- comment\nSELECT 2;"
            mssqlCount <- countOrFail MsSql "SELECT [a;b]; -- comment\nSELECT TOP 1 * FROM [t];"
            snowflakeCount <- countOrFail Snowflake "SELECT \"a;b\"; -- comment\nSELECT 2;"
            bigqueryCount <- countOrFail BigQuery "SELECT `a;b`; -- comment\nSELECT 2;"
            assertEqual "DuckDB statements stay within units" 2 duckdbCount
            assertEqual "ClickHouse statements stay within units" 2 clickhouseCount
            assertEqual "Oracle statements stay within units" 2 oracleCount
            assertEqual "Ansi statements stay within units" 2 ansiCount
            assertEqual "MsSql statements stay within units" 2 mssqlCount
            assertEqual "Snowflake statements stay within units" 2 snowflakeCount
            assertEqual "BigQuery statements stay within units" 2 bigqueryCount
        , testCase "inconclusive oracle outcomes abort reduction" $ do
            pool <- outcomePool (\_ -> pure OracleInconclusive)
            result <-
                try (reduceSQL GenericSQL pool "SELECT 1;") ::
                    IO (Either OracleInconclusiveError ByteString)
            case result of
                Left _ -> pure ()
                Right _ -> assertFailure "inconclusive oracle outcome was treated as a definite decision"
        , testCase "invalid SQL still counts its raw statement units; transport errors stay explicit" $ do
            invalidCount <- countOrFail SQLite "SELECT FROM ; SELECT 42;"
            assertEqual "invalid syntax does not turn statement count into zero" 2 invalidCount
            invalidUtf8 <- countStatements SQLite (BS.pack [255])
            case invalidUtf8 of
                Left err -> assertEqual "invalid UTF-8 has a typed parser error" ParserInvalidUtf8 (parserErrorKind err)
                Right count -> assertFailure ("invalid UTF-8 unexpectedly counted as " ++ show count)
        , testCase "reducer propagates parser transport failures" $ do
            pool <- predicatePool (== BS.pack [255])
            let invalidUtf8 = BS.pack [255]
            result <-
                try (reduceSQL SQLite pool invalidUtf8) ::
                    IO (Either IOException ByteString)
            case result of
                Left _ -> pure ()
                Right _ -> assertFailure "invalid UTF-8 returned as a successful reduction"
        , testCase "findInterestingPrefix binary search handles empty, 1-stmt, tail, middle, and no-match" $ do
            let mkSQL n = BSC.unlines [BSC.pack ("SELECT " ++ show i ++ ";") | i <- [1 .. n]]
            -- Case 1: Empty list
            emptyPool <- predicatePool (const True)
            (resEmpty, testsEmpty) <- findInterestingPrefix emptyPool BS.empty []
            assertEqual "empty statements returns Nothing" Nothing resEmpty
            assertEqual "empty statements does 0 tests" 0 testsEmpty

            -- Case 2: 1-statement matching
            onePool <- predicatePool (const True)
            parsed1 <- parseSQL (defaultParseOptions SQLite) "SELECT 1;"
            case parsed1 of
                Left err -> assertFailure (show err)
                Right p1 -> do
                    let stmts1 = map unitStatement (parsedUnits p1)
                    (res1, tests1) <- findInterestingPrefix onePool "SELECT 1;" stmts1
                    assertBool "1-stmt matches" (isJust res1)
                    assertEqual "1-stmt does 1 test" 1 tests1

            -- Case 3: 21 statements with tail crash (prefix >= 21)
            tailPool <- predicatePool (BSC.isInfixOf "SELECT 21;")
            let sql21 = mkSQL (21 :: Int)
            parsed21 <- parseSQL (defaultParseOptions SQLite) sql21
            case parsed21 of
                Left err -> assertFailure (show err)
                Right p21 -> do
                    let stmts21 = map unitStatement (parsedUnits p21)
                    (resTail, testsTail) <- findInterestingPrefix tailPool sql21 stmts21
                    case resTail of
                        Nothing -> assertFailure "failed to find tail crash prefix"
                        Just (_, _, prefixLen) -> do
                            assertEqual "tail crash is at prefix 21" 21 prefixLen
                            assertBool ("binary search takes <= 5 tests (took " ++ show testsTail ++ ")") (testsTail <= 5)

            -- Case 4: 21 statements with middle crash (prefix >= 7)
            midPool <- predicatePool (BSC.isInfixOf "SELECT 7;")
            case parsed21 of
                Left err -> assertFailure (show err)
                Right p21 -> do
                    let stmts21 = map unitStatement (parsedUnits p21)
                    (resMid, testsMid) <- findInterestingPrefix midPool sql21 stmts21
                    case resMid of
                        Nothing -> assertFailure "failed to find middle crash prefix"
                        Just (_, _, prefixLen) -> do
                            assertEqual "middle crash is at prefix 7" 7 prefixLen
                            assertBool ("binary search takes <= 5 tests (took " ++ show testsMid ++ ")") (testsMid <= 5)

            -- Case 5: 21 statements with no match
            noPool <- predicatePool (const False)
            case parsed21 of
                Left err -> assertFailure (show err)
                Right p21 -> do
                    let stmts21 = map unitStatement (parsedUnits p21)
                    (resNo, testsNo) <- findInterestingPrefix noPool sql21 stmts21
                    assertEqual "no match returns Nothing" Nothing resNo
                    assertBool ("binary search takes <= 5 tests (took " ++ show testsNo ++ ")") (testsNo <= 5)
        , testCase "syntax search reaches a late clause after earlier same-category rejections" $ do
            let statements =
                    [ let value = BSC.pack (show index)
                       in "SELECT " <> value <> " WHERE " <> value <> " = " <> value <> ";"
                    | index <- [1 :: Int .. 42]
                    ]
                earlier = init statements
                sql = BSC.unlines statements
                expected = BSC.unlines (earlier ++ ["SELECT 42;"])
                accepts candidate =
                    candidate == sql
                        || ( all (`BSC.isInfixOf` candidate) earlier
                                && BSC.isInfixOf "SELECT 42;" candidate
                           )
            rejected <- newIORef (0 :: Int)
            pool <- outcomePool $ \candidate ->
                if accepts candidate
                    then pure OracleAccepted
                    else do
                        modifyIORef' rejected (+ 1)
                        pure OracleRejected
            reduced <- syntaxOnly SQLite pool sql
            rejectionCount <- readIORef rejected
            assertEqual "the final clause is removed after earlier clauses remain" expected reduced
            assertBool
                "the earlier same-category candidates were rejected"
                (rejectionCount >= length earlier)
        , testCase "safe clauses (ORDER BY / LIMIT) are prioritized before WHERE removal" $ do
            let sql = "SELECT * FROM t WHERE a = 1 ORDER BY id LIMIT 5;\r\n"
            probes <- newIORef ([] :: [ByteString])
            pool <- outcomePool $ \candidate -> do
                modifyIORef' probes (candidate :)
                if BSC.isInfixOf "WHERE a = 1" candidate
                    then pure OracleAccepted
                    else pure OracleRejected
            reduced <- syntaxOnly SQLite pool sql
            assertEqual
                "ORDER BY and LIMIT are removed while WHERE is preserved"
                "SELECT * FROM t WHERE a = 1;\r\n"
                reduced
            seen <- reverse <$> readIORef probes
            let withoutWhere = "SELECT * FROM t ORDER BY id LIMIT 5;\r\n"
                withoutLimit = "SELECT * FROM t WHERE a = 1 ORDER BY id;\r\n"
                withoutOrder = "SELECT * FROM t WHERE a = 1 LIMIT 5;\r\n"
            let whereIdx = elemIndex withoutWhere seen
                limitIdx = elemIndex withoutLimit seen
                orderIdx = elemIndex withoutOrder seen
            assertBool "safe clause candidate is tested before WHERE removal" $
                case (whereIdx, limitIdx, orderIdx) of
                    (Just w, Just l, _) -> l < w
                    (Just w, _, Just o) -> o < w
                    (Nothing, Just _, _) -> True
                    (Nothing, _, Just _) -> True
                    _ -> False
        , testCase "expression unwrapping is prioritized before destructive constant replacements" $ do
            let sql = "SELECT * FROM t WHERE (a = 1);\r\n"
            probes <- newIORef ([] :: [ByteString])
            pool <- outcomePool $ \candidate -> do
                modifyIORef' probes (candidate :)
                if BSC.isInfixOf "a = 1" candidate
                    then pure OracleAccepted
                    else pure OracleRejected
            reduced <- syntaxOnly SQLite pool sql
            assertEqual
                "parentheses are unwrapped"
                "SELECT * FROM t WHERE a = 1;\r\n"
                reduced
            seen <- reverse <$> readIORef probes
            let unwrapCandidate = "SELECT * FROM t WHERE a = 1;\r\n"
            assertBool "unwrap candidate was tested" (unwrapCandidate `elem` seen)
        , testCase "statement ddmin re-tests a removal enabled by a later shrink (jobs 1 and 4)" $ do
            -- Candidate memo is keyed by exact bytes only to allow removals enabled by later shrinks.
            let sql = BSC.unlines ["SELECT 0;", "SELECT 1;", "SELECT 0;", "SELECT 2;", "SELECT 42;"]
                expected = BSC.pack "SELECT 1;\nSELECT 42;\n"
                interesting candidate =
                    BSC.isInfixOf "SELECT 1;" candidate
                        && BSC.isInfixOf "SELECT 42;" candidate
                        && ( not (BSC.isInfixOf "SELECT 2;" candidate)
                                || length (filter (== "SELECT 0;") (BSC.lines candidate)) >= 2
                           )
            mapM_
                ( \jobs -> do
                    pool <- jobsPool jobs interesting
                    reduced <- statementDdmin GenericSQL pool sql
                    assertEqual ("ddmin reaches C,M with jobs=" ++ show jobs) expected reduced
                )
                [1, 4]
        , testCase "jobs 1 and 4 agree on audit, ddmin, and syntax fixed points" $ do
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t1 (a INT, b TEXT);"
                        , "CREATE TABLE t2 (c INT);"
                        , "INSERT INTO t1 VALUES (1, 'x');"
                        , "CREATE INDEX i1 ON t1 (a);"
                        , "SELECT * FROM t1 WHERE a > 0 AND 1 = 1 ORDER BY a LIMIT 1;"
                        ]
                interesting candidate =
                    BSC.isInfixOf "CREATE TABLE t1" candidate
                        && BSC.isInfixOf "INSERT INTO t1" candidate
                        && BSC.isInfixOf "WHERE a > 0" candidate
            mapM_
                ( \(name, reduce) -> do
                    serialPool <- jobsPool 1 interesting
                    serial <- reduce serialPool sql
                    widePool <- jobsPool 4 interesting
                    wide <- reduce widePool sql
                    assertEqual (name ++ " agrees across job counts") serial wide
                )
                [ ("statement audit", statementAudit GenericSQL)
                , ("statement ddmin", statementDdmin GenericSQL)
                , ("syntax only", syntaxOnly GenericSQL)
                ]
        , testCase "dead earlier update on a column is pruned by dependency slice" $ do
            pool <-
                predicatePool
                    ( \candidate ->
                        BSC.isInfixOf "SELECT a FROM t;" candidate
                            && BSC.isInfixOf "a = 2" candidate
                    )
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT, a INT);"
                        , "INSERT INTO t VALUES (1, 10);"
                        , "UPDATE t SET a = 1;"
                        , "UPDATE t SET a = 2;"
                        , "SELECT a FROM t;"
                        ]
            let config =
                    defaultReductionConfig
                        { reductionUnrestricted = False
                        , reductionIntraStatement = False
                        }
            reduced <- reduceSQLWithConfig config nullLogger pool sql
            assertBool "dead earlier update (a = 1) must be eliminated" (not (BSC.isInfixOf "a = 1" reduced))
            assertBool "effective update (a = 2) must be preserved" (BSC.isInfixOf "a = 2" reduced)
            assertBool "insert must be preserved for row existence" (BSC.isInfixOf "INSERT INTO t" reduced)
        , testCase "dead earlier update on a column is pruned during full reduction" $ do
            pool <-
                predicatePool
                    ( \candidate ->
                        BSC.isInfixOf "INSERT INTO t" candidate
                            && BSC.isInfixOf "SELECT a FROM t;" candidate
                            && BSC.isInfixOf "a = 2" candidate
                    )
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT, a INT);"
                        , "INSERT INTO t VALUES (1, 10);"
                        , "UPDATE t SET a = 1;"
                        , "UPDATE t SET a = 2;"
                        , "SELECT a FROM t;"
                        ]
            reduced <- reduceSQL GenericSQL pool sql
            assertBool "dead earlier update (a = 1) must be eliminated" (not (BSC.isInfixOf "a = 1" reduced))
            assertBool "effective update (a = 2) must be preserved" (BSC.isInfixOf "a = 2" reduced)
            assertBool "insert must be preserved" (BSC.isInfixOf "INSERT INTO t" reduced)
        , testCase "updates to distinct columns read by query are both preserved during reduction" $ do
            pool <-
                predicatePool
                    ( \candidate ->
                        BSC.isInfixOf "SELECT a, b FROM t;" candidate
                            && BSC.isInfixOf "a = 1" candidate
                            && BSC.isInfixOf "b = 2" candidate
                    )
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT, a INT, b INT);"
                        , "INSERT INTO t VALUES (1, 10, 20);"
                        , "UPDATE t SET a = 1;"
                        , "UPDATE t SET b = 2;"
                        , "SELECT a, b FROM t;"
                        ]
            reduced <- reduceSQL GenericSQL pool sql
            assertBool
                "both updates must be preserved"
                (BSC.isInfixOf "a = 1" reduced && BSC.isInfixOf "b = 2" reduced)
        , testCase "UPDATE on table is pruned during reduction when query only reads COUNT(*)" $ do
            pool <-
                predicatePool
                    ( \candidate ->
                        BSC.isInfixOf "INSERT INTO t" candidate
                            && BSC.isInfixOf "SELECT COUNT(*) FROM t;" candidate
                    )
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT, a INT);"
                        , "INSERT INTO t VALUES (1, 10);"
                        , "UPDATE t SET a = 1;"
                        , "UPDATE t SET a = 2;"
                        , "SELECT COUNT(*) FROM t;"
                        ]
            reduced <- reduceSQL GenericSQL pool sql
            assertBool "all updates must be eliminated" (not (BSC.isInfixOf "UPDATE t" reduced))
            assertBool "insert must be preserved" (BSC.isInfixOf "INSERT INTO t" reduced)
            assertBool "count query must be preserved" (BSC.isInfixOf "SELECT COUNT(*) FROM t;" reduced)
        , testCase "update with LIMIT does not eliminate earlier update in dependency slice" $ do
            pool <-
                predicatePool
                    ( \candidate ->
                        BSC.isInfixOf "SELECT a FROM t;" candidate
                            && BSC.isInfixOf "a = 2" candidate
                    )
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (id INT, a INT);"
                        , "INSERT INTO t VALUES (1, 10);"
                        , "UPDATE t SET a = 1;"
                        , "UPDATE t SET a = 2 LIMIT 1;"
                        , "SELECT a FROM t;"
                        ]
            let config =
                    defaultReductionConfig
                        { reductionDialect = MySQL
                        , reductionUnrestricted = False
                        , reductionIntraStatement = False
                        }
            reduced <- reduceSQLWithConfig config nullLogger pool sql
            assertBool "earlier update (a = 1) must be preserved" (BSC.isInfixOf "a = 1" reduced)
            assertBool "effective update (a = 2) must be preserved" (BSC.isInfixOf "a = 2" reduced)
            assertBool "insert must be preserved" (BSC.isInfixOf "INSERT INTO t" reduced)
        , testCase
            "atomic column reduction succeeds on clean table despite transaction block on another table"
            $ do
                pool <-
                    predicatePool
                        ( \candidate ->
                            BSC.isInfixOf "CREATE TABLE t" candidate
                                && BSC.isInfixOf "a INT" candidate
                                && BSC.isInfixOf "SELECT a FROM t" candidate
                                && BSC.isInfixOf "10" candidate
                                && BSC.isInfixOf "other" candidate
                        )
                let sql =
                        BSC.unlines
                            [ "BEGIN;"
                            , "CREATE TABLE other (x INT);"
                            , "INSERT INTO other VALUES (1);"
                            , "COMMIT;"
                            , "CREATE TABLE t (a INT, b INT);"
                            , "INSERT INTO t (a, b) VALUES (1, 10);"
                            , "SELECT a FROM t;"
                            ]
                let config =
                        defaultReductionConfig
                            { reductionDialect = SQLite
                            , reductionSlicing = False
                            , reductionClosure = False
                            , reductionIntraStatement = True
                            , reductionColumnMode = ColumnAtomic
                            }
                reduced <- reduceSQLWithConfig config nullLogger pool sql
                assertBool "keeps required table t with column a" (BSC.isInfixOf "a INT" reduced)
                assertBool "unneeded column b is dropped" (not (BSC.isInfixOf "b INT" reduced))
                assertBool "keeps other table in transaction" (BSC.isInfixOf "other" reduced)
        , testCase "atomic column reduction succeeds on clean table despite scoped barrier on another table" $ do
            pool <-
                predicatePool
                    ( \candidate ->
                        BSC.isInfixOf "CREATE TABLE t" candidate
                            && BSC.isInfixOf "a INT" candidate
                            && BSC.isInfixOf "SELECT a FROM t" candidate
                            && BSC.isInfixOf "10" candidate
                            && BSC.isInfixOf "CREATE VIRTUAL TABLE vt" candidate
                    )
            let sql =
                    BSC.unlines
                        [ "CREATE TABLE t (a INT, b INT);"
                        , "INSERT INTO t (a, b) VALUES (1, 10);"
                        , "SELECT a FROM t;"
                        , "CREATE VIRTUAL TABLE vt USING fts5(content);"
                        ]
            let config =
                    defaultReductionConfig
                        { reductionDialect = SQLite
                        , reductionSlicing = False
                        , reductionClosure = False
                        , reductionIntraStatement = True
                        , reductionColumnMode = ColumnAtomic
                        }
            reduced <- reduceSQLWithConfig config nullLogger pool sql
            assertBool "keeps required table t with column a" (BSC.isInfixOf "a INT" reduced)
            assertBool "unneeded column b is dropped" (not (BSC.isInfixOf "b INT" reduced))
            assertBool "keeps scoped barrier" (BSC.isInfixOf "CREATE VIRTUAL TABLE vt" reduced)
        , testCase "SQLite releases statements after a timeout during execution" $ do
            let query =
                    "WITH RECURSIVE r(x) AS (VALUES (1) UNION ALL SELECT x + 1 FROM r WHERE x < 1000000000) SELECT sum(x) FROM r;"
            forM_ [query, "CREATE TABLE t AS " <> query] $ \sql -> do
                outcome <- executeSqliteCandidateDetailedTimeout 50000 sql
                assertEqual "long execution reaches its timeout" (Left SqliteTimeout) outcome
            recovered <- executeSqliteCandidateDetailedTimeout 1000000 "SELECT 42;"
            assertEqual "subsequent SQLite run succeeds" (Right [[Number 42]]) recovered
        , testCase
            "Early-rejection exact-prefix certificate distinguishes mismatching and matching SQLite prefixes"
            $ do
                let sql =
                        BSC.unlines
                            [ "CREATE TABLE t(x INT);"
                            , "INSERT INTO t VALUES (1);"
                            , "INSERT INTO t VALUES (42) RETURNING x AS answer;"
                            , "CREATE TABLE noise(y INT);"
                            , "SELECT 0 AS answer;"
                            , "SELECT 42 AS answer;"
                            ]
                contractRes <- generateSqliteContractWithTrace 500000 sql
                (contract, trace) <- either (assertFailure . show) pure contractRes
                parsed <- parseSQL (defaultParseOptions SQLite) sql >>= either (assertFailure . show) pure
                let units = parsedUnits parsed
                    stmts = map unitStatement units
                    cert = buildEarlyRejectCertifier sql units (Just trace)
                    prefixKept n = IntSet.fromList [1 .. n]
                    prefixBytes n = cutRemovedStatementInts sql stmts (prefixKept n)
                assertBool "deterministic SQLite trace expected" (traceDeterministic trace)
                assertEqual
                    "prefix 1 (CREATE TABLE) is certified rejected"
                    (Just EarlyRejectExactPrefix)
                    (checkEarlyRejection cert (prefixKept 1) (prefixBytes 1))
                assertEqual
                    "prefix 2 (CREATE + INSERT) is certified rejected"
                    (Just EarlyRejectExactPrefix)
                    (checkEarlyRejection cert (prefixKept 2) (prefixBytes 2))
                assertEqual
                    "prefix 3 (matching RETURNING) must NOT be rejected"
                    Nothing
                    (checkEarlyRejection cert (prefixKept 3) (prefixBytes 3))
                assertEqual
                    "prefix 4 (matching RETURNING + DDL) must NOT be rejected"
                    Nothing
                    (checkEarlyRejection cert (prefixKept 4) (prefixBytes 4))
                assertEqual
                    "prefix 5 (overwriting SELECT 0) is certified rejected"
                    (Just EarlyRejectExactPrefix)
                    (checkEarlyRejection cert (prefixKept 5) (prefixBytes 5))
                forM_ [1 .. 5] $ \n -> do
                    let kept = prefixKept n
                        cand = prefixBytes n
                        certRes = checkEarlyRejection cert kept cand
                    outcome <- sqliteContractOracle contract cand
                    when (isJust certRes) $
                        assertEqual
                            ("certified prefix " ++ show n ++ " must be rejected by native SQLite")
                            OracleRejected
                            outcome
                outcome3 <- sqliteContractOracle contract (prefixBytes 3)
                outcome4 <- sqliteContractOracle contract (prefixBytes 4)
                assertEqual "prefix 3 actually matches native SQLite" OracleAccepted outcome3
                assertEqual "prefix 4 actually matches native SQLite" OracleAccepted outcome4
        , testCase "Nondeterministic SQLite scripts disable exact-prefix certification" $ do
            let scripts =
                    [ "CREATE TABLE t(x INT); SELECT random();"
                    , "CREATE TABLE t(x INT); SELECT datetime('now');"
                    , "CREATE TABLE t(x INT); SELECT CURRENT_TIMESTAMP;"
                    , "CREATE TABLE t(ts TEXT DEFAULT CURRENT_TIMESTAMP); INSERT INTO t DEFAULT VALUES; SELECT ts FROM t; SELECT 1;"
                    ]
            forM_ scripts $ \sql -> do
                contractRes <- generateSqliteContractWithTrace 500000 sql
                (_, trace) <- either (assertFailure . show) pure contractRes
                parsed <- parseSQL (defaultParseOptions SQLite) sql >>= either (assertFailure . show) pure
                let cert = buildEarlyRejectCertifier sql (parsedUnits parsed) (Just trace)
                assertBool
                    ("expected traceDeterministic=False for " ++ BSC.unpack sql)
                    (not (traceDeterministic trace))
                assertBool
                    ("expected empty exact-prefix byte lengths for " ++ BSC.unpack sql)
                    (IntSet.null (certExactPrefixByteLens cert))
                assertBool
                    ("expected empty exact-prefix lengths for " ++ BSC.unpack sql)
                    (IntSet.null (certExactPrefixLengths cert))
        , testCase
            "Zero-result allowlist excludes all potentially result-bearing statements on SQLite and DuckDB"
            $ do
                let sqliteSql =
                        BSC.unlines
                            [ "CREATE TABLE t(x INT);"
                            , "CREATE TABLE ctas AS SELECT 1 AS x;"
                            , "INSERT INTO t VALUES (1);"
                            , "INSERT INTO t VALUES (2) RETURNING x;"
                            , "UPDATE t SET x = 3;"
                            , "UPDATE t SET x = 4 RETURNING x;"
                            , "DELETE FROM t WHERE x = 1;"
                            , "DELETE FROM t WHERE x = 4 RETURNING x;"
                            , "PRAGMA foreign_keys = ON;"
                            , "PRAGMA foreign_keys;"
                            , "EXPLAIN SELECT 1;"
                            , "SELECT 42 AS answer;"
                            ]
                contractRes <- generateSqliteContractWithTrace 500000 sqliteSql
                (_, trace) <- either (assertFailure . show) pure contractRes
                parsed <- parseSQL (defaultParseOptions SQLite) sqliteSql >>= either (assertFailure . show) pure
                let cert = buildEarlyRejectCertifier sqliteSql (parsedUnits parsed) (Just trace)
                    guaranteed = certGuaranteedNonResult cert
                -- 1: CREATE TABLE, 3: INSERT, 5: UPDATE, 7: DELETE are allowlisted.
                -- 2: CTAS, 4/6/8: RETURNING, 9/10: PRAGMA, 11: EXPLAIN, 12: SELECT are excluded.
                assertEqual "SQLite guaranteed non-result set" (IntSet.fromList [1, 3, 5, 7]) guaranteed
                hasDuck <- isDuckDBAvailable
                when hasDuck $ do
                    let duckSql =
                            BSC.unlines
                                [ "CREATE TABLE t(x INT);"
                                , "CREATE TABLE ctas AS SELECT 1 AS x;"
                                , "INSERT INTO t VALUES (1);"
                                , "INSERT INTO t VALUES (2) RETURNING x;"
                                , "UPDATE t SET x = 3;"
                                , "UPDATE t SET x = 4 RETURNING x;"
                                , "DELETE FROM t WHERE x = 1;"
                                , "DELETE FROM t WHERE x = 4 RETURNING x;"
                                , "CREATE VIEW v AS SELECT 1 AS x;"
                                , "CREATE INDEX idx_t ON t(x);"
                                , "ALTER TABLE t ADD COLUMN y INT;"
                                , "BEGIN;"
                                , "COMMIT;"
                                , "DROP VIEW v;"
                                , "EXPLAIN SELECT 1;"
                                , "SELECT 42 AS answer;"
                                ]
                    duckRes <- generateDuckDBContractWithTrace defaultDuckDBCandidateTimeoutMicros duckSql
                    (duckContract, duckTrace) <- either (assertFailure . show) pure duckRes
                    duckParsed <- parseSQL (defaultParseOptions DuckDB) duckSql >>= either (assertFailure . show) pure
                    let duckCert = buildEarlyRejectCertifier duckSql (parsedUnits duckParsed) (Just duckTrace)
                        duckGuaranteed = certGuaranteedNonResult duckCert
                        duckStmts = map unitStatement (parsedUnits duckParsed)
                    assertEqual
                        "DuckDB guaranteed non-result set"
                        (IntSet.fromList [1, 3, 5, 7, 9, 10, 11, 12, 13, 14])
                        duckGuaranteed
                    -- Verify every allowlisted statement individually rejects on native DuckDB.
                    forM_ (IntSet.toList duckGuaranteed) $ \sid -> do
                        let cand = cutRemovedStatementInts duckSql duckStmts (IntSet.singleton sid)
                        outcome <- duckdbContractOracle duckContract cand
                        assertEqual
                            ("allowlisted DuckDB statement " ++ show sid ++ " must reject natively")
                            OracleRejected
                            outcome
        , testCase "Exhaustive native soundness validation on SQLite and DuckDB finds zero false rejections" $ do
            let allSubsets ids =
                    map IntSet.fromList (foldr (\x acc -> acc ++ map (x :) acc) [[]] ids)
                sqliteScripts =
                    [ BSC.unlines
                        [ "CREATE TABLE t(x INT);"
                        , "CREATE TRIGGER tr AFTER INSERT ON t BEGIN SELECT 1; END;"
                        , "INSERT INTO t VALUES (42);"
                        , "SAVEPOINT s;"
                        , "UPDATE t SET x = 0;"
                        , "ROLLBACK TO s;"
                        , "RELEASE s;"
                        , "SELECT x AS answer FROM t;"
                        ]
                    , BSC.unlines
                        [ "PRAGMA foreign_keys = ON;"
                        , "CREATE TABLE p(id INT PRIMARY KEY);"
                        , "CREATE TABLE c(pid INT REFERENCES p(id));"
                        , "INSERT INTO p VALUES (1);"
                        , "INSERT INTO c VALUES (1) RETURNING pid AS answer;"
                        , "SELECT 0 AS answer;"
                        , "SELECT 1 AS answer;"
                        ]
                    , BSC.unlines
                        [ "CREATE TABLE a(x INT); -- trailing comment"
                        , "CREATE VIEW v AS SELECT 42 AS answer;"
                        , "INSERT INTO a VALUES (1);"
                        , "CREATE TABLE b AS SELECT x FROM a;"
                        , "SELECT answer FROM v WHERE 0;"
                        ]
                    , BSC.unlines
                        [ "CREATE TABLE t(x INT);"
                        , "CREATE TABLE u(y INT);"
                        , "INSERT INTO u VALUES (10);"
                        , "ALTER TABLE u RENAME TO t2;"
                        , "WITH t AS (SELECT 1 AS x) SELECT x + (SELECT y FROM t2) AS answer FROM t;"
                        ]
                    , BSC.unlines
                        [ "CREATE TABLE t(x INT);"
                        , "BEGIN;"
                        , "INSERT INTO t VALUES (42);"
                        , "COMMIT;"
                        , "DROP TABLE IF EXISTS missing_tbl;"
                        , "SELECT x AS answer FROM t;"
                        ]
                    ]
            sqliteCheckedRef <- newIORef (0 :: Int)
            forM_ sqliteScripts $ \sql -> do
                contractRes <- generateSqliteContractWithTrace 500000 sql
                (contract, trace) <- either (assertFailure . show) pure contractRes
                parsed <- parseSQL (defaultParseOptions SQLite) sql >>= either (assertFailure . show) pure
                let units = parsedUnits parsed
                    stmts = map unitStatement units
                    cert = buildEarlyRejectCertifier sql units (Just trace)
                    ids = map (unStatementId . stmtId) stmts
                forM_ (allSubsets ids) $ \kept -> do
                    let cand = cutRemovedStatementInts sql stmts kept
                    case checkEarlyRejection cert kept cand of
                        Nothing -> pure ()
                        Just reason -> do
                            modifyIORef' sqliteCheckedRef (+ 1)
                            outcome <- sqliteContractOracle contract cand
                            assertEqual
                                ( "Soundness violation on SQLite ("
                                    ++ show reason
                                    ++ ") for kept="
                                    ++ show (IntSet.toList kept)
                                    ++ " sql="
                                    ++ BSC.unpack cand
                                )
                                OracleRejected
                                outcome
            sqliteChecked <- readIORef sqliteCheckedRef
            assertBool
                ("expected >= 100 SQLite certified subsets validated, got " ++ show sqliteChecked)
                (sqliteChecked >= 100)
            hasDuck <- isDuckDBAvailable
            when hasDuck $ do
                let duckScripts =
                        [ BSC.unlines
                            [ "CREATE TABLE t(x INT);"
                            , "CREATE TABLE n(y INT);"
                            , "INSERT INTO t VALUES (40);"
                            , "INSERT INTO t VALUES (2);"
                            , "INSERT INTO n VALUES (99);"
                            , "SELECT sum(x) AS answer FROM t;"
                            ]
                        , BSC.unlines
                            [ "CREATE TABLE t(x INT);"
                            , "INSERT INTO t VALUES (7) RETURNING x AS answer;"
                            , "CREATE TABLE ctas AS SELECT 1 AS z;"
                            , "SELECT 42 AS answer WHERE FALSE;"
                            , "CREATE TABLE tail(a INT);"
                            ]
                        ]
                duckCheckedRef <- newIORef (0 :: Int)
                forM_ duckScripts $ \sql -> do
                    duckRes <- generateDuckDBContractWithTrace defaultDuckDBCandidateTimeoutMicros sql
                    (contract, trace) <- either (assertFailure . show) pure duckRes
                    parsed <- parseSQL (defaultParseOptions DuckDB) sql >>= either (assertFailure . show) pure
                    let units = parsedUnits parsed
                        stmts = map unitStatement units
                        cert = buildEarlyRejectCertifier sql units (Just trace)
                        ids = map (unStatementId . stmtId) stmts
                    forM_ (allSubsets ids) $ \kept -> do
                        let cand = cutRemovedStatementInts sql stmts kept
                        case checkEarlyRejection cert kept cand of
                            Nothing -> pure ()
                            Just reason -> do
                                modifyIORef' duckCheckedRef (+ 1)
                                outcome <- duckdbContractOracle contract cand
                                assertEqual
                                    ( "Soundness violation on DuckDB ("
                                        ++ show reason
                                        ++ ") for kept="
                                        ++ show (IntSet.toList kept)
                                        ++ " sql="
                                        ++ BSC.unpack cand
                                    )
                                    OracleRejected
                                    outcome
                duckChecked <- readIORef duckCheckedRef
                assertBool
                    ("expected >= 30 DuckDB certified subsets validated, got " ++ show duckChecked)
                    (duckChecked >= 30)
        , testCase "Early rejection preserves exact reduced SQL and cuts physical oracle calls on SQLite" $ do
            let inputRow i =
                    "INSERT INTO t VALUES ("
                        ++ show (i :: Int)
                        ++ "); CREATE TABLE noise_"
                        ++ show i
                        ++ "(x INT);"
                sql =
                    BSC.pack $
                        unlines
                            ( "CREATE TABLE t(x INT);"
                                : map inputRow [1 .. 16]
                                ++ ["SELECT sum(x) AS answer FROM t;"]
                            )
                runWith earlyReject = do
                    (remember, lookupTrace) <- newResultTraceTracker True
                    prepared <- prepareResultOracleWithTrace remember SQLite 500000 sql
                    (_, observer) <- either (assertFailure . show) pure prepared
                    callsRef <- newIORef (0 :: Int)
                    certHitsRef <- newIORef (0 :: Int)
                    let logger =
                            Logger
                                defaultLogConfig
                                ( \_ _ msg fields ->
                                    when (msg == "Early rejection certificate summary") $ do
                                        let getInt k = case lookup k fields of
                                                Just (Number n) -> round n
                                                _ -> (0 :: Int)
                                        modifyIORef' certHitsRef (+ (getInt "exact_prefix_rejects" + getInt "no_result_rejects"))
                                )
                                (pure ())
                        decide cand = do
                            modifyIORef' callsRef (+ 1)
                            details <- observer (pure ()) cand
                            pure (observationOutcome (detailedObservation details))
                    pool <- outcomePool decide
                    let cfg =
                            defaultReductionConfig
                                { reductionDialect = SQLite
                                , reductionEarlyReject = earlyReject
                                }
                    ReductionResult out _ <-
                        reduceSQLWithResultTrace lookupTrace (const (pure ())) cfg logger pool sql
                    calls <- readIORef callsRef
                    hits <- readIORef certHitsRef
                    pure (out, calls, hits)
            (outOff, callsOff, hitsOff) <- runWith False
            (outOn, callsOn, hitsOn) <- runWith True
            assertEqual "early rejection must preserve exact reduced SQL" outOff outOn
            assertEqual "disabled early rejection must record zero certificate hits" 0 hitsOff
            assertBool ("expected certificate hits > 0, got " ++ show hitsOn) (hitsOn > 0)
            assertBool
                ( "expected strictly fewer oracle calls with early rejection: on="
                    ++ show callsOn
                    ++ " off="
                    ++ show callsOff
                )
                (callsOn < callsOff)
        , testCase
            "Synthetic non-result oracle via reduceSQLWithResultSlicing never activates early rejection"
            $ do
                let sql =
                        BSC.unlines
                            [ "CREATE TABLE noise(x INT);"
                            , "CREATE TABLE keep(x INT);"
                            , "INSERT INTO keep VALUES (1);"
                            ]
                pool <-
                    predicatePool
                        (\cand -> BSC.isInfixOf "CREATE TABLE keep" cand && BSC.isInfixOf "INSERT INTO keep" cand)
                let locate _ = pure (Just (ResultStatement 3 2))
                    cfg =
                        defaultReductionConfig
                            { reductionDialect = SQLite
                            , reductionEarlyReject = True
                            }
                ReductionResult out _ <-
                    reduceSQLWithResultSlicing locate (const (pure ())) cfg nullLogger pool sql
                assertBool "must preserve keep table" (BSC.isInfixOf "CREATE TABLE keep" out)
                assertBool "must preserve keep insert" (BSC.isInfixOf "INSERT INTO keep" out)
                assertBool "must remove noise table" (not (BSC.isInfixOf "noise" out))
        , testCase
            "Column-arity certificate skips outer projection deletions while preserving inner CTE pruning"
            $ do
                let sql =
                        BSC.unlines
                            [ "CREATE TABLE t(a INT, b INT, c INT);"
                            , "INSERT INTO t VALUES (1, 2, 3);"
                            , "WITH cte AS (SELECT a, b, c, 99 AS unused_cte_col FROM t) SELECT a, b, c FROM cte;"
                            ]
                    runWith earlyReject = do
                        (remember, lookupTrace) <- newResultTraceTracker True
                        prepared <- prepareResultOracleWithTrace remember SQLite 500000 sql
                        (_, observer) <- either (assertFailure . show) pure prepared
                        callsRef <- newIORef (0 :: Int)
                        arityHitsRef <- newIORef (0 :: Int)
                        let logger =
                                Logger
                                    defaultLogConfig
                                    ( \_ _ msg fields ->
                                        when (msg == "Early rejection certificate summary") $ do
                                            let getInt k = case lookup k fields of
                                                    Just (Number n) -> round n
                                                    _ -> (0 :: Int)
                                            modifyIORef' arityHitsRef (+ getInt "column_arity_rejects")
                                    )
                                    (pure ())
                            decide cand = do
                                modifyIORef' callsRef (+ 1)
                                details <- observer (pure ()) cand
                                pure (observationOutcome (detailedObservation details))
                        pool <- outcomePool decide
                        let cfg =
                                defaultReductionConfig
                                    { reductionDialect = SQLite
                                    , reductionIntraStatement = True
                                    , reductionEarlyReject = earlyReject
                                    }
                        ReductionResult out _ <-
                            reduceSQLWithResultTrace lookupTrace (const (pure ())) cfg logger pool sql
                        calls <- readIORef callsRef
                        hits <- readIORef arityHitsRef
                        pure (out, calls, hits)
                (outOff, callsOff, hitsOff) <- runWith False
                (outOn, callsOn, hitsOn) <- runWith True
                assertEqual "column-arity certificate preserves exact reduced SQL" outOff outOn
                assertEqual "disabled early rejection records zero arity hits" 0 hitsOff
                assertBool ("expected column_arity_rejects > 0, got " ++ show hitsOn) (hitsOn > 0)
                assertBool "inner CTE still pruned unused column" (not ("unused_cte_col" `BSC.isInfixOf` outOn))
                assertBool
                    ("expected strictly fewer oracle calls: on=" ++ show callsOn ++ " off=" ++ show callsOff)
                    (callsOn < callsOff)
        , testCase "Deferred literal leaf span-shift matches full syntax reparse exactly on multi-row VALUES" $ do
            let rows =
                    [ "(" <> BSC.pack (show i) <> ", 'payload_string_value_" <> BSC.pack (show i) <> "')"
                    | i <- [1 .. 24 :: Int]
                    ]
                sql =
                    BSC.unlines
                        [ "CREATE TABLE t(id INT, label TEXT);"
                        , "INSERT INTO t VALUES " <> BS.intercalate ", " rows <> ";"
                        , "SELECT sum(id), count(*) FROM t;"
                        ]
            contractRes <- generateSqliteContract sql
            contract <- either (assertFailure . show) pure contractRes
            let runWith fullReparse = do
                    pool <- outcomePool (sqliteContractOracle contract)
                    let cfg =
                            defaultReductionConfig
                                { reductionDialect = SQLite
                                , reductionIntraStatement = True
                                , reductionFullSyntaxReparse = fullReparse
                                }
                    reduceSQLWithConfig cfg nullLogger pool sql
            fullOut <- runWith True
            fastOut <- runWith False
            assertEqual "deferred literal leaf shift matches full reparse byte-for-byte" fullOut fastOut
            verdict <- sqliteContractOracle contract fastOut
            assertEqual "deferred literal output preserves SQLite contract" OracleAccepted verdict
        , testCase
            "Distributed cross-statement literal batching cuts calls and preserves output across SQLite and DuckDB"
            $ do
                let mkScript colType n =
                        BSC.pack $
                            unlines $
                                ("CREATE TABLE t (a INT, x " ++ colType ++ ");")
                                    : [ "INSERT INTO t VALUES ("
                                            ++ show i
                                            ++ ", 'payload_"
                                            ++ show (1000 + i)
                                            ++ "');"
                                      | i <- [1 .. n :: Int]
                                      ]
                                    ++ ["SELECT count(*) AS n FROM t;"]
                    sqliteSql = mkScript "TEXT" 36
                sqliteContract <- either (assertFailure . show) pure =<< generateSqliteContract sqliteSql
                let runSqlite cfg = do
                        inSyntaxRef <- newIORef False
                        syntaxCallsRef <- newIORef (0 :: Int)
                        let report (StageStarted StageIntraStatementRewrites) = writeIORef inSyntaxRef True
                            report (StageFinished StageIntraStatementRewrites _ _ _) = writeIORef inSyntaxRef False
                            report _ = pure ()
                        pool <- outcomePool $ \cand -> do
                            inSyn <- readIORef inSyntaxRef
                            when inSyn (modifyIORef' syntaxCallsRef (+ 1))
                            sqliteContractOracle sqliteContract cand
                        ReductionResult out _ <-
                            reduceSQLWithResultTrace (const (pure Nothing)) report cfg nullLogger pool sqliteSql
                        calls <- readIORef syntaxCallsRef
                        pure (out, calls)
                    baseCfg =
                        defaultReductionConfig
                            { reductionDialect = SQLite
                            , reductionSlicing = False
                            , reductionClosure = False
                            , reductionUnrestricted = False
                            , reductionIntraStatement = True
                            , reductionDistributedLiteralBatches = False
                            , reductionDeferLiteralVariants = DeferLiteralsOff
                            }
                    distCfg = baseCfg {reductionDistributedLiteralBatches = True}
                (outOff, callsOff) <- runSqlite baseCfg
                (outDist, callsDist) <- runSqlite distCfg
                (outLitBatch, callsLitBatch) <- runSqlite distCfg {reductionLiteralBatches = True}
                (outAstBatch, callsAstBatch) <- runSqlite distCfg {reductionAstBatching = True}
                assertEqual "distributed batching matches single-edit SQLite output" outOff outDist
                assertEqual "--literal-batches matches single-edit SQLite output" outOff outLitBatch
                assertEqual "--ast-batching matches single-edit SQLite output" outOff outAstBatch
                assertBool
                    ( "distributed batching cuts SQLite syntax calls sharply ("
                        ++ show callsDist
                        ++ " vs "
                        ++ show callsOff
                        ++ ")"
                    )
                    (callsDist * 4 < callsOff)
                assertBool
                    ( "--literal-batches keeps cross-statement speedup ("
                        ++ show callsLitBatch
                        ++ " vs "
                        ++ show callsOff
                        ++ ")"
                    )
                    (callsLitBatch * 4 < callsOff)
                assertBool
                    ( "--ast-batching keeps cross-statement speedup ("
                        ++ show callsAstBatch
                        ++ " vs "
                        ++ show callsOff
                        ++ ")"
                    )
                    (callsAstBatch * 4 < callsOff)
                hasDuck <- isDuckDBAvailable
                when hasDuck $ do
                    let duckSql = mkScript "VARCHAR" 32
                        duckTimeoutMicros = 2000000
                    (duckContract, _) <-
                        either (assertFailure . show) pure
                            =<< generateDuckDBContractWithTrace duckTimeoutMicros duckSql
                    let runDuck cfg = do
                            inSyntaxRef <- newIORef False
                            syntaxCallsRef <- newIORef (0 :: Int)
                            let report (StageStarted StageIntraStatementRewrites) = writeIORef inSyntaxRef True
                                report (StageFinished StageIntraStatementRewrites _ _ _) = writeIORef inSyntaxRef False
                                report _ = pure ()
                            pool <- outcomePool $ \cand -> do
                                inSyn <- readIORef inSyntaxRef
                                when inSyn (modifyIORef' syntaxCallsRef (+ 1))
                                duckdbContractOracleTimeout duckTimeoutMicros duckContract cand
                            ReductionResult out _ <-
                                reduceSQLWithResultTrace (const (pure Nothing)) report cfg nullLogger pool duckSql
                            calls <- readIORef syntaxCallsRef
                            pure (out, calls)
                        duckBase = baseCfg {reductionDialect = DuckDB}
                        duckDist = distCfg {reductionDialect = DuckDB}
                    (duckOff, duckCallsOff) <- runDuck duckBase
                    (duckOn, duckCallsOn) <- runDuck duckDist
                    assertEqual "distributed batching matches single-edit DuckDB output" duckOff duckOn
                    assertBool
                        ( "distributed batching cuts DuckDB syntax calls sharply ("
                            ++ show duckCallsOn
                            ++ " vs "
                            ++ show duckCallsOff
                            ++ ")"
                        )
                        (duckCallsOn * 4 < duckCallsOff)
        , testCase
            "Distributed literal batching handles typed, nonzero, head-required, and multi-column scripts"
            $ do
                let typedSql =
                        BSC.pack $
                            unlines $
                                "CREATE TABLE t (a INT, x);"
                                    : "INSERT INTO t VALUES (0, 0);"
                                    : [ "INSERT INTO t VALUES ("
                                            ++ show i
                                            ++ ", 'payload_"
                                            ++ show (1000 + i)
                                            ++ "');"
                                      | i <- [1 .. 32 :: Int]
                                      ]
                                    ++ ["SELECT count(*) AS n, sum(typeof(x) = 'text') AS ok FROM t;"]
                    nonzeroSql =
                        BSC.pack $
                            unlines $
                                "CREATE TABLE t (x INT);"
                                    : "INSERT INTO t VALUES (0);"
                                    : replicate 32 "INSERT INTO t VALUES (1000);"
                                    ++ ["SELECT count(*) AS n, sum(CASE WHEN x > 0 THEN 1 ELSE 0 END) AS ok FROM t;"]
                    first16Sql =
                        BSC.pack $
                            unlines $
                                "CREATE TABLE t (a INT, x TEXT);"
                                    : [ "INSERT INTO t VALUES ("
                                            ++ show i
                                            ++ ", 'payload_"
                                            ++ show (1000 + i)
                                            ++ "');"
                                      | i <- [1 .. 40 :: Int]
                                      ]
                                    ++ ["SELECT count(*) AS n, sum(CASE WHEN a <= 16 THEN length(x) ELSE 0 END) AS s FROM t;"]
                    multiColSql =
                        BSC.pack $
                            unlines $
                                "CREATE TABLE t (a INT, x TEXT);"
                                    : [ "INSERT INTO t VALUES ("
                                            ++ show i
                                            ++ ", 'payload_"
                                            ++ show (1000 + i)
                                            ++ "');"
                                      | i <- [1 .. 36 :: Int]
                                      ]
                                    ++ ["SELECT sum(a) AS s FROM t;"]
                    cases =
                        [ ("typed", typedSql)
                        , ("nonzero", nonzeroSql)
                        , ("first16", first16Sql)
                        , ("multi-col", multiColSql)
                        ]
                forM_ cases $ \(label, sql) -> do
                    contract <- either (assertFailure . show) pure =<< generateSqliteContract sql
                    let runDist distOn = do
                            callsRef <- newIORef (0 :: Int)
                            pool <- outcomePool $ \cand -> do
                                modifyIORef' callsRef (+ 1)
                                sqliteContractOracle contract cand
                            let cfg =
                                    defaultReductionConfig
                                        { reductionDialect = SQLite
                                        , reductionSlicing = False
                                        , reductionClosure = False
                                        , reductionUnrestricted = False
                                        , reductionIntraStatement = True
                                        , reductionDistributedLiteralBatches = distOn
                                        , reductionDeferLiteralVariants = DeferLiteralsZeroOne
                                        }
                            out <- reduceSQLWithConfig cfg nullLogger pool sql
                            calls <- readIORef callsRef
                            pure (out, calls)
                    (outOff, callsOff) <- runDist False
                    (outOn, callsOn) <- runDist True
                    if label == "first16"
                        then
                            assertEqual
                                (label ++ " distributed output matches single-edit minimum size")
                                (BS.length outOff)
                                (BS.length outOn)
                        else assertEqual (label ++ " distributed output matches singles") outOff outOn
                    verdict <- sqliteContractOracle contract outOn
                    assertEqual (label ++ " output preserves SQLite contract") OracleAccepted verdict
                    assertBool
                        (label ++ " distributed batching cuts calls (" ++ show callsOn ++ " vs " ++ show callsOff ++ ")")
                        (callsOn < callsOff)
        , testCase
            "Incremental distributed literal batch refresh matches full syntax reparse trace across dialects"
            $ do
                let sql =
                        BSC.pack $
                            unlines $
                                "CREATE TABLE t (a INT, x);"
                                    : [ "INSERT INTO t VALUES ("
                                            ++ show i
                                            ++ ", 'payload_"
                                            ++ show (1000 + i)
                                            ++ "');"
                                      | i <- [1 .. 36 :: Int]
                                      ]
                                    ++ ["SELECT sum(a) FROM t;"]
                    keepKeys = [BSC.pack ("VALUES (" ++ show i ++ ",") | i <- [1 .. 16 :: Int]]
                    accepted cand =
                        BSC.isInfixOf "CREATE TABLE t" cand
                            && BSC.isInfixOf "SELECT sum(a)" cand
                            && all (`BSC.isInfixOf` cand) keepKeys
                            && length (filter (BSC.isPrefixOf "INSERT INTO t VALUES") (BSC.lines cand)) == 36
                forM_ [SQLite, PostgreSQL, MySQL, DuckDB] $ \dialect ->
                    forM_ [False, True] $ \astBatching -> do
                        let run fullReparse = do
                                traceRef <- newIORef ([] :: [ByteString])
                                pool <- outcomePool $ \cand -> do
                                    modifyIORef' traceRef (cand :)
                                    pure (if accepted cand then OracleAccepted else OracleRejected)
                                let cfg =
                                        defaultReductionConfig
                                            { reductionDialect = dialect
                                            , reductionSlicing = False
                                            , reductionClosure = False
                                            , reductionUnrestricted = False
                                            , reductionIntraStatement = True
                                            , reductionDistributedLiteralBatches = True
                                            , reductionAstBatching = astBatching
                                            , reductionDeferLiteralVariants = DeferLiteralsZeroOne
                                            , reductionFullSyntaxReparse = fullReparse
                                            }
                                reduced <- reduceSQLWithConfig cfg nullLogger pool sql
                                trace <- reverse <$> readIORef traceRef
                                pure (reduced, trace)
                        fast@(reducedFast, _) <- run False
                        full <- run True
                        assertBool
                            (show dialect ++ " astBatch=" ++ show astBatching ++ " shrinks SQL")
                            (BS.length reducedFast < BS.length sql)
                        assertEqual
                            (show dialect ++ " astBatch=" ++ show astBatching ++ " exact ordered trace parity")
                            full
                            fast
        , testCase
            "Literal boundary and deferred follow-up edits match full reparse trace across dialects"
            $ do
                let sql =
                        BSC.unlines
                            [ "SELECT'payload_tight'FROM t;"
                            , "SELECT DATE '2020-01-01', 'alpha_one', 12345, 'beta_two';"
                            ]
                    accepted cand =
                        BSC.isInfixOf "FROM t;" cand
                            && BSC.isInfixOf "SELECT DATE " cand
                            && (BSC.isInfixOf "'alpha_one'" cand || BSC.isInfixOf "'0'" cand)
                            && (BSC.isInfixOf "12345" cand || BSC.isInfixOf "NULL" cand)
                            && length (BSC.lines cand) == 2
                forM_ [SQLite, PostgreSQL, MySQL, DuckDB] $ \dialect -> do
                    let run fullReparse = do
                            traceRef <- newIORef ([] :: [ByteString])
                            pool <- outcomePool $ \cand -> do
                                modifyIORef' traceRef (cand :)
                                pure (if accepted cand then OracleAccepted else OracleRejected)
                            let cfg =
                                    defaultReductionConfig
                                        { reductionDialect = dialect
                                        , reductionSlicing = False
                                        , reductionClosure = False
                                        , reductionUnrestricted = False
                                        , reductionIntraStatement = True
                                        , reductionDeferLiteralVariants = DeferLiteralsZeroOne
                                        , reductionFullSyntaxReparse = fullReparse
                                        }
                            reduced <- reduceSQLWithConfig cfg nullLogger pool sql
                            trace <- reverse <$> readIORef traceRef
                            pure (reduced, trace)
                    fast@(reducedFast, _) <- run False
                    full <- run True
                    assertBool
                        (show dialect ++ " shrinks boundary/deferred literals")
                        (BS.length reducedFast < BS.length sql)
                    assertBool (show dialect ++ " reaches deferred '0'") (BSC.isInfixOf "'0'" reducedFast)
                    assertBool (show dialect ++ " reaches deferred NULL") (BSC.isInfixOf "NULL" reducedFast)
                    assertEqual (show dialect ++ " boundary/deferred trace parity") full fast
        , testCase "Deferred literal variants test primary replacements first and preserve interacting edits" $ do
            let multiCycleSql =
                    "CREATE TABLE t (a INT, b INT, tag TEXT); INSERT INTO t VALUES (100, 200, 'Acme Corp'), (300, 400, 'Beta LLC'); SELECT a, tag FROM t WHERE a >= 100 AND b > 0 ORDER BY a LIMIT 2;"
                nonzeroSql =
                    "CREATE TABLE t (a INT, b INT); INSERT INTO t VALUES (100, 200), (300, 400); SELECT count(*) AS n FROM t WHERE a > 0 AND b > 0;"
                quote0Sql =
                    "CREATE TABLE t (x TEXT); INSERT INTO t VALUES ('payload_alpha'), ('payload_beta'); SELECT count(*) AS n FROM t WHERE x <> '' AND typeof(x) = 'text';"
                unlockSql =
                    "CREATE TABLE t (a INT, x TEXT); INSERT INTO t VALUES (100, 'payload_alpha'); SELECT count(*) AS n FROM t WHERE (x = '0' OR x = 'payload_alpha') AND a > 0;"
                runSqlite sql deferMode = do
                    contract <- either (assertFailure . show) pure =<< generateSqliteContract sql
                    inSyntaxRef <- newIORef False
                    traceRef <- newIORef ([] :: [ByteString])
                    let report (StageStarted StageIntraStatementRewrites) = writeIORef inSyntaxRef True
                        report (StageFinished StageIntraStatementRewrites _ _ _) = writeIORef inSyntaxRef False
                        report _ = pure ()
                    pool <- outcomePool $ \cand -> do
                        inSyn <- readIORef inSyntaxRef
                        when inSyn (modifyIORef' traceRef (cand :))
                        sqliteContractOracle contract cand
                    let cfg =
                            defaultReductionConfig
                                { reductionDialect = SQLite
                                , reductionSlicing = False
                                , reductionClosure = False
                                , reductionUnrestricted = False
                                , reductionIntraStatement = True
                                , reductionDistributedLiteralBatches = False
                                , reductionDeferLiteralVariants = deferMode
                                }
                    ReductionResult out _ <-
                        reduceSQLWithResultTrace (const (pure Nothing)) report cfg nullLogger pool sql
                    trace <- reverse <$> readIORef traceRef
                    verdict <- sqliteContractOracle contract out
                    assertEqual "output satisfies SQLite contract" OracleAccepted verdict
                    pure (out, trace)
            (mcOff, mcTraceOff) <- runSqlite multiCycleSql DeferLiteralsOff
            (mcOn, mcTraceOn) <- runSqlite multiCycleSql DeferLiteralsZeroOne
            assertEqual "multi-cycle deferred literals match byte-for-byte" mcOff mcOn
            assertBool
                ( "deferred literal variants cut multi-cycle syntax calls ("
                    ++ show (length mcTraceOn)
                    ++ " vs "
                    ++ show (length mcTraceOff)
                    ++ ")"
                )
                (length mcTraceOn < length mcTraceOff)
            let firstPrimaryIdx = elemIndex True (map (BSC.isInfixOf "(0, 200") mcTraceOn)
                firstDeferredIdx = elemIndex True (map (BSC.isInfixOf "NULL") mcTraceOn)
            case (firstPrimaryIdx, firstDeferredIdx) of
                (Just pIdx, Just dIdx) ->
                    assertBool "primary replacement is tested before deferred NULL" (pIdx < dIdx)
                (Just _, Nothing) -> pure ()
                _ -> assertFailure "expected primary replacement candidate in trace"
            forM_ [("nonzero", nonzeroSql), ("quote0", quote0Sql), ("unlock", unlockSql)] $ \(label, sql) -> do
                (outOff, _) <- runSqlite sql DeferLiteralsOff
                (outOn, _) <- runSqlite sql DeferLiteralsZeroOne
                assertEqual (label ++ " interacting deferred literal matches exhaustive output") outOff outOn
            (unlockOut, _) <- runSqlite unlockSql DeferLiteralsZeroOne
            assertBool
                "deferred '0' shrinks payload_alpha and collapses OR branch"
                (not (BSC.isInfixOf "payload_alpha" unlockOut))
        , testCase
            "Distributed literal batching defers low-yield empty-string edits under DeferLiteralsZeroOne"
            $ do
                let sql =
                        BSC.pack $
                            unlines $
                                "CREATE TABLE t (a INT, x);"
                                    : replicate 32 "INSERT INTO t VALUES (1000, '');"
                                    ++ ["SELECT count(*) AS n, group_concat(typeof(x)) AS ok FROM t;"]
                    countMatches needle = go (0 :: Int)
                        where
                            go !acc bs =
                                case BSC.breakSubstring needle bs of
                                    (_, rest)
                                        | BSC.null rest -> acc
                                        | otherwise -> go (acc + 1) (BSC.drop (BS.length needle) rest)
                contract <- either (assertFailure . show) pure =<< generateSqliteContract sql
                traceRef <- newIORef ([] :: [ByteString])
                pool <- outcomePool $ \cand -> do
                    modifyIORef' traceRef (cand :)
                    sqliteContractOracle contract cand
                let cfg =
                        defaultReductionConfig
                            { reductionDialect = SQLite
                            , reductionSlicing = False
                            , reductionClosure = False
                            , reductionUnrestricted = False
                            , reductionIntraStatement = True
                            , reductionDistributedLiteralBatches = True
                            , reductionDeferLiteralVariants = DeferLiteralsZeroOne
                            }
                out <- reduceSQLWithConfig cfg nullLogger pool sql
                trace <- reverse <$> readIORef traceRef
                assertBool
                    "never batches multiple low-yield '' -> 0 edits ahead of deferral"
                    (not (any (\c -> countMatches "VALUES (0, 0)" c >= 2) trace))
                assertBool
                    "reduces primary 1000 -> 0 while preserving ''"
                    (BSC.isInfixOf "(0, '')" out && not (BSC.isInfixOf "1000" out))
                verdict <- sqliteContractOracle contract out
                assertEqual "output preserves SQLite contract" OracleAccepted verdict
        , testCase
            "Recursive predicate flag roundtrips through binary FFI and scopes only to recursive members"
            $ do
                let sql =
                        BSC.unlines
                            [ "WITH RECURSIVE"
                            , "  h(emp_id, manager_id) AS ("
                            , "    SELECT emp_id, manager_id FROM emp WHERE manager_id IS NULL"
                            , "    UNION ALL"
                            , "    SELECT e.emp_id, e.manager_id FROM emp e JOIN h ON e.manager_id = h.emp_id WHERE e.active = 1"
                            , "  ),"
                            , "  dept AS (SELECT emp_id FROM h WHERE emp_id > 0)"
                            , "SELECT * FROM dept WHERE emp_id > 0;"
                            ]
                    opts = (defaultParseOptions SQLite) {parseSyntaxEdits = True}
                parsed <- either (assertFailure . show) pure =<< parseSQL opts sql
                let edits = concatMap (maybe [] factEdits . unitFacts) (parsedUnits parsed)
                    snippet e = BS.take (parserEditEnd e - parserEditStart e) (BS.drop (parserEditStart e) sql)
                    recEdits = filter parserEditRecursivePredicate edits
                    nonRecEdits = filter (not . parserEditRecursivePredicate) edits
                assertBool
                    "recursive member JOIN ON predicate has recursive_predicate=True"
                    (any (\e -> "e.manager_id = h.emp_id" `BSC.isInfixOf` snippet e) recEdits)
                assertBool
                    "recursive member WHERE predicate has recursive_predicate=True"
                    (any (\e -> "e.active = 1" `BSC.isInfixOf` snippet e) recEdits)
                assertBool
                    "remove-join is not flagged as recursive_predicate"
                    (all (\e -> parserEditLabel e /= "remove-join") recEdits)
                assertBool
                    "base member WHERE is not flagged as recursive_predicate"
                    (not (any (\e -> "manager_id IS NULL" `BSC.isInfixOf` snippet e) recEdits))
                assertBool
                    "downstream non-recursive CTE and outer WHERE are not flagged as recursive_predicate"
                    (not (any (\e -> "emp_id > 0" `BSC.isInfixOf` snippet e) recEdits))
                assertBool
                    "non-recursive edits exist alongside recursive ones"
                    (any (\e -> "emp_id > 0" `BSC.isInfixOf` snippet e) nonRecEdits)
        , testCase
            "Deferred recursive predicates retire other edits first while preserving inconclusive timeout status"
            $ do
                let sql =
                        BSC.unlines
                            [ "WITH RECURSIVE h(emp_id, manager_id) AS ("
                            , "  SELECT emp_id, manager_id FROM emp WHERE manager_id IS NULL"
                            , "  UNION ALL"
                            , "  SELECT e.emp_id, e.manager_id FROM emp e JOIN h ON e.manager_id = h.emp_id WHERE e.active = 1"
                            , ")"
                            , "SELECT emp_id, 'long_department_label', 'long_employee_name' FROM h;"
                            ]
                    runWith deferRec = do
                        bestRef <- newIORef sql
                        pool <- outcomePool $ \cand ->
                            if not (BSC.isInfixOf "e.manager_id = h.emp_id" cand)
                                && BSC.isInfixOf "JOIN h ON" cand
                                then pure OracleInconclusive
                                else
                                    if BSC.isInfixOf "WITH RECURSIVE h" cand
                                        && BSC.isInfixOf "WHERE manager_id IS NULL" cand
                                        && BSC.isInfixOf "JOIN h ON e.manager_id = h.emp_id WHERE e.active = 1" cand
                                        && BSC.isInfixOf "FROM h;" cand
                                        then do
                                            prev <- readIORef bestRef
                                            when (BS.length cand < BS.length prev) (writeIORef bestRef cand)
                                            pure OracleAccepted
                                        else pure OracleRejected
                        let cfg =
                                defaultReductionConfig
                                    { reductionDialect = SQLite
                                    , reductionSlicing = False
                                    , reductionClosure = False
                                    , reductionUnrestricted = False
                                    , reductionIntraStatement = True
                                    , reductionDeferRecursivePredicates = deferRec
                                    }
                        res <-
                            try (reduceSQLWithConfig cfg nullLogger pool sql) ::
                                IO (Either OracleInconclusiveError ByteString)
                        best <- readIORef bestRef
                        pure (res, best)
                (resOff, bestOff) <- runWith False
                (resOn, bestOn) <- runWith True
                case resOff of
                    Left _ -> pure ()
                    Right _ -> assertFailure "expected OracleInconclusiveError when recursive deferral is off"
                case resOn of
                    Left _ -> pure ()
                    Right _ -> assertFailure "deferred recursive timeout must preserve OracleInconclusiveError"
                assertBool
                    "deferred mode retires non-recursive string literals before recursive timeout"
                    ( not (BSC.isInfixOf "long_department_label" bestOn)
                        && not (BSC.isInfixOf "long_employee_name" bestOn)
                    )
                assertBool
                    ( "deferred checkpoint is strictly smaller ("
                        ++ show (BS.length bestOn)
                        ++ " vs "
                        ++ show (BS.length bestOff)
                        ++ ")"
                    )
                    (BS.length bestOn < BS.length bestOff)
        , testCase "Deferred recursive predicates still simplify and complete on terminating recursive CTEs" $ do
            let sqliteSql =
                    "CREATE TABLE edges (u INT, v INT, note TEXT); INSERT INTO edges VALUES (1, 2, 'alpha_edge'), (2, 3, 'beta_edge'); WITH RECURSIVE reach(n) AS (SELECT 1 UNION SELECT e.v FROM edges e JOIN reach r ON e.u = r.n AND e.v > 0) SELECT sum(n) AS answer FROM reach;"
            sqliteContract <- either (assertFailure . show) pure =<< generateSqliteContract sqliteSql
            let runSqlite deferRec = do
                    pool <- outcomePool (sqliteContractOracle sqliteContract)
                    let cfg =
                            defaultReductionConfig
                                { reductionDialect = SQLite
                                , reductionIntraStatement = True
                                , reductionDeferRecursivePredicates = deferRec
                                }
                    reduceSQLWithConfig cfg nullLogger pool sqliteSql
            sqliteOff <- runSqlite False
            sqliteOn <- runSqlite True
            assertEqual "SQLite terminating recursive CTE matches non-deferred output" sqliteOff sqliteOn
            assertBool
                "SQLite redundant recursive JOIN conjunct is removed"
                (not (BSC.isInfixOf "e.v > 0" sqliteOn))
            sqliteVerdict <- sqliteContractOracle sqliteContract sqliteOn
            assertEqual "SQLite output satisfies contract" OracleAccepted sqliteVerdict
            hasDuck <- isDuckDBAvailable
            when hasDuck $ do
                let duckSql =
                        "CREATE TABLE edges (u INT, v INT, note VARCHAR); INSERT INTO edges VALUES (1, 2, 'alpha_edge'), (2, 3, 'beta_edge'); WITH RECURSIVE reach(n) AS (SELECT 1 UNION SELECT e.v FROM edges e JOIN reach r ON e.u = r.n AND e.v > 0) SELECT sum(n) AS answer FROM reach;"
                    duckTimeoutMicros = 2000000
                (duckContract, _) <-
                    either (assertFailure . show) pure
                        =<< generateDuckDBContractWithTrace duckTimeoutMicros duckSql
                let runDuck deferRec = do
                        pool <- outcomePool (duckdbContractOracleTimeout duckTimeoutMicros duckContract)
                        let cfg =
                                defaultReductionConfig
                                    { reductionDialect = DuckDB
                                    , reductionIntraStatement = True
                                    , reductionDeferRecursivePredicates = deferRec
                                    }
                        reduceSQLWithConfig cfg nullLogger pool duckSql
                duckOff <- runDuck False
                duckOn <- runDuck True
                assertEqual "DuckDB terminating recursive CTE matches non-deferred output" duckOff duckOn
                assertBool
                    "DuckDB redundant recursive JOIN conjunct is removed"
                    (not (BSC.isInfixOf "e.v > 0" duckOn))
                duckVerdict <- duckdbContractOracleTimeout duckTimeoutMicros duckContract duckOn
                assertEqual "DuckDB output satisfies contract" OracleAccepted duckVerdict
        ]

countOrFail :: SQLDialect -> ByteString -> IO Int
countOrFail dialect sql = do
    result <- countStatements dialect sql
    case result of
        Left err -> assertFailure ("SQL parser transport error: " ++ show err)
        Right count -> pure count

-- | Throwaway in-process pool for search-logic tests.
predicatePool :: (ByteString -> Bool) -> IO OraclePool
predicatePool predicate = testPoolWith 1 (pure . decide)
    where
        decide candidate = if predicate candidate then OracleAccepted else OracleRejected

outcomePool :: (ByteString -> IO OracleOutcome) -> IO OraclePool
outcomePool = testPoolWith 1

jobsPool :: Int -> (ByteString -> Bool) -> IO OraclePool
jobsPool jobs predicate = testPoolWith jobs (pure . decide)
    where
        decide candidate = if predicate candidate then OracleAccepted else OracleRejected

testPoolWith :: Int -> (ByteString -> IO OracleOutcome) -> IO OraclePool
testPoolWith jobs decide =
    newOraclePool
        PoolConfig
            { poolJobs = jobs
            , poolCacheEnabled = False
            , poolPermits = Nothing
            , poolPrecomputed = Map.empty
            , poolMetrics = Nothing
            , poolExecute = fmap (observationDetails . toObservation) . decide
            , poolBill = const (pure ())
            , poolRetire = \_ _ _ _ -> pure ()
            , poolReportCount = Nothing
            }
    where
        toObservation OracleAccepted = OracleFinished ExitSuccess
        toObservation OracleRejected = OracleFinished (ExitFailure 1)
        toObservation OracleInconclusive = OracleTimedOut

syntaxOnly :: SQLDialect -> OraclePool -> ByteString -> IO ByteString
syntaxOnly dialect =
    reduceSQLWithConfig
        defaultReductionConfig
            { reductionSlicing = False
            , reductionClosure = False
            , reductionUnrestricted = False
            , reductionIntraStatement = True
            , reductionDialect = dialect
            }
        nullLogger
