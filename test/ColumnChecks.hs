module ColumnChecks (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Map.Strict qualified as Map
import Occam.Logger (nullLogger)
import Occam.Parser (
    ParseOptions (..),
    ParsedSQL (..),
    SQLDialect (..),
    StatementFacts (..),
    StatementUnit (..),
    defaultParseOptions,
    parseSQL,
 )
import Occam.Pool (OraclePool, PoolConfig (..), newOraclePool, predicateDetails)
import Occam.Reducer (
    ColumnMode (..),
    ReductionConfig (..),
    defaultReductionConfig,
    reduceSQLWithConfig,
 )
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hSetBinaryMode)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
    testGroup
        "Column reduction"
        [ testCase "atomic drops the unused column" testAtomicJoint
        , testCase "single keeps the joint column" testSingleKeeps
        , testCase "permuted explicit order binds by list position" testPermuted
        , testCase "implicit order binds by definition order" testImplicit
        , testCase "quoted multibyte identifiers round-trip" testUtf8
        , testCase "atomic includes the single-only column" testSingleOnly
        , testCase "syntax gate drops only edits" testSyntaxGate
        ]

-- ----------------------------------------------------------------------------
-- Actual SQLite oracle: every candidate executes in a fresh in-memory database
-- (isolated per call, no shared state, no reset). Binary-safe end to end.
-- ----------------------------------------------------------------------------

hGetStrict :: Handle -> IO ByteString
hGetStrict handle = go []
    where
        go acc = do
            chunk <- BS.hGetSome handle 4096
            if BS.null chunk then pure (BS.concat (reverse acc)) else go (chunk : acc)

runSQLiteBytes :: ByteString -> IO (ExitCode, ByteString)
runSQLiteBytes sql = do
    let process = (proc "sqlite3" [":memory:"]) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
    (Just stdinHandle, Just stdoutHandle, Just stderrHandle, processHandle) <- createProcess process
    hSetBinaryMode stdinHandle True
    hSetBinaryMode stdoutHandle True
    BS.hPut stdinHandle sql
    hClose stdinHandle
    out <- hGetStrict stdoutHandle
    _err <- hGetStrict stderrHandle
    code <- waitForProcess processHandle
    pure (code, out)

{- | Accepts exactly the candidates that execute cleanly and preserve the
expected ordered rowset of the trailing query.
-}
sqliteOracle :: ByteString -> ByteString -> IO Bool
sqliteOracle expected candidate = do
    (code, out) <- runSQLiteBytes (".bail on\n" <> candidate)
    pure (code == ExitSuccess && out == expected)

-- | Live schema of `t` after running a script, via the database itself.
tableColumns :: ByteString -> IO [ByteString]
tableColumns script = do
    let probe = script <> "\nSELECT '---';\nSELECT name FROM pragma_table_info('t') ORDER BY cid;\n"
    (code, out) <- runSQLiteBytes probe
    assertEqual "probe script executes" ExitSuccess code
    let (_, after) = BS.breakSubstring "---\n" out
    pure (BSC.lines (BS.drop 4 after))

testPoolWith :: (ByteString -> IO Bool) -> IO OraclePool
testPoolWith predicate =
    newOraclePool
        PoolConfig
            { poolJobs = 1
            , poolCacheEnabled = False
            , poolPermits = Nothing
            , poolPrecomputed = Map.empty
            , poolMetrics = Nothing
            , poolExecute = predicateDetails predicate
            , poolBill = const (pure ())
            , poolRetire = \_ _ _ _ -> pure ()
            }

reduceWith :: ColumnMode -> OraclePool -> ByteString -> IO ByteString
reduceWith mode =
    reduceSQLWithConfig
        ( defaultReductionConfig
            { reductionDialect = SQLite
            , reductionIntraStatement = True
            , reductionColumnMode = mode
            }
        )
        nullLogger

assertSchemaAndRows :: ByteString -> [ByteString] -> ByteString -> IO ()
assertSchemaAndRows script expectedColumns expectedRows = do
    columns <- tableColumns script
    assertEqual "live schema" expectedColumns columns
    (code, out) <- runSQLiteBytes script
    assertEqual "script executes" ExitSuccess code
    assertEqual "rowset preserved" expectedRows out

explicitScript :: ByteString
explicitScript =
    BSC.unlines
        [ "CREATE TABLE t(a INT, b INT);"
        , "INSERT INTO t(a, b) VALUES (1, 10), (2, 20);"
        , "SELECT a FROM t ORDER BY a;"
        ]

testAtomicJoint :: IO ()
testAtomicJoint = do
    pool <- testPoolWith (sqliteOracle "1\n2\n")
    reduced <- reduceWith ColumnAtomic pool explicitScript
    assertSchemaAndRows reduced ["a"] "1\n2\n"

testSingleKeeps :: IO ()
testSingleKeeps = do
    pool <- testPoolWith (sqliteOracle "1\n2\n")
    reduced <- reduceWith ColumnSingle pool explicitScript
    assertSchemaAndRows reduced ["a", "b"] "1\n2\n"

-- | The list order, not the definition order, binds explicit rows.
testPermuted :: IO ()
testPermuted = do
    let script =
            BSC.unlines
                [ "CREATE TABLE t(a INT, b INT);"
                , "INSERT INTO t(b, a) VALUES (10, 1), (20, 2);"
                , "SELECT a FROM t ORDER BY a;"
                ]
    pool <- testPoolWith (sqliteOracle "1\n2\n")
    reduced <- reduceWith ColumnAtomic pool script
    assertSchemaAndRows reduced ["a"] "1\n2\n"

testImplicit :: IO ()
testImplicit = do
    let script =
            BSC.unlines
                [ "CREATE TABLE t(a INT, b INT);"
                , "INSERT INTO t VALUES (1, 10), (2, 20);"
                , "SELECT a FROM t ORDER BY a;"
                ]
    pool <- testPoolWith (sqliteOracle "1\n2\n")
    reduced <- reduceWith ColumnAtomic pool script
    assertSchemaAndRows reduced ["a"] "1\n2\n"

testUtf8 :: IO ()
testUtf8 = do
    let script =
            BSC.unlines
                [ "CREATE TABLE t(a INT, \"b\xc3\xa9\" INT);"
                , "INSERT INTO t(a, \"b\xc3\xa9\") VALUES (1, 10), (2, 20);"
                , "SELECT A FROM t ORDER BY A;"
                ]
    pool <- testPoolWith (sqliteOracle "1\n2\n")
    reduced <- reduceWith ColumnAtomic pool script
    assertSchemaAndRows reduced ["a"] "1\n2\n"

{- | A lone-`CREATE`-site column has no bundle (it would duplicate the single);
both arms drop it through the identical single-point move.
-}
testSingleOnly :: IO ()
testSingleOnly = do
    let script =
            BSC.unlines
                [ "CREATE TABLE t(a INT, b INT);"
                , "INSERT INTO t(a) VALUES (1), (2);"
                , "SELECT a FROM t ORDER BY a;"
                ]
    singlePool <- testPoolWith (sqliteOracle "1\n2\n")
    singleReduced <- reduceWith ColumnSingle singlePool script
    assertSchemaAndRows singleReduced ["a"] "1\n2\n"
    atomicPool <- testPoolWith (sqliteOracle "1\n2\n")
    atomicReduced <- reduceWith ColumnAtomic atomicPool script
    assertSchemaAndRows atomicReduced ["a"] "1\n2\n"

testSyntaxGate :: IO ()
testSyntaxGate = do
    let base = defaultParseOptions SQLite
    parsedFull <- parseSQL (base {parseSyntaxEdits = True}) explicitScript
    parsedGated <- parseSQL (base {parseSyntaxEdits = False}) explicitScript
    case (parsedFull, parsedGated) of
        (Right full, Right gated) -> do
            assertEqual "same session" (parsedInitialSession full) (parsedInitialSession gated)
            assertEqual
                "same units modulo edits"
                (stripEdits (parsedUnits full))
                (stripEdits (parsedUnits gated))
            assertEqual "same columns" (parsedColumns full) (parsedColumns gated)
            assertBool "full carries edits" (not (all (null . unitEdits) (parsedUnits full)))
            assertBool "gated drops all edits" (all (null . unitEdits) (parsedUnits gated))
        _ -> assertFailure "parse failed"
    where
        stripEdits = map (\unit -> unit {unitFacts = fmap (\facts -> facts {factEdits = []}) (unitFacts unit)})
        unitEdits unit = maybe [] factEdits (unitFacts unit)
