module ColumnChecks (tests) where

import Control.Concurrent (forkIOWithUnmask, killThread, newEmptyMVar, putMVar, readMVar)
import Control.Exception (SomeException, bracket, finally, handle, throwIO, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Map.Strict qualified as Map
import Occam.Core (Ident (..))
import Occam.Facts (ColumnTarget (..))
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
    defaultReductionConfigFor,
    reduceSQLWithConfig,
 )
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hSetBinaryMode)
import System.IO.Error (isResourceVanishedError)
import System.Process (CreateProcess (..), StdStream (..), proc, waitForProcess, withCreateProcess)
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
        , testCase "atomic drops despite add-column on another column" testAtomicAlterAddOther
        , testCase "atomic drops despite rename of another column" testAtomicRenameColumnOther
        , testCase "atomic follows a table rename" testAtomicRenameFollow
        , testCase "syntax gate drops only edits" testSyntaxGate
        , testCase "alter-column on another column keeps the bundle" testAlterColumnOtherKeepsBundle
        , testCase "atomic drops despite transaction block on another table" testAtomicTransactionOther
        , testCase "atomic drops despite scoped barrier on another table" testAtomicScopedBarrierOther
        ]

-- In-memory SQLite oracle execution.

hGetStrict :: Handle -> IO ByteString
hGetStrict pipeHandle = go []
    where
        go acc = do
            chunk <- BS.hGetSome pipeHandle 4096
            if BS.null chunk then pure (BS.concat (reverse acc)) else go (chunk : acc)

withDrain :: Handle -> (IO ByteString -> IO a) -> IO a
withDrain pipeHandle use =
    bracket
        ( do
            result <- newEmptyMVar
            tid <- forkIOWithUnmask $ \unmask -> do
                res <- try @SomeException (unmask (hGetStrict pipeHandle `finally` hClose pipeHandle))
                putMVar result res
            pure (tid, result)
        )
        ( \(tid, result) -> do
            killThread tid
            _ <- readMVar result
            pure ()
        )
        (\(_, result) -> use (readMVar result >>= either throwIO pure))

runSQLiteBytes :: ByteString -> IO (ExitCode, ByteString)
runSQLiteBytes sql =
    withCreateProcess process $ \mIn mOut mErr ph ->
        case (mIn, mOut, mErr) of
            (Just stdinHandle, Just stdoutHandle, Just stderrHandle) -> do
                hSetBinaryMode stdinHandle True
                hSetBinaryMode stdoutHandle True
                hSetBinaryMode stderrHandle True
                withDrain stdoutHandle $ \waitOut ->
                    withDrain stderrHandle $ \waitErr -> do
                        handle
                            (\err -> if isResourceVanishedError err then pure () else throwIO err)
                            (BS.hPut stdinHandle sql)
                            `finally` hClose stdinHandle
                        out <- waitOut
                        _err <- waitErr
                        code <- waitForProcess ph
                        pure (code, out)
            _ -> ioError (userError "Missing sqlite3 process pipes")
    where
        process = (proc "sqlite3" [":memory:"]) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}

-- | Accept candidates matching expected query output in SQLite.
sqliteOracle :: ByteString -> ByteString -> IO Bool
sqliteOracle expected candidate = do
    (code, out) <- runSQLiteBytes (".bail on\n" <> candidate)
    pure (code == ExitSuccess && out == expected)

-- | Live schema of a table after running a script, via the database itself.
tableColumnsOn :: ByteString -> ByteString -> IO [ByteString]
tableColumnsOn table script = do
    let probe =
            script <> "\nSELECT '---';\nSELECT name FROM pragma_table_info('" <> table <> "') ORDER BY cid;\n"
    (code, out) <- runSQLiteBytes probe
    assertEqual "probe script executes" ExitSuccess code
    let (_, after) = BS.breakSubstring "---\n" out
    pure (BSC.lines (BS.drop 4 after))

-- | Live schema of `t` after running a script, via the database itself.
tableColumns :: ByteString -> IO [ByteString]
tableColumns = tableColumnsOn "t"

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
            , poolReportCount = Nothing
            }

reduceWith :: ColumnMode -> OraclePool -> ByteString -> IO ByteString
reduceWith mode =
    reduceSQLWithConfig
        ( (defaultReductionConfigFor SQLite)
            { reductionIntraStatement = True
            , reductionColumnMode = mode
            }
        )
        nullLogger

assertSchemaAndRowsOn :: ByteString -> ByteString -> [ByteString] -> ByteString -> IO ()
assertSchemaAndRowsOn table script expectedColumns expectedRows = do
    columns <- tableColumnsOn table script
    assertEqual "live schema" expectedColumns columns
    (code, out) <- runSQLiteBytes script
    assertEqual "script executes" ExitSuccess code
    assertEqual "rowset preserved" expectedRows out

assertSchemaAndRows :: ByteString -> [ByteString] -> ByteString -> IO ()
assertSchemaAndRows = assertSchemaAndRowsOn "t"

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

-- Single-site column without bundle drops via single-point move.
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

testAtomicAlterAddOther :: IO ()
testAtomicAlterAddOther = do
    -- `ADD COLUMN c` touches neither `a` nor `b`, so `b`'s bundle forms.
    -- The `ALTER` itself may or may not survive statement-level reduction.
    let script =
            BSC.unlines
                [ "CREATE TABLE t(a INT, b INT);"
                , "ALTER TABLE t ADD COLUMN c INT;"
                , "INSERT INTO t(a, b) VALUES (1, 10), (2, 20);"
                , "SELECT a FROM t ORDER BY a;"
                ]
    pool <- testPoolWith (sqliteOracle "1\n2\n")
    reduced <- reduceWith ColumnAtomic pool script
    columns <- tableColumns reduced
    assertBool "b dropped" ("b" `notElem` columns)
    assertBool "a kept" ("a" `elem` columns)
    (code, out) <- runSQLiteBytes reduced
    assertEqual "script executes" ExitSuccess code
    assertEqual "rowset preserved" "1\n2\n" out

testAtomicRenameColumnOther :: IO ()
testAtomicRenameColumnOther = do
    -- Renaming `c` to `d` touches neither `a` nor `b`, so `b`'s bundle forms.
    let script =
            BSC.unlines
                [ "CREATE TABLE t(a INT, b INT, c INT);"
                , "ALTER TABLE t RENAME COLUMN c TO d;"
                , "INSERT INTO t(a, b, d) VALUES (1, 10, 100), (2, 20, 200);"
                , "SELECT a FROM t ORDER BY a;"
                ]
    pool <- testPoolWith (sqliteOracle "1\n2\n")
    reduced <- reduceWith ColumnAtomic pool script
    assertSchemaAndRows reduced ["a", "d"] "1\n2\n"

testAtomicRenameFollow :: IO ()
testAtomicRenameFollow = do
    -- The bundle follows `t` across the rename: `b` drops from the `t`
    -- definition and the post-rename `u` inserts.
    let script =
            BSC.unlines
                [ "CREATE TABLE t(a INT, b INT);"
                , "ALTER TABLE t RENAME TO u;"
                , "INSERT INTO u(a, b) VALUES (1, 10), (2, 20);"
                , "SELECT a FROM u ORDER BY a;"
                ]
    pool <- testPoolWith (sqliteOracle "1\n2\n")
    reduced <- reduceWith ColumnAtomic pool script
    assertSchemaAndRowsOn "u" reduced ["a"] "1\n2\n"

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

testAlterColumnOtherKeepsBundle :: IO ()
testAlterColumnOtherKeepsBundle = do
    -- ALTER COLUMN a retains b's candidate bundle.
    let script =
            BSC.unlines
                [ "CREATE TABLE t(a INT, b INT);"
                , "ALTER TABLE t ALTER COLUMN a SET NOT NULL;"
                , "INSERT INTO t(a, b) VALUES (1, 10), (2, 20);"
                ]
    parsed <- parseSQL (base {parseColumnIR = True, parseColumnBundles = True}) script
    case parsed of
        Left err -> assertFailure ("parse failed: " <> show err)
        Right ok -> do
            let byColumn = [(targetColumn target, targetBlocked target) | target <- parsedColumns ok]
            assertEqual "b bundle survives" (Just Nothing) (lookup (Ident "b") byColumn)
            assertEqual
                "a bundle fenced"
                (Just (Just "column-generation-ambiguous"))
                (lookup (Ident "a") byColumn)
    where
        base = defaultParseOptions PostgreSQL

testAtomicTransactionOther :: IO ()
testAtomicTransactionOther = do
    -- A transaction block on an unrelated table does not block atomic column reduction on `t`.
    let script =
            BSC.unlines
                [ "BEGIN;"
                , "CREATE TABLE other(x INT);"
                , "INSERT INTO other VALUES (1);"
                , "COMMIT;"
                , "CREATE TABLE t(a INT, b INT);"
                , "INSERT INTO t(a, b) VALUES (1, 10), (2, 20);"
                , "SELECT a FROM t ORDER BY a;"
                ]
    pool <- testPoolWith (sqliteOracle "1\n2\n")
    reduced <- reduceWith ColumnAtomic pool script
    assertSchemaAndRows reduced ["a"] "1\n2\n"

testAtomicScopedBarrierOther :: IO ()
testAtomicScopedBarrierOther = do
    -- A scoped relation barrier (e.g. SQLite virtual table) on an unrelated table does not block atomic column reduction on `t`.
    let script =
            BSC.unlines
                [ "CREATE TABLE t(a INT, b INT);"
                , "INSERT INTO t(a, b) VALUES (1, 10), (2, 20);"
                , "SELECT a FROM t ORDER BY a;"
                , "CREATE VIRTUAL TABLE vt USING fts5(content);"
                ]
    pool <- testPoolWith (sqliteOracle "1\n2\n")
    reduced <- reduceWith ColumnAtomic pool script
    assertSchemaAndRows reduced ["a"] "1\n2\n"
