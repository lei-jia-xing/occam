module CrashEvidenceChecks (tests) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), decodeStrict', encode)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSLC
import Data.List (isSuffixOf, sort)
import Data.Text qualified as T
import Occam.Harness.Evidence (
    extractDbmsEvidence,
    hasPsqlMetaCommand,
 )
import Occam.Harness.Regex (compileRegex, matchExpectedSignature)
import Occam.Harness.Signature (
    BaselineRef (..),
    Corroboration (..),
    CrashCategory (..),
    CrashSignature (..),
    DbmsKind (..),
    EvidenceResult (..),
    SlippageMatchType (..),
    SlippageVerdict (..),
    buildCrashSignature,
    evaluateSlippage,
    normalizeSourcePath,
    normalizeVolatileTokens,
 )
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
    testGroup
        "Harness Regex, Evidence & Crash Signature Classifier"
        [ testCase "Regex engine matches all corpus patterns and falls back on invalid regex" do
            assertEqual
                "PG trap with escaped brackets and parens"
                (True, True)
                ( matchExpectedSignature
                    "TRAP:\\s+FailedAssertion\\(\"!\\(clauses != \\(\\(List \\*\\) \\(\\(void \\*\\)0\\)\\)\\)\", File: \"mcv\\.c\", Line: 155[0-9]\\)"
                    "TRAP: FailedAssertion(\"!(clauses != ((List *) ((void *)0)))\", File: \"mcv.c\", Line: 1553)"
                )
            assertEqual
                "Case-insensitive non-capturing alternation and DOTALL"
                (True, True)
                ( matchExpectedSignature
                    "(?:Received signal\\s+(?:11|Segmentation fault)|Segmentation fault|signal 11|SIGSEGV)"
                    "BaseDaemon: received signal\n  11"
                )
            assertEqual
                "MySQL optional clause with quantifiers"
                (True, True)
                ( matchExpectedSignature
                    "val_json.*Assertion `!current_thd->is_error\\(\\)(?: && has_value)?' failed"
                    "val_json: foo\nAssertion `!current_thd->is_error()' failed"
                )
            assertEqual
                "Invalid regex falls back cleanly to case-insensitive literal match"
                (True, False)
                (matchExpectedSignature "[" "Assertion failure: [x] failed")
            assertEqual
                "Invalid regex non-match returns False, False"
                (False, False)
                (matchExpectedSignature "[" "no bracket here")
        , testCase
            "PG13 rejects LOG statement echoes, tab continuations, and multiline CSV while catching TRAP/57P02"
            do
                let spoofCsv =
                        "2026-11-22 12:00:00.000 UTC,\"postgres\",\"occam_test\",101,\"\",6700.1,1,\"SELECT\",2026-11-22 12:00:00 UTC,2/1,0,LOG,00000,\"statement: SELECT 'TRAP: FailedAssertion(\"\"x\"\")\nFATAL: spoof';\",,,,,,,\"\"\n"
                    spoofLog =
                        T.unlines
                            [ "2026-11-22 12:00:00 UTC [101] LOG:  statement: SELECT 'spoof';"
                            , "\tTRAP: FailedAssertion(\"spoof\", File: \"evil.c\", Line: 1)"
                            , "2026-11-22 12:00:00 UTC [102] ERROR:  syntax error"
                            , "2026-11-22 12:00:00 UTC [102] STATEMENT:  SELECT 'PANIC: fake';"
                            , spoofCsv
                            ]
                    evSpoof = extractDbmsEvidence DbmsPostgres spoofLog "ERROR: syntax error" 0
                assertEqual "spoofed PG log emits zero faults" [] (erFaults evSpoof)

                let realLog =
                        T.unlines
                            [ "2026-11-22 12:00:00 UTC [100] TRAP: FailedAssertion(\"!(strcmp(a, \"b\") == 0)\", File: \"foo.c\", Line: 10, PID: 100)"
                            , "2026-11-22 12:00:00 UTC [1] LOG:  server process (PID 100) exited with exit code 2"
                            , "2026-11-22 12:00:00.000 UTC,\"postgres\",\"occam_test\",105,\"\",6700.1,1,\"\",2026-11-22 12:00:00 UTC,2/1,0,FATAL,57P02,\"terminating connection\",,,,,,,\"\""
                            ]
                    evReal = extractDbmsEvidence DbmsPostgres realLog "server closed the connection unexpectedly" 2
                assertBool "57P02 detected in CSV" (corSqlstate57p02 (erCorroboration evReal))
                assertBool "connection lost detected" (corConnectionLost (erCorroboration evReal))
                case buildCrashSignature DbmsPostgres (erFaults evReal) (erCandidateLines evReal) 2 5 of
                    Nothing -> assertFailure "Expected PG signature"
                    Just sig -> do
                        assertEqual "PG category" CatAssertion (csCategory sig)
                        assertEqual "PG file" (Just "foo.c") (csFile sig)
                        assertEqual "PG line" (Just 10) (csLine sig)
                        assertEqual "PG assertion keeps inner quotes" (Just "!(strcmp(a, \"b\") == 0)") (csAssertion sig)
        , testCase
            "MySQL/MariaDB exclude [Note]/[Warning] and Query dumps while parsing C++ backtraces and InnoDB asserts"
            do
                let warnAndDump =
                        T.unlines
                            [ "2026-11-22T12:00:00.123456Z 9 [Warning] [MY-010000] [Server] Unsafe statement:"
                            , "Assertion failure: spoof.cc:1:fake"
                            , "2026-11-22 12:00:00 10 [Note] Aborted connection 12 to db: 'occam_test'"
                            , "Query (7f123456): SELECT 'fake_assertion_spoof'"
                            , "  Assertion failure: spoof.cc:2:fake2"
                            , "Connection ID (thread ID): 10"
                            ]
                    evClean = extractDbmsEvidence DbmsMySql warnAndDump "" 0
                assertEqual "Warning and Query dump skipped" [] (erFaults evClean)

                let btLog =
                        T.unlines
                            [ "12:00:00 UTC - mysqld got signal 11 ;"
                            , "/usr/sbin/mysqld-debug(my_print_stacktrace(unsigned char const*, unsigned long)+0x43) [0x55a1b2c3d4e5]"
                            , "/usr/sbin/mysqld-debug(handle_fatal_signal+0x31a) [0x55a1b2345678]"
                            , "/lib/x86_64-linux-gnu/libpthread.so.0(+0x12980) [0x7f8b71234980]"
                            , "/usr/sbin/mysqld-debug(Item_func_rollup_const::val_str(String*)+0x52) [0x55a1b1234567]"
                            , "sql/sql_select.cc:4512(JOIN::optimize_inner())"
                            , "[entrypoint_target_mysql] mysqld-debug terminated (exit status: 139). Restarting..."
                            ]
                    evBt = extractDbmsEvidence DbmsMySql btLog "Lost connection to MySQL server during query" 1
                assertBool "MySQL connection lost" (corConnectionLost (erCorroboration evBt))
                assertEqual "supervisor exits" [139] (corSupervisorExitCodes (erCorroboration evBt))
                case buildCrashSignature DbmsMySql (erFaults evBt) (erCandidateLines evBt) 1 5 of
                    Nothing -> assertFailure "Expected MySQL signal signature"
                    Just sig -> do
                        assertEqual "signal category" CatSignal (csCategory sig)
                        assertEqual "SIGSEGV" (Just "SIGSEGV") (csSignal sig)
                        assertEqual
                            "wrapper frames stripped and MySQL bt normalized"
                            [ "Item_func_rollup_const::val_str(String*)"
                            , "JOIN::optimize_inner()@sql_select.cc:4512"
                            ]
                            (csFrames sig)

                let mariaAssertLog =
                        T.unlines
                            [ "2026-11-22 12:00:00 0 [ERROR] [FATAL] InnoDB: Assertion failure in thread 140 in file btr0cur.cc line 1234"
                            , "2026-11-22 12:00:00 0 [ERROR] [FATAL] InnoDB: Failing assertion: cursor->index"
                            , "mysqld terminated (exit status: 134)"
                            ]
                    evMaria = extractDbmsEvidence DbmsMariaDb mariaAssertLog "" 1
                case buildCrashSignature DbmsMariaDb (erFaults evMaria) (erCandidateLines evMaria) 1 5 of
                    Nothing -> assertFailure "Expected MariaDB assertion signature"
                    Just sig -> do
                        assertEqual "MariaDB assertion category" CatAssertion (csCategory sig)
                        assertEqual "InnoDB file" (Just "btr0cur.cc") (csFile sig)
                        assertEqual "InnoDB line" (Just 1234) (csLine sig)
        , testCase "ClickHouse strips multiline BaseDaemon query echoes and keeps fatal frames" do
            let chLog =
                    T.unlines
                        [ "2026-11-22 12:00:00.123000 [ 25819 ] {qid} <Error> executeQuery: Code: 62. Syntax error (spoof)"
                        , "SELECT 'Received signal 11 ASan heap-use-after-free'"
                        , "2026-11-22 12:00:00.123456 [ 25819 ] {} <Fatal> BaseDaemon: (version 26.8.44.3) (query: SELECT (1 + 2), 'fake_paren';"
                        , "SELECT 'fake_in_query') Received signal Segmentation fault (11)"
                        , "2026-11-22 12:00:00.123500 [ 25819 ] {} <Fatal> BaseDaemon: 0. ./build_docker/./src/Parsers/ParserQuery.cpp:42: DB::ParserQuery::parseImpl() @ 0x000000001234abcd"
                        , "2026-11-22 12:00:00.123510 [ 25819 ] {} <Fatal> BaseDaemon: 1. ./build_docker/./src/Interpreters/executeQuery.cpp:99: DB::executeQuery() @ 0x00000000deadbeef"
                        , "[entrypoint_target_clickhouse] clickhouse-server terminated (exit status: 139)."
                        ]
                evCh = extractDbmsEvidence DbmsClickHouse chLog "Connection reset by peer" 210
            assertBool
                "spoof token removed from candidate lines"
                (not (any ("fake_in_query" `T.isInfixOf`) (erCandidateLines evCh)))
            assertBool
                "fake paren removed from candidate lines"
                (not (any ("fake_paren" `T.isInfixOf`) (erCandidateLines evCh)))
            case buildCrashSignature DbmsClickHouse (erFaults evCh) (erCandidateLines evCh) 210 5 of
                Nothing -> assertFailure "Expected ClickHouse signature"
                Just sig -> do
                    assertEqual "SIGSEGV" (Just "SIGSEGV") (csSignal sig)
                    assertEqual
                        "ClickHouse frames keep src relative path"
                        [ "DB::ParserQuery::parseImpl()@src/Parsers/ParserQuery.cpp:42"
                        , "DB::executeQuery()@src/Interpreters/executeQuery.cpp:99"
                        ]
                        (csFrames sig)
        , testCase
            "SQLite strips error blocks and distinguishes SQLITE_ABORT vs ASan+ABRT assertion identity"
            do
                let spoofStderr =
                        T.unlines
                            [ "Parse error near line 1: near \"SELECT ==1==ERROR: AddressSanitizer: SEGV\n  sqlite3: x.c:1: f: Assertion `0' failed.\": syntax error"
                            , "  SELECT 1"
                            , "  ^--- error here"
                            ]
                    evSpoof = extractDbmsEvidence DbmsSqlite "" spoofStderr 1
                assertEqual "spoofed SQLite syntax error yields zero faults" [] (erFaults evSpoof)

                let abortEv = extractDbmsEvidence DbmsSqlite "" "Runtime error: abort due to ABORT" 4
                assertTrue "retcode 4 marks sqlite_abort" (corSqliteAbort (erCorroboration abortEv))
                assertEqual "sqlite_abort emits no faults" [] (erFaults abortEv)

                let asanStderr =
                        T.unlines
                            [ "==55==ERROR: AddressSanitizer: heap-use-after-free on address 0x6020"
                            , "    #0 0x7f11 in __asan_report_load4 (/usr/lib/libasan.so.8+0x1234) (BuildId: deadbeef)"
                            , "    #1 0x5511 in std::vector::at /usr/include/c++/13/bits/vector.tcc:123:5"
                            , "    #2 0x5522 in sqlite3Step (/usr/local/bin/sqlite3+0x4444) (BuildId: cafebabe)"
                            ]
                    evAsan = extractDbmsEvidence DbmsSqlite "" asanStderr 134
                case buildCrashSignature DbmsSqlite (erFaults evAsan) (erCandidateLines evAsan) 134 5 of
                    Nothing -> assertFailure "Expected SQLite ASan signature"
                    Just sig -> do
                        assertEqual "ASan category" CatAsan (csCategory sig)
                        assertEqual
                            "BuildId stripped"
                            ["std::vector::at@vector.tcc:123", "sqlite3Step@sqlite3+0x4444"]
                            (csFrames sig)

                let asanWithAssert =
                        T.unlines
                            [ "sqlite3: vdbe.c:99: sqlite3VdbeExec: Assertion `pOp!=0' failed."
                            , "==56==ERROR: AddressSanitizer: ABRT on unknown address"
                            , "    #0 0x1111 in sqlite3VdbeExec vdbe.c:99:1"
                            ]
                    evAsanAssert = extractDbmsEvidence DbmsSqlite "" asanWithAssert 134
                case buildCrashSignature DbmsSqlite (erFaults evAsanAssert) (erCandidateLines evAsanAssert) 134 5 of
                    Nothing -> assertFailure "Expected assertion signature"
                    Just sig ->
                        assertEqual "ASan ABRT after assertion keeps assertion category" CatAssertion (csCategory sig)
        , testCase "DuckDB nonfatal errors never swallow trailing INTERNAL Error or OOM/SIGKILL" do
            let duckStderr =
                    T.unlines
                        [ "Catalog Error: Table with name \"fake; INTERNAL Error: spoof\""
                        , "does not exist!"
                        , "LINE 1: SELECT 'Assertion failed: (false), function f, file a.cpp, line 1';"
                        , "                                                                        ^"
                        , "INTERNAL Error: Assertion triggered in file \"src/foo.cpp\" on line 42: x == 0xdeadbeef"
                        ]
                evDuck = extractDbmsEvidence DbmsDuckDb "" duckStderr 1
            case buildCrashSignature DbmsDuckDb (erFaults evDuck) (erCandidateLines evDuck) 1 5 of
                Nothing -> assertFailure "Expected DuckDB INTERNAL Error signature"
                Just sig -> do
                    assertEqual "DuckDB internal error category" CatInternalError (csCategory sig)
                    assertEqual
                        "volatile hex normalized"
                        (Just "Assertion triggered in file \"src/foo.cpp\" on line 42: x == 0xADDR")
                        (csAssertion sig)

            let oomEv = extractDbmsEvidence DbmsDuckDb "" "Out of Memory Error: failed to pin block" 1
            assertTrue "DuckDB OOM detected" (corOom (erCorroboration oomEv))
            assertEqual "DuckDB OOM cause" (Just "duckdb_oom_error") (corOomCause (erCorroboration oomEv))

            let sigKillEv = extractDbmsEvidence DbmsDuckDb "" "" 137
            assertTrue "SIGKILL external kill" (corExternalKill (erCorroboration sigKillEv))
            assertTrue "SIGKILL marked OOM" (corOom (erCorroboration sigKillEv))
        , testCase "Volatile token normalization, path normalization, psql meta check, and slippage" do
            assertEqual
                "ASLR/UUID/PID scrubbed"
                "ptr=0xADDR uuid=<UUID> PID <N>"
                (normalizeVolatileTokens "ptr=0x7fff1234abc uuid=404ee645-9173-4286-9afb-93d4dec49793 PID   42")
            assertEqual
                "keep deep src path"
                (Just "src/Core/Block.cpp")
                (normalizeSourcePath True "./build/src/Core/Block.cpp")
            assertEqual
                "basename shallow sql path"
                (Just "sql_select.cc")
                (normalizeSourcePath True "sql/sql_select.cc")
            assertTrue "psql backslash detected" (hasPsqlMetaCommand "SELECT 1;\n  \\! rm -rf /\n")
            assertBool "ordinary SQL clean" (not (hasPsqlMetaCommand "SELECT '\\n';"))

            let baseSig =
                    CrashSignature
                        { csId = "assertion:a.c:10:x"
                        , csHash = "0123456789abcdef"
                        , csCategory = CatAssertion
                        , csSignal = Just "SIGABRT"
                        , csSignalNum = Just 6
                        , csAssertion = Just "x"
                        , csAsanError = Nothing
                        , csFile = Just "a.c"
                        , csLine = Just 10
                        , csFunction = Nothing
                        , csExitCode = Nothing
                        , csFrames = []
                        , csTopFrames = 5
                        }
                otherSig = baseSig {csId = "signal:SIGSEGV", csHash = "fedcba9876543210", csCategory = CatSignal}
                sameIdSlip =
                    evaluateSlippage
                        (BaselineRef "assertion:a.c:10:x" (Just "assertion:a.c:10:x") Nothing (Just "assertion"))
                        baseSig
                diffSlip =
                    evaluateSlippage
                        ( BaselineRef
                            "0123456789abcdef"
                            (Just "0123456789abcdef")
                            (Just "0123456789abcdef")
                            (Just "assertion")
                        )
                        otherSig
            assertEqual "exact id match" (False, MatchExactId) (svSlipped sameIdSlip, svMatchType sameIdSlip)
            assertEqual
                "slippage mismatch detected"
                (True, MatchMismatch, Just True)
                (svSlipped diffSlip, svMatchType diffSlip, svCategoryChanged diffSlip)
            assertBool "CrashSignature JSON encodes" (not (BSLC.null (encode baseSig)))
        , testCase "Every expected_signature in real_fuzz_corpus compiles cleanly under Thompson NFA" do
            let corpusDir = "benchmarks/real_fuzz_corpus"
            hasCorpus <- doesDirectoryExist corpusDir
            assertBool "benchmarks/real_fuzz_corpus directory present" hasCorpus
            metaFiles <- sort . filter (".meta.json" `isSuffixOf`) <$> listDirectory corpusDir
            assertBool "corpus metadata files present" (not (null metaFiles))
            forM_ metaFiles \fname -> do
                raw <- BS.readFile (corpusDir </> fname)
                case decodeStrict' raw of
                    Just (Object obj)
                        | Just (Object sigObj) <- KM.lookup "crash_signature" obj
                        , Just (String pat) <- KM.lookup "expected_signature" sigObj ->
                            case compileRegex pat of
                                Right _ -> pure ()
                                Left err ->
                                    assertFailure (fname <> ": failed to compile expected_signature " <> show pat <> ": " <> err)
                    _ -> pure ()
        ]
    where
        assertTrue = assertBool
