module OracleAndResetChecks (tests) where

import Control.Exception (bracket, try)
import Control.Exception.Safe (SomeException)
import Control.Monad (forM_, when)
import Data.Aeson (Object, Value (..), eitherDecodeStrict')
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.Char (isSpace)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Occam.Harness.DbmsExec (decodeExitCode)
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    findExecutable,
    getCurrentDirectory,
    getTemporaryDirectory,
    removeDirectoryRecursive,
 )
import System.Environment (getEnvironment, getExecutablePath, lookupEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (joinPath, splitDirectories, takeDirectory, (</>))
import System.Posix.Temp (mkdtemp)
import System.Process (
    CreateProcess (..),
    env,
    proc,
    readCreateProcessWithExitCode,
    readProcess,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
    testGroup
        "Harness Reset & Crash Oracle"
        [ testCase "reset postgres uses PGHOST, sets PGPASSWORD, and avoids SQL interpolation" do
            harnessBin <- findOccamHarnessBin
            withTempDir "occam-reset-pg" \td -> do
                let argsFile = td </> "args.txt"
                    sqlFile = td </> "sql.txt"
                    pwdFile = td </> "pwd.txt"
                _ <- writeMock td "pg_isready" "exit 0\n"
                _ <-
                    writeMock
                        td
                        "psql"
                        ( unlines
                            [ "printf '%s\\n' \"$@\" > " <> sq argsFile
                            , "cat > " <> sq sqlFile
                            , "printf '%s' \"$PGPASSWORD\" > " <> sq pwdFile
                            , "exit 0"
                            ]
                        )
                cEnv <- cleanEnv td [("PGHOST", "pg-host"), ("MYSQL_HOST", "mysql-host")]
                (rc, _, err) <-
                    runHarness
                        harnessBin
                        ["reset", "--dbms", "postgres", "--password", "secret"]
                        BS.empty
                        cEnv
                assertEqual ("pg reset ok: " <> T.unpack err) 0 rc
                argv <- lines <$> readFile argsFile
                assertBool "uses PGHOST" ("pg-host" `elem` argv)
                assertBool "ignores MYSQL_HOST" ("mysql-host" `notElem` argv)
                pwd <- readFile pwdFile
                assertEqual "exports PGPASSWORD" "secret" pwd
                sql <- readFile sqlFile
                assertBool "drops via psql var" ("DROP DATABASE IF EXISTS :\"dbname\";" `isInfixOf` sql)
                assertBool
                    "creates via psql var"
                    ("CREATE DATABASE :\"dbname\" TEMPLATE :\"template\";" `isInfixOf` sql)

                let dangerous = "occam_test\"; DROP DATABASE postgres; --"
                (rc2, _, err2) <-
                    runHarness
                        harnessBin
                        ["reset", "--dbms", "postgres", "--dbname", dangerous]
                        BS.empty
                        cEnv
                assertEqual ("pg dangerous dbname ok: " <> T.unpack err2) 0 rc2
                argv2 <- lines <$> readFile argsFile
                assertBool "passes dangerous dbname via -v" (any (dangerous `isInfixOf`) argv2)
                sql2 <- readFile sqlFile
                assertBool "does not interpolate dangerous dbname into SQL" (not (dangerous `isInfixOf` sql2))
        , testCase "reset mysql, mariadb, and clickhouse validate identifiers and isolate env defaults" do
            harnessBin <- findOccamHarnessBin
            withTempDir "occam-reset-srv" \td -> do
                let argsFile = td </> "args.txt"
                    sqlFile = td </> "sql.txt"
                    pwdFile = td </> "pwd.txt"
                    calledFlag = td </> "called"
                _ <- writeMock td "mysqladmin" "exit 0\n"
                _ <-
                    writeMock
                        td
                        "mysql"
                        ( unlines
                            [ "printf '%s\\n' \"$@\" > " <> sq argsFile
                            , "cat > " <> sq sqlFile
                            , "printf '%s' \"$MYSQL_PWD\" > " <> sq pwdFile
                            , "exit 0"
                            ]
                        )
                envMy <- cleanEnv td [("PGHOST", "pg-host"), ("MYSQL_HOST", "mysql-host")]
                (rcMy, _, errMy) <-
                    runHarness
                        harnessBin
                        ["reset", "--dbms", "mysql", "--password", "secret"]
                        BS.empty
                        envMy
                assertEqual ("mysql reset ok: " <> T.unpack errMy) 0 rcMy
                myArgs <- lines <$> readFile argsFile
                assertBool "mysql host used" ("mysql-host" `elem` myArgs)
                assertBool "pg host ignored" ("pg-host" `notElem` myArgs)
                assertBool "no-defaults passed" ("--no-defaults" `elem` myArgs)
                assertEqual "MYSQL_PWD set" "secret" =<< readFile pwdFile
                mySql <- readFile sqlFile
                assertBool "clears super_read_only" ("SET GLOBAL super_read_only = OFF;" `isInfixOf` mySql)
                assertBool "clears read_only" ("SET GLOBAL read_only = OFF;" `isInfixOf` mySql)
                assertBool "drops occam_test" ("DROP DATABASE IF EXISTS `occam_test`;" `isInfixOf` mySql)

                (rcSock, _, errSock) <-
                    runHarness
                        harnessBin
                        [ "reset"
                        , "--dbms"
                        , "mysql"
                        , "--host"
                        , "ignored-host"
                        , "--port"
                        , "9999"
                        , "--socket"
                        , "/tmp/mysql.sock"
                        ]
                        BS.empty
                        envMy
                assertEqual ("mysql socket reset ok: " <> T.unpack errSock) 0 rcSock
                sockArgs <- lines <$> readFile argsFile
                assertBool "--socket passed" ("--socket" `elem` sockArgs && "/tmp/mysql.sock" `elem` sockArgs)
                assertBool "-h omitted with socket" ("-h" `notElem` sockArgs)
                assertBool "port omitted with socket" ("9999" `notElem` sockArgs)

                _ <- writeMock td "mariadb-admin" "exit 0\n"
                _ <-
                    writeMock
                        td
                        "mariadb"
                        ( unlines
                            [ "printf '%s\\n' \"$@\" > " <> sq argsFile
                            , "cat > " <> sq sqlFile
                            , "printf '%s' \"$MYSQL_PWD\" > " <> sq pwdFile
                            , "exit 0"
                            ]
                        )
                envMaria <-
                    cleanEnv
                        td
                        [("MYSQL_HOST", "mysql-host"), ("MARIADB_HOST", "maria-host"), ("MARIADB_DATABASE", "maria_db")]
                (rcMaria, _, errMaria) <-
                    runHarness harnessBin ["reset", "--dbms", "mariadb", "--password", "msecret"] BS.empty envMaria
                assertEqual ("mariadb reset ok: " <> T.unpack errMaria) 0 rcMaria
                mariaArgs <- lines <$> readFile argsFile
                assertBool "MARIADB_HOST wins" ("maria-host" `elem` mariaArgs && "mysql-host" `notElem` mariaArgs)
                mariaSql <- readFile sqlFile
                assertBool "mariadb clears read_only" ("SET GLOBAL read_only = OFF;" `isInfixOf` mariaSql)
                assertBool "mariadb omits super_read_only" (not ("super_read_only" `isInfixOf` mariaSql))
                assertBool "mariadb uses M_DB" ("DROP DATABASE IF EXISTS `maria_db`;" `isInfixOf` mariaSql)

                _ <-
                    writeMock
                        td
                        "clickhouse-client"
                        ( unlines
                            [ "if echo \"$*\" | grep -q -- '--query'; then exit 0; fi"
                            , "printf '%s\\n' \"$@\" > " <> sq argsFile
                            , "cat > " <> sq sqlFile
                            , "printf '%s' \"$CLICKHOUSE_PASSWORD\" > " <> sq pwdFile
                            , "exit 0"
                            ]
                        )
                envCh <-
                    cleanEnv
                        td
                        [ ("PGHOST", "pg-host")
                        , ("CLICKHOUSE_HOST", "ch-host")
                        , ("CLICKHOUSE_PORT", "9001")
                        , ("CLICKHOUSE_DATABASE", "ch_test")
                        ]
                (rcCh, _, errCh) <-
                    runHarness harnessBin ["reset", "--dbms", "clickhouse", "--password", "chsecret"] BS.empty envCh
                assertEqual ("clickhouse reset ok: " <> T.unpack errCh) 0 rcCh
                chArgs <- lines <$> readFile argsFile
                assertBool "multiquery passed" ("--multiquery" `elem` chArgs)
                assertBool "ch host and port" ("ch-host" `elem` chArgs && "9001" `elem` chArgs)
                assertBool "password never in argv" ("--password" `notElem` chArgs && "chsecret" `notElem` chArgs)
                assertEqual "CLICKHOUSE_PASSWORD exported" "chsecret" =<< readFile pwdFile
                chSql <- readFile sqlFile
                assertBool "SYNC drop used" ("DROP DATABASE IF EXISTS `ch_test` SYNC;" `isInfixOf` chSql)

                -- Unsafe DB names must fail without invoking client tools
                _ <- writeMock td "mysqladmin" ("touch " <> sq calledFlag <> "\nexit 0\n")
                _ <- writeMock td "mysql" ("touch " <> sq calledFlag <> "\nexit 0\n")
                (rcBad, _, errBad) <-
                    runHarness harnessBin ["reset", "--dbms", "mysql", "--dbname", "bad;name"] BS.empty envMy
                assertBool "unsafe mysql dbname fails" (rcBad /= 0)
                assertBool "mentions must match" ("must match" `T.isInfixOf` errBad)
                assertBool "did not touch called" . not =<< doesFileExist calledFlag
        , testCase
            "reset embedded local no-ops and container mode restarts stopped containers and reports timeouts"
            do
                harnessBin <- findOccamHarnessBin
                withTempDir "occam-reset-emb" \td -> do
                    cEnv <- cleanEnv td []
                    forM_ ["sqlite", "duckdb"] \d -> do
                        (rc, _, err) <- runHarness harnessBin ["reset", "--dbms", d] BS.empty cEnv
                        assertEqual ("stateless local reset " <> d <> ": " <> T.unpack err) 0 rc

                    let startedFlag = td </> "started"
                        probeSql = td </> "probe.sql"
                    _ <-
                        writeMock
                            td
                            "docker"
                            ( unlines
                                [ "if [ \"$1\" = \"ps\" ]; then"
                                , "  if [ -f " <> sq startedFlag <> " ]; then echo \"occam-target-duckdb\"; fi"
                                , "  exit 0"
                                , "fi"
                                , "if [ \"$1\" = \"start\" ] && [ \"$2\" = \"occam-target-duckdb\" ]; then"
                                , "  : > " <> sq startedFlag
                                , "  exit 0"
                                , "fi"
                                , "if [ \"$1\" = \"exec\" ] && [ \"$2\" = \"-i\" ] && [ \"$3\" = \"occam-target-duckdb\" ] && [ \"$4\" = \"occam-exec\" ]; then"
                                , "  cat > " <> sq probeSql
                                , "  exit 0"
                                , "fi"
                                , "exit 1"
                                ]
                            )
                    (rcDock, _, errDock) <-
                        runHarness
                            harnessBin
                            ["reset", "--dbms", "duckdb", "--container-name", "occam-target-duckdb", "--max-wait", "2"]
                            BS.empty
                            cEnv
                    assertEqual ("duckdb container reset ok: " <> T.unpack errDock) 0 rcDock
                    assertBool "container started" =<< doesFileExist startedFlag
                    assertEqual "probe SQL SELECT 1;" "SELECT 1;" . trimStr =<< readFile probeSql

                    _ <- writeMock td "docker" "exit 1\n"
                    (rcDead, _, errDead) <-
                        runHarness
                            harnessBin
                            ["reset", "--dbms", "sqlite", "--container-name", "dead-sqlite", "--max-wait", "1"]
                            BS.empty
                            cEnv
                    assertBool "dead container fails" (rcDead /= 0)
                    assertBool "reports not ready" ("not ready" `T.isInfixOf` errDead)
        , testCase
            "oracle rejects empty SQL, missing logs, stderr spoofs, and enforces psql meta-command guard"
            do
                harnessBin <- findOccamHarnessBin
                withTempDir "occam-oracle-guards" \td -> do
                    let logPath = td </> "server.log"
                        decisions = td </> "decisions.jsonl"
                        workdir = td </> "work"
                        cwdFile = td </> "cwd.txt"
                    createDirectoryIfMissing True workdir
                    writeFile logPath "Log started\n"
                    cEnv <- cleanEnv td []

                    (rcEmpty, _, _) <- runHarness harnessBin ["oracle", "--log-file", logPath] "   \n\t " cEnv
                    assertEqual "whitespace SQL rejected" 1 rcEmpty

                    (rcMissing, _, _) <- runHarness harnessBin ["oracle", "--decision-log", decisions] "SELECT 1;" cEnv
                    assertEqual "missing server log rejected" 1 rcMissing
                    recsMissing <- readJsonl decisions
                    case recsMissing of
                        [r0] -> do
                            assertEqual "missing_evidence" "missing_evidence" (kmStr "classification" r0)
                            assertEqual "log-source-required" "log-source-required" (kmStr "reason" r0)
                        _ -> assertFailure "Expected 1 decision record for missing_evidence"

                    _ <-
                        writeMock
                            td
                            "psql"
                            ( unlines
                                [ "cat >/dev/null || true"
                                , "pwd > " <> sq cwdFile
                                , "echo 'TRAP: FailedAssertion(\"!(x)\", File: \"subselect.c\", Line: 1)' >&2"
                                , "echo 'server closed the connection unexpectedly' >&2"
                                , "exit 2"
                                ]
                            )
                    cEnvWd <- cleanEnv td [("OCCAM_WORKDIR", workdir)]
                    (rcStderrOnly, _, _) <-
                        runHarness
                            harnessBin
                            [ "oracle"
                            , "--log-file"
                            , logPath
                            , "--expected-signature"
                            , "subselect.c"
                            , "--decision-log"
                            , decisions
                            ]
                            "SELECT 1;"
                            cEnvWd
                    assertEqual "stderr-only signature rejected" 1 rcStderrOnly
                    assertEqual "workdir used as cwd" workdir . trimStr =<< readFile cwdFile

                    _ <- writeMock td "psql" "cat >/dev/null || true\nexit 0\n"
                    (rcMeta, _, _) <-
                        runHarness
                            harnessBin
                            ["oracle", "--log-file", logPath, "--decision-log", decisions]
                            "\\! echo unsafe\n"
                            cEnv
                    assertEqual "psql meta rejected" 1 rcMeta
                    recs <- readJsonl decisions
                    case reverse recs of
                        (lastRec : _) -> assertEqual "reason psql-meta-command" "psql-meta-command" (kmStr "reason" lastRec)
                        [] -> assertFailure "Expected decision record for psql-meta-command"
        , testCase
            "oracle handles log flush retries, rotation, timeouts, PG 57P02, MySQL flags, and RQ3 slippage"
            do
                harnessBin <- findOccamHarnessBin
                withTempDir "occam-oracle-srv" \td -> do
                    let logPath = td </> "server.log"
                        decisions = td </> "decisions.jsonl"
                        baselineFile = td </> "baseline.json"
                    writeFile logPath "Log started\n"
                    cEnv <- cleanEnv td []

                    -- Delayed log flush + PG 57P02 + postmaster child PID
                    _ <-
                        writeMock
                            td
                            "psql"
                            ( unlines
                                [ "cat >/dev/null || true"
                                , "sleep 0.25"
                                , "printf '2026-11-22 12:00:00 UTC [100] TRAP: FailedAssertion(\"!(clauses != NIL)\", File: \"mcv.c\", Line: 1553, PID: 100)\\n' >> "
                                    <> sq logPath
                                , "printf '2026-11-22 12:00:00.123 UTC,\"postgres\",\"occam_test\",1234,\"\",673f,1,\"SELECT\",2026-11-22 12:00:00 UTC,3/1,0,FATAL,57P02,\"terminating connection\",,,,,,,,,\"psql\"\\n' >> "
                                    <> sq logPath
                                , "printf '2026-11-22 12:00:00.124 UTC [1] LOG:  server process (PID 1230) was terminated by signal 11: Segmentation fault\\n' >> "
                                    <> sq logPath
                                , "echo 'server closed the connection unexpectedly' >&2"
                                , "exit 2"
                                ]
                            )
                    (rcFlush, outFlush, errFlush) <-
                        runHarness
                            harnessBin
                            [ "oracle"
                            , "--dbms"
                            , "postgres"
                            , "--log-file"
                            , logPath
                            , "--expected-signature"
                            , "mcv\\.c"
                            , "--print-signature"
                            , "--decision-log"
                            , decisions
                            ]
                            "SELECT 1;"
                            cEnv
                    assertEqual ("delayed log accepted: " <> T.unpack errFlush) 0 rcFlush
                    writeFile baselineFile (T.unpack outFlush)

                    -- Log truncation / rotation
                    writeFile logPath (concat (replicate 10 "old log bytes\n"))
                    _ <-
                        writeMock
                            td
                            "psql"
                            ( unlines
                                [ "cat >/dev/null || true"
                                , ": > " <> sq logPath
                                , "echo 'TRAP: FailedAssertion(\"!(y)\", File: \"subselect.c\", Line: 2883)' >> "
                                    <> sq logPath
                                , "exit 2"
                                ]
                            )
                    (rcSlipMeas, _, _) <-
                        runHarness
                            harnessBin
                            [ "oracle"
                            , "--log-file"
                            , logPath
                            , "--baseline-signature"
                            , baselineFile
                            , "--decision-log"
                            , decisions
                            ]
                            "SELECT 1;"
                            cEnv
                    assertEqual "rotated log accepted with measurement-only slippage" 0 rcSlipMeas

                    -- Appending the slipped crash again under --reject-on-slippage must reject with signature-slippage
                    _ <-
                        writeMock
                            td
                            "psql"
                            ( unlines
                                [ "cat >/dev/null || true"
                                , "echo 'TRAP: FailedAssertion(\"!(y)\", File: \"subselect.c\", Line: 2883)' >> "
                                    <> sq logPath
                                , "exit 2"
                                ]
                            )
                    (rcSlipRej, _, _) <-
                        runHarness
                            harnessBin
                            [ "oracle"
                            , "--log-file"
                            , logPath
                            , "--baseline-signature"
                            , baselineFile
                            , "--reject-on-slippage"
                            , "--decision-log"
                            , decisions
                            ]
                            "SELECT 1;"
                            cEnv
                    assertEqual "slipped signature rejected under --reject-on-slippage" 1 rcSlipRej

                    -- Client timeout exit 124
                    _ <- writeMock td "psql" "cat >/dev/null || true\nexec sleep 5\n"
                    (rcTmout, _, _) <-
                        runHarness
                            harnessBin
                            ["oracle", "--log-file", logPath, "--timeout", "0.3", "--decision-log", decisions]
                            "SELECT pg_sleep(10);"
                            cEnv
                    assertEqual "explicit client timeout returns 124" 124 rcTmout

                    recs <- readJsonl decisions
                    case recs of
                        (firstRec : _ : slipRec : _) -> do
                            assertEqual "first confirmed_fault" "confirmed_fault" (kmStr "classification" firstRec)
                            assertEqual "slippage rejected reason" "signature-slippage" (kmStr "reason" slipRec)
                        _ -> assertFailure ("Expected at least 3 decision records, got " <> show (length recs))
        , testCase
            "oracle covers embedded container occam-exec, container server log mode, infra 127, OOM/SIGKILL, and MySQL"
            do
                harnessBin <- findOccamHarnessBin
                withTempDir "occam-oracle-emb" \td -> do
                    let decisions = td </> "decisions.jsonl"
                        argsFile = td </> "args.txt"
                        pwdFile = td </> "pwd.txt"
                        logPath = td </> "mysql.log"
                    writeFile logPath "Log started\n"
                    cEnv <- cleanEnv td []

                    -- SQLite container occam-exec SIGSEGV and infra 127
                    _ <-
                        writeMock
                            td
                            "docker"
                            ( unlines
                                [ "cat >/dev/null || true"
                                , "printf '%s\\n' \"$@\" > " <> sq argsFile
                                , "exit 139"
                                ]
                            )
                    (rcCont, _, errCont) <-
                        runHarness
                            harnessBin
                            [ "oracle"
                            , "--dbms"
                            , "sqlite"
                            , "--container-name"
                            , "occam-target-sqlite"
                            , "--decision-log"
                            , decisions
                            ]
                            "SELECT 1;"
                            cEnv
                    assertEqual ("sqlite container crash: " <> T.unpack errCont) 0 rcCont
                    assertEqual
                        "docker exec args"
                        ["exec", "-i", "occam-target-sqlite", "occam-exec"]
                        . lines
                        =<< readFile argsFile

                    _ <- writeMock td "docker" "cat >/dev/null || true\nexit 127\n"
                    (rcInfra, _, _) <-
                        runHarness
                            harnessBin
                            [ "oracle"
                            , "--dbms"
                            , "sqlite"
                            , "--container-name"
                            , "occam-target-sqlite"
                            , "--decision-log"
                            , decisions
                            ]
                            "SELECT 1;"
                            cEnv
                    assertEqual "infra 127 rejected" 1 rcInfra

                    -- DuckDB SIGKILL 137 -> oom-kill
                    _ <- writeMock td "duckdb" "cat >/dev/null || true\nexit 137\n"
                    (rcKill, _, _) <-
                        runHarness
                            harnessBin
                            ["oracle", "--dbms", "duckdb", "--decision-log", decisions]
                            "SELECT range(100000000);"
                            cEnv
                    assertEqual "SIGKILL rejected as oom-kill" 1 rcKill

                    -- MySQL client hardening flags
                    _ <-
                        writeMock
                            td
                            "mysql"
                            ( unlines
                                [ "cat >/dev/null || true"
                                , "printf '%s\\n' \"$@\" > " <> sq argsFile
                                , "printf '%s' \"$MYSQL_PWD\" > " <> sq pwdFile
                                , "echo '2026-11-22T12:00:00.123456Z 0 [ERROR] [MY-013183] [InnoDB] Assertion failure: btr0cur.cc:1234:cursor->index' >> "
                                    <> sq logPath
                                , "exit 1"
                                ]
                            )
                    (rcMy, _, errMy) <-
                        runHarness
                            harnessBin
                            [ "oracle"
                            , "--dbms"
                            , "mysql"
                            , "--log-file"
                            , logPath
                            , "--password"
                            , "secret"
                            , "--expected-signature"
                            , "MY-013183"
                            , "--decision-log"
                            , decisions
                            ]
                            "SELECT 1;"
                            cEnv
                    assertEqual ("mysql oracle match: " <> T.unpack errMy) 0 rcMy
                    myArgs <- lines <$> readFile argsFile
                    assertBool "mysql --binary-mode" ("--binary-mode" `elem` myArgs)
                    assertBool "mysql --force" ("--force" `elem` myArgs)
                    assertBool "no -psecret" ("-psecret" `notElem` myArgs)
                    assertEqual "MYSQL_PWD passed" "secret" =<< readFile pwdFile

                    -- Container server log mode (docker exec stat/tail + docker logs fallback)
                    let srvLog = td </> "srv_cont.log"
                    writeFile srvLog "initial\n"
                    _ <-
                        writeMock
                            td
                            "docker"
                            ( unlines
                                [ "if [ \"$1\" = \"exec\" ] && [ \"$3\" = \"stat\" ]; then"
                                , "  printf '%s 999\\n' \"$(wc -c < " <> sq srvLog <> " | tr -d ' ')\""
                                , "  exit 0"
                                , "fi"
                                , "if [ \"$1\" = \"exec\" ] && [ \"$3\" = \"tail\" ]; then"
                                , "  tail -c \"$5\" " <> sq srvLog
                                , "  exit 0"
                                , "fi"
                                , "if [ \"$1\" = \"logs\" ]; then exit 0; fi"
                                , "exit 1"
                                ]
                            )
                    _ <-
                        writeMock
                            td
                            "psql"
                            ( unlines
                                [ "cat >/dev/null || true"
                                , "echo '2026-11-22 12:00:00 UTC [10] PANIC:  cache lookup failed' >> " <> sq srvLog
                                , "exit 2"
                                ]
                            )
                    (rcContSrv, _, errContSrv) <-
                        runHarness
                            harnessBin
                            [ "oracle"
                            , "--dbms"
                            , "postgres"
                            , "--container-name"
                            , "pg-ctr"
                            , "--decision-log"
                            , decisions
                            ]
                            "SELECT 1;"
                            cEnv
                    assertEqual ("container log mode accepted: " <> T.unpack errContSrv) 0 rcContSrv

                    -- Invalid --timeout or --stack-frames fails cleanly
                    (rcBadFrames, _, _) <-
                        runHarness harnessBin ["oracle", "--stack-frames", "0"] "SELECT 1;" cEnv
                    assertEqual "invalid --stack-frames exits 2" 2 rcBadFrames

                    recs <- readJsonl decisions
                    case recs of
                        (_ : r1 : r2 : _) -> do
                            assertEqual "infra classification" "missing_evidence" (kmStr "classification" r1)
                            assertEqual "oom-kill reason" "oom-kill" (kmStr "reason" r2)
                        _ -> assertFailure ("Expected at least 3 decision records, got " <> show (length recs))
        ]

findOccamHarnessBin :: IO FilePath
findOccamHarnessBin = do
    mEnv <- lookupEnv "OCCAM_HARNESS_BIN"
    case mEnv of
        Just p | not (null p) -> doesFileExist p >>= \ok -> if ok then pure p else searchSibling
        _ -> searchSibling
    where
        searchSibling = do
            exe <- getExecutablePath
            let dirs = splitDirectories exe
                mNix = takeDirectory (takeDirectory (takeDirectory exe)) </> "occam-harness" </> "occam-harness"
                cabalCandidate = joinPath (replaceSubDirs dirs)
            nixOk <- doesFileExist mNix
            if nixOk
                then pure mNix
                else do
                    cabalOk <- doesFileExist cabalCandidate
                    if cabalOk
                        then pure cabalCandidate
                        else do
                            mPath <- findExecutable "occam-harness"
                            case mPath of
                                Just found -> pure found
                                Nothing -> do
                                    res <-
                                        try (readProcess "cabal" ["list-bin", "exe:occam-harness"] "") :: IO (Either SomeException String)
                                    case res of
                                        Right out -> do
                                            let p = trimStr out
                                            ok <- doesFileExist p
                                            if ok then pure p else assertFailure "occam-harness binary not found"
                                        Left _ -> assertFailure "occam-harness binary not found"

        replaceSubDirs [] = []
        replaceSubDirs ("t" : "occam-test" : rest) = "x" : "occam-harness" : replaceSubDirs rest
        replaceSubDirs ["occam-test", "occam-test"] = ["occam-harness", "occam-harness"]
        replaceSubDirs (x : xs) = x : replaceSubDirs xs

cleanEnv :: FilePath -> [(String, String)] -> IO [(String, String)]
cleanEnv mockDir extras = do
    base <- Map.fromList <$> getEnvironment
    cwd <- getCurrentDirectory
    let scrubbed = foldr Map.delete base envKeysToStrip
        origPath = fromMaybe "" (Map.lookup "PATH" base)
        harnessJson = cwd </> "config" </> "generated" </> "harness.json"
    hasCfg <- doesFileExist harnessJson
    let withPath = Map.insert "PATH" (mockDir <> ":" <> origPath) scrubbed
        withCfg =
            if hasCfg
                then Map.insertWith (\_ old -> old) "OCCAM_HARNESS_JSON" harnessJson withPath
                else withPath
        finalEnv = foldr (uncurry Map.insert) withCfg extras
    pure (Map.toList finalEnv)
    where
        envKeysToStrip =
            [ "DBMS"
            , "PGHOST"
            , "PGPORT"
            , "PGUSER"
            , "PGPASSWORD"
            , "PGDATABASE"
            , "MYSQL_HOST"
            , "MYSQL_TCP_PORT"
            , "MYSQL_USER"
            , "MYSQL_PWD"
            , "MYSQL_DATABASE"
            , "MYSQL_SOCKET"
            , "MARIADB_HOST"
            , "MARIADB_TCP_PORT"
            , "MARIADB_USER"
            , "MARIADB_PWD"
            , "MARIADB_DATABASE"
            , "MARIADB_UNIX_PORT"
            , "CLICKHOUSE_HOST"
            , "CLICKHOUSE_PORT"
            , "CLICKHOUSE_USER"
            , "CLICKHOUSE_PASSWORD"
            , "CLICKHOUSE_DATABASE"
            , "CLICKHOUSE_DB"
            , "SQLITE_BIN"
            , "DUCKDB_BIN"
            , "CONTAINER_NAME"
            , "TEMPLATE_DB"
            , "EXPECTED_SIGNATURE"
            , "BASELINE_SIGNATURE"
            , "ORACLE_REJECT_ON_SLIPPAGE"
            , "ORACLE_STACK_FRAMES"
            , "ORACLE_TIMEOUT"
            , "ORACLE_VERBOSE"
            , "ORACLE_ALLOW_STDERR_ONLY"
            , "ORACLE_ALLOW_PSQL_META"
            , "ORACLE_DECISION_LOG"
            , "OCCAM_WORKDIR"
            ]

writeMock :: FilePath -> String -> String -> IO FilePath
writeMock dir name body = do
    let path = dir </> name
        script = "#!/bin/sh\nset -e\n" <> body
    -- Write in a single-threaded child process so concurrent Haskell fork()
    -- calls never inherit an O_WRONLY descriptor to this script (Linux ETXTBSY).
    (ec, _out, err) <-
        readCreateProcessWithExitCode
            (proc "/bin/sh" ["-c", "cat > \"$1\" && chmod 0755 \"$1\"", "write-mock", path])
            script
    when (ec /= ExitSuccess) $
        assertFailure ("Failed to write mock executable " <> path <> ": " <> err)
    pure path

runHarness :: FilePath -> [String] -> BS.ByteString -> [(String, String)] -> IO (Int, Text, Text)
runHarness bin args stdinBytes childEnv = do
    (ec, outStr, errStr) <-
        readCreateProcessWithExitCode
            ((proc bin args) {env = Just childEnv})
            (T.unpack (TE.decodeUtf8 stdinBytes))
    pure (decodeExitCode ec, T.pack outStr, T.pack errStr)

readJsonl :: FilePath -> IO [Object]
readJsonl path = do
    raw <- BS.readFile path
    let ls = filter (not . T.null . T.strip) (T.lines (TE.decodeUtf8 raw))
    mapM parseLine ls
    where
        parseLine l = case eitherDecodeStrict' (TE.encodeUtf8 l) of
            Right obj -> pure obj
            Left err -> assertFailure ("Invalid JSONL line: " <> err <> "\nLine: " <> T.unpack l)

kmStr :: KM.Key -> Object -> Text
kmStr k obj = case KM.lookup k obj of
    Just (String t) -> t
    _ -> ""

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
    tmp <- getTemporaryDirectory
    bracket
        (mkdtemp (tmp </> prefix <> "-"))
        (\dir -> doesDirectoryExist dir >>= \ok -> when ok (removeDirectoryRecursive dir))
        action

trimStr :: String -> String
trimStr = reverse . dropWhile isSpace . reverse . dropWhile isSpace

sq :: String -> String
sq s = "'" <> concatMap (\c -> if c == '\'' then "'\\''" else [c]) s <> "'"
