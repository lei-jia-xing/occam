module CliChecks (tests) where

import Control.Exception (SomeException, bracket, finally, try)
import Control.Monad (forM_, when)
import Data.Aeson (FromJSON (..), Value (..), eitherDecodeStrict', withObject, (.:))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isSpace)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Occam.Evaluation (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    EvaluationStatus (..),
 )
import Occam.Parser (SQLDialect (..), dialectName)
import OracleChecks qualified
import System.Directory (
    doesFileExist,
    findExecutable,
    getTemporaryDirectory,
    removeFile,
 )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (hClose, openTempFile)
import System.Process (
    CreateProcess (..),
    StdStream (..),
    proc,
    readCreateProcessWithExitCode,
    readProcess,
    readProcessWithExitCode,
    shell,
    terminateProcess,
    waitForProcess,
    withCreateProcess,
 )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

getOccamBinary :: IO FilePath
getOccamBinary = resolveBinary "OCCAM_BIN" "occam"

-- | Env override, then PATH, then `cabal list-bin`; "cabal" means `cabal run`.
resolveBinary :: String -> String -> IO FilePath
resolveBinary envVar name = do
    mEnv <- lookupEnv envVar
    case mEnv of
        Just p -> do
            exists <- doesFileExist p
            if exists then pure p else fromPath
        Nothing -> fromPath
    where
        fromPath = do
            mPath <- findExecutable name
            maybe fromListBin pure mPath
        fromListBin = do
            mPath <- tryListBin name
            maybe (pure "cabal") pure mPath

tryListBin :: String -> IO (Maybe FilePath)
tryListBin name = do
    result <- try (readProcess "cabal" ["list-bin", name] "") :: IO (Either SomeException String)
    case result of
        Left _ -> pure Nothing
        Right out -> do
            let path = stripEnd out
            exists <- doesFileExist path
            pure (if exists then Just path else Nothing)

runOccamCLI :: [String] -> IO (ExitCode, String, String)
runOccamCLI cliArgs = runOccamCLIWithStdin cliArgs ""

runOccamCLIWithStdin :: [String] -> String -> IO (ExitCode, String, String)
runOccamCLIWithStdin cliArgs stdinData = do
    bin <- getOccamBinary
    if bin == "cabal"
        then readProcessWithExitCode "cabal" (["run", "-v0", "occam", "--"] ++ cliArgs) stdinData
        else readProcessWithExitCode bin cliArgs stdinData

withCLIInput :: BS.ByteString -> (FilePath -> FilePath -> IO a) -> IO a
withCLIInput sql action =
    bracket create cleanup $ \input -> do
        BS.writeFile input sql
        action input (input ++ ".min.sql")
    where
        create = do
            directory <- getTemporaryDirectory
            (path, handle) <- openTempFile directory "occam-cli-"
            hClose handle
            pure path
        cleanup path = do
            removeFile path
            exists <- doesFileExist (path ++ ".min.sql")
            when exists (removeFile (path ++ ".min.sql"))

stripEnd :: String -> String
stripEnd = reverse . dropWhile isSpace . reverse

-- | Minimal probe into the CLI's stats.json (whose Stats type lives in app/).
data CliStats = CliStats
    { cliStatus :: !EvaluationStatus
    , cliBudget :: !EvaluationConfig
    , cliMetrics :: !EvaluationMetrics
    , cliStrategy :: !T.Text
    }

instance FromJSON CliStats where
    parseJSON = withObject "CliStats" $ \stats -> do
        settings <- stats .: "reduction_settings"
        strategy <- withObject "reduction_settings" (.: "strategy") settings
        CliStats
            <$> stats .: "status"
            <*> stats .: "evaluation_budget"
            <*> stats .: "evaluation_metrics"
            <*> pure strategy

findPythonExecutable :: IO String
findPythonExecutable = do
    mPy3 <- findExecutable "python3"
    case mPy3 of
        Just _ -> pure "python3"
        Nothing -> do
            mPy <- findExecutable "python"
            case mPy of
                Just _ -> pure "python"
                Nothing -> pure "python3"

tests :: TestTree
tests =
    testGroup
        "CLI predicate contract"
        [ testCase "CLI refuses rejected or signaled initial predicates" $
            mapM_
                ( \extra ->
                    mapM_
                        ( \command -> withCLIInput "SELECT 1;" $ \input outFile -> do
                            (code, _, _) <-
                                runOccamCLI
                                    ([input, "--dialect", "sqlite", "--oracle", command, "--out", outFile, "--quiet"] ++ extra)
                            assertBool ("Noninteresting input was accepted: " ++ command) (code /= ExitSuccess)
                            exists <- doesFileExist outFile
                            assertBool "Reduction started before the initial predicate accepted" (not exists)
                        )
                        ["false", "exit 139", "kill -TERM $$"]
                )
                [[], ["--strategy", "ddmin-only"]]
        , testCase "CLI shrinks SQL while preserving the supplied predicate" $ do
            py <- findPythonExecutable
            let initialSQL =
                    BSC.unlines
                        [ "CREATE TABLE t1 (a INT, b TEXT);"
                        , "CREATE TABLE t2 (c INT);"
                        , "INSERT INTO t1 VALUES (1, 'x');"
                        , "CREATE INDEX i1 ON t1 (a);"
                        , "SELECT * FROM t1 WHERE a > 0 AND 1 = 1 ORDER BY a LIMIT 1;"
                        ]
                predicate =
                    py
                        ++ " -c \"import sys; s=sys.stdin.read(); sys.exit(0 if 'WHERE a > 0' in s and 'CREATE TABLE t1' in s and 'INSERT INTO t1' in s else 1)\""
            mapM_
                ( \extra -> withCLIInput initialSQL $ \input outFile -> do
                    (code, _, err) <-
                        runOccamCLI
                            ([input, "--dialect", "sqlite", "--oracle", predicate, "--out", outFile, "--quiet"] ++ extra)
                    assertEqual ("Reduction failed: " ++ err) ExitSuccess code
                    reduced <- BSC.readFile outFile
                    (verified, _, _) <- readCreateProcessWithExitCode (shell predicate) (BSC.unpack reduced)
                    assertEqual "Reduced SQL lost the target property" ExitSuccess verified
                    assertBool "No SQL reduction occurred" (BS.length reduced < BS.length initialSQL)
                )
                [[], ["--strategy", "ddmin-only"]]
        , testCase "selected strategies own their preflight and execution budget" $
            withCLIInput "SELECT 1; SELECT 2;" $ \input outRoot ->
                withCLIInput "" $ \calls _ -> do
                    let predicate = "cat >/dev/null; printf x >> " ++ show calls
                    mapM_
                        ( \(extra, expectedStrategy) -> do
                            let outFile = outRoot ++ "-" ++ T.unpack expectedStrategy ++ ".sql"
                                statsFile = outRoot ++ "-" ++ T.unpack expectedStrategy ++ ".json"
                            (code, _, _) <-
                                runOccamCLI
                                    ( [ input
                                      , "--dialect"
                                      , "sqlite"
                                      , "--oracle"
                                      , predicate
                                      , "--max-oracle-executions"
                                      , "1"
                                      , "--out"
                                      , outFile
                                      , "--stats"
                                      , statsFile
                                      , "--quiet"
                                      ]
                                        ++ extra
                                    )
                            assertBool "Execution-budget exhaustion must remain nonzero" (code /= ExitSuccess)
                            encoded <- BS.readFile statsFile
                            case eitherDecodeStrict' encoded :: Either String CliStats of
                                Right stats -> do
                                    let evaluation = cliMetrics stats
                                    assertEqual "Wrong selected strategy" expectedStrategy (cliStrategy stats)
                                    assertEqual
                                        "Own preflight consumes this run's cap"
                                        1
                                        (evaluationPreflightAttempts (cliBudget stats))
                                    assertEqual "Budget result was lost" EvaluationExecutionBudget (evaluationStatus evaluation)
                                    assertEqual
                                        "Top-level status was lost"
                                        EvaluationExecutionBudget
                                        (cliStatus stats)
                                    assertEqual
                                        "No reduction call remains after preflight"
                                        0
                                        (evaluationDatabaseObservations evaluation)
                                    assertBool "An unchecked checkpoint is not validated" (not (evaluationValidationPassed evaluation))
                                Left err -> assertFailure ("stats.json did not decode: " ++ err)
                            checkpoint <- BS.readFile outFile
                            assertEqual "Accepted input checkpoint was lost" "SELECT 1; SELECT 2;" checkpoint
                            outExists <- doesFileExist outFile
                            when outExists (removeFile outFile)
                            statsExists <- doesFileExist statsFile
                            when statsExists (removeFile statsFile)
                        )
                        [(["--strategy", "ddmin-only"], "ddmin-only"), (["--no-slicing", "--no-closure"], "full")]
                    observed <- BSC.readFile calls
                    assertEqual "Each run must physically execute its own preflight" "xx" observed
        , testCase "CLI surfaces --jobs help and rejects invalid counts" $ do
            mapM_
                ( \extra -> do
                    (helpCode, helpOut, _) <- runOccamCLI ("--help" : extra)
                    assertEqual "help must exit zero" ExitSuccess helpCode
                    assertBool "help hides --jobs" ("--jobs" `isInfixOf` helpOut)
                    mapM_
                        ( \jobs -> withCLIInput "SELECT 1;" $ \input outFile -> do
                            (code, _, _) <-
                                runOccamCLI
                                    ( [input, "--dialect", "sqlite", "--oracle", "exit 0", "--out", outFile, "--jobs", jobs, "--quiet"]
                                        ++ extra
                                    )
                            assertBool ("invalid --jobs was accepted: " ++ jobs) (code /= ExitSuccess)
                            exists <- doesFileExist outFile
                            assertBool "reduction started with invalid --jobs" (not exists)
                        )
                        ["0", "-1", "abc"]
                )
                [[], ["--strategy", "ddmin-only"]]
        , testCase "CLI rejects bad column modes before any oracle call" $
            withCLIInput "SELECT 1;" $ \input outFile -> do
                (helpCode, helpOut, _) <- runOccamCLI ["--help"]
                assertEqual "help must exit zero" ExitSuccess helpCode
                assertBool "help hides --column-mode" ("--column-mode" `elem` words helpOut)
                mapM_
                    ( \mode -> do
                        (code, _, _) <-
                            runOccamCLI
                                [ input
                                , "--dialect"
                                , "sqlite"
                                , "--oracle"
                                , "exit 0"
                                , "--out"
                                , outFile
                                , "--column-mode"
                                , mode
                                , "--quiet"
                                ]
                        assertBool ("bad --column-mode was accepted: " ++ mode) (code /= ExitSuccess)
                        exists <- doesFileExist outFile
                        assertBool "reduction started with bad --column-mode" (not exists)
                    )
                    ["bogus", "single-atomic"]
        , testCase "CLI requires --intra-statement for column modes and starts no oracle" $
            withCLIInput "SELECT 1;" $ \input outFile ->
                withCLIInput "" $ \calls _ -> do
                    let oracle = "cat >/dev/null; printf x >> " ++ show calls
                    mapM_
                        ( \mode -> do
                            (code, _, err) <-
                                runOccamCLI
                                    [ input
                                    , "--dialect"
                                    , "sqlite"
                                    , "--oracle"
                                    , oracle
                                    , "--out"
                                    , outFile
                                    , "--column-mode"
                                    , mode
                                    , "--quiet"
                                    ]
                            assertBool ("column mode without intra-statement was accepted: " ++ mode) (code /= ExitSuccess)
                            assertBool "wrong rejection reason" ("--intra-statement" `elem` words err)
                            exists <- doesFileExist outFile
                            assertBool "reduction started without --intra-statement" (not exists)
                        )
                        ["single", "atomic"]
                    observed <- BSC.readFile calls
                    assertEqual "oracle ran before validation" "" observed
        , testCase "help starts with a compact usage line and points to the dialect list" $ do
            (helpCode, helpOut, _) <- runOccamCLI ["--help"]
            assertEqual "help must exit zero" ExitSuccess helpCode
            assertEqual
                "usage and options should appear before any explanatory text"
                [ "Usage: occam INPUT.sql -d DIALECT (--oracle COMMAND | --result-oracle) [OPTIONS]"
                , ""
                , "Available options:"
                ]
                (take 3 (lines helpOut))
            assertEqual
                "input, oracle, and output should be listed first"
                ["INPUT.sql", "-c,--oracle", "-o,--out"]
                (map (takeWhile (/= ' ') . dropWhile (== ' ')) (take 3 (drop 3 (lines helpOut))))
            assertBool "missing dialect list pointer" ("See --list-dialects" `isInfixOf` helpOut)
            (listCode, dialects, _) <- runOccamCLI ["--list-dialects"]
            assertEqual "dialect list must exit zero" ExitSuccess listCode
            mapM_
                ( \dialect ->
                    let name = T.unpack (dialectName dialect)
                     in assertBool ("--list-dialects hides dialect: " ++ name) (name `elem` words dialects)
                )
                ([minBound .. maxBound] :: [SQLDialect])
        , testCase "native result oracle removes irrelevant statements through the CLI" $
            mapM_
                ( \(dialect, sql) ->
                    withCLIInput sql $ \input _ -> do
                        (code, out, err) <- runOccamCLI [input, "--dialect", dialect, "--result-oracle", "--quiet"]
                        assertEqual err ExitSuccess code
                        assertEqual "only the result-producing statement remains" "SELECT 42 AS answer;" out
                )
                [ ("sqlite", "CREATE TABLE noise (x INT);SELECT 42 AS answer;CREATE TABLE tail (x INT);")
                , ("duckdb", "SELECT [1,2] AS ignored;SELECT 42 AS answer;CREATE TABLE tail (x INT);")
                ]
        , testCase "default stdout includes the best reduction under a two-call budget" $
            withCLIInput "SELECT 1; SELECT 2;" $ \input _ -> do
                let predicate = "grep -q 'SELECT 2'"
                (code, out, _) <-
                    runOccamCLI [input, "-d", "sqlite", "-c", predicate, "--max-oracle-executions", "2", "--quiet"]
                assertBool "budget exhaustion must remain nonzero" (code /= ExitSuccess)
                assertEqual "preflight must leave one call for reduction" "SELECT 2;" out
        , testCase "--out - keeps stdout pure SQL under --verbose" $
            withCLIInput "CREATE TABLE t (a INT);\nINSERT INTO t VALUES (1);\nINSERT INTO t VALUES (2);\n" $ \input _ -> do
                (code, out, _) <-
                    runOccamCLI [input, "-d", "sqlite", "--oracle", "grep -q INSERT", "--out", "-", "--verbose"]
                assertEqual "reduction failed" ExitSuccess code
                assertBool "stdout missing reduced SQL" ("INSERT" `isInfixOf` out)
        , testCase "--dump-facts emits per-statement JSON and stays hidden from --help" $
            withCLIInput "CREATE TABLE t (a INT);\nINSERT INTO t VALUES (1);\n" $ \input _ -> do
                (helpCode, helpOut, _) <- runOccamCLI ["--help"]
                assertEqual "help must exit zero" ExitSuccess helpCode
                assertBool "--dump-facts leaked into --help" (not ("dump-facts" `isInfixOf` helpOut))
                (code, out, err) <- runOccamCLI [input, "--dump-facts", "--dialect", "sqlite"]
                assertEqual ("--dump-facts failed: " ++ err) ExitSuccess code
                case eitherDecodeStrict' (BSC.pack out) :: Either String Value of
                    Left e -> assertFailure ("--dump-facts output is not JSON: " ++ e)
                    Right _ -> assertBool "missing statements key" ("\"statements\"" `isInfixOf` out)
        , testCase "informational actions exit 0 without input file" $ do
            (dCode, dOut, _) <- runOccamCLI ["--list-dialects"]
            assertEqual "--list-dialects exit code" ExitSuccess dCode
            assertBool "missing sqlite in --list-dialects" ("sqlite" `isInfixOf` dOut)
            assertBool "missing postgresql in --list-dialects" ("postgresql" `isInfixOf` dOut)
            assertBool "missing mariadb in --list-dialects" ("mariadb" `isInfixOf` dOut)

            (mCode, mOut, _) <- runOccamCLI ["--list-column-modes"]
            assertEqual "--list-column-modes exit code" ExitSuccess mCode
            assertBool "missing single in --list-column-modes" ("single" `isInfixOf` mOut)
            assertBool "missing atomic in --list-column-modes" ("atomic" `isInfixOf` mOut)

            (sCode, sOut, _) <- runOccamCLI ["--list-strategies"]
            assertEqual "--list-strategies exit code" ExitSuccess sCode
            assertBool "missing full in --list-strategies" ("full" `isInfixOf` sOut)
            assertBool "missing ddmin-only in --list-strategies" ("ddmin-only" `isInfixOf` sOut)

            (pCode, pOut, _) <- runOccamCLI ["--print-defaults"]
            assertEqual "--print-defaults exit code" ExitSuccess pCode
            case eitherDecodeStrict' (BSC.pack pOut) :: Either String (Map.Map String Value) of
                Left e -> assertFailure ("--print-defaults is not JSON: " ++ e)
                Right defaults -> do
                    assertEqual "default timeout" (Just (Number 10)) (Map.lookup "timeout" defaults)
                    assertEqual "default jobs" (Just (Number 1)) (Map.lookup "jobs" defaults)
                    assertEqual "no default dialect" Nothing (Map.lookup "dialect" defaults)
                    assertEqual "default strategy" (Just (String "full")) (Map.lookup "strategy" defaults)
                    assertEqual
                        "default statement_order"
                        (Just (String "ranked-fallback"))
                        (Map.lookup "statement_order" defaults)
                    assertEqual "default literal_batches" (Just (Bool False)) (Map.lookup "literal_batches" defaults)
                    assertEqual "default ast_batching" (Just (Bool False)) (Map.lookup "ast_batching" defaults)
                    assertEqual "default adaptive_values" (Just (Bool False)) (Map.lookup "adaptive_values" defaults)
                    assertEqual "default log level" (Just (String "warn")) (Map.lookup "log_level" defaults)
                    assertEqual "json log level" (Just (String "info")) (Map.lookup "log_level_json" defaults)
        , testCase "--completion generates valid shell scripts and supports dynamic completion" $ do
            (bCode, bOut, _) <- runOccamCLI ["--completion", "bash"]
            assertEqual "--completion bash exit code" ExitSuccess bCode
            assertBool "bash completion missing function" ("_occam()" `isInfixOf` bOut)
            assertBool "bash completion missing complete" ("-F _occam" `isInfixOf` bOut)
            assertBool "bash completion missing dynamic hook" ("--bash-completion-index" `isInfixOf` bOut)

            (zCode, zOut, _) <- runOccamCLI ["--completion", "zsh"]
            assertEqual "--completion zsh exit code" ExitSuccess zCode
            assertBool "zsh completion missing compdef" ("#compdef occam" `isInfixOf` zOut)
            assertBool "zsh completion missing compadd" ("compadd" `isInfixOf` zOut)
            assertBool "zsh completion missing dynamic hook" ("--bash-completion-enriched" `isInfixOf` zOut)

            (fCode, fOut, _) <- runOccamCLI ["--completion", "fish"]
            assertEqual "--completion fish exit code" ExitSuccess fCode
            assertBool "fish completion missing complete command" ("--command occam" `isInfixOf` fOut)
            assertBool "fish completion missing function" ("_occam" `isInfixOf` fOut)
            assertBool "fish completion missing dynamic hook" ("--bash-completion-enriched" `isInfixOf` fOut)

            -- Dynamic option completion
            (cCode, cOut, _) <-
                runOccamCLI
                    ["--bash-completion-index", "1", "--bash-completion-word", "occam", "--bash-completion-word", "--"]
            assertEqual "completion options exit code" ExitSuccess cCode
            assertBool "completion includes --print-defaults" ("--print-defaults" `isInfixOf` cOut)
            assertBool "completion includes --dialect" ("--dialect" `isInfixOf` cOut)
            assertBool "completion includes --strategy" ("--strategy" `isInfixOf` cOut)
            assertBool "completion includes --completion" ("--completion" `isInfixOf` cOut)

            -- Dynamic argument completion (completeWith)
            (dCode, dOut, _) <-
                runOccamCLI
                    [ "--bash-completion-index"
                    , "2"
                    , "--bash-completion-word"
                    , "occam"
                    , "--bash-completion-word"
                    , "--dialect"
                    , "--bash-completion-word"
                    , ""
                    ]
            assertEqual "dialect completion exit code" ExitSuccess dCode
            assertBool "completion includes postgresql" ("postgresql" `isInfixOf` dOut)
            assertBool "completion includes mysql" ("mysql" `isInfixOf` dOut)
            assertBool "completion includes mariadb" ("mariadb" `isInfixOf` dOut)
            assertBool "completion includes sqlite" ("sqlite" `isInfixOf` dOut)

            (sCode, sOut, _) <-
                runOccamCLI
                    [ "--bash-completion-index"
                    , "2"
                    , "--bash-completion-word"
                    , "occam"
                    , "--bash-completion-word"
                    , "--strategy"
                    , "--bash-completion-word"
                    , ""
                    ]
            assertEqual "strategy completion exit code" ExitSuccess sCode
            assertBool "completion includes full" ("full" `isInfixOf` sOut)
            assertBool "completion includes ddmin-only" ("ddmin-only" `isInfixOf` sOut)
            assertBool "completion includes audit-only" ("audit-only" `isInfixOf` sOut)

            (mCode, mOut, _) <-
                runOccamCLI
                    [ "--bash-completion-index"
                    , "2"
                    , "--bash-completion-word"
                    , "occam"
                    , "--bash-completion-word"
                    , "--completion"
                    , "--bash-completion-word"
                    , ""
                    ]
            assertEqual "shell completion exit code" ExitSuccess mCode
            assertBool "completion includes bash" ("bash" `isInfixOf` mOut)
            assertBool "completion includes zsh" ("zsh" `isInfixOf` mOut)
            assertBool "completion includes fish" ("fish" `isInfixOf` mOut)

            (colCode, colOut, _) <-
                runOccamCLI
                    [ "--bash-completion-index"
                    , "2"
                    , "--bash-completion-word"
                    , "occam"
                    , "--bash-completion-word"
                    , "--color"
                    , "--bash-completion-word"
                    , ""
                    ]
            assertEqual "color completion exit code" ExitSuccess colCode
            assertBool "completion includes auto" ("auto" `isInfixOf` colOut)
            assertBool "completion includes always" ("always" `isInfixOf` colOut)
            assertBool "completion includes never" ("never" `isInfixOf` colOut)

            (llCode, llOut, _) <-
                runOccamCLI
                    [ "--bash-completion-index"
                    , "2"
                    , "--bash-completion-word"
                    , "occam"
                    , "--bash-completion-word"
                    , "--log-level"
                    , "--bash-completion-word"
                    , ""
                    ]
            assertEqual "log-level completion exit code" ExitSuccess llCode
            assertBool "completion includes debug" ("debug" `isInfixOf` llOut)
            assertBool "completion includes info" ("info" `isInfixOf` llOut)
            assertBool "completion includes warn" ("warn" `isInfixOf` llOut)
            assertBool "completion includes error" ("error" `isInfixOf` llOut)

            (lfCode, lfOut, _) <-
                runOccamCLI
                    [ "--bash-completion-index"
                    , "2"
                    , "--bash-completion-word"
                    , "occam"
                    , "--bash-completion-word"
                    , "--log-format"
                    , "--bash-completion-word"
                    , ""
                    ]
            assertEqual "log-format completion exit code" ExitSuccess lfCode
            assertBool "completion includes text" ("text" `isInfixOf` lfOut)
            assertBool "completion includes json" ("json" `isInfixOf` lfOut)

            (cmCode, cmOut, _) <-
                runOccamCLI
                    [ "--bash-completion-index"
                    , "2"
                    , "--bash-completion-word"
                    , "occam"
                    , "--bash-completion-word"
                    , "--column-mode"
                    , "--bash-completion-word"
                    , ""
                    ]
            assertEqual "column-mode completion exit code" ExitSuccess cmCode
            assertBool "completion includes off" ("off" `isInfixOf` cmOut)
            assertBool "completion includes single" ("single" `isInfixOf` cmOut)
            assertBool "completion includes atomic" ("atomic" `isInfixOf` cmOut)

            -- Shell completion case-insensitivity and invalid shell rejection
            (upperCode, upperOut, _) <- runOccamCLI ["--completion", "BASH"]
            assertEqual "--completion BASH exit code" ExitSuccess upperCode
            assertBool "case-insensitive completion missing function" ("_occam()" `isInfixOf` upperOut)

            (invCode, _, invErr) <- runOccamCLI ["--completion", "powershell"]
            assertEqual "--completion powershell exit failure" (ExitFailure 1) invCode
            assertBool "invalid shell error message" ("Expected 'bash', 'zsh', or 'fish'" `isInfixOf` invErr)
        , testCase "CLI requires in-place mode to overwrite the input" $
            withCLIInput "SELECT 1; SELECT 2;" $ \input _ -> do
                (code, _, err) <- runOccamCLI [input, "-d", "sqlite", "-c", "exit 0", "--out", input]
                assertEqual ("--out input must exit 2: " ++ err) (ExitFailure 2) code
                inputAfter <- BS.readFile input
                assertEqual "input file changed" "SELECT 1; SELECT 2;" inputAfter
        , testCase "CLI preserves input and backup permissions for in-place output" $
            withCLIInput "SELECT 1; SELECT 2;" $ \input _ -> do
                let backup = input ++ ".bak"
                BS.writeFile backup "old backup"
                flip finally (removeFile backup) $
                    OracleChecks.checkInPlacePermissions input backup $ do
                        (code, _, err) <-
                            runOccamCLI [input, "--in-place", "--dialect", "sqlite", "--result-oracle", "--quiet"]
                        assertEqual err ExitSuccess code
                        original <- BS.readFile backup
                        assertEqual "backup contains the original input" "SELECT 1; SELECT 2;" original
        , testCase "CLI rejects --stats paths that collide with input or output" $
            withCLIInput "SELECT 1;" $ \input outFile -> do
                (inputCode, _, inputErr) <-
                    runOccamCLI [input, "-d", "sqlite", "-c", "exit 0", "--stats", input]
                assertEqual ("--stats input must exit 2: " ++ inputErr) (ExitFailure 2) inputCode
                inputAfter <- BS.readFile input
                assertEqual "input was overwritten by stats" "SELECT 1;" inputAfter

                (outputCode, _, outputErr) <-
                    runOccamCLI [input, "-d", "sqlite", "-c", "exit 0", "--out", outFile, "--stats", outFile]
                assertEqual ("--stats output must exit 2: " ++ outputErr) (ExitFailure 2) outputCode

                let backup = input ++ ".bak"
                (backupCode, _, backupErr) <-
                    runOccamCLI [input, "-d", "sqlite", "-i", "-c", "exit 0", "--stats", backup]
                assertEqual ("--stats backup must exit 2: " ++ backupErr) (ExitFailure 2) backupCode
                backupExists <- doesFileExist backup
                when backupExists (removeFile backup)
        , testCase "CLI strictly rejects conflicting arguments with exit code 2" $ do
            (code1, _, err1) <-
                runOccamCLI ["input.sql", "-d", "sqlite", "--verbose", "--quiet", "-c", "exit 0"]
            assertEqual "verbose + quiet must exit 2" (ExitFailure 2) code1
            assertBool
                "wrong error for verbose + quiet"
                ("Cannot specify both --verbose and --quiet" `isInfixOf` err1)

            (code2, _, err2) <- runOccamCLI ["input.sql", "-d", "sqlite", "-i", "-o", "out.sql", "-c", "exit 0"]
            assertEqual "in-place + out must exit 2" (ExitFailure 2) code2
            assertBool
                "wrong error for in-place + out"
                ("Cannot specify both --in-place and --out" `isInfixOf` err2)

            (code3, _, err3) <- runOccamCLI ["-", "-d", "sqlite", "-i", "-c", "exit 0"]
            assertEqual "in-place on stdin must exit 2" (ExitFailure 2) code3
            assertBool "wrong error for in-place on stdin" ("Cannot modify stdin in place" `isInfixOf` err3)

            (code4, _, err4) <-
                runOccamCLI ["input.sql", "-d", "sqlite", "--strategy", "ddmin-only", "--slicing", "-c", "exit 0"]
            assertEqual "ddmin-only + slicing must exit 2" (ExitFailure 2) code4
            assertBool
                "wrong error for ddmin-only + slicing"
                ("--slicing conflicts with --strategy ddmin-only" `isInfixOf` err4)

            (code5, _, err5) <-
                runOccamCLI
                    ["input.sql", "-d", "sqlite", "--strategy", "audit-only", "--intra-statement", "-c", "exit 0"]
            assertEqual "audit-only + intra-statement must exit 2" (ExitFailure 2) code5
            assertBool
                "wrong error for audit-only + intra-statement"
                ("--intra-statement conflicts with --strategy audit-only" `isInfixOf` err5)

            (code6, _, err6) <-
                runOccamCLI
                    ["input.sql", "-d", "sqlite", "--strategy", "ddmin-only", "--intra-statement", "-c", "exit 0"]
            assertEqual "ddmin-only + intra-statement must exit 2" (ExitFailure 2) code6
            assertBool
                "wrong error for ddmin-only + intra-statement"
                ("--intra-statement conflicts with --strategy ddmin-only" `isInfixOf` err6)

            (code7, _, err7) <-
                runOccamCLI
                    ["input.sql", "-d", "sqlite", "--strategy", "audit-only", "--unrestricted", "-c", "exit 0"]
            assertEqual "audit-only + unrestricted must exit 2" (ExitFailure 2) code7
            assertBool
                "wrong error for audit-only + unrestricted"
                ("--unrestricted conflicts with --strategy audit-only" `isInfixOf` err7)

            (code8, _, err8) <-
                runOccamCLI
                    [ "input.sql"
                    , "-d"
                    , "sqlite"
                    , "--strategy"
                    , "audit-only"
                    , "--statement-order"
                    , "complement-first"
                    , "-c"
                    , "exit 0"
                    ]
            assertEqual "audit-only + statement-order must exit 2" (ExitFailure 2) code8
            assertBool
                "wrong error for audit-only + statement-order"
                ("--statement-order conflicts with --strategy audit-only" `isInfixOf` err8)

            (code9, _, err9) <-
                runOccamCLI ["input.sql", "-d", "sqlite", "--literal-batches", "-c", "exit 0"]
            assertEqual "literal-batches without intra-statement must exit 2" (ExitFailure 2) code9
            assertBool
                "wrong error for literal-batches without intra-statement"
                ("--literal-batches requires --intra-statement" `isInfixOf` err9)

            (code10, _, err10) <-
                runOccamCLI ["input.sql", "-d", "sqlite", "--adaptive-values", "-c", "exit 0"]
            assertEqual "adaptive-values without intra-statement must exit 2" (ExitFailure 2) code10
            assertBool
                "wrong error for adaptive-values without intra-statement"
                ("--adaptive-values requires --intra-statement" `isInfixOf` err10)

            (code11, _, err11) <-
                runOccamCLI ["input.sql", "-d", "sqlite", "--ast-batching", "-c", "exit 0"]
            assertEqual "ast-batching without intra-statement must exit 2" (ExitFailure 2) code11
            assertBool
                "wrong error for ast-batching without intra-statement"
                ("--ast-batching requires --intra-statement" `isInfixOf` err11)
        , testCase "CLI exposes search policy flags in completion and stats" $
            withCLIInput "SELECT 1234, 1234, 9999, 9999;" $ \input outFile -> do
                let statsFile = outFile ++ ".stats.json"
                (ordCompCode, ordCompOut, _) <-
                    runOccamCLI
                        [ "--bash-completion-index"
                        , "2"
                        , "--bash-completion-word"
                        , "occam"
                        , "--bash-completion-word"
                        , "--statement-order"
                        , "--bash-completion-word"
                        , ""
                        ]
                assertEqual "statement-order completion exit code" ExitSuccess ordCompCode
                assertBool "completion includes ranked-fallback" ("ranked-fallback" `isInfixOf` ordCompOut)
                assertBool "completion includes ranked" ("ranked" `isInfixOf` ordCompOut)
                assertBool "completion includes subset-first" ("subset-first" `isInfixOf` ordCompOut)
                assertBool "completion includes complement-first" ("complement-first" `isInfixOf` ordCompOut)
                assertBool "completion includes byte-weighted" ("byte-weighted" `isInfixOf` ordCompOut)

                (code, _, err) <-
                    runOccamCLI
                        [ input
                        , "-c"
                        , "sql=$(cat); [ \"$sql\" = 'SELECT 1234, 1234, 9999, 9999;' ] || [ \"$sql\" = 'SELECT 0, 0, 9999, 9999;' ]"
                        , "--dialect"
                        , "sqlite"
                        , "--statement-order"
                        , "complement-first"
                        , "--intra-statement"
                        , "--literal-batches"
                        , "--adaptive-values"
                        , "--out"
                        , outFile
                        , "--stats"
                        , statsFile
                        , "--quiet"
                        ]
                assertEqual ("CLI policy run failed: " ++ err) ExitSuccess code
                reduced <- BS.readFile outFile
                assertEqual "literal-batches coupled shrink works through CLI" "SELECT 0, 0, 9999, 9999;" reduced
                statsBytes <- BS.readFile statsFile
                case eitherDecodeStrict' statsBytes :: Either String Value of
                    Left e -> assertFailure ("stats.json did not decode: " ++ e)
                    Right val -> do
                        assertBool "stats missing statement_order" ("\"complement-first\"" `BSC.isInfixOf` statsBytes)
                        assertBool "stats missing literal_batches" ("\"literal_batches\":true" `BSC.isInfixOf` statsBytes)
                        assertBool "stats missing adaptive_values" ("\"adaptive_values\":true" `BSC.isInfixOf` statsBytes)
                        let _ = val
                        pure ()
                removeFile statsFile
        , testCase "CLI requires explicit --dialect with exit code 2 before starting any oracle" $
            withCLIInput "SELECT 1;" $ \input _ ->
                withCLIInput "" $ \calls _ -> do
                    let oracle = "cat >/dev/null; printf x >> " ++ show calls
                    (code, _, err) <- runOccamCLI [input, "--oracle", oracle]
                    assertEqual "omitted --dialect must exit 2" (ExitFailure 2) code
                    assertBool "missing --dialect error text" ("Missing required --dialect" `isInfixOf` err)
                    observed <- BSC.readFile calls
                    assertEqual "oracle ran despite missing --dialect" "" observed

                    (dumpCode, _, dumpErr) <- runOccamCLI [input, "--dump-facts"]
                    assertEqual "--dump-facts without --dialect must exit 2" (ExitFailure 2) dumpCode
                    assertBool "missing --dialect error text for --dump-facts" ("--dialect" `isInfixOf` dumpErr)

                    (mariaCode, mariaOut, mariaErr) <- runOccamCLI [input, "--dump-facts", "-d", "mariadb"]
                    assertEqual ("mariadb --dump-facts failed: " ++ mariaErr) ExitSuccess mariaCode
                    assertBool "mariadb --dump-facts missing statements" ("\"statements\"" `isInfixOf` mariaOut)
        , testCase "CLI rejects missing input files with exit code 1" $ do
            (code, _, err) <-
                runOccamCLI ["nonexistent-test-file-never-created.sql", "-d", "sqlite", "-c", "exit 0"]
            assertEqual "missing input file must exit 1" (ExitFailure 1) code
            assertBool "error message mentions missing file" ("No such file or directory" `isInfixOf` err)
        , testCase "--quiet suppresses jobs without reset warning" $
            withCLIInput "CREATE TABLE t (a INT);\nINSERT INTO t VALUES (1);\n" $ \input _ -> do
                (code, _, err) <-
                    runOccamCLI [input, "-d", "sqlite", "-c", "grep -q INSERT", "-j", "2", "--quiet", "--out", "-"]
                assertEqual "reduction should succeed" ExitSuccess code
                assertBool "warning should be suppressed by --quiet" (not ("state contamination" `isInfixOf` err))
        , testCase "CLI reads stdin only with explicit '-' and rejects omitted input" $ do
            (okCode, okOut, okErr) <-
                runOccamCLIWithStdin
                    ["-", "--dialect", "sqlite", "--result-oracle", "--quiet"]
                    "CREATE TABLE noise (x INT);SELECT 42 AS answer;"
            assertEqual ("explicit '-' stdin reduction failed: " ++ okErr) ExitSuccess okCode
            assertEqual "explicit '-' reduced SQL mismatch" "SELECT 42 AS answer;" okOut

            (missCode, _, missErr) <-
                runOccamCLIWithStdin ["-d", "sqlite", "-c", "exit 0", "--quiet"] "SELECT 1;"
            assertBool "omitted input must be rejected even when stdin is piped" (missCode /= ExitSuccess)
            assertBool "missing input error should name INPUT.sql" ("Missing: INPUT.sql" `isInfixOf` missErr)
        , testCase "CLI rejects --stats - stdout collision before reading stdin" $ do
            bin <- getOccamBinary
            let baseProc =
                    if bin == "cabal"
                        then proc "cabal" ["run", "-v0", "occam", "--", "-", "-d", "sqlite", "-c", "exit 0", "--stats", "-"]
                        else proc bin ["-", "-d", "sqlite", "-c", "exit 0", "--stats", "-"]
                cliProc =
                    baseProc
                        { std_in = CreatePipe
                        , std_out = CreatePipe
                        , std_err = CreatePipe
                        }
            withCreateProcess cliProc $ \mIn _ mErr ph -> do
                -- Intentionally keep stdin open without EOF; validation must fail fast.
                res <- timeout 10000000 (waitForProcess ph)
                case res of
                    Nothing -> do
                        terminateProcess ph
                        _ <- waitForProcess ph
                        assertFailure "CLI blocked on stdin before rejecting --stats - collision"
                    Just code -> do
                        errText <- maybe (pure "") (fmap BSC.unpack . BSC.hGetContents) mErr
                        assertEqual ("--stats - with stdout must exit 2: " ++ errText) (ExitFailure 2) code
                        assertBool
                            "wrong error for --stats - with stdout"
                            ("--stats - cannot be combined" `isInfixOf` errText)
                forM_ mIn hClose
        ]
