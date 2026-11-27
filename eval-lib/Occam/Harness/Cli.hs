-- | Command-line dispatcher for the @occam-harness@ helper executable.
module Occam.Harness.Cli (
    BatchRunArgs (..),
    HarnessCommand (..),
    parseHarnessArgs,
    runHarnessCommand,
    runHarnessMain,
) where

import Data.Aeson (encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Char (isSpace, toLower)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Occam.AtomicFile (writeAtomicLazyBytes)
import Occam.Benchmark.BatchReport (aggregateOutDir)
import Occam.Harness.BarrierCensus (barrierCensus)
import Occam.Harness.Batch (BatchOptions (..), BatchSummary (..), runBatch)
import Occam.Harness.BatchInputs (BatchInputOptions (..), BatchInputSource (..))
import Occam.Harness.BatchPlan (
    BatchPlanOptions (..),
    PlanOverrides (..),
    pbpPreview,
    prepareBatchPlan,
 )
import Occam.Harness.Collect (
    ArtifactSource (..),
    CollectOptions (..),
    collectArtifacts,
 )
import Occam.Harness.Config (
    HarnessConfig (..),
    SquirrelRenderOptions (..),
    loadExperimentPlan,
    loadHarnessConfig,
    lookupSquirrelSpec,
    renderSquirrelYaml,
    resolveHarnessConfigPath,
    squirrelFieldText,
 )
import Occam.Harness.DbmsExec (ConnectionOverrides (..))
import Occam.Harness.Oracle (
    OracleDecision (..),
    OracleOptions (..),
    OracleOverrides (..),
    OracleResult (..),
    probeCrashOracle,
    resolveOracleOptions,
 )
import Occam.Harness.Reset (ResetOptions (..), resetDb)
import Occam.Harness.Signature (DbmsKind (..), parseDbmsKind)
import Occam.Harness.SqlancerSanitize (
    SqlancerSanitizeResult (..),
    sanitizeSqlancerLog,
 )
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitFailure, exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

-- | Supported @occam-harness@ subcommands.
data HarnessCommand
    = CmdAggregate !Bool !FilePath
    | CmdBarrierCensus !FilePath
    | CmdBatch !(Maybe FilePath) !BatchRunArgs
    | CmdCollect !(Maybe FilePath) !CollectOptions
    | CmdOracle !(Maybe FilePath) !OracleOverrides
    | CmdReset !(Maybe FilePath) !(Maybe DbmsKind) !ConnectionOverrides !Int
    | CmdPlanConfig !(Maybe FilePath) !Text
    | CmdPlanBatch !(Maybe FilePath) !BatchPlanOptions
    | CmdSanitizeSqlancer !(Maybe FilePath) !(Maybe FilePath) !Bool
    | CmdRenderSquirrelYaml !(Maybe FilePath) !SquirrelRenderOptions
    | CmdSquirrelField !(Maybe FilePath) !Text !Text
    | CmdCheckConfig !(Maybe FilePath)
    deriving (Eq, Show)

data BatchRunArgs
    = BatchRunFull !BatchPlanOptions !Bool !(Maybe FilePath) !(Maybe FilePath) !(Maybe Text)
    | BatchAggregateOnly !FilePath
    deriving (Eq, Show)

-- | Parse command-line arguments purely for @occam-harness@.
parseHarnessArgs :: [String] -> ParserResult HarnessCommand
parseHarnessArgs =
    execParserPure defaultPrefs parserInfo
    where
        parserInfo =
            info
                (commandParser <**> helper)
                ( fullDesc
                    <> progDesc
                        "Typed Haskell harness helper for batch reporting, Dhall config validation, and SQLancer log sanitization"
                    <> header "occam-harness - Occam evaluation harness helper"
                )

commandParser :: Parser HarnessCommand
commandParser =
    subparser
        ( command
            "aggregate"
            (info aggregateParser (progDesc "Aggregate batch runs into aggregated_stats.json"))
            <> command
                "barrier-census"
                (info barrierCensusParser (progDesc "Analyze SQL fixtures and print barrier counts as JSON"))
            <> command
                "batch"
                ( info
                    (batchParser <**> helper)
                    (progDesc "Execute native Haskell batch qualification, reduction, and reporting")
                )
            <> command
                "collect"
                ( info
                    (collectParser <**> helper)
                    (progDesc "Import SQL artifacts into a manifest with optional replay verification")
                )
            <> command
                "oracle"
                (info (oracleParser <**> helper) (progDesc "Execute SQL from stdin and validate a DBMS crash"))
            <> command
                "reset"
                (info (resetParser <**> helper) (progDesc "Reset target DBMS database state cleanly"))
            <> command
                "plan-config"
                ( info
                    (planConfigParser <**> helper)
                    (progDesc "Load a named, JSON, or Dhall experiment plan and print JSON")
                )
            <> command
                "plan-batch"
                ( info
                    (planBatchParser <**> helper)
                    (progDesc "Write plan.json without executing SQL or starting containers")
                )
            <> command
                "sanitize-sqlancer"
                (info sanitizeParser (progDesc "Sanitize a SQLancer crash log into replayable SQL and metadata"))
            <> command
                "render-squirrel-yaml"
                (info renderSquirrelParser (progDesc "Render SQUIRREL YAML config from Dhall-compiled harness.json"))
            <> command
                "squirrel-field"
                ( info
                    squirrelFieldParser
                    (progDesc "Print a single scalar SQUIRREL template field from harness.json")
                )
            <> command
                "check-config"
                (info checkConfigParser (progDesc "Validate config/generated/harness.json against typed schemas"))
        )

aggregateParser :: Parser HarnessCommand
aggregateParser =
    CmdAggregate
        <$> switch (long "print-json" <> help "Print the aggregated report JSON to stdout")
        <*> strArgument (metavar "OUT_DIR" <> help "Batch output directory containing runs/ and qualified/")

barrierCensusParser :: Parser HarnessCommand
barrierCensusParser =
    CmdBarrierCensus
        <$> strArgument
            (metavar "CORPUS_DIR" <> help "Directory with SQL files and their .meta.json dialect metadata")

batchInputOptionsParser :: Parser BatchInputOptions
batchInputOptionsParser =
    BatchInputOptions
        <$> source
        <*> strOption
            (long "dbms" <> value "" <> metavar "DBMS" <> help "Select a DBMS or supply one for raw SQL inputs")
        <*> strOption
            (long "cases" <> value "" <> metavar "IDS" <> help "Comma-separated corpus or manifest case IDs")
        <*> strOption
            ( long "expected-signature"
                <> value ""
                <> metavar "REGEX"
                <> help "Default signature for cases without one"
            )
        <*> strOption
            (long "image" <> value "" <> metavar "IMAGE" <> help "Override the image recorded for every case")
        <*> many
            ( strOption
                (long "image-map" <> metavar "KEY=IMAGE" <> help "Override by DBMS, DBMS version, or case ID")
            )
    where
        source =
            ( CorpusDirectory
                <$> strOption
                    (long "corpus-dir" <> metavar "DIR" <> help "Directory with SQL files and optional metadata")
            )
                <|> (ManifestFile <$> strOption (long "manifest" <> metavar "FILE" <> help "JSON case manifest"))
                <|> (SqlFiles <$> ((:|) <$> sqlFile <*> many sqlFile))
                <|> ( SqlFiles
                        <$> ( flag' () (long "inputs" <> help "Raw SQL files passed as positional arguments")
                                *> ((:|) <$> posSqlFile <*> many posSqlFile)
                            )
                    )
        sqlFile = strOption (long "input" <> metavar "SQL_FILE" <> help "Raw SQL file, repeat for multiple inputs")
        posSqlFile = strArgument (metavar "SQL_FILES..." <> help "Raw SQL files following --inputs")

collectParser :: Parser HarnessCommand
collectParser =
    CmdCollect
        <$> configOption
        <*> ( CollectOptions
                <$> option
                    (eitherReader readSource)
                    (long "source" <> metavar "TYPE" <> help "raw, sqlsmith, sqlancer, or squirrel")
                <*> strOption (long "dbms" <> metavar "DBMS" <> help "Target DBMS")
                <*> strOption (long "input-dir" <> metavar "DIR" <> help "Directory containing existing artifacts")
                <*> strOption
                    (long "out-dir" <> short 'o' <> metavar "DIR" <> help "Directory for collected SQL and manifest.json")
                <*> optional (strOption (long "exclude-dir" <> internal))
                <*> switch (long "include-cur-logs" <> help "Include SQLancer current logs alongside exception logs")
                <*> (not <$> switch (long "no-fallback-cur-logs" <> help "Disable SQLancer current-log fallback"))
                <*> strOption
                    (long "expected-signature" <> value "" <> metavar "REGEX" <> help "Override the expected signature")
                <*> switch (long "verify" <> help "Verify stability via N-replay and deduplicate by crash signature")
                <*> option
                    auto
                    (long "replays" <> value 5 <> metavar "COUNT" <> help "Required passing replays under --verify")
                <*> (not <$> switch (long "no-dedup-by-signature" <> help "Keep multiple cases with identical signature"))
                <*> connectionOverridesParser False
                <*> optional (strOption (long "log-file" <> metavar "PATH" <> help "Host path to DBMS server log"))
                <*> optional
                    (strOption (long "container-log" <> metavar "PATH" <> help "Server log path inside container"))
                <*> switch (long "allow-stderr-only" <> help "UNSAFE: accept stderr crash evidence without server log")
                <*> switch (long "allow-psql-meta" <> help "UNSAFE: allow psql backslash meta-commands")
                <*> option
                    doubleReader
                    (long "probe-timeout" <> value 15.0 <> metavar "SECONDS" <> help "Per-probe timeout under --verify")
                <*> ( not
                        <$> switch (long "no-reset-before-verify" <> help "Do not reset server DB between replay checks")
                    )
                <*> option
                    auto
                    (long "stack-frames" <> value 5 <> metavar "COUNT" <> help "Stack frames retained in signatures")
            )
    where
        readSource "raw" = Right Raw
        readSource "sqlsmith" = Right Sqlsmith
        readSource "sqlancer" = Right Sqlancer
        readSource "squirrel" = Right Squirrel
        readSource _ = Left "Choose raw, sqlsmith, sqlancer, or squirrel"

batchPlanOptionsParser :: Parser BatchPlanOptions
batchPlanOptionsParser =
    BatchPlanOptions
        <$> batchInputOptionsParser
        <*> strOption (long "plan" <> value "smoke" <> metavar "PLAN" <> help "Plan name or path")
        <*> strOption
            ( long "out-dir"
                <> short 'o'
                <> value "occam-out/batch_eval"
                <> metavar "DIR"
                <> help "Directory for batch output"
            )
        <*> optional (strOption (long "profiles" <> metavar "NAMES" <> help "Comma-separated profile names"))
        <*> many (strOption (long "profile-spec" <> metavar "NAME=FLAGS" <> help "Define custom profile flags"))
        <*> planOverridesParser
        <*> optional
            ( strOption
                (long "containers" <> metavar "NAMES" <> help "External container names with optional addresses")
            )
        <*> switch (long "local-embedded" <> help "Plan embedded SQLite or DuckDB runs")
        <*> switch (long "allow-unsafe-occam-jobs" <> help "Allow concurrent Occam jobs within a server worker")
        <*> option auto (long "replays" <> value 5 <> metavar "COUNT" <> help "Clean-state verification count")
        <*> switch (long "skip-input-replay" <> help "Skip input qualification")
        <*> option
            doubleReader
            (long "probe-timeout" <> value 30 <> metavar "SECONDS" <> help "Standalone probe timeout")
        <*> (not <$> switch (long "no-resume" <> help "Rerun all jobs"))
        <*> switch (long "rerun-failed" <> help "Retry failed jobs when resuming")

planBatchParser :: Parser HarnessCommand
planBatchParser =
    CmdPlanBatch <$> configOption <*> batchPlanOptionsParser

batchParser :: Parser HarnessCommand
batchParser =
    CmdBatch <$> configOption <*> (aggregateOnlyParser <|> fullRunParser)
    where
        aggregateOnlyParser =
            flag' () (long "aggregate-only" <> help "Regenerate aggregated_stats.json without executing jobs")
                *> ( BatchAggregateOnly
                        <$> strOption (long "out-dir" <> short 'o' <> value "occam-out/batch_eval" <> metavar "DIR")
                   )
        fullRunParser =
            BatchRunFull
                <$> batchPlanOptionsParser
                <*> switch (long "dry-run" <> help "Write plan.json and print preview without starting workers")
                <*> optional (strOption (long "occam-bin" <> metavar "PATH" <> help "Override occam executable"))
                <*> optional
                    (strOption (long "engine-bin" <> metavar "PATH" <> help "Override local embedded engine binary"))
                <*> optional (strOption (long "run-tag" <> metavar "TAG" <> help "Unique container prefix tag"))

planOverridesParser :: Parser PlanOverrides
planOverridesParser =
    PlanOverrides
        <$> optional
            (option auto (long "repeats" <> metavar "COUNT" <> help "Repetitions per case and profile"))
        <*> optional (option auto (long "jobs" <> short 'j' <> metavar "COUNT" <> help "Batch workers"))
        <*> optional
            (option auto (long "occam-jobs" <> metavar "COUNT" <> help "Concurrent jobs per Occam invocation"))
        <*> optional
            (option auto (long "container-wait" <> metavar "SECONDS" <> help "Container startup and reset limit"))
        <*> optional (option doubleReader (long "timeout" <> metavar "SECONDS" <> help "Per-probe limit"))
        <*> optional
            (option doubleReader (long "budget" <> metavar "SECONDS" <> help "Per-run wall-time budget"))
        <*> optional
            (option auto (long "max-oracle-executions" <> metavar "COUNT" <> help "Per-run execution budget"))
        <*> switch (long "no-cache" <> help "Disable Occam's cache")
        <*> switch (long "reject-on-slippage" <> help "Reject signature changes")
        <*> optional
            (option auto (long "stack-frames" <> metavar "COUNT" <> help "Stack frames retained in signatures"))

planConfigParser :: Parser HarnessCommand
planConfigParser =
    CmdPlanConfig
        <$> configOption
        <*> strArgument (metavar "PLAN" <> help "Built-in plan name or path to JSON or Dhall")

sanitizeParser :: Parser HarnessCommand
sanitizeParser =
    CmdSanitizeSqlancer
        <$> optional
            ( strOption
                (long "input" <> short 'i' <> metavar "FILE" <> help "Input SQLancer log (defaults to stdin)")
            )
        <*> optional
            (strOption (long "meta-out" <> metavar "FILE" <> help "Write extracted metadata JSON to FILE"))
        <*> switch (long "json" <> help "Print JSON object {\"sql\": ..., \"metadata\": ...} to stdout")

configOption :: Parser (Maybe FilePath)
configOption =
    optional
        ( strOption
            ( long "config"
                <> metavar "FILE"
                <> help "Path to harness.json (defaults to OCCAM_HARNESS_JSON or config/generated/harness.json)"
            )
        )

renderSquirrelParser :: Parser HarnessCommand
renderSquirrelParser =
    CmdRenderSquirrelYaml
        <$> configOption
        <*> ( SquirrelRenderOptions
                <$> strOption (long "dbms" <> metavar "DBMS" <> help "Target DBMS (sqlite, postgres, mysql, mariadb)")
                <*> strOption (long "squirrel-home" <> metavar "DIR" <> help "SQUIRREL installation directory")
                <*> strOption (long "host" <> value "" <> metavar "HOST" <> help "Database host")
                <*> option auto (long "port" <> value 0 <> metavar "PORT" <> help "Database port")
                <*> strOption (long "user" <> value "" <> metavar "USER" <> help "Database user")
                <*> strOption (long "password" <> value "" <> metavar "PASSWORD" <> help "Database password")
                <*> strOption (long "socket" <> value "" <> metavar "PATH" <> help "Unix socket path")
                <*> strOption (long "db-name" <> value "" <> metavar "NAME" <> help "Database name or prefix")
                <*> strOption (long "server-cmd" <> value "" <> metavar "CMD" <> help "Server startup command")
            )

squirrelFieldParser :: Parser HarnessCommand
squirrelFieldParser =
    CmdSquirrelField
        <$> configOption
        <*> strArgument (metavar "DBMS" <> help "Target SQUIRREL DBMS")
        <*> strArgument (metavar "FIELD" <> help "Field name (e.g. target_bin_rel, default_input_subdir)")

checkConfigParser :: Parser HarnessCommand
checkConfigParser =
    CmdCheckConfig
        <$> optional (strArgument (metavar "CONFIG_JSON" <> help "Optional path to harness.json"))

dbmsReader :: ReadM DbmsKind
dbmsReader = eitherReader (\s -> maybe (Left ("Unsupported DBMS: " <> s)) Right (parseDbmsKind (T.pack s)))

doubleReader :: ReadM Double
doubleReader = eitherReader \raw ->
    let trimmed = reverse . dropWhile isSpace . reverse . dropWhile isSpace $ raw
     in case map toLower trimmed of
            "nan" -> Right (0 / 0)
            "inf" -> Right (1 / 0)
            "+inf" -> Right (1 / 0)
            "-inf" -> Right ((-1) / 0)
            _ -> maybe (Left ("Invalid number: " <> raw)) Right (readMaybe trimmed)

connectionOverridesParser :: Bool -> Parser ConnectionOverrides
connectionOverridesParser includeTemplate =
    ConnectionOverrides
        <$> optional
            (strOption (long "host" <> metavar "HOST" <> help "Database host or unix socket directory"))
        <*> optional (option auto (long "port" <> metavar "PORT" <> help "Database port"))
        <*> optional (strOption (long "user" <> metavar "USER" <> help "Database user"))
        <*> optional (strOption (long "password" <> metavar "PASSWORD" <> help "Database password"))
        <*> optional (strOption (long "dbname" <> metavar "DBNAME" <> help "Target database name"))
        <*> ( if includeTemplate
                then
                    optional
                        (strOption (long "template-db" <> metavar "TEMPLATE" <> help "PostgreSQL template database"))
                else pure Nothing
            )
        <*> optional (strOption (long "socket" <> metavar "PATH" <> help "MySQL/MariaDB unix socket path"))
        <*> optional (strOption (long "container-name" <> metavar "CONTAINER" <> help "Docker container name"))
        <*> optional
            (strOption (long "engine-bin" <> metavar "PATH" <> help "Override embedded engine executable"))

oracleParser :: Parser HarnessCommand
oracleParser =
    CmdOracle
        <$> configOption
        <*> ( OracleOverrides
                <$> optional (option dbmsReader (long "dbms" <> metavar "DBMS" <> help "Target DBMS"))
                <*> connectionOverridesParser False
                <*> optional
                    ( strOption
                        ( long "expected-signature"
                            <> metavar "REGEX"
                            <> help "Regex or literal substring of expected crash signature"
                        )
                    )
                <*> optional
                    ( strOption
                        ( long "baseline-signature"
                            <> metavar "REF"
                            <> help "Baseline signature id, hash, or JSON path for RQ3 slippage"
                        )
                    )
                <*> switch
                    ( long "reject-on-slippage"
                        <> help "Reject faults whose crash_signature differs from --baseline-signature"
                    )
                <*> optional
                    (option auto (long "stack-frames" <> metavar "N" <> help "Maximum non-wrapper stack frames retained"))
                <*> switch (long "print-signature" <> help "Print extracted crash_signature JSON to stdout")
                <*> optional (strOption (long "log-file" <> metavar "PATH" <> help "Host path to DBMS server log"))
                <*> optional
                    (strOption (long "container-log" <> metavar "PATH" <> help "Server log path inside container"))
                <*> optional
                    (strOption (long "decision-log" <> metavar "PATH" <> help "Append one JSON verdict record per call"))
                <*> optional
                    ( option
                        doubleReader
                        (long "timeout" <> metavar "SECS" <> help "Optional client-side timeout in seconds (0 = disabled)")
                    )
                <*> switch
                    (long "allow-stderr-only" <> help "UNSAFE: accept connection loss from stderr without server log")
                <*> switch (long "allow-psql-meta" <> help "UNSAFE: allow psql backslash meta-commands")
                <*> switch (long "verbose" <> help "Print verbose diagnostics to stderr")
            )

resetParser :: Parser HarnessCommand
resetParser =
    CmdReset
        <$> configOption
        <*> optional (option dbmsReader (long "dbms" <> metavar "DBMS" <> help "Target DBMS"))
        <*> connectionOverridesParser True
        <*> option
            auto
            ( long "max-wait"
                <> value 30
                <> metavar "SECS"
                <> help "Max wait seconds for server readiness (default: 30)"
            )

-- | CLI entry point invoked by @occam-harness@.
runHarnessMain :: IO ()
runHarnessMain = do
    cmd <-
        execParser
            ( info
                (commandParser <**> helper)
                ( fullDesc
                    <> progDesc "Typed Haskell harness helper for Occam"
                    <> header "occam-harness"
                )
            )
    runHarnessCommand cmd

-- | Execute one parsed @HarnessCommand@.
runHarnessCommand :: HarnessCommand -> IO ()
runHarnessCommand = \case
    CmdAggregate printJson outDir -> do
        agg <- aggregateOutDir outDir
        whenTrue printJson (BSL.putStr (encode agg <> BSL.pack [10]))
    CmdBarrierCensus corpusDir ->
        barrierCensus corpusDir >>= either dieWith (\report -> BSL.putStr (encode report <> BSL.pack [10]))
    CmdBatch mCfg runArgs -> do
        cfgPath <- resolveHarnessConfigPath mCfg
        cfg <- loadConfigOrDie cfgPath
        bOpts <- case runArgs of
            BatchAggregateOnly outDir ->
                let dummyPlan =
                        BatchPlanOptions
                            (BatchInputOptions (CorpusDirectory ".") "" "" "" "" [])
                            "smoke"
                            outDir
                            Nothing
                            []
                            (PlanOverrides Nothing Nothing Nothing Nothing Nothing Nothing Nothing False False Nothing)
                            Nothing
                            False
                            False
                            5
                            False
                            30.0
                            True
                            False
                 in pure (BatchOptions cfgPath dummyPlan False True Nothing Nothing Nothing Nothing)
            BatchRunFull pOpts dryRun occBin engBin rTag ->
                pure (BatchOptions cfgPath pOpts dryRun False occBin engBin Nothing rTag)
        res <- runBatch cfg bOpts
        case res of
            Left err -> dieValidation err
            Right summary -> do
                BSL.putStr (encode (bsPayload summary) <> BSL.pack [10])
                if bsExitCode summary == 0
                    then exitSuccess
                    else exitWith (ExitFailure (bsExitCode summary))
    CmdCollect mCfg options -> do
        cfg <- loadOrDie mCfg
        collectArtifacts cfg options
            >>= either dieValidation (\summary -> BSL.putStr (encode summary <> BSL.pack [10]))
    CmdOracle mCfg overrides -> do
        cfg <- loadOrDie mCfg
        opts <- either dieValidation pure =<< resolveOracleOptions cfg overrides
        sqlBytes <- BS.getContents
        res <- probeCrashOracle cfg opts sqlBytes
        whenTrue (ooPrintSignature opts) $
            for_ (odCrashSignature (orDecision res)) \sig ->
                BSL.putStr (encode sig <> BSL.pack [10])
        if orExitCode res == 0
            then exitSuccess
            else exitWith (ExitFailure (orExitCode res))
    CmdReset mCfg mDbms connOv maxWait -> do
        cfg <- loadOrDie mCfg
        envDbms <- lookupEnv "DBMS"
        dbms <- case mDbms <|> (envDbms >>= parseDbmsKind . T.pack) of
            Just d -> pure d
            Nothing -> case envDbms of
                Just raw | not (null raw) -> dieValidation ("Unsupported DBMS in DBMS env: " <> raw)
                _ -> pure DbmsPostgres
        resetDb cfg (ResetOptions dbms connOv maxWait) >>= either dieWith pure
    CmdPlanConfig mCfg ref -> do
        path <- resolveHarnessConfigPath mCfg
        cfg <- loadConfigOrDie path
        loadExperimentPlan path cfg ref
            >>= either dieValidation (\plan -> BSL.putStr (encode plan <> BSL.pack [10]))
    CmdPlanBatch mCfg options -> do
        path <- resolveHarnessConfigPath mCfg
        cfg <- loadConfigOrDie path
        prepareBatchPlan path cfg options
            >>= either dieValidation (\plan -> BSL.putStr (encode (pbpPreview plan) <> BSL.pack [10]))
    CmdSanitizeSqlancer mInput mMetaOut asJson -> do
        rawBytes <- maybe BS.getContents BS.readFile mInput
        let res = sanitizeSqlancerLog rawBytes
        case mMetaOut of
            Nothing -> pure ()
            Just metaPath -> writeAtomicLazyBytes metaPath (encode (sanitizedMeta res) <> BSL.pack [10])
        if asJson
            then BSL.putStr (encode res <> BSL.pack [10])
            else BS.putStr (sanitizedSql res)
    CmdRenderSquirrelYaml mCfg opts -> do
        hc <- loadOrDie mCfg
        case renderSquirrelYaml hc opts of
            Left err -> dieWith (T.unpack err)
            Right rendered -> TIO.putStr rendered
    CmdSquirrelField mCfg dbms field -> do
        hc <- loadOrDie mCfg
        case lookupSquirrelSpec hc dbms of
            Nothing -> dieWith ("Unsupported SQUIRREL DBMS '" <> T.unpack dbms <> "'")
            Just spec -> for_ (squirrelFieldText spec field) TIO.putStr
    CmdCheckConfig mCfg -> do
        hc <- loadOrDie mCfg
        putStrLn $
            unwords
                [ "Valid harness config:"
                , show (length (hcDbms hc))
                , "dbms,"
                , show (length (hcProfiles hc))
                , "profiles,"
                , show (Map.size (hcPlans hc))
                , "plans,"
                , show (length (hcSquirrel hc))
                , "squirrel templates"
                ]
    where
        whenTrue True act = act
        whenTrue False _ = pure ()

        loadOrDie mCfg = resolveHarnessConfigPath mCfg >>= loadConfigOrDie

        loadConfigOrDie path = do
            eCfg <- loadHarnessConfig path
            case eCfg of
                Left err -> dieWith err
                Right hc -> pure hc

        dieWith msg = do
            hPutStrLn stderr ("occam-harness: " <> msg)
            exitFailure

        dieValidation msg = do
            hPutStrLn stderr ("occam-harness: " <> msg)
            exitWith (ExitFailure 2)
