module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Data.Aeson (FromJSON, ToJSON, Value, encode, object, toJSON, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Char (toLower)
import Data.Text qualified as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import Occam qualified
import Occam.Core (
    AlterChange (..),
    AlterTable (..),
    BarrierScope (..),
    CatalogOp (..),
    ConflictAction (..),
    ConstraintRef (..),
    DataWrite (..),
    DmlEvent (..),
    DropMode (..),
    DropObject (..),
    DropOp (..),
    Ident (..),
    IndexDef (..),
    IndexRef (..),
    NameRef (..),
    ObjectRef (..),
    PartitionLink (..),
    ReadFact (..),
    ReadMode (..),
    RefreshOp (..),
    RelationKind (..),
    RenameOp (..),
    SchemaDef (..),
    ScopedBarrier (..),
    SemanticFacts (..),
    SequenceDef (..),
    SequenceWrite (..),
    SessionEffect (..),
    Span (..),
    Statement (..),
    TableDef (..),
    TriggerBody (..),
    TriggerDef (..),
    TriggerEffect (..),
    TriggerRef (..),
    TxEffect (..),
    TypeDef (..),
    ViewDef (..),
    WriteKind (..),
    nameText,
 )
import Occam.Evaluation (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    EvaluationResult (..),
    EvaluationStatus (..),
    metricPolicyForReduction,
    parseSQLDialect,
    runEvaluatedReductionWithProgress,
    sqlDialectName,
 )
import Occam.Facts (StatementFacts (..), StatementUnit (..))
import Occam.Logger (
    LogConfig (..),
    LogFormat (..),
    LogLevel (..),
    Logger,
    logError,
    logInfo,
    logInfo',
    withLogger,
 )
import Occam.Oracle (
    OracleConfig (..),
    OracleObservationDetails (..),
    prepareOracle,
    withCandidateWorkdir,
 )
import Occam.Parser (
    ParsedSQL (..),
    ParserError (..),
    SQLDialect (..),
    countStatements,
    defaultParseOptions,
    dialectName,
    parseSQL,
 )
import Occam.Reducer (
    ColumnMode (..),
    ReductionConfig (..),
    columnModeName,
    defaultReductionConfig,
    reduceSQLWithConfig,
 )
import Occam.UI (
    UI (..),
    UIConfig (..),
    newUI,
 )
import Options.Applicative (
    Parser,
    ParserInfo,
    auto,
    bashCompleter,
    completer,
    eitherReader,
    execParser,
    flag',
    fullDesc,
    header,
    help,
    helper,
    info,
    infoOption,
    internal,
    long,
    metavar,
    option,
    optional,
    progDesc,
    short,
    showDefault,
    strArgument,
    strOption,
    switch,
    value,
    (<**>),
 )
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
 )
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (hFlush, hIsTerminalDevice, hPutStrLn, stderr, stdout)
import System.Timeout (timeout)

data Options = Options
    { optInputFile :: !FilePath
    , optOracleCmd :: !(Maybe String)
    , optOutDir :: !FilePath
    , optResetCmd :: !(Maybe String)
    , optTimeoutSecs :: !Double
    , optLogLevel :: !LogLevel
    , optLogFormat :: !LogFormat
    , optVerbose :: !Bool
    , optQuiet :: !Bool
    , optBudgetSecs :: !(Maybe Double)
    , optMaxOracleExecutions :: !(Maybe Int)
    , optJobs :: !Int
    , optReductionOptions :: !ReductionOptions
    , optDumpFacts :: !Bool
    }
    deriving (Eq, Show)

data ReductionOptions = ReductionOptions
    { optSlicing :: !Bool
    , optClosure :: !Bool
    , optIntraStatement :: !Bool
    , optColumnMode :: !ColumnMode
    , optUnrestricted :: !Bool
    , optCache :: !Bool
    , optDialect :: !SQLDialect
    }
    deriving (Eq, Show)

data Stats = Stats
    { initial_bytes :: !Int
    , reduced_bytes :: !Int
    , reduction_ratio :: !Double
    , initial_statements :: !Int
    , reduced_statements :: !Int
    , initial_tokens :: !(Maybe Int)
    , reduced_tokens :: !(Maybe Int)
    , oracle_executions :: !Int
    , oracle_attempts :: !Int
    , initial_oracle_observations :: !Int
    , cache_hits :: !Int
    , cache_misses :: !Int
    , cache_joins :: !Int
    , cache_hit_ratio :: !Double
    , cancelled_observations :: !Int
    , status :: !EvaluationStatus
    , reduction_settings :: !Value
    , evaluation_budget :: !EvaluationConfig
    , evaluation_metrics :: !EvaluationMetrics
    , initial_oracle_seconds :: !Double
    , elapsed_seconds :: !Double
    }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

optionsParser :: Parser Options
optionsParser =
    Options
        <$> strArgument
            ( metavar "INPUT.sql"
                <> help "Path to fuzz SQL input, or '-' for stdin"
                <> completer (bashCompleter "file")
            )
        <*> optional
            ( strOption
                ( long "oracle"
                    <> metavar "COMMAND"
                    <> help "Interestingness predicate command (SQL on stdin; exit 0 accepts)"
                )
            )
        <*> strOption
            ( long "out"
                <> short 'o'
                <> metavar "DIR"
                <> value "occam-out"
                <> showDefault
                <> help "Directory where reduced artifacts will be written, or '-' for stdout"
                <> completer (bashCompleter "directory")
            )
        <*> optional
            ( strOption
                ( long "reset"
                    <> metavar "COMMAND"
                    <> help "Lightweight state reset command (e.g. dropdb/createdb)"
                )
            )
        <*> option
            auto
            ( long "timeout"
                <> metavar "SECONDS"
                <> value 10.0
                <> showDefault
                <> help "Per-execution timeout in seconds"
            )
        <*> option
            (eitherReader parseLogLevel)
            ( long "log-level"
                <> metavar "LEVEL"
                <> value LevelInfo
                <> help "Log severity level (debug, info, warn, error; default: info)"
            )
        <*> option
            (eitherReader parseLogFormat)
            ( long "log-format"
                <> metavar "FORMAT"
                <> value FormatText
                <> help "Log output format (text or json; default: text)"
            )
        <*> switch
            ( long "verbose"
                <> help "Enable verbose debug output (equivalent to --log-level debug)"
            )
        <*> switch
            ( long "quiet"
                <> short 'q'
                <> help "Suppress info logs and banners (equivalent to --log-level error)"
            )
        <*> optional
            ( option
                auto
                ( long "budget"
                    <> metavar "SECONDS"
                    <> help
                        "Shared wall-clock cap including the initial predicate check, reduction, audit, and repeated final validation"
                )
            )
        <*> optional
            ( option
                auto
                ( long "max-oracle-executions"
                    <> metavar "N"
                    <> help "Maximum uncached oracle attempts including the initial check and final validation"
                )
            )
        <*> option
            (eitherReader parseJobs)
            ( long "jobs"
                <> metavar "N"
                <> value 1
                <> showDefault
                <> help
                    "Maximum concurrent oracle/reset executions (default 1). Declares the predicate AND reset support N concurrent independent calls with independent database state; Occam enforces at most N in flight but provides no database isolation"
            )
        <*> reductionOptionsParser
        <*> switch
            ( long "dump-facts"
                <> internal
            )

reductionOptionsParser :: Parser ReductionOptions
reductionOptionsParser =
    ReductionOptions
        <$> toggle True "slicing" "no-slicing" "dependency slicing"
        <*> toggle True "closure" "no-closure" "dependency closure"
        <*> toggle False "intra-statement" "no-intra-statement" "intra-statement reductions"
        <*> option
            (eitherReader parseColumnMode)
            ( long "column-mode"
                <> metavar "MODE"
                <> value ColumnOff
                <> help
                    "Column co-reduction listing: off, single, or atomic (default: off; non-off requires --intra-statement)"
            )
        <*> toggle
            True
            "unrestricted"
            "no-unrestricted"
            "unrestricted fallback search (final deletion audit always runs)"
        <*> toggle True "cache" "no-cache" "candidate caching"
        <*> option
            (eitherReader parseSQLDialect)
            ( long "dialect"
                <> metavar "DIALECT"
                <> value GenericSQL
                <> showDefault
                <> help
                    "SQL dependency dialect: generic, postgresql, mysql, sqlite, duckdb, clickhouse, oracle, ansi, bigquery, databricks, hive, mssql, redshift, snowflake, spark, or teradata"
            )

toggle :: Bool -> String -> String -> String -> Parser Bool
toggle defaultValue enableName disableName description =
    flag' True (long enableName <> help ("Enable " ++ description))
        <|> flag' False (long disableName <> help ("Disable " ++ description))
        <|> pure defaultValue

parseLogLevel :: String -> Either String LogLevel
parseLogLevel s = case map toLower s of
    "debug" -> Right LevelDebug
    "info" -> Right LevelInfo
    "warn" -> Right LevelWarn
    "warning" -> Right LevelWarn
    "error" -> Right LevelError
    _ -> Left "Expected debug, info, warn, or error"

parseLogFormat :: String -> Either String LogFormat
parseLogFormat s = case map toLower s of
    "text" -> Right FormatText
    "json" -> Right FormatJSON
    _ -> Left "Expected text or json"

{- | Positive job count parsed as 'Integer' first so huge inputs range-fail
instead of overflowing 'Int'.
-}
parseJobs :: String -> Either String Int
parseJobs s = case reads s of
    [(n, "")]
        | n >= (1 :: Integer)
        , n <= toInteger (maxBound :: Int) ->
            Right (fromInteger n)
    _ -> Left "Expected a positive --jobs value"

parseColumnMode :: String -> Either String ColumnMode
parseColumnMode s = case map toLower s of
    "off" -> Right ColumnOff
    "single" -> Right ColumnSingle
    "atomic" -> Right ColumnAtomic
    _ -> Left "Expected off, single, or atomic"

versionOption :: Parser (a -> a)
versionOption =
    infoOption
        ("Occam v" ++ Occam.version)
        ( long "version"
            <> short 'v'
            <> help "Show version information"
        )

optsInfo :: ParserInfo Options
optsInfo =
    info
        (optionsParser <**> versionOption <**> helper)
        ( fullDesc
            <> progDesc "Occam: Dependency-Aware SQL Test Case Minimizer"
            <> header ("Occam v" ++ Occam.version ++ " - Multi-statement SQL reducer")
        )

main :: IO ()
main = execParser optsInfo >>= runOccam

runOccam :: Options -> IO ()
runOccam opts@Options {..} = do
    let effectiveLevel
            | optQuiet = LevelError
            | optVerbose = LevelDebug
            | otherwise = optLogLevel
        effectiveFormat = optLogFormat
        logCfg =
            LogConfig
                { logMinLevel = effectiveLevel
                , logFormat = effectiveFormat
                , logUseColor = True
                }
        runWithLogger logger = runOccamWithLogger logger opts
    withLogger logCfg runWithLogger

{- | Reduction checkpoint location. Stdout runs get a per-run temporary
directory (cleaned on exit); directory runs checkpoint the output file.
-}
withCheckpointPath :: Bool -> FilePath -> (FilePath -> IO a) -> IO a
withCheckpointPath writeToStdout minimalPath action
    | not writeToStdout = action minimalPath
    | otherwise = withCandidateWorkdir (\tmpDir -> action (tmpDir </> "occam-checkpoint.sql"))

runOccamWithLogger :: Logger -> Options -> IO ()
runOccamWithLogger logger Options {..} = do
    let dialect = optDialect optReductionOptions
    when optDumpFacts do
        dumpStatementFacts dialect optInputFile
        exitSuccess
    oracleCmd <- case optOracleCmd of
        Just cmd -> pure cmd
        Nothing -> do
            hPutStrLn stderr "occam: error: --oracle COMMAND is required (unless --dump-facts is given)"
            exitFailure
    let finitePositive seconds = seconds > 0 && not (isNaN seconds || isInfinite seconds)
        maxSeconds = fromIntegral (maxBound :: Int) / 1000000
        finiteTimeout seconds =
            finitePositive seconds && seconds <= maxSeconds && seconds * 1000000 >= 1
        validBudget =
            finiteTimeout optTimeoutSecs
                && maybe True finiteTimeout optBudgetSecs
                && maybe True (> 0) optMaxOracleExecutions
                && optJobs > 0
    unless validBudget do
        hPutStrLn stderr "Error: timeout, budget, execution, and job limits must be positive"
        exitFailure

    let columnMode = optColumnMode optReductionOptions
    when (columnMode /= ColumnOff && not (optIntraStatement optReductionOptions)) do
        hPutStrLn stderr "Error: --column-mode single|atomic requires --intra-statement"
        exitFailure

    initialSQL <-
        if optInputFile == "-"
            then BS.getContents
            else do
                fileExists <- doesFileExist optInputFile
                unless fileExists do
                    let msg = "cannot open '" ++ optInputFile ++ "': No such file or directory"
                    logError logger "occam::cli" (T.pack msg)
                    hPutStrLn stderr ("occam: error: " ++ msg)
                    exitFailure
                BS.readFile optInputFile

    initialStmts <- countStatementsForCLI logger dialect initialSQL
    let initialBytes = BS.length initialSQL

    ui <-
        newUI
            UIConfig
                { uiTargetFile = optInputFile
                , uiInitialBytes = initialBytes
                , uiInitialStmts = initialStmts
                , uiDialect = sqlDialectName dialect
                , uiQuiet = optQuiet
                , uiVerbose = optVerbose || optLogLevel == LevelDebug
                , uiUseColor = True
                }

    let baseOracleConfig =
            OracleConfig
                { oracleCommand = "sh"
                , oracleArgs = ["-c", oracleCmd]
                , oracleTimeoutMicros = round (optTimeoutSecs * 1000000)
                , oracleResetCommand = optResetCmd
                }
        reductionConfig = toReductionConfig optReductionOptions
        configuredBudget =
            EvaluationConfig
                { evaluationCacheEnabled = optCache optReductionOptions
                , evaluationJobs = optJobs
                , evaluationTotalTimeoutMicros = round . (* 1000000) <$> optBudgetSecs
                , evaluationMaxOracleExecutions = optMaxOracleExecutions
                , evaluationPreflightMicros = 0
                , evaluationPreflightAttempts = 0
                , evaluationPreflightResetMicros = Nothing
                , evaluationPreflightOracleMicros = Nothing
                }
    logInfo'
        logger
        "occam::cli"
        "Starting reduction session"
        [ ("input_file", toJSON optInputFile)
        , ("initial_bytes", toJSON initialBytes)
        , ("initial_statements", toJSON initialStmts)
        , ("oracle_cmd", toJSON oracleCmd)
        , ("evaluation_budget", toJSON configuredBudget)
        , ("reduction_settings", reductionSettings (optCache optReductionOptions) reductionConfig)
        ]

    initialStart <- getCurrentTime
    preflightStart <- toInteger <$> getMonotonicTimeNSec
    let totalBudgetMicros = round . (* 1000000) <$> optBudgetSecs :: Maybe Integer
    prepared <-
        runWithinBudget
            preflightStart
            totalBudgetMicros
            (prepareOracle baseOracleConfig initialSQL)
    preflightDetails <- case prepared of
        Nothing -> do
            hPutStrLn stderr "occam: error: initial predicate check exceeded the total budget"
            exitFailure
        Just (Left err) -> do
            hPutStrLn stderr ("occam: error: initial predicate check failed: " ++ T.unpack err)
            hPutStrLn stderr "occam: note: oracle command rejected the original unreduced SQL input"
            exitFailure
        Just (Right details) -> pure details
    preflightEnd <- toInteger <$> getMonotonicTimeNSec
    initialEnd <- getCurrentTime
    let oracleCfg = baseOracleConfig
        preflightMicros =
            fromInteger
                (min (toInteger (maxBound :: Int)) (max 0 ((preflightEnd - preflightStart) `div` 1000)))
        budgetConfig =
            configuredBudget
                { evaluationPreflightMicros = preflightMicros
                , evaluationPreflightAttempts = 1
                , evaluationPreflightResetMicros = Just (resetElapsedMicros preflightDetails)
                , evaluationPreflightOracleMicros = oracleElapsedMicros preflightDetails
                }
    logInfo logger "occam::oracle" "Initial predicate check passed"

    let writeToStdout = optOutDir == "-"
        minimalPath = if writeToStdout then "" else optOutDir </> "minimal.sql"
        statsPath = if writeToStdout then "" else optOutDir </> "stats.json"

    unless writeToStdout (createDirectoryIfMissing True optOutDir)
    logInfo logger "occam::reducer" "Running hierarchical reduction engine..."

    metricsPolicy <- case metricPolicyForReduction reductionConfig optJobs of
        Left err -> do
            hPutStrLn stderr ("occam: error: " ++ T.unpack err)
            exitFailure
        Right policy -> pure (Just policy)
    result <-
        withCheckpointPath writeToStdout minimalPath $ \checkpointPath ->
            runEvaluatedReductionWithProgress
                (uiOnProgress ui)
                metricsPolicy
                budgetConfig
                dialect
                oracleCfg
                (Just (detailedObservation preflightDetails))
                logger
                initialSQL
                checkpointPath
                (\reducerLogger oracle -> reduceSQLWithConfig reductionConfig reducerLogger oracle initialSQL)
    let reducedSQL = evaluatedSQL result
        evaluation = evaluationMetrics result
        totalHits = evaluationCacheHits evaluation
        totalMisses = evaluationCacheMisses evaluation
        totalLookups = totalHits + totalMisses
        hitRatio =
            if totalLookups > 0
                then fromIntegral totalHits / fromIntegral totalLookups
                else 0.0
    reducedStmts <- countStatementsForCLI logger dialect reducedSQL
    let reducedBytes = BS.length reducedSQL
        ratio =
            if initialBytes > 0
                then (1.0 - (fromIntegral reducedBytes / fromIntegral initialBytes :: Double)) * 100.0
                else 0.0
        initialOracleSeconds = realToFrac (diffUTCTime initialEnd initialStart)
        totalOracleExecutions =
            evaluationPreflightAttempts budgetConfig
                + evaluationDatabaseObservations evaluation
                + evaluationValidationDatabaseObservations evaluation
        totalOracleAttempts =
            evaluationPreflightAttempts budgetConfig
                + evaluationOracleAttempts evaluation
                + evaluationValidationObservations evaluation
        stats =
            Stats
                { initial_bytes = initialBytes
                , reduced_bytes = reducedBytes
                , reduction_ratio = ratio
                , initial_statements = initialStmts
                , reduced_statements = reducedStmts
                , initial_tokens = Nothing
                , reduced_tokens = Nothing
                , oracle_executions = totalOracleExecutions
                , oracle_attempts = totalOracleAttempts
                , initial_oracle_observations = evaluationPreflightAttempts budgetConfig
                , cache_hits = totalHits
                , cache_misses = totalMisses
                , cache_joins = evaluationCacheJoins evaluation
                , cache_hit_ratio = hitRatio
                , cancelled_observations = evaluationCancelledObservations evaluation
                , status = evaluationStatus evaluation
                , reduction_settings = reductionSettings (optCache optReductionOptions) reductionConfig
                , evaluation_budget = budgetConfig
                , evaluation_metrics = evaluation
                , initial_oracle_seconds = initialOracleSeconds
                , elapsed_seconds = evaluationTotalSeconds evaluation
                }
    unless writeToStdout do
        BS.writeFile minimalPath reducedSQL
        BSL.writeFile statsPath (encode stats)

    isStdoutTTY <- hIsTerminalDevice stdout
    when (writeToStdout || not isStdoutTTY) do
        BS.putStr reducedSQL
        hFlush stdout

    logInfo'
        logger
        "occam::cli"
        "Reduction session finished"
        [ ("status", toJSON (evaluationStatus evaluation))
        , ("initial_statements", toJSON initialStmts)
        , ("reduced_statements", toJSON reducedStmts)
        , ("initial_bytes", toJSON initialBytes)
        , ("reduced_bytes", toJSON reducedBytes)
        , ("database_observations", toJSON totalOracleExecutions)
        , ("cache_hits", toJSON totalHits)
        , ("cache_misses", toJSON totalMisses)
        , ("elapsed_seconds", toJSON (evaluationTotalSeconds evaluation))
        ]

    uiFinish ui evaluation initialBytes reducedBytes initialStmts reducedStmts minimalPath statsPath
    unless
        (evaluationStatus evaluation == EvaluationCompleted && evaluationValidationPassed evaluation)
        exitFailure

runWithinBudget :: Integer -> Maybe Integer -> IO a -> IO (Maybe a)
runWithinBudget _ Nothing action = Just <$> action
runWithinBudget started (Just totalMicros) action = do
    now <- toInteger <$> getMonotonicTimeNSec
    let remaining = max 0 (totalMicros - (now - started) `div` 1000)
        timeoutMicros = fromInteger (min remaining (toInteger (maxBound :: Int)))
    timeout timeoutMicros action

countStatementsForCLI :: Logger -> SQLDialect -> BS.ByteString -> IO Int
countStatementsForCLI logger dialect sql = do
    result <- countStatements dialect sql
    case result of
        Left err -> do
            let message = "SQL parser failed while counting statements: " <> T.pack (show err)
            logError logger "occam::parser" message
            hPutStrLn stderr ("occam: error: " ++ T.unpack message)
            exitFailure
        Right count -> pure count

toReductionConfig :: ReductionOptions -> ReductionConfig
toReductionConfig ReductionOptions {..} =
    defaultReductionConfig
        { reductionSlicing = optSlicing
        , reductionClosure = optClosure
        , reductionIntraStatement = optIntraStatement
        , reductionColumnMode = optColumnMode
        , reductionUnrestricted = optUnrestricted
        , reductionDialect = optDialect
        }

reductionSettings :: Bool -> ReductionConfig -> Value
reductionSettings cacheEnabled ReductionConfig {..} =
    object
        [ "slicing" .= reductionSlicing
        , "closure" .= reductionClosure
        , "intra_statement" .= reductionIntraStatement
        , "column_mode" .= columnModeName reductionColumnMode
        , "unrestricted" .= reductionUnrestricted
        , "cache" .= cacheEnabled
        , "dialect" .= sqlDialectName reductionDialect
        ]

{- | Hidden @--dump-facts@ handler: parse INPUT with the selected dialect and
print per-statement modeled facts as JSON on stdout. No oracle interaction.
-}
dumpStatementFacts :: SQLDialect -> FilePath -> IO ()
dumpStatementFacts dialect inputFile = do
    sql <-
        if inputFile == "-"
            then BS.getContents
            else do
                fileExists <- doesFileExist inputFile
                unless fileExists do
                    hPutStrLn stderr ("occam: error: cannot open '" ++ inputFile ++ "': No such file or directory")
                    exitFailure
                BS.readFile inputFile
    parsed <- parseSQL (defaultParseOptions dialect) sql
    case parsed of
        Left err -> do
            hPutStrLn stderr ("occam: error: failed to parse input: " ++ T.unpack (parserErrorMessage err))
            exitFailure
        Right result -> BSL.putStr (encode (factDumpJson dialect result) <> "\n")

factDumpJson :: SQLDialect -> ParsedSQL -> Value
factDumpJson dialect parsed =
    object
        [ "dialect" .= dialectName dialect
        , "statements" .= zipWith statementFactsJson [0 :: Int ..] (parsedUnits parsed)
        ]

statementFactsJson :: Int -> StatementUnit -> Value
statementFactsJson idx unit =
    object
        [ "index" .= idx
        , "start" .= startByte
        , "end" .= endByte
        , "parse_error" .= unitParseError unit
        , "reads" .= readsJson
        , "writes" .= writesJson
        , "ddl" .= ddlJson
        , "barriers" .= map barrierJson (factBarriers semantics)
        , "session" .= map sessionEffectName (factSession semantics)
        , "pre_tx" .= map txEffectName (factPreTx semantics)
        , "post_tx" .= map txEffectName (factPostTx semantics)
        , "trigger_effects" .= triggerEffectsJson ops
        ]
    where
        Span startByte endByte = stmtSpan (unitStatement unit)
        semantics = maybe (SemanticFacts [] [] [] [] []) factSemantics (unitFacts unit)
        ops = factOps semantics
        readsJson = map readFactJson (concatMap opReads ops)
        writesJson = concatMap opWrites ops
        ddlJson = concatMap opDdl ops

{- | Reads evaluated when the statement itself runs: direct reads, DML source
reads, and CTAS/view definition sources. Trigger WHEN-clause reads fire
with the trigger, so they are reported under @trigger_effects@ instead.
-}
opReads :: CatalogOp -> [ReadFact]
opReads (ReadOp r) = [r]
opReads (WriteDataOp w) = dataWriteReads w
opReads (DefineTableOp t) = tableSources t
opReads (DefineViewOp v) = viewSources v
opReads _ = []

opWrites :: CatalogOp -> [Value]
opWrites (WriteDataOp w) = [dataWriteJson w]
opWrites (RefreshCatalogOp r) =
    [ object
        [ "target" .= nameText (nameRefName (refreshTarget r))
        , "kind" .= ("refresh" :: T.Text)
        , "columns" .= ([] :: [T.Text])
        , "unqualified_columns" .= ([] :: [T.Text])
        ]
    ]
opWrites _ = []

opDdl :: CatalogOp -> [Value]
opDdl (DefineTableOp t) = [ddlOpJson "create-table" (nameText (tableName t))]
opDdl (DefineViewOp v) =
    [ ddlOpJson
        (if viewMaterialized v then "create-materialized-view" else "create-view")
        (nameText (viewName v))
    ]
opDdl (DefineIndexOp i) =
    [ object
        [ "op" .= ("create-index" :: T.Text)
        , "target" .= nameText (nameRefName (indexTable i))
        , "name" .= indexRefName (indexRef i)
        ]
    ]
opDdl (DefineTriggerOp t) =
    [ object
        [ "op" .= ("create-trigger" :: T.Text)
        , "target" .= nameText (nameRefName (triggerTable t))
        , "name" .= triggerRefName (triggerRef t)
        ]
    ]
opDdl (DefineSequenceOp s) = [ddlOpJson "create-sequence" (nameText (sequenceName s))]
opDdl (DefineTypeOp t) = [ddlOpJson "create-type" (nameText (typeName t))]
opDdl (DefineSchemaOp s) = [ddlOpJson "create-schema" (nameText (schemaName s))]
opDdl (AlterTableOp a) =
    [ object
        [ "op" .= ("alter-table" :: T.Text)
        , "target" .= nameText (nameRefName (alterTableTarget a))
        , "changes" .= map alterChangeName (alterTableChanges a)
        ]
    ]
opDdl (AttachPartitionOp l) = [ddlOpJson "attach-partition" (nameText (nameRefName (linkChild l)))]
opDdl (DetachPartitionOp l) = [ddlOpJson "detach-partition" (nameText (nameRefName (linkChild l)))]
opDdl (DropCatalogOp d) = map (dropObjectJson (dropMode d)) (dropObjects d)
opDdl (RenameOp r) = [renameJson r]
opDdl (WriteSequenceOp s) = [ddlOpJson "sequence-write" (nameText (nameRefName (sequenceWriteTarget s)))]
opDdl _ = []

ddlOpJson :: T.Text -> T.Text -> Value
ddlOpJson op target = object ["op" .= op, "target" .= target]

readFactJson :: ReadFact -> Value
readFactJson r =
    object
        [ "table" .= nameText (nameRefName (readSource r))
        , "temp" .= nameRefTemp (readSource r)
        , "columns" .= map identText (readColumns r)
        , "mode" .= (case readMode r of ReadData -> ("data" :: T.Text); ReadSchema -> "schema")
        ]

dataWriteJson :: DataWrite -> Value
dataWriteJson w =
    object
        [ "target" .= nameText (nameRefName (dataWriteTarget w))
        , "kind" .= writeKindName (dataWriteKind w)
        , "columns" .= map identText (dataWriteColumns w)
        , "unqualified_columns" .= map identText (dataWriteUnqualifiedColumns w)
        ]

writeKindName :: WriteKind -> T.Text
writeKindName (InsertWrite ConflictError) = "insert"
writeKindName (InsertWrite ConflictIgnore) = "insert-or-ignore"
writeKindName (InsertWrite ConflictReplace) = "insert-or-replace"
writeKindName (InsertWrite ConflictUpdate) = "insert-or-update"
writeKindName UpdateWrite = "update"
writeKindName (DeleteWrite _) = "delete"
writeKindName MergeWrite = "merge"
writeKindName (TruncateWrite _ _) = "truncate"

alterChangeName :: AlterChange -> T.Text
alterChangeName (AddColumn _ _) = "add-column"
alterChangeName (DropColumn _ _) = "drop-column"
alterChangeName (AddConstraint _) = "add-constraint"
alterChangeName (DropConstraint _ _) = "drop-constraint"
alterChangeName (RenameConstraint _ _) = "rename-constraint"
alterChangeName (ValidateConstraint _) = "validate-constraint"
alterChangeName (DropIndexChange _) = "drop-index"
alterChangeName (AlterColumn _ _) = "alter-column"
alterChangeName (OtherChange detail) = "other:" <> detail

dropObjectJson :: DropMode -> DropObject -> Value
dropObjectJson mode obj =
    object
        [ "op" .= opName
        , "target" .= target
        , "mode" .= (if mode == DropCascade then ("cascade" :: T.Text) else "restrict")
        ]
    where
        (opName, target) = case obj of
            DropRelation r kind -> (relKindName kind, nameText (nameRefName r))
            DropIndex i -> ("drop-index" :: T.Text, indexRefName i)
            DropTrigger t -> ("drop-trigger", triggerRefName t)
            DropSequence s -> ("drop-sequence", nameText (nameRefName s))
            DropType t -> ("drop-type", nameText t)
            DropSchema s -> ("drop-schema", nameText s)
        relKindName TableRelation = "drop-table"
        relKindName ViewRelation = "drop-view"
        relKindName MaterializedViewRelation = "drop-materialized-view"

renameJson :: RenameOp -> Value
renameJson (RenameRelation old new _) =
    object
        [ "op" .= ("rename-relation" :: T.Text)
        , "target" .= nameText (nameRefName old)
        , "name" .= nameText new
        ]
renameJson (RenameColumnOp t old new) =
    object
        [ "op" .= ("rename-column" :: T.Text)
        , "target" .= nameText (nameRefName t)
        , "name" .= identText new
        , "detail" .= identText old
        ]

indexRefName :: IndexRef -> T.Text
indexRefName (NamedIndex n) = nameText n
indexRefName (TableIndex t i) = nameText (nameRefName t) <> "." <> identText i

triggerRefName :: TriggerRef -> T.Text
triggerRefName (NamedTrigger n) = nameText n
triggerRefName (TableTrigger t i) = nameText (nameRefName t) <> "." <> identText i

barrierJson :: ScopedBarrier -> Value
barrierJson b =
    object
        [ "scope" .= barrierScopeName (barrierScope b)
        , "reason" .= T.pack (show (barrierReason b))
        , "detail" .= barrierDetail b
        ]

barrierScopeName :: BarrierScope -> T.Text
barrierScopeName GlobalScope = "global"
barrierScopeName (RelationScope r) = "relation:" <> nameText (nameRefName r)
barrierScopeName (SchemaScope n) = "schema:" <> nameText n
barrierScopeName (ObjectScope o) = "object:" <> objectRefName o
barrierScopeName SessionStateScope = "session"
barrierScopeName TransactionStateScope = "transaction"

objectRefName :: ObjectRef -> T.Text
objectRefName (ObjectRelation r) = "relation:" <> nameText (nameRefName r)
objectRefName (ObjectIndex i) = "index:" <> indexRefName i
objectRefName (ObjectTrigger t) = "trigger:" <> triggerRefName t
objectRefName (ObjectSequence s) = "sequence:" <> nameText (nameRefName s)
objectRefName (ObjectType t) = "type:" <> nameText t
objectRefName (ObjectSchema s) = "schema:" <> nameText s
objectRefName (ObjectConstraint (ConstraintRef t n)) = "constraint:" <> nameText (nameRefName t) <> "." <> identText n

sessionEffectName :: SessionEffect -> T.Text
sessionEffectName SetVariable {} = "SetVariable"
sessionEffectName (SetSearchPath _ _) = "SetSearchPath"
sessionEffectName (SetNamespace _ _) = "SetNamespace"
sessionEffectName (SetSessionUser _ _) = "SetSessionUser"
sessionEffectName (ResetSessionUser _) = "ResetSessionUser"
sessionEffectName (SetCurrentUser _ _) = "SetCurrentUser"
sessionEffectName (ResetCurrentUser _) = "ResetCurrentUser"
sessionEffectName (SetAutocommit _ _) = "SetAutocommit"
sessionEffectName (SetForeignKeyChecks _ _) = "SetForeignKeyChecks"
sessionEffectName (ResetForeignKeyChecks _) = "ResetForeignKeyChecks"
sessionEffectName (ResetVariable _ _) = "ResetVariable"
sessionEffectName (ResetAll _) = "ResetAll"

txEffectName :: TxEffect -> T.Text
txEffectName (TxBegin _) = "TxBegin"
txEffectName (TxCommit _) = "TxCommit"
txEffectName TxRollbackAll = "TxRollbackAll"
txEffectName (TxRollbackTo _) = "TxRollbackTo"
txEffectName (TxSavepoint _) = "TxSavepoint"
txEffectName (TxReleaseSavepoint _) = "TxReleaseSavepoint"
txEffectName (TxSetReadOnly _) = "TxSetReadOnly"

{- | Trigger body and WHEN-clause effects fire when the trigger fires, not at
CREATE time, so they are reported separately from the statement's own reads.
-}
triggerEffectsJson :: [CatalogOp] -> Value
triggerEffectsJson ops =
    object ["reads" .= readsJson, "writes" .= writesJson, "other" .= otherJson]
    where
        effects = concatMap triggerBodyEffects ops
        whenReads = concatMap triggerWhenOf ops
        readsJson = map readFactJson (whenReads ++ [r | TriggerRead r <- effects])
        writesJson = [dmlEventJson e | TriggerDml e <- effects]
        otherJson =
            [T.pack (show b) | TriggerBarrier b <- effects]
                ++ [sessionEffectName s | TriggerSession s <- effects]
                ++ ["sequence-write" :: T.Text | TriggerWriteSequence _ <- effects]

triggerWhenOf :: CatalogOp -> [ReadFact]
triggerWhenOf (DefineTriggerOp t) = triggerWhenReads t
triggerWhenOf _ = []

triggerBodyEffects :: CatalogOp -> [TriggerEffect]
triggerBodyEffects (DefineTriggerOp t) = case triggerBody t of
    TriggerInline effs -> effs
    TriggerFunction _ _ -> []
    TriggerOpaque -> []
triggerBodyEffects _ = []

dmlEventJson :: DmlEvent -> Value
dmlEventJson event = case event of
    DmlInsert t cols -> trigWrite "insert" t cols
    DmlUpdate t cols -> trigWrite "update" t cols
    DmlMerge t cols -> trigWrite "merge" t cols
    DmlDelete t _ -> trigWrite "delete" t []
    DmlTruncate t _ -> trigWrite "truncate" t []
    where
        trigWrite kind t cols =
            object
                ["target" .= nameText (nameRefName t), "kind" .= (kind :: T.Text), "columns" .= map identText cols]
