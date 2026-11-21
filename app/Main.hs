module Main (main) where

import Completion (ShellType (..), generateCompletion, parseShellType)
import Control.Applicative ((<|>))
import Control.Monad (forM_, unless, when)
import Data.Aeson (Value, encode, object, toJSON, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Char (toLower)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import GHC.Clock (getMonotonicTimeNSec)
import Occam qualified
import Occam.AtomicFile (writeAtomicBytes, writeAtomicLazyBytes)
import Occam.Evaluation (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    EvaluationRequest (..),
    EvaluationResult (..),
    EvaluationStatus (..),
    ReductionActionResult (..),
    Stats (..),
    metricPolicyForReduction,
    parseSQLDialect,
    runEvaluationRequest,
    sqlDialectName,
 )
import Occam.Logger (
    LogConfig (..),
    LogFormat (..),
    LogLevel (..),
    Logger,
    logError,
    logInfo',
    logWarn,
    withLogger,
 )
import Occam.Oracle (
    OracleConfig (..),
    OracleObservationDetails (..),
    prepareOracle,
    shellOracleConfig,
    withCandidateWorkdir,
 )
import Occam.Parser (
    ParseOptions (..),
    ParserError (..),
    SQLDialect (..),
    ServerConfig (..),
    countStatements,
    defaultParseOptions,
    dialectName,
    factDumpJson,
    parseSQL,
 )
import Occam.Reducer (
    AuditStats (..),
    ColumnMode (..),
    ReductionConfig (..),
    ReductionEvidence (..),
    ReductionResult (..),
    ReductionStage (..),
    StageEvent (..),
    columnModeName,
    defaultReductionConfig,
    reduceSQLWithEvidence,
    statementAuditWithLogger,
    statementDdmin,
 )
import Occam.UI (
    FinishSummary (..),
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
    strArgument,
    strOption,
    switch,
    value,
    (<**>),
 )
import System.Directory (
    doesFileExist,
    makeAbsolute,
 )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitFailure, exitSuccess, exitWith)
import System.FilePath (dropExtension, normalise, takeExtension, (</>))
import System.IO (hFlush, hIsTerminalDevice, hPutStrLn, stderr, stdout)
import System.Timeout (timeout)

-- | Tri-state distinguishing defaults from explicit CLI flags.
data Specified a
    = Default a
    | Explicit a
    deriving (Eq, Show, Functor)

isExplicit :: Specified a -> Bool
isExplicit (Explicit _) = True
isExplicit (Default _) = False

getSpecified :: Specified a -> a
getSpecified (Default x) = x
getSpecified (Explicit x) = x

-- ----------------------------------------------------------------------------
-- Color Mode
-- ----------------------------------------------------------------------------

data ColorMode = ColorAuto | ColorAlways | ColorNever
    deriving (Eq, Show)

parseColorMode :: String -> Either String ColorMode
parseColorMode s = case map toLower s of
    "auto" -> Right ColorAuto
    "always" -> Right ColorAlways
    "never" -> Right ColorNever
    _ -> Left "Expected 'auto', 'always', or 'never'"

resolveColor :: ColorMode -> Bool -> IO Bool
resolveColor ColorAlways _ = pure True
resolveColor ColorNever _ = pure False
resolveColor ColorAuto isTTY = do
    noColor <- isJust <$> lookupEnv "NO_COLOR"
    pure (isTTY && not noColor)

colorModeName :: ColorMode -> Text
colorModeName ColorAuto = "auto"
colorModeName ColorAlways = "always"
colorModeName ColorNever = "never"

logFormatName :: LogFormat -> Text
logFormatName FormatText = "text"
logFormatName FormatJSON = "json"

-- ----------------------------------------------------------------------------
-- Actions and Options ADTs
-- ----------------------------------------------------------------------------

data Action
    = ActionCompletion !ShellType
    | ActionListDialects
    | ActionListColumnModes
    | ActionListStrategies
    | ActionPrintDefaults
    | ActionNormal !RawOptions

data ReductionStrategy = StrategyFull | StrategyDdminOnly | StrategyAuditOnly
    deriving (Eq, Show)

strategyName :: ReductionStrategy -> T.Text
strategyName StrategyFull = "full"
strategyName StrategyDdminOnly = "ddmin-only"
strategyName StrategyAuditOnly = "audit-only"

data RawOptions = RawOptions
    { rawInputFile :: !FilePath
    , rawOracleCmd :: !(Maybe String)
    , rawOut :: !(Maybe FilePath)
    , rawInPlace :: !Bool
    , rawStatsFile :: !(Maybe FilePath)
    , rawResetCmd :: !(Maybe String)
    , rawTimeoutSecs :: !Double
    , rawLogLevel :: !(Specified LogLevel)
    , rawLogFormat :: !LogFormat
    , rawVerbose :: !Bool
    , rawQuiet :: !Bool
    , rawColor :: !ColorMode
    , rawBudgetSecs :: !(Maybe Double)
    , rawMaxOracleExecutions :: !(Maybe Int)
    , rawJobs :: !Int
    , rawSlicing :: !(Specified Bool)
    , rawClosure :: !(Specified Bool)
    , rawIntraStatement :: !(Specified Bool)
    , rawColumnMode :: !(Specified ColumnMode)
    , rawUnrestricted :: !(Specified Bool)
    , rawCache :: !(Specified Bool)
    , rawDialect :: !SQLDialect
    , rawStrategy :: !(Specified ReductionStrategy)
    , rawDumpFacts :: !Bool
    , rawDumpFactsFkOn :: !Bool
    }
    deriving (Eq, Show)

-- | Default CLI options.
defaultRawOptions :: RawOptions
defaultRawOptions =
    RawOptions
        { rawInputFile = "INPUT.sql"
        , rawOracleCmd = Nothing
        , rawOut = Nothing
        , rawInPlace = False
        , rawStatsFile = Nothing
        , rawResetCmd = Nothing
        , rawTimeoutSecs = 10.0
        , rawLogLevel = Default LevelWarn
        , rawLogFormat = FormatText
        , rawVerbose = False
        , rawQuiet = False
        , rawColor = ColorAuto
        , rawBudgetSecs = Nothing
        , rawMaxOracleExecutions = Nothing
        , rawJobs = 1
        , rawSlicing = Default True
        , rawClosure = Default True
        , rawIntraStatement = Default False
        , rawColumnMode = Default ColumnOff
        , rawUnrestricted = Default True
        , rawCache = Default True
        , rawDialect = GenericSQL
        , rawStrategy = Default StrategyFull
        , rawDumpFacts = False
        , rawDumpFactsFkOn = False
        }

-- | Log level used when the format is not explicitly configured as JSON.
defaultLogLevelForFormat :: LogFormat -> LogLevel
defaultLogLevelForFormat FormatJSON = LevelInfo
defaultLogLevelForFormat FormatText = getSpecified (rawLogLevel defaultRawOptions)

-- | Validated, strategy-resolved CLI options.
data ValidatedOptions = ValidatedOptions
    { valInputFile :: !FilePath
    , valOracleCmd :: !(Maybe String)
    , valOut :: !(Maybe FilePath)
    , valInPlace :: !Bool
    , valStatsFile :: !(Maybe FilePath)
    , valResetCmd :: !(Maybe String)
    , valTimeoutSecs :: !Double
    , valLogLevel :: !(Specified LogLevel)
    , valLogFormat :: !LogFormat
    , valVerbose :: !Bool
    , valQuiet :: !Bool
    , valColor :: !ColorMode
    , valBudgetSecs :: !(Maybe Double)
    , valMaxOracleExecutions :: !(Maybe Int)
    , valJobs :: !Int
    , valSlicing :: !Bool
    , valClosure :: !Bool
    , valIntraStatement :: !Bool
    , valColumnMode :: !ColumnMode
    , valUnrestricted :: !Bool
    , valCache :: !Bool
    , valDialect :: !SQLDialect
    , valStrategy :: !ReductionStrategy
    , valDumpFacts :: !Bool
    , valDumpFactsFkOn :: !Bool
    }
    deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- Two-Phase Validation
-- ----------------------------------------------------------------------------

validateCliOptions :: RawOptions -> Either String ValidatedOptions
validateCliOptions RawOptions {..} = do
    -- 1. Mutually exclusive flags
    when (rawVerbose && rawQuiet) $
        Left "Cannot specify both --verbose and --quiet"

    when (rawInPlace && isJust rawOut) $
        Left "Cannot specify both --in-place and --out"

    when (rawInPlace && rawInputFile == "-") $
        Left "Cannot modify stdin in place (--in-place with '-' input)"

    -- 2. Strategy conflicts
    case rawStrategy of
        Explicit StrategyDdminOnly -> do
            when (isExplicit rawSlicing && getSpecified rawSlicing) $
                Left "--slicing conflicts with --strategy ddmin-only (slicing is disabled in ddmin-only mode)"
            when (isExplicit rawClosure && getSpecified rawClosure) $
                Left
                    "--closure conflicts with --strategy ddmin-only (closure search is disabled in ddmin-only mode)"
            when (isExplicit rawIntraStatement && getSpecified rawIntraStatement) $
                Left
                    "--intra-statement conflicts with --strategy ddmin-only (intra-statement rewrites are disabled in ddmin-only mode)"
            when (isExplicit rawUnrestricted && getSpecified rawUnrestricted) $
                Left
                    "--unrestricted conflicts with --strategy ddmin-only (unrestricted fallback is disabled in ddmin-only mode)"
        Explicit StrategyAuditOnly -> do
            when (isExplicit rawSlicing && getSpecified rawSlicing) $
                Left "--slicing conflicts with --strategy audit-only"
            when (isExplicit rawClosure && getSpecified rawClosure) $
                Left "--closure conflicts with --strategy audit-only"
            when (isExplicit rawIntraStatement && getSpecified rawIntraStatement) $
                Left "--intra-statement conflicts with --strategy audit-only"
            when (isExplicit rawUnrestricted && getSpecified rawUnrestricted) $
                Left "--unrestricted conflicts with --strategy audit-only"
        _ -> pure ()

    -- 3. Column mode prerequisite check
    let colMode = getSpecified rawColumnMode
        intra = getSpecified rawIntraStatement
    when (colMode /= ColumnOff && not intra) $
        Left
            "--column-mode single|atomic requires --intra-statement to be enabled (add '--intra-statement')"

    -- 4. Numeric bounds
    let maxSeconds = fromIntegral (maxBound :: Int) / 1000000 :: Double
    when
        ( rawTimeoutSecs <= 0
            || rawTimeoutSecs > maxSeconds
            || isNaN rawTimeoutSecs
            || isInfinite rawTimeoutSecs
        )
        $ Left "--timeout must be a positive finite number of seconds"
    when (rawJobs <= 0) $
        Left "--jobs must be positive"
    case rawBudgetSecs of
        Just b
            | b <= 0 || b > maxSeconds || isNaN b || isInfinite b ->
                Left "--budget must be a positive finite number of seconds"
        _ -> pure ()
    case rawMaxOracleExecutions of
        Just m | m <= 0 -> Left "--max-oracle-executions must be positive"
        _ -> pure ()

    -- 5. Effective flags according to strategy
    let effSlicing = case getSpecified rawStrategy of
            StrategyFull -> getSpecified rawSlicing
            StrategyDdminOnly -> False
            StrategyAuditOnly -> False
        effClosure = case getSpecified rawStrategy of
            StrategyFull -> getSpecified rawClosure
            StrategyDdminOnly -> False
            StrategyAuditOnly -> False
        effIntra = case getSpecified rawStrategy of
            StrategyFull -> getSpecified rawIntraStatement
            StrategyDdminOnly -> False
            StrategyAuditOnly -> False
        effUnrestricted = case getSpecified rawStrategy of
            StrategyFull -> getSpecified rawUnrestricted
            StrategyDdminOnly -> False
            StrategyAuditOnly -> False

    pure
        ValidatedOptions
            { valInputFile = rawInputFile
            , valOracleCmd = rawOracleCmd
            , valOut = rawOut
            , valInPlace = rawInPlace
            , valStatsFile = rawStatsFile
            , valResetCmd = rawResetCmd
            , valTimeoutSecs = rawTimeoutSecs
            , valLogLevel = rawLogLevel
            , valLogFormat = rawLogFormat
            , valVerbose = rawVerbose
            , valQuiet = rawQuiet
            , valColor = rawColor
            , valBudgetSecs = rawBudgetSecs
            , valMaxOracleExecutions = rawMaxOracleExecutions
            , valJobs = rawJobs
            , valSlicing = effSlicing
            , valClosure = effClosure
            , valIntraStatement = effIntra
            , valColumnMode = colMode
            , valUnrestricted = effUnrestricted
            , valCache = getSpecified rawCache
            , valDialect = rawDialect
            , valStrategy = getSpecified rawStrategy
            , valDumpFacts = rawDumpFacts
            , valDumpFactsFkOn = rawDumpFactsFkOn
            }

-- ----------------------------------------------------------------------------
-- Parsers
-- ----------------------------------------------------------------------------

actionParser :: Parser Action
actionParser =
    flag'
        ActionListDialects
        ( long "list-dialects"
            <> help "List supported SQL dialects and exit"
        )
        <|> flag'
            ActionListColumnModes
            ( long "list-column-modes"
                <> help "List supported column co-reduction modes and exit"
            )
        <|> flag'
            ActionListStrategies
            ( long "list-strategies"
                <> help "List supported reduction strategies and exit"
            )
        <|> flag'
            ActionPrintDefaults
            ( long "print-defaults"
                <> help "Print default configuration in JSON format and exit"
            )
        <|> ( ActionCompletion
                <$> option
                    (eitherReader parseShellType)
                    ( long "completion"
                        <> metavar "SHELL"
                        <> help "Generate shell completion script (bash, zsh, fish) and exit"
                    )
            )
        <|> (ActionNormal <$> rawOptionsParser)

rawOptionsParser :: Parser RawOptions
rawOptionsParser =
    RawOptions
        <$> strArgument
            ( metavar "INPUT.sql"
                <> help "Path to fuzz SQL input, or '-' for stdin"
                <> completer (bashCompleter "file")
            )
        <*> optional
            ( strOption
                ( long "oracle"
                    <> short 'c'
                    <> metavar "COMMAND"
                    <> help "Interestingness predicate command (SQL on stdin; exit 0 accepts)"
                )
            )
        <*> optional
            ( strOption
                ( long "out"
                    <> short 'o'
                    <> metavar "PATH"
                    <> help "Output SQL file path, or '-' for stdout (default: <input>.min.sql or stdout)"
                    <> completer (bashCompleter "file")
                )
            )
        <*> switch
            ( long "in-place"
                <> short 'i'
                <> help "Modify input SQL file in place (creates backup <input>.bak)"
            )
        <*> optional
            ( strOption
                ( long "stats"
                    <> metavar "FILE"
                    <> help "Write JSON reduction statistics to FILE"
                    <> completer (bashCompleter "file")
                )
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
                <> value (rawTimeoutSecs defaultRawOptions)
                <> help "Per-execution timeout in seconds (default: 10.0)"
            )
        <*> ( ( Explicit
                    <$> option
                        (eitherReader parseLogLevel)
                        ( long "log-level"
                            <> metavar "LEVEL"
                            <> help "Log severity level: debug, info, warn, or error (default: warn)"
                        )
              )
                <|> pure (rawLogLevel defaultRawOptions)
            )
        <*> option
            (eitherReader parseLogFormat)
            ( long "log-format"
                <> metavar "FORMAT"
                <> value (rawLogFormat defaultRawOptions)
                <> help "Log output format: text or json (default: text)"
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
        <*> option
            (eitherReader parseColorMode)
            ( long "color"
                <> metavar "WHEN"
                <> value (rawColor defaultRawOptions)
                <> help "Colorize terminal output: auto, always, or never (default: auto; honors NO_COLOR)"
            )
        <*> optional
            ( option
                auto
                ( long "budget"
                    <> long "time-limit"
                    <> metavar "SECONDS"
                    <> help "Shared wall-clock cap for entire reduction session"
                )
            )
        <*> optional
            ( option
                auto
                ( long "max-oracle-executions"
                    <> long "max-evals"
                    <> metavar "N"
                    <> help "Maximum uncached oracle attempts"
                )
            )
        <*> option
            (eitherReader parseJobs)
            ( short 'j'
                <> long "jobs"
                <> metavar "N"
                <> value (rawJobs defaultRawOptions)
                <> help "Maximum concurrent oracle executions (default: 1)"
            )
        <*> toggleSpecified
            "slicing"
            "no-slicing"
            "dependency slicing"
            (getSpecified (rawSlicing defaultRawOptions))
        <*> toggleSpecified
            "closure"
            "no-closure"
            "dependency closure"
            (getSpecified (rawClosure defaultRawOptions))
        <*> toggleSpecified
            "intra-statement"
            "no-intra-statement"
            "intra-statement reductions"
            (getSpecified (rawIntraStatement defaultRawOptions))
        <*> columnModeParser
        <*> toggleSpecified
            "unrestricted"
            "no-unrestricted"
            "unrestricted fallback search"
            (getSpecified (rawUnrestricted defaultRawOptions))
        <*> toggleSpecified "cache" "no-cache" "candidate caching" (getSpecified (rawCache defaultRawOptions))
        <*> option
            (eitherReader parseSQLDialect)
            ( long "dialect"
                <> short 'd'
                <> metavar "DIALECT"
                <> value (rawDialect defaultRawOptions)
                <> help
                    "SQL dependency dialect: generic (default), postgresql, mysql, sqlite, duckdb, clickhouse, oracle, ansi, bigquery, databricks, hive, mssql, redshift, snowflake, spark, or teradata"
            )
        <*> strategyParser
        <*> switch
            ( long "dump-facts"
                <> internal
            )
        <*> switch
            ( long "dump-facts-fk-on"
                <> internal
            )

toggleSpecified :: String -> String -> String -> Bool -> Parser (Specified Bool)
toggleSpecified enableName disableName description def =
    flag' (Explicit True) (long enableName <> help ("Enable " ++ description))
        <|> flag' (Explicit False) (long disableName <> help ("Disable " ++ description))
        <|> pure (Default def)

strategyParser :: Parser (Specified ReductionStrategy)
strategyParser =
    option
        (Explicit <$> eitherReader parseStrategy)
        ( long "strategy"
            <> metavar "STRATEGY"
            <> help "Reduction strategy: full (default), ddmin-only, or audit-only"
        )
        <|> pure (rawStrategy defaultRawOptions)

columnModeParser :: Parser (Specified ColumnMode)
columnModeParser =
    option
        (Explicit <$> eitherReader parseColumnMode)
        ( long "column-mode"
            <> metavar "MODE"
            <> help "Column co-reduction mode: off (default), single, or atomic (requires --intra-statement)"
        )
        <|> pure (rawColumnMode defaultRawOptions)

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

parseStrategy :: String -> Either String ReductionStrategy
parseStrategy s = case map toLower s of
    "full" -> Right StrategyFull
    "ddmin-only" -> Right StrategyDdminOnly
    "audit-only" -> Right StrategyAuditOnly
    _ -> Left "Expected full, ddmin-only, or audit-only"

versionOption :: Parser (a -> a)
versionOption =
    infoOption
        ("Occam v" ++ Occam.version)
        ( long "version"
            <> short 'v'
            <> help "Show version information"
        )

optsInfo :: ParserInfo Action
optsInfo =
    info
        (actionParser <**> versionOption <**> helper)
        ( fullDesc
            <> progDesc "Occam is a dependency-aware test case reducer for multi-statement SQL."
            <> header "occam - Multi-Statement SQL Reducer"
        )

-- ----------------------------------------------------------------------------
-- Exit Codes Contract
-- 0: Success (reduction finished or action completed)
-- 1: Oracle rejected initial SQL or internal error
-- 2: Usage / CLI validation error
-- 124: Budget timeout
-- ----------------------------------------------------------------------------

exitCliError :: IO a
exitCliError = exitWith (ExitFailure 2)

exitTimeout :: IO a
exitTimeout = exitWith (ExitFailure 124)

defaultMinPath :: FilePath -> FilePath
defaultMinPath "-" = "minimal.sql"
defaultMinPath fp
    | takeExtension fp == ".sql" = dropExtension fp ++ ".min.sql"
    | otherwise = fp ++ ".min.sql"

-- ----------------------------------------------------------------------------
-- Atomic File Output
-- ----------------------------------------------------------------------------

writeAtomicFile :: FilePath -> BS.ByteString -> IO ()
writeAtomicFile "-" content = do
    BS.putStr content
    hFlush stdout
writeAtomicFile path content = writeAtomicBytes path content

writeAtomicLazy :: FilePath -> BSL.ByteString -> IO ()
writeAtomicLazy "-" content = do
    BSL.putStr content
    hFlush stdout
writeAtomicLazy path content = writeAtomicLazyBytes path content

-- | Ensure stats and output file paths do not collide with inputs or backups.
validateOutputPaths :: FilePath -> FilePath -> Bool -> Bool -> Maybe FilePath -> IO ()
validateOutputPaths input target writesStdout inPlace statsFile =
    forM_ statsFile \stats -> do
        when (stats == "-" && writesStdout) $ do
            hPutStrLn stderr "error: --stats - cannot be combined with writing reduced SQL to stdout"
            exitCliError
        unless (stats == "-") $ do
            statsAbs <- absoluteNormalized stats
            when (input /= "-") $ do
                inputAbs <- absoluteNormalized input
                when (statsAbs == inputAbs) $ do
                    hPutStrLn stderr "error: --stats path must differ from the input path"
                    exitCliError
                when inPlace $ do
                    backupAbs <- absoluteNormalized (input ++ ".bak")
                    when (statsAbs == backupAbs) $ do
                        hPutStrLn stderr "error: --stats path must differ from the in-place backup path"
                        exitCliError
            unless (null target) $ do
                targetAbs <- absoluteNormalized target
                when (statsAbs == targetAbs) $ do
                    hPutStrLn stderr "error: --stats path must differ from the output path"
                    exitCliError
    where
        absoluteNormalized path = normalise <$> makeAbsolute path

-- ----------------------------------------------------------------------------
-- Main
-- ----------------------------------------------------------------------------

main :: IO ()
main = execParser optsInfo >>= runAction

runAction :: Action -> IO ()
runAction (ActionCompletion shell) = do
    putStr (generateCompletion shell)
    exitSuccess
runAction ActionListDialects = do
    putStrLn "Supported SQL dialects:"
    forM_ ([minBound .. maxBound] :: [SQLDialect]) $ \d ->
        putStrLn ("  " ++ T.unpack (dialectName d))
    exitSuccess
runAction ActionListColumnModes = do
    putStrLn "Supported column co-reduction modes:"
    putStrLn "  off       Disable column co-reduction (default)"
    putStrLn "  single    List per-column single sites (requires --intra-statement)"
    putStrLn "  atomic    List per-column sites and joint bundles (requires --intra-statement)"
    exitSuccess
runAction ActionListStrategies = do
    putStrLn "Supported reduction strategies:"
    putStrLn
        "  full          Full multi-stage reduction: slice -> closure -> intra -> audit -> fallback (default)"
    putStrLn "  ddmin-only    Fast statement ddmin and audit only"
    putStrLn "  audit-only    Statement deletion audit only"
    exitSuccess
runAction ActionPrintDefaults = do
    let opts = defaultRawOptions
        defaults =
            object
                [ "timeout" .= rawTimeoutSecs opts
                , "jobs" .= rawJobs opts
                , "dialect" .= dialectName (rawDialect opts)
                , "strategy" .= strategyName (getSpecified (rawStrategy opts))
                , "column_mode" .= columnModeName (getSpecified (rawColumnMode opts))
                , "slicing" .= getSpecified (rawSlicing opts)
                , "closure" .= getSpecified (rawClosure opts)
                , "intra_statement" .= getSpecified (rawIntraStatement opts)
                , "unrestricted" .= getSpecified (rawUnrestricted opts)
                , "cache" .= getSpecified (rawCache opts)
                , "log_level" .= getSpecified (rawLogLevel opts)
                , "log_level_json" .= defaultLogLevelForFormat FormatJSON
                , "log_format" .= logFormatName (rawLogFormat opts)
                , "color" .= colorModeName (rawColor opts)
                ]
    BSL.putStr (encode defaults <> "\n")
    exitSuccess
runAction (ActionNormal rawOpts) =
    case validateCliOptions rawOpts of
        Left err -> do
            hPutStrLn stderr ("error: " ++ err)
            exitCliError
        Right validatedOpts -> runOccam validatedOpts

runOccam :: ValidatedOptions -> IO ()
runOccam opts@ValidatedOptions {..} = do
    isStderrTTY <- hIsTerminalDevice stderr
    useColor <- resolveColor valColor isStderrTTY
    let isVerboseMode = valVerbose || getSpecified valLogLevel == LevelDebug
        resolvedLevel = case valLogLevel of
            Explicit lvl -> lvl
            Default defLvl
                | valLogFormat == FormatJSON -> LevelInfo
                | otherwise -> defLvl
        effectiveLevel
            | valQuiet = LevelError
            | isVerboseMode = LevelDebug
            | otherwise = resolvedLevel
        effectiveFormat = valLogFormat
        logCfg =
            LogConfig
                { logMinLevel = effectiveLevel
                , logFormat = effectiveFormat
                , logUseColor = useColor
                }
        runWithLogger logger = runOccamWithLogger logger useColor opts
    withLogger logCfg runWithLogger

runOccamWithLogger :: Logger -> Bool -> ValidatedOptions -> IO ()
runOccamWithLogger logger useColor ValidatedOptions {..} = do
    when valDumpFacts do
        dumpStatementFacts valDialect valDumpFactsFkOn valInputFile
        exitSuccess

    oracleCmd <- requireOracleCommand valOracleCmd
    (initialSQL, initialStmts, initialBytes) <- loadInitialInput logger valDialect valInputFile

    ui <-
        newUI
            UIConfig
                { uiTargetFile = valInputFile
                , uiInitialBytes = initialBytes
                , uiInitialStmts = initialStmts
                , uiDialect = sqlDialectName valDialect
                , uiQuiet = valQuiet
                , uiVerbose = valVerbose || getSpecified valLogLevel == LevelDebug
                , uiUseColor = useColor
                }

    let baseOracleConfig =
            shellOracleConfig oracleCmd (round (valTimeoutSecs * 1000000)) valResetCmd
        reductionConfig =
            buildReductionConfig
                valSlicing
                valClosure
                valIntraStatement
                valColumnMode
                valUnrestricted
                valDialect
        configuredBudget = buildBudgetConfig valCache valJobs valBudgetSecs valMaxOracleExecutions initialStmts

    logInfo'
        logger
        "cli"
        "Starting reduction session"
        [ ("input_file", toJSON valInputFile)
        , ("initial_bytes", toJSON initialBytes)
        , ("initial_statements", toJSON initialStmts)
        , ("oracle_cmd", toJSON oracleCmd)
        , ("jobs", toJSON valJobs)
        , ("strategy", toJSON (strategyName valStrategy))
        ]

    let totalBudgetMicros = round . (* 1000000) <$> valBudgetSecs :: Maybe Integer
    (initialStart, initialEnd, preflightDetails, preflightMicros) <-
        runPreflight baseOracleConfig initialSQL totalBudgetMicros
    let oracleCfg = baseOracleConfig
        budgetConfig =
            configuredBudget
                { evaluationPreflightMicros = preflightMicros
                , evaluationPreflightAttempts = 1
                , evaluationPreflightResetMicros = Just (resetElapsedMicros preflightDetails)
                , evaluationPreflightOracleMicros = oracleElapsedMicros preflightDetails
                }
    logInfo'
        logger
        "oracle"
        "Initial predicate check passed"
        [ ("initial_statements", toJSON initialStmts)
        , ("initial_bytes", toJSON initialBytes)
        ]

    when (valJobs > 1 && isNothing valResetCmd) do
        logWarn
            logger
            "cli"
            "running with --jobs > 1 without --reset may lead to shared database state contamination"

    (writeToStdout, targetMinimalPath, statsPath) <-
        resolveOutputTargets valInputFile valOut valInPlace valStatsFile

    metricsPolicy <- case valStrategy of
        StrategyFull -> case metricPolicyForReduction reductionConfig valJobs of
            Left err -> do
                logError logger "cli" err
                exitCliError
            Right pol -> pure (Just pol)
        StrategyDdminOnly -> pure Nothing
        StrategyAuditOnly -> pure Nothing

    let reductionAction reporter reducerLogger pool = case valStrategy of
            StrategyFull -> do
                ReductionResult sql evidence <-
                    reduceSQLWithEvidence reporter reductionConfig reducerLogger pool initialSQL
                pure (ReductionActionResult sql evidence)
            StrategyDdminOnly -> do
                sql <- statementDdmin (reductionDialect reductionConfig) pool initialSQL
                pure (ReductionActionResult sql Nothing)
            StrategyAuditOnly -> do
                auditStart <- toInteger <$> getMonotonicTimeNSec
                reporter (StageStarted StageStatementAudit)
                (sql, stats) <-
                    statementAuditWithLogger reducerLogger (reductionDialect reductionConfig) pool initialSQL
                auditEnd <- toInteger <$> getMonotonicTimeNSec
                let elapsed = fromIntegral (auditEnd - auditStart) / 1000000000 :: Double
                reporter (StageFinished StageStatementAudit elapsed (BS.length initialSQL) (BS.length sql))
                pure (ReductionActionResult sql (Just (ReductionEvidence sql (auditMinimalityConfirmed stats))))

    result <-
        withCandidateWorkdir $ \tmpDir -> do
            let checkpointPath = tmpDir </> "occam-checkpoint.sql"
            runEvaluationRequest
                EvaluationRequest
                    { requestOnProgress = uiOnProgress ui
                    , requestMetricsPolicy = metricsPolicy
                    , requestConfig = budgetConfig
                    , requestDialect = valDialect
                    , requestOracleConfig = oracleCfg
                    , requestPreflightObservation = Just (detailedObservation preflightDetails)
                    , requestLogger = logger
                    , requestInitialSQL = initialSQL
                    , requestCheckpoint = checkpointPath
                    , requestAction = reductionAction
                    }

    let reducedSQL = evaluatedSQL result
        evaluation = evaluationMetrics result
        totalHits = evaluationCacheHits evaluation
        totalMisses = evaluationCacheMisses evaluation
        totalLookups = totalHits + totalMisses
        hitRatio =
            if totalLookups > 0
                then fromIntegral totalHits / fromIntegral totalLookups
                else 0.0
    reducedStmts <- countStatementsForCLI logger valDialect reducedSQL
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
                , oracle_executions = totalOracleExecutions
                , oracle_attempts = totalOracleAttempts
                , initial_oracle_observations = evaluationPreflightAttempts budgetConfig
                , cache_hits = totalHits
                , cache_misses = totalMisses
                , cache_joins = evaluationCacheJoins evaluation
                , cache_hit_ratio = hitRatio
                , cancelled_observations = evaluationCancelledObservations evaluation
                , status = evaluationStatus evaluation
                , reduction_settings =
                    reductionSettings valCache reductionConfig valStrategy
                , evaluation_budget = budgetConfig
                , evaluation_metrics = evaluation
                , initial_oracle_seconds = initialOracleSeconds
                , elapsed_seconds = evaluationTotalSeconds evaluation
                }

    writeReductionOutputs
        logger
        valInputFile
        valInPlace
        writeToStdout
        targetMinimalPath
        statsPath
        initialSQL
        reducedSQL
        stats
        evaluation

    logInfo'
        logger
        "cli"
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

    uiFinish
        ui
        FinishSummary
            { finishMetrics = evaluation
            , finishInitialBytes = initialBytes
            , finishReducedBytes = reducedBytes
            , finishInitialStmts = initialStmts
            , finishReducedStmts = reducedStmts
            , finishMinimalPath = targetMinimalPath
            , finishStatsPath = statsPath
            }
    case evaluationStatus evaluation of
        EvaluationCompleted
            | evaluationValidationPassed evaluation -> exitSuccess
            | otherwise -> exitFailure
        EvaluationTimeout -> exitTimeout
        EvaluationExecutionBudget -> exitTimeout
        _ -> exitFailure

requireOracleCommand :: Maybe String -> IO String
requireOracleCommand (Just command) = pure command
requireOracleCommand Nothing = do
    hPutStrLn stderr "error: --oracle COMMAND is required (unless --dump-facts is given)"
    hPutStrLn stderr " note: specify predicate with -c or --oracle (e.g. -c './check.sh')"
    exitCliError

loadInitialInput :: Logger -> SQLDialect -> FilePath -> IO (BS.ByteString, Int, Int)
loadInitialInput logger dialect inputFile = do
    sql <-
        if inputFile == "-"
            then BS.getContents
            else do
                fileExists <- doesFileExist inputFile
                unless fileExists do
                    let msg = "cannot open '" ++ inputFile ++ "': No such file or directory"
                    logError logger "cli" (T.pack msg)
                    hPutStrLn stderr ("error: " ++ msg)
                    exitFailure
                BS.readFile inputFile
    statements <- countStatementsForCLI logger dialect sql
    pure (sql, statements, BS.length sql)

buildReductionConfig ::
    Bool -> Bool -> Bool -> ColumnMode -> Bool -> SQLDialect -> ReductionConfig
buildReductionConfig slicing closure intra columnMode unrestricted dialect =
    defaultReductionConfig
        { reductionSlicing = slicing
        , reductionClosure = closure
        , reductionIntraStatement = intra
        , reductionColumnMode = columnMode
        , reductionUnrestricted = unrestricted
        , reductionDialect = dialect
        }

buildBudgetConfig ::
    Bool -> Int -> Maybe Double -> Maybe Int -> Int -> EvaluationConfig
buildBudgetConfig cacheEnabled jobs budgetSecs maxExecutions initialStmts =
    EvaluationConfig
        { evaluationCacheEnabled = cacheEnabled
        , evaluationJobs = jobs
        , evaluationTotalTimeoutMicros = round . (* 1000000) <$> budgetSecs
        , evaluationMaxOracleExecutions = maxExecutions
        , evaluationPreflightMicros = 0
        , evaluationPreflightAttempts = 0
        , evaluationPreflightResetMicros = Nothing
        , evaluationPreflightOracleMicros = Nothing
        , evaluationInitialStatements = Just initialStmts
        }

runPreflight ::
    OracleConfig ->
    BS.ByteString ->
    Maybe Integer ->
    IO (UTCTime, UTCTime, OracleObservationDetails, Int)
runPreflight oracleCfg initialSQL totalBudgetMicros = do
    initialStart <- getCurrentTime
    preflightStart <- toInteger <$> getMonotonicTimeNSec
    prepared <-
        runWithinBudget
            preflightStart
            totalBudgetMicros
            (prepareOracle oracleCfg initialSQL)
    details <- case prepared of
        Nothing -> do
            hPutStrLn stderr "error: initial predicate check exceeded the total budget"
            exitTimeout
        Just (Left err) -> do
            hPutStrLn stderr ("error: initial predicate check failed: " ++ T.unpack err)
            hPutStrLn stderr " note: oracle command rejected the original unreduced SQL input"
            exitFailure
        Just (Right checked) -> pure checked
    preflightEnd <- toInteger <$> getMonotonicTimeNSec
    initialEnd <- getCurrentTime
    let micros =
            fromInteger
                (min (toInteger (maxBound :: Int)) (max 0 ((preflightEnd - preflightStart) `div` 1000)))
    pure (initialStart, initialEnd, details, micros)

resolveOutputTargets ::
    FilePath ->
    Maybe FilePath ->
    Bool ->
    Maybe FilePath ->
    IO (Bool, FilePath, FilePath)
resolveOutputTargets inputFile outputFile inPlace statsFile = do
    isStdoutTTY <- hIsTerminalDevice stdout
    let writeToStdout =
            outputFile == Just "-"
                || (isNothing outputFile && not inPlace && (not isStdoutTTY || inputFile == "-"))
    targetMinimalPath <-
        if writeToStdout
            then pure ""
            else case outputFile of
                Just path -> pure path
                Nothing
                    | inPlace -> pure inputFile
                    | otherwise -> pure (defaultMinPath inputFile)
    let statsPath = fromMaybe "" statsFile
    validateOutputPaths inputFile targetMinimalPath writeToStdout inPlace statsFile
    pure (writeToStdout, targetMinimalPath, statsPath)

writeReductionOutputs ::
    Logger ->
    FilePath ->
    Bool ->
    Bool ->
    FilePath ->
    FilePath ->
    BS.ByteString ->
    BS.ByteString ->
    Stats ->
    EvaluationMetrics ->
    IO ()
writeReductionOutputs logger inputFile inPlace writeToStdout targetMinimalPath statsPath initialSQL reducedSQL stats evaluation = do
    let canWriteOutput =
            not (inPlace && not (evaluationValidationPassed evaluation))
    unless canWriteOutput do
        let msg = "refusing in-place modification because final candidate failed target validation"
        logError logger "cli" (T.pack msg)
        hPutStrLn stderr ("error: " ++ msg)

    unless (writeToStdout || not canWriteOutput) do
        when (inPlace && inputFile /= "-") do
            writeAtomicFile (inputFile ++ ".bak") initialSQL
        writeAtomicFile targetMinimalPath reducedSQL

    unless (null statsPath) do
        writeAtomicLazy statsPath (encode stats)

    when (writeToStdout && canWriteOutput) do
        BS.putStr reducedSQL
        hFlush stdout

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
            logError logger "parser" message
            hPutStrLn stderr ("error: " ++ T.unpack message)
            exitFailure
        Right count -> pure count

reductionSettings :: Bool -> ReductionConfig -> ReductionStrategy -> Value
reductionSettings cacheEnabled ReductionConfig {..} strategy =
    object
        [ "slicing" .= reductionSlicing
        , "closure" .= reductionClosure
        , "intra_statement" .= reductionIntraStatement
        , "column_mode" .= columnModeName reductionColumnMode
        , "unrestricted" .= reductionUnrestricted
        , "cache" .= cacheEnabled
        , "dialect" .= sqlDialectName reductionDialect
        , "strategy" .= strategyName strategy
        ]

dumpStatementFacts :: SQLDialect -> Bool -> FilePath -> IO ()
dumpStatementFacts dialect fkOn inputFile = do
    sql <-
        if inputFile == "-"
            then BS.getContents
            else do
                fileExists <- doesFileExist inputFile
                unless fileExists do
                    hPutStrLn stderr ("error: cannot open '" ++ inputFile ++ "': No such file or directory")
                    exitFailure
                BS.readFile inputFile
    let baseOptions = defaultParseOptions dialect
        parseOptions
            | fkOn =
                baseOptions
                    { parseServerConfig =
                        (parseServerConfig baseOptions) {serverForeignKeyChecks = Just True}
                    }
            | otherwise = baseOptions
    parsed <- parseSQL parseOptions sql
    case parsed of
        Left err -> do
            hPutStrLn stderr ("error: failed to parse input: " ++ T.unpack (parserErrorMessage err))
            exitFailure
        Right result -> BSL.putStr (encode (factDumpJson dialect result) <> "\n")
