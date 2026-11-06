{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import Numeric (showFFloat)
import Occam qualified
import Occam.Dependency (SQLDialect (..))
import Occam.Evaluation (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    EvaluationResult (..),
    parseSQLDialect,
    parseSearchStrategy,
    runEvaluatedReduction,
    searchStrategyName,
    sqlDialectName,
 )
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
 )
import Occam.Reducer (
    ReductionConfig (..),
    SearchStrategy (..),
    defaultReductionConfig,
    reduceSQLWithConfig,
 )
import Occam.TreeSitter (countStatements)
import Options.Applicative (
    Parser,
    ParserInfo,
    auto,
    eitherReader,
    execParser,
    flag',
    fullDesc,
    header,
    help,
    helper,
    info,
    infoOption,
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
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

data Options = Options
    { optInputFile :: !FilePath
    , optOracleCmd :: !String
    , optOutDir :: !FilePath
    , optResetCmd :: !(Maybe String)
    , optTimeoutSecs :: !Double
    , optLogLevel :: !String
    , optLogFormat :: !String
    , optVerbose :: !Bool
    , optQuiet :: !Bool
    , optBudgetSecs :: !(Maybe Double)
    , optMaxOracleExecutions :: !(Maybe Int)
    , optReductionOptions :: !ReductionOptions
    }
    deriving (Eq, Show)

data ReductionOptions = ReductionOptions
    { optSlicing :: !Bool
    , optClosure :: !Bool
    , optIntraStatement :: !Bool
    , optUnrestricted :: !Bool
    , optCache :: !Bool
    , optSearchStrategy :: !SearchStrategy
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
    , cache_hit_ratio :: !Double
    , status :: !T.Text
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
                <> help "Path to fuzz SQL input, including schema and setup statements"
            )
        <*> strOption
            ( long "oracle"
                <> metavar "COMMAND"
                <> help "Interestingness predicate command (SQL on stdin; exit 0 accepts)"
            )
        <*> strOption
            ( long "out"
                <> metavar "DIR"
                <> value "occam-out"
                <> showDefault
                <> help "Directory where reduced artifacts will be written"
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
        <*> strOption
            ( long "log-level"
                <> metavar "LEVEL"
                <> value "info"
                <> showDefault
                <> help "Log severity level (debug, info, warn, error)"
            )
        <*> strOption
            ( long "log-format"
                <> metavar "FORMAT"
                <> value "text"
                <> showDefault
                <> help "Log output format (text or json)"
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
        <*> reductionOptionsParser

reductionOptionsParser :: Parser ReductionOptions
reductionOptionsParser =
    ReductionOptions
        <$> toggle True "slicing" "no-slicing" "dependency slicing"
        <*> toggle True "closure" "no-closure" "dependency closure"
        <*> toggle True "intra-statement" "no-intra-statement" "intra-statement reductions"
        <*> toggle True "unrestricted" "no-unrestricted" "unrestricted reductions"
        <*> toggle True "cache" "no-cache" "candidate caching"
        <*> option
            (eitherReader parseSearchStrategy)
            ( long "search-strategy"
                <> metavar "STRATEGY"
                <> value CountDD
                <> showDefault
                <> help "Search strategy: count-dd, weighted-dd, or cdd"
            )
        <*> option
            (eitherReader parseSQLDialect)
            ( long "dialect"
                <> metavar "DIALECT"
                <> value GenericSQL
                <> showDefault
                <> help "SQL dependency dialect: generic, postgresql, mysql, or sqlite"
            )

toggle :: Bool -> String -> String -> String -> Parser Bool
toggle defaultValue enableName disableName description =
    flag' True (long enableName <> help ("Enable " ++ description))
        <|> flag' False (long disableName <> help ("Disable " ++ description))
        <|> pure defaultValue

parseLogLevel :: String -> LogLevel
parseLogLevel s = case map toLower s of
    "debug" -> LevelDebug
    "warn" -> LevelWarn
    "warning" -> LevelWarn
    "error" -> LevelError
    _ -> LevelInfo

parseLogFormat :: String -> LogFormat
parseLogFormat s = case map toLower s of
    "json" -> FormatJSON
    _ -> FormatText

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
            | otherwise = parseLogLevel optLogLevel
        effectiveFormat = parseLogFormat optLogFormat
        logCfg =
            LogConfig
                { logMinLevel = effectiveLevel
                , logFormat = effectiveFormat
                , logUseColor = True
                }
    withLogger logCfg $ \logger -> runOccamWithLogger logger opts

runOccamWithLogger :: Logger -> Options -> IO ()
runOccamWithLogger logger Options {..} = do
    let finitePositive value = value > 0 && not (isNaN value || isInfinite value)
        maxSeconds = fromIntegral (maxBound :: Int) / 1000000
        finiteTimeout value =
            finitePositive value && value <= maxSeconds && value * 1000000 >= 1
        validBudget =
            finiteTimeout optTimeoutSecs
                && maybe True finiteTimeout optBudgetSecs
                && maybe True (> 0) optMaxOracleExecutions
    unless validBudget $ do
        hPutStrLn stderr "Error: timeout, budget, and execution limits must be positive"
        exitFailure

    fileExists <- doesFileExist optInputFile
    unless fileExists $ do
        logError logger "occam::cli" ("Error: Input file not found: " <> T.pack optInputFile)
        hPutStrLn stderr ("Error: Input file not found: " ++ optInputFile)
        exitFailure

    initialSQL <- BS.readFile optInputFile
    initialStmts <- countStatements initialSQL
    let initialBytes = BS.length initialSQL

    when (not optQuiet && parseLogFormat optLogFormat == FormatText) $ do
        putStrLn "======================================================================"
        putStrLn $ "Occam v" ++ Occam.version ++ " starting reduction"
        putStrLn "======================================================================"
        putStrLn $
            "  Target SQL:   "
                ++ optInputFile
                ++ " ("
                ++ show initialBytes
                ++ " bytes, "
                ++ show initialStmts
                ++ " statements)"
        putStrLn $ "  Oracle Cmd:   " ++ optOracleCmd
        putStrLn $ "  Artifact Dir: " ++ optOutDir
        putStrLn "----------------------------------------------------------------------"

    let baseOracleConfig =
            OracleConfig
                { oracleCommand = "sh"
                , oracleArgs = ["-c", optOracleCmd]
                , oracleTimeoutMicros = round (optTimeoutSecs * 1000000)
                , oracleResetCommand = optResetCmd
                }
        reductionConfig = toReductionConfig optReductionOptions
        configuredBudget =
            EvaluationConfig
                { evaluationCacheEnabled = optCache optReductionOptions
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
        , ("oracle_cmd", toJSON optOracleCmd)
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
            hPutStrLn stderr "Error: Initial predicate check exceeded the total budget"
            exitFailure
        Just (Left err) -> do
            hPutStrLn stderr ("Error: " ++ T.unpack err)
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

    createDirectoryIfMissing True optOutDir

    let minimalPath = optOutDir </> "minimal.sql"
    logInfo logger "occam::reducer" "Running hierarchical reduction engine..."

    result <-
        runEvaluatedReduction
            budgetConfig
            oracleCfg
            logger
            initialSQL
            minimalPath
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
    reducedStmts <- countStatements reducedSQL
    let reducedBytes = BS.length reducedSQL
        ratio =
            if initialBytes > 0
                then (1.0 - (fromIntegral reducedBytes / fromIntegral initialBytes :: Double)) * 100.0
                else 0.0
        initialOracleSeconds = realToFrac (diffUTCTime initialEnd initialStart)
        statsPath = optOutDir </> "stats.json"
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
                , cache_hit_ratio = hitRatio
                , status = evaluationStatus evaluation
                , reduction_settings = reductionSettings (optCache optReductionOptions) reductionConfig
                , evaluation_budget = budgetConfig
                , evaluation_metrics = evaluation
                , initial_oracle_seconds = initialOracleSeconds
                , elapsed_seconds = evaluationTotalSeconds evaluation
                }
    BS.writeFile minimalPath reducedSQL
    BSL.writeFile statsPath (encode stats)

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

    when (not optQuiet && parseLogFormat optLogFormat == FormatText) $ do
        putStrLn "----------------------------------------------------------------------"
        putStrLn "Reduction Finished"
        putStrLn "======================================================================"
        putStrLn "Occam Reduction Summary"
        putStrLn "======================================================================"
        putStrLn $ "  Status:       " ++ T.unpack (evaluationStatus evaluation)
        putStrLn $ "  Statements:   " ++ show initialStmts ++ " -> " ++ show reducedStmts
        putStrLn $
            "  Bytes:        "
                ++ show initialBytes
                ++ " B -> "
                ++ show reducedBytes
                ++ " B (-"
                ++ showFFloat (Just 1) ratio "%"
                ++ ")"
        putStrLn $ "  DB Observes:  " ++ show totalOracleExecutions
        putStrLn $ "  Cache Hits:   " ++ show totalHits
        putStrLn $ "  Cache Misses: " ++ show totalMisses
        putStrLn $ "  Hit Ratio:    " ++ showFFloat (Just 1) (hitRatio * 100.0) "%"
        putStrLn $ "  Elapsed Time: " ++ showFFloat (Just 3) (evaluationTotalSeconds evaluation) "s"
        putStrLn $ "  Artifacts:    " ++ minimalPath
        putStrLn $ "                " ++ statsPath
        putStrLn "======================================================================"
    unless
        (evaluationStatus evaluation == "completed" && evaluationValidationPassed evaluation)
        exitFailure

runWithinBudget :: Integer -> Maybe Integer -> IO a -> IO (Maybe a)
runWithinBudget _ Nothing action = Just <$> action
runWithinBudget started (Just totalMicros) action = do
    now <- toInteger <$> getMonotonicTimeNSec
    let remaining = max 0 (totalMicros - (now - started) `div` 1000)
        timeoutMicros = fromInteger (min remaining (toInteger (maxBound :: Int)))
    timeout timeoutMicros action

toReductionConfig :: ReductionOptions -> ReductionConfig
toReductionConfig ReductionOptions {..} =
    defaultReductionConfig
        { reductionSlicing = optSlicing
        , reductionClosure = optClosure
        , reductionIntraStatement = optIntraStatement
        , reductionUnrestricted = optUnrestricted
        , reductionSearch = optSearchStrategy
        , reductionDialect = optDialect
        }

reductionSettings :: Bool -> ReductionConfig -> Value
reductionSettings cacheEnabled ReductionConfig {..} =
    object
        [ "slicing" .= reductionSlicing
        , "closure" .= reductionClosure
        , "intra_statement" .= reductionIntraStatement
        , "unrestricted" .= reductionUnrestricted
        , "cache" .= cacheEnabled
        , "search_strategy" .= searchStrategyName reductionSearch
        , "dialect" .= sqlDialectName reductionDialect
        ]
