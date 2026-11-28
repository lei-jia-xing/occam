-- | Atomic output writers, path collision validation, statement counting, and CLI stats summaries.
module Output (
    computeSessionStats,
    countStatementsForCLI,
    dumpStatementFacts,
    exitCliError,
    exitTimeout,
    readInputBytes,
    resolveOutputTargets,
    writeReductionOutputs,
) where

import Control.Monad (forM_, unless, when)
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, diffUTCTime)
import Occam.AtomicFile (writeAtomicBytes, writeAtomicBytesWithPermissionsOf, writeAtomicLazyBytes)
import Occam.Evaluation (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    Stats (..),
    sqlDialectName,
 )
import Occam.Logger (Logger, logError)
import Occam.Parser (
    ParseOptions (..),
    SQLDialect,
    ServerConfig (..),
    countStatements,
    defaultParseOptions,
    factDumpJson,
    parseSQL,
    parserErrorMessage,
 )
import Occam.Reducer (
    ReductionConfig (..),
    columnModeName,
    statementSearchPolicyName,
 )
import Options (ReductionStrategy, strategyName)
import System.Directory (canonicalizePath, doesFileExist)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

-- | Terminate immediately with exit status @2@ for CLI usage\/validation failures.
exitCliError :: IO a
exitCliError = exitWith (ExitFailure 2)

-- | Terminate immediately with exit status @124@ when reduction hits timeout\/budget limits.
exitTimeout :: IO a
exitTimeout = exitWith (ExitFailure 124)

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

rejectPathCollision :: Maybe FilePath -> Maybe FilePath -> String -> IO ()
rejectPathCollision left right message =
    when (isJust left && left == right) (failCliPath message)

failCliPath :: String -> IO a
failCliPath message = do
    hPutStrLn stderr ("error: " ++ message)
    exitCliError

-- | Ensure stats and output file paths do not collide with inputs or backups.
validateOutputPaths :: FilePath -> FilePath -> Bool -> Bool -> Maybe FilePath -> IO ()
validateOutputPaths input target writesStdout inPlace statsFile = do
    inputPath <- traverse canonicalizePath (if input == "-" then Nothing else Just input)
    targetPath <- traverse canonicalizePath (if writesStdout then Nothing else Just target)
    unless inPlace $
        rejectPathCollision
            inputPath
            targetPath
            "--out path must differ from the input path (use --in-place)"
    forM_ statsFile (validateStatsPath input writesStdout inPlace inputPath targetPath)

validateStatsPath ::
    FilePath -> Bool -> Bool -> Maybe FilePath -> Maybe FilePath -> FilePath -> IO ()
validateStatsPath _ True _ _ _ "-" =
    failCliPath "--stats - cannot be combined with writing reduced SQL to stdout"
validateStatsPath _ False _ _ _ "-" = pure ()
validateStatsPath input _ inPlace inputPath targetPath stats = do
    statsPath <- Just <$> canonicalizePath stats
    rejectPathCollision statsPath inputPath "--stats path must differ from the input path"
    rejectPathCollision statsPath targetPath "--stats path must differ from the output path"
    when inPlace $ do
        backupPath <- Just <$> canonicalizePath (input ++ ".bak")
        rejectPathCollision statsPath backupPath "--stats path must differ from the in-place backup path"

-- | Ensure output and @--stats@ paths do not collide with the input file, backup, or each other.
resolveOutputTargets ::
    FilePath ->
    Maybe FilePath ->
    Bool ->
    Maybe FilePath ->
    IO (Bool, FilePath, FilePath)
resolveOutputTargets inputFile outputFile inPlace statsFile = do
    let targetMinimalPath = fromMaybe (if inPlace then inputFile else "-") outputFile
        writeToStdout = targetMinimalPath == "-"
        statsPath = fromMaybe "" statsFile
    validateOutputPaths inputFile targetMinimalPath writeToStdout inPlace statsFile
    pure (writeToStdout, targetMinimalPath, statsPath)

-- | Atomically write reduced SQL, optional @.bak@ backup, and optional @stats.json@ output.
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
    let canWriteOutput = not (inPlace && not (evaluationValidationPassed evaluation))
    unless canWriteOutput do
        let msg = "refusing in-place modification because final candidate failed target validation"
        logError logger "cli" (T.pack msg)
        hPutStrLn stderr ("error: " ++ msg)

    when (not writeToStdout && canWriteOutput) do
        if inPlace
            then do
                writeAtomicBytesWithPermissionsOf inputFile (inputFile ++ ".bak") initialSQL
                writeAtomicBytesWithPermissionsOf inputFile targetMinimalPath reducedSQL
            else writeAtomicFile targetMinimalPath reducedSQL

    unless (null statsPath) do
        writeAtomicLazy statsPath (encode stats)

    when (writeToStdout && canWriteOutput) do
        BS.putStr reducedSQL
        hFlush stdout

-- | Read the initial SQL input from stdin (@-@) or a file, exiting with a CLI error if missing.
readInputBytes :: Maybe Logger -> FilePath -> IO BS.ByteString
readInputBytes _ "-" = BS.getContents
readInputBytes mLogger inputFile = do
    fileExists <- doesFileExist inputFile
    unless fileExists do
        let msg = "cannot open '" ++ inputFile ++ "': No such file or directory"
        forM_ mLogger (\logger -> logError logger "cli" (T.pack msg))
        hPutStrLn stderr ("error: " ++ msg)
        exitFailure
    BS.readFile inputFile

-- | Count statements in a SQL buffer for CLI reporting, exiting cleanly on parse failure.
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

-- | Assemble the serialized 'Stats' report for a completed reduction session.
computeSessionStats ::
    Int ->
    Int ->
    BS.ByteString ->
    Int ->
    UTCTime ->
    UTCTime ->
    Bool ->
    ReductionConfig ->
    ReductionStrategy ->
    EvaluationConfig ->
    EvaluationMetrics ->
    Stats
computeSessionStats initialBytes initialStmts reducedSQL reducedStmts initialStart initialEnd cacheEnabled reductionConfig strategy budgetConfig evaluation =
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
        , reduction_settings = reductionSettings cacheEnabled reductionConfig strategy
        , evaluation_budget = budgetConfig
        , evaluation_metrics = evaluation
        , initial_oracle_seconds = initialOracleSeconds
        , elapsed_seconds = evaluationTotalSeconds evaluation
        }
    where
        totalHits = evaluationCacheHits evaluation
        totalMisses = evaluationCacheMisses evaluation
        totalLookups = totalHits + totalMisses
        hitRatio =
            if totalLookups > 0
                then fromIntegral totalHits / fromIntegral totalLookups
                else 0.0
        reducedBytes = BS.length reducedSQL
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

reductionSettings :: Bool -> ReductionConfig -> ReductionStrategy -> Value
reductionSettings cacheEnabled ReductionConfig {..} strategy =
    object
        [ "slicing" .= reductionSlicing
        , "closure" .= reductionClosure
        , "statement_order" .= statementSearchPolicyName reductionStatementSearch
        , "intra_statement" .= reductionIntraStatement
        , "literal_batches" .= reductionLiteralBatches
        , "ast_batching" .= reductionAstBatching
        , "adaptive_values" .= reductionAdaptiveValues
        , "column_mode" .= columnModeName reductionColumnMode
        , "unrestricted" .= reductionUnrestricted
        , "cache" .= cacheEnabled
        , "dialect" .= sqlDialectName reductionDialect
        , "strategy" .= strategyName strategy
        ]

-- | Parse the input SQL and print per-statement frontend facts and edits to stdout (@--dump-facts@).
dumpStatementFacts :: SQLDialect -> Bool -> FilePath -> IO ()
dumpStatementFacts dialect fkOn inputFile = do
    sql <- readInputBytes Nothing inputFile
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
