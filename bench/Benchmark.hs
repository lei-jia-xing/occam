module Benchmark (
    BenchmarkMetric (..),
    Profile (..),
    parseProfile,
    profileName,
    profileCacheEnabled,
    runProfileBenchmark,
) where

import Data.Aeson (FromJSON, ToJSON, Value, encode, object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Occam.Evaluation (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    EvaluationResult (..),
    runEvaluatedReduction,
    sqlDialectName,
 )
import Occam.Logger (Logger)
import Occam.Oracle (OracleConfig)
import Occam.Parser (SQLDialect (..), countStatements)
import Occam.Reducer (
    Oracle,
    ReductionConfig (..),
    defaultReductionConfig,
    reduceSQLWithConfig,
    statementAudit,
    statementDdmin,
 )
import System.FilePath ((</>))

data BenchmarkMetric = BenchmarkMetric
    { metricAlgorithm :: !Text
    , metricExperimentLine :: !Text
    , metricInitialBytes :: !Int
    , metricReducedBytes :: !Int
    , metricByteReduction :: !Double
    , metricInitialStmts :: !Int
    , metricReducedStmts :: !Int
    , metricStmtReduction :: !Double
    , metricOracleCalls :: !Int
    , metricTimeSeconds :: !Double
    , metricEvaluationMetrics :: !EvaluationMetrics
    , metricReductionSettings :: !Value
    , metricSQLArtifact :: !FilePath
    , metricMetricsArtifact :: !FilePath
    }
    deriving (Eq, Show, Generic)

instance ToJSON BenchmarkMetric
instance FromJSON BenchmarkMetric

type BenchmarkAction = Logger -> Oracle -> ByteString -> IO ByteString

type BenchmarkProfile = (Text, Bool, Value, BenchmarkAction)

data Profile
    = Full
    | Sequential
    | StatementDdmin
    | NoSlicing
    | NoClosure
    | GraphOff
    | NoIntra
    | HardClosure
    | StatementFixedPoint
    | StatementWithSyntax
    | NoCache
    deriving (Eq, Show, Enum, Bounded)

profileName :: Profile -> Text
profileName = \case
    Full -> "full"
    Sequential -> "sequential"
    StatementDdmin -> "statement-ddmin"
    NoSlicing -> "no-slicing"
    NoClosure -> "no-closure"
    GraphOff -> "graph-off"
    NoIntra -> "no-intra"
    HardClosure -> "hard-closure"
    StatementFixedPoint -> "statement-ddmin-fixed-point"
    StatementWithSyntax -> "statement-ddmin-with-syntax"
    NoCache -> "no-cache"

parseProfile :: String -> Either String Profile
parseProfile name =
    case lookup (T.pack name) [(profileName profile, profile) | profile <- [minBound .. maxBound]] of
        Just profile -> Right profile
        Nothing ->
            Left
                ( "Unknown internal profile; choose one of: "
                    ++ T.unpack (T.intercalate ", " (map profileName [minBound .. maxBound]))
                )

profileCacheEnabled :: Profile -> Bool
profileCacheEnabled profile = profile /= NoCache

runProfileBenchmark ::
    Profile ->
    SQLDialect ->
    EvaluationConfig ->
    OracleConfig ->
    Logger ->
    ByteString ->
    FilePath ->
    IO BenchmarkMetric
runProfileBenchmark profile dialect evaluationConfig oracleConfig logger sql outDir = do
    initialStmts <- countStatementsOrFail dialect sql
    let cacheEnabled = profileCacheEnabled profile
        baseConfig = defaultReductionConfig {reductionDialect = dialect}
        config = case profile of
            NoSlicing -> baseConfig {reductionSlicing = False}
            NoClosure -> baseConfig {reductionClosure = False}
            GraphOff -> baseConfig {reductionSlicing = False, reductionClosure = False}
            NoIntra -> baseConfig {reductionIntraStatement = False}
            HardClosure -> baseConfig {reductionUnrestricted = False}
            StatementFixedPoint ->
                baseConfig
                    { reductionSlicing = False
                    , reductionClosure = False
                    , reductionIntraStatement = False
                    , reductionUnrestricted = False
                    }
            StatementWithSyntax ->
                baseConfig
                    { reductionSlicing = False
                    , reductionClosure = False
                    , reductionUnrestricted = False
                    }
            _ -> baseConfig
        (settings, action) = case profile of
            Sequential ->
                ( statementOnlySettings dialect cacheEnabled "sequential-deletion"
                , \_ oracle input -> statementAudit dialect oracle input
                )
            StatementDdmin ->
                ( statementOnlySettings dialect cacheEnabled "statement-ddmin"
                , \_ oracle input -> statementDdmin dialect oracle input
                )
            _ -> (reductionSettings cacheEnabled config, reduceSQLWithConfig config)
    runOne
        dialect
        evaluationConfig
        oracleConfig
        logger
        sql
        initialStmts
        outDir
        (profileName profile, cacheEnabled, settings, action)

runOne ::
    SQLDialect ->
    EvaluationConfig ->
    OracleConfig ->
    Logger ->
    ByteString ->
    Int ->
    FilePath ->
    BenchmarkProfile ->
    IO BenchmarkMetric
runOne dialect evaluationConfig oracleConfig logger sql initialStmts outDir (name, cacheEnabled, settings, action) = do
    let sqlPath = outDir </> "minimal.sql"
        metricsPath = outDir </> "profile.json"
        runConfig = evaluationConfig {evaluationCacheEnabled = cacheEnabled}
        initialBytes = BS.length sql
    EvaluationResult {..} <-
        runEvaluatedReduction
            runConfig
            dialect
            oracleConfig
            Nothing
            logger
            sql
            sqlPath
            (\reducerLogger oracle -> action reducerLogger oracle sql)
    reducedStmts <- countStatementsOrFail dialect evaluatedSQL
    let metrics = evaluationMetrics
        reducedBytes = BS.length evaluatedSQL
        metric =
            BenchmarkMetric
                { metricAlgorithm = name
                , metricExperimentLine = "internal"
                , metricInitialBytes = initialBytes
                , metricReducedBytes = reducedBytes
                , metricByteReduction = reductionPercent initialBytes reducedBytes
                , metricInitialStmts = initialStmts
                , metricReducedStmts = reducedStmts
                , metricStmtReduction = reductionPercent initialStmts reducedStmts
                , metricOracleCalls = evaluationOracleAttempts metrics
                , metricTimeSeconds = evaluationReductionSeconds metrics
                , metricEvaluationMetrics = metrics
                , metricReductionSettings = settings
                , metricSQLArtifact = sqlPath
                , metricMetricsArtifact = metricsPath
                }
    BSL.writeFile metricsPath (encode metric)
    pure metric

reductionPercent :: Int -> Int -> Double
reductionPercent initial reduced
    | initial > 0 = (1.0 - fromIntegral reduced / fromIntegral initial) * 100.0
    | otherwise = 0.0

countStatementsOrFail :: SQLDialect -> ByteString -> IO Int
countStatementsOrFail dialect sql = do
    result <- countStatements dialect sql
    case result of
        Left err -> ioError (userError ("SQL parser failed while counting statements: " ++ show err))
        Right count -> pure count

reductionSettings :: Bool -> ReductionConfig -> Value
reductionSettings cacheEnabled ReductionConfig {..} =
    object
        [ "kind" .= ("shared-reducer" :: Text)
        , "slicing" .= reductionSlicing
        , "closure" .= reductionClosure
        , "intra_statement" .= reductionIntraStatement
        , "unrestricted" .= reductionUnrestricted
        , "cache" .= cacheEnabled
        , "dialect" .= sqlDialectName reductionDialect
        ]

statementOnlySettings :: SQLDialect -> Bool -> Text -> Value
statementOnlySettings dialect cacheEnabled strategy =
    object
        [ "kind" .= strategy
        , "unit" .= ("top-level-statement" :: Text)
        , "cache" .= cacheEnabled
        , "dialect" .= sqlDialectName dialect
        ]
