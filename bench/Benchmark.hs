{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Benchmark (
    BenchmarkMetric (..),
    runComparisonBenchmark,
    runSingleAlgorithmBenchmark,
) where

import Baseline (runNaiveDdmin, runSequentialDrop)
import Data.Aeson (FromJSON, ToJSON, Value, encode, object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import GHC.Generics (Generic)
import Occam.Evaluation (
    EvaluationConfig (..),
    EvaluationMetrics (..),
    EvaluationResult (..),
    runEvaluatedReduction,
    searchStrategyName,
    sqlDialectName,
 )
import Occam.Logger (Logger)
import Occam.Oracle (OracleConfig)
import Occam.Parser (SQLDialect (..), countStatements)
import Occam.Reducer (
    Oracle,
    ReductionConfig (..),
    SearchStrategy (..),
    reduceSQLWithConfig,
    statementDdmin,
 )
import System.FilePath ((</>))

data BenchmarkMetric = BenchmarkMetric
    { metricAlgorithm :: !Text
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

type BenchmarkProfile = (Text, FilePath, Bool, Value, BenchmarkAction)

runComparisonBenchmark ::
    EvaluationConfig ->
    OracleConfig ->
    Logger ->
    ReductionConfig ->
    ByteString ->
    FilePath ->
    IO [BenchmarkMetric]
runComparisonBenchmark evaluationConfig oracleConfig logger baseConfig sql outDir = do
    let dialect = reductionDialect baseConfig
    initialStmts <- countStatementsOrFail dialect sql
    let cacheEnabled = evaluationCacheEnabled evaluationConfig
        noSlicing = baseConfig {reductionSlicing = False}
        noClosure = baseConfig {reductionClosure = False}
        noSlicingOrClosure = baseConfig {reductionSlicing = False, reductionClosure = False}
        noIntra = baseConfig {reductionIntraStatement = False}
        hardClosure = baseConfig {reductionClosure = True, reductionUnrestricted = False}
        unrestrictedFallback = baseConfig {reductionClosure = True, reductionUnrestricted = True}
        plainStatementDdmin =
            baseConfig
                { reductionSlicing = False
                , reductionClosure = False
                , reductionIntraStatement = False
                , reductionUnrestricted = False
                }
        intraStatementTrack =
            baseConfig
                { reductionSlicing = False
                , reductionClosure = False
                , reductionIntraStatement = True
                , reductionUnrestricted = False
                }
        pipeline name artifact config useCache =
            (name, artifact, useCache, reductionSettings useCache config, reduceSQLWithConfig config)
        statementTrack name artifact strategy =
            ( name
            , artifact
            , cacheEnabled
            , statementSettings dialect cacheEnabled strategy
            , \_ oracle input -> statementDdmin dialect strategy oracle input
            )
        profiles =
            [ pipeline "Occam" "occam" baseConfig cacheEnabled
            ,
                ( "B1 (Sequential)"
                , "b1-sequential"
                , cacheEnabled
                , statementOnlySettings dialect cacheEnabled "sequential-deletion"
                , \_ oracle input -> runSequentialDrop dialect oracle input
                )
            ,
                ( "B2 (Statement ddmin)"
                , "b2-statement-ddmin"
                , cacheEnabled
                , statementSettings dialect cacheEnabled CountDD
                , \_ oracle input -> runNaiveDdmin dialect oracle input
                )
            , statementTrack "WeightedDD (statement ddmin)" "weighted-dd" WeightedDD
            , statementTrack "CDD (statement ddmin)" "cdd" CDD
            , pipeline "Shared (no slicing)" "ablation-no-slicing" noSlicing cacheEnabled
            , pipeline "Shared (no closure)" "ablation-no-closure" noClosure cacheEnabled
            , pipeline
                "Shared (no slicing or closure)"
                "ablation-no-slicing-no-closure"
                noSlicingOrClosure
                cacheEnabled
            , pipeline "Shared (no intra-statement)" "ablation-no-intra-statement" noIntra cacheEnabled
            , pipeline "Shared (hard closure)" "closure-hard" hardClosure cacheEnabled
            , pipeline "Shared (unrestricted fallback)" "closure-unrestricted" unrestrictedFallback cacheEnabled
            , pipeline "Shared (plain statement ddmin)" "shared-statement-ddmin" plainStatementDdmin cacheEnabled
            , pipeline "Shared (intra/expression)" "shared-intra-expression" intraStatementTrack cacheEnabled
            , pipeline
                (if cacheEnabled then "Occam (cache disabled)" else "Occam (cache enabled)")
                "ablation-cache"
                baseConfig
                (not cacheEnabled)
            ]
    mapM (runOne dialect evaluationConfig oracleConfig logger sql initialStmts outDir) profiles

runSingleAlgorithmBenchmark ::
    SQLDialect ->
    Int ->
    EvaluationConfig ->
    OracleConfig ->
    Logger ->
    ByteString ->
    FilePath ->
    IO BenchmarkMetric
runSingleAlgorithmBenchmark dialect baselineIdx evaluationConfig oracleConfig logger sql outDir = do
    initialStmts <- countStatementsOrFail dialect sql
    let cacheEnabled = evaluationCacheEnabled evaluationConfig
    profile <- case baselineIdx of
        1 ->
            pure
                ( "B1 (Sequential)"
                , "b1-sequential"
                , cacheEnabled
                , statementOnlySettings dialect cacheEnabled "sequential-deletion"
                , \_ oracle input -> runSequentialDrop dialect oracle input
                )
        2 ->
            pure
                ( "B2 (Statement ddmin)"
                , "b2-statement-ddmin"
                , cacheEnabled
                , statementSettings dialect cacheEnabled CountDD
                , \_ oracle input -> runNaiveDdmin dialect oracle input
                )
        _ -> ioError (userError "baseline index must be 1 or 2")
    runOne dialect evaluationConfig oracleConfig logger sql initialStmts outDir profile

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
runOne dialect evaluationConfig oracleConfig logger sql initialStmts outDir (name, artifactName, cacheEnabled, settings, action) = do
    let sqlPath = outDir </> (artifactName ++ ".sql")
        metricsPath = outDir </> (artifactName ++ ".json")
        runConfig = evaluationConfig {evaluationCacheEnabled = cacheEnabled}
        initialBytes = BS.length sql
    EvaluationResult {..} <-
        runEvaluatedReduction
            runConfig
            dialect
            oracleConfig
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
        , "search_strategy" .= searchStrategyName reductionSearch
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

statementSettings :: SQLDialect -> Bool -> SearchStrategy -> Value
statementSettings dialect cacheEnabled strategy =
    object
        [ "kind" .= ("statement-ddmin" :: Text)
        , "unit" .= ("top-level-statement" :: Text)
        , "search_strategy" .= searchStrategyName strategy
        , "cache" .= cacheEnabled
        , "dialect" .= sqlDialectName dialect
        ]
