-- | Shared parsing, timing, certifier tracking, and SQL analysis helpers for the reducer.
module Occam.Reducer.Support (
    seconds,
    lastMay,
    logStage,
    timedReductionStage,
    kindOfOptions,
    parseUnitsOrThrow,
    StatementParser,
    parseStatements,
    parseStatementsFast,
    earlyExit,
    hoistMaybe,
    reuseLatestSQL,
    reuseLatestSQLWithSeed,
    syntaxParseOptions,
    columnSingleParseOptions,
    columnAtomicParseOptions,
    columnParseOptions,
    recordEarlyRejectSkip,
    rejectIfEarly,
    buildCertifierTracked,
    logEarlyRejectStats,
    timeStatementBuild,
    timeTaggedStatementBuild,
    SQLAnalysis (..),
    analyzeSQL,
) where

import Control.Applicative (empty)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson (toJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Certificate (
    EarlyRejectCertifier (..),
    EarlyRejectStats (..),
    EarlyRejectTracker,
    buildEarlyRejectCertifier,
    emptyEarlyRejectCertifier,
    readEarlyRejectStats,
    recordEarlyRejectBuildNanos,
 )
import Occam.Core (
    EarlyRejectReason (..),
    ResultTrace,
    SessionInit,
    Statement,
 )
import Occam.Dependency (DependencyGraph (..), analyzeStatements)
import Occam.Logger (Logger, logDebug', logInfo')
import Occam.Metrics (
    FamilyTag (..),
    MetricsStore,
    ParseKind (..),
    recordCandidateBuildTiming,
    recordGeneratedSkipped,
 )
import Occam.Parser (
    ColumnTarget,
    ParseOptions (..),
    ParsedSQL (..),
    SQLDialect,
    StatementUnit (..),
    defaultParseOptions,
    parseSQLTimed,
    spanOnlyOptions,
 )
import Occam.Reducer.Config (
    ColumnMode (..),
    ReductionStage,
    StageEvent (..),
    StageReporter,
    stageLogName,
 )

seconds :: Word64 -> Double
seconds ns = fromIntegral ns / 1000000000

-- | Total 'last': the final element, or 'Nothing' for an empty list.
lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay [only] = Just only
lastMay (_ : rest) = lastMay rest

logStage :: Logger -> T.Text -> Word64 -> IO ()
logStage logger stage elapsed =
    logDebug'
        logger
        "reducer"
        ("Finished " <> stage)
        [("stage", toJSON stage), ("duration_seconds", toJSON (seconds elapsed))]

-- | Timed stage with typed boundary events. Keeps the legacy log line.
timedReductionStage ::
    StageReporter -> Logger -> ReductionStage -> ByteString -> IO (ByteString, a) -> IO (ByteString, a)
timedReductionStage reporter logger stage input action = do
    reporter (StageStarted stage)
    started <- getMonotonicTimeNSec
    (output, extra) <- action
    finished <- getMonotonicTimeNSec
    let elapsed = finished - started
    logStage logger (stageLogName stage) elapsed
    reporter (StageFinished stage (seconds elapsed) (BS.length input) (BS.length output))
    pure (output, extra)

kindOfOptions :: ParseOptions -> ParseKind
kindOfOptions options
    | parseColumnIR options || parseColumnBundles options = ParseColumnIR
    | parseChunkEdits options = ParseSyntaxChunks
    | otherwise = ParseStatementOnly

parseUnitsOrThrow ::
    Maybe MetricsStore ->
    ParseOptions ->
    ByteString ->
    IO (SessionInit, [StatementUnit], [ColumnTarget])
parseUnitsOrThrow store options sql = do
    parsed <- parseSQLTimed store (kindOfOptions options) options sql
    case parsed of
        Left parserError -> ioError (userError ("SQL parser transport failure: " ++ show parserError))
        Right result -> pure (parsedInitialSession result, parsedUnits result, parsedColumns result)

type StatementParser = ByteString -> IO (SessionInit, [StatementUnit])

parseStatements :: Maybe MetricsStore -> SQLDialect -> StatementParser
parseStatements store dialect sql = do
    (session, units, _) <- parseUnitsOrThrow store (defaultParseOptions dialect) sql
    pure (session, units)

parseStatementsFast :: Maybe MetricsStore -> SQLDialect -> StatementParser
parseStatementsFast store dialect sql = do
    (session, units, _) <- parseUnitsOrThrow store (spanOnlyOptions dialect) sql
    pure (session, units)

-- | Monadic early exit in 'ContT', discarding the continuation and returning immediately.
earlyExit :: (Monad m) => r -> ContT r m a
earlyExit r = ContT (\_ -> pure r)

-- | Lift a pure 'Maybe' into 'MaybeT'.
hoistMaybe :: (Applicative m) => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

-- Share the latest analysis within a reduction, keyed by exact SQL bytes.
reuseLatestSQL :: (ByteString -> IO a) -> IO (ByteString -> IO a)
reuseLatestSQL = reuseLatestSQLWithSeed Nothing

reuseLatestSQLWithSeed :: Maybe (ByteString, a) -> (ByteString -> IO a) -> IO (ByteString -> IO a)
reuseLatestSQLWithSeed initialSeed analyze = do
    latest <- newIORef initialSeed
    pure $ \sql -> do
        cached <- readIORef latest
        case cached of
            Just (previous, result) | previous == sql -> pure result
            _ -> do
                result <- analyze sql
                writeIORef latest (Just (sql, result))
                pure result

-- | Parse options for syntax-level rewrites (skips unused semantic dependency extraction).
syntaxParseOptions :: SQLDialect -> ParseOptions
syntaxParseOptions dialect =
    (defaultParseOptions dialect)
        { parseChunkEdits = True
        , parseSyntaxEdits = True
        , parseSyntaxOnly = True
        }

-- | Single-point column sites only: no bundle planning or payload.
columnSingleParseOptions :: SQLDialect -> ParseOptions
columnSingleParseOptions dialect = (syntaxParseOptions dialect) {parseColumnIR = True, parseSyntaxOnly = False}

-- | Full atomic bundles; implies single-point geometry.
columnAtomicParseOptions :: SQLDialect -> ParseOptions
columnAtomicParseOptions dialect =
    (syntaxParseOptions dialect)
        { parseColumnIR = True
        , parseColumnBundles = True
        , parseSyntaxOnly = False
        }

columnParseOptions :: ColumnMode -> SQLDialect -> ParseOptions
columnParseOptions ColumnOff = syntaxParseOptions
columnParseOptions ColumnSingle = columnSingleParseOptions
columnParseOptions ColumnAtomic = columnAtomicParseOptions

recordEarlyRejectSkip :: Maybe MetricsStore -> EarlyRejectReason -> IO ()
recordEarlyRejectSkip store EarlyRejectExactPrefix =
    recordGeneratedSkipped store "early-reject-exact-prefix"
recordEarlyRejectSkip store EarlyRejectNoResultStatement =
    recordGeneratedSkipped store "early-reject-no-result"
recordEarlyRejectSkip store EarlyRejectFailingPrefix =
    recordGeneratedSkipped store "early-reject-failing-prefix"
recordEarlyRejectSkip store EarlyRejectMissingDefinition =
    recordGeneratedSkipped store "early-reject-missing-definition"
recordEarlyRejectSkip store EarlyRejectColumnArityMismatch =
    recordGeneratedSkipped store "early-reject-column-arity"

rejectIfEarly :: Maybe MetricsStore -> Maybe EarlyRejectReason -> MaybeT IO ()
rejectIfEarly _ Nothing = pure ()
rejectIfEarly store (Just reason) = liftIO (recordEarlyRejectSkip store reason) >> empty

buildCertifierTracked ::
    Bool ->
    EarlyRejectTracker ->
    ByteString ->
    [StatementUnit] ->
    Maybe ResultTrace ->
    IO EarlyRejectCertifier
buildCertifierTracked False _ _ _ _ = pure emptyEarlyRejectCertifier
buildCertifierTracked True _ _ _ Nothing = pure emptyEarlyRejectCertifier
buildCertifierTracked True tracker baseSQL units mTrace = do
    t0 <- getMonotonicTimeNSec
    let !cert = buildEarlyRejectCertifier baseSQL units mTrace
        !_ =
            IntSet.size (certExactPrefixByteLens cert)
                + IntSet.size (certGuaranteedNonResult cert)
                + IntMap.size (certMissingDefReqs cert)
    t1 <- getMonotonicTimeNSec
    recordEarlyRejectBuildNanos tracker (t1 - t0)
    pure cert

logEarlyRejectStats :: Logger -> T.Text -> EarlyRejectTracker -> IO ()
logEarlyRejectStats logger stage tracker = do
    stats <- readEarlyRejectStats tracker
    when (earlyRejectChecked stats > 0 || earlyRejectCheckNanos stats > 0) $
        logInfo'
            logger
            "reducer"
            "Early rejection certificate summary"
            [ ("stage", toJSON stage)
            , ("checked", toJSON (earlyRejectChecked stats))
            , ("exact_prefix_rejects", toJSON (earlyRejectExactPrefix stats))
            , ("no_result_rejects", toJSON (earlyRejectNoResult stats))
            , ("failing_prefix_rejects", toJSON (earlyRejectFailingPrefix stats))
            , ("missing_definition_rejects", toJSON (earlyRejectMissingDef stats))
            , ("column_arity_rejects", toJSON (earlyRejectColumnArity stats))
            , ("check_nanos", toJSON (earlyRejectCheckNanos stats))
            , ("check_micros", toJSON (fromIntegral (earlyRejectCheckNanos stats) / (1000.0 :: Double)))
            ]

-- | Record statement assembly time if metrics are enabled.
timeStatementBuild :: Maybe MetricsStore -> ByteString -> IO ByteString
timeStatementBuild store = timeTaggedStatementBuild store TagStatement

timeTaggedStatementBuild :: Maybe MetricsStore -> FamilyTag -> ByteString -> IO ByteString
timeTaggedStatementBuild Nothing _ bytes = pure bytes
timeTaggedStatementBuild (Just store) tag bytes = do
    started <- getMonotonicTimeNSec
    BS.length bytes `seq` pure ()
    finished <- getMonotonicTimeNSec
    recordCandidateBuildTiming
        (Just store)
        tag
        (fromIntegral ((finished - started) `div` 1000))
    pure bytes

-- | Parsed statements plus their dependency graph for one exact candidate byte string.
data SQLAnalysis = SQLAnalysis
    { analyzedUnits :: ![StatementUnit]
    , analyzedStatements :: ![Statement]
    , analyzedGraph :: !DependencyGraph
    , analyzedSession :: !SessionInit
    }

analyzeSQL :: StatementParser -> Logger -> SQLDialect -> ByteString -> IO SQLAnalysis
analyzeSQL parse logger dialect sql = do
    started <- getMonotonicTimeNSec
    (initialSession, units) <- parse sql
    let statements = map unitStatement units
        graph = analyzeStatements initialSession units
    graph `seq` pure ()
    finished <- getMonotonicTimeNSec
    let elapsed = finished - started
        statementCount = length statements
        edgeCount = graphEdgeCount graph
        barrierActions = graphBarrierActions graph
        barrierStatements = graphBarrierStatements graph
        barrierEdges = graphBarrierEdges graph
    logDebug'
        logger
        "reducer"
        "Dependency analysis completed"
        [ ("analysis_seconds", toJSON (seconds elapsed))
        , ("statements", toJSON statementCount)
        , ("dependency_edges", toJSON edgeCount)
        , ("barrier_actions", toJSON barrierActions)
        , ("barrier_statements", toJSON barrierStatements)
        , ("barrier_edges", toJSON barrierEdges)
        , ("dialect", toJSON (T.pack (show dialect)))
        ]
    pure
        SQLAnalysis
            { analyzedUnits = units
            , analyzedStatements = statements
            , analyzedGraph = graph
            , analyzedSession = initialSession
            }
