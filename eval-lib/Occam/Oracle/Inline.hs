module Occam.Oracle.Inline (
    newResultStatementTracker,
    newResultTraceTracker,
    prepareResultOracle,
    prepareResultOracleWithTrace,
    prepareResultOracleWithTraceAndFailures,
) where

import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Certificate (FailingPrefixCache, recordFailingPrefix)
import Occam.Core (ResultStatement, ResultTrace, traceResultStatement)
import Occam.Evaluation (EvaluationObserver)
import Occam.Oracle (OracleObservation (OracleFinished), OracleObservationDetails (..))
import Occam.Oracle.DuckDB (
    generateDuckDBContractWithTrace,
    prepareDuckDBContractObserverWithReplay,
 )
import Occam.Oracle.Sqlite (
    generateSqliteContractWithTrace,
    prepareSqliteContractObserver,
 )
import Occam.Parser (SQLDialect (..))
import System.Exit (ExitCode (ExitSuccess))

newResultStatementTracker ::
    Bool ->
    IO (BS.ByteString -> ResultStatement -> IO (), BS.ByteString -> IO (Maybe ResultStatement))
newResultStatementTracker False = pure (\_ _ -> pure (), const (pure Nothing))
newResultStatementTracker True = do
    resultStatements <- newIORef Map.empty
    let rememberResult sql location =
            atomicModifyIORef' resultStatements (\locations -> (Map.insert sql location locations, ()))
        -- A later iteration can reuse a cached observation for a shorter candidate.
        takeResult sql =
            atomicModifyIORef' resultStatements $ \locations ->
                ( Map.filterWithKey (\candidate _ -> BS.length candidate < BS.length sql) locations
                , Map.lookup sql locations
                )
    pure (rememberResult, takeResult)

newResultTraceTracker ::
    Bool ->
    IO (BS.ByteString -> ResultTrace -> IO (), BS.ByteString -> IO (Maybe ResultTrace))
newResultTraceTracker False = pure (\_ _ -> pure (), const (pure Nothing))
newResultTraceTracker True = do
    tracesRef <- newIORef Map.empty
    let rememberTrace sql trace =
            atomicModifyIORef' tracesRef (\traces -> (Map.insert (BS.length sql, sql) trace traces, ()))
        -- Keep the current SQL trace across stages within the same iteration while
        -- pruning strictly larger superseded candidates. Length comes first
        -- in the key so a single tree boundary separates the two groups.
        lookupTrace sql =
            let size = BS.length sql
             in atomicModifyIORef' tracesRef $ \traces ->
                    ( Map.takeWhileAntitone ((<= size) . fst) traces
                    , Map.lookup (size, sql) traces
                    )
    pure (rememberTrace, lookupTrace)

prepareResultOracle ::
    (BS.ByteString -> ResultStatement -> IO ()) ->
    SQLDialect ->
    Int ->
    BS.ByteString ->
    IO (Either Text (OracleObservationDetails, EvaluationObserver))
prepareResultOracle rememberResult =
    prepareResultOracleWithTrace (\sql trace -> rememberResult sql (traceResultStatement trace))

prepareResultOracleWithTrace ::
    (BS.ByteString -> ResultTrace -> IO ()) ->
    SQLDialect ->
    Int ->
    BS.ByteString ->
    IO (Either Text (OracleObservationDetails, EvaluationObserver))
prepareResultOracleWithTrace = prepareResultOracleWithTraceAndFailures Nothing

prepareResultOracleWithTraceAndFailures ::
    Maybe FailingPrefixCache ->
    (BS.ByteString -> ResultTrace -> IO ()) ->
    SQLDialect ->
    Int ->
    BS.ByteString ->
    IO (Either Text (OracleObservationDetails, EvaluationObserver))
prepareResultOracleWithTraceAndFailures mFailCache rememberTrace dialect micros sql = do
    start <- getMonotonicTimeNSec
    let onFail = maybe (const (pure ())) recordFailingPrefix mFailCache
    prepared <- case dialect of
        SQLite ->
            fmap (first (\c -> prepareSqliteContractObserver micros c onFail))
                <$> generateSqliteContractWithTrace micros sql
        DuckDB -> do
            result <- generateDuckDBContractWithTrace micros sql
            traverse
                ( \(contract, trace) -> do
                    observe <- prepareDuckDBContractObserverWithReplay micros contract sql onFail
                    pure (observe, trace)
                )
                result
        _ -> pure (Left "Result oracle requires SQLite or DuckDB")
    end <- getMonotonicTimeNSec
    case prepared of
        Left err -> pure (Left err)
        Right (observe, trace) -> do
            rememberTrace sql trace
            let details =
                    OracleObservationDetails
                        (OracleFinished ExitSuccess)
                        (elapsed start end)
                        0
                        (Just (elapsed start end))
            pure (Right (details, timeObservation observe))
    where
        elapsed start end = fromIntegral ((end - start) `div` 1000)
        timeObservation observe started candidate = do
            start <- getMonotonicTimeNSec
            _ <- started
            (observation, trace) <- observe candidate
            mapM_ (rememberTrace candidate) trace
            end <- getMonotonicTimeNSec
            pure (OracleObservationDetails observation (elapsed start end) 0 (Just (elapsed start end)))
