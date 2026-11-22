module Occam.Oracle.Inline (newResultStatementTracker, prepareResultOracle) where

import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Core (ResultStatement)
import Occam.Evaluation (EvaluationObserver)
import Occam.Oracle (OracleObservation (OracleFinished), OracleObservationDetails (..))
import Occam.Oracle.DuckDB (
    duckdbContractObservationWithResultStatement,
    generateDuckDBContractWithResultStatement,
 )
import Occam.Oracle.Sqlite (
    generateSqliteContractWithResultStatement,
    sqliteContractObservationWithResultStatement,
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

prepareResultOracle ::
    (BS.ByteString -> ResultStatement -> IO ()) ->
    SQLDialect ->
    Int ->
    BS.ByteString ->
    IO (Either Text (OracleObservationDetails, EvaluationObserver))
prepareResultOracle rememberResult dialect micros sql = do
    start <- getMonotonicTimeNSec
    prepared <- case dialect of
        SQLite ->
            fmap (first (sqliteContractObservationWithResultStatement micros))
                <$> generateSqliteContractWithResultStatement micros sql
        DuckDB ->
            fmap (first (duckdbContractObservationWithResultStatement micros))
                <$> generateDuckDBContractWithResultStatement micros sql
        _ -> pure (Left "Result oracle requires SQLite or DuckDB")
    end <- getMonotonicTimeNSec
    case prepared of
        Left err -> pure (Left err)
        Right (observe, location) -> do
            rememberResult sql location
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
            (observation, location) <- observe candidate
            mapM_ (rememberResult candidate) location
            end <- getMonotonicTimeNSec
            pure (OracleObservationDetails observation (elapsed start end) 0 (Just (elapsed start end)))
