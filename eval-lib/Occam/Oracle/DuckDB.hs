{-# LANGUAGE OverloadedStrings #-}

module Occam.Oracle.DuckDB (
    DuckDBContract (..),
    DuckDBError (..),
    loadDuckDBContract,
    generateDuckDBContract,
    generateDuckDBContractTimeout,
    generateDuckDBContractWithResultStatement,
    generateDuckDBContractWithTrace,
    executeDuckDBCandidate,
    executeDuckDBCandidateTimeout,
    executeDuckDBCandidateDetailedTimeout,
    duckdbContractOracle,
    duckdbContractOracleTimeout,
    duckdbContractObservation,
    duckdbContractObservationTimeout,
    duckdbContractObservationWithResultStatement,
    duckdbContractObservationWithTrace,
    prepareDuckDBContractObserver,
    prepareDuckDBContractObserverWithReplay,
    isDuckDBAvailable,
    defaultDuckDBCandidateTimeoutMicros,
) where

import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value (..),
    eitherDecodeStrict',
    object,
    withObject,
    (.!=),
    (.:),
    (.:?),
    (.=),
 )
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (isJust)
import Data.Text (Text)
import Occam.Core (Oracle, ResultStatement (..), ResultTrace (..), traceResultStatement)
import Occam.Oracle (OracleObservation (..), observationOutcome)
import Occam.Oracle.DuckDB.Execution (
    ExecutionResult,
    ObserveResult,
    executeDuckDBResult,
    executeDuckDBResultWith,
 )
import Occam.Oracle.DuckDB.FFI (
    DuckDBError (..),
    isDuckDBAvailable,
    unsupportedResultTypes,
 )
import Occam.Oracle.DuckDB.Replay (prepareDuckDBReplayRunner)
import Occam.Oracle.DuckDB.Result (encodeExpectedRows, matchResult, observeRows)
import Occam.Oracle.Result (ResultSet (..))
import System.Exit (ExitCode (..))

data DuckDBContract = DuckDBContract
    { contractExpectedRows :: ![[Value]]
    , contractExpectedColumns :: ![Text]
    , contractExpectedTypes :: ![Int]
    , contractStateSha256 :: !(Maybe Text)
    , contractCheckState :: !Bool
    }
    deriving (Eq, Show)

instance FromJSON DuckDBContract where
    parseJSON = withObject "DuckDBContract" $ \o ->
        DuckDBContract
            <$> o .: "expected_rows"
            <*> o .: "expected_columns"
            <*> o .: "expected_types"
            <*> o .:? "state_sha256"
            <*> o .:? "check_state" .!= False

instance ToJSON DuckDBContract where
    toJSON :: DuckDBContract -> Value
    toJSON contract =
        object
            [ "expected_rows" .= contractExpectedRows contract
            , "expected_columns" .= contractExpectedColumns contract
            , "expected_types" .= contractExpectedTypes contract
            , "state_sha256" .= contractStateSha256 contract
            , "check_state" .= contractCheckState contract
            ]

loadDuckDBContract :: FilePath -> IO (Either String DuckDBContract)
loadDuckDBContract path = do
    bytes <- BS.readFile path
    pure (eitherDecodeStrict' bytes)

defaultDuckDBCandidateTimeoutMicros :: Int
defaultDuckDBCandidateTimeoutMicros = 100000

executeDuckDBCandidateDetailedTimeout :: Int -> ByteString -> IO (Either DuckDBError [[Value]])
executeDuckDBCandidateDetailedTimeout micros sql =
    fmap (maybe [] (resultRows . fst)) <$> executeDuckDBResult micros sql

executeDuckDBCandidateTimeout :: Int -> ByteString -> IO (Either Text [[Value]])
executeDuckDBCandidateTimeout micros sql = do
    res <- executeDuckDBCandidateDetailedTimeout micros sql
    pure $ case res of
        Left DuckDBTimeout -> Left "timeout"
        Left (DuckDBExecutionError err) -> Left err
        Left (DuckDBUnsupportedResultTypes types) -> Left (unsupportedResultTypes types)
        Left (DuckDBInfrastructureError err) -> Left ("infrastructure error: " <> err)
        Right rows -> Right rows

executeDuckDBCandidate :: ByteString -> IO (Either Text [[Value]])
executeDuckDBCandidate = executeDuckDBCandidateTimeout defaultDuckDBCandidateTimeoutMicros

generateDuckDBContract :: ByteString -> IO (Either Text DuckDBContract)
generateDuckDBContract = generateDuckDBContractTimeout defaultDuckDBCandidateTimeoutMicros

generateDuckDBContractTimeout :: Int -> ByteString -> IO (Either Text DuckDBContract)
generateDuckDBContractTimeout micros sql =
    fmap fst <$> generateDuckDBContractWithTrace micros sql

generateDuckDBContractWithResultStatement ::
    Int -> ByteString -> IO (Either Text (DuckDBContract, ResultStatement))
generateDuckDBContractWithResultStatement micros sql =
    fmap (second traceResultStatement) <$> generateDuckDBContractWithTrace micros sql

generateDuckDBContractWithTrace ::
    Int -> ByteString -> IO (Either Text (DuckDBContract, ResultTrace))
generateDuckDBContractWithTrace micros sql = do
    res <- executeDuckDBResult micros sql
    pure $ case res of
        Left DuckDBTimeout -> Left "timeout"
        Left (DuckDBExecutionError err) -> Left err
        Left (DuckDBUnsupportedResultTypes types) -> Left (unsupportedResultTypes types)
        Left (DuckDBInfrastructureError err) -> Left err
        Right Nothing -> Left "Input must produce a result set"
        Right (Just (result, trace)) ->
            Right
                ( DuckDBContract
                    { contractExpectedRows = resultRows result
                    , contractExpectedColumns = resultColumns result
                    , contractExpectedTypes = resultColumnTypes result
                    , contractStateSha256 = Nothing
                    , contractCheckState = False
                    }
                , trace
                )

duckdbContractOracleTimeout :: Int -> DuckDBContract -> Oracle
duckdbContractOracleTimeout micros contract candidate =
    observationOutcome <$> duckdbContractObservationTimeout micros contract candidate

duckdbContractOracle :: DuckDBContract -> Oracle
duckdbContractOracle = duckdbContractOracleTimeout defaultDuckDBCandidateTimeoutMicros

duckdbContractObservation :: DuckDBContract -> ByteString -> IO OracleObservation
duckdbContractObservation = duckdbContractObservationTimeout defaultDuckDBCandidateTimeoutMicros

duckdbContractObservationTimeout :: Int -> DuckDBContract -> ByteString -> IO OracleObservation
duckdbContractObservationTimeout micros contract candidate =
    fst <$> duckdbContractObservationWithTrace micros contract candidate

duckdbContractObservationWithResultStatement ::
    Int -> DuckDBContract -> ByteString -> IO (OracleObservation, Maybe ResultStatement)
duckdbContractObservationWithResultStatement micros contract candidate =
    second (fmap traceResultStatement) <$> duckdbContractObservationWithTrace micros contract candidate

duckdbContractObservationWithTrace ::
    Int -> DuckDBContract -> ByteString -> IO (OracleObservation, Maybe ResultTrace)
duckdbContractObservationWithTrace = prepareDuckDBContractObserver

{- | Encode the expected values once. Candidates compare directly against DuckDB
chunk vectors, including full strings, BLOBs, and exact floating-point bits.
-}
prepareDuckDBContractObserver ::
    Int -> DuckDBContract -> ByteString -> IO (OracleObservation, Maybe ResultTrace)
prepareDuckDBContractObserver micros contract =
    preparedObserver contract (`executeDuckDBResultWith` micros)

{- | The replay checkpoint owns an immutable, configured database. Every
candidate still gets its own connection, and only statically read-only
queries over the checkpoint's tables can use that database.
-}
prepareDuckDBContractObserverWithReplay ::
    Int ->
    DuckDBContract ->
    ByteString ->
    (ByteString -> IO ()) ->
    IO (ByteString -> IO (OracleObservation, Maybe ResultTrace))
prepareDuckDBContractObserverWithReplay micros contract baseline reportFailure = do
    run <- prepareDuckDBReplayRunner micros baseline reportFailure
    pure (preparedObserver contract run)

preparedObserver ::
    DuckDBContract ->
    (ObserveResult Bool -> ByteString -> IO (ExecutionResult Bool)) ->
    ByteString ->
    IO (OracleObservation, Maybe ResultTrace)
preparedObserver contract run
    -- TODO: Define a reproducible database-state observation before accepting state contracts.
    | contractCheckState contract || isJust (contractStateSha256 contract) =
        const (pure (OracleIOFailed "DuckDB state contracts are unsupported", Nothing))
    | otherwise = observe
    where
        expected = ResultSet columns types (contractExpectedRows contract)
        columns = contractExpectedColumns contract
        types = contractExpectedTypes contract
        encoded = encodeExpectedRows expected
        readMatch = case encoded of
            Just bytes -> matchResult (map fromIntegral types) columns bytes
            Nothing -> \api ptr -> fmap (== expected) <$> observeRows api ptr
        observe candidate = do
            res <- run readMatch candidate
            pure $ case res of
                Left DuckDBTimeout -> (OracleTimedOut, Nothing)
                Left (DuckDBExecutionError _) -> (OracleFinished (ExitFailure 1), Nothing)
                Left (DuckDBUnsupportedResultTypes actualTypes)
                    | actualTypes /= types -> (OracleFinished (ExitFailure 1), Nothing)
                    | otherwise -> (OracleIOFailed (unsupportedResultTypes actualTypes), Nothing)
                Left (DuckDBInfrastructureError err) -> (OracleIOFailed err, Nothing)
                Right (Just (True, trace)) -> (OracleFinished ExitSuccess, Just trace)
                Right _ -> (OracleFinished (ExitFailure 1), Nothing)
