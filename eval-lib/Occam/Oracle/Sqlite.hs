{-# LANGUAGE OverloadedStrings #-}

module Occam.Oracle.Sqlite (
    SqliteContract (..),
    SqliteError (..),
    loadSqliteContract,
    generateSqliteContract,
    generateSqliteContractTimeout,
    generateSqliteContractWithResultStatement,
    generateSqliteContractWithTrace,
    executeSqliteCandidate,
    executeSqliteCandidateTimeout,
    executeSqliteCandidateDetailedTimeout,
    sqliteContractOracle,
    sqliteContractOracleTimeout,
    sqliteContractObservation,
    sqliteContractObservationTimeout,
    sqliteContractObservationWithResultStatement,
    sqliteContractObservationWithTrace,
    prepareSqliteContractObserver,
    prepareSqliteContractObserverWithTrace,
    prepareFreshSqliteContractObserver,
    splitStatements,
    sqlDataToValue,
) where

import Control.Monad (when)
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
import Data.Foldable (for_)
import Data.Maybe (isJust)
import Data.Text (Text)
import Occam.Core (Oracle, ResultStatement (..), ResultTrace (..), traceResultStatement)
import Occam.Oracle (OracleObservation (..), observationOutcome)
import Occam.Oracle.Result (ResultSet (..))
import Occam.Oracle.Sqlite.FFI (
    splitStatements,
    withPooledSqliteConn,
    withSqliteDeadline,
 )
import Occam.Oracle.Sqlite.Match (
    buildSqliteCheckpoints,
    prepareSqliteResultObserver,
 )
import Occam.Oracle.Sqlite.Result (
    SqliteError (..),
    SqliteRun (..),
    buildSqliteTrace,
    encodeSqliteExpected,
    executeSqliteRun,
    executeSqliteRunWithin,
    sqlDataToValue,
 )

data SqliteContract = SqliteContract
    { contractExpectedRows :: ![[Value]]
    , contractExpectedColumns :: ![Text]
    , contractStateSha256 :: !(Maybe Text)
    , contractCheckState :: !Bool
    }
    deriving (Eq, Show)

instance FromJSON SqliteContract where
    parseJSON = withObject "SqliteContract" $ \o ->
        SqliteContract
            <$> o .: "expected_rows"
            <*> o .: "expected_columns"
            <*> o .:? "state_sha256"
            <*> o .:? "check_state" .!= False

instance ToJSON SqliteContract where
    toJSON :: SqliteContract -> Value
    toJSON contract =
        object
            [ "expected_rows" .= contractExpectedRows contract
            , "expected_columns" .= contractExpectedColumns contract
            , "state_sha256" .= contractStateSha256 contract
            , "check_state" .= contractCheckState contract
            ]

loadSqliteContract :: FilePath -> IO (Either String SqliteContract)
loadSqliteContract path = do
    bytes <- BS.readFile path
    pure (eitherDecodeStrict' bytes)

-- | Default per-candidate execution timeout (100ms for in-memory SQLite).
defaultCandidateTimeoutMicros :: Int
defaultCandidateTimeoutMicros = 100000

-- | Execute candidate SQL against an in-memory SQLite database with timeout.
executeSqliteCandidateDetailedTimeout :: Int -> ByteString -> IO (Either SqliteError [[Value]])
executeSqliteCandidateDetailedTimeout micros sql =
    fmap (maybe [] (resultRows . srLastResult)) <$> executeSqliteRun micros sql

executeSqliteCandidateTimeout :: Int -> ByteString -> IO (Either Text [[Value]])
executeSqliteCandidateTimeout micros sql = do
    res <- executeSqliteCandidateDetailedTimeout micros sql
    pure $ case res of
        Left SqliteTimeout -> Left "timeout"
        Left (SqliteExecutionError err) -> Left err
        Left (SqliteInfrastructureError err) -> Left ("infrastructure error: " <> err)
        Right rows -> Right rows

executeSqliteCandidate :: ByteString -> IO (Either Text [[Value]])
executeSqliteCandidate = executeSqliteCandidateTimeout defaultCandidateTimeoutMicros

-- | Generate a contract by executing an authoritative SQL script.
generateSqliteContract :: ByteString -> IO (Either Text SqliteContract)
generateSqliteContract = generateSqliteContractTimeout defaultCandidateTimeoutMicros

generateSqliteContractTimeout :: Int -> ByteString -> IO (Either Text SqliteContract)
generateSqliteContractTimeout micros sql =
    fmap fst <$> generateSqliteContractWithTrace micros sql

generateSqliteContractWithResultStatement ::
    Int -> ByteString -> IO (Either Text (SqliteContract, ResultStatement))
generateSqliteContractWithResultStatement micros sql =
    fmap (second traceResultStatement) <$> generateSqliteContractWithTrace micros sql

generateSqliteContractWithTrace ::
    Int -> ByteString -> IO (Either Text (SqliteContract, ResultTrace))
generateSqliteContractWithTrace micros sql =
    withSqliteDeadline micros (Left "timeout") $ \remaining -> do
        res <- executeSqliteRunWithin False remaining sql
        case res of
            Left SqliteTimeout -> pure (Left "timeout")
            Left (SqliteExecutionError err) -> pure (Left err)
            Left (SqliteInfrastructureError err) -> pure (Left err)
            Right Nothing -> pure (Left "Input must produce a result set")
            Right (Just run) -> do
                let result = srLastResult run
                trace <- buildSqliteTrace sql result run
                when (traceDeterministic trace && traceStatementCount trace >= 8) $
                    for_ (encodeSqliteExpected result) \expBytes ->
                        withPooledSqliteConn $ \conn -> do
                            ok <- buildSqliteCheckpoints remaining expBytes sql (traceStatementCount trace) conn
                            pure (ok, ())
                pure $
                    Right
                        ( SqliteContract
                            { contractExpectedRows = resultRows result
                            , contractExpectedColumns = resultColumns result
                            , contractStateSha256 = Nothing
                            , contractCheckState = False
                            }
                        , trace
                        )

{- | Prepare a reusable fast SQLite contract observer that pre-encodes expected
results once into binary form for zero-allocation C-side matching and
optionally reports deterministic semicolon-terminated failing prefixes.
-}
prepareSqliteContractObserver ::
    Int ->
    SqliteContract ->
    (ByteString -> IO ()) ->
    (ByteString -> IO (OracleObservation, Maybe ResultTrace))
prepareSqliteContractObserver = prepareSqliteContractObserverWithTrace True

prepareSqliteContractObserverWithTrace ::
    Bool ->
    Int ->
    SqliteContract ->
    (ByteString -> IO ()) ->
    (ByteString -> IO (OracleObservation, Maybe ResultTrace))
prepareSqliteContractObserverWithTrace = prepareSqliteObserver False

prepareFreshSqliteContractObserver :: Int -> SqliteContract -> ByteString -> IO OracleObservation
prepareFreshSqliteContractObserver micros contract =
    fmap fst . prepareSqliteObserver True False micros contract (const (pure ()))

prepareSqliteObserver ::
    Bool ->
    Bool ->
    Int ->
    SqliteContract ->
    (ByteString -> IO ()) ->
    ByteString ->
    IO (OracleObservation, Maybe ResultTrace)
prepareSqliteObserver fresh needTrace micros contract onFailingPrefix
    -- TODO: Define a reproducible database-state observation before accepting state contracts.
    | contractCheckState contract || isJust (contractStateSha256 contract) =
        const (pure (OracleIOFailed "SQLite state contracts are unsupported", Nothing))
    | otherwise =
        prepareSqliteResultObserver fresh needTrace micros expected onFailingPrefix
    where
        expected = ResultSet (contractExpectedColumns contract) [] (contractExpectedRows contract)

sqliteContractOracleTimeout :: Int -> SqliteContract -> Oracle
sqliteContractOracleTimeout micros contract candidate =
    observationOutcome <$> sqliteContractObservationTimeout micros contract candidate

sqliteContractOracle :: SqliteContract -> Oracle
sqliteContractOracle = sqliteContractOracleTimeout defaultCandidateTimeoutMicros

sqliteContractObservation :: SqliteContract -> ByteString -> IO OracleObservation
sqliteContractObservation = sqliteContractObservationTimeout defaultCandidateTimeoutMicros

sqliteContractObservationTimeout :: Int -> SqliteContract -> ByteString -> IO OracleObservation
sqliteContractObservationTimeout micros contract candidate =
    fst <$> prepareSqliteContractObserverWithTrace False micros contract (const (pure ())) candidate

sqliteContractObservationWithResultStatement ::
    Int -> SqliteContract -> ByteString -> IO (OracleObservation, Maybe ResultStatement)
sqliteContractObservationWithResultStatement micros contract candidate =
    second (fmap traceResultStatement) <$> sqliteContractObservationWithTrace micros contract candidate

sqliteContractObservationWithTrace ::
    Int -> SqliteContract -> ByteString -> IO (OracleObservation, Maybe ResultTrace)
sqliteContractObservationWithTrace micros contract =
    prepareSqliteContractObserver micros contract (const (pure ()))
