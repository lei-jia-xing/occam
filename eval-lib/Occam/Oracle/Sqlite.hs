{-# LANGUAGE OverloadedStrings #-}

-- | In-process SQLite result oracle: contract serialization, trace construction, and fast matching.
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

-- | Serializable oracle contract capturing expected SQLite result rows, columns, and optional state hash.
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

-- | Decode a JSON 'SqliteContract' file from disk.
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

-- | Execute SQLite SQL with an explicit timeout and return the final result rows or an error message.
executeSqliteCandidateTimeout :: Int -> ByteString -> IO (Either Text [[Value]])
executeSqliteCandidateTimeout micros sql = do
    res <- executeSqliteCandidateDetailedTimeout micros sql
    pure $ case res of
        Left SqliteTimeout -> Left "timeout"
        Left (SqliteExecutionError err) -> Left err
        Left (SqliteInfrastructureError err) -> Left ("infrastructure error: " <> err)
        Right rows -> Right rows

-- | Execute SQLite SQL using 'defaultCandidateTimeoutMicros'.
executeSqliteCandidate :: ByteString -> IO (Either Text [[Value]])
executeSqliteCandidate = executeSqliteCandidateTimeout defaultCandidateTimeoutMicros

-- | Generate a contract by executing an authoritative SQL script.
generateSqliteContract :: ByteString -> IO (Either Text SqliteContract)
generateSqliteContract = generateSqliteContractTimeout defaultCandidateTimeoutMicros

-- | Generate a SQLite contract under an explicit microsecond timeout.
generateSqliteContractTimeout :: Int -> ByteString -> IO (Either Text SqliteContract)
generateSqliteContractTimeout micros sql =
    fmap fst <$> generateSqliteContractWithTrace micros sql

-- | Generate a SQLite contract together with a 'ResultStatement' summary.
generateSqliteContractWithResultStatement ::
    Int -> ByteString -> IO (Either Text (SqliteContract, ResultStatement))
generateSqliteContractWithResultStatement micros sql =
    fmap (second traceResultStatement) <$> generateSqliteContractWithTrace micros sql

-- | Generate a SQLite contract together with a full execution 'ResultTrace' and checkpoints.
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

-- | Prepare an observer yielding 'OracleObservation' and optionally a 'ResultTrace', defaulting to tracing enabled.
prepareSqliteContractObserverWithTrace ::
    Bool ->
    Int ->
    SqliteContract ->
    (ByteString -> IO ()) ->
    (ByteString -> IO (OracleObservation, Maybe ResultTrace))
prepareSqliteContractObserverWithTrace = prepareSqliteObserver False

-- | Prepare a stateless observer creating a fresh database per invocation (used for validation).
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

-- | Convert a 'SqliteContract' into a primitive 'Oracle' under an explicit timeout.
sqliteContractOracleTimeout :: Int -> SqliteContract -> Oracle
sqliteContractOracleTimeout micros contract candidate =
    observationOutcome <$> sqliteContractObservationTimeout micros contract candidate

-- | Convert a 'SqliteContract' into a primitive 'Oracle' using 'defaultCandidateTimeoutMicros'.
sqliteContractOracle :: SqliteContract -> Oracle
sqliteContractOracle = sqliteContractOracleTimeout defaultCandidateTimeoutMicros

-- | Observe a candidate against a 'SqliteContract' using 'defaultCandidateTimeoutMicros'.
sqliteContractObservation :: SqliteContract -> ByteString -> IO OracleObservation
sqliteContractObservation = sqliteContractObservationTimeout defaultCandidateTimeoutMicros

-- | Observe a candidate against a 'SqliteContract' under an explicit timeout.
sqliteContractObservationTimeout :: Int -> SqliteContract -> ByteString -> IO OracleObservation
sqliteContractObservationTimeout micros contract candidate =
    fst <$> prepareSqliteContractObserverWithTrace False micros contract (const (pure ())) candidate

-- | Observe a candidate against a 'SqliteContract' and summarize its final 'ResultStatement'.
sqliteContractObservationWithResultStatement ::
    Int -> SqliteContract -> ByteString -> IO (OracleObservation, Maybe ResultStatement)
sqliteContractObservationWithResultStatement micros contract candidate =
    second (fmap traceResultStatement) <$> sqliteContractObservationWithTrace micros contract candidate

-- | Observe a candidate against a 'SqliteContract' and optionally return a full 'ResultTrace'.
sqliteContractObservationWithTrace ::
    Int -> SqliteContract -> ByteString -> IO (OracleObservation, Maybe ResultTrace)
sqliteContractObservationWithTrace micros contract =
    prepareSqliteContractObserver micros contract (const (pure ()))
