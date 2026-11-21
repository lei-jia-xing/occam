{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Occam.Oracle.Sqlite (
    SqliteContract (..),
    SqliteError (..),
    loadSqliteContract,
    generateSqliteContract,
    executeSqliteCandidate,
    executeSqliteCandidateTimeout,
    executeSqliteCandidateDetailedTimeout,
    sqliteContractOracle,
    sqliteContractOracleTimeout,
    sqliteContractObservation,
    splitStatements,
    sqlDataToValue,
) where

import Control.Exception.Safe (SomeException, displayException, fromException, try)
import Control.Monad (forM_, unless)
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
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isSpace)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Database.SQLite3 (
    SQLData (..),
    SQLError (..),
    Statement,
    StepResult (..),
    columns,
    exec,
    interruptibly,
    step,
    withDatabase,
    withStatement,
 )
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Occam.Core (Oracle, OracleOutcome (..))
import Occam.Oracle (OracleObservation (..))
import System.Exit (ExitCode (..))
import System.Timeout (timeout)

data SqliteContract = SqliteContract
    { contractExpectedRows :: ![[Value]]
    , contractStateSha256 :: !(Maybe Text)
    , contractCheckState :: !Bool
    }
    deriving (Eq, Show)

instance FromJSON SqliteContract where
    parseJSON = withObject "SqliteContract" $ \o ->
        SqliteContract
            <$> o .: "expected_rows"
            <*> o .:? "state_sha256"
            <*> o .:? "check_state" .!= False

instance ToJSON SqliteContract where
    toJSON :: SqliteContract -> Value
    toJSON contract =
        object
            [ "expected_rows" .= contractExpectedRows contract
            , "state_sha256" .= contractStateSha256 contract
            , "check_state" .= contractCheckState contract
            ]

loadSqliteContract :: FilePath -> IO (Either String SqliteContract)
loadSqliteContract path = do
    bytes <- BS.readFile path
    pure (eitherDecodeStrict' bytes)

foreign import ccall unsafe "sqlite3_complete"
    c_sqlite3_complete :: CString -> IO CInt

isCompleteStatement :: ByteString -> IO Bool
isCompleteStatement bs =
    BS.useAsCString bs $ \cStr -> do
        res <- c_sqlite3_complete cStr
        pure (res /= 0)

trimBS :: ByteString -> ByteString
trimBS = BSC.dropWhile isSpace . fst . BSC.spanEnd isSpace

-- | Split SQL into individual statements matching sqlite3_complete semantics.
splitStatements :: ByteString -> IO [ByteString]
splitStatements sql = go (BSC.lines sql) "" []
    where
        go [] buf acc
            | not (BSC.null (trimBS buf)) = pure (reverse (trimBS buf : acc))
            | otherwise = pure (reverse acc)
        go (line : rest) buf acc = do
            let buf' = if BSC.null buf then line else buf <> "\n" <> line
            complete <- isCompleteStatement (buf' <> "\0")
            if complete
                then
                    let trimmed = trimBS buf'
                     in if BSC.null trimmed
                            then go rest "" acc
                            else go rest "" (trimmed : acc)
                else go rest buf' acc

sqlDataToValue :: SQLData -> Value
sqlDataToValue (SQLInteger n) = Number (fromIntegral n)
sqlDataToValue (SQLFloat f) = toJSON f
sqlDataToValue (SQLText t) = String t
sqlDataToValue (SQLBlob b) = String (TE.decodeUtf8 b)
sqlDataToValue SQLNull = Null

stepAll :: Statement -> IO [[Value]]
stepAll stmt = go []
    where
        go acc = do
            res <- step stmt
            case res of
                Row -> do
                    cols <- columns stmt
                    let vals = map sqlDataToValue cols
                    go (vals : acc)
                Done -> pure (reverse acc)

-- | Default per-candidate execution timeout (100ms for in-memory SQLite).
defaultCandidateTimeoutMicros :: Int
defaultCandidateTimeoutMicros = 100000

-- | Failure classes for in-process SQLite execution.
data SqliteError
    = SqliteTimeout
    | SqliteExecutionError !Text
    | SqliteInfrastructureError !Text
    deriving (Eq, Show)

classifySqliteException :: SomeException -> SqliteError
classifySqliteException err =
    case fromException err of
        Just sqlErr ->
            let details = sqlErrorDetails sqlErr
                context = sqlErrorContext sqlErr
                message = if T.null details then T.pack (show (sqlError sqlErr)) else details
             in SqliteExecutionError (if T.null context then message else context <> ": " <> message)
        Nothing -> SqliteInfrastructureError (T.pack (displayException err))

-- | Execute candidate SQL against an in-memory SQLite database with timeout.
executeSqliteCandidateDetailedTimeout :: Int -> ByteString -> IO (Either SqliteError [[Value]])
executeSqliteCandidateDetailedTimeout micros sql = do
    stmts <- splitStatements sql
    res <-
        try $
            withDatabase ":memory:" $ \db ->
                interruptibly db $ do
                    mRes <-
                        timeout micros $ do
                            exec db "PRAGMA foreign_keys = ON;"
                            lastRowsRef <- newIORef []
                            forM_ stmts $ \stmtBytes -> do
                                let trimmed = trimBS stmtBytes
                                unless (BSC.null trimmed) $ do
                                    let stmtText = TE.decodeUtf8 trimmed
                                    withStatement db stmtText $ \stmt -> do
                                        cols <- stepAll stmt
                                        unless (null cols) $
                                            writeIORef lastRowsRef cols
                            readIORef lastRowsRef
                    pure $ case mRes of
                        Nothing -> Left SqliteTimeout
                        Just rows -> Right rows
    pure $ case res of
        Left (err :: SomeException) -> Left (classifySqliteException err)
        Right inner -> inner

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
generateSqliteContract sql = do
    res <- executeSqliteCandidate sql
    pure $ case res of
        Left err -> Left err
        Right rows ->
            Right
                SqliteContract
                    { contractExpectedRows = rows
                    , contractStateSha256 = Nothing
                    , contractCheckState = False
                    }

-- | In-process contract oracle for SQLite candidate reduction.
sqliteContractOracleTimeout :: Int -> SqliteContract -> Oracle
sqliteContractOracleTimeout micros contract candidate = do
    res <- executeSqliteCandidateDetailedTimeout micros candidate
    case res of
        Left SqliteTimeout -> pure OracleInconclusive
        Left (SqliteInfrastructureError _) -> pure OracleInconclusive
        Left (SqliteExecutionError _) -> pure OracleRejected
        Right rows
            | rows == contractExpectedRows contract -> pure OracleAccepted
            | otherwise -> pure OracleRejected

sqliteContractOracle :: SqliteContract -> Oracle
sqliteContractOracle = sqliteContractOracleTimeout defaultCandidateTimeoutMicros

-- | Observation adapter that preserves timeout and execution-error semantics.
sqliteContractObservation :: SqliteContract -> ByteString -> IO OracleObservation
sqliteContractObservation contract candidate = do
    res <- executeSqliteCandidateDetailedTimeout defaultCandidateTimeoutMicros candidate
    pure $ case res of
        Left SqliteTimeout -> OracleTimedOut
        Left (SqliteExecutionError err) -> OracleIOFailed err
        Left (SqliteInfrastructureError err) -> OracleIOFailed err
        Right rows
            | rows == contractExpectedRows contract -> OracleFinished ExitSuccess
            | otherwise -> OracleFinished (ExitFailure 1)
