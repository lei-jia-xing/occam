{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Occam.Oracle.Sqlite (
    SqliteContract (..),
    SqliteError (..),
    loadSqliteContract,
    generateSqliteContract,
    generateSqliteContractTimeout,
    generateSqliteContractWithResultStatement,
    executeSqliteCandidate,
    executeSqliteCandidateTimeout,
    executeSqliteCandidateDetailedTimeout,
    sqliteContractOracle,
    sqliteContractOracleTimeout,
    sqliteContractObservation,
    sqliteContractObservationTimeout,
    sqliteContractObservationWithResultStatement,
    splitStatements,
    sqlDataToValue,
) where

import Control.Exception.Safe (
    SomeException,
    bracket,
    displayException,
    fromException,
    throwIO,
    try,
 )
import Control.Monad (forM)
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
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isSpace, toLower)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Database.SQLite3 (
    SQLData (..),
    SQLError (..),
    Statement,
    StepResult (..),
    columnCount,
    columnName,
    columns,
    exec,
    finalize,
    interruptibly,
    step,
    withDatabase,
 )
import Database.SQLite3.Bindings (CDatabase, CError (..), c_sqlite3_prepare_v2)
import Database.SQLite3.Direct qualified as Direct
import Foreign (
    FunPtr,
    Ptr,
    alloca,
    freeHaskellFunPtr,
    minusPtr,
    nullFunPtr,
    nullPtr,
    peek,
    plusPtr,
 )
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import GHC.Float (castDoubleToWord64)
import Occam.Core (Oracle, ResultStatement (..))
import Occam.Oracle (OracleObservation (..), observationOutcome)
import Occam.Oracle.Result (ResultSet (..))
import System.Exit (ExitCode (..))
import System.Timeout (timeout)

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
sqlDataToValue (SQLFloat f) = object ["float64_bits" .= castDoubleToWord64 f]
sqlDataToValue (SQLText t) = String t
sqlDataToValue (SQLBlob b) = object ["blob" .= BS.unpack b]
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
executeSqliteCandidateDetailedTimeout micros sql =
    fmap (maybe [] (resultRows . fst)) <$> executeSqliteResult micros sql

executeSqliteResult ::
    Int -> ByteString -> IO (Either SqliteError (Maybe (ResultSet, ResultStatement)))
executeSqliteResult micros sql
    | BS.elem 0 sql = pure (Left (SqliteExecutionError "SQL contains a NUL byte"))
    | Left err <- TE.decodeUtf8' sql = pure (Left (SqliteInfrastructureError (T.pack (show err))))
    | otherwise = do
        res <- try $
            withDatabase ":memory:" $ \db ->
                timeout micros $
                    interruptibly db $ do
                        -- Keep the engine baseline aligned with SQLite parser session defaults.
                        exec db "PRAGMA foreign_keys = OFF;"
                        withLocalStatements db $ BS.useAsCStringLen sql $ \(start, size) ->
                            runStatements db (start `plusPtr` size) start 0 Nothing
        pure $ case res of
            Left err -> Left (classifySqliteException err)
            Right Nothing -> Left SqliteTimeout
            Right (Just result) -> Right result

type Authorizer = Ptr () -> CInt -> CString -> CString -> CString -> CString -> IO CInt

foreign import ccall "wrapper" makeAuthorizer :: Authorizer -> IO (FunPtr Authorizer)

foreign import ccall unsafe "sqlite3_set_authorizer"
    setAuthorizer :: Ptr CDatabase -> FunPtr Authorizer -> Ptr () -> IO CInt

-- NOTE: A fresh in-memory database still permits ATTACH and some process-wide
-- PRAGMAs. Allow only database-local PRAGMAs here.
withLocalStatements :: Direct.Database -> IO a -> IO a
withLocalStatements (Direct.Database db) action =
    bracket (makeAuthorizer authorize) freeHaskellFunPtr $ \callback ->
        bracket (install callback) (const (install nullFunPtr)) (const action)
    where
        install callback = do
            status <- setAuthorizer db callback nullPtr
            if status == 0 then pure () else fail "Could not install SQLite authorizer"
        authorize _ op arg _ _ _
            | op == 19 && arg /= nullPtr = do
                -- SQLITE_PRAGMA
                pragma <- BSC.map toLower <$> BS.packCString arg
                pure $
                    if pragma `elem` ["foreign_keys", "defer_foreign_keys", "recursive_triggers", "user_version"]
                        then 0
                        else 1
            | op `elem` [24, 25, 29] = pure 1 -- ATTACH, DETACH, CREATE VIRTUAL TABLE
            | otherwise = pure 0

-- SQLite supplies the unconsumed tail, including for trigger bodies and
-- multiple statements on one line. Every prepared handle is finalized before
-- the next statement starts.
runStatements ::
    Direct.Database ->
    CString ->
    CString ->
    Int ->
    Maybe (ResultSet, Int) ->
    IO (Maybe (ResultSet, ResultStatement))
runStatements db@(Direct.Database handle) end start ordinal previous
    | start == end = pure (fmap (second (ResultStatement ordinal)) previous)
    | otherwise = do
        (next, nextOrdinal, result) <- bracket prepare release $ \(tailPtr, statement) -> do
            (nextIndex, observed) <- case statement of
                Nothing -> pure (ordinal, previous)
                Just stmt -> do
                    count <- columnCount stmt
                    names <- forM [0 .. count - 1] $ \i -> do
                        name <- columnName stmt i
                        maybe (fail "SQLite omitted a result column name") pure name
                    rows <- stepAll stmt
                    pure (ordinal + 1, if null names then previous else Just (ResultSet names [] rows, ordinal))
            pure (tailPtr, nextIndex, observed)
        runStatements db end next nextOrdinal result
    where
        prepare = alloca $ \out -> alloca $ \tailOut -> do
            CError status <- c_sqlite3_prepare_v2 handle start (fromIntegral (end `minusPtr` start)) out tailOut
            if status /= 0
                then do
                    code <- Direct.errcode db
                    Direct.Utf8 message <- Direct.errmsg db
                    throwIO (SQLError code (TE.decodeUtf8 message) "prepare")
                else do
                    next <- peek tailOut
                    stmt <- peek out
                    pure (next, if stmt == nullPtr then Nothing else Just (Direct.Statement stmt))
        release (_, statement) = mapM_ finalize statement

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
    fmap fst <$> generateSqliteContractWithResultStatement micros sql

generateSqliteContractWithResultStatement ::
    Int -> ByteString -> IO (Either Text (SqliteContract, ResultStatement))
generateSqliteContractWithResultStatement micros sql = do
    res <- executeSqliteResult micros sql
    pure $ case res of
        Left SqliteTimeout -> Left "timeout"
        Left (SqliteExecutionError err) -> Left err
        Left (SqliteInfrastructureError err) -> Left err
        Right Nothing -> Left "Input must produce a result set"
        Right (Just (result, location)) ->
            Right
                ( SqliteContract
                    { contractExpectedRows = resultRows result
                    , contractExpectedColumns = resultColumns result
                    , contractStateSha256 = Nothing
                    , contractCheckState = False
                    }
                , location
                )

sqliteContractOracleTimeout :: Int -> SqliteContract -> Oracle
sqliteContractOracleTimeout micros contract candidate =
    observationOutcome <$> sqliteContractObservationTimeout micros contract candidate

sqliteContractOracle :: SqliteContract -> Oracle
sqliteContractOracle = sqliteContractOracleTimeout defaultCandidateTimeoutMicros

sqliteContractObservation :: SqliteContract -> ByteString -> IO OracleObservation
sqliteContractObservation = sqliteContractObservationTimeout defaultCandidateTimeoutMicros

sqliteContractObservationTimeout :: Int -> SqliteContract -> ByteString -> IO OracleObservation
sqliteContractObservationTimeout micros contract candidate =
    fst <$> sqliteContractObservationWithResultStatement micros contract candidate

sqliteContractObservationWithResultStatement ::
    Int -> SqliteContract -> ByteString -> IO (OracleObservation, Maybe ResultStatement)
sqliteContractObservationWithResultStatement micros contract candidate
    -- TODO: Define a reproducible database-state observation before accepting state contracts.
    | contractCheckState contract || isJust (contractStateSha256 contract) =
        pure (OracleIOFailed "SQLite state contracts are unsupported", Nothing)
    | otherwise = do
        res <- executeSqliteResult micros candidate
        pure $ case res of
            Left SqliteTimeout -> (OracleTimedOut, Nothing)
            Left (SqliteExecutionError _) -> (OracleFinished (ExitFailure 1), Nothing)
            Left (SqliteInfrastructureError err) -> (OracleIOFailed err, Nothing)
            Right observed
                | Just (result, location) <- observed
                , result == expected ->
                    (OracleFinished ExitSuccess, Just location)
                | otherwise -> (OracleFinished (ExitFailure 1), Nothing)
    where
        expected = ResultSet (contractExpectedColumns contract) [] (contractExpectedRows contract)
