{-# LANGUAGE ForeignFunctionInterface #-}
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
    splitStatements,
    sqlDataToValue,
) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (bracket, mask, onException)
import Control.Exception.Safe (
    SomeException,
    displayException,
    fromException,
    throwIO,
    try,
 )
import Control.Monad (forM, unless, when)
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
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Unsafe qualified as BSU
import Data.Char (isSpace)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntSet qualified as IntSet
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import Database.SQLite3 (
    SQLData (..),
    SQLError (..),
    Statement,
    StepResult (..),
    close,
    columnCount,
    columnName,
    columns,
    exec,
    finalize,
    interruptibly,
    open,
    step,
 )
import Database.SQLite3.Bindings (
    CDatabase,
    CStatement,
    c_sqlite3_finalize,
 )
import Database.SQLite3.Direct qualified as Direct
import Foreign (
    FunPtr,
    Ptr,
    alloca,
    allocaArray,
    castPtr,
    free,
    malloc,
    nullFunPtr,
    nullPtr,
    peek,
    peekArray,
    plusPtr,
    poke,
 )
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..), CSize (..))
import GHC.Float (castDoubleToWord64)
import Occam.Core (Oracle, ResultStatement (..), ResultTrace (..), traceResultStatement)
import Occam.Oracle (OracleObservation (..), observationOutcome)
import Occam.Oracle.Result (ResultSet (..))
import System.Exit (ExitCode (..))
import System.IO.Unsafe (unsafePerformIO)
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
    fmap (maybe [] (resultRows . srLastResult)) <$> executeSqliteRun micros sql

data SqliteConn = SqliteConn
    { scDb :: !Direct.Database
    , scTemplate :: !Direct.Database
    , scDirtyPtr :: !(Ptr CInt)
    , scDmlTainted :: !(IORef Bool)
    }

dirtyTemp, dirtyDml, dirtyConnCounter, dirtyNondet :: CInt
dirtyTemp = 4
dirtyDml = 8
dirtyConnCounter = 16
dirtyNondet = 32

maxPooledConns :: Int
maxPooledConns = 16

{-# NOINLINE sharedSqlitePool #-}
sharedSqlitePool :: MVar [SqliteConn]
sharedSqlitePool = unsafePerformIO (newMVar [])

type Authorizer = Ptr () -> CInt -> CString -> CString -> CString -> CString -> IO CInt

foreign import ccall "&occam_sqlite_authorizer"
    sharedAuthorizer :: FunPtr Authorizer

foreign import ccall unsafe "sqlite3_set_authorizer"
    setAuthorizer :: Ptr CDatabase -> FunPtr Authorizer -> Ptr () -> IO CInt

foreign import ccall safe "occam_sqlite_reset"
    c_occamSqliteReset :: Ptr CDatabase -> Ptr CDatabase -> Ptr CInt -> IO CInt

foreign import ccall safe "occam_sqlite_next_result"
    c_occamSqliteNextResult ::
        Ptr CDatabase ->
        CString ->
        CString ->
        CString ->
        CInt ->
        Ptr CInt ->
        CInt ->
        Ptr CString ->
        Ptr (Ptr CStatement) ->
        Ptr CInt ->
        IO CInt

openFreshMemoryDb :: IO Direct.Database
openFreshMemoryDb = open ":memory:"

createSqliteConn :: IO SqliteConn
createSqliteConn = do
    db <- openFreshMemoryDb
    tmpl <- openFreshMemoryDb `onException` close db
    dirtyPtr <- malloc `onException` (close tmpl >> close db)
    let cleanup = free dirtyPtr >> close tmpl >> close db
    poke dirtyPtr 0
    -- Keep the engine baseline aligned with SQLite parser session defaults.
    exec db "PRAGMA foreign_keys = OFF;" `onException` cleanup
    dmlTainted <- newIORef False
    let Direct.Database dbPtr = db
    status <- setAuthorizer dbPtr sharedAuthorizer (castPtr dirtyPtr)
    if status /= 0
        then cleanup >> fail "Could not install SQLite authorizer"
        else
            pure SqliteConn {scDb = db, scTemplate = tmpl, scDirtyPtr = dirtyPtr, scDmlTainted = dmlTainted}

destroySqliteConn :: SqliteConn -> IO ()
destroySqliteConn SqliteConn {..} = do
    let Direct.Database dbPtr = scDb
    _ <- setAuthorizer dbPtr nullFunPtr nullPtr
    free scDirtyPtr
    close scTemplate
    close scDb

resetSqliteConn :: SqliteConn -> IO Bool
resetSqliteConn SqliteConn {..} = do
    let Direct.Database dbPtr = scDb
        Direct.Database tmplPtr = scTemplate
    (/= 0) <$> c_occamSqliteReset dbPtr tmplPtr scDirtyPtr

acquireSqliteConn :: IO SqliteConn
acquireSqliteConn = do
    mConn <- modifyMVar sharedSqlitePool $ \case
        [] -> pure ([], Nothing)
        (c : cs) -> pure (cs, Just c)
    maybe createSqliteConn pure mConn

releaseSqliteConn :: Bool -> SqliteConn -> IO ()
releaseSqliteConn False conn = destroySqliteConn conn
releaseSqliteConn True conn = do
    ok <- resetSqliteConn conn
    if not ok
        then destroySqliteConn conn
        else do
            returned <- modifyMVar sharedSqlitePool $ \conns ->
                if length conns < maxPooledConns
                    then pure (conn : conns, True)
                    else pure (conns, False)
            unless returned (destroySqliteConn conn)

withPooledSqliteConn :: (SqliteConn -> IO (Bool, a)) -> IO a
withPooledSqliteConn action = mask $ \restore -> do
    conn <- acquireSqliteConn
    (keep, val) <- restore (action conn) `onException` destroySqliteConn conn
    releaseSqliteConn keep conn
    pure val

data SqliteRun = SqliteRun
    { srLastResult :: !ResultSet
    , srLastIndex :: !Int
    , srResults :: ![(Int, ResultSet)]
    , srStatementCount :: !Int
    , srDeterministic :: !Bool
    , srEndOffsets :: !(Maybe [Int])
    }

buildSqliteTrace :: ResultSet -> SqliteRun -> ResultTrace
buildSqliteTrace expected SqliteRun {..} =
    ResultTrace
        { traceStatementCount = srStatementCount
        , traceFirstResultIndex = maybe srLastIndex fst (listHead srResults)
        , traceLastResultIndex = srLastIndex
        , traceResultIndices = IntSet.fromList (map fst srResults)
        , tracePrefixMatchIndices = prefixMatches 0 Nothing srResults IntSet.empty
        , traceDeterministic = srDeterministic
        , traceEndByteOffsets = srEndOffsets
        }
    where
        listHead [] = Nothing
        listHead (x : _) = Just x
        prefixMatches !k !latest pending !acc
            | k >= srStatementCount = acc
            | otherwise = case pending of
                (ord, rs) : rest | ord == k ->
                    let acc' = if rs == expected then IntSet.insert k acc else acc
                     in prefixMatches (k + 1) (Just rs) rest acc'
                _ ->
                    let acc' = if latest == Just expected then IntSet.insert k acc else acc
                     in prefixMatches (k + 1) latest pending acc'

runCandidateOnConn ::
    Int ->
    ByteString ->
    SqliteConn ->
    IO (Bool, Bool, Either SqliteError (Maybe SqliteRun))
runCandidateOnConn micros sql conn = do
    poke (scDirtyPtr conn) 0
    tainted <- readIORef (scDmlTainted conn)
    let db = scDb conn
        maxOffsets = max 64 (BS.count 59 sql + 16)
    res <- try $
        timeout micros $
            interruptibly db $
                BS.useAsCStringLen sql $ \(start, size) ->
                    allocaArray maxOffsets $ \offsetsPtr ->
                        runStatements db offsetsPtr maxOffsets start (start `plusPtr` size) start 0 []
    dirty <- peek (scDirtyPtr conn)
    when ((dirty .&. dirtyDml) /= 0) $
        writeIORef (scDmlTainted conn) True
    let poolable = (dirty .&. dirtyTemp) == 0
        counterPolluted = tainted && (dirty .&. dirtyConnCounter) /= 0
        authorizerDet = (dirty .&. dirtyNondet) == 0
    case res of
        Left err ->
            case classifySqliteException err of
                e@(SqliteExecutionError _) -> pure (poolable, counterPolluted, Left e)
                e -> pure (False, False, Left e)
        Right Nothing -> pure (False, False, Left SqliteTimeout)
        Right (Just (totalStmts, revResults, offsets)) ->
            case revResults of
                [] -> pure (poolable, counterPolluted, Right Nothing)
                (lastIdx, lastRs) : _ -> do
                    hasNondetKw <-
                        if authorizerDet
                            then
                                (/= 0)
                                    <$> BSU.unsafeUseAsCStringLen sql (\(ptr, len) -> c_hasNondetKeyword (castPtr ptr) (fromIntegral len))
                            else pure True
                    pure
                        ( poolable
                        , counterPolluted
                        , Right $
                            Just
                                SqliteRun
                                    { srLastResult = lastRs
                                    , srLastIndex = lastIdx
                                    , srResults = reverse revResults
                                    , srStatementCount = totalStmts
                                    , srDeterministic = authorizerDet && not hasNondetKw
                                    , srEndOffsets = offsets
                                    }
                        )

foreign import ccall unsafe "occam_sqlite_check_sql"
    c_checkSql :: Ptr Word8 -> CSize -> IO CInt

foreign import ccall unsafe "occam_sqlite_has_nondet_keyword"
    c_hasNondetKeyword :: Ptr Word8 -> CSize -> IO CInt

executeSqliteRun ::
    Int -> ByteString -> IO (Either SqliteError (Maybe SqliteRun))
executeSqliteRun micros sql = do
    checkStatus <- BSU.unsafeUseAsCStringLen sql $ \(ptr, len) ->
        c_checkSql (castPtr ptr) (fromIntegral len)
    case checkStatus of
        1 -> pure (Left (SqliteExecutionError "SQL contains a NUL byte"))
        2 ->
            pure $ case TE.decodeUtf8' sql of
                Left err -> Left (SqliteInfrastructureError (T.pack (show err)))
                Right _ -> Left (SqliteInfrastructureError "Invalid UTF-8")
        _ -> do
            (retryFresh, outcome) <- withPooledSqliteConn $ \conn -> do
                (poolable, counterPolluted, outcome) <- runCandidateOnConn micros sql conn
                pure (poolable, (counterPolluted, outcome))
            if retryFresh
                then bracket createSqliteConn destroySqliteConn $ \freshConn -> do
                    (_, _, outcomeFresh) <- runCandidateOnConn micros sql freshConn
                    pure outcomeFresh
                else pure outcome

-- SQLite supplies the unconsumed tail, including for trigger bodies and
-- multiple statements on one line. Zero-column statements run in C before
-- returning each result-bearing statement to Haskell.
runStatements ::
    Direct.Database ->
    Ptr CInt ->
    Int ->
    CString ->
    CString ->
    CString ->
    Int ->
    [(Int, ResultSet)] ->
    IO (Int, [(Int, ResultSet)], Maybe [Int])
runStatements db@(Direct.Database handle) offsetsPtr maxOffsets base end start0 ordinal0 revResults0 =
    alloca $ \outTail ->
        alloca $ \outStmt ->
            alloca $ \outOrdinal ->
                let finish !total revResults = do
                        offsets <-
                            if total <= maxOffsets
                                then Just . map fromIntegral <$> peekArray total offsetsPtr
                                else pure Nothing
                        pure (total, revResults, offsets)
                    go !start !ordinal revResults
                        | start >= end = finish ordinal revResults
                        | otherwise = do
                            rc <-
                                c_occamSqliteNextResult
                                    handle
                                    base
                                    start
                                    end
                                    (fromIntegral ordinal)
                                    offsetsPtr
                                    (fromIntegral maxOffsets)
                                    outTail
                                    outStmt
                                    outOrdinal
                            tailPtr <- peek outTail
                            stmtPtr <- peek outStmt
                            nextOrd <- fromIntegral <$> peek outOrdinal
                            case rc of
                                0 -> finish nextOrd revResults
                                1 -> do
                                    let stmt = Direct.Statement stmtPtr
                                    rs <-
                                        ( do
                                            count <- columnCount stmt
                                            names <- forM [0 .. count - 1] $ \i -> do
                                                name <- columnName stmt i
                                                maybe (fail "SQLite omitted a result column name") pure name
                                            rows <- stepAll stmt
                                            pure (ResultSet names [] rows)
                                        )
                                            `onException` c_sqlite3_finalize stmtPtr
                                    finalize stmt
                                    go tailPtr (nextOrd + 1) ((nextOrd, rs) : revResults)
                                -1 -> do
                                    code <- Direct.errcode db
                                    Direct.Utf8 message <- Direct.errmsg db
                                    throwIO (SQLError code (TE.decodeUtf8 message) "prepare")
                                _ -> do
                                    code <- Direct.errcode db
                                    Direct.Utf8 message <- Direct.errmsg db
                                    when (stmtPtr /= nullPtr) $ do
                                        _ <- c_sqlite3_finalize stmtPtr
                                        pure ()
                                    throwIO (SQLError code (TE.decodeUtf8 message) "step")
                 in go start0 ordinal0 revResults0

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
generateSqliteContractWithTrace micros sql = do
    res <- executeSqliteRun micros sql
    pure $ case res of
        Left SqliteTimeout -> Left "timeout"
        Left (SqliteExecutionError err) -> Left err
        Left (SqliteInfrastructureError err) -> Left err
        Right Nothing -> Left "Input must produce a result set"
        Right (Just run) ->
            let result = srLastResult run
             in Right
                    ( SqliteContract
                        { contractExpectedRows = resultRows result
                        , contractExpectedColumns = resultColumns result
                        , contractStateSha256 = Nothing
                        , contractCheckState = False
                        }
                    , buildSqliteTrace result run
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
    fst <$> sqliteContractObservationWithTrace micros contract candidate

sqliteContractObservationWithResultStatement ::
    Int -> SqliteContract -> ByteString -> IO (OracleObservation, Maybe ResultStatement)
sqliteContractObservationWithResultStatement micros contract candidate =
    second (fmap traceResultStatement) <$> sqliteContractObservationWithTrace micros contract candidate

sqliteContractObservationWithTrace ::
    Int -> SqliteContract -> ByteString -> IO (OracleObservation, Maybe ResultTrace)
sqliteContractObservationWithTrace micros contract candidate
    -- TODO: Define a reproducible database-state observation before accepting state contracts.
    | contractCheckState contract || isJust (contractStateSha256 contract) =
        pure (OracleIOFailed "SQLite state contracts are unsupported", Nothing)
    | otherwise = do
        res <- executeSqliteRun micros candidate
        pure $ case res of
            Left SqliteTimeout -> (OracleTimedOut, Nothing)
            Left (SqliteExecutionError _) -> (OracleFinished (ExitFailure 1), Nothing)
            Left (SqliteInfrastructureError err) -> (OracleIOFailed err, Nothing)
            Right observed
                | Just run <- observed
                , srLastResult run == expected ->
                    (OracleFinished ExitSuccess, Just (buildSqliteTrace expected run))
                | otherwise -> (OracleFinished (ExitFailure 1), Nothing)
    where
        expected = ResultSet (contractExpectedColumns contract) [] (contractExpectedRows contract)
