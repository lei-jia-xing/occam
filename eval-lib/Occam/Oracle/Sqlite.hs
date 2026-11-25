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
    prepareSqliteContractObserver,
    prepareSqliteContractObserverWithTrace,
    prepareFreshSqliteContractObserver,
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
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT (..), evalContT)
import Data.Aeson (
    FromJSON (..),
    Result (..),
    ToJSON (..),
    Value (..),
    eitherDecodeStrict',
    fromJSON,
    object,
    withObject,
    (.!=),
    (.:),
    (.:?),
    (.=),
 )
import Data.Aeson.KeyMap qualified as KM
import Data.Bifunctor (second)
import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.Char (isSpace)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.IntSet qualified as IntSet
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64, Word8)
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
import GHC.Clock (getMonotonicTimeNSec)
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

data SqliteCache

data SqliteConn = SqliteConn
    { scDb :: !Direct.Database
    , scTemplate :: !Direct.Database
    , scDirtyPtr :: !(Ptr CInt)
    , scCachePtr :: !(Ptr SqliteCache)
    , scCheckpointContract :: !(IORef (Maybe ByteString))
    , scDmlTainted :: !(IORef Bool)
    }

dirtyPragma, dirtyTemp, dirtyDml, dirtyConnCounter, dirtyNondet :: CInt
dirtyPragma = 2
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

foreign import ccall unsafe "occam_sqlite_cache_create"
    c_occamSqliteCacheCreate :: IO (Ptr SqliteCache)

foreign import ccall unsafe "occam_sqlite_cache_destroy"
    c_occamSqliteCacheDestroy :: Ptr SqliteCache -> IO ()

foreign import ccall safe "occam_sqlite_cache_clear_ckpts"
    c_occamSqliteCacheClearCkpts :: Ptr SqliteCache -> IO ()

foreign import ccall safe "occam_sqlite_run_and_match"
    c_occamSqliteRunAndMatch ::
        Ptr CDatabase ->
        Ptr CInt ->
        Ptr SqliteCache ->
        Ptr Word8 ->
        CSize ->
        Ptr Word8 ->
        CSize ->
        Int64 ->
        CInt ->
        Ptr CInt ->
        Ptr CInt ->
        Ptr CInt ->
        Ptr CInt ->
        Ptr Word8 ->
        CInt ->
        Ptr CInt ->
        Ptr CInt ->
        IO CInt

foreign import ccall safe "occam_sqlite_build_ckpts"
    c_occamSqliteBuildCkpts ::
        Ptr CDatabase ->
        Ptr CDatabase ->
        Ptr CInt ->
        Ptr SqliteCache ->
        Ptr Word8 ->
        CSize ->
        Ptr Word8 ->
        CSize ->
        CInt ->
        Int64 ->
        IO CInt

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
    cachePtr <- c_occamSqliteCacheCreate `onException` (free dirtyPtr >> close tmpl >> close db)
    let cleanup = do
            when (cachePtr /= nullPtr) (c_occamSqliteCacheDestroy cachePtr)
            free dirtyPtr
            close tmpl
            close db
    when (cachePtr == nullPtr) (cleanup >> fail "Could not allocate SQLite checkpoint cache")
    poke dirtyPtr 0
    -- Keep the engine baseline aligned with SQLite parser session defaults.
    exec db "PRAGMA foreign_keys = OFF;" `onException` cleanup
    dmlTainted <- newIORef False
    checkpointContract <- newIORef Nothing
    let Direct.Database dbPtr = db
    status <- setAuthorizer dbPtr sharedAuthorizer (castPtr dirtyPtr)
    if status /= 0
        then cleanup >> fail "Could not install SQLite authorizer"
        else
            pure
                SqliteConn
                    { scDb = db
                    , scTemplate = tmpl
                    , scDirtyPtr = dirtyPtr
                    , scCachePtr = cachePtr
                    , scCheckpointContract = checkpointContract
                    , scDmlTainted = dmlTainted
                    }

destroySqliteConn :: SqliteConn -> IO ()
destroySqliteConn SqliteConn {..} = do
    let Direct.Database dbPtr = scDb
    _ <- setAuthorizer dbPtr nullFunPtr nullPtr
    c_occamSqliteCacheDestroy scCachePtr
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

{- | All phases of one observation share a monotonic deadline. The remaining
time is passed to the native calls, which also check at statement boundaries.
-}
withSqliteDeadline :: Int -> a -> (IO Int -> IO a) -> IO a
withSqliteDeadline micros expired action
    | micros <= 0 = pure expired
    | otherwise = do
        start <- getMonotonicTimeNSec
        let remaining = do
                now <- getMonotonicTimeNSec
                let elapsed = (now - start) `div` 1000
                pure (if elapsed >= fromIntegral micros then 0 else micros - fromIntegral elapsed)
        result <- action remaining
        left <- remaining
        pure (if left == 0 then expired else result)

buildSqliteCheckpoints :: IO Int -> ByteString -> ByteString -> Int -> SqliteConn -> IO Bool
buildSqliteCheckpoints remaining expectedBytes sql count conn = do
    current <- readIORef (scCheckpointContract conn)
    unless (current == Just expectedBytes) $ do
        c_occamSqliteCacheClearCkpts (scCachePtr conn)
        writeIORef (scCheckpointContract conn) (Just expectedBytes)
    let Direct.Database dbPtr = scDb conn
        Direct.Database tmplPtr = scTemplate conn
    -- Replay can change connection-local counters even after restoring the
    -- database contents. A later counter query must retry on a fresh connection.
    writeIORef (scDmlTainted conn) True
    interruptibly (scDb conn) $
        BSU.unsafeUseAsCStringLen sql $ \(sqlPtr, sqlLen) ->
            BSU.unsafeUseAsCStringLen expectedBytes $ \(expPtr, expLen) -> do
                -- Leave time for cleanup even if optional replay uses its budget.
                budget <- (`div` 2) <$> remaining
                (/= 0)
                    <$> c_occamSqliteBuildCkpts
                        dbPtr
                        tmplPtr
                        (scDirtyPtr conn)
                        (scCachePtr conn)
                        (castPtr sqlPtr)
                        (fromIntegral sqlLen)
                        (castPtr expPtr)
                        (fromIntegral expLen)
                        (fromIntegral count)
                        (fromIntegral budget)

data SqliteRun = SqliteRun
    { srLastResult :: !ResultSet
    , srLastIndex :: !Int
    , srResults :: ![(Int, ResultSet)]
    , srStatementCount :: !Int
    , srAuthorizerDeterministic :: !Bool
    , srEndOffsets :: !(Maybe [Int])
    }

buildSqliteTrace :: ByteString -> ResultSet -> SqliteRun -> IO ResultTrace
buildSqliteTrace sql expected SqliteRun {..} = do
    deterministic <-
        if srAuthorizerDeterministic
            then BSU.unsafeUseAsCStringLen sql $ \(ptr, len) ->
                (== 0) <$> c_hasNondetKeyword (castPtr ptr) (fromIntegral len)
            else pure False
    pure
        ResultTrace
            { traceStatementCount = srStatementCount
            , traceFirstResultIndex = maybe srLastIndex fst (listHead srResults)
            , traceLastResultIndex = srLastIndex
            , traceResultIndices = IntSet.fromDistinctAscList (map fst srResults)
            , tracePrefixMatchIndices = IntSet.fromDistinctAscList (prefixMatches 0 False srResults)
            , traceDeterministic = deterministic
            , traceEndByteOffsets = srEndOffsets
            }
    where
        listHead [] = Nothing
        listHead (x : _) = Just x
        prefixMatches !k !matches pending
            | k >= srStatementCount = []
            | otherwise =
                case pending of
                    (ord, rs) : rest | ord == k -> emit (rs == expected) rest
                    _ -> emit matches pending
            where
                emit matched results
                    | matched = k : prefixMatches (k + 1) matched results
                    | otherwise = prefixMatches (k + 1) matched results

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
                evalContT do
                    start <- ContT $ BS.useAsCString sql
                    offsetsPtr <- ContT $ allocaArray maxOffsets
                    liftIO $ runStatements db offsetsPtr maxOffsets start (start `plusPtr` BS.length sql) start 0 []
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
                (lastIdx, lastRs) : _ ->
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
                                    , srAuthorizerDeterministic = authorizerDet
                                    , srEndOffsets = offsets
                                    }
                        )

foreign import ccall unsafe "occam_sqlite_check_sql"
    c_checkSql :: Ptr Word8 -> CSize -> IO CInt

foreign import ccall unsafe "occam_sqlite_has_nondet_keyword"
    c_hasNondetKeyword :: Ptr Word8 -> CSize -> IO CInt

executeSqliteRun ::
    Int -> ByteString -> IO (Either SqliteError (Maybe SqliteRun))
executeSqliteRun micros sql =
    withSqliteDeadline micros (Left SqliteTimeout) $ \remaining ->
        executeSqliteRunWithin False remaining sql

executeSqliteRunWithin ::
    Bool -> IO Int -> ByteString -> IO (Either SqliteError (Maybe SqliteRun))
executeSqliteRunWithin fresh remaining sql = do
    checkStatus <- BSU.unsafeUseAsCStringLen sql $ \(ptr, len) ->
        c_checkSql (castPtr ptr) (fromIntegral len)
    case checkStatus of
        1 -> pure (Left (SqliteExecutionError "SQL contains a NUL byte"))
        2 ->
            pure $ case TE.decodeUtf8' sql of
                Left err -> Left (SqliteInfrastructureError (T.pack (show err)))
                Right _ -> Left (SqliteInfrastructureError "Invalid UTF-8")
        _ | fresh -> runFresh
        _ -> do
            (retryFresh, outcome) <- withPooledSqliteConn $ \conn -> do
                micros <- remaining
                (poolable, counterPolluted, outcome) <- runCandidateOnConn micros sql conn
                pure (poolable, (counterPolluted, outcome))
            if retryFresh
                then runFresh
                else pure outcome
    where
        runFresh = bracket createSqliteConn destroySqliteConn $ \conn -> do
            micros <- remaining
            (_, _, outcome) <- runCandidateOnConn micros sql conn
            pure outcome

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
    evalContT do
        outTail <- ContT alloca
        outStmt <- ContT alloca
        outOrdinal <- ContT alloca
        liftIO $
            let finish !total revResults = do
                    offsets <-
                        if total <= maxOffsets
                            then Just . map fromIntegral <$> peekArray total offsetsPtr
                            else pure Nothing
                    pure (total, revResults, offsets)
                go !start !ordinal revResults
                    | start >= end = finish ordinal revResults
                    -- C may return a live statement even when stepping failed.
                    -- Keep ownership transfer and finalization masked.
                    | otherwise = mask $ \restore -> do
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
                            0 -> restore (finish nextOrd revResults)
                            1 -> do
                                let stmt = Direct.Statement stmtPtr
                                rs <-
                                    restore
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
                                restore (go tailPtr (nextOrd + 1) ((nextOrd, rs) : revResults))
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
                    case encodeSqliteExpected result of
                        Nothing -> pure ()
                        Just expBytes ->
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

-- | Encode a SQLite ResultSet into the compact binary format for C-side matching.
encodeSqliteExpected :: ResultSet -> Maybe ByteString
encodeSqliteExpected (ResultSet cols _ rows) = do
    let colCount = length cols
    rowBuilders <- mapM (encodeRow colCount) rows
    let header =
            BB.word32LE (fromIntegral colCount)
                <> BB.word32LE (fromIntegral (length rows))
                <> mconcat (map encodeColName cols)
    pure (BL.toStrict (BB.toLazyByteString (header <> mconcat rowBuilders)))
    where
        encodeColName name =
            let bs = TE.encodeUtf8 name
             in BB.word32LE (fromIntegral (BS.length bs)) <> BB.byteString bs
        encodeRow expectedCols row
            | length row /= expectedCols = Nothing
            | otherwise = mconcat <$> mapM encodeVal row
        encodeVal Null = Just (BB.word8 5)
        encodeVal (String t) =
            let bs = TE.encodeUtf8 t
             in Just (BB.word8 3 <> BB.word32LE (fromIntegral (BS.length bs)) <> BB.byteString bs)
        encodeVal v@(Number _) = case fromJSON v of
            Success (n :: Int64) -> Just (BB.word8 1 <> BB.int64LE n)
            Error _ -> Nothing
        encodeVal (Object o)
            | KM.size o == 1
            , Just fv <- KM.lookup "float64_bits" o = case fromJSON fv of
                Success (bits :: Word64) -> Just (BB.word8 2 <> BB.word64LE bits)
                Error _ -> Nothing
            | KM.size o == 1
            , Just bv <- KM.lookup "blob" o = case fromJSON bv of
                Success (bytes :: [Word8]) ->
                    let bs = BS.pack bytes
                     in Just (BB.word8 4 <> BB.word32LE (fromIntegral (BS.length bs)) <> BB.byteString bs)
                Error _ -> Nothing
            | otherwise = Nothing
        encodeVal _ = Nothing

matchCandidateOnConn ::
    IO Int ->
    ByteString ->
    ByteString ->
    Bool ->
    Bool ->
    (ByteString -> IO ()) ->
    SqliteConn ->
    IO (Bool, Bool, (OracleObservation, Maybe ResultTrace))
matchCandidateOnConn remaining expectedBytes sql allowCkpt needTrace onFailingPrefix conn = do
    poke (scDirtyPtr conn) 0
    tainted <- readIORef (scDmlTainted conn)
    checkpointContract <- readIORef (scCheckpointContract conn)
    let db = scDb conn
        Direct.Database dbPtr = db
        useCheckpoint = allowCkpt && checkpointContract == Just expectedBytes
        maxOffsets = if needTrace then max 64 (BS.count 59 sql + 16) else 0
        allocOffsets = if needTrace then allocaArray maxOffsets else ($ nullPtr)
        allocFlags = if needTrace then allocaArray maxOffsets else ($ nullPtr)
    res <-
        try $
            interruptibly db $
                evalContT do
                    (sqlPtr, sqlLen) <- ContT $ BSU.unsafeUseAsCStringLen sql
                    (expPtr, expLen) <- ContT $ BSU.unsafeUseAsCStringLen expectedBytes
                    stmtCountPtr <- ContT alloca
                    firstResPtr <- ContT alloca
                    lastResPtr <- ContT alloca
                    offsetsPtr <- ContT allocOffsets
                    flagsPtr <- ContT allocFlags
                    usedCkptDmlPtr <- ContT alloca
                    failEndPtr <- ContT alloca
                    liftIO do
                        micros <- remaining
                        status <-
                            c_occamSqliteRunAndMatch
                                dbPtr
                                (scDirtyPtr conn)
                                (scCachePtr conn)
                                (castPtr sqlPtr)
                                (fromIntegral sqlLen)
                                (castPtr expPtr)
                                (fromIntegral expLen)
                                (fromIntegral micros)
                                (if useCheckpoint then 1 else 0)
                                stmtCountPtr
                                firstResPtr
                                lastResPtr
                                offsetsPtr
                                flagsPtr
                                (fromIntegral maxOffsets)
                                usedCkptDmlPtr
                                failEndPtr
                        stmtCount <- fromIntegral <$> peek stmtCountPtr
                        firstRes <- fromIntegral <$> peek firstResPtr
                        lastRes <- fromIntegral <$> peek lastResPtr
                        usedCkptDml <- (/= 0) <$> peek usedCkptDmlPtr
                        failEnd <- fromIntegral <$> peek failEndPtr
                        if status == 0 && needTrace
                            then do
                                (offsets, flags) <-
                                    if stmtCount <= maxOffsets
                                        then do
                                            rawOffsets <- peekArray stmtCount offsetsPtr
                                            rawFlags <- peekArray stmtCount flagsPtr
                                            pure (Just (map fromIntegral rawOffsets), rawFlags)
                                        else pure (Nothing, [])
                                pure (status, stmtCount, firstRes, lastRes, offsets, flags, usedCkptDml, failEnd)
                            else pure (status, stmtCount, firstRes, lastRes, Nothing, [], usedCkptDml, failEnd)
    dirty <- peek (scDirtyPtr conn)
    when ((dirty .&. dirtyDml) /= 0) $
        writeIORef (scDmlTainted conn) True
    let poolable = (dirty .&. dirtyTemp) == 0
        authorizerDet = (dirty .&. dirtyNondet) == 0
    case res of
        Left (err :: SomeException) ->
            pure (False, False, (OracleIOFailed (T.pack (displayException err)), Nothing))
        Right (status, stmtCount, firstRes, lastRes, offsets, flags, usedCkptDml, failEnd) ->
            let counterPolluted = (tainted || usedCkptDml) && (dirty .&. dirtyConnCounter) /= 0
             in case status of
                    0 -> do
                        deterministic <-
                            if authorizerDet
                                then BSU.unsafeUseAsCStringLen sql $ \(ptr, len) ->
                                    (== 0) <$> c_hasNondetKeyword (castPtr ptr) (fromIntegral len)
                                else pure False
                        let trace =
                                if needTrace
                                    then
                                        let (resIdxList, prefMatchList) = decodeResultFlags flags
                                         in Just
                                                ResultTrace
                                                    { traceStatementCount = stmtCount
                                                    , traceFirstResultIndex = if firstRes >= 0 then firstRes else lastRes
                                                    , traceLastResultIndex = lastRes
                                                    , traceResultIndices = IntSet.fromDistinctAscList resIdxList
                                                    , tracePrefixMatchIndices = IntSet.fromDistinctAscList prefMatchList
                                                    , traceDeterministic = deterministic
                                                    , traceEndByteOffsets = offsets
                                                    }
                                    else Nothing
                        keepConn <-
                            if allowCkpt
                                && not counterPolluted
                                && poolable
                                && deterministic
                                && stmtCount >= 8
                                && (dirty .&. (dirtyTemp .|. dirtyPragma .|. dirtyConnCounter .|. dirtyNondet)) == 0
                                then do
                                    resetOk <- resetSqliteConn conn
                                    if resetOk
                                        then buildSqliteCheckpoints remaining expectedBytes sql stmtCount conn
                                        else pure False
                                else pure poolable
                        pure (keepConn, counterPolluted, (OracleFinished ExitSuccess, trace))
                    1 -> pure (poolable, counterPolluted, (OracleFinished (ExitFailure 1), Nothing))
                    2 -> do
                        when
                            ( not counterPolluted
                                && failEnd > 0
                                && failEnd <= BS.length sql
                                && (dirty .&. (dirtyTemp .|. dirtyNondet)) == 0
                            )
                            $ do
                                let prefix = BS.take failEnd sql
                                isDetPrefix <- BSU.unsafeUseAsCStringLen prefix $ \(ptr, len) ->
                                    (== 0) <$> c_hasNondetKeyword (castPtr ptr) (fromIntegral len)
                                when isDetPrefix (onFailingPrefix prefix)
                        pure (poolable, counterPolluted, (OracleFinished (ExitFailure 1), Nothing))
                    3 -> pure (False, False, (OracleTimedOut, Nothing))
                    _ -> pure (False, False, (OracleIOFailed "SQLite execution infrastructure error", Nothing))
    where
        decodeResultFlags = go 0 False [] []
            where
                go _ _ resAcc prefAcc [] = (reverse resAcc, reverse prefAcc)
                go !k !matches resAcc prefAcc (f : fs)
                    | (f .&. 1) /= 0 =
                        let !m = (f .&. 2) /= 0
                         in go (k + 1) m (k : resAcc) (if m then k : prefAcc else prefAcc) fs
                    | matches = go (k + 1) True resAcc (k : prefAcc) fs
                    | otherwise = go (k + 1) False resAcc prefAcc fs

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
    | Just expectedBytes <- encodeSqliteExpected expected = \candidate ->
        withSqliteDeadline micros (OracleTimedOut, Nothing) $ \remaining -> do
            let runFresh = bracket createSqliteConn destroySqliteConn $ \conn -> do
                    (_, _, outcome) <-
                        matchCandidateOnConn remaining expectedBytes candidate False needTrace onFailingPrefix conn
                    pure outcome
            checkStatus <- BSU.unsafeUseAsCStringLen candidate $ \(ptr, len) ->
                c_checkSql (castPtr ptr) (fromIntegral len)
            case checkStatus of
                1 -> pure (OracleFinished (ExitFailure 1), Nothing)
                2 ->
                    pure $ case TE.decodeUtf8' candidate of
                        Left err -> (OracleIOFailed (T.pack (show err)), Nothing)
                        Right _ -> (OracleIOFailed "Invalid UTF-8", Nothing)
                _ | fresh -> runFresh
                _ -> do
                    (retryFresh, outcome) <- withPooledSqliteConn $ \conn -> do
                        (keepConn, counterPolluted, res) <-
                            matchCandidateOnConn remaining expectedBytes candidate True needTrace onFailingPrefix conn
                        pure (keepConn, (counterPolluted, res))
                    if retryFresh then runFresh else pure outcome
    | otherwise = \candidate ->
        withSqliteDeadline micros (OracleTimedOut, Nothing) $ \remaining ->
            sqliteContractObservationFallback needTrace remaining fresh expected candidate
    where
        expected = ResultSet (contractExpectedColumns contract) [] (contractExpectedRows contract)

sqliteContractObservationFallback ::
    Bool -> IO Int -> Bool -> ResultSet -> ByteString -> IO (OracleObservation, Maybe ResultTrace)
sqliteContractObservationFallback needTrace remaining fresh expected candidate = do
    res <- executeSqliteRunWithin fresh remaining candidate
    case res of
        Left SqliteTimeout -> pure (OracleTimedOut, Nothing)
        Left (SqliteExecutionError _) -> pure (OracleFinished (ExitFailure 1), Nothing)
        Left (SqliteInfrastructureError err) -> pure (OracleIOFailed err, Nothing)
        Right observed
            | Just run <- observed
            , srLastResult run == expected -> do
                trace <- if needTrace then Just <$> buildSqliteTrace candidate expected run else pure Nothing
                pure (OracleFinished ExitSuccess, trace)
            | otherwise -> pure (OracleFinished (ExitFailure 1), Nothing)

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
