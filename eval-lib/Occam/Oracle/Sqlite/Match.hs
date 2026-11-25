{-# LANGUAGE OverloadedStrings #-}

module Occam.Oracle.Sqlite.Match (
    buildSqliteCheckpoints,
    prepareSqliteResultObserver,
) where

import Control.Exception (bracket)
import Control.Exception.Safe (SomeException, displayException, try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT (..), evalContT)
import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.IORef (readIORef, writeIORef)
import Data.IntSet qualified as IntSet
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import Database.SQLite3 (interruptibly)
import Database.SQLite3.Direct qualified as Direct
import Foreign (
    alloca,
    allocaArray,
    castPtr,
    nullPtr,
    peek,
    peekArray,
    poke,
 )
import Foreign.C.Types (CInt)
import Occam.Core (ResultTrace (..))
import Occam.Oracle (OracleObservation (..))
import Occam.Oracle.Result (ResultSet (..))
import Occam.Oracle.Sqlite.FFI (
    SqliteConn (..),
    c_checkSql,
    c_hasNondetKeyword,
    c_occamSqliteBuildCkpts,
    c_occamSqliteCacheClearCkpts,
    c_occamSqliteRunAndMatch,
    createSqliteConn,
    destroySqliteConn,
    dirtyConnCounter,
    dirtyDml,
    dirtyNondet,
    dirtyPragma,
    dirtyTemp,
    resetSqliteConn,
    withPooledSqliteConn,
    withSqliteDeadline,
 )
import Occam.Oracle.Sqlite.Result (
    SqliteError (..),
    SqliteRun (..),
    buildSqliteTrace,
    encodeSqliteExpected,
    executeSqliteRunWithin,
 )
import System.Exit (ExitCode (..))

data SqliteMatchCall = SqliteMatchCall
    { smcStatus :: !CInt
    , smcStmtCount :: !Int
    , smcFirstRes :: !Int
    , smcLastRes :: !Int
    , smcOffsets :: !(Maybe [Int])
    , smcFlags :: ![Word8]
    , smcUsedCkptDml :: !Bool
    , smcFailEnd :: !Int
    }

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

prepareSqliteResultObserver ::
    Bool ->
    Bool ->
    Int ->
    ResultSet ->
    (ByteString -> IO ()) ->
    ByteString ->
    IO (OracleObservation, Maybe ResultTrace)
prepareSqliteResultObserver fresh needTrace micros expected onFailingPrefix
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
    let useCheckpoint = allowCkpt && checkpointContract == Just expectedBytes
    res <- try $ invokeSqliteRunAndMatch remaining expectedBytes sql useCheckpoint needTrace conn
    dirty <- peek (scDirtyPtr conn)
    when ((dirty .&. dirtyDml) /= 0) $
        writeIORef (scDmlTainted conn) True
    let poolable = (dirty .&. dirtyTemp) == 0
        authorizerDet = (dirty .&. dirtyNondet) == 0
    case res of
        Left (err :: SomeException) ->
            pure (False, False, (OracleIOFailed (T.pack (displayException err)), Nothing))
        Right call ->
            let counterPolluted = (tainted || smcUsedCkptDml call) && (dirty .&. dirtyConnCounter) /= 0
             in handleSqliteMatchCall
                    remaining
                    expectedBytes
                    sql
                    allowCkpt
                    needTrace
                    onFailingPrefix
                    conn
                    dirty
                    poolable
                    authorizerDet
                    counterPolluted
                    call

invokeSqliteRunAndMatch ::
    IO Int -> ByteString -> ByteString -> Bool -> Bool -> SqliteConn -> IO SqliteMatchCall
invokeSqliteRunAndMatch remaining expectedBytes sql useCheckpoint needTrace conn =
    interruptibly db $ evalContT do
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
            (offsets, flags) <-
                if status == 0 && needTrace && stmtCount <= maxOffsets
                    then do
                        rawOffsets <- peekArray stmtCount offsetsPtr
                        rawFlags <- peekArray stmtCount flagsPtr
                        pure (Just (map fromIntegral rawOffsets), rawFlags)
                    else pure (Nothing, [])
            pure (SqliteMatchCall status stmtCount firstRes lastRes offsets flags usedCkptDml failEnd)
    where
        db@(Direct.Database dbPtr) = scDb conn
        maxOffsets = if needTrace then max 64 (BS.count 59 sql + 16) else 0
        allocOffsets = if needTrace then allocaArray maxOffsets else ($ nullPtr)
        allocFlags = if needTrace then allocaArray maxOffsets else ($ nullPtr)

handleSqliteMatchCall ::
    IO Int ->
    ByteString ->
    ByteString ->
    Bool ->
    Bool ->
    (ByteString -> IO ()) ->
    SqliteConn ->
    CInt ->
    Bool ->
    Bool ->
    Bool ->
    SqliteMatchCall ->
    IO (Bool, Bool, (OracleObservation, Maybe ResultTrace))
handleSqliteMatchCall remaining expectedBytes sql allowCkpt needTrace onFailingPrefix conn dirty poolable authorizerDet counterPolluted call =
    case smcStatus call of
        0 -> do
            deterministic <-
                if authorizerDet
                    then BSU.unsafeUseAsCStringLen sql $ \(ptr, len) ->
                        (== 0) <$> c_hasNondetKeyword (castPtr ptr) (fromIntegral len)
                    else pure False
            let trace = if needTrace then Just (buildSqliteMatchTrace call deterministic) else Nothing
                canCheckpoint =
                    allowCkpt
                        && not counterPolluted
                        && poolable
                        && deterministic
                        && smcStmtCount call >= 8
                        && (dirty .&. (dirtyTemp .|. dirtyPragma .|. dirtyConnCounter .|. dirtyNondet)) == 0
            keepConn <-
                if canCheckpoint
                    then rebuildSqliteCheckpoints remaining expectedBytes sql (smcStmtCount call) conn
                    else pure poolable
            pure (keepConn, counterPolluted, (OracleFinished ExitSuccess, trace))
        1 -> pure (poolable, counterPolluted, (OracleFinished (ExitFailure 1), Nothing))
        2 -> do
            recordSqliteFailingPrefix sql dirty counterPolluted (smcFailEnd call) onFailingPrefix
            pure (poolable, counterPolluted, (OracleFinished (ExitFailure 1), Nothing))
        3 -> pure (False, False, (OracleTimedOut, Nothing))
        _ -> pure (False, False, (OracleIOFailed "SQLite execution infrastructure error", Nothing))

rebuildSqliteCheckpoints :: IO Int -> ByteString -> ByteString -> Int -> SqliteConn -> IO Bool
rebuildSqliteCheckpoints remaining expectedBytes sql stmtCount conn = do
    resetOk <- resetSqliteConn conn
    if resetOk
        then buildSqliteCheckpoints remaining expectedBytes sql stmtCount conn
        else pure False

recordSqliteFailingPrefix :: ByteString -> CInt -> Bool -> Int -> (ByteString -> IO ()) -> IO ()
recordSqliteFailingPrefix sql dirty counterPolluted failEnd onFailingPrefix =
    when
        ( not counterPolluted
            && failEnd > 0
            && failEnd <= BS.length sql
            && (dirty .&. (dirtyTemp .|. dirtyNondet)) == 0
        )
        do
            let prefix = BS.take failEnd sql
            isDetPrefix <- BSU.unsafeUseAsCStringLen prefix $ \(ptr, len) ->
                (== 0) <$> c_hasNondetKeyword (castPtr ptr) (fromIntegral len)
            when isDetPrefix (onFailingPrefix prefix)

buildSqliteMatchTrace :: SqliteMatchCall -> Bool -> ResultTrace
buildSqliteMatchTrace call deterministic =
    ResultTrace
        { traceStatementCount = smcStmtCount call
        , traceFirstResultIndex = if smcFirstRes call >= 0 then smcFirstRes call else smcLastRes call
        , traceLastResultIndex = smcLastRes call
        , traceResultIndices = IntSet.fromDistinctAscList resIdxList
        , tracePrefixMatchIndices = IntSet.fromDistinctAscList prefMatchList
        , traceDeterministic = deterministic
        , traceEndByteOffsets = smcOffsets call
        }
    where
        (resIdxList, prefMatchList) = decodeResultFlags (smcFlags call)

decodeResultFlags :: [Word8] -> ([Int], [Int])
decodeResultFlags = go 0 False [] []
    where
        go _ _ resAcc prefAcc [] = (reverse resAcc, reverse prefAcc)
        go !k !matches resAcc prefAcc (f : fs)
            | (f .&. 1) /= 0 =
                let !m = (f .&. 2) /= 0
                 in go (k + 1) m (k : resAcc) (if m then k : prefAcc else prefAcc) fs
            | matches = go (k + 1) True resAcc (k : prefAcc) fs
            | otherwise = go (k + 1) False resAcc prefAcc fs

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
