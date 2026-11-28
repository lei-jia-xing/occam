{-# LANGUAGE OverloadedStrings #-}

-- | SQLite result-value decoding and deterministic 'ResultSet' \/ 'ResultTrace' conversion.
module Occam.Oracle.Sqlite.Result (
    SqliteError (..),
    SqliteRun (..),
    buildSqliteTrace,
    encodeSqliteExpected,
    executeSqliteRun,
    executeSqliteRunWithin,
    runCandidateOnConn,
    sqlDataToValue,
) where

import Control.Exception (bracket, mask, onException)
import Control.Exception.Safe (
    SomeException,
    displayException,
    fromException,
    throwIO,
    try,
 )
import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT (..), evalContT)
import Data.Aeson (
    Result (..),
    Value (..),
    fromJSON,
    object,
    (.=),
 )
import Data.Aeson.KeyMap qualified as KM
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.IORef (readIORef, writeIORef)
import Data.Int (Int64)
import Data.IntSet qualified as IntSet
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64, Word8)
import Database.SQLite3 (
    SQLData (..),
    SQLError (..),
    Statement,
    StepResult (..),
    columnCount,
    columnName,
    columns,
    finalize,
    interruptibly,
    step,
 )
import Database.SQLite3.Bindings (
    CStatement,
    c_sqlite3_finalize,
 )
import Database.SQLite3.Direct qualified as Direct
import Foreign (
    Ptr,
    alloca,
    allocaArray,
    castPtr,
    nullPtr,
    peek,
    peekArray,
    plusPtr,
    poke,
 )
import Foreign.C.String (CString)
import Foreign.C.Types (CInt)
import GHC.Float (castDoubleToWord64)
import Occam.Core (ResultTrace (..))
import Occam.Oracle.Result (ResultSet (..))
import Occam.Oracle.Sqlite.FFI (
    SqliteConn (..),
    c_checkSql,
    c_hasNondetKeyword,
    c_occamSqliteNextResult,
    createSqliteConn,
    destroySqliteConn,
    dirtyConnCounter,
    dirtyDml,
    dirtyNondet,
    dirtyTemp,
    withPooledSqliteConn,
    withSqliteDeadline,
 )
import System.Timeout (timeout)

-- | Failure classes for in-process SQLite execution.
data SqliteError
    = SqliteTimeout
    | SqliteExecutionError !Text
    | SqliteInfrastructureError !Text
    deriving (Eq, Show)

-- | Raw result sequence, statement count, determinism flag, and end offsets from a SQLite run.
data SqliteRun = SqliteRun
    { srLastResult :: !ResultSet
    , srLastIndex :: !Int
    , srResults :: ![(Int, ResultSet)]
    , srStatementCount :: !Int
    , srAuthorizerDeterministic :: !Bool
    , srEndOffsets :: !(Maybe [Int])
    }

-- | Convert a native @direct-sqlite@ 'SQLData' value to the contract's canonical JSON representation.
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
                    vals <- map sqlDataToValue <$> columns stmt
                    go (vals : acc)
                Done -> pure (reverse acc)

classifySqliteException :: SomeException -> SqliteError
classifySqliteException err =
    case fromException err of
        Just sqlErr ->
            let details = sqlErrorDetails sqlErr
                context = sqlErrorContext sqlErr
                message = if T.null details then T.pack (show (sqlError sqlErr)) else details
             in SqliteExecutionError (if T.null context then message else context <> ": " <> message)
        Nothing -> SqliteInfrastructureError (T.pack (displayException err))

-- | Convert a completed 'SqliteRun' and SQL buffer into a 'ResultTrace' summary.
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

-- | Execute SQL on a pooled/reusable 'SqliteConn' under a microsecond timeout.
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
    res <-
        try $
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
    pure $ case res of
        Left err -> case classifySqliteException err of
            e@(SqliteExecutionError _) -> (poolable, counterPolluted, Left e)
            e -> (False, False, Left e)
        Right Nothing -> (False, False, Left SqliteTimeout)
        Right (Just (_, [], _)) -> (poolable, counterPolluted, Right Nothing)
        Right (Just (totalStmts, revResults@((lastIdx, lastRs) : _), offsets)) ->
            ( poolable
            , counterPolluted
            , Right . Just $
                SqliteRun
                    { srLastResult = lastRs
                    , srLastIndex = lastIdx
                    , srResults = reverse revResults
                    , srStatementCount = totalStmts
                    , srAuthorizerDeterministic = authorizerDet
                    , srEndOffsets = offsets
                    }
            )

-- | Execute SQL on a fresh/pooled in-memory SQLite instance under a microsecond timeout.
executeSqliteRun ::
    Int -> ByteString -> IO (Either SqliteError (Maybe SqliteRun))
executeSqliteRun micros sql =
    withSqliteDeadline micros (Left SqliteTimeout) $ \remaining ->
        executeSqliteRunWithin False remaining sql

-- | Execute SQL inside an existing deadline callback, optionally forcing a fresh connection.
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
runStatements db offsetsPtr maxOffsets base end start0 ordinal0 revResults0 =
    evalContT do
        outTail <- ContT alloca
        outStmt <- ContT alloca
        outOrdinal <- ContT alloca
        liftIO $
            stepSqliteStatements
                db
                offsetsPtr
                maxOffsets
                base
                end
                outTail
                outStmt
                outOrdinal
                start0
                ordinal0
                revResults0

stepSqliteStatements ::
    Direct.Database ->
    Ptr CInt ->
    Int ->
    CString ->
    CString ->
    Ptr CString ->
    Ptr (Ptr CStatement) ->
    Ptr CInt ->
    CString ->
    Int ->
    [(Int, ResultSet)] ->
    IO (Int, [(Int, ResultSet)], Maybe [Int])
stepSqliteStatements db@(Direct.Database handle) offsetsPtr maxOffsets base end outTail outStmt outOrdinal = go
    where
        finish !total revResults = do
            offsets <-
                if total <= maxOffsets
                    then Just . map fromIntegral <$> peekArray total offsetsPtr
                    else pure Nothing
            pure (total, revResults, offsets)

        -- C may return a live statement even when stepping failed.
        -- Keep ownership transfer and finalization masked.
        go !start !ordinal revResults
            | start >= end = finish ordinal revResults
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
                        rs <- restore (readSqliteResultSet stmt) `onException` c_sqlite3_finalize stmtPtr
                        finalize stmt
                        restore (go tailPtr (nextOrd + 1) ((nextOrd, rs) : revResults))
                    -1 -> throwSqliteError db "prepare"
                    _ -> do
                        when (stmtPtr /= nullPtr) $ do
                            _ <- c_sqlite3_finalize stmtPtr
                            pure ()
                        throwSqliteError db "step"

readSqliteResultSet :: Statement -> IO ResultSet
readSqliteResultSet stmt = do
    count <- columnCount stmt
    names <- forM [0 .. count - 1] $ \i -> do
        name <- columnName stmt i
        maybe (fail "SQLite omitted a result column name") pure name
    rows <- stepAll stmt
    pure (ResultSet names [] rows)

throwSqliteError :: Direct.Database -> Text -> IO a
throwSqliteError db phase = do
    code <- Direct.errcode db
    Direct.Utf8 message <- Direct.errmsg db
    throwIO (SQLError code (TE.decodeUtf8 message) phase)

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
