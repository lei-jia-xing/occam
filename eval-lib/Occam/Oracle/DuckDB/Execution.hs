{-# LANGUAGE OverloadedStrings #-}

module Occam.Oracle.DuckDB.Execution (
    ExecutionResult,
    ObserveResult,
    StatementFailure,
    executeDuckDBResult,
    executeDuckDBResultWith,
    executeDuckDBResultWithReplay,
    executeWithAPI,
    ignoreStatementFailure,
) where

import Control.Concurrent (
    forkIO,
    forkIOWithUnmask,
    killThread,
    newEmptyMVar,
    putMVar,
    readMVar,
    threadDelay,
    tryReadMVar,
 )
import Control.Exception qualified as Exception
import Control.Exception.Safe (SomeException, bracket, displayException, try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT (..), evalContT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.IntSet qualified as IntSet
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes, callocBytes, free)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import Occam.Core (ResultTrace (..))
import Occam.Oracle.DuckDB.FFI (
    DuckDBAPI (..),
    DuckDBError (..),
    DuckDBResult,
    DuckdbConnection,
    DuckdbDatabase,
    DuckdbExtractedStatements,
    DuckdbPreparedStatement,
    IdxT,
    closeDatabase,
    duckdbErrorText,
    getDuckDBAPI,
    openFreshDatabase,
    resultKind,
    resultSize,
 )
import Occam.Oracle.DuckDB.Result (observeRows)
import Occam.Oracle.Result (ResultSet (..))
import System.Timeout (timeout)

type ObserveResult a = DuckDBAPI -> Ptr DuckDBResult -> IO (Either DuckDBError a)

type ExecutionResult a = Either DuckDBError (Maybe (a, ResultTrace))

-- The callback receives the complete native statement count, the failing
-- ordinal and a native extraction check for a proposed prefix boundary.
type StatementFailure = Int -> Int -> (ByteString -> IO Bool) -> Text -> IO ()

ignoreStatementFailure :: StatementFailure
ignoreStatementFailure _ _ _ _ = pure ()

executeDuckDBResult ::
    Int -> ByteString -> IO (Either DuckDBError (Maybe (ResultSet, ResultTrace)))
executeDuckDBResult = executeDuckDBResultWith observeRows

executeDuckDBResultWith :: ObserveResult a -> Int -> ByteString -> IO (ExecutionResult a)
executeDuckDBResultWith observe =
    executeDuckDBResultWithReplay observe Nothing ignoreStatementFailure

executeDuckDBResultWithReplay ::
    ObserveResult a ->
    Maybe (Bool, DuckdbDatabase) ->
    StatementFailure ->
    Int ->
    ByteString ->
    IO (ExecutionResult a)
executeDuckDBResultWithReplay observe db onFailure micros sql
    | BS.elem 0 sql = pure (Left (DuckDBExecutionError "SQL contains a NUL byte"))
    | otherwise = do
        mApi <- getDuckDBAPI
        case mApi of
            Left err -> pure (Left err)
            Right api -> executeWithAPI api observe db onFailure micros sql

executeWithAPI ::
    DuckDBAPI ->
    ObserveResult a ->
    Maybe (Bool, DuckdbDatabase) ->
    StatementFailure ->
    Int ->
    ByteString ->
    IO (ExecutionResult a)
executeWithAPI api observe existing onFailure micros sql = do
    res <- try $ case existing of
        Just (configureDb, db) -> runOnDb configureDb db
        Nothing -> bracket (openFreshDatabase api) (closeDatabase api) (runOnDb True)
    pure $ case res of
        Left (err :: SomeException) -> Left (DuckDBInfrastructureError (T.pack (displayException err)))
        Right inner -> inner
    where
        connectDb db conPtr = do
            st <- apiConnect api db conPtr
            if st /= 0
                then fail "Failed to connect to in-memory DuckDB database"
                else peek conPtr
        runOnDb configureDb db = evalContT do
            conPtr <- ContT alloca
            con <- ContT $ bracket (connectDb db conPtr) (\_ -> apiDisconnect api conPtr)
            liftIO do
                mRes <- executeInterruptibly micros (apiInterrupt api con) do
                    let needsConfigure = configureDb && isNothing (apiConfigOpen api)
                    when needsConfigure (configureDuckDBConnection api con)
                    runDuckDBStatements api observe onFailure con sql
                pure (fromMaybe (Left DuckDBTimeout) mRes)

withExtractedStatements ::
    DuckDBAPI ->
    DuckdbConnection ->
    ByteString ->
    (IdxT -> DuckdbExtractedStatements -> CString -> IO b) ->
    IO b
withExtractedStatements api con bytes use = evalContT do
    sqlCStr <- ContT $ BSC.useAsCString bytes
    extPtr <- ContT alloca
    (n, ext) <- ContT $ bracket (extractOnce sqlCStr extPtr) (\_ -> apiDestroyExtracted api extPtr)
    liftIO do
        cErr <- apiExtractStatementsError api ext
        use n ext cErr
    where
        extractOnce sqlCStr extPtr = do
            num <- apiExtractStatements api con sqlCStr extPtr
            extracted <- peek extPtr
            pure (num, extracted)

runDuckDBStatements ::
    DuckDBAPI ->
    ObserveResult a ->
    StatementFailure ->
    DuckdbConnection ->
    ByteString ->
    IO (ExecutionResult a)
runDuckDBStatements api observe onFailure con sql =
    withExtractedStatements api con sql $ \n ext cErr ->
        if cErr /= nullPtr
            then Left . DuckDBExecutionError . T.pack <$> peekCString cErr
            else case n of
                0 -> pure (Right Nothing)
                1 -> executeSingleExtracted api observe onFailure con ext
                _ -> executeManyExtracted api observe onFailure con ext n

-- NOTE: In-memory connections can otherwise read or modify external files.
configureDuckDBConnection :: DuckDBAPI -> DuckdbConnection -> IO ()
configureDuckDBConnection api con = evalContT do
    settings <- ContT $ withCString "SET enable_external_access = false; SET lock_configuration = true;"
    status <- ContT $ \use -> withDuckDBResult api (apiQuery api con settings) (\s _ -> use s)
    liftIO $ when (status /= 0) (fail "Could not disable and lock DuckDB external access")

withDuckDBResult ::
    DuckDBAPI -> (Ptr DuckDBResult -> IO CInt) -> (CInt -> Ptr DuckDBResult -> IO a) -> IO a
withDuckDBResult api execute use = do
    size <- resultSize
    evalContT do
        ptr <- ContT $ allocaBytes (fromIntegral size)
        status <- ContT $ bracket (execute ptr) (const (apiDestroyResult api ptr))
        liftIO $ use status ptr

prepareAndExecuteStatement ::
    DuckDBAPI ->
    DuckdbConnection ->
    DuckdbExtractedStatements ->
    IdxT ->
    Ptr DuckdbPreparedStatement ->
    Ptr DuckDBResult ->
    IO (Either DuckDBError CInt)
prepareAndExecuteStatement api con ext idx prepPtr buf =
    bracket
        (apiPrepareExtractedStatement api con ext idx prepPtr)
        (const (apiDestroyPrepare api prepPtr))
        $ \status -> do
            prep <- peek prepPtr
            if status /= 0
                then Left . DuckDBExecutionError <$> duckdbErrorText "Prepare error" (apiPrepareError api prep)
                else do
                    execStatus <- apiExecutePrepared api prep buf
                    if execStatus /= 0
                        then Left . DuckDBExecutionError <$> duckdbErrorText "Execution error" (apiResultError api buf)
                        else Right <$> resultKind (apiResultKind api) buf

executeSingleExtracted ::
    DuckDBAPI ->
    ObserveResult a ->
    StatementFailure ->
    DuckdbConnection ->
    DuckdbExtractedStatements ->
    IO (ExecutionResult a)
executeSingleExtracted api observe onFailure con ext = evalContT do
    prepPtr <- ContT alloca
    size <- liftIO $ fromIntegral <$> resultSize
    current <- ContT $ bracket (callocBytes size) (\ptr -> apiDestroyResult api ptr >> free ptr)
    liftIO do
        executed <- prepareAndExecuteStatement api con ext 0 prepPtr current
        case executed of
            Left err@(DuckDBExecutionError message) -> do
                onFailure 1 0 (verifyStatementBoundary api con 1) message
                pure (Left err)
            Left err -> pure (Left err)
            Right 3 ->
                observeRetainedResult api observe current 1 (Just 0) (Just 0) (IntSet.singleton 0)
            Right _ -> pure (Right Nothing)

executeManyExtracted ::
    DuckDBAPI ->
    ObserveResult a ->
    StatementFailure ->
    DuckdbConnection ->
    DuckdbExtractedStatements ->
    IdxT ->
    IO (ExecutionResult a)
executeManyExtracted api observe onFailure con ext n = evalContT do
    prepPtr <- ContT alloca
    size <- liftIO $ fromIntegral <$> resultSize
    let withBuffer = bracket (callocBytes size) (\ptr -> apiDestroyResult api ptr >> free ptr)
        clear ptr = Exception.mask_ (apiDestroyResult api ptr >> fillBytes ptr 0 size)
    -- The buffers own the latest query result and the current execution.
    current0 <- ContT withBuffer
    retained0 <- ContT withBuffer
    liftIO $ loop clear prepPtr 0 current0 retained0 Nothing Nothing IntSet.empty
    where
        loop _ _ i _ retained firstResult lastResult resultIndices
            | i >= n = observeRetainedResult api observe retained n firstResult lastResult resultIndices
        loop clear prepPtr i current retained firstResult lastResult resultIndices = do
            executed <- prepareAndExecuteStatement api con ext i prepPtr current
            case executed of
                Left err@(DuckDBExecutionError message) -> do
                    onFailure (fromIntegral n) (fromIntegral i) (verifyStatementBoundary api con (i + 1)) message
                    pure (Left err)
                Left err -> pure (Left err)
                Right 3 -> do
                    -- DUCKDB_RESULT_TYPE_QUERY_RESULT
                    clear retained
                    loop
                        clear
                        prepPtr
                        (i + 1)
                        retained
                        current
                        (Just (fromMaybe i firstResult))
                        (Just i)
                        (IntSet.insert (fromIntegral i) resultIndices)
                Right _ -> do
                    clear current
                    loop clear prepPtr (i + 1) current retained firstResult lastResult resultIndices

verifyStatementBoundary :: DuckDBAPI -> DuckdbConnection -> IdxT -> ByteString -> IO Bool
verifyStatementBoundary api con expectedCount prefix =
    withExtractedStatements api con prefix $ \count _ errPtr ->
        pure (count == expectedCount && errPtr == nullPtr)

observeRetainedResult ::
    DuckDBAPI ->
    ObserveResult a ->
    Ptr DuckDBResult ->
    IdxT ->
    Maybe IdxT ->
    Maybe IdxT ->
    IntSet.IntSet ->
    IO (ExecutionResult a)
observeRetainedResult _ _ _ _ _ Nothing _ = pure (Right Nothing)
observeRetainedResult api observe retained n firstResult (Just index) resultIndices = do
    result <- observe api retained
    let trace =
            ResultTrace
                { traceStatementCount = fromIntegral n
                , traceFirstResultIndex = fromIntegral (fromMaybe index firstResult)
                , traceLastResultIndex = fromIntegral index
                , traceResultIndices = resultIndices
                , tracePrefixMatchIndices = IntSet.empty
                , traceDeterministic = False
                , traceEndByteOffsets = Nothing
                }
    pure (fmap (\rows -> Just (rows, trace)) result)

-- NOTE: Native interruption is cooperative. Keep the connection alive until
-- the worker leaves DuckDB, including when the shared budget cancels a probe.
executeInterruptibly :: Int -> IO () -> IO a -> IO (Maybe a)
executeInterruptibly micros interrupt work =
    bracket start terminate $ \(_, done) ->
        timeout micros (readMVar done >>= either Exception.throwIO pure)
    where
        start = do
            done <- newEmptyMVar
            worker <- forkIOWithUnmask $ \unmask ->
                Exception.try @SomeException (unmask work) >>= putMVar done
            pure (worker, done)
        terminate (worker, done) = Exception.uninterruptibleMask_ $ do
            pending <- tryReadMVar done
            case pending of
                Just _ -> pure ()
                Nothing -> do
                    _ <- forkIO (killThread worker)
                    drain done
        drain done = do
            interrupt
            threadDelay 1000
            result <- tryReadMVar done
            case result of
                Nothing -> drain done
                Just _ -> pure ()
