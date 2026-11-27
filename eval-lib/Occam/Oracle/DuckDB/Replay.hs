{-# LANGUAGE OverloadedStrings #-}

module Occam.Oracle.DuckDB.Replay (
    prepareDuckDBReplayRunner,
) where

import Control.Concurrent (MVar, modifyMVar, newMVar, tryReadMVar)
import Control.Exception qualified as Exception
import Control.Exception.Safe (SomeException, try)
import Control.Monad (guard, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Either (fromRight)
import Data.IntSet qualified as IntSet
import Data.Set qualified as Set
import Data.Text qualified as T
import Foreign.Concurrent qualified as Foreign
import Foreign.ForeignPtr (ForeignPtr, finalizeForeignPtr, withForeignPtr)
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Core (ResultTrace (..))
import Occam.Oracle.DuckDB.Execution (
    ExecutionResult,
    ObserveResult,
    StatementFailure,
    executeDuckDBResultWithReplay,
    executeWithAPI,
    ignoreStatementFailure,
 )
import Occam.Oracle.DuckDB.FFI (
    closeDatabase,
    getDuckDBAPI,
    openFreshDatabase,
 )
import Occam.Oracle.DuckDB.Result (observeRows)
import Occam.Parser (DuckDBReplayUnit (..), nativeDuckDBReplayUnits)

{- | Build a candidate runner that reuses an immutable, configured replay
database for statically read-only query suffixes over the checkpoint's tables.
-}
prepareDuckDBReplayRunner ::
    Int ->
    ByteString ->
    (ByteString -> IO ()) ->
    IO (ObserveResult Bool -> ByteString -> IO (ExecutionResult Bool))
prepareDuckDBReplayRunner micros baseline reportFailure = do
    -- An eligible setup prefix needs a terminator for each of its statements.
    baselineUnits <-
        if BS.count 59 baseline >= minPrefixStatements
            then nativeDuckDBReplayUnits baseline
            else pure []
    let (setup, suffix) = span isSetup baselineUnits
        prefixCount = length setup
        tables =
            Set.fromList
                [table | unit <- setup, replayUnitKind unit == "create", Just table <- [replayUnitTable unit]]
        readOnly unit =
            replayUnitKind unit == "query" && maybe True (`Set.member` tables) (replayUnitTable unit)
        prefix
            | prefixCount >= minPrefixStatements
            , all readOnly suffix
            , lastSetup : _ <- reverse setup
            , Just end <- replayUnitEnd lastSetup =
                BS.take end baseline
            | otherwise = BS.empty
    saved <- newMVar Nothing
    pure $ \readMatch candidate -> do
        start <- getMonotonicTimeNSec
        let remaining = do
                now <- getMonotonicTimeNSec
                pure (max 0 (micros - fromIntegral ((now - start) `div` 1000)))
            fresh = do
                budget <- remaining
                executeDuckDBResultWithReplay
                    readMatch
                    Nothing
                    (captureFailure reportFailure candidate)
                    budget
                    candidate
            tailSQL = BS.drop (BS.length prefix) candidate
        replayed <- runMaybeT do
            guard (not (BS.null prefix) && prefix `BS.isPrefixOf` candidate)
            tailUnits <- liftIO $ nativeDuckDBReplayUnits tailSQL
            guard (not (null tailUnits) && all readOnly tailUnits)
            db <- MaybeT (loadReplayCheckpoint saved prefix remaining)
            liftIO $
                executeOnReplayCheckpoint db readMatch reportFailure candidate prefix prefixCount remaining tailSQL
        maybe fresh pure replayed
    where
        minPrefixStatements = 8
        isSetup unit = replayUnitKind unit == "create" || replayUnitKind unit == "insert"

loadReplayCheckpoint ::
    MVar (Maybe (ForeignPtr ())) -> ByteString -> IO Int -> IO (Maybe (ForeignPtr ()))
loadReplayCheckpoint saved prefix remaining = do
    mCurrent <- tryReadMVar saved
    case mCurrent of
        Just loaded@(Just _) -> pure loaded
        _ -> modifyMVar saved $ \current -> case current of
            Just _ -> pure (current, current)
            Nothing -> do
                budget <- remaining
                loaded <- buildReplayCheckpoint prefix budget
                pure (loaded, loaded)

executeOnReplayCheckpoint ::
    ForeignPtr () ->
    ObserveResult a ->
    (ByteString -> IO ()) ->
    ByteString ->
    ByteString ->
    Int ->
    IO Int ->
    ByteString ->
    IO (ExecutionResult a)
executeOnReplayCheckpoint db readMatch reportFailure candidate prefix prefixCount remaining tailSQL =
    withForeignPtr db $ \database -> do
        budget <- remaining
        let onTailFailure count index verify =
                captureFailure
                    reportFailure
                    candidate
                    (count + prefixCount)
                    (index + prefixCount)
                    (verify . BS.drop (BS.length prefix))
        result <-
            executeDuckDBResultWithReplay readMatch (Just (False, database)) onTailFailure budget tailSQL
        pure (fmap (fmap (second (shiftReplayTrace prefixCount))) result)

shiftReplayTrace :: Int -> ResultTrace -> ResultTrace
shiftReplayTrace offset trace =
    trace
        { traceStatementCount = offset + traceStatementCount trace
        , traceFirstResultIndex = offset + traceFirstResultIndex trace
        , traceLastResultIndex = offset + traceLastResultIndex trace
        , traceResultIndices = IntSet.mapMonotonic (+ offset) (traceResultIndices trace)
        }

buildReplayCheckpoint :: ByteString -> Int -> IO (Maybe (ForeignPtr ()))
buildReplayCheckpoint prefix micros = do
    loaded <- getDuckDBAPI
    case loaded of
        Left _ -> pure Nothing
        Right api -> do
            opened <- try $ Exception.mask $ \restore -> do
                db <- openFreshDatabase api
                handle <-
                    Foreign.newForeignPtr db (closeDatabase api db) `Exception.onException` closeDatabase api db
                result <-
                    restore (executeWithAPI api observeRows (Just (True, db)) ignoreStatementFailure micros prefix)
                        `Exception.onException` finalizeForeignPtr handle
                case result of
                    Right Nothing -> pure (Just handle)
                    _ -> finalizeForeignPtr handle >> pure Nothing
            pure (fromRight Nothing (opened :: Either SomeException (Maybe (ForeignPtr ()))))

captureFailure :: (ByteString -> IO ()) -> ByteString -> StatementFailure
captureFailure record candidate count index verify message
    | not (any (`T.isPrefixOf` message) deterministicErrorPrefixes) = pure ()
    | otherwise = do
        units <- nativeDuckDBReplayUnits candidate
        let prefixUnits = take (index + 1) units
        case reverse prefixUnits of
            failed : _
                | length units == count
                , all ((/= "other") . replayUnitKind) prefixUnits
                , Just end <- replayUnitEnd failed -> do
                    let bytes = BS.take end candidate
                    valid <- verify bytes
                    when valid (record bytes)
            _ -> pure ()
    where
        deterministicErrorPrefixes =
            ["Catalog Error:", "Binder Error:", "Conversion Error:", "Out of Range Error:"]
