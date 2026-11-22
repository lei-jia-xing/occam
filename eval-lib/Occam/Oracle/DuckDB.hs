{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Occam.Oracle.DuckDB (
    DuckDBContract (..),
    DuckDBError (..),
    loadDuckDBContract,
    generateDuckDBContract,
    executeDuckDBCandidate,
    executeDuckDBCandidateTimeout,
    executeDuckDBCandidateDetailedTimeout,
    duckdbContractOracle,
    duckdbContractOracleTimeout,
    duckdbContractObservation,
    isDuckDBAvailable,
    defaultDuckDBCandidateTimeoutMicros,
) where

import Control.Exception.Safe (SomeException, bracket, displayException, try)
import Control.Monad (filterM, forM)
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
import Data.Int (Int64)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, castPtr, nullPtr)
import Foreign.Storable (Storable (..))
import Occam.Core (Oracle, OracleOutcome (..))
import Occam.Oracle (OracleObservation (..))
import System.Directory (doesDirectoryExist, doesFileExist, findExecutable, listDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import Text.Read (readMaybe)

#if defined(mingw32_HOST_OS)
import Foreign.C.String (withCString)
import Foreign.Ptr (nullFunPtr)

foreign import ccall "windows.h LoadLibraryA"
    c_LoadLibrary :: CString -> IO (Ptr ())
foreign import ccall "windows.h GetProcAddress"
    c_GetProcAddress :: Ptr () -> CString -> IO (FunPtr ())
foreign import ccall "windows.h FreeLibrary"
    c_FreeLibrary :: Ptr () -> IO Bool
#else
import System.Posix.DynamicLinker (
    DL,
    RTLDFlags (..),
    dlclose,
    dlopen,
    dlsym,
 )
#endif

type DuckdbDatabase = Ptr ()
type DuckdbConnection = Ptr ()
type DuckdbPreparedStatement = Ptr ()
type DuckdbExtractedStatements = Ptr ()
type IdxT = Word64

data DuckDBResult
    = DuckDBResult
        !IdxT
        !IdxT
        !IdxT
        !(Ptr ())
        !CString
        !(Ptr ())

instance Storable DuckDBResult where
    sizeOf _ = 48
    alignment _ = 8
    peek p =
        DuckDBResult
            <$> peekByteOff p 0
            <*> peekByteOff p 8
            <*> peekByteOff p 16
            <*> peekByteOff p 24
            <*> peekByteOff p 32
            <*> peekByteOff p 40
    poke p (DuckDBResult a b c d e f) = do
        pokeByteOff p 0 a
        pokeByteOff p 8 b
        pokeByteOff p 16 c
        pokeByteOff p 24 d
        pokeByteOff p 32 e
        pokeByteOff p 40 f

type FnOpen = CString -> Ptr DuckdbDatabase -> IO CInt
type FnClose = Ptr DuckdbDatabase -> IO ()
type FnConnect = DuckdbDatabase -> Ptr DuckdbConnection -> IO CInt
type FnDisconnect = Ptr DuckdbConnection -> IO ()
type FnInterrupt = DuckdbConnection -> IO ()
type FnExtract = DuckdbConnection -> CString -> Ptr DuckdbExtractedStatements -> IO IdxT
type FnExtractError = DuckdbExtractedStatements -> IO CString
type FnPrepExtracted = DuckdbConnection -> DuckdbExtractedStatements -> IdxT -> Ptr DuckdbPreparedStatement -> IO CInt
type FnPrepError = DuckdbPreparedStatement -> IO CString
type FnPrepType = DuckdbPreparedStatement -> IO CInt
type FnExecPrepared = DuckdbPreparedStatement -> Ptr DuckDBResult -> IO CInt
type FnDestroyPrepare = Ptr DuckdbPreparedStatement -> IO ()
type FnDestroyExtracted = Ptr DuckdbExtractedStatements -> IO ()
type FnDestroyResult = Ptr DuckDBResult -> IO ()
type FnResultError = Ptr DuckDBResult -> IO CString
type FnRowCount = Ptr DuckDBResult -> IO IdxT
type FnColumnCount = Ptr DuckDBResult -> IO IdxT
type FnColumnType = Ptr DuckDBResult -> IdxT -> IO CInt
type FnValueIsNull = Ptr DuckDBResult -> IdxT -> IdxT -> IO CBool
type FnValueBoolean = Ptr DuckDBResult -> IdxT -> IdxT -> IO CBool
type FnValueInt64 = Ptr DuckDBResult -> IdxT -> IdxT -> IO Int64
type FnValueUint64 = Ptr DuckDBResult -> IdxT -> IdxT -> IO Word64
type FnValueDouble = Ptr DuckDBResult -> IdxT -> IdxT -> IO Double
type FnValueVarchar = Ptr DuckDBResult -> IdxT -> IdxT -> IO CString
type FnFree = Ptr () -> IO ()

foreign import ccall "dynamic" mkOpen :: FunPtr FnOpen -> FnOpen
foreign import ccall "dynamic" mkClose :: FunPtr FnClose -> FnClose
foreign import ccall "dynamic" mkConnect :: FunPtr FnConnect -> FnConnect
foreign import ccall "dynamic" mkDisconnect :: FunPtr FnDisconnect -> FnDisconnect
foreign import ccall "dynamic" mkInterrupt :: FunPtr FnInterrupt -> FnInterrupt
foreign import ccall "dynamic" mkExtract :: FunPtr FnExtract -> FnExtract
foreign import ccall "dynamic" mkExtractError :: FunPtr FnExtractError -> FnExtractError
foreign import ccall "dynamic" mkPrepExtracted :: FunPtr FnPrepExtracted -> FnPrepExtracted
foreign import ccall "dynamic" mkPrepError :: FunPtr FnPrepError -> FnPrepError
foreign import ccall "dynamic" mkPrepType :: FunPtr FnPrepType -> FnPrepType
foreign import ccall "dynamic" mkExecPrepared :: FunPtr FnExecPrepared -> FnExecPrepared
foreign import ccall "dynamic" mkDestroyPrepare :: FunPtr FnDestroyPrepare -> FnDestroyPrepare
foreign import ccall "dynamic" mkDestroyExtracted :: FunPtr FnDestroyExtracted -> FnDestroyExtracted
foreign import ccall "dynamic" mkDestroyResult :: FunPtr FnDestroyResult -> FnDestroyResult
foreign import ccall "dynamic" mkResultError :: FunPtr FnResultError -> FnResultError
foreign import ccall "dynamic" mkRowCount :: FunPtr FnRowCount -> FnRowCount
foreign import ccall "dynamic" mkColumnCount :: FunPtr FnColumnCount -> FnColumnCount
foreign import ccall "dynamic" mkColumnType :: FunPtr FnColumnType -> FnColumnType
foreign import ccall "dynamic" mkValueIsNull :: FunPtr FnValueIsNull -> FnValueIsNull
foreign import ccall "dynamic" mkValueBoolean :: FunPtr FnValueBoolean -> FnValueBoolean
foreign import ccall "dynamic" mkValueInt64 :: FunPtr FnValueInt64 -> FnValueInt64
foreign import ccall "dynamic" mkValueUint64 :: FunPtr FnValueUint64 -> FnValueUint64
foreign import ccall "dynamic" mkValueDouble :: FunPtr FnValueDouble -> FnValueDouble
foreign import ccall "dynamic" mkValueVarchar :: FunPtr FnValueVarchar -> FnValueVarchar
foreign import ccall "dynamic" mkFree :: FunPtr FnFree -> FnFree

data DuckDBAPI = DuckDBAPI
    { apiOpen :: !FnOpen
    , apiClose :: !FnClose
    , apiConnect :: !FnConnect
    , apiDisconnect :: !FnDisconnect
    , apiInterrupt :: !FnInterrupt
    , apiExtractStatements :: !FnExtract
    , apiExtractStatementsError :: !FnExtractError
    , apiPrepareExtractedStatement :: !FnPrepExtracted
    , apiPrepareError :: !FnPrepError
    , apiPreparedStatementType :: !FnPrepType
    , apiExecutePrepared :: !FnExecPrepared
    , apiDestroyPrepare :: !FnDestroyPrepare
    , apiDestroyExtracted :: !FnDestroyExtracted
    , apiDestroyResult :: !FnDestroyResult
    , apiResultError :: !FnResultError
    , apiRowCount :: !FnRowCount
    , apiColumnCount :: !FnColumnCount
    , apiColumnType :: !FnColumnType
    , apiValueIsNull :: !FnValueIsNull
    , apiValueBoolean :: !FnValueBoolean
    , apiValueInt64 :: !FnValueInt64
    , apiValueUint64 :: !FnValueUint64
    , apiValueDouble :: !FnValueDouble
    , apiValueVarchar :: !FnValueVarchar
    , apiFree :: !FnFree
    }

data DuckDBContract = DuckDBContract
    { contractExpectedRows :: ![[Value]]
    , contractStateSha256 :: !(Maybe Text)
    , contractCheckState :: !Bool
    }
    deriving (Eq, Show)

instance FromJSON DuckDBContract where
    parseJSON = withObject "DuckDBContract" $ \o ->
        DuckDBContract
            <$> o .: "expected_rows"
            <*> o .:? "state_sha256"
            <*> o .:? "check_state" .!= False

instance ToJSON DuckDBContract where
    toJSON :: DuckDBContract -> Value
    toJSON contract =
        object
            [ "expected_rows" .= contractExpectedRows contract
            , "state_sha256" .= contractStateSha256 contract
            , "check_state" .= contractCheckState contract
            ]

loadDuckDBContract :: FilePath -> IO (Either String DuckDBContract)
loadDuckDBContract path = do
    bytes <- BS.readFile path
    pure (eitherDecodeStrict' bytes)

defaultDuckDBCandidateTimeoutMicros :: Int
defaultDuckDBCandidateTimeoutMicros = 100000

data DuckDBError
    = DuckDBTimeout
    | DuckDBExecutionError !Text
    | DuckDBInfrastructureError !Text
    deriving (Eq, Show)

{-# NOINLINE duckdbApiCache #-}
duckdbApiCache :: IORef (Maybe (Either DuckDBError DuckDBAPI))
duckdbApiCache = unsafePerformIO (newIORef Nothing)

findDuckDBLibPath :: IO (Maybe FilePath)
findDuckDBLibPath = do
    mEnv <- lookupEnv "OCCAM_DUCKDB_LIB"
    case mEnv of
        Just p -> pure (Just p)
        Nothing -> do
            mEnv2 <- lookupEnv "DUCKDB_LIB_PATH"
            case mEnv2 of
                Just p -> pure (Just p)
                Nothing -> findSystemDuckDBLib

findSystemDuckDBLib :: IO (Maybe FilePath)
#if defined(mingw32_HOST_OS)
findSystemDuckDBLib = do
    pure (Just "duckdb.dll")
#elif defined(darwin_HOST_OS)
findSystemDuckDBLib = do
    mBare <- try (dlopen "libduckdb.dylib" [RTLD_LAZY]) :: IO (Either SomeException DL)
    case mBare of
        Right dl -> do
            dlclose dl
            pure (Just "libduckdb.dylib")
        Left _ -> pure Nothing
#else
findSystemDuckDBLib = do
    mBare <- try (dlopen "libduckdb.so" [RTLD_LAZY]) :: IO (Either SomeException DL)
    case mBare of
        Right dl -> do
            dlclose dl
            pure (Just "libduckdb.so")
        Left _ -> do
            let nixStore = "/nix/store"
            hasNix <- doesDirectoryExist nixStore
            if hasNix
                then do
                    entries <- listDirectory nixStore
                    let matches =
                            [ nixStore </> e </> "lib" </> "libduckdb.so"
                            | e <- entries
                            , "duckdb" `elem` words (map (\c -> if c == '-' then ' ' else c) e)
                            , "lib" `elem` words (map (\c -> if c == '-' then ' ' else c) e)
                            ]
                    existing <- filterM doesFileExist matches
                    case existing of
                        (f : _) -> pure (Just f)
                        [] -> findByExecutable
                else findByExecutable
  where
    findByExecutable = do
        mExe <- findExecutable "duckdb"
        case mExe of
            Just exePath -> do
                let candidate = exePath </> ".." </> ".." </> "lib" </> "libduckdb.so"
                exists <- doesFileExist candidate
                if exists then pure (Just candidate) else pure Nothing
            Nothing -> pure Nothing
#endif

loadDuckDBAPI :: IO (Either DuckDBError DuckDBAPI)
loadDuckDBAPI = do
    mLibPath <- findDuckDBLibPath
    case mLibPath of
        Nothing ->
            pure
                ( Left
                    ( DuckDBInfrastructureError
                        "Could not locate libduckdb. Set OCCAM_DUCKDB_LIB to its path."
                    )
                )
        Just libPath -> do
            res <- try (loadSymbols libPath) :: IO (Either SomeException DuckDBAPI)
            pure $ case res of
                Left err -> Left (DuckDBInfrastructureError (T.pack (displayException err)))
                Right api -> Right api

#if defined(mingw32_HOST_OS)
loadSymbols :: FilePath -> IO DuckDBAPI
loadSymbols libPath = withCString libPath $ \cLib -> do
    hMod <- c_LoadLibrary cLib
    if hMod == nullPtr
        then fail ("LoadLibrary failed for " <> libPath)
        else do
            let getProc :: String -> (FunPtr a -> b) -> IO b
                getProc name mk = withCString name $ \cName -> do
                    p <- c_GetProcAddress hMod cName
                    if p == nullFunPtr
                        then fail ("GetProcAddress failed for symbol: " <> name)
                        else pure (mk (castFunPtr p))
            apiOpen <- getProc "duckdb_open" mkOpen
            apiClose <- getProc "duckdb_close" mkClose
            apiConnect <- getProc "duckdb_connect" mkConnect
            apiDisconnect <- getProc "duckdb_disconnect" mkDisconnect
            apiInterrupt <- getProc "duckdb_interrupt" mkInterrupt
            apiExtractStatements <- getProc "duckdb_extract_statements" mkExtract
            apiExtractStatementsError <- getProc "duckdb_extract_statements_error" mkExtractError
            apiPrepareExtractedStatement <- getProc "duckdb_prepare_extracted_statement" mkPrepExtracted
            apiPrepareError <- getProc "duckdb_prepare_error" mkPrepError
            apiPreparedStatementType <- getProc "duckdb_prepared_statement_type" mkPrepType
            apiExecutePrepared <- getProc "duckdb_execute_prepared" mkExecPrepared
            apiDestroyPrepare <- getProc "duckdb_destroy_prepare" mkDestroyPrepare
            apiDestroyExtracted <- getProc "duckdb_destroy_extracted" mkDestroyExtracted
            apiDestroyResult <- getProc "duckdb_destroy_result" mkDestroyResult
            apiResultError <- getProc "duckdb_result_error" mkResultError
            apiRowCount <- getProc "duckdb_row_count" mkRowCount
            apiColumnCount <- getProc "duckdb_column_count" mkColumnCount
            apiColumnType <- getProc "duckdb_column_type" mkColumnType
            apiValueIsNull <- getProc "duckdb_value_is_null" mkValueIsNull
            apiValueBoolean <- getProc "duckdb_value_boolean" mkValueBoolean
            apiValueInt64 <- getProc "duckdb_value_int64" mkValueInt64
            apiValueUint64 <- getProc "duckdb_value_uint64" mkValueUint64
            apiValueDouble <- getProc "duckdb_value_double" mkValueDouble
            apiValueVarchar <- getProc "duckdb_value_varchar" mkValueVarchar
            apiFree <- getProc "duckdb_free" mkFree
            pure DuckDBAPI{..}
#else
loadSymbols :: FilePath -> IO DuckDBAPI
loadSymbols libPath = do
    dl <- dlopen libPath [RTLD_LAZY]
    let getSym :: String -> (FunPtr a -> b) -> IO b
        getSym name mk = do
            p <- dlsym dl name
            pure (mk (castFunPtr p))
    apiOpen <- getSym "duckdb_open" mkOpen
    apiClose <- getSym "duckdb_close" mkClose
    apiConnect <- getSym "duckdb_connect" mkConnect
    apiDisconnect <- getSym "duckdb_disconnect" mkDisconnect
    apiInterrupt <- getSym "duckdb_interrupt" mkInterrupt
    apiExtractStatements <- getSym "duckdb_extract_statements" mkExtract
    apiExtractStatementsError <- getSym "duckdb_extract_statements_error" mkExtractError
    apiPrepareExtractedStatement <- getSym "duckdb_prepare_extracted_statement" mkPrepExtracted
    apiPrepareError <- getSym "duckdb_prepare_error" mkPrepError
    apiPreparedStatementType <- getSym "duckdb_prepared_statement_type" mkPrepType
    apiExecutePrepared <- getSym "duckdb_execute_prepared" mkExecPrepared
    apiDestroyPrepare <- getSym "duckdb_destroy_prepare" mkDestroyPrepare
    apiDestroyExtracted <- getSym "duckdb_destroy_extracted" mkDestroyExtracted
    apiDestroyResult <- getSym "duckdb_destroy_result" mkDestroyResult
    apiResultError <- getSym "duckdb_result_error" mkResultError
    apiRowCount <- getSym "duckdb_row_count" mkRowCount
    apiColumnCount <- getSym "duckdb_column_count" mkColumnCount
    apiColumnType <- getSym "duckdb_column_type" mkColumnType
    apiValueIsNull <- getSym "duckdb_value_is_null" mkValueIsNull
    apiValueBoolean <- getSym "duckdb_value_boolean" mkValueBoolean
    apiValueInt64 <- getSym "duckdb_value_int64" mkValueInt64
    apiValueUint64 <- getSym "duckdb_value_uint64" mkValueUint64
    apiValueDouble <- getSym "duckdb_value_double" mkValueDouble
    apiValueVarchar <- getSym "duckdb_value_varchar" mkValueVarchar
    apiFree <- getSym "duckdb_free" mkFree
    pure DuckDBAPI{..}
#endif

getDuckDBAPI :: IO (Either DuckDBError DuckDBAPI)
getDuckDBAPI = do
    cached <- readIORef duckdbApiCache
    case cached of
        Just res -> pure res
        Nothing -> do
            res <- loadDuckDBAPI
            writeIORef duckdbApiCache (Just res)
            pure res

isDuckDBAvailable :: IO Bool
isDuckDBAvailable = do
    res <- getDuckDBAPI
    pure $ case res of
        Right _ -> True
        Left _ -> False

convertCell :: DuckDBAPI -> Ptr DuckDBResult -> IdxT -> IdxT -> IO Value
convertCell api resPtr col row = do
    isNull <- apiValueIsNull api resPtr col row
    if isNull /= 0
        then pure Null
        else do
            colType <- apiColumnType api resPtr col
            case colType of
                1 -> do
                    b <- apiValueBoolean api resPtr col row
                    pure (Bool (b /= 0))
                n | n >= 2 && n <= 5 -> do
                    v <- apiValueInt64 api resPtr col row
                    pure (Number (fromIntegral v))
                n | n >= 6 && n <= 9 -> do
                    v <- apiValueUint64 api resPtr col row
                    pure (Number (fromIntegral v))
                n | n >= 10 && n <= 11 -> do
                    v <- apiValueDouble api resPtr col row
                    pure (toJSON v)
                n | n == 16 || n == 32 -> do
                    cStr <- apiValueVarchar api resPtr col row
                    if cStr == nullPtr
                        then pure Null
                        else do
                            str <- peekCString cStr
                            apiFree api (castPtr cStr)
                            case readMaybe str of
                                Just (val :: Integer) -> pure (Number (fromInteger val))
                                Nothing -> pure (String (T.pack str))
                _ -> do
                    cStr <- apiValueVarchar api resPtr col row
                    if cStr == nullPtr
                        then pure Null
                        else do
                            bs <- BS.packCString cStr
                            apiFree api (castPtr cStr)
                            pure (String (TE.decodeUtf8 bs))

executeDuckDBCandidateDetailedTimeout :: Int -> ByteString -> IO (Either DuckDBError [[Value]])
executeDuckDBCandidateDetailedTimeout micros sql = do
    mApi <- getDuckDBAPI
    case mApi of
        Left err -> pure (Left err)
        Right api -> executeWithAPI api micros sql

executeWithAPI :: DuckDBAPI -> Int -> ByteString -> IO (Either DuckDBError [[Value]])
executeWithAPI api micros sql = do
    res <- try $
        alloca $ \dbPtr ->
            bracket
                ( do
                    st <- apiOpen api nullPtr dbPtr
                    if st /= 0
                        then fail "Failed to initialize in-memory DuckDB database"
                        else peek dbPtr
                )
                ( \_ -> do
                    apiClose api dbPtr
                )
                $ \db ->
                    alloca $ \conPtr ->
                        bracket
                            ( do
                                st <- apiConnect api db conPtr
                                if st /= 0
                                    then fail "Failed to connect to in-memory DuckDB database"
                                    else peek conPtr
                            )
                            ( \_ -> do
                                apiDisconnect api conPtr
                            )
                            $ \con -> do
                                mRes <- timeout micros (runStatements con)
                                case mRes of
                                    Nothing -> do
                                        apiInterrupt api con
                                        pure (Left DuckDBTimeout)
                                    Just inner -> pure inner
    pure $ case res of
        Left (err :: SomeException) -> Left (DuckDBInfrastructureError (T.pack (displayException err)))
        Right inner -> inner
  where
    runStatements con =
        BSC.useAsCString sql $ \sqlCStr ->
            alloca $ \extPtr ->
                bracket
                    ( do
                        n <- apiExtractStatements api con sqlCStr extPtr
                        ext <- peek extPtr
                        pure (n, ext)
                    )
                    ( \_ -> apiDestroyExtracted api extPtr
                    )
                    $ \(n, ext) -> do
                        cErr <- apiExtractStatementsError api ext
                        if cErr /= nullPtr
                            then do
                                errStr <- peekCString cErr
                                pure (Left (DuckDBExecutionError (T.pack errStr)))
                            else if n == 0
                                then pure (Right [])
                                else executeExtractedStatements con ext n

    executeExtractedStatements con ext n = do
        lastRowsRef <- newIORef []
        alloca $ \prepPtr ->
            allocaBytes 48 $ \resPtr -> do
                let loop i
                        | i >= n = Right <$> readIORef lastRowsRef
                        | otherwise = do
                            prepSt <- apiPrepareExtractedStatement api con ext i prepPtr
                            if prepSt /= 0
                                then do
                                    prep <- peek prepPtr
                                    cErr <- apiPrepareError api prep
                                    errStr <- if cErr == nullPtr then pure "Prepare error" else peekCString cErr
                                    apiDestroyPrepare api prepPtr
                                    pure (Left (DuckDBExecutionError (T.pack errStr)))
                                else do
                                    prep <- peek prepPtr
                                    stype <- apiPreparedStatementType api prep
                                    execSt <- apiExecutePrepared api prep resPtr
                                    if execSt /= 0
                                        then do
                                            cErr <- apiResultError api resPtr
                                            errStr <- if cErr == nullPtr then pure "Execution error" else peekCString cErr
                                            apiDestroyResult api resPtr
                                            apiDestroyPrepare api prepPtr
                                            pure (Left (DuckDBExecutionError (T.pack errStr)))
                                        else do
                                            rows <- apiRowCount api resPtr
                                            cols <- apiColumnCount api resPtr
                                            let isQuery = stype == 1 || (stype /= 2 && stype /= 3 && stype /= 5 && stype /= 7 && stype /= 15 && rows > 0)
                                            if isQuery && rows > 0 && cols > 0
                                                then do
                                                    rowList <- forM [0 .. rows - 1] $ \r ->
                                                        forM [0 .. cols - 1] $ \c ->
                                                            convertCell api resPtr c r
                                                    writeIORef lastRowsRef rowList
                                                else pure ()
                                            apiDestroyResult api resPtr
                                            apiDestroyPrepare api prepPtr
                                            loop (i + 1)
                loop 0

executeDuckDBCandidateTimeout :: Int -> ByteString -> IO (Either Text [[Value]])
executeDuckDBCandidateTimeout micros sql = do
    res <- executeDuckDBCandidateDetailedTimeout micros sql
    pure $ case res of
        Left DuckDBTimeout -> Left "timeout"
        Left (DuckDBExecutionError err) -> Left err
        Left (DuckDBInfrastructureError err) -> Left ("infrastructure error: " <> err)
        Right rows -> Right rows

executeDuckDBCandidate :: ByteString -> IO (Either Text [[Value]])
executeDuckDBCandidate = executeDuckDBCandidateTimeout defaultDuckDBCandidateTimeoutMicros

generateDuckDBContract :: ByteString -> IO (Either Text DuckDBContract)
generateDuckDBContract sql = do
    res <- executeDuckDBCandidate sql
    pure $ case res of
        Left err -> Left err
        Right rows ->
            Right
                DuckDBContract
                    { contractExpectedRows = rows
                    , contractStateSha256 = Nothing
                    , contractCheckState = False
                    }

duckdbContractOracleTimeout :: Int -> DuckDBContract -> Oracle
duckdbContractOracleTimeout micros contract candidate = do
    res <- executeDuckDBCandidateDetailedTimeout micros candidate
    case res of
        Left DuckDBTimeout -> pure OracleInconclusive
        Left (DuckDBInfrastructureError _) -> pure OracleInconclusive
        Left (DuckDBExecutionError _) -> pure OracleRejected
        Right rows
            | rows == contractExpectedRows contract -> pure OracleAccepted
            | otherwise -> pure OracleRejected

duckdbContractOracle :: DuckDBContract -> Oracle
duckdbContractOracle = duckdbContractOracleTimeout defaultDuckDBCandidateTimeoutMicros

duckdbContractObservation :: DuckDBContract -> ByteString -> IO OracleObservation
duckdbContractObservation contract candidate = do
    res <- executeDuckDBCandidateDetailedTimeout defaultDuckDBCandidateTimeoutMicros candidate
    pure $ case res of
        Left DuckDBTimeout -> OracleTimedOut
        Left (DuckDBExecutionError err) -> OracleIOFailed err
        Left (DuckDBInfrastructureError err) -> OracleIOFailed err
        Right rows
            | rows == contractExpectedRows contract -> OracleFinished ExitSuccess
            | otherwise -> OracleFinished (ExitFailure 1)
