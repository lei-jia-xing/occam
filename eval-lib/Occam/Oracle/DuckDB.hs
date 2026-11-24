{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Occam.Oracle.DuckDB (
    DuckDBContract (..),
    DuckDBError (..),
    loadDuckDBContract,
    generateDuckDBContract,
    generateDuckDBContractTimeout,
    generateDuckDBContractWithResultStatement,
    generateDuckDBContractWithTrace,
    executeDuckDBCandidate,
    executeDuckDBCandidateTimeout,
    executeDuckDBCandidateDetailedTimeout,
    duckdbContractOracle,
    duckdbContractOracleTimeout,
    duckdbContractObservation,
    duckdbContractObservationTimeout,
    duckdbContractObservationWithResultStatement,
    duckdbContractObservationWithTrace,
    isDuckDBAvailable,
    defaultDuckDBCandidateTimeoutMicros,
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
import Control.Monad (filterM, forM, when)
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
import Data.Bits (shiftL)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntSet qualified as IntSet
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CBool (..), CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes, callocBytes, free)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, castPtr, nullPtr)
import Foreign.Storable (Storable, peek, peekByteOff, peekElemOff, poke)
import GHC.Float (castDoubleToWord64)
import Occam.Core (Oracle, ResultStatement (..), ResultTrace (..), traceResultStatement)
import Occam.Oracle (OracleObservation (..), observationOutcome)
import Occam.Oracle.Result (ResultSet (..))
import System.Directory (doesDirectoryExist, doesFileExist, findExecutable, listDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

#if defined(mingw32_HOST_OS)
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
type DuckdbConfig = Ptr ()
type DuckdbPreparedStatement = Ptr ()
type DuckdbExtractedStatements = Ptr ()
type IdxT = Word64

data DuckDBResult

foreign import ccall unsafe "occam_duckdb_result_size"
    resultSize :: IO CSize

foreign import ccall unsafe "occam_duckdb_result_kind"
    resultKind :: FunPtr FnResultKind -> Ptr DuckDBResult -> IO CInt

foreign import ccall unsafe "occam_duckdb_fetch_chunk"
    fetchChunk :: FunPtr FnFetchChunk -> Ptr DuckDBResult -> IO Chunk

foreign import ccall unsafe "occam_duckdb_vector_bytes"
    vectorBytes :: FunPtr FnStringData -> Ptr () -> IdxT -> Ptr IdxT -> IO CString

-- The by-value signatures are called by the C shim.
data FnResultKind
data FnFetchChunk
data FnStringData

type Chunk = Ptr ()
type Vector = Ptr ()

type FnOpen = CString -> Ptr DuckdbDatabase -> IO CInt
type FnCreateConfig = Ptr DuckdbConfig -> IO CInt
type FnSetConfig = DuckdbConfig -> CString -> CString -> IO CInt
type FnOpenExt = CString -> Ptr DuckdbDatabase -> DuckdbConfig -> Ptr CString -> IO CInt
type FnDestroyConfig = Ptr DuckdbConfig -> IO ()
type FnClose = Ptr DuckdbDatabase -> IO ()
type FnConnect = DuckdbDatabase -> Ptr DuckdbConnection -> IO CInt
type FnDisconnect = Ptr DuckdbConnection -> IO ()
type FnInterrupt = DuckdbConnection -> IO ()
type FnExtract = DuckdbConnection -> CString -> Ptr DuckdbExtractedStatements -> IO IdxT
type FnExtractError = DuckdbExtractedStatements -> IO CString
type FnPrepExtracted =
    DuckdbConnection -> DuckdbExtractedStatements -> IdxT -> Ptr DuckdbPreparedStatement -> IO CInt
type FnPrepError = DuckdbPreparedStatement -> IO CString
type FnQuery = DuckdbConnection -> CString -> Ptr DuckDBResult -> IO CInt
type FnExecPrepared = DuckdbPreparedStatement -> Ptr DuckDBResult -> IO CInt
type FnDestroyPrepare = Ptr DuckdbPreparedStatement -> IO ()
type FnDestroyExtracted = Ptr DuckdbExtractedStatements -> IO ()
type FnDestroyResult = Ptr DuckDBResult -> IO ()
type FnResultError = Ptr DuckDBResult -> IO CString
type FnColumnCount = Ptr DuckDBResult -> IO IdxT
type FnColumnName = Ptr DuckDBResult -> IdxT -> IO CString
type FnColumnType = Ptr DuckDBResult -> IdxT -> IO CInt
type FnChunkSize = Chunk -> IO IdxT
type FnDestroyChunk = Ptr Chunk -> IO ()
type FnChunkVector = Chunk -> IdxT -> IO Vector
type FnVectorData = Vector -> IO (Ptr ())
type FnValidity = Vector -> IO (Ptr Word64)
type FnRowValid = Ptr Word64 -> IdxT -> IO CBool
foreign import ccall "dynamic" mkOpen :: FunPtr FnOpen -> FnOpen
foreign import ccall "dynamic" mkCreateConfig :: FunPtr FnCreateConfig -> FnCreateConfig
foreign import ccall "dynamic" mkSetConfig :: FunPtr FnSetConfig -> FnSetConfig
foreign import ccall "dynamic" mkOpenExt :: FunPtr FnOpenExt -> FnOpenExt
foreign import ccall "dynamic" mkDestroyConfig :: FunPtr FnDestroyConfig -> FnDestroyConfig
foreign import ccall "dynamic" mkClose :: FunPtr FnClose -> FnClose
foreign import ccall "dynamic" mkConnect :: FunPtr FnConnect -> FnConnect
foreign import ccall "dynamic" mkDisconnect :: FunPtr FnDisconnect -> FnDisconnect
foreign import ccall "dynamic" mkInterrupt :: FunPtr FnInterrupt -> FnInterrupt
foreign import ccall "dynamic" mkExtract :: FunPtr FnExtract -> FnExtract
foreign import ccall "dynamic" mkExtractError :: FunPtr FnExtractError -> FnExtractError
foreign import ccall "dynamic" mkPrepExtracted :: FunPtr FnPrepExtracted -> FnPrepExtracted
foreign import ccall "dynamic" mkPrepError :: FunPtr FnPrepError -> FnPrepError
foreign import ccall "dynamic" mkQuery :: FunPtr FnQuery -> FnQuery
foreign import ccall "dynamic" mkExecPrepared :: FunPtr FnExecPrepared -> FnExecPrepared
foreign import ccall "dynamic" mkDestroyPrepare :: FunPtr FnDestroyPrepare -> FnDestroyPrepare
foreign import ccall "dynamic" mkDestroyExtracted :: FunPtr FnDestroyExtracted -> FnDestroyExtracted
foreign import ccall "dynamic" mkDestroyResult :: FunPtr FnDestroyResult -> FnDestroyResult
foreign import ccall "dynamic" mkResultError :: FunPtr FnResultError -> FnResultError
foreign import ccall "dynamic" mkColumnCount :: FunPtr FnColumnCount -> FnColumnCount
foreign import ccall "dynamic" mkColumnName :: FunPtr FnColumnName -> FnColumnName
foreign import ccall "dynamic" mkColumnType :: FunPtr FnColumnType -> FnColumnType
foreign import ccall "dynamic" mkChunkSize :: FunPtr FnChunkSize -> FnChunkSize
foreign import ccall "dynamic" mkDestroyChunk :: FunPtr FnDestroyChunk -> FnDestroyChunk
foreign import ccall "dynamic" mkChunkVector :: FunPtr FnChunkVector -> FnChunkVector
foreign import ccall "dynamic" mkVectorData :: FunPtr FnVectorData -> FnVectorData
foreign import ccall "dynamic" mkValidity :: FunPtr FnValidity -> FnValidity
foreign import ccall "dynamic" mkRowValid :: FunPtr FnRowValid -> FnRowValid

data DuckDBConfigOpen = DuckDBConfigOpen
    { apiCreateConfig :: !FnCreateConfig
    , apiSetConfig :: !FnSetConfig
    , apiOpenExt :: !FnOpenExt
    , apiDestroyConfig :: !FnDestroyConfig
    }

data DuckDBAPI = DuckDBAPI
    { apiOpen :: !FnOpen
    , apiConfigOpen :: !(Maybe DuckDBConfigOpen)
    , apiClose :: !FnClose
    , apiConnect :: !FnConnect
    , apiDisconnect :: !FnDisconnect
    , apiInterrupt :: !FnInterrupt
    , apiExtractStatements :: !FnExtract
    , apiExtractStatementsError :: !FnExtractError
    , apiPrepareExtractedStatement :: !FnPrepExtracted
    , apiPrepareError :: !FnPrepError
    , apiQuery :: !FnQuery
    , apiResultKind :: !(FunPtr FnResultKind)
    , apiExecutePrepared :: !FnExecPrepared
    , apiDestroyPrepare :: !FnDestroyPrepare
    , apiDestroyExtracted :: !FnDestroyExtracted
    , apiDestroyResult :: !FnDestroyResult
    , apiResultError :: !FnResultError
    , apiColumnCount :: !FnColumnCount
    , apiColumnName :: !FnColumnName
    , apiColumnType :: !FnColumnType
    , apiFetchChunk :: !(FunPtr FnFetchChunk)
    , apiDestroyChunk :: !FnDestroyChunk
    , apiChunkSize :: !FnChunkSize
    , apiChunkVector :: !FnChunkVector
    , apiVectorData :: !FnVectorData
    , apiValidity :: !FnValidity
    , apiRowValid :: !FnRowValid
    , apiStringData :: !(FunPtr FnStringData)
    }

data DuckDBContract = DuckDBContract
    { contractExpectedRows :: ![[Value]]
    , contractExpectedColumns :: ![Text]
    , contractExpectedTypes :: ![Int]
    , contractStateSha256 :: !(Maybe Text)
    , contractCheckState :: !Bool
    }
    deriving (Eq, Show)

instance FromJSON DuckDBContract where
    parseJSON = withObject "DuckDBContract" $ \o ->
        DuckDBContract
            <$> o .: "expected_rows"
            <*> o .: "expected_columns"
            <*> o .: "expected_types"
            <*> o .:? "state_sha256"
            <*> o .:? "check_state" .!= False

instance ToJSON DuckDBContract where
    toJSON :: DuckDBContract -> Value
    toJSON contract =
        object
            [ "expected_rows" .= contractExpectedRows contract
            , "expected_columns" .= contractExpectedColumns contract
            , "expected_types" .= contractExpectedTypes contract
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
    | DuckDBUnsupportedResultTypes ![Int]
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
            let tryProc :: String -> (FunPtr a -> b) -> IO (Maybe b)
                tryProc name mk = withCString name $ \cName -> do
                    p <- c_GetProcAddress hMod cName
                    pure (if p == nullFunPtr then Nothing else Just (mk (castFunPtr p)))
            apiOpen <- getProc "duckdb_open" mkOpen
            apiConfigOpen <- do
                c1 <- tryProc "duckdb_create_config" mkCreateConfig
                c2 <- tryProc "duckdb_set_config" mkSetConfig
                c3 <- tryProc "duckdb_open_ext" mkOpenExt
                c4 <- tryProc "duckdb_destroy_config" mkDestroyConfig
                pure (DuckDBConfigOpen <$> c1 <*> c2 <*> c3 <*> c4)
            apiClose <- getProc "duckdb_close" mkClose
            apiConnect <- getProc "duckdb_connect" mkConnect
            apiDisconnect <- getProc "duckdb_disconnect" mkDisconnect
            apiInterrupt <- getProc "duckdb_interrupt" mkInterrupt
            apiExtractStatements <- getProc "duckdb_extract_statements" mkExtract
            apiExtractStatementsError <- getProc "duckdb_extract_statements_error" mkExtractError
            apiPrepareExtractedStatement <- getProc "duckdb_prepare_extracted_statement" mkPrepExtracted
            apiPrepareError <- getProc "duckdb_prepare_error" mkPrepError
            apiQuery <- getProc "duckdb_query" mkQuery
            apiExecutePrepared <- getProc "duckdb_execute_prepared" mkExecPrepared
            apiDestroyPrepare <- getProc "duckdb_destroy_prepare" mkDestroyPrepare
            apiDestroyExtracted <- getProc "duckdb_destroy_extracted" mkDestroyExtracted
            apiDestroyResult <- getProc "duckdb_destroy_result" mkDestroyResult
            apiResultError <- getProc "duckdb_result_error" mkResultError
            apiColumnCount <- getProc "duckdb_column_count" mkColumnCount
            apiColumnName <- getProc "duckdb_column_name" mkColumnName
            apiColumnType <- getProc "duckdb_column_type" mkColumnType
            apiFetchChunk <- getProc "duckdb_fetch_chunk" id
            apiDestroyChunk <- getProc "duckdb_destroy_data_chunk" mkDestroyChunk
            apiChunkSize <- getProc "duckdb_data_chunk_get_size" mkChunkSize
            apiChunkVector <- getProc "duckdb_data_chunk_get_vector" mkChunkVector
            apiVectorData <- getProc "duckdb_vector_get_data" mkVectorData
            apiValidity <- getProc "duckdb_vector_get_validity" mkValidity
            apiRowValid <- getProc "duckdb_validity_row_is_valid" mkRowValid
            apiStringData <- getProc "duckdb_string_t_data" id
            apiResultKind <- getProc "duckdb_result_return_type" id
            pure DuckDBAPI{..}
#else
loadSymbols :: FilePath -> IO DuckDBAPI
loadSymbols libPath = do
    dl <- dlopen libPath [RTLD_LAZY]
    let getSym :: String -> (FunPtr a -> b) -> IO b
        getSym name mk = do
            p <- dlsym dl name
            pure (mk (castFunPtr p))
    let trySym :: String -> (FunPtr a -> b) -> IO (Maybe b)
        trySym name mk = do
            res <- try (dlsym dl name) :: IO (Either SomeException (FunPtr ()))
            pure $ case res of
                Right p -> Just (mk (castFunPtr p))
                Left _ -> Nothing
    apiOpen <- getSym "duckdb_open" mkOpen
    apiConfigOpen <- do
        c1 <- trySym "duckdb_create_config" mkCreateConfig
        c2 <- trySym "duckdb_set_config" mkSetConfig
        c3 <- trySym "duckdb_open_ext" mkOpenExt
        c4 <- trySym "duckdb_destroy_config" mkDestroyConfig
        pure (DuckDBConfigOpen <$> c1 <*> c2 <*> c3 <*> c4)
    apiClose <- getSym "duckdb_close" mkClose
    apiConnect <- getSym "duckdb_connect" mkConnect
    apiDisconnect <- getSym "duckdb_disconnect" mkDisconnect
    apiInterrupt <- getSym "duckdb_interrupt" mkInterrupt
    apiExtractStatements <- getSym "duckdb_extract_statements" mkExtract
    apiExtractStatementsError <- getSym "duckdb_extract_statements_error" mkExtractError
    apiPrepareExtractedStatement <- getSym "duckdb_prepare_extracted_statement" mkPrepExtracted
    apiPrepareError <- getSym "duckdb_prepare_error" mkPrepError
    apiQuery <- getSym "duckdb_query" mkQuery
    apiExecutePrepared <- getSym "duckdb_execute_prepared" mkExecPrepared
    apiDestroyPrepare <- getSym "duckdb_destroy_prepare" mkDestroyPrepare
    apiDestroyExtracted <- getSym "duckdb_destroy_extracted" mkDestroyExtracted
    apiDestroyResult <- getSym "duckdb_destroy_result" mkDestroyResult
    apiResultError <- getSym "duckdb_result_error" mkResultError
    apiColumnCount <- getSym "duckdb_column_count" mkColumnCount
    apiColumnName <- getSym "duckdb_column_name" mkColumnName
    apiColumnType <- getSym "duckdb_column_type" mkColumnType
    apiFetchChunk <- getSym "duckdb_fetch_chunk" id
    apiDestroyChunk <- getSym "duckdb_destroy_data_chunk" mkDestroyChunk
    apiChunkSize <- getSym "duckdb_data_chunk_get_size" mkChunkSize
    apiChunkVector <- getSym "duckdb_data_chunk_get_vector" mkChunkVector
    apiVectorData <- getSym "duckdb_vector_get_data" mkVectorData
    apiValidity <- getSym "duckdb_vector_get_validity" mkValidity
    apiRowValid <- getSym "duckdb_validity_row_is_valid" mkRowValid
    apiStringData <- getSym "duckdb_string_t_data" id
    apiResultKind <- getSym "duckdb_result_return_type" id
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

-- NOTE: DuckDB's legacy string accessors truncate at embedded NULs. Read
-- chunk vectors here, including for scalars, to preserve complete values.
convertCell :: DuckDBAPI -> (CInt, Ptr (), Ptr Word64) -> IdxT -> IO Value
convertCell api (colType, values, validity) row = do
    valid <- if validity == nullPtr then pure 1 else apiRowValid api validity row
    if valid == 0
        then pure Null
        else case colType of
            1 -> Bool . (/= (0 :: Word8)) <$> item
            2 -> integral @Int8
            3 -> integral @Int16
            4 -> integral @Int32
            5 -> integral @Int64
            6 -> integral @Word8
            7 -> integral @Word16
            8 -> integral @Word32
            9 -> integral @Word64
            10 -> floatBits . realToFrac <$> item @Float
            11 -> floatBits <$> item @Double
            16 -> wide (toInteger . (fromIntegral :: Word64 -> Int64))
            17 -> do
                value <- readBytes
                either (fail . show) (pure . String) (TE.decodeUtf8' value)
            18 -> do
                value <- readBytes
                pure (object ["blob" .= BS.unpack value])
            32 -> wide toInteger
            36 -> pure Null
            _ -> fail ("Unsupported DuckDB result type " ++ show colType)
    where
        item :: (Storable a) => IO a
        item = peekElemOff (castPtr values) (fromIntegral row)
        integral :: forall a. (Integral a, Storable a) => IO Value
        integral = Number . fromIntegral <$> item @a
        floatBits value = object ["float64_bits" .= castDoubleToWord64 value]
        wide highInteger = do
            let offset = fromIntegral row * 16
            low <- peekByteOff values offset :: IO Word64
            high <- peekByteOff values (offset + 8) :: IO Word64
            pure (Number (fromInteger ((highInteger high `shiftL` 64) + toInteger low)))
        readBytes = alloca $ \sizePtr -> do
            ptr <- vectorBytes (apiStringData api) values row sizePtr
            size <- peek sizePtr
            if size == 0 then pure BS.empty else BS.packCStringLen (ptr, fromIntegral size)

executeDuckDBCandidateDetailedTimeout :: Int -> ByteString -> IO (Either DuckDBError [[Value]])
executeDuckDBCandidateDetailedTimeout micros sql =
    fmap (maybe [] (resultRows . fst)) <$> executeDuckDBResult micros sql

executeDuckDBResult ::
    Int -> ByteString -> IO (Either DuckDBError (Maybe (ResultSet, ResultTrace)))
executeDuckDBResult micros sql
    | BS.elem 0 sql = pure (Left (DuckDBExecutionError "SQL contains a NUL byte"))
    | otherwise = do
        mApi <- getDuckDBAPI
        case mApi of
            Left err -> pure (Left err)
            Right api -> executeWithAPI api micros sql

openMemoryDb :: DuckDBAPI -> Ptr DuckdbDatabase -> IO CInt
openMemoryDb api dbPtr = case apiConfigOpen api of
    Nothing -> apiOpen api nullPtr dbPtr
    Just DuckDBConfigOpen {..} ->
        alloca $ \cfgPtr ->
            bracket
                ( do
                    st <- apiCreateConfig cfgPtr
                    if st /= 0 then pure nullPtr else peek cfgPtr
                )
                (\cfg -> when (cfg /= nullPtr) (apiDestroyConfig cfgPtr))
                $ \cfg ->
                    if cfg == nullPtr
                        then apiOpen api nullPtr dbPtr
                        else do
                            _ <- withCString "threads" $ \k -> withCString "1" $ \v -> apiSetConfig cfg k v
                            apiOpenExt nullPtr dbPtr cfg nullPtr

executeWithAPI ::
    DuckDBAPI -> Int -> ByteString -> IO (Either DuckDBError (Maybe (ResultSet, ResultTrace)))
executeWithAPI api micros sql = do
    res <- try $
        alloca $ \dbPtr ->
            bracket
                ( do
                    st <- openMemoryDb api dbPtr
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
                                mRes <- executeInterruptibly micros (apiInterrupt api con) $ do
                                    configure con
                                    runStatements con
                                pure (fromMaybe (Left DuckDBTimeout) mRes)
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
                        (\_ -> apiDestroyExtracted api extPtr)
                        $ \(n, ext) -> do
                            cErr <- apiExtractStatementsError api ext
                            if cErr /= nullPtr
                                then do
                                    errStr <- peekCString cErr
                                    pure (Left (DuckDBExecutionError (T.pack errStr)))
                                else
                                    if n == 0
                                        then pure (Right Nothing)
                                        else executeExtractedStatements con ext n

        -- NOTE: In-memory connections can otherwise read or modify external files.
        configure con = withCString "SET enable_external_access = false; SET lock_configuration = true;" $ \settings ->
            withResult (apiQuery api con settings) $ \status _ ->
                when (status /= 0) (fail "Could not disable and lock DuckDB external access")

        withResult :: (Ptr DuckDBResult -> IO CInt) -> (CInt -> Ptr DuckDBResult -> IO a) -> IO a
        withResult run use = do
            size <- resultSize
            allocaBytes (fromIntegral size) $ \ptr ->
                bracket (run ptr) (const (apiDestroyResult api ptr)) (`use` ptr)

        executeExtractedStatements con ext n = alloca $ \prepPtr -> do
            size <- fromIntegral <$> resultSize
            let withBuffer = bracket (callocBytes size) $ \ptr -> do
                    apiDestroyResult api ptr
                    free ptr
                clear ptr = Exception.mask_ $ do
                    apiDestroyResult api ptr
                    fillBytes ptr 0 size
                loop i current retained firstResult lastResult resultIndices
                    | i >= n = case lastResult of
                        Nothing -> pure (Right Nothing)
                        Just index -> do
                            result <- observeRows retained
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
                    | otherwise = do
                        executed <- bracket
                            (apiPrepareExtractedStatement api con ext i prepPtr)
                            (const (apiDestroyPrepare api prepPtr))
                            $ \status -> do
                                prep <- peek prepPtr
                                if status /= 0
                                    then Left . DuckDBExecutionError <$> errorText "Prepare error" (apiPrepareError api prep)
                                    else do
                                        execStatus <- apiExecutePrepared api prep current
                                        if execStatus /= 0
                                            then Left . DuckDBExecutionError <$> errorText "Execution error" (apiResultError api current)
                                            else Right <$> resultKind (apiResultKind api) current
                        case executed of
                            Left err -> pure (Left err)
                            Right kind
                                | kind == 3 -> do
                                    -- DUCKDB_RESULT_TYPE_QUERY_RESULT
                                    clear retained
                                    loop
                                        (i + 1)
                                        retained
                                        current
                                        (Just (fromMaybe i firstResult))
                                        (Just i)
                                        (IntSet.insert (fromIntegral i) resultIndices)
                                | otherwise -> do
                                    clear current
                                    loop (i + 1) current retained firstResult lastResult resultIndices
            -- The buffers own the latest query result and the current execution.
            withBuffer $ \current -> withBuffer $ \retained -> loop 0 current retained Nothing Nothing IntSet.empty

        errorText fallback action = do
            message <- action
            if message == nullPtr then pure fallback else T.pack <$> peekCString message

        observeRows ptr = do
            count <- apiColumnCount api ptr
            let columns = takeWhile (< count) [0 ..]
            names <- forM columns $ \col -> do
                name <- apiColumnName api ptr col
                if name == nullPtr
                    then fail "DuckDB omitted a result column name"
                    else TE.decodeUtf8 <$> BS.packCString name
            types <- mapM (apiColumnType api ptr) columns
            -- TODO: Add lossless decoding for exact decimals and composite types.
            let columnTypes = map fromIntegral types
            if all supportedType types
                then do
                    rows <- readChunks ptr (zip types columns) []
                    pure (Right (ResultSet names columnTypes rows))
                else pure (Left (DuckDBUnsupportedResultTypes columnTypes))

        readChunks ptr columnTypes accumulated = do
            chunkRows <- alloca $ \chunkPtr -> do
                bracket
                    (do chunk <- fetchChunk (apiFetchChunk api) ptr; poke chunkPtr chunk; pure chunk)
                    (\chunk -> when (chunk /= nullPtr) (apiDestroyChunk api chunkPtr))
                    $ \chunk ->
                        if chunk == nullPtr
                            then pure Nothing
                            else do
                                size <- apiChunkSize api chunk
                                vectors <- forM columnTypes $ \(ty, col) -> do
                                    vector <- apiChunkVector api chunk col
                                    values <- apiVectorData api vector
                                    validity <- apiValidity api vector
                                    pure (ty, values, validity)
                                Just
                                    <$> forM (takeWhile (< size) [0 ..]) (\row -> mapM (\vector -> convertCell api vector row) vectors)
            case chunkRows of
                Nothing -> pure (concat (reverse accumulated))
                Just rows -> readChunks ptr columnTypes (rows : accumulated)

        supportedType ty = (ty >= 1 && ty <= 11) || ty `elem` [16, 17, 18, 32, 36]

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

executeDuckDBCandidateTimeout :: Int -> ByteString -> IO (Either Text [[Value]])
executeDuckDBCandidateTimeout micros sql = do
    res <- executeDuckDBCandidateDetailedTimeout micros sql
    pure $ case res of
        Left DuckDBTimeout -> Left "timeout"
        Left (DuckDBExecutionError err) -> Left err
        Left (DuckDBUnsupportedResultTypes types) -> Left (unsupportedResultTypes types)
        Left (DuckDBInfrastructureError err) -> Left ("infrastructure error: " <> err)
        Right rows -> Right rows

executeDuckDBCandidate :: ByteString -> IO (Either Text [[Value]])
executeDuckDBCandidate = executeDuckDBCandidateTimeout defaultDuckDBCandidateTimeoutMicros

generateDuckDBContract :: ByteString -> IO (Either Text DuckDBContract)
generateDuckDBContract = generateDuckDBContractTimeout defaultDuckDBCandidateTimeoutMicros

generateDuckDBContractTimeout :: Int -> ByteString -> IO (Either Text DuckDBContract)
generateDuckDBContractTimeout micros sql =
    fmap fst <$> generateDuckDBContractWithTrace micros sql

generateDuckDBContractWithResultStatement ::
    Int -> ByteString -> IO (Either Text (DuckDBContract, ResultStatement))
generateDuckDBContractWithResultStatement micros sql =
    fmap (second traceResultStatement) <$> generateDuckDBContractWithTrace micros sql

generateDuckDBContractWithTrace ::
    Int -> ByteString -> IO (Either Text (DuckDBContract, ResultTrace))
generateDuckDBContractWithTrace micros sql = do
    res <- executeDuckDBResult micros sql
    pure $ case res of
        Left DuckDBTimeout -> Left "timeout"
        Left (DuckDBExecutionError err) -> Left err
        Left (DuckDBUnsupportedResultTypes types) -> Left (unsupportedResultTypes types)
        Left (DuckDBInfrastructureError err) -> Left err
        Right Nothing -> Left "Input must produce a result set"
        Right (Just (result, trace)) ->
            Right
                ( DuckDBContract
                    { contractExpectedRows = resultRows result
                    , contractExpectedColumns = resultColumns result
                    , contractExpectedTypes = resultColumnTypes result
                    , contractStateSha256 = Nothing
                    , contractCheckState = False
                    }
                , trace
                )

duckdbContractOracleTimeout :: Int -> DuckDBContract -> Oracle
duckdbContractOracleTimeout micros contract candidate =
    observationOutcome <$> duckdbContractObservationTimeout micros contract candidate

duckdbContractOracle :: DuckDBContract -> Oracle
duckdbContractOracle = duckdbContractOracleTimeout defaultDuckDBCandidateTimeoutMicros

duckdbContractObservation :: DuckDBContract -> ByteString -> IO OracleObservation
duckdbContractObservation = duckdbContractObservationTimeout defaultDuckDBCandidateTimeoutMicros

duckdbContractObservationTimeout :: Int -> DuckDBContract -> ByteString -> IO OracleObservation
duckdbContractObservationTimeout micros contract candidate =
    fst <$> duckdbContractObservationWithTrace micros contract candidate

duckdbContractObservationWithResultStatement ::
    Int -> DuckDBContract -> ByteString -> IO (OracleObservation, Maybe ResultStatement)
duckdbContractObservationWithResultStatement micros contract candidate =
    second (fmap traceResultStatement) <$> duckdbContractObservationWithTrace micros contract candidate

duckdbContractObservationWithTrace ::
    Int -> DuckDBContract -> ByteString -> IO (OracleObservation, Maybe ResultTrace)
duckdbContractObservationWithTrace micros contract candidate
    -- TODO: Define a reproducible database-state observation before accepting state contracts.
    | contractCheckState contract || isJust (contractStateSha256 contract) =
        pure (OracleIOFailed "DuckDB state contracts are unsupported", Nothing)
    | otherwise = do
        res <- executeDuckDBResult micros candidate
        pure $ case res of
            Left DuckDBTimeout -> (OracleTimedOut, Nothing)
            Left (DuckDBExecutionError _) -> (OracleFinished (ExitFailure 1), Nothing)
            Left (DuckDBUnsupportedResultTypes types)
                | types /= contractExpectedTypes contract -> (OracleFinished (ExitFailure 1), Nothing)
                | otherwise -> (OracleIOFailed (unsupportedResultTypes types), Nothing)
            Left (DuckDBInfrastructureError err) -> (OracleIOFailed err, Nothing)
            Right observed
                | Just (result, trace) <- observed
                , result == expected ->
                    (OracleFinished ExitSuccess, Just trace)
                | otherwise -> (OracleFinished (ExitFailure 1), Nothing)
    where
        expected =
            ResultSet
                (contractExpectedColumns contract)
                (contractExpectedTypes contract)
                (contractExpectedRows contract)

unsupportedResultTypes :: [Int] -> Text
unsupportedResultTypes types = "Unsupported DuckDB result column types " <> T.pack (show types)
