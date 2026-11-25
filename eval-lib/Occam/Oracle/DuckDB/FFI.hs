{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Occam.Oracle.DuckDB.FFI (
    DuckDBAPI (..),
    DuckDBDecimalAPI (..),
    DuckDBError (..),
    DuckDBResult,
    DuckdbConnection,
    DuckdbDatabase,
    DuckdbExtractedStatements,
    DuckdbPreparedStatement,
    IdxT,
    closeDatabase,
    duckdbErrorText,
    fetchChunk,
    getDuckDBAPI,
    isDuckDBAvailable,
    matchRows,
    mkChunkSize,
    mkChunkVector,
    mkDestroyChunk,
    mkValidity,
    mkVectorData,
    openFreshDatabase,
    resultKind,
    resultSize,
    unsupportedResultTypes,
    vectorBytes,
) where

import Control.Applicative ((<|>))
import Control.Exception (bracket)
import Control.Exception.Safe (SomeException, displayException, try)
import Control.Monad (filterM, guard, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT (..), evalContT)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64, Word8)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CBool (..), CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, nullPtr)
import Foreign.Storable (peek)
import System.Directory (doesDirectoryExist, doesFileExist, findExecutable, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

#if defined(mingw32_HOST_OS)
import Foreign.Ptr (nullFunPtr)

foreign import ccall "windows.h LoadLibraryA"
    c_LoadLibrary :: CString -> IO (Ptr ())
foreign import ccall "windows.h GetProcAddress"
    c_GetProcAddress :: Ptr () -> CString -> IO (FunPtr ())
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
type DuckdbLogicalType = Ptr ()
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

foreign import ccall safe "occam_duckdb_match_rows"
    matchRows ::
        FunPtr FnFetchChunk ->
        FunPtr FnDestroyChunk ->
        FunPtr FnChunkSize ->
        FunPtr FnChunkVector ->
        FunPtr FnVectorData ->
        FunPtr FnValidity ->
        FunPtr FnStringData ->
        Ptr DuckDBResult ->
        Ptr CInt ->
        CSize ->
        CString ->
        CSize ->
        IO CInt

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
type FnColumnLogicalType = Ptr DuckDBResult -> IdxT -> IO DuckdbLogicalType
type FnDecimalDimension = DuckdbLogicalType -> IO Word8
type FnDecimalStorage = DuckdbLogicalType -> IO CInt
type FnDestroyLogicalType = Ptr DuckdbLogicalType -> IO ()
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
foreign import ccall "dynamic"
    mkColumnLogicalType :: FunPtr FnColumnLogicalType -> FnColumnLogicalType
foreign import ccall "dynamic" mkDecimalDimension :: FunPtr FnDecimalDimension -> FnDecimalDimension
foreign import ccall "dynamic" mkDecimalStorage :: FunPtr FnDecimalStorage -> FnDecimalStorage
foreign import ccall "dynamic"
    mkDestroyLogicalType :: FunPtr FnDestroyLogicalType -> FnDestroyLogicalType
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

data DuckDBDecimalAPI = DuckDBDecimalAPI
    { apiColumnLogicalType :: !FnColumnLogicalType
    , apiDecimalWidth :: !FnDecimalDimension
    , apiDecimalScale :: !FnDecimalDimension
    , apiDecimalStorage :: !FnDecimalStorage
    , apiDestroyLogicalType :: !FnDestroyLogicalType
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
    , apiDecimalAPI :: !(Maybe DuckDBDecimalAPI)
    , apiFetchChunk :: !(FunPtr FnFetchChunk)
    , apiDestroyChunk :: !(FunPtr FnDestroyChunk)
    , apiChunkSize :: !(FunPtr FnChunkSize)
    , apiChunkVector :: !(FunPtr FnChunkVector)
    , apiVectorData :: !(FunPtr FnVectorData)
    , apiValidity :: !(FunPtr FnValidity)
    , apiRowValid :: !FnRowValid
    , apiStringData :: !(FunPtr FnStringData)
    }

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
findDuckDBLibPath =
    runMaybeT $
        MaybeT (lookupEnv "OCCAM_DUCKDB_LIB")
            <|> MaybeT (lookupEnv "DUCKDB_LIB_PATH")
            <|> MaybeT findSystemDuckDBLib

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
findSystemDuckDBLib =
    runMaybeT $
        tryBareLib
            <|> findInNixStore
            <|> findByExecutable
  where
    tryBareLib = MaybeT do
        mBare <- try (dlopen "libduckdb.so" [RTLD_LAZY]) :: IO (Either SomeException DL)
        case mBare of
            Right dl -> dlclose dl >> pure (Just "libduckdb.so")
            Left _ -> pure Nothing
    findInNixStore = do
        let nixStore = "/nix/store"
        guard =<< liftIO (doesDirectoryExist nixStore)
        entries <- liftIO (listDirectory nixStore)
        let matches =
                [ nixStore </> e </> "lib" </> "libduckdb.so"
                | e <- entries
                , "duckdb" `elem` words (map (\c -> if c == '-' then ' ' else c) e)
                , "lib" `elem` words (map (\c -> if c == '-' then ' ' else c) e)
                ]
        MaybeT $
            filterM doesFileExist matches >>= \case
                f : _ -> pure (Just f)
                [] -> pure Nothing
    findByExecutable = do
        exePath <- MaybeT (findExecutable "duckdb")
        let candidate = exePath </> ".." </> ".." </> "lib" </> "libduckdb.so"
        guard =<< liftIO (doesFileExist candidate)
        pure candidate
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
            apiDecimalAPI <- do
                logical <- tryProc "duckdb_column_logical_type" mkColumnLogicalType
                width <- tryProc "duckdb_decimal_width" mkDecimalDimension
                scale <- tryProc "duckdb_decimal_scale" mkDecimalDimension
                storage <- tryProc "duckdb_decimal_internal_type" mkDecimalStorage
                destroy <- tryProc "duckdb_destroy_logical_type" mkDestroyLogicalType
                pure (DuckDBDecimalAPI <$> logical <*> width <*> scale <*> storage <*> destroy)
            apiFetchChunk <- getProc "duckdb_fetch_chunk" id
            apiDestroyChunk <- getProc "duckdb_destroy_data_chunk" id
            apiChunkSize <- getProc "duckdb_data_chunk_get_size" id
            apiChunkVector <- getProc "duckdb_data_chunk_get_vector" id
            apiVectorData <- getProc "duckdb_vector_get_data" id
            apiValidity <- getProc "duckdb_vector_get_validity" id
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
    apiDecimalAPI <- do
        logical <- trySym "duckdb_column_logical_type" mkColumnLogicalType
        width <- trySym "duckdb_decimal_width" mkDecimalDimension
        scale <- trySym "duckdb_decimal_scale" mkDecimalDimension
        storage <- trySym "duckdb_decimal_internal_type" mkDecimalStorage
        destroy <- trySym "duckdb_destroy_logical_type" mkDestroyLogicalType
        pure (DuckDBDecimalAPI <$> logical <*> width <*> scale <*> storage <*> destroy)
    apiFetchChunk <- getSym "duckdb_fetch_chunk" id
    apiDestroyChunk <- getSym "duckdb_destroy_data_chunk" id
    apiChunkSize <- getSym "duckdb_data_chunk_get_size" id
    apiChunkVector <- getSym "duckdb_data_chunk_get_vector" id
    apiVectorData <- getSym "duckdb_vector_get_data" id
    apiValidity <- getSym "duckdb_vector_get_validity" id
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

openMemoryDb :: DuckDBAPI -> Ptr DuckdbDatabase -> IO CInt
openMemoryDb api dbPtr = case apiConfigOpen api of
    Nothing -> apiOpen api nullPtr dbPtr
    Just DuckDBConfigOpen {..} -> evalContT do
        cfgPtr <- ContT alloca
        cfg <-
            ContT $
                bracket
                    ( do
                        st <- apiCreateConfig cfgPtr
                        if st /= 0 then pure nullPtr else peek cfgPtr
                    )
                    (\c -> when (c /= nullPtr) (apiDestroyConfig cfgPtr))
        if cfg == nullPtr
            then liftIO $ apiOpen api nullPtr dbPtr
            else do
                let setOption key val = do
                        k <- ContT $ withCString key
                        v <- ContT $ withCString val
                        liftIO $ apiSetConfig cfg k v
                _ <- setOption "threads" "1"
                _ <- setOption "enable_external_access" "false"
                _ <- setOption "lock_configuration" "true"
                liftIO $ apiOpenExt nullPtr dbPtr cfg nullPtr

duckdbErrorText :: Text -> IO CString -> IO Text
duckdbErrorText fallback action = do
    message <- action
    if message == nullPtr then pure fallback else T.pack <$> peekCString message

openFreshDatabase :: DuckDBAPI -> IO DuckdbDatabase
openFreshDatabase api = evalContT do
    dbPtr <- ContT alloca
    liftIO do
        st <- openMemoryDb api dbPtr
        if st /= 0
            then fail "Failed to initialize in-memory DuckDB database"
            else peek dbPtr

closeDatabase :: DuckDBAPI -> DuckdbDatabase -> IO ()
closeDatabase api db = with db (apiClose api)

unsupportedResultTypes :: [Int] -> Text
unsupportedResultTypes types = "Unsupported DuckDB result column types " <> T.pack (show types)
