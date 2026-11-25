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
    prepareDuckDBContractObserver,
    prepareDuckDBContractObserverWithReplay,
    isDuckDBAvailable,
    defaultDuckDBCandidateTimeoutMicros,
) where

import Control.Concurrent (
    forkIO,
    forkIOWithUnmask,
    killThread,
    modifyMVar,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    threadDelay,
    tryReadMVar,
 )
import Control.Exception qualified as Exception
import Control.Exception.Safe (SomeException, bracket, displayException, try)
import Control.Monad (filterM, forM, guard, when, zipWithM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT (..), evalContT)
import Data.Aeson (
    FromJSON (..),
    Result (Error, Success),
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
import Data.Bits (shiftL, shiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.Either (fromRight)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntSet qualified as IntSet
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Ratio ((%))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CBool (..), CInt (..), CSize (..))
import Foreign.Concurrent qualified as Foreign
import Foreign.ForeignPtr (ForeignPtr, finalizeForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes, callocBytes, free)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (fillBytes, with)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, castPtr, nullPtr)
import Foreign.Storable (Storable, peek, peekByteOff, peekElemOff, poke)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Float (castDoubleToWord64)
import Occam.Core (Oracle, ResultStatement (..), ResultTrace (..), traceResultStatement)
import Occam.Oracle (OracleObservation (..), observationOutcome)
import Occam.Oracle.Result (ResultSet (..))
import Occam.Parser (DuckDBReplayUnit (..), nativeDuckDBReplayUnits)
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

data DuckDBColumnType
    = ScalarColumn !CInt
    | DecimalColumn !Word8 !Word8 !CInt

-- Decimal contracts store the native tag in the low byte, width in the
-- next byte, and scale in the third byte. Scalar tags retain their format.
contractTypeTag :: DuckDBColumnType -> Int
contractTypeTag (ScalarColumn ty) = fromIntegral ty
contractTypeTag (DecimalColumn width scale _) =
    19 + (fromIntegral width `shiftL` 8) + (fromIntegral scale `shiftL` 16)

readColumnType :: DuckDBAPI -> Ptr DuckDBResult -> (CInt, IdxT) -> IO (Maybe DuckDBColumnType)
readColumnType api result (ty, column)
    | supportedType ty = pure (Just (ScalarColumn ty))
    | ty == 19
    , Just DuckDBDecimalAPI {..} <- apiDecimalAPI api = alloca $ \logicalPtr ->
        bracket
            (do logical <- apiColumnLogicalType result column; poke logicalPtr logical; pure logical)
            (const (apiDestroyLogicalType logicalPtr))
            $ \logical ->
                if logical == nullPtr
                    then pure Nothing
                    else do
                        width <- apiDecimalWidth logical
                        scale <- apiDecimalScale logical
                        storage <- apiDecimalStorage logical
                        pure $ do
                            guard (width > 0 && scale <= width && storage `elem` [3, 4, 5, 16])
                            pure (DecimalColumn width scale storage)
    | otherwise = pure Nothing

-- NOTE: DuckDB's legacy string accessors truncate at embedded NULs. Read
-- chunk vectors here, including for scalars, to preserve complete values.
convertCell :: DuckDBAPI -> (DuckDBColumnType, Ptr (), Ptr Word64) -> IdxT -> IO Value
convertCell api (colType, values, validity) row = do
    valid <- if validity == nullPtr then pure 1 else apiRowValid api validity row
    if valid == 0
        then pure Null
        else case colType of
            DecimalColumn _ scale storage -> do
                unscaled <- readDecimalStorage storage
                pure (Number (fromRational (unscaled % (10 ^ scale))))
            ScalarColumn ty -> readScalar ty
    where
        readDecimalStorage storage = case storage of
            3 -> toInteger <$> item @Int16
            4 -> toInteger <$> item @Int32
            5 -> toInteger <$> item @Int64
            _ -> wide signedHigh

        readScalar ty = case ty of
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
            16 -> Number . fromInteger <$> wide signedHigh
            17 -> do
                value <- readBytes
                either (fail . show) (pure . String) (TE.decodeUtf8' value)
            18 -> do
                value <- readBytes
                pure (object ["blob" .= BS.unpack value])
            32 -> Number . fromInteger <$> wide toInteger
            36 -> pure Null
            _ -> fail ("Unsupported DuckDB result type " ++ show ty)
        item :: (Storable a) => IO a
        item = peekElemOff (castPtr values) (fromIntegral row)
        integral :: forall a. (Integral a, Storable a) => IO Value
        integral = Number . fromIntegral <$> item @a
        floatBits value = object ["float64_bits" .= castDoubleToWord64 value]
        signedHigh = toInteger . (fromIntegral :: Word64 -> Int64)
        wide highInteger = do
            let offset = fromIntegral row * 16
            low <- peekByteOff values offset :: IO Word64
            high <- peekByteOff values (offset + 8) :: IO Word64
            pure ((highInteger high `shiftL` 64) + toInteger low)
        readBytes = evalContT do
            sizePtr <- ContT alloca
            liftIO do
                ptr <- vectorBytes (apiStringData api) values row sizePtr
                size <- peek sizePtr
                if size == 0 then pure BS.empty else BS.packCStringLen (ptr, fromIntegral size)

executeDuckDBCandidateDetailedTimeout :: Int -> ByteString -> IO (Either DuckDBError [[Value]])
executeDuckDBCandidateDetailedTimeout micros sql =
    fmap (maybe [] (resultRows . fst)) <$> executeDuckDBResult micros sql

executeDuckDBResult ::
    Int -> ByteString -> IO (Either DuckDBError (Maybe (ResultSet, ResultTrace)))
executeDuckDBResult = executeDuckDBResultWith observeRows

type ObserveResult a = DuckDBAPI -> Ptr DuckDBResult -> IO (Either DuckDBError a)

type ExecutionResult a = Either DuckDBError (Maybe (a, ResultTrace))

-- The callback receives the complete native statement count, the failing
-- ordinal and a native extraction check for a proposed prefix boundary.
type StatementFailure = Int -> Int -> (ByteString -> IO Bool) -> Text -> IO ()

ignoreStatementFailure :: StatementFailure
ignoreStatementFailure _ _ _ _ = pure ()

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
        Just (configureDb, db) -> run configureDb db
        Nothing ->
            bracket (openFreshDatabase api) (closeDatabase api) (run True)
    pure $ case res of
        Left (err :: SomeException) -> Left (DuckDBInfrastructureError (T.pack (displayException err)))
        Right inner -> inner
    where
        run configureDb db = evalContT do
            conPtr <- ContT alloca
            con <-
                ContT $
                    bracket
                        ( do
                            st <- apiConnect api db conPtr
                            if st /= 0
                                then fail "Failed to connect to in-memory DuckDB database"
                                else peek conPtr
                        )
                        (\_ -> apiDisconnect api conPtr)
            liftIO do
                mRes <- executeInterruptibly micros (apiInterrupt api con) do
                    let needsConfigure = configureDb && isNothing (apiConfigOpen api)
                    when needsConfigure (configure con)
                    runStatements con
                pure (fromMaybe (Left DuckDBTimeout) mRes)

        withExtracted ::
            DuckdbConnection ->
            ByteString ->
            (IdxT -> DuckdbExtractedStatements -> CString -> IO b) ->
            IO b
        withExtracted con bytes use = evalContT do
            sqlCStr <- ContT $ BSC.useAsCString bytes
            extPtr <- ContT alloca
            (n, ext) <-
                ContT $
                    bracket
                        ( do
                            num <- apiExtractStatements api con sqlCStr extPtr
                            extracted <- peek extPtr
                            pure (num, extracted)
                        )
                        (\_ -> apiDestroyExtracted api extPtr)
            liftIO do
                cErr <- apiExtractStatementsError api ext
                use n ext cErr

        runStatements con = withExtracted con sql $ \n ext cErr ->
            if cErr /= nullPtr
                then Left . DuckDBExecutionError . T.pack <$> peekCString cErr
                else
                    if n == 0
                        then pure (Right Nothing)
                        else executeExtractedStatements con ext n

        -- NOTE: In-memory connections can otherwise read or modify external files.
        configure con = evalContT do
            settings <- ContT $ withCString "SET enable_external_access = false; SET lock_configuration = true;"
            status <- ContT $ \use -> withResult (apiQuery api con settings) (\s _ -> use s)
            liftIO $ when (status /= 0) (fail "Could not disable and lock DuckDB external access")

        withResult :: (Ptr DuckDBResult -> IO CInt) -> (CInt -> Ptr DuckDBResult -> IO a) -> IO a
        withResult execute use = do
            size <- resultSize
            evalContT do
                ptr <- ContT $ allocaBytes (fromIntegral size)
                status <- ContT $ bracket (execute ptr) (const (apiDestroyResult api ptr))
                liftIO $ use status ptr

        executeExtractedStatements con ext 1 = evalContT do
            prepPtr <- ContT alloca
            size <- liftIO $ fromIntegral <$> resultSize
            current <- ContT $ bracket (callocBytes size) (\ptr -> apiDestroyResult api ptr >> free ptr)
            status <-
                ContT $
                    bracket
                        (apiPrepareExtractedStatement api con ext 0 prepPtr)
                        (const (apiDestroyPrepare api prepPtr))
            liftIO do
                prep <- peek prepPtr
                executed <-
                    if status /= 0
                        then Left . DuckDBExecutionError <$> errorText "Prepare error" (apiPrepareError api prep)
                        else do
                            execStatus <- apiExecutePrepared api prep current
                            if execStatus /= 0
                                then Left . DuckDBExecutionError <$> errorText "Execution error" (apiResultError api current)
                                else Right <$> resultKind (apiResultKind api) current
                case executed of
                    Left err@(DuckDBExecutionError message) -> do
                        let exactBoundary prefix =
                                withExtracted con prefix $ \count _ errPtr ->
                                    pure (count == 1 && errPtr == nullPtr)
                        onFailure 1 0 exactBoundary message
                        pure (Left err)
                    Left err -> pure (Left err)
                    Right kind
                        | kind == 3 -> do
                            result <- observe api current
                            let trace =
                                    ResultTrace
                                        { traceStatementCount = 1
                                        , traceFirstResultIndex = 0
                                        , traceLastResultIndex = 0
                                        , traceResultIndices = IntSet.singleton 0
                                        , tracePrefixMatchIndices = IntSet.empty
                                        , traceDeterministic = False
                                        , traceEndByteOffsets = Nothing
                                        }
                            pure (fmap (\rows -> Just (rows, trace)) result)
                        | otherwise -> pure (Right Nothing)
        executeExtractedStatements con ext n = evalContT do
            prepPtr <- ContT alloca
            size <- liftIO $ fromIntegral <$> resultSize
            liftIO do
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
                                Left err@(DuckDBExecutionError message) -> do
                                    let exactBoundary prefix =
                                            withExtracted con prefix $ \count _ errPtr ->
                                                pure (count == i + 1 && errPtr == nullPtr)
                                    onFailure (fromIntegral n) (fromIntegral i) exactBoundary message
                                    pure (Left err)
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

readResultColumns :: DuckDBAPI -> Ptr DuckDBResult -> IO ([Text], [CInt])
readResultColumns api ptr = do
    count <- apiColumnCount api ptr
    let columns = takeWhile (< count) [0 ..]
    names <- forM columns $ \col -> do
        name <- apiColumnName api ptr col
        if name == nullPtr
            then fail "DuckDB omitted a result column name"
            else TE.decodeUtf8 <$> BS.packCString name
    types <- mapM (apiColumnType api ptr) columns
    pure (names, types)

supportedType :: (Integral a) => a -> Bool
supportedType ty = (ty >= 1 && ty <= 11) || ty `elem` [16, 17, 18, 32, 36]

observeRows :: DuckDBAPI -> Ptr DuckDBResult -> IO (Either DuckDBError ResultSet)
observeRows api ptr = do
    (names, types) <- readResultColumns api ptr
    descriptors <- traverse (readColumnType api ptr) (zip types [0 ..])
    case sequence descriptors of
        Nothing -> pure (Left (DuckDBUnsupportedResultTypes (map fromIntegral types)))
        Just columnTypes -> do
            rows <- readChunks (zip columnTypes [0 ..]) []
            pure (Right (ResultSet names (map contractTypeTag columnTypes) rows))
    where
        readChunks columnTypes accumulated = do
            chunkRows <- evalContT do
                chunkPtr <- ContT alloca
                chunk <-
                    ContT $
                        bracket
                            (do chunk <- fetchChunk (apiFetchChunk api) ptr; poke chunkPtr chunk; pure chunk)
                            (\c -> when (c /= nullPtr) (mkDestroyChunk (apiDestroyChunk api) chunkPtr))
                if chunk == nullPtr
                    then pure Nothing
                    else liftIO do
                        size <- mkChunkSize (apiChunkSize api) chunk
                        vectors <- forM columnTypes $ \(ty, col) -> do
                            vector <- mkChunkVector (apiChunkVector api) chunk col
                            values <- mkVectorData (apiVectorData api) vector
                            validity <- mkValidity (apiValidity api) vector
                            pure (ty, values, validity)
                        Just
                            <$> forM (takeWhile (< size) [0 ..]) (\row -> mapM (\vector -> convertCell api vector row) vectors)
            case chunkRows of
                Nothing -> pure (concat (reverse accumulated))
                Just rows -> readChunks columnTypes (rows : accumulated)

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
duckdbContractObservationWithTrace = prepareDuckDBContractObserver

{- | Encode the expected values once. Candidates compare directly against DuckDB
chunk vectors, including full strings, BLOBs, and exact floating-point bits.
-}
prepareDuckDBContractObserver ::
    Int -> DuckDBContract -> ByteString -> IO (OracleObservation, Maybe ResultTrace)
prepareDuckDBContractObserver micros contract =
    preparedObserver contract (`executeDuckDBResultWith` micros)

{- | The replay checkpoint owns an immutable, configured database. Every
candidate still gets its own connection, and only statically read-only
queries over the checkpoint's tables can use that database.
-}
prepareDuckDBContractObserverWithReplay ::
    Int ->
    DuckDBContract ->
    ByteString ->
    (ByteString -> IO ()) ->
    IO (ByteString -> IO (OracleObservation, Maybe ResultTrace))
prepareDuckDBContractObserverWithReplay micros contract baseline reportFailure = do
    baselineUnits <- nativeDuckDBReplayUnits baseline
    let (setup, suffix) = span isSetup baselineUnits
        prefixCount = length setup
        tables =
            Set.fromList
                [table | unit <- setup, replayUnitKind unit == "create", Just table <- [replayUnitTable unit]]
        readOnly unit =
            replayUnitKind unit == "query" && maybe True (`Set.member` tables) (replayUnitTable unit)
        prefix
            | prefixCount >= 8
            , all readOnly suffix
            , lastSetup : _ <- reverse setup
            , Just end <- replayUnitEnd lastSetup =
                BS.take end baseline
            | otherwise = BS.empty
    saved <- newMVar Nothing
    pure $ preparedObserver contract $ \readMatch candidate -> do
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
        if BS.null prefix || not (prefix `BS.isPrefixOf` candidate)
            then fresh
            else do
                tailUnits <- nativeDuckDBReplayUnits tailSQL
                if null tailUnits || not (all readOnly tailUnits)
                    then fresh
                    else do
                        mCurrent <- tryReadMVar saved
                        checkpoint <- case mCurrent of
                            Just loaded@(Just _) -> pure loaded
                            _ -> modifyMVar saved $ \current -> case current of
                                Nothing -> do
                                    budget <- remaining
                                    loaded <- buildReplayCheckpoint prefix budget
                                    pure (loaded, loaded)
                                Just _ -> pure (current, current)
                        case checkpoint of
                            Nothing -> fresh
                            Just db -> withForeignPtr db $ \database -> do
                                budget <- remaining
                                result <-
                                    executeDuckDBResultWithReplay
                                        readMatch
                                        (Just (False, database))
                                        ( \count index verify ->
                                            captureFailure
                                                reportFailure
                                                candidate
                                                (count + prefixCount)
                                                (index + prefixCount)
                                                (verify . BS.drop (BS.length prefix))
                                        )
                                        budget
                                        tailSQL
                                pure (fmap (fmap (second (shiftTrace prefixCount))) result)
    where
        isSetup unit =
            replayUnitKind unit == "create" || replayUnitKind unit == "insert"
        shiftTrace offset trace =
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
captureFailure record candidate count index verify message =
    when
        ( any
            (`T.isPrefixOf` message)
            ["Catalog Error:", "Binder Error:", "Conversion Error:", "Out of Range Error:"]
        )
        $ do
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

preparedObserver ::
    DuckDBContract ->
    (ObserveResult Bool -> ByteString -> IO (ExecutionResult Bool)) ->
    ByteString ->
    IO (OracleObservation, Maybe ResultTrace)
preparedObserver contract run
    -- TODO: Define a reproducible database-state observation before accepting state contracts.
    | contractCheckState contract || isJust (contractStateSha256 contract) =
        const (pure (OracleIOFailed "DuckDB state contracts are unsupported", Nothing))
    | otherwise = observe
    where
        expected = ResultSet columns types (contractExpectedRows contract)
        columns = contractExpectedColumns contract
        types = contractExpectedTypes contract
        encoded = encodeExpectedRows expected
        readMatch = case encoded of
            Just bytes -> matchResult (map fromIntegral types) columns bytes
            Nothing -> \api ptr -> fmap (== expected) <$> observeRows api ptr
        observe candidate = do
            res <- run readMatch candidate
            pure $ case res of
                Left DuckDBTimeout -> (OracleTimedOut, Nothing)
                Left (DuckDBExecutionError _) -> (OracleFinished (ExitFailure 1), Nothing)
                Left (DuckDBUnsupportedResultTypes actualTypes)
                    | actualTypes /= types -> (OracleFinished (ExitFailure 1), Nothing)
                    | otherwise -> (OracleIOFailed (unsupportedResultTypes actualTypes), Nothing)
                Left (DuckDBInfrastructureError err) -> (OracleIOFailed err, Nothing)
                Right (Just (True, trace)) -> (OracleFinished ExitSuccess, Just trace)
                Right _ -> (OracleFinished (ExitFailure 1), Nothing)

matchResult ::
    [CInt] -> [Text] -> ByteString -> DuckDBAPI -> Ptr DuckDBResult -> IO (Either DuckDBError Bool)
matchResult types names expected api ptr = do
    (actualNames, actualTypes) <- readResultColumns api ptr
    if not (all supportedType actualTypes)
        then pure (Left (DuckDBUnsupportedResultTypes (map fromIntegral actualTypes)))
        else
            if actualTypes /= types || actualNames /= names
                then pure (Right False)
                else evalContT do
                    (count, columnTypes) <- ContT $ \use -> withArrayLen types (curry use)
                    (buf, size) <- ContT $ BSU.unsafeUseAsCStringLen expected
                    liftIO do
                        result <-
                            matchRows
                                (apiFetchChunk api)
                                (apiDestroyChunk api)
                                (apiChunkSize api)
                                (apiChunkVector api)
                                (apiVectorData api)
                                (apiValidity api)
                                (apiStringData api)
                                ptr
                                columnTypes
                                (fromIntegral count)
                                buf
                                (fromIntegral size)
                        pure $ case result of
                            -1 -> Left (DuckDBInfrastructureError "Could not allocate DuckDB result matcher")
                            _ -> Right (result == 1)

encodeExpectedRows :: ResultSet -> Maybe ByteString
encodeExpectedRows (ResultSet columns types rows)
    | length types /= length columns || not (all supportedType types) = Nothing
    | otherwise = do
        encoded <- traverse encodeRow rows
        pure
            (BL.toStrict (BB.toLazyByteString (BB.word64LE (fromIntegral (length rows)) <> mconcat encoded)))
    where
        encodeRow row
            | length row == length types = mconcat <$> zipWithM encodeCell types row
            | otherwise = Nothing
        encodeCell _ Null = Just (BB.word8 1)
        encodeCell ty value = (BB.word8 0 <>) <$> encodeValue ty value
        encodeValue 1 (Bool value) = Just (BB.word64LE (if value then 1 else 0))
        encodeValue ty value@Number {}
            | ty >= 2 && ty <= 5 = BB.int64LE <$> decodeValue value
            | ty >= 6 && ty <= 9 = BB.word64LE <$> decodeValue value
            | ty == 16 && valueInRange (-(2 ^ (127 :: Int))) (2 ^ (127 :: Int)) value =
                wide <$> decodeValue value
            | ty == 32 && valueInRange 0 (2 ^ (128 :: Int)) value = wide <$> decodeValue value
        encodeValue ty (Object fields)
            | KM.size fields == 1
            , ty == 10 || ty == 11 =
                BB.word64LE <$> (KM.lookup "float64_bits" fields >>= decodeValue)
            | KM.size fields == 1
            , ty == 18 =
                encodeBytes . BS.pack <$> (KM.lookup "blob" fields >>= decodeValue)
        encodeValue 17 (String value) = Just (encodeBytes (TE.encodeUtf8 value))
        encodeValue _ _ = Nothing
        encodeBytes bytes = BB.word64LE (fromIntegral (BS.length bytes)) <> BB.byteString bytes
        wide (value :: Integer) = BB.word64LE (fromIntegral value) <> BB.word64LE (fromIntegral (value `shiftR` 64))
        valueInRange lo hi (Number n) = lo <= n && n < hi
        valueInRange _ _ _ = False
        decodeValue :: (FromJSON a) => Value -> Maybe a
        decodeValue value = case fromJSON value of
            Success x -> Just x
            Error _ -> Nothing

unsupportedResultTypes :: [Int] -> Text
unsupportedResultTypes types = "Unsupported DuckDB result column types " <> T.pack (show types)
