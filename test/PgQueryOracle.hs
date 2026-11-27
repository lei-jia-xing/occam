{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Test-only dynamic syntax oracle backed by @libpg_query@ (PostgreSQL 18).
module PgQueryOracle (
    PgSyntaxError (..),
    isPgQueryAvailable,
    pgQuerySplitCheck,
    pgQueryParseCheck,
) where

import Control.Applicative ((<|>))
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception.Safe (SomeException, displayException, try)
import Control.Monad (filterM, guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr)
import Foreign.Storable (peek)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

#if defined(mingw32_HOST_OS)
import Foreign.C.String (withCString)
import Foreign.Ptr (nullFunPtr, nullPtr)

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

data PgSyntaxError = PgSyntaxError
    { pgErrMessage :: !Text
    , pgErrCursorPos :: !Int
    }
    deriving (Eq, Show)

-- Phantom tags for by-value C function pointers invoked inside pg_query_shim.c.
data FnSplitWithParser
data FnFreeSplitResult
data FnParse
data FnFreeParseResult

foreign import ccall safe "occam_pg_query_split_check"
    c_pg_query_split_check ::
        FunPtr FnSplitWithParser ->
        FunPtr FnFreeSplitResult ->
        CString ->
        Ptr CInt ->
        Ptr CInt ->
        CString ->
        CSize ->
        IO CInt

foreign import ccall safe "occam_pg_query_parse_check"
    c_pg_query_parse_check ::
        FunPtr FnParse ->
        FunPtr FnFreeParseResult ->
        CString ->
        Ptr CInt ->
        CString ->
        CSize ->
        IO CInt

data PgQueryAPI = PgQueryAPI
    { apiSplitWithParser :: !(FunPtr FnSplitWithParser)
    , apiFreeSplitResult :: !(FunPtr FnFreeSplitResult)
    , apiParse :: !(FunPtr FnParse)
    , apiFreeParseResult :: !(FunPtr FnFreeParseResult)
    }

{-# NOINLINE pgQueryApiCache #-}
pgQueryApiCache :: IORef (Maybe (Either String PgQueryAPI))
pgQueryApiCache = unsafePerformIO (newIORef Nothing)

{-# NOINLINE pgQueryLock #-}
pgQueryLock :: MVar ()
pgQueryLock = unsafePerformIO (newMVar ())

errBufSize :: Int
errBufSize = 2048

findPgQueryLibPath :: IO (Maybe FilePath)
findPgQueryLibPath =
    runMaybeT $
        MaybeT (lookupEnvNonEmpty "OCCAM_PG_QUERY_LIB")
            <|> MaybeT (lookupEnvNonEmpty "PG_QUERY_LIB_PATH")
            <|> MaybeT findSystemPgQueryLib
    where
        lookupEnvNonEmpty name = do
            mVal <- lookupEnv name
            pure $ case mVal of
                Just v | not (null v) -> Just v
                _ -> Nothing

findSystemPgQueryLib :: IO (Maybe FilePath)
#if defined(mingw32_HOST_OS)
findSystemPgQueryLib = pure (Just "pg_query.dll")
#elif defined(darwin_HOST_OS)
findSystemPgQueryLib =
    runMaybeT $
        tryBareLib "libpg_query.dylib"
            <|> findInNixStore "libpg_query.dylib"
#else
findSystemPgQueryLib =
    runMaybeT $
        tryBareLib "libpg_query.so"
            <|> tryBareLib "libpg_query.so.1"
            <|> findInNixStore "libpg_query.so"
#endif

#if !defined(mingw32_HOST_OS)
tryBareLib :: FilePath -> MaybeT IO FilePath
tryBareLib libName = MaybeT do
    mBare <- try (dlopen libName [RTLD_LAZY]) :: IO (Either SomeException DL)
    case mBare of
        Right dl -> dlclose dl >> pure (Just libName)
        Left _ -> pure Nothing

findInNixStore :: FilePath -> MaybeT IO FilePath
findInNixStore soname = do
    let nixStore = "/nix/store"
    guard =<< liftIO (doesDirectoryExist nixStore)
    entries <- liftIO (listDirectory nixStore)
    let matches =
            sortOn Down
                [ nixStore </> e </> "lib" </> soname
                | e <- entries
                , "-libpg_query-" `isInfixOf` e
                ]
    MaybeT $
        filterM doesFileExist matches >>= \case
            f : _ -> pure (Just f)
            [] -> pure Nothing
#endif

loadPgQueryAPI :: IO (Either String PgQueryAPI)
loadPgQueryAPI = do
    mLibPath <- findPgQueryLibPath
    case mLibPath of
        Nothing ->
            pure
                ( Left
                    "Could not locate libpg_query. Set OCCAM_PG_QUERY_LIB to its path."
                )
        Just libPath -> do
            res <- try (loadSymbols libPath) :: IO (Either SomeException PgQueryAPI)
            pure $ case res of
                Left err -> Left (displayException err)
                Right api -> Right api

#if defined(mingw32_HOST_OS)
loadSymbols :: FilePath -> IO PgQueryAPI
loadSymbols libPath = withCString libPath \cLib -> do
    hMod <- c_LoadLibrary cLib
    if hMod == nullPtr
        then fail ("LoadLibrary failed for " <> libPath)
        else do
            let getProc :: String -> IO (FunPtr a)
                getProc name = withCString name \cName -> do
                    p <- c_GetProcAddress hMod cName
                    if p == nullFunPtr
                        then fail ("GetProcAddress failed for symbol: " <> name)
                        else pure (castFunPtr p)
            apiSplitWithParser <- getProc "pg_query_split_with_parser"
            apiFreeSplitResult <- getProc "pg_query_free_split_result"
            apiParse <- getProc "pg_query_parse"
            apiFreeParseResult <- getProc "pg_query_free_parse_result"
            pure PgQueryAPI {..}
#else
loadSymbols :: FilePath -> IO PgQueryAPI
loadSymbols libPath = do
    dl <- dlopen libPath [RTLD_LAZY]
    let getSym :: String -> IO (FunPtr a)
        getSym name = castFunPtr <$> dlsym dl name
    apiSplitWithParser <- getSym "pg_query_split_with_parser"
    apiFreeSplitResult <- getSym "pg_query_free_split_result"
    apiParse <- getSym "pg_query_parse"
    apiFreeParseResult <- getSym "pg_query_free_parse_result"
    pure PgQueryAPI {..}
#endif

getPgQueryAPI :: IO (Either String PgQueryAPI)
getPgQueryAPI =
    withMVar pgQueryLock \_ -> do
        cached <- readIORef pgQueryApiCache
        case cached of
            Just res -> pure res
            Nothing -> do
                res <- loadPgQueryAPI
                writeIORef pgQueryApiCache (Just res)
                pure res

isPgQueryAvailable :: IO Bool
isPgQueryAvailable = do
    res <- getPgQueryAPI
    pure $ case res of
        Right _ -> True
        Left _ -> False

requirePgQueryAPI :: IO PgQueryAPI
requirePgQueryAPI =
    getPgQueryAPI >>= \case
        Right api -> pure api
        Left msg -> ioError (userError ("PgQueryOracle unavailable: " <> msg))

pgQuerySplitCheck :: ByteString -> IO (Either PgSyntaxError Int)
pgQuerySplitCheck sql
    | BS.elem 0 sql =
        pure (Left (PgSyntaxError "NUL byte in SQL input" 0))
    | otherwise = do
        api <- requirePgQueryAPI
        withMVar pgQueryLock \_ ->
            BSC.useAsCString sql \cSql ->
                alloca \nStmtsPtr ->
                    alloca \cursorPosPtr ->
                        allocaBytes errBufSize \errBuf -> do
                            status <-
                                c_pg_query_split_check
                                    (apiSplitWithParser api)
                                    (apiFreeSplitResult api)
                                    cSql
                                    nStmtsPtr
                                    cursorPosPtr
                                    errBuf
                                    (fromIntegral errBufSize)
                            if status == 0
                                then Right . fromIntegral <$> peek nStmtsPtr
                                else do
                                    msg <- T.pack <$> peekCString errBuf
                                    pos <- fromIntegral <$> peek cursorPosPtr
                                    pure (Left (PgSyntaxError msg pos))

pgQueryParseCheck :: ByteString -> IO (Either PgSyntaxError ())
pgQueryParseCheck sql
    | BS.elem 0 sql =
        pure (Left (PgSyntaxError "NUL byte in SQL input" 0))
    | otherwise = do
        api <- requirePgQueryAPI
        withMVar pgQueryLock \_ ->
            BSC.useAsCString sql \cSql ->
                alloca \cursorPosPtr ->
                    allocaBytes errBufSize \errBuf -> do
                        status <-
                            c_pg_query_parse_check
                                (apiParse api)
                                (apiFreeParseResult api)
                                cSql
                                cursorPosPtr
                                errBuf
                                (fromIntegral errBufSize)
                        if status == 0
                            then pure (Right ())
                            else do
                                msg <- T.pack <$> peekCString errBuf
                                pos <- fromIntegral <$> peek cursorPosPtr
                                pure (Left (PgSyntaxError msg pos))
