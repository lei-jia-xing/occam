{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Occam.Oracle.Sqlite.FFI (
    SqliteCache,
    SqliteConn (..),
    c_checkSql,
    c_hasNondetKeyword,
    c_occamSqliteBuildCkpts,
    c_occamSqliteCacheClearCkpts,
    c_occamSqliteNextResult,
    c_occamSqliteRunAndMatch,
    createSqliteConn,
    destroySqliteConn,
    dirtyConnCounter,
    dirtyDml,
    dirtyNondet,
    dirtyPragma,
    dirtyTemp,
    resetSqliteConn,
    splitStatements,
    withPooledSqliteConn,
    withSqliteDeadline,
) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (mask, onException, uninterruptibleMask_)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isSpace)
import Data.IORef (IORef, newIORef)
import Data.Int (Int64)
import Data.Word (Word8)
import Database.SQLite3 (close, exec, open)
import Database.SQLite3.Bindings (CDatabase, CStatement)
import Database.SQLite3.Direct qualified as Direct
import Foreign (
    FunPtr,
    Ptr,
    castPtr,
    free,
    malloc,
    nullFunPtr,
    nullPtr,
    poke,
 )
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..), CSize (..))
import GHC.Clock (getMonotonicTimeNSec)
import System.IO.Unsafe (unsafePerformIO)

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

foreign import ccall unsafe "sqlite3_complete"
    c_sqlite3_complete :: CString -> IO CInt

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

foreign import ccall unsafe "occam_sqlite_check_sql"
    c_checkSql :: Ptr Word8 -> CSize -> IO CInt

foreign import ccall unsafe "occam_sqlite_has_nondet_keyword"
    c_hasNondetKeyword :: Ptr Word8 -> CSize -> IO CInt

isCompleteStatement :: ByteString -> IO Bool
isCompleteStatement bs =
    BSC.useAsCString bs $ \cStr -> do
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
            if not complete
                then go rest buf' acc
                else
                    let trimmed = trimBS buf'
                        acc' = if BSC.null trimmed then acc else trimmed : acc
                     in go rest "" acc'

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
            returned <- uninterruptibleMask_ $ modifyMVar sharedSqlitePool $ \conns ->
                if length conns < maxPooledConns
                    then pure (conn : conns, True)
                    else pure (conns, False)
            unless returned (destroySqliteConn conn)

withPooledSqliteConn :: (SqliteConn -> IO (Bool, a)) -> IO a
withPooledSqliteConn action = mask $ \restore -> do
    conn <- acquireSqliteConn
    (keep, val) <- restore (action conn) `onException` destroySqliteConn conn
    releaseSqliteConn keep conn `onException` destroySqliteConn conn
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
