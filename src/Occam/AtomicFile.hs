{-# LANGUAGE CPP #-}

-- | Atomic file replacement using temporary files.
module Occam.AtomicFile (
    writeAtomicBytes,
    writeAtomicBytesWithPermissionsOf,
    writeAtomicLazyBytes,
    isSpecialDevice,
) where

import Control.Exception (IOException, catch, mask, onException)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Char (toLower)
import System.Directory (createDirectoryIfMissing, removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (Handle, hClose, openBinaryTempFile, openBinaryTempFileWithDefaultPermissions)
#ifdef mingw32_HOST_OS
import System.Directory (copyPermissions)
#else
import Data.Bits ((.&.))
import System.Posix.Files (accessModes, fileMode, getFileStatus, setFileMode)
#endif

-- | Return 'True' for OS device pseudo-files such as @\/dev\/null@ or @NUL@.
isSpecialDevice :: FilePath -> Bool
isSpecialDevice path =
    map toLower path
        `elem` ["/dev/null", "/dev/stdout", "/dev/stderr", "nul", "\\\\.\\nul", "con", "conout$"]

-- | Atomically write strict bytes to @path@ via temp-file rename (direct write on OS special devices).
writeAtomicBytes :: FilePath -> BS.ByteString -> IO ()
writeAtomicBytes path content
    | isSpecialDevice path = BS.writeFile path content
    | otherwise = writeAtomicWith path (`BS.hPut` content)

-- | Write privately, then give the replacement the source file's permissions.
writeAtomicBytesWithPermissionsOf :: FilePath -> FilePath -> BS.ByteString -> IO ()
writeAtomicBytesWithPermissionsOf source path content =
    replaceAtomicFile openBinaryTempFile copyMode path (`BS.hPut` content)
    where
#ifdef mingw32_HOST_OS
    copyMode = copyPermissions source
#else
    copyMode temporary = do
        mode <- fileMode <$> getFileStatus source
        setFileMode temporary (mode .&. accessModes)
#endif

-- | Atomically write lazy bytes to @path@ via temp-file rename (direct write on OS special devices).
writeAtomicLazyBytes :: FilePath -> BSL.ByteString -> IO ()
writeAtomicLazyBytes path content
    | isSpecialDevice path = BSL.writeFile path content
    | otherwise = writeAtomicWith path (`BSL.hPut` content)

writeAtomicWith :: FilePath -> (Handle -> IO ()) -> IO ()
writeAtomicWith = replaceAtomicFile openBinaryTempFileWithDefaultPermissions (const (pure ()))

replaceAtomicFile ::
    (FilePath -> String -> IO (FilePath, Handle)) ->
    (FilePath -> IO ()) ->
    FilePath ->
    (Handle -> IO ()) ->
    IO ()
replaceAtomicFile openTemporary prepareReplacement path write = mask \restore -> do
    let dir = takeDirectory path
    unless (null dir || dir == ".") (createDirectoryIfMissing True dir)
    (temporary, handle) <-
        openTemporary dir ("." ++ takeFileName path ++ ".tmp")
    (restore (write handle) >> hClose handle >> prepareReplacement temporary >> renameFile temporary path)
        `onException` (ignoreIOException (hClose handle) >> ignoreIOException (removeFile temporary))

ignoreIOException :: IO () -> IO ()
ignoreIOException action = action `catch` \(_ :: IOException) -> pure ()
