-- | Atomic file replacement using temporary files.
module Occam.AtomicFile (
    writeAtomicBytes,
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
import System.IO (Handle, hClose, openBinaryTempFileWithDefaultPermissions)

isSpecialDevice :: FilePath -> Bool
isSpecialDevice path =
    map toLower path
        `elem` ["/dev/null", "/dev/stdout", "/dev/stderr", "nul", "\\\\.\\nul", "con", "conout$"]

writeAtomicBytes :: FilePath -> BS.ByteString -> IO ()
writeAtomicBytes path content
    | isSpecialDevice path = BS.writeFile path content
    | otherwise = writeAtomicWith path (`BS.hPut` content)

writeAtomicLazyBytes :: FilePath -> BSL.ByteString -> IO ()
writeAtomicLazyBytes path content
    | isSpecialDevice path = BSL.writeFile path content
    | otherwise = writeAtomicWith path (`BSL.hPut` content)

writeAtomicWith :: FilePath -> (Handle -> IO ()) -> IO ()
writeAtomicWith path write = mask \restore -> do
    let dir = takeDirectory path
    unless (null dir || dir == ".") (createDirectoryIfMissing True dir)
    (temporary, handle) <-
        openBinaryTempFileWithDefaultPermissions dir ("." ++ takeFileName path ++ ".tmp")
    (restore (write handle) >> hClose handle >> renameFile temporary path)
        `onException` (ignoreIOException (hClose handle) >> ignoreIOException (removeFile temporary))

ignoreIOException :: IO () -> IO ()
ignoreIOException action = action `catch` \(_ :: IOException) -> pure ()
