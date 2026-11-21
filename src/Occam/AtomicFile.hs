{- | Crash-safe file replacement: write a sibling temporary file, then rename
it over the target. The target is never observed half-written, and a failed
write attempts to remove its temporary file. Special devices (/dev/null, NUL,
etc.) bypass temporary file creation.
-}
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
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (createDirectoryIfMissing, removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName, (</>))

isSpecialDevice :: FilePath -> Bool
isSpecialDevice path =
    map toLower path
        `elem` ["/dev/null", "/dev/stdout", "/dev/stderr", "nul", "\\\\.\\nul", "con", "conout$"]

writeAtomicBytes :: FilePath -> BS.ByteString -> IO ()
writeAtomicBytes path content
    | isSpecialDevice path = BS.writeFile path content
    | otherwise = writeAtomicWith path (`BS.writeFile` content)

writeAtomicLazyBytes :: FilePath -> BSL.ByteString -> IO ()
writeAtomicLazyBytes path content
    | isSpecialDevice path = BSL.writeFile path content
    | otherwise = writeAtomicWith path (`BSL.writeFile` content)

writeAtomicWith :: FilePath -> (FilePath -> IO ()) -> IO ()
writeAtomicWith path write = mask \restore -> do
    let dir = takeDirectory path
    unless (null dir || dir == ".") (createDirectoryIfMissing True dir)
    temporary <- freshTemporaryPath path
    (restore (write temporary) >> renameFile temporary path)
        `onException` ignoreIOException (removeFile temporary)

freshTemporaryPath :: FilePath -> IO FilePath
freshTemporaryPath path = do
    now <- getMonotonicTimeNSec
    pure (takeDirectory path </> ("." ++ takeFileName path ++ ".tmp." ++ show now))

ignoreIOException :: IO () -> IO ()
ignoreIOException action = action `catch` \(_ :: IOException) -> pure ()
