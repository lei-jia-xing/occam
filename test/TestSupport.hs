module TestSupport (
    emptySemanticFacts,
    withSentinelPath,
    checkParserValidity,
) where

import Control.Exception (finally)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Occam.Core (SemanticFacts (..))
import Occam.Parser (
    ParsedSQL (..),
    SQLDialect,
    StatementUnit (..),
    defaultParseOptions,
    parseSQL,
 )
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)

emptySemanticFacts :: SemanticFacts
emptySemanticFacts = SemanticFacts [] [] [] [] []

-- | Create a path that does not exist yet and clean it up afterwards.
withSentinelPath :: (FilePath -> IO a) -> IO a
withSentinelPath action = do
    directory <- getTemporaryDirectory
    (path, handle) <- openTempFile directory "occam-cancel-sentinel"
    hClose handle
    removeFile path
    action path `finally` removeIfExists path
    where
        removeIfExists path = do
            exists <- doesFileExist path
            when exists (removeFile path)

checkParserValidity :: SQLDialect -> ByteString -> IO Bool
checkParserValidity dialect sql = do
    result <- parseSQL (defaultParseOptions dialect) sql
    pure $ case result of
        Left _ -> False
        Right parsed -> all validUnit (parsedUnits parsed)
    where
        validUnit unit =
            case unitParseError unit of
                Nothing -> True
                Just _ -> False
