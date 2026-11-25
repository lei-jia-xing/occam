module TestSupport (
    emptySemanticFacts,
    withSentinelPath,
    checkParserValidity,
    testPredicatePool,
    testCountingPool,
    boolObservation,
) where

import Control.Exception (finally)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Occam.Core (SemanticFacts (..))
import Occam.Oracle (OracleObservation (..), OracleObservationDetails)
import Occam.Parser (
    ParsedSQL (..),
    SQLDialect,
    StatementUnit (..),
    defaultParseOptions,
    parseSQL,
 )
import Occam.Pool (OraclePool, PoolConfig (..), newOraclePool, predicateDetails)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
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

-- | Create a test oracle pool backed by an in-process predicate.
testPredicatePool :: (ByteString -> IO Bool) -> IO OraclePool
testPredicatePool predicate =
    newOraclePool
        PoolConfig
            { poolJobs = 1
            , poolCacheEnabled = False
            , poolPermits = Nothing
            , poolPrecomputed = Map.empty
            , poolMetrics = Nothing
            , poolExecute = predicateDetails predicate
            , poolBill = const (pure ())
            , poolRetire = \_ _ _ _ -> pure ()
            , poolReportCount = Nothing
            }

testCountingPool :: Int -> (ByteString -> IO OracleObservationDetails) -> IO OraclePool
testCountingPool jobs execute =
    newOraclePool
        PoolConfig
            { poolJobs = jobs
            , poolCacheEnabled = True
            , poolPermits = Nothing
            , poolPrecomputed = Map.empty
            , poolMetrics = Nothing
            , poolExecute = execute
            , poolBill = const (pure ())
            , poolRetire = \_ _ _ _ -> pure ()
            , poolReportCount = Nothing
            }

boolObservation :: Bool -> OracleObservation
boolObservation accepted =
    if accepted then OracleFinished ExitSuccess else OracleFinished (ExitFailure 1)
