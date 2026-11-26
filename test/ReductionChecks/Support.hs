module ReductionChecks.Support (
    countOrFail,
    predicatePool,
    outcomePool,
    jobsPool,
    testPoolWith,
    syntaxOnly,
) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Occam.Core (OracleOutcome (..))
import Occam.Logger (nullLogger)
import Occam.Oracle (OracleObservation (..))
import Occam.Parser (SQLDialect, countStatements)
import Occam.Pool (OraclePool, PoolConfig (..), newOraclePool, observationDetails)
import Occam.Reducer (ReductionConfig (..), defaultReductionConfigFor, reduceSQLWithConfig)
import System.Exit (ExitCode (..))
import Test.Tasty.HUnit (assertFailure)

countOrFail :: SQLDialect -> ByteString -> IO Int
countOrFail dialect sql = do
    result <- countStatements dialect sql
    case result of
        Left err -> assertFailure ("SQL parser transport error: " ++ show err)
        Right count -> pure count

-- | Throwaway in-process pool for search-logic tests.
predicatePool :: (ByteString -> Bool) -> IO OraclePool
predicatePool predicate = testPoolWith 1 (pure . decide)
    where
        decide candidate = if predicate candidate then OracleAccepted else OracleRejected

outcomePool :: (ByteString -> IO OracleOutcome) -> IO OraclePool
outcomePool = testPoolWith 1

jobsPool :: Int -> (ByteString -> Bool) -> IO OraclePool
jobsPool jobs predicate = testPoolWith jobs (pure . decide)
    where
        decide candidate = if predicate candidate then OracleAccepted else OracleRejected

testPoolWith :: Int -> (ByteString -> IO OracleOutcome) -> IO OraclePool
testPoolWith jobs decide =
    newOraclePool
        PoolConfig
            { poolJobs = jobs
            , poolCacheEnabled = False
            , poolPermits = Nothing
            , poolPrecomputed = Map.empty
            , poolMetrics = Nothing
            , poolExecute = fmap (observationDetails . toObservation) . decide
            , poolBill = const (pure ())
            , poolRetire = \_ _ _ _ -> pure ()
            , poolReportCount = Nothing
            }
    where
        toObservation OracleAccepted = OracleFinished ExitSuccess
        toObservation OracleRejected = OracleFinished (ExitFailure 1)
        toObservation OracleInconclusive = OracleTimedOut

syntaxOnly :: SQLDialect -> OraclePool -> ByteString -> IO ByteString
syntaxOnly dialect =
    reduceSQLWithConfig
        (defaultReductionConfigFor dialect)
            { reductionSlicing = False
            , reductionClosure = False
            , reductionUnrestricted = False
            , reductionIntraStatement = True
            }
        nullLogger
