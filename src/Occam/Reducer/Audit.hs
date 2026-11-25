-- | Repeated single-statement deletion until reaching a 1-minimal fixed point.
module Occam.Reducer.Audit (
    AuditStats (..),
    statementAudit,
    statementAuditWithLogger,
    statementAuditWithParser,
    statementAuditWithCert,
) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (evalContT)
import Data.Aeson (toJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Maybe (fromMaybe)
import Occam.Certificate (
    EarlyRejectCertifier,
    EarlyRejectTracker,
    checkEarlyRejectionBytesTracked,
    checkEarlyRejectionIdsTracked,
    newEarlyRejectTracker,
 )
import Occam.Core (
    OracleOutcome (..),
    ResultTrace,
    Statement (..),
    StatementId (..),
    oracleAccepts,
 )
import Occam.Facts (StatementUnit (..))
import Occam.Logger (Logger, logDebug', logInfo', nullLogger)
import Occam.Metrics (FamilyTag (..), recordGeneratedSkipped)
import Occam.Parser.Types (SQLDialect)
import Occam.Pool (
    OraclePool,
    poolMetricsStore,
    poolOracle,
    reportCandidateCount,
    testOrderedGenerated,
 )
import Occam.Reducer.Statement (
    StatementSnapshot (..),
    buildStatementSnapshot,
    cutRemovedStatementInts,
    renderSingletonDeletion,
    statementIdsInt,
 )
import Occam.Reducer.Support (
    StatementParser,
    buildCertifierTracked,
    earlyExit,
    parseStatementsFast,
    recordEarlyRejectSkip,
    timeStatementBuild,
 )

data AuditStats = AuditStats
    { auditPasses :: !Int
    , auditAttempts :: !Int
    , auditRemovals :: !Int
    , auditMinimalityConfirmed :: !Bool
    }

-- | Repeated single-statement deletion until reaching a 1-minimal fixed point.
statementAudit :: SQLDialect -> OraclePool -> ByteString -> IO ByteString
statementAudit dialect pool initialSQL = fst <$> statementAuditWithLogger nullLogger dialect pool initialSQL

statementAuditWithLogger ::
    Logger -> SQLDialect -> OraclePool -> ByteString -> IO (ByteString, AuditStats)
statementAuditWithLogger logger dialect pool =
    statementAuditWithParser logger (parseStatementsFast (poolMetricsStore pool) dialect) pool

statementAuditWithParser ::
    Logger -> StatementParser -> OraclePool -> ByteString -> IO (ByteString, AuditStats)
statementAuditWithParser logger parse pool initialSQL = do
    tracker <- newEarlyRejectTracker
    statementAuditWithCert False tracker Nothing logger parse pool initialSQL

data AuditEnv = AuditEnv
    { auditTracker :: !EarlyRejectTracker
    , auditCert :: !EarlyRejectCertifier
    , auditPool :: !OraclePool
    , auditBaseSQL :: !ByteString
    , auditAllStatements :: ![Statement]
    }

type PrevDeletion = Maybe (Int, Int, ByteString)

statementAuditWithCert ::
    Bool ->
    EarlyRejectTracker ->
    Maybe ResultTrace ->
    Logger ->
    StatementParser ->
    OraclePool ->
    ByteString ->
    IO (ByteString, AuditStats)
statementAuditWithCert earlyReject tracker mTrace logger parse pool initialSQL = do
    initialOK <- oracleAccepts (poolOracle pool) initialSQL
    outcome <-
        if initialOK
            then runConfirmedAudit
            else pure (initialSQL, AuditStats 0 0 0 False)
    logAuditStats logger (snd outcome)
    pure outcome
    where
        runConfirmedAudit = do
            (_, units) <- parse initialSQL
            let statements = map unitStatement units
                !initialKept = statementIdsInt statements
                !initialCount = length statements
            cert <- buildCertifierTracked earlyReject tracker initialSQL units mTrace
            let env = AuditEnv tracker cert pool initialSQL statements
            auditLoop env initialKept initialCount 0 0 0

-- The audit only removes whole statements, and those statements keep
-- their original byte spans. Parse once, then track the retained set.
auditLoop ::
    AuditEnv -> IntSet.IntSet -> Int -> Int -> Int -> Int -> IO (ByteString, AuditStats)
auditLoop env kept !keptCount passCount attemptCount removalCount
    | keptCount <= 0 || IntSet.null kept =
        pure (renderAuditKept env kept, AuditStats passCount attemptCount removalCount True)
    | otherwise = do
        (nextKept, nextKeptCount, attempts, removals) <- runAuditPass env kept keptCount
        let nextPassCount = passCount + 1
            totalAttempts = attemptCount + attempts
        if removals > 0
            then auditLoop env nextKept nextKeptCount nextPassCount totalAttempts (removalCount + removals)
            else pure (renderAuditKept env nextKept, AuditStats nextPassCount totalAttempts removalCount True)

runAuditPass :: AuditEnv -> IntSet.IntSet -> Int -> IO (IntSet.IntSet, Int, Int, Int)
runAuditPass env kept keptCount =
    go initSnap kept keptCount (auditAllStatements env) 0 0
    where
        !initSnap =
            buildStatementSnapshot (auditBaseSQL env) (auditAllStatements env) kept (renderAuditKept env kept)
        retire (!tested, _) (sid, rest) bytes (outcome, _) =
            pure (tested + 1, if outcome == OracleAccepted then Just (sid, rest, bytes) else Nothing)
        go snap currentKept !currentCount statements !attempts !removals = do
            (tested, accepted) <-
                testOrderedGenerated
                    (auditPool env)
                    (nextAuditCandidate env snap currentKept (currentCount - 1))
                    retire
                    (0, Nothing)
                    (Nothing, statements)
            case accepted of
                Nothing -> pure (currentKept, currentCount, attempts + tested, removals)
                Just (sid, rest, bytes) -> do
                    let nextKept = IntSet.delete sid currentKept
                        nextSnap = buildStatementSnapshot (auditBaseSQL env) (auditAllStatements env) nextKept bytes
                    go nextSnap nextKept (currentCount - 1) rest (attempts + tested) (removals + 1)

nextAuditCandidate ::
    AuditEnv ->
    StatementSnapshot ->
    IntSet.IntSet ->
    Int ->
    (PrevDeletion, [Statement]) ->
    IO (Maybe ((FamilyTag, ByteString, (Int, [Statement])), (PrevDeletion, [Statement])))
nextAuditCandidate _ _ _ _ (_, []) = pure Nothing
nextAuditCandidate env snap currentKept count (previous, statement : rest)
    | IntSet.notMember sid currentKept = nextAuditCandidate env snap currentKept count (previous, rest)
    | otherwise = do
        (nextLast, candidate) <- evaluateAuditCandidate env snap currentKept statement sid previous
        case candidate of
            Nothing -> nextAuditCandidate env snap currentKept count (nextLast, rest)
            Just bytes -> do
                reportCandidateCount (auditPool env) bytes count
                pure (Just ((TagStatement, bytes, (sid, rest)), (nextLast, rest)))
    where
        sid = unStatementId (stmtId statement)

evaluateAuditCandidate ::
    AuditEnv ->
    StatementSnapshot ->
    IntSet.IntSet ->
    Statement ->
    Int ->
    PrevDeletion ->
    IO (PrevDeletion, Maybe ByteString)
evaluateAuditCandidate env snap currentKept statement sid previous = evalContT do
    let store = poolMetricsStore (auditPool env)
        keptAfter = IntSet.delete sid currentKept
    earlyId <- liftIO $ checkEarlyRejectionIdsTracked (auditTracker env) (auditCert env) keptAfter
    for_ earlyId \reason -> do
        liftIO $ recordEarlyRejectSkip store reason
        earlyExit (previous, Nothing)
    (bytes, nextLast) <- liftIO $ renderAuditCandidate env snap statement sid keptAfter previous
    when (BS.length bytes >= BS.length (snapSQL snap)) do
        liftIO $ recordGeneratedSkipped store "nonshrinking"
        earlyExit (nextLast, Nothing)
    earlyBytes <-
        liftIO $ checkEarlyRejectionBytesTracked (auditTracker env) (auditCert env) False bytes
    for_ earlyBytes \reason -> do
        liftIO $ recordEarlyRejectSkip store reason
        earlyExit (nextLast, Nothing)
    pure (nextLast, Just bytes)

renderAuditCandidate ::
    AuditEnv ->
    StatementSnapshot ->
    Statement ->
    Int ->
    IntSet.IntSet ->
    PrevDeletion ->
    IO (ByteString, PrevDeletion)
renderAuditCandidate env snap statement sid keptAfter previous =
    case (previous, IntMap.lookup sid (snapOffsets snap)) of
        (Just (prevOff, prevLen, bytes), Just (off, len))
            | off == prevOff + prevLen
            , len == prevLen
            , deletedBytes off len == deletedBytes prevOff prevLen ->
                pure (bytes, Just (off, len, bytes))
        (_, extent) -> do
            let fallback = renderAuditKept env keptAfter
                raw = fromMaybe fallback (renderSingletonDeletion snap statement)
            bytes <- timeStatementBuild (poolMetricsStore (auditPool env)) raw
            pure (bytes, fmap (\(off, len) -> (off, len, bytes)) extent)
    where
        deletedBytes off len = BS.take len (BS.drop off (snapSQL snap))

renderAuditKept :: AuditEnv -> IntSet.IntSet -> ByteString
renderAuditKept env = cutRemovedStatementInts (auditBaseSQL env) (auditAllStatements env)

logAuditStats :: Logger -> AuditStats -> IO ()
logAuditStats logger stats =
    let removals = auditRemovals stats
     in if removals > 0
            then
                logInfo'
                    logger
                    "reducer"
                    "Statement deletion audit reduced statements"
                    [ ("statements_removed", toJSON removals)
                    , ("audit_attempts", toJSON (auditAttempts stats))
                    , ("statement_1_minimal", toJSON (auditMinimalityConfirmed stats))
                    ]
            else
                logDebug'
                    logger
                    "reducer"
                    "Statement deletion audit confirmed 1-minimality"
                    [ ("audit_attempts", toJSON (auditAttempts stats))
                    , ("statement_1_minimal", toJSON (auditMinimalityConfirmed stats))
                    ]
