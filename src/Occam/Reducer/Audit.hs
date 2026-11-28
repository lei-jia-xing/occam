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
import Data.IntSet qualified as IntSet
import Data.Maybe (listToMaybe)
import Occam.Certificate (
    EarlyRejectCertifier,
    EarlyRejectTracker,
    checkEarlyRejectionBytesTracked,
    newEarlyRejectTracker,
    prepareDeletionCertificateCheck,
 )
import Occam.Core.Oracle (OracleOutcome (..), oracleAccepts)
import Occam.Core.Types (
    EarlyRejectReason,
    ResultTrace,
    Span (spanStartByte),
    Statement (..),
    StatementId (..),
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
import Occam.Reducer.Statement (statementIdsInt)
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
    , auditCheckDeletion :: !(Int -> IntSet.IntSet -> IO (Maybe EarlyRejectReason))
    , auditPool :: !OraclePool
    , auditFirstOffset :: !Int
    , auditExtents :: ![StatementExtent]
    }

-- | Statement ID and byte length through the next statement start or EOF.
data StatementExtent = StatementExtent !Int !Int

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
                start = spanStartByte . stmtSpan
                firstOffset = maybe 0 start (listToMaybe statements)
                ends = map start (drop 1 statements) ++ [BS.length initialSQL]
                extents =
                    zipWith
                        (\stmt end -> StatementExtent (unStatementId (stmtId stmt)) (end - start stmt))
                        statements
                        ends
                !initialKept = statementIdsInt statements
                !initialCount = length statements
            cert <- buildCertifierTracked earlyReject tracker initialSQL units mTrace
            checkDeletion <- prepareDeletionCertificateCheck tracker cert initialKept
            let env = AuditEnv tracker cert checkDeletion pool firstOffset extents
            auditLoop env initialSQL initialKept initialCount 0 0 0

-- The audit preserves the extent lengths of retained statements. Each pass
-- walks those extents in source order, starting after the original preamble.
auditLoop ::
    AuditEnv -> ByteString -> IntSet.IntSet -> Int -> Int -> Int -> Int -> IO (ByteString, AuditStats)
auditLoop env sql kept !keptCount passCount attemptCount removalCount
    | keptCount <= 0 || IntSet.null kept =
        pure (sql, AuditStats passCount attemptCount removalCount True)
    | otherwise = do
        (nextSQL, nextKept, nextKeptCount, attempts, removals) <- runAuditPass env sql kept keptCount
        let nextPassCount = passCount + 1
            totalAttempts = attemptCount + attempts
        if removals > 0
            then
                auditLoop env nextSQL nextKept nextKeptCount nextPassCount totalAttempts (removalCount + removals)
            else pure (nextSQL, AuditStats nextPassCount totalAttempts removalCount True)

runAuditPass ::
    AuditEnv -> ByteString -> IntSet.IntSet -> Int -> IO (ByteString, IntSet.IntSet, Int, Int, Int)
runAuditPass env sql kept keptCount =
    go sql kept keptCount (auditFirstOffset env) (auditExtents env) 0 0
    where
        retire (!tested, _) (sid, offset, rest) bytes (outcome, _) =
            pure (tested + 1, if outcome == OracleAccepted then Just (sid, offset, rest, bytes) else Nothing)
        go currentSQL currentKept !currentCount !offset extents !attempts !removals = do
            (tested, accepted) <-
                testOrderedGenerated
                    (auditPool env)
                    (nextAuditCandidate env currentSQL currentKept (currentCount - 1))
                    retire
                    (0, Nothing)
                    (Nothing, offset, extents)
            case accepted of
                Nothing -> pure (currentSQL, currentKept, currentCount, attempts + tested, removals)
                Just (sid, deletedOffset, rest, bytes) ->
                    -- The next extent moves to the start of the removed bytes.
                    go
                        bytes
                        (IntSet.delete sid currentKept)
                        (currentCount - 1)
                        deletedOffset
                        rest
                        (attempts + tested)
                        (removals + 1)

nextAuditCandidate ::
    AuditEnv ->
    ByteString ->
    IntSet.IntSet ->
    Int ->
    (PrevDeletion, Int, [StatementExtent]) ->
    IO
        ( Maybe
            ((FamilyTag, ByteString, (Int, Int, [StatementExtent])), (PrevDeletion, Int, [StatementExtent]))
        )
nextAuditCandidate _ _ _ _ (_, _, []) = pure Nothing
nextAuditCandidate env sql currentKept count (previous, !offset, StatementExtent sid len : rest)
    | IntSet.notMember sid currentKept =
        nextAuditCandidate env sql currentKept count (previous, offset, rest)
    | otherwise = do
        (nextLast, candidate) <- evaluateAuditCandidate env sql currentKept sid offset len previous
        let next = (nextLast, offset + len, rest)
        case candidate of
            Nothing -> nextAuditCandidate env sql currentKept count next
            Just bytes -> do
                reportCandidateCount (auditPool env) bytes count
                pure (Just ((TagStatement, bytes, (sid, offset, rest)), next))

evaluateAuditCandidate ::
    AuditEnv ->
    ByteString ->
    IntSet.IntSet ->
    Int ->
    Int ->
    Int ->
    PrevDeletion ->
    IO (PrevDeletion, Maybe ByteString)
evaluateAuditCandidate env sql currentKept sid offset len previous = evalContT do
    let store = poolMetricsStore (auditPool env)
        keptAfter = IntSet.delete sid currentKept
    earlyId <- liftIO $ auditCheckDeletion env sid keptAfter
    for_ earlyId \reason -> do
        liftIO $ recordEarlyRejectSkip store reason
        earlyExit (previous, Nothing)
    (bytes, nextLast) <- liftIO $ renderAuditCandidate env sql offset len previous
    when (BS.length bytes >= BS.length sql) do
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
    ByteString ->
    Int ->
    Int ->
    PrevDeletion ->
    IO (ByteString, PrevDeletion)
renderAuditCandidate env sql off len previous =
    case previous of
        Just (prevOff, prevLen, bytes)
            | off == prevOff + prevLen
            , len == prevLen
            , deletedBytes off len == deletedBytes prevOff prevLen ->
                pure (bytes, Just (off, len, bytes))
        _ -> do
            let raw = BS.take off sql <> BS.drop (off + len) sql
            bytes <- timeStatementBuild (poolMetricsStore (auditPool env)) raw
            pure (bytes, Just (off, len, bytes))
    where
        deletedBytes start size = BS.take size (BS.drop start sql)

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
