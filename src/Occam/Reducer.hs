module Occam.Reducer (
    ReductionConfig (..),
    ColumnMode (..),
    columnModeName,
    reductionSearchPolicy,
    defaultReductionConfig,
    isValidClosure,
    closeSet,
    closureDdmin,
    cutRemovedStatements,
    cutRemovedStatementIds,
    replaceSpan,
    partitionChunks,
    reduceSQL,
    reduceSQLWithLogger,
    reduceSQLWithConfig,
    reduceSQLWithEvidence,
    ReductionResult (..),
    ReductionEvidence (..),
    ReductionStage (..),
    reductionStageName,
    StageEvent (..),
    StageReporter,
    nullStageReporter,
    statementDdmin,
    statementAudit,
    statementAuditWithLogger,
    AuditStats (..),
    findInterestingPrefix,
) where

import Control.Monad (when)
import Data.Aeson (toJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Core (
    Ident (..),
    OracleOutcome (..),
    SessionInit (..),
    Span (..),
    Statement (..),
    StatementId (..),
    nameText,
    oracleAccepts,
 )
import Occam.Dependency (
    DependencyGraph (..),
    analyzeStatements,
 )
import Occam.Facts (editCategoryName)
import Occam.Logger (Logger, logInfo', logWarn, nullLogger)
import Occam.Metrics (
    FamilyTag (..),
    MetricsStore,
    ParseKind (..),
    recordCandidateBuildTiming,
    recordGeneratedSkipped,
 )
import Occam.Parser (
    ColumnSite (..),
    ColumnTarget (..),
    ParseOptions (..),
    ParsedSQL (..),
    ParserEdit (..),
    ParserEditCategory (..),
    SQLDialect (..),
    StatementFacts (..),
    StatementUnit (..),
    defaultParseOptions,
    defaultSQLDialect,
    parseSQLTimed,
 )
import Occam.Pool (
    OraclePool,
    poolJobCount,
    poolMetricsStore,
    poolOracle,
    reportCandidateCount,
    testOrderedTagged,
 )
import Occam.Slicer (backwardSlice, backwardSliceMany)

data ColumnMode = ColumnOff | ColumnSingle | ColumnAtomic
    deriving (Eq, Show)

columnModeName :: ColumnMode -> T.Text
columnModeName ColumnOff = "off"
columnModeName ColumnSingle = "single"
columnModeName ColumnAtomic = "atomic"

syntaxPolicyName :: ColumnMode -> T.Text
syntaxPolicyName ColumnOff = "phase-forward-sweep-list-chunks"
syntaxPolicyName ColumnSingle = "phase-forward-sweep-list-chunks+column-single"
syntaxPolicyName ColumnAtomic = "phase-forward-sweep-list-chunks+column-atomic"

{- | Total policy description. Without intra-statement passes the column mode
never executes, so the base policy is reported; the invalid combination is
rejected by the reducer entry guard before any oracle call.
-}
reductionSearchPolicy :: ReductionConfig -> T.Text
reductionSearchPolicy config
    | not (reductionIntraStatement config) = syntaxPolicyName ColumnOff
    | otherwise = syntaxPolicyName (reductionColumnMode config)

data ReductionConfig = ReductionConfig
    { reductionSlicing :: !Bool
    , reductionClosure :: !Bool
    , reductionIntraStatement :: !Bool
    , reductionUnrestricted :: !Bool
    , reductionDialect :: !SQLDialect
    , reductionColumnMode :: !ColumnMode
    }
    deriving (Eq, Show)

defaultReductionConfig :: ReductionConfig
defaultReductionConfig =
    ReductionConfig
        { reductionSlicing = True
        , reductionClosure = True
        , reductionIntraStatement = False
        , reductionUnrestricted = True
        , reductionDialect = defaultSQLDialect
        , reductionColumnMode = ColumnOff
        }

-- | Typed reduction stage, emitted at exact boundaries for progress and summaries.
data ReductionStage
    = StageSlicing
    | StageClosureSearch
    | StageGraphFreeSearch
    | StageUnrestrictedFallback
    | StageStatementAudit
    | StageIntraStatementRewrites
    deriving (Eq, Ord, Show, Enum, Bounded)

reductionStageName :: ReductionStage -> T.Text
reductionStageName StageSlicing = "slicing"
reductionStageName StageClosureSearch = "closure-search"
reductionStageName StageGraphFreeSearch = "graph-free-search"
reductionStageName StageUnrestrictedFallback = "unrestricted-fallback"
reductionStageName StageStatementAudit = "statement-audit"
reductionStageName StageIntraStatementRewrites = "intra-statement-rewrites"

-- | Legacy log-stage labels, kept stable for user-visible logs.
stageLogName :: ReductionStage -> T.Text
stageLogName StageSlicing = "backward-slicing"
stageLogName StageClosureSearch = "closure-search"
stageLogName StageGraphFreeSearch = "graph-free-statement-search"
stageLogName StageUnrestrictedFallback = "unrestricted-fallback"
stageLogName StageStatementAudit = "statement-audit"
stageLogName StageIntraStatementRewrites = "intra-statement-rewrites"

-- | Boundary event: enter names the stage; finish carries seconds and bytes.
data StageEvent
    = StageStarted !ReductionStage
    | StageFinished !ReductionStage !Double !Int !Int
    deriving (Eq, Show)

type StageReporter = StageEvent -> IO ()

nullStageReporter :: StageReporter
nullStageReporter _ = pure ()

{- | Proof returned alongside reduced SQL: these exact bytes need no further
audit work. Every skip guarded by evidence rechecks byte-identity first;
evidence never crosses a byte change.
-}
data ReductionEvidence = ReductionEvidence
    { evidenceBytes :: !ByteString
    , evidenceAuditMinimal :: !Bool
    }
    deriving (Eq, Show)

data ReductionResult = ReductionResult
    { reductionSQL :: !ByteString
    , reductionEvidence :: !(Maybe ReductionEvidence)
    }
    deriving (Eq, Show)

seconds :: Word64 -> Double
seconds ns = fromIntegral ns / 1000000000

logStage :: Logger -> T.Text -> Word64 -> IO ()
logStage logger stage elapsed =
    logInfo'
        logger
        "occam::reducer"
        ("Finished " <> stage)
        [("stage", toJSON stage), ("duration_seconds", toJSON (seconds elapsed))]

-- | Timed stage with typed boundary events. Keeps the legacy log line.
timedReductionStage ::
    StageReporter -> Logger -> ReductionStage -> ByteString -> IO (ByteString, a) -> IO (ByteString, a)
timedReductionStage reporter logger stage input action = do
    reporter (StageStarted stage)
    started <- getMonotonicTimeNSec
    (output, extra) <- action
    finished <- getMonotonicTimeNSec
    let elapsed = finished - started
    logStage logger (stageLogName stage) elapsed
    reporter (StageFinished stage (seconds elapsed) (BS.length input) (BS.length output))
    pure (output, extra)

kindOfOptions :: ParseOptions -> ParseKind
kindOfOptions options
    | parseColumnIR options || parseColumnBundles options = ParseColumnIR
    | parseChunkEdits options = ParseSyntaxChunks
    | otherwise = ParseStatementOnly

parseUnitsOrThrow ::
    Maybe MetricsStore ->
    ParseOptions ->
    ByteString ->
    IO (SessionInit, [StatementUnit], [ColumnTarget])
parseUnitsOrThrow store options sql = do
    parsed <- parseSQLTimed store (kindOfOptions options) options sql
    case parsed of
        Left parserError -> ioError (userError ("SQL parser transport failure: " ++ show parserError))
        Right result -> pure (parsedInitialSession result, parsedUnits result, parsedColumns result)

{- | The syntax pass alone opts into list-chunk deletions; statement-only
parses keep the legacy payload without chunk materialization.
-}
syntaxParseOptions :: SQLDialect -> ParseOptions
syntaxParseOptions dialect = (defaultParseOptions dialect) {parseChunkEdits = True, parseSyntaxEdits = True}

-- | Single-point column sites only: no bundle planning or payload.
columnSingleParseOptions :: SQLDialect -> ParseOptions
columnSingleParseOptions dialect = (syntaxParseOptions dialect) {parseColumnIR = True}

-- | Full atomic bundles; implies single-point geometry.
columnAtomicParseOptions :: SQLDialect -> ParseOptions
columnAtomicParseOptions dialect = (syntaxParseOptions dialect) {parseColumnIR = True, parseColumnBundles = True}

columnParseOptions :: ColumnMode -> SQLDialect -> ParseOptions
columnParseOptions ColumnOff = syntaxParseOptions
columnParseOptions ColumnSingle = columnSingleParseOptions
columnParseOptions ColumnAtomic = columnAtomicParseOptions

{- | A retained candidate set K is closed iff every kept statement's direct
predecessors are also retained.
-}
isValidClosure :: DependencyGraph -> Set StatementId -> Bool
isValidClosure DependencyGraph {..} kept =
    all isClosureOk (Set.toList kept)
    where
        isClosureOk sid =
            let predecessors = Map.findWithDefault Set.empty sid graphEdges
             in predecessors `Set.isSubsetOf` kept

-- | Transitive backward closure of a set of statement IDs.
closeSet :: DependencyGraph -> Set StatementId -> Set StatementId
closeSet graph roots = backwardSliceMany roots graph

{- | Cut removed statements without unparsing or changing retained bytes.

Each statement owns the source bytes from its first token up to the first
token of the next statement, or to end-of-input for the final statement. That
partition includes the statement's own terminator and separator, so removing
a statement cannot leave an orphaned delimiter or blank line behind. The
partition is exact: it never inspects or rewrites the bytes it keeps.
-}
cutRemovedStatementIds :: ByteString -> [Statement] -> Set StatementId -> ByteString
cutRemovedStatementIds baseSQL allStatements keptIds =
    case chunks 0 allStatements of
        [] -> BS.empty
        [single] -> single
        slices -> BS.concat slices
    where
        sourceLength = BS.length baseSQL

        chunks !cursor []
            | cursor < sourceLength = [BS.drop cursor baseSQL]
            | otherwise = []
        chunks !cursor (stmt : rest)
            | Set.member (stmtId stmt) keptIds = chunks cursor rest
            | otherwise =
                let Span start _ = stmtSpan stmt
                    nextStart = case rest of
                        next : _ -> spanStartByte (stmtSpan next)
                        [] -> sourceLength
                    prefix =
                        if start > cursor
                            then (BS.take (start - cursor) (BS.drop cursor baseSQL) :)
                            else id
                 in prefix (chunks nextStart rest)

cutRemovedStatements :: ByteString -> [Statement] -> [Statement] -> ByteString
cutRemovedStatements baseSQL allStatements keptStatements =
    cutRemovedStatementIds baseSQL allStatements (statementIds keptStatements)

-- | Split a list into at most n approximately equal, contiguous chunks.
partitionChunks :: Int -> [a] -> [[a]]
partitionChunks n xs
    | n <= 1 = [xs]
    | null xs = []
    | otherwise = go n (length xs) xs
    where
        go _ _ [] = []
        go remaining total rest =
            let chunkSize = (total + remaining - 1) `div` remaining
                (chunk, next) = splitAt chunkSize rest
             in chunk : go (remaining - 1) (total - chunkSize) next

statementSpanBytes :: Statement -> Int
statementSpanBytes statement =
    let Span start end = stmtSpan statement
     in end - start

statementIds :: [Statement] -> Set StatementId
statementIds = Set.fromList . map stmtId

statementSQL :: ByteString -> [Statement] -> [Statement] -> ByteString
statementSQL = cutRemovedStatements

-- | Closure-constrained delta debugging.
closureDdmin :: OraclePool -> ByteString -> [Statement] -> DependencyGraph -> IO [Statement]
closureDdmin = closureDdminWithLogger nullLogger

closureDdminWithLogger ::
    Logger -> OraclePool -> ByteString -> [Statement] -> DependencyGraph -> IO [Statement]
closureDdminWithLogger logger pool baseSQL initialStatements graph = do
    closureCache <- newIORef Map.empty
    closureStats <- newIORef (ClosureStats 0 0 0 0)
    let weights = Map.fromList [(stmtId s, statementSpanBytes s) | s <- initialStatements]
        buildCandidate current seedIds = do
            modifyIORef' closureStats (\stats -> stats {closureRequests = closureRequests stats + 1})
            cached <- readIORef closureCache
            closedIds <-
                case Map.lookup seedIds cached of
                    Just result -> do
                        modifyIORef' closureStats (\stats -> stats {closureCacheHits = closureCacheHits stats + 1})
                        pure result
                    Nothing -> do
                        let result = closeSet graph seedIds
                        modifyIORef' closureCache (Map.insert seedIds result)
                        pure result
            let currentIds = statementIds current
                keptIds = closedIds `Set.intersection` currentIds
                added = keptIds `Set.difference` seedIds
                gainedBytes = sum [Map.findWithDefault 0 sid weights | sid <- Set.toList added]
            modifyIORef'
                closureStats
                ( \stats ->
                    stats
                        { closureGainStatements = closureGainStatements stats + Set.size added
                        , closureGainBytes = closureGainBytes stats + gainedBytes
                        }
                )
            if isValidClosure graph keptIds
                then pure (Just (filter (\s -> Set.member (stmtId s) keptIds) current))
                else pure Nothing
    result <-
        searchStatements
            buildCandidate
            pool
            baseSQL
            initialStatements
            initialStatements
    stats <- readIORef closureStats
    logInfo'
        logger
        "occam::reducer"
        "Closure search summary"
        [ ("closure_requests", toJSON (closureRequests stats))
        , ("closure_cache_hits", toJSON (closureCacheHits stats))
        , ("closure_gain_statements", toJSON (closureGainStatements stats))
        , ("closure_gain_bytes", toJSON (closureGainBytes stats))
        ]
    pure result

data ClosureStats = ClosureStats
    { closureRequests :: !Int
    , closureCacheHits :: !Int
    , closureGainStatements :: !Int
    , closureGainBytes :: !Int
    }

data CandidateVerdict
    = CandidateAccepted
    | CandidateRejected
    deriving (Eq, Show)

{- | Verdict memo keyed ONLY by exact candidate bytes. Statement-ID-keyed
rejection reuse is unsafe for arbitrary oracles: a removal rejected under
one snapshot can become valid after the current SQL shrinks, so IDs must
never skip re-testing a changed candidate. Same exact bytes stay valid.
-}
newtype Feedback = Feedback
    { fbEvaluated :: IORef (HashMap ByteString CandidateVerdict)
    }

newFeedback :: IO Feedback
newFeedback = Feedback <$> newIORef HashMap.empty
searchStatements ::
    CandidateBuilder ->
    OraclePool ->
    ByteString ->
    [Statement] ->
    [Statement] ->
    IO [Statement]
searchStatements buildCandidate pool baseSQL allStatements initialStatements = do
    feedback <- newFeedback
    searchDDMin
        feedback
        buildCandidate
        pool
        baseSQL
        allStatements
        initialStatements

type CandidateBuilder = [Statement] -> Set StatementId -> IO (Maybe [Statement])

plainCandidate :: CandidateBuilder
plainCandidate current keptIds =
    pure (Just (filter (\s -> Set.member (stmtId s) keptIds) current))

-- | Classified seeds: previously accepted seeds commit; fresh seeds test in order.
data SeedToken
    = SeedAccepted ![Statement]
    | SeedFresh ![Statement] !ByteString

searchDDMin ::
    Feedback ->
    CandidateBuilder ->
    OraclePool ->
    ByteString ->
    [Statement] ->
    [Statement] ->
    IO [Statement]
searchDDMin feedback buildCandidate pool baseSQL allStatements = go 2
    where
        jobs = poolJobCount pool
        go _ [] = pure []
        go granularity current = do
            let n = min (length current) (max 1 granularity)
                chunks = partitionChunks n current
                currentIds = statementIds current
                currentSQL = cutRemovedStatementIds baseSQL allStatements currentIds
                currentLen = BS.length currentSQL
            subset <- trySubsets current currentIds currentLen chunks
            case subset of
                Just reduced -> go 2 reduced
                Nothing -> do
                    complement <- tryComplements current currentIds currentLen chunks
                    case complement of
                        Just reduced -> go (max 2 (n - 1)) reduced
                        Nothing
                            | n < length current -> go (min (length current) (2 * n)) current
                            | otherwise -> pure current

        trySubsets current currentIds currentLen chunks =
            trySeeds current currentIds currentLen (map statementIds chunks)
        tryComplements current currentIds currentLen chunks =
            trySeeds
                current
                currentIds
                currentLen
                [currentIds `Set.difference` statementIds chunk | chunk <- chunks]

        trySeeds current currentIds currentLen = pull
            where
                -- Pull only the next window of eligible candidates, test and
                -- record them, then classify further seeds with updated
                -- feedback. Never materializes SQL for the whole granularity.
                -- Duplicate candidate bytes within one pull are probed once:
                -- the byte memo would skip the twin right after the first
                -- rejects, and any other outcome stops before it. The scan is
                -- bounded too: at most one window of classify work is pulled
                -- before testing, never a full duplicate-list scan.
                pull [] = pure Nothing
                pull seeds = collect jobs [] HashSet.empty seeds
                collect _ acc _ [] = testAndContinue (reverse acc) [] Nothing
                collect 0 acc _ rest = testAndContinue (reverse acc) rest Nothing
                collect n acc seen (seedIds : rest) = do
                    token <- classifySeed current currentIds currentLen seedIds
                    case token of
                        Nothing -> collect (n - 1) acc seen rest
                        Just (SeedAccepted kept) -> testAndContinue (reverse acc) rest (Just kept)
                        Just (SeedFresh kept sql)
                            | HashSet.member sql seen -> collect (n - 1) acc seen rest
                            | otherwise -> collect (n - 1) ((kept, sql) : acc) (HashSet.insert sql seen) rest
                testAndContinue batch rest barrier = do
                    accepted <- testBatch batch
                    case accepted of
                        Just kept -> pure (Just kept)
                        Nothing -> case barrier of
                            Just kept -> pure (Just kept)
                            Nothing -> pull rest

        testBatch [] = pure Nothing
        testBatch batch = do
            mapM_ (\(kept, sql) -> reportCandidateCount pool sql (length kept)) batch
            outcomes <- testOrderedTagged pool [(TagStatement, sql) | (_, sql) <- batch]
            let retired = zip batch outcomes
            mapM_ recordRetired retired
            pure $ listToMaybe [kept | ((kept, _), (outcome, _)) <- retired, outcome == OracleAccepted]

        recordRetired ((_, sql), (outcome, _)) =
            let verdict = if outcome == OracleAccepted then CandidateAccepted else CandidateRejected
             in modifyIORef' (fbEvaluated feedback) (HashMap.insert sql verdict)

        classifySeed current currentIds currentLen seedIds
            | seedIds == currentIds = pure Nothing
            | otherwise = do
                maybeKept <- buildCandidate current seedIds
                case maybeKept of
                    Nothing -> pure Nothing
                    Just kept -> do
                        candidateSQL <- timeStatementBuild (poolMetricsStore pool) (statementSQL baseSQL allStatements kept)
                        if BS.length candidateSQL >= currentLen
                            then pure Nothing
                            else do
                                evaluated <- readIORef (fbEvaluated feedback)
                                case HashMap.lookup candidateSQL evaluated of
                                    Just CandidateAccepted -> pure (Just (SeedAccepted kept))
                                    Just CandidateRejected -> do
                                        recordGeneratedSkipped (poolMetricsStore pool) "duplicate-bytes"
                                        pure Nothing
                                    Nothing -> pure (Just (SeedFresh kept candidateSQL))

-- | Graph-free statement ddmin over original top-level statement byte spans.
statementDdmin :: SQLDialect -> OraclePool -> ByteString -> IO ByteString
statementDdmin dialect pool initialSQL = do
    initialOK <- oracleAccepts (poolOracle pool) initialSQL
    if not initialOK
        then pure initialSQL
        else do
            (_, units, _) <- parseUnitsOrThrow (poolMetricsStore pool) (defaultParseOptions dialect) initialSQL
            let allStatements = map unitStatement units
            reduced <-
                searchStatements plainCandidate pool initialSQL allStatements allStatements
            pure (statementSQL initialSQL allStatements reduced)

{- | One-statement deletion to a fixed point; the empty candidate is allowed.
Successive rejected deletions share the snapshot, so each window pulls the
next retained statements and retires them in order.
-}
statementAudit :: SQLDialect -> OraclePool -> ByteString -> IO ByteString
statementAudit dialect pool initialSQL = fst <$> statementAuditWithLogger nullLogger dialect pool initialSQL

statementAuditWithLogger ::
    Logger -> SQLDialect -> OraclePool -> ByteString -> IO (ByteString, AuditStats)
statementAuditWithLogger logger dialect pool initialSQL = do
    initialOK <- oracleAccepts (poolOracle pool) initialSQL
    outcome <-
        if initialOK
            then do
                (_, units, _) <- parseUnitsOrThrow (poolMetricsStore pool) (defaultParseOptions dialect) initialSQL
                let statements = map unitStatement units
                loop initialSQL statements (statementIds statements) 0 0 0
            else pure (initialSQL, AuditStats 0 0 0 False)
    logAuditStats logger (snd outcome)
    pure outcome
    where
        jobs = poolJobCount pool
        -- The audit only removes whole statements, and those statements keep
        -- their original byte spans. Parse once, then track the retained set.
        loop baseSQL allStatements kept passCount attemptCount removalCount
            | Set.null kept =
                pure
                    ( renderKept baseSQL allStatements kept
                    , AuditStats passCount attemptCount removalCount True
                    )
            | otherwise = do
                (nextKept, attempts, removals) <- auditPass baseSQL allStatements kept
                let nextPassCount = passCount + 1
                if removals > 0
                    then
                        loop
                            baseSQL
                            allStatements
                            nextKept
                            nextPassCount
                            (attemptCount + attempts)
                            (removalCount + removals)
                    else
                        pure
                            ( renderKept baseSQL allStatements nextKept
                            , AuditStats nextPassCount (attemptCount + attempts) removalCount True
                            )

        auditPass baseSQL allStatements kept = go (renderKept baseSQL allStatements kept) kept allStatements 0 0
            where
                go _ currentKept [] attempts removals = pure (currentKept, attempts, removals)
                go currentSQL currentKept statements attempts removals = do
                    let batch = take jobs [s | s <- statements, Set.member (stmtId s) currentKept]
                    case batch of
                        [] -> pure (currentKept, attempts, removals)
                        _ -> do
                            let currentLen = BS.length currentSQL
                            candidates <-
                                mapM
                                    ( \statement -> do
                                        bytes <-
                                            timeStatementBuild
                                                (poolMetricsStore pool)
                                                (renderKept baseSQL allStatements (Set.delete (stmtId statement) currentKept))
                                        pure (statement, bytes)
                                    )
                                    batch
                            let testables =
                                    [(statement, sql) | (statement, sql) <- candidates, BS.length sql < currentLen]
                            if null testables
                                then do
                                    mapM_ (const (recordGeneratedSkipped (poolMetricsStore pool) "nonshrinking")) batch
                                    go currentSQL currentKept (afterStatement (last batch) statements) attempts removals
                                else do
                                    mapM_ (\(_, sql) -> reportCandidateCount pool sql (Set.size currentKept - 1)) testables
                                    outcomes <- testOrderedTagged pool [(TagStatement, sql) | (_, sql) <- testables]
                                    let retired = zip testables outcomes
                                        tested = length outcomes
                                    case listToMaybe [(s, sql) | ((s, sql), (outcome, _)) <- retired, outcome == OracleAccepted] of
                                        Just (acceptedStatement, acceptedSQL) ->
                                            go
                                                acceptedSQL
                                                (Set.delete (stmtId acceptedStatement) currentKept)
                                                (afterStatement acceptedStatement statements)
                                                (attempts + tested)
                                                (removals + 1)
                                        Nothing ->
                                            go
                                                currentSQL
                                                currentKept
                                                (afterStatement (last batch) statements)
                                                (attempts + tested)
                                                removals

                afterStatement target = goAfter
                    where
                        goAfter [] = []
                        goAfter (s : ss)
                            | stmtId s == stmtId target = ss
                            | otherwise = goAfter ss

        renderKept = cutRemovedStatementIds

data AuditStats = AuditStats
    { auditPasses :: !Int
    , auditAttempts :: !Int
    , auditRemovals :: !Int
    , auditMinimalityConfirmed :: !Bool
    }

logAuditStats :: Logger -> AuditStats -> IO ()
logAuditStats logger stats =
    logInfo'
        logger
        "occam::reducer"
        "Statement deletion audit reached a fixed point"
        [ ("audit_passes", toJSON (auditPasses stats))
        , ("audit_attempts", toJSON (auditAttempts stats))
        , ("audit_removals", toJSON (auditRemovals stats))
        , ("statement_1_minimal", toJSON (auditMinimalityConfirmed stats))
        ]

{- | Binary prefix probes. Only monotone predicates guarantee the first accepted
prefix; arbitrary predicates may have earlier accepted prefixes we never visit.
-}
findInterestingPrefix ::
    OraclePool -> ByteString -> [Statement] -> IO (Maybe (Statement, ByteString, Int), Int)
findInterestingPrefix pool baseSQL statements
    | null statements = pure (Nothing, 0)
    | otherwise = go 1 (length statements) Nothing 0
    where
        go !low !high best !tests
            | low > high = pure (best, tests)
            | otherwise = do
                let mid = low + (high - low) `div` 2
                    prefix = take mid statements
                candidate <- timeStatementBuild (poolMetricsStore pool) (statementSQL baseSQL statements prefix)
                reportCandidateCount pool candidate (length prefix)
                ok <- oracleAccepts (poolOracle pool) candidate
                let tests' = tests + 1
                if ok
                    then case reverse prefix of
                        statement : _ ->
                            go low (mid - 1) (Just (statement, candidate, mid)) tests'
                        [] ->
                            go low (mid - 1) best tests'
                    else go (mid + 1) high best tests'

-- | Parsed statements plus their dependency graph for one exact candidate byte string.
data SQLAnalysis = SQLAnalysis
    { analyzedStatements :: ![Statement]
    , analyzedGraph :: !DependencyGraph
    }

analyzeSQL :: Maybe MetricsStore -> Logger -> SQLDialect -> ByteString -> IO SQLAnalysis
analyzeSQL store logger dialect sql = do
    started <- getMonotonicTimeNSec
    (initialSession, units, _) <- parseUnitsOrThrow store (defaultParseOptions dialect) sql
    let statements = map unitStatement units
        graph = analyzeStatements initialSession units
    graph `seq` pure ()
    finished <- getMonotonicTimeNSec
    let elapsed = finished - started
        statementCount = length statements
        edgeCount = graphEdgeCount graph
        barrierActions = graphBarrierActions graph
        barrierStatements = graphBarrierStatements graph
        barrierEdges = graphBarrierEdges graph
    logInfo'
        logger
        "occam::reducer"
        "Dependency analysis completed"
        [ ("analysis_seconds", toJSON (seconds elapsed))
        , ("statements", toJSON statementCount)
        , ("dependency_edges", toJSON edgeCount)
        , ("barrier_actions", toJSON barrierActions)
        , ("barrier_statements", toJSON barrierStatements)
        , ("barrier_edges", toJSON barrierEdges)
        , ("dialect", toJSON (T.pack (show dialect)))
        ]
    pure SQLAnalysis {analyzedStatements = statements, analyzedGraph = graph}

{- | Slice @sql@ using an analysis of the same bytes. The analysis is returned
only when the result is byte-identical to @sql@, so the caller can safely reuse
it for the next stage without re-parsing.
-}
sliceWithAnalysis ::
    Logger -> OraclePool -> SQLAnalysis -> ByteString -> IO (ByteString, Maybe SQLAnalysis)
sliceWithAnalysis logger pool analysis sql = do
    let statements = analyzedStatements analysis
        graph = analyzedGraph analysis
        keep result =
            pure (result, if result == sql then Just analysis else Nothing)
    if null statements
        then keep sql
        else do
            let sliceFor target =
                    let slice = backwardSlice (stmtId target) graph
                        retained = filter (\s -> Set.member (stmtId s) slice) statements
                     in (retained, cutRemovedStatementIds sql statements slice)
                acceptSlice strategy target retained candidate = do
                    let StatementId targetId = stmtId target
                    logInfo'
                        logger
                        "occam::reducer"
                        "Backward slicing accepted"
                        [ ("seed_strategy", toJSON (strategy :: String))
                        , ("target_statement_id", toJSON targetId)
                        , ("statements_before", toJSON (length statements))
                        , ("statements_after", toJSON (length retained))
                        , ("bytes_before", toJSON (BS.length sql))
                        , ("bytes_after", toJSON (BS.length candidate))
                        ]
                    pure candidate
                lastStatement = last statements
                (lastRetained, lastCandidateRaw) = sliceFor lastStatement
            lastCandidate <- timeStatementBuild (poolMetricsStore pool) lastCandidateRaw
            lastAccepted <-
                if BS.length lastCandidate < BS.length sql
                    then do
                        reportCandidateCount pool lastCandidate (length lastRetained)
                        oracleAccepts (poolOracle pool) lastCandidate
                    else pure False
            let tryPrefix bestCandidate = do
                    (maybeTarget, prefixTests) <- findInterestingPrefix pool sql statements
                    case maybeTarget of
                        Nothing -> pure bestCandidate
                        Just (targetStatement, prefixSQL, prefixLength) -> do
                            let (retained, candidateRaw) = sliceFor targetStatement
                            candidate <- timeStatementBuild (poolMetricsStore pool) candidateRaw
                            if BS.length candidate >= BS.length sql
                                then pure bestCandidate
                                else do
                                    let reusedPrefixResult = candidate == prefixSQL
                                    ok <-
                                        if reusedPrefixResult
                                            then pure True
                                            else do
                                                reportCandidateCount pool candidate (length retained)
                                                oracleAccepts (poolOracle pool) candidate
                                    logInfo'
                                        logger
                                        "occam::reducer"
                                        "Prefix oracle result considered for backward slice"
                                        [ ("prefix_tests", toJSON prefixTests)
                                        , ("prefix_length", toJSON prefixLength)
                                        , ("prefix_result_reused", toJSON reusedPrefixResult)
                                        ]
                                    let bestLen = maybe (BS.length sql) BS.length bestCandidate
                                    if ok && BS.length candidate < bestLen
                                        then Just <$> acceptSlice "first-interesting-prefix" targetStatement retained candidate
                                        else pure bestCandidate
            let initialBest = if lastAccepted then Just lastCandidate else Nothing
            better <- tryPrefix initialBest
            case better of
                Just result
                    | Just result /= initialBest -> keep result
                _ ->
                    if lastAccepted
                        then acceptSlice "last-statement" lastStatement lastRetained lastCandidate >>= keep
                        else keep sql

closureSearchWithAnalysis ::
    Logger -> OraclePool -> ByteString -> SQLAnalysis -> IO ByteString
closureSearchWithAnalysis logger pool sql analysis =
    let statements = analyzedStatements analysis
     in if null statements
            then pure sql
            else do
                retained <-
                    closureDdminWithLogger
                        logger
                        pool
                        sql
                        statements
                        (analyzedGraph analysis)
                pure (statementSQL sql statements retained)

reduceSQL :: SQLDialect -> OraclePool -> ByteString -> IO ByteString
reduceSQL dialect = reduceSQLWithLogger dialect nullLogger

reduceSQLWithLogger :: SQLDialect -> Logger -> OraclePool -> ByteString -> IO ByteString
reduceSQLWithLogger dialect =
    reduceSQLWithConfig (defaultReductionConfig {reductionDialect = dialect})

reduceSQLWithConfig :: ReductionConfig -> Logger -> OraclePool -> ByteString -> IO ByteString
reduceSQLWithConfig config logger pool initialSQL =
    reductionSQL <$> reduceSQLWithEvidence nullStageReporter config logger pool initialSQL

reduceSQLWithEvidence ::
    StageReporter -> ReductionConfig -> Logger -> OraclePool -> ByteString -> IO ReductionResult
reduceSQLWithEvidence reporter config logger pool initialSQL = do
    when (reductionColumnMode config /= ColumnOff && not (reductionIntraStatement config)) $
        ioError (userError "reductionColumnMode requires reductionIntraStatement")
    initialOK <- oracleAccepts (poolOracle pool) initialSQL
    if not initialOK
        then do
            logWarn logger "occam::reducer" "Initial SQL candidate failed oracle check"
            pure (ReductionResult initialSQL Nothing)
        else loop (1 :: Int) initialSQL Nothing
    where
        loop iteration currentSQL priorEvidence = do
            logInfo'
                logger
                "occam::reducer"
                "Starting reduction iteration"
                [ ("iteration", toJSON iteration)
                , ("bytes", toJSON (BS.length currentSQL))
                , ("slicing", toJSON (reductionSlicing config))
                , ("closure", toJSON (reductionClosure config))
                , ("unrestricted", toJSON (reductionUnrestricted config))
                , ("intra_statement", toJSON (reductionIntraStatement config))
                , ("dialect", toJSON (T.pack (show (reductionDialect config))))
                ]
            (nextSQL, evidence) <-
                reduceIterationWithEvidence reporter config logger pool currentSQL priorEvidence
            if BS.length nextSQL < BS.length currentSQL
                then loop (iteration + 1) nextSQL evidence
                else do
                    logInfo'
                        logger
                        "occam::reducer"
                        "Rewrite fixed point reached"
                        [ ("total_iterations", toJSON iteration)
                        , ("final_bytes", toJSON (BS.length currentSQL))
                        ]
                    -- The loop keeps the pre-iteration bytes on no shrink, so
                    -- evidence only survives when it attests those exact bytes.
                    let confirmed =
                            if maybe False ((== currentSQL) . evidenceBytes) evidence then evidence else Nothing
                    pure (ReductionResult currentSQL confirmed)

reduceIterationWithEvidence ::
    StageReporter ->
    ReductionConfig ->
    Logger ->
    OraclePool ->
    ByteString ->
    Maybe ReductionEvidence ->
    IO (ByteString, Maybe ReductionEvidence)
reduceIterationWithEvidence reporter config logger pool initialSQL priorEvidence = do
    (sliced, reusableAnalysis) <-
        if reductionSlicing config
            then timedReductionStage reporter logger StageSlicing initialSQL do
                analysis <- analyzeSQL (poolMetricsStore pool) logger (reductionDialect config) initialSQL
                sliceWithAnalysis logger pool analysis initialSQL
            else pure (initialSQL, Nothing)
    (searched, ()) <-
        if reductionClosure config
            then timedReductionStage reporter logger StageClosureSearch sliced do
                analysis <-
                    case reusableAnalysis of
                        Just cached -> pure cached
                        Nothing -> analyzeSQL (poolMetricsStore pool) logger (reductionDialect config) sliced
                out <- closureSearchWithAnalysis logger pool sliced analysis
                pure (out, ())
            else timedReductionStage reporter logger StageGraphFreeSearch sliced do
                out <- statementDdmin (reductionDialect config) pool sliced
                pure (out, ())
    fallback <-
        if reductionClosure config && reductionUnrestricted config
            then
                fst <$> timedReductionStage reporter logger StageUnrestrictedFallback searched do
                    out <- statementDdmin (reductionDialect config) pool searched
                    pure (out, ())
            else pure searched
    interleaveStages fallback
    where
        interleaveStages base
            | Just evidence <- priorEvidence
            , evidenceBytes evidence == base =
                -- The previous iteration already drove the interleave to a
                -- fixed point on these exact bytes; re-running would only
                -- replay cache hits.
                pure (base, priorEvidence)
            | otherwise = case (reductionUnrestricted config, reductionIntraStatement config) of
                (True, True) -> do
                    (out, minimal) <- interleaveFixedPoint (1 :: Int) base
                    pure (out, Just (ReductionEvidence out minimal))
                (True, False) -> do
                    (result, stats) <-
                        timedReductionStage reporter logger StageStatementAudit base do
                            (r, s) <- statementAuditWithLogger logger (reductionDialect config) pool base
                            pure (r, s)
                    pure (result, Just (ReductionEvidence result (auditMinimalityConfirmed stats)))
                (False, True) -> do
                    (out, _) <-
                        timedReductionStage reporter logger StageIntraStatementRewrites base do
                            r <-
                                reduceSyntaxWithDialect
                                    logger
                                    (columnParseOptions (reductionColumnMode config) (reductionDialect config))
                                    pool
                                    (const True)
                                    (reductionColumnMode config)
                                    base
                            pure (r, ())
                    pure (out, Just (ReductionEvidence out False))
                (False, False) -> pure (base, Just (ReductionEvidence base False))

        interleaveFixedPoint iter current = do
            (afterAudit, auditStats) <-
                timedReductionStage reporter logger StageStatementAudit current do
                    (r, s) <- statementAuditWithLogger logger (reductionDialect config) pool current
                    pure (r, s)
            afterSyntax <-
                fst <$> timedReductionStage reporter logger StageIntraStatementRewrites afterAudit do
                    out <-
                        reduceSyntaxWithDialect
                            logger
                            (columnParseOptions (reductionColumnMode config) (reductionDialect config))
                            pool
                            (const True)
                            (reductionColumnMode config)
                            afterAudit
                    pure (out, ())
            if BS.length afterSyntax < BS.length current
                then do
                    logInfo'
                        logger
                        "occam::reducer"
                        "Interleaved reduction step accepted"
                        [ ("interleave_iteration", toJSON iter)
                        , ("bytes_before", toJSON (BS.length current))
                        , ("bytes_after", toJSON (BS.length afterSyntax))
                        ]
                    interleaveFixedPoint (iter + 1) afterSyntax
                else do
                    logInfo'
                        logger
                        "occam::reducer"
                        "Interleaved audit and syntax global fixed point reached"
                        [ ("total_interleave_iterations", toJSON iter)
                        , ("final_bytes", toJSON (BS.length current))
                        ]
                    pure (afterSyntax, auditMinimalityConfirmed auditStats)

data SyntaxStats = SyntaxStats
    { syntaxAttempts :: !Int
    , syntaxAccepted :: !Int
    , syntaxElapsed :: !Word64
    }

-- | Canonical syntax-edit category order.
allEditCategories :: [ParserEditCategory]
allEditCategories = [EditClause, EditList, EditColumn, EditConstraint, EditType, EditExpression, EditLiteral]

isSafeClauseLabel :: T.Text -> Bool
isSafeClauseLabel label =
    label
        `elem` [ "remove-order_by"
               , "remove-limit"
               , "remove-offset"
               , "remove-fetch"
               ]

isUnwrapExpression :: ParserEdit -> Bool
isUnwrapExpression edit =
    case parserEditLabel edit of
        "simplify-parenthesized_expression" -> True
        "simplify-scalar-subquery" -> True
        label
            | "unwrap-" `T.isPrefixOf` label -> True
            | label `elem` ["simplify-unary_expression", "simplify-binary_expression", "simplify-in_list"] ->
                parserEditReplacement edit `notElem` ["0", "1", "'0'"]
        _ -> False

clauseEditRank :: ParserEdit -> Int
clauseEditRank edit =
    case parserEditLabel edit of
        label
            | isSafeClauseLabel label -> 0
        "remove-where" -> 2
        _ -> 1

expressionEditRank :: ParserEdit -> Int
expressionEditRank edit =
    if isUnwrapExpression edit then 0 else 1

reduceSyntaxWithDialect ::
    Logger ->
    ParseOptions ->
    OraclePool ->
    (ParserEdit -> Bool) ->
    ColumnMode ->
    ByteString ->
    IO ByteString
reduceSyntaxWithDialect logger options pool shouldRun mode initialSQL = do
    logInfo'
        logger
        "occam::reducer"
        "Using ordinal-forward syntax candidate schedule"
        [ ("search_policy", toJSON (syntaxPolicyName mode))
        , ("category_order", toJSON categoryOrderName)
        ]
    stats <- newIORef (SyntaxStats 0 0 0)
    result <- go stats initialSQL
    finalStats <- readIORef stats
    logInfo'
        logger
        "occam::reducer"
        "Syntax replacement summary"
        [ ("replacement_attempts", toJSON (syntaxAttempts finalStats))
        , ("replacement_accepted", toJSON (syntaxAccepted finalStats))
        , ("replacement_oracle_seconds", toJSON (seconds (syntaxElapsed finalStats)))
        , ("search_policy", toJSON (syntaxPolicyName mode))
        , ("category_order", toJSON categoryOrderName)
        ]
    pure result
    where
        store = poolMetricsStore pool
        categoryOrder = case mode of
            -- Off keeps the exact legacy six-category order.
            ColumnOff -> filter (/= EditColumn) allEditCategories
            _ -> allEditCategories
        categoryOrderName = T.intercalate "," (map editCategoryName categoryOrder)
        parseEdits currentSQL = do
            (_, units, columns) <- parseUnitsOrThrow store options currentSQL
            let edits =
                    concatMap
                        (filter shouldRun . maybe [] factEdits . unitFacts)
                        units
            -- Blocked bundles are counted once per snapshot with the
            -- frontend's cause; they never enter the candidate stream.
            mapM_
                (maybe (pure ()) (recordGeneratedSkipped store) . targetBlocked)
                columns
            -- Fold into ordered buckets once; category transitions reuse this snapshot.
            pure
                (length units, groupEdits edits, [target | target <- columns, isNothing (targetBlocked target)])

        groupEdits edits =
            let buckets = foldr (\e -> Map.insertWith (++) (parserEditCategory e) [e]) Map.empty edits
             in [(cat, sortCategoryEdits cat (Map.findWithDefault [] cat buckets)) | cat <- allEditCategories]

        sortCategoryEdits EditClause edits =
            -- Prioritize high-probability/safe clause edits (ORDER BY, LIMIT, OFFSET, FETCH)
            -- before structural clauses (GROUP BY, HAVING, WITH), and fragile WHERE last.
            sortOn clauseEditRank edits
        sortCategoryEdits EditExpression edits =
            -- Prioritize expression unwrapping (parens, subquery collapse, operand unwrap)
            -- before destructive constant replacements (0, 1, '0', exists).
            sortOn expressionEditRank edits
        sortCategoryEdits _ edits = edits

        go stats currentSQL = do
            (startCount, phases, bundles) <- parseEdits currentSQL
            visitPhases stats currentSQL startCount phases bundles categoryOrder False

        visitPhases stats currentSQL currentCount phases bundles [] shrunk
            | shrunk = visitPhases stats currentSQL currentCount phases bundles categoryOrder False
            | otherwise = pure currentSQL
        visitPhases stats currentSQL currentCount phases bundles (category : rest) shrunk = do
            (nextSQL, nextCount, nextPhases, nextBundles, categoryShrunk) <-
                exhaustCategory stats currentSQL currentCount phases bundles category
            visitPhases
                stats
                nextSQL
                nextCount
                nextPhases
                nextBundles
                rest
                (shrunk || categoryShrunk)
        -- After a strict shrink, reparse and resume at the accepted ordinal.
        -- Any candidates before that cursor are revisited in the next full cycle.
        -- Each window pulls testable edits from the unchanged snapshot and
        -- retires them in parser-emitted order.
        jobs = poolJobCount pool
        -- The Column stream in atomic mode is bundles first (joint moves
        -- subsume their singles), then the identical singles in the identical
        -- relative order as single mode. Any other category is singles only.
        initialStream phases bundles category
            | category == EditColumn && mode == ColumnAtomic =
                map BundleCandidate bundles ++ map SingleCandidate (phaseCandidates category phases)
            | otherwise = map SingleCandidate (phaseCandidates category phases)
        exhaustCategory stats sourceSQL sourceCount phases bundles category =
            sweep False 0 sourceSQL sourceCount phases bundles (initialStream phases bundles category)
            where
                sweep changed _ currentSQL currentCount currentPhases currentBundles [] =
                    pure (currentSQL, currentCount, currentPhases, currentBundles, changed)
                sweep changed ordinal currentSQL currentCount currentPhases currentBundles candidates = do
                    (windowTasks, nextOrdinal, rest) <- fillWindow currentSQL ordinal candidates
                    if null windowTasks
                        then sweep changed nextOrdinal currentSQL currentCount currentPhases currentBundles rest
                        else do
                            mapM_ (\(_, _, bytes) -> reportCandidateCount pool bytes currentCount) windowTasks
                            started <- getMonotonicTimeNSec
                            outcomes <-
                                testOrderedTagged pool [(candidateTag candidate, bytes) | (_, candidate, bytes) <- windowTasks]
                            finished <- getMonotonicTimeNSec
                            let elapsed = finished - started
                                retired = zip windowTasks outcomes
                                tested = length outcomes
                            case listToMaybe [(o, e, c) | ((o, e, c), (outcome, _)) <- retired, outcome == OracleAccepted] of
                                Just (acceptedOrdinal, candidate, acceptedBytes) -> do
                                    modifyIORef'
                                        stats
                                        ( \s ->
                                            s
                                                { syntaxAttempts = syntaxAttempts s + tested
                                                , syntaxAccepted = syntaxAccepted s + 1
                                                , syntaxElapsed = syntaxElapsed s + elapsed
                                                }
                                        )
                                    logAccept candidate currentSQL acceptedBytes
                                    (freshCount, freshPhases, freshBundles) <- parseEdits acceptedBytes
                                    let freshCandidates =
                                            drop acceptedOrdinal (initialStream freshPhases freshBundles category)
                                    sweep
                                        True
                                        acceptedOrdinal
                                        acceptedBytes
                                        freshCount
                                        freshPhases
                                        freshBundles
                                        freshCandidates
                                Nothing -> do
                                    modifyIORef'
                                        stats
                                        ( \s ->
                                            s
                                                { syntaxAttempts = syntaxAttempts s + tested
                                                , syntaxElapsed = syntaxElapsed s + elapsed
                                                }
                                        )
                                    sweep changed nextOrdinal currentSQL currentCount currentPhases currentBundles rest

                logAccept (SingleCandidate edit) currentSQL acceptedBytes =
                    let editStart = parserEditStart edit
                        editEnd = parserEditEnd edit
                        replacement = parserEditReplacement edit
                     in logInfo'
                            logger
                            "occam::reducer"
                            "Accepted byte-preserving syntax rewrite"
                            [ ("rewrite_kind", toJSON (parserEditLabel edit))
                            , ("rewrite_category", toJSON (parserEditCategory edit))
                            , ("span_start", toJSON editStart)
                            , ("span_end", toJSON editEnd)
                            , ("replacement", toJSON (TE.decodeUtf8Lenient replacement))
                            , ("bytes_before", toJSON (BS.length currentSQL))
                            , ("bytes_after", toJSON (BS.length acceptedBytes))
                            ]
                logAccept (BundleCandidate target) currentSQL acceptedBytes =
                    logInfo'
                        logger
                        "occam::reducer"
                        "Accepted atomic column co-reduction"
                        [ ("rewrite_kind", toJSON ("remove-column-bundle" :: T.Text))
                        , ("rewrite_category", toJSON EditColumn)
                        , ("table", toJSON (nameText (targetTable target)))
                        , ("column", toJSON (identText (targetColumn target)))
                        , ("sites", toJSON (length (targetSites target)))
                        , ("bytes_before", toJSON (BS.length currentSQL))
                        , ("bytes_after", toJSON (BS.length acceptedBytes))
                        ]

                fillWindow currentSQL ordinal = fill jobs ordinal []
                    where
                        fill _ o acc [] = pure (reverse acc, o, [])
                        fill 0 o acc rest = pure (reverse acc, o, rest)
                        fill n o acc (candidate : rest) = do
                            (built, buildMicros) <- timeCandidateBuild currentSQL candidate
                            case built of
                                Nothing -> do
                                    recordGeneratedSkipped store "nonshrinking"
                                    fill n (o + 1) acc rest
                                Just bytes -> do
                                    recordCandidateBuildTiming store (candidateTag candidate) buildMicros
                                    fill (n - 1) (o + 1) ((o, candidate, bytes) : acc) rest

                timeCandidateBuild currentSQL candidate = case store of
                    -- Unmeasured: no clocks, no extra forcing, same engine.
                    Nothing -> pure (buildCandidate, 0)
                    Just _ -> do
                        started <- getMonotonicTimeNSec
                        -- Force the assembled bytes before stopping the clock;
                        -- an O(1) length touch, never a deepseq.
                        let forced = maybe 0 BS.length buildCandidate
                        forced `seq` pure ()
                        finished <- getMonotonicTimeNSec
                        pure (buildCandidate, fromIntegral ((finished - started) `div` 1000))
                    where
                        buildCandidate = case candidate of
                            SingleCandidate edit -> singleBytes currentSQL edit
                            BundleCandidate target -> bundleBytes currentSQL target

        phaseCandidates category phases =
            fromMaybe [] (lookup category phases)

replaceSpan :: Int -> Int -> ByteString -> ByteString -> ByteString
replaceSpan start end replacement source =
    BS.concat [BS.take start source, replacement, BS.drop end source]

data SyntaxCandidate
    = SingleCandidate !ParserEdit
    | BundleCandidate !ColumnTarget

candidateTag :: SyntaxCandidate -> FamilyTag
candidateTag (SingleCandidate edit) = TagSyntax (parserEditCategory edit) (parserEditLabel edit)
candidateTag (BundleCandidate _) = TagBundle "remove-column-bundle"

singleBytes :: ByteString -> ParserEdit -> Maybe ByteString
singleBytes currentSQL edit
    | BS.length (parserEditReplacement edit) >= parserEditEnd edit - parserEditStart edit = Nothing
    | otherwise =
        Just
            (replaceSpan (parserEditStart edit) (parserEditEnd edit) (parserEditReplacement edit) currentSQL)

bundleBytes :: ByteString -> ColumnTarget -> Maybe ByteString
bundleBytes currentSQL target =
    applySpans [(siteStart site, siteEnd site) | site <- targetSites target] currentSQL

{- | Single-pass multi-span deletion assembly. Input spans must be sorted by
start (the frontend sorts once at build); this pass only verifies
non-overlap, splices kept slices with one 'BS.concat', and accepts only on
strict total shrink.
-}
applySpans :: [(Int, Int)] -> ByteString -> Maybe ByteString
applySpans spans source = case spans of
    [] -> Nothing
    (start, end) : rest
        | start < 0 || start >= end || end > sourceLength -> Nothing
        | otherwise -> go rest end [BS.take start source] (end - start)
    where
        sourceLength = BS.length source
        go [] previousEnd acc removed =
            let candidate = BS.concat (reverse (BS.drop previousEnd source : acc))
             in if removed > 0 && BS.length candidate < sourceLength then Just candidate else Nothing
        go ((start, end) : more) previousEnd acc removed
            | start < previousEnd || start >= end || end > sourceLength = Nothing
            | otherwise =
                go
                    more
                    end
                    (BS.take (start - previousEnd) (BS.drop previousEnd source) : acc)
                    (removed + (end - start))

{- | Time statement-candidate byte assembly when measured; otherwise return
the bytes untouched with no clocks and no extra forcing. The actual length
is forced only when a store is present. No candidate, order, or oracle
change either way.
-}
timeStatementBuild :: Maybe MetricsStore -> ByteString -> IO ByteString
timeStatementBuild Nothing bytes = pure bytes
timeStatementBuild (Just store) bytes = do
    started <- getMonotonicTimeNSec
    BS.length bytes `seq` pure ()
    finished <- getMonotonicTimeNSec
    recordCandidateBuildTiming
        (Just store)
        TagStatement
        (fromIntegral ((finished - started) `div` 1000))
    pure bytes
