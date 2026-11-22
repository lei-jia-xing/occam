-- | SQL test case reduction pipeline combining slicing, ddmin, and intra-statement rewrites.
module Occam.Reducer (
    ReductionConfig (..),
    ColumnMode (..),
    columnModeName,
    reductionSearchPolicy,
    defaultReductionConfig,
    isValidClosure,
    isValidClosureFast,
    closeSet,
    closeSetFast,
    closureDdmin,
    cutRemovedStatements,
    cutRemovedStatementIds,
    cutRemovedStatementInts,
    statementIds,
    statementIdsInt,
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
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (find, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Core (
    BarrierScope (GlobalScope),
    Ident (..),
    OracleOutcome (..),
    ScopedBarrier (barrierScope),
    SemanticFacts (factBarriers),
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
import Occam.Logger (Logger, logDebug', logInfo', logWarn, nullLogger)
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
import Occam.Slicer (
    backwardSliceFast,
    backwardSliceMany,
    backwardSliceManyFast,
 )

-- | Column co-reduction mode: disabled, single-site, or atomic multi-site bundles.
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

-- | Total policy description for the given reduction configuration.
reductionSearchPolicy :: ReductionConfig -> T.Text
reductionSearchPolicy config
    | not (reductionIntraStatement config) = syntaxPolicyName ColumnOff
    | otherwise = syntaxPolicyName (reductionColumnMode config)

-- | Reduction pipeline configuration options.
data ReductionConfig = ReductionConfig
    { reductionSlicing :: !Bool
    , reductionClosure :: !Bool
    , reductionIntraStatement :: !Bool
    , reductionUnrestricted :: !Bool
    , reductionDialect :: !SQLDialect
    , reductionColumnMode :: !ColumnMode
    }
    deriving (Eq, Show)

-- | Default reduction pipeline configuration.
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

-- | Evidence indicating the reduced SQL has satisfied statement-level audit minimality.
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

-- | Total 'last': the final element, or 'Nothing' for an empty list.
lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay [only] = Just only
lastMay (_ : rest) = lastMay rest

logStage :: Logger -> T.Text -> Word64 -> IO ()
logStage logger stage elapsed =
    logDebug'
        logger
        "reducer"
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

-- | Parse options for syntax-level rewrites.
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

-- | Check whether the retained statement set is closed under dependency predecessors.
isValidClosure :: DependencyGraph -> Set StatementId -> Bool
isValidClosure graph kept =
    let !keptInts = IntSet.fromList (map unStatementId (Set.toList kept))
     in isValidClosureFast graph keptInts

-- | Fast bitwise closure check directly over statement integers.
isValidClosureFast :: DependencyGraph -> IntSet.IntSet -> Bool
isValidClosureFast DependencyGraph {..} keptInts
    | IntMap.null graphFastEdges =
        let keptSet = Set.fromList (map StatementId (IntSet.toList keptInts))
         in all (isClosureOk keptSet) (Set.toList keptSet)
    | otherwise = all isClosureOkFast (IntSet.toList keptInts)
    where
        isClosureOk keptSet sid =
            let predecessors = Map.findWithDefault Set.empty sid graphEdges
             in predecessors `Set.isSubsetOf` keptSet

        isClosureOkFast sid =
            let predecessors = IntMap.findWithDefault IntSet.empty sid graphFastEdges
             in predecessors `IntSet.isSubsetOf` keptInts

-- | Transitive backward closure of a set of statement IDs.
closeSet :: DependencyGraph -> Set StatementId -> Set StatementId
closeSet graph roots = backwardSliceMany roots graph

-- | Transitive backward closure of a set of statement integers.
closeSetFast :: DependencyGraph -> IntSet.IntSet -> IntSet.IntSet
closeSetFast graph roots = backwardSliceManyFast roots graph

-- | Cut removed statements from the source SQL, preserving retained bytes and delimiters.
cutRemovedStatementIds :: ByteString -> [Statement] -> Set StatementId -> ByteString
cutRemovedStatementIds baseSQL allStatements keptIds =
    let !keptInts = IntSet.fromList (map unStatementId (Set.toList keptIds))
     in cutRemovedStatementInts baseSQL allStatements keptInts

-- | Fast statement byte surgery directly driven by an 'IntSet.IntSet' of retained statement IDs.
cutRemovedStatementInts :: ByteString -> [Statement] -> IntSet.IntSet -> ByteString
cutRemovedStatementInts baseSQL allStatements keptInts =
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
            | IntSet.member (unStatementId (stmtId stmt)) keptInts = chunks cursor rest
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
    cutRemovedStatementInts baseSQL allStatements (statementIdsInt keptStatements)

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

-- | Collect statement IDs as a bitwise 'IntSet.IntSet'.
statementIdsInt :: [Statement] -> IntSet.IntSet
statementIdsInt = IntSet.fromList . map (unStatementId . stmtId)

statementSQL :: ByteString -> [Statement] -> [Statement] -> ByteString
statementSQL = cutRemovedStatements

-- | Closure-constrained delta debugging.
closureDdmin :: OraclePool -> ByteString -> [Statement] -> DependencyGraph -> IO [Statement]
closureDdmin = closureDdminWithLogger nullLogger

closureDdminWithLogger ::
    Logger -> OraclePool -> ByteString -> [Statement] -> DependencyGraph -> IO [Statement]
closureDdminWithLogger logger pool baseSQL initialStatements graph = do
    closureCache <- newIORef HashMap.empty
    closureStats <- newIORef (ClosureStats 0 0 0 0)
    let weights = IntMap.fromList [(unStatementId (stmtId s), statementSpanBytes s) | s <- initialStatements]
        buildCandidate current seedInts = do
            modifyIORef' closureStats (\stats -> stats {closureRequests = closureRequests stats + 1})
            cached <- readIORef closureCache
            closedInts <-
                case HashMap.lookup seedInts cached of
                    Just result -> do
                        modifyIORef' closureStats (\stats -> stats {closureCacheHits = closureCacheHits stats + 1})
                        pure result
                    Nothing -> do
                        let result = closeSetFast graph seedInts
                        modifyIORef' closureCache (HashMap.insert seedInts result)
                        pure result
            let currentInts = statementIdsInt current
                keptInts = closedInts `IntSet.intersection` currentInts
                added = keptInts `IntSet.difference` seedInts
                gainedBytes = sum [IntMap.findWithDefault 0 sid weights | sid <- IntSet.toList added]
            modifyIORef'
                closureStats
                ( \stats ->
                    stats
                        { closureGainStatements = closureGainStatements stats + IntSet.size added
                        , closureGainBytes = closureGainBytes stats + gainedBytes
                        }
                )
            if isValidClosureFast graph keptInts
                then pure (Just (filter (\s -> IntSet.member (unStatementId (stmtId s)) keptInts) current))
                else pure Nothing
    result <-
        searchStatements
            buildCandidate
            pool
            baseSQL
            initialStatements
            initialStatements
    stats <- readIORef closureStats
    let gainStmts = closureGainStatements stats
        gainBytes = closureGainBytes stats
    if gainBytes > 0 || gainStmts > 0
        then
            logInfo'
                logger
                "reducer"
                "Closure search accepted reduction"
                [ ("gain_statements", toJSON gainStmts)
                , ("gain_bytes", toJSON gainBytes)
                ]
        else
            logDebug'
                logger
                "reducer"
                "Closure search found no further reductions"
                [ ("closure_requests", toJSON (closureRequests stats))
                , ("closure_cache_hits", toJSON (closureCacheHits stats))
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

-- | Verdict memo keyed by exact candidate bytes.
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

type CandidateBuilder = [Statement] -> IntSet.IntSet -> IO (Maybe [Statement])

plainCandidate :: CandidateBuilder
plainCandidate current keptInts =
    pure (Just (filter (\s -> IntSet.member (unStatementId (stmtId s)) keptInts) current))

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
                currentInts = statementIdsInt current
                currentSQL = cutRemovedStatementInts baseSQL allStatements currentInts
                currentLen = BS.length currentSQL
            subset <- trySubsets current currentInts currentLen chunks
            case subset of
                Just reduced -> go 2 reduced
                Nothing -> do
                    complement <- tryComplements current currentInts currentLen chunks
                    case complement of
                        Just reduced -> go (max 2 (n - 1)) reduced
                        Nothing
                            | n < length current -> go (min (length current) (2 * n)) current
                            | otherwise -> pure current

        trySubsets current currentInts currentLen chunks =
            trySeeds current currentInts currentLen (map statementIdsInt chunks)
        tryComplements current currentInts currentLen chunks =
            trySeeds
                current
                currentInts
                currentLen
                [currentInts `IntSet.difference` statementIdsInt chunk | chunk <- chunks]

        trySeeds current currentInts currentLen = pull
            where
                -- Pull and test eligible candidate windows incrementally.
                pull [] = pure Nothing
                pull seeds = collect jobs [] HashSet.empty seeds
                collect _ acc _ [] = testAndContinue (reverse acc) [] Nothing
                collect 0 acc _ rest = testAndContinue (reverse acc) rest Nothing
                collect n acc seen (seedInts : rest) = do
                    token <- classifySeed current currentInts currentLen seedInts
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

        classifySeed current currentInts currentLen seedInts
            | seedInts == currentInts = pure Nothing
            | otherwise = do
                maybeKept <- buildCandidate current seedInts
                case maybeKept of
                    Nothing -> pure Nothing
                    Just kept -> do
                        candidateSQL <-
                            timeStatementBuild
                                (poolMetricsStore pool)
                                (cutRemovedStatementInts baseSQL allStatements (statementIdsInt kept))
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

-- | Repeated single-statement deletion until reaching a 1-minimal fixed point.
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
                loop initialSQL statements (statementIdsInt statements) 0 0 0
            else pure (initialSQL, AuditStats 0 0 0 False)
    logAuditStats logger (snd outcome)
    pure outcome
    where
        jobs = poolJobCount pool
        -- The audit only removes whole statements, and those statements keep
        -- their original byte spans. Parse once, then track the retained set.
        loop baseSQL allStatements kept passCount attemptCount removalCount
            | IntSet.null kept =
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
                    let batch = take jobs [s | s <- statements, IntSet.member (unStatementId (stmtId s)) currentKept]
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
                                                (renderKept baseSQL allStatements (IntSet.delete (unStatementId (stmtId statement)) currentKept))
                                        pure (statement, bytes)
                                    )
                                    batch
                            let testables =
                                    [(statement, sql) | (statement, sql) <- candidates, BS.length sql < currentLen]
                            if null testables
                                then do
                                    mapM_ (const (recordGeneratedSkipped (poolMetricsStore pool) "nonshrinking")) batch
                                    case lastMay batch of
                                        Nothing -> pure (currentKept, attempts, removals)
                                        Just lastStatement ->
                                            go currentSQL currentKept (afterStatement lastStatement statements) attempts removals
                                else do
                                    mapM_ (\(_, sql) -> reportCandidateCount pool sql (IntSet.size currentKept - 1)) testables
                                    outcomes <- testOrderedTagged pool [(TagStatement, sql) | (_, sql) <- testables]
                                    let retired = zip testables outcomes
                                        tested = length outcomes
                                    case listToMaybe [(s, sql) | ((s, sql), (outcome, _)) <- retired, outcome == OracleAccepted] of
                                        Just (acceptedStatement, acceptedSQL) ->
                                            go
                                                acceptedSQL
                                                (IntSet.delete (unStatementId (stmtId acceptedStatement)) currentKept)
                                                (afterStatement acceptedStatement statements)
                                                (attempts + tested)
                                                (removals + 1)
                                        Nothing ->
                                            case lastMay batch of
                                                Nothing -> pure (currentKept, attempts, removals)
                                                Just lastStatement ->
                                                    go
                                                        currentSQL
                                                        currentKept
                                                        (afterStatement lastStatement statements)
                                                        (attempts + tested)
                                                        removals

                afterStatement target = goAfter
                    where
                        goAfter [] = []
                        goAfter (s : ss)
                            | stmtId s == stmtId target = ss
                            | otherwise = goAfter ss

        renderKept = cutRemovedStatementInts

data AuditStats = AuditStats
    { auditPasses :: !Int
    , auditAttempts :: !Int
    , auditRemovals :: !Int
    , auditMinimalityConfirmed :: !Bool
    }

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

-- | Binary search for the shortest interesting prefix of statements.
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
    logDebug'
        logger
        "reducer"
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

-- | Slice SQL statements using a precomputed dependency analysis.
sliceWithAnalysis ::
    Logger -> OraclePool -> SQLAnalysis -> ByteString -> IO (ByteString, Maybe SQLAnalysis)
sliceWithAnalysis logger pool analysis sql =
    case lastMay statements of
        Nothing -> keep sql
        Just lastStatement -> do
            let (lastRetained, lastCandidateRaw) = sliceFor lastStatement
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
                                    logDebug'
                                        logger
                                        "reducer"
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
    where
        statements = analyzedStatements analysis
        graph = analyzedGraph analysis
        keep result =
            pure (result, if result == sql then Just analysis else Nothing)
        sliceFor target =
            let sliceInts = backwardSliceFast (unStatementId (stmtId target)) graph
                retained = filter (\s -> IntSet.member (unStatementId (stmtId s)) sliceInts) statements
             in (retained, cutRemovedStatementInts sql statements sliceInts)
        acceptSlice strategy target retained candidate = do
            let StatementId targetId = stmtId target
            logInfo'
                logger
                "reducer"
                "Backward slicing accepted"
                [ ("seed_strategy", toJSON (strategy :: String))
                , ("target_statement_id", toJSON targetId)
                , ("statements_before", toJSON (length statements))
                , ("statements_after", toJSON (length retained))
                , ("bytes_before", toJSON (BS.length sql))
                , ("bytes_after", toJSON (BS.length candidate))
                ]
            pure candidate

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

-- | Reduce SQL test case using default configuration and dialect.
reduceSQL :: SQLDialect -> OraclePool -> ByteString -> IO ByteString
reduceSQL dialect = reduceSQLWithLogger dialect nullLogger

-- | Reduce SQL test case using custom logger and dialect.
reduceSQLWithLogger :: SQLDialect -> Logger -> OraclePool -> ByteString -> IO ByteString
reduceSQLWithLogger dialect =
    reduceSQLWithConfig (defaultReductionConfig {reductionDialect = dialect})

-- | Reduce SQL test case using custom configuration, logger, and oracle pool.
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
            logWarn logger "reducer" "Initial SQL candidate failed oracle check"
            pure (ReductionResult initialSQL Nothing)
        else loop (1 :: Int) initialSQL Nothing
    where
        loop iteration currentSQL priorEvidence = do
            logInfo'
                logger
                "reducer"
                "Starting reduction iteration"
                [ ("iteration", toJSON iteration)
                , ("current_bytes", toJSON (BS.length currentSQL))
                ]
            (nextSQL, evidence) <-
                reduceIterationWithEvidence reporter config logger pool currentSQL priorEvidence
            if BS.length nextSQL < BS.length currentSQL
                then loop (iteration + 1) nextSQL evidence
                else do
                    logInfo'
                        logger
                        "reducer"
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
    -- A reusable analysis means slicing kept these exact bytes.
    let useEarlyUnrestricted =
            reductionClosure config
                && reductionUnrestricted config
                && maybe False hasGlobalBarrier reusableAnalysis
    searchInput <-
        if useEarlyUnrestricted then unrestrictedSearch sliced else pure sliced
    (searched, ()) <-
        if reductionClosure config
            then timedReductionStage reporter logger StageClosureSearch searchInput do
                analysis <-
                    case reusableAnalysis of
                        Just cached | searchInput == sliced -> pure cached
                        _ -> analyzeSQL (poolMetricsStore pool) logger (reductionDialect config) searchInput
                out <- closureSearchWithAnalysis logger pool searchInput analysis
                pure (out, ())
            else timedReductionStage reporter logger StageGraphFreeSearch searchInput do
                out <- statementDdmin (reductionDialect config) pool searchInput
                pure (out, ())
    fallback <-
        if reductionClosure config
            && reductionUnrestricted config
            && (not useEarlyUnrestricted || searched /= searchInput)
            then unrestrictedSearch searched
            else pure searched
    interleaveStages fallback
    where
        hasGlobalBarrier = any (any ((== GlobalScope) . barrierScope) . factBarriers) . graphFacts . analyzedGraph

        unrestrictedSearch sql =
            fst <$> timedReductionStage reporter logger StageUnrestrictedFallback sql do
                out <- statementDdmin (reductionDialect config) pool sql
                pure (out, ())

        interleaveStages base
            | Just evidence <- priorEvidence
            , evidenceBytes evidence == base =
                -- Prior evidence already confirms fixed point on these bytes.
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
                        "reducer"
                        "Interleaved reduction step accepted"
                        [ ("interleave_iteration", toJSON iter)
                        , ("bytes_before", toJSON (BS.length current))
                        , ("bytes_after", toJSON (BS.length afterSyntax))
                        ]
                    interleaveFixedPoint (iter + 1) afterSyntax
                else do
                    logInfo'
                        logger
                        "reducer"
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

-- | Read-only configuration shared by the syntax-reduction phases.
data SyntaxContext = SyntaxContext
    { syntaxLogger :: !Logger
    , syntaxStore :: !(Maybe MetricsStore)
    , syntaxPool :: !OraclePool
    , syntaxShouldRun :: ParserEdit -> Bool
    , syntaxMode :: !ColumnMode
    , syntaxOptions :: !ParseOptions
    }

data SyntaxCandidate
    = SingleCandidate !ParserEdit
    | BundleCandidate !ColumnTarget

-- | Clause edit classification in priority order.
data ClauseEditClass = SafeClauseEdit | StructuralClauseEdit | FragileClauseEdit
    deriving (Eq, Ord, Show)

-- | Expression edit classification in priority order.
data ExpressionEditClass = UnwrapExpressionEdit | AggressiveExpressionEdit
    deriving (Eq, Ord, Show)

-- | Clause labels whose removal cannot change predicate truth.
safeClauseLabels :: Set T.Text
safeClauseLabels =
    Set.fromList
        [ "remove-order_by"
        , "remove-limit"
        , "remove-offset"
        , "remove-fetch"
        , "remove-distinct"
        , "simplify-window_spec_empty"
        , "remove-window_partition_by"
        , "remove-window_order_by"
        , "remove-window_frame"
        ]

-- | Predicate-bearing clause labels, attempted after structural clauses.
fragileClauseLabels :: Set T.Text
fragileClauseLabels = Set.singleton "remove-where"

-- | Expression labels that only collapse redundant structure.
unwrappedExpressionLabels :: Set T.Text
unwrappedExpressionLabels =
    Set.fromList
        [ "simplify-parenthesized_expression"
        , "simplify-scalar-subquery"
        , "simplify-cast"
        , "simplify-function_to_argument"
        , "simplify-case_result"
        , "simplify-case_else"
        ]

-- | Operand simplifications that unwrap unless they discard the operand value.
operandSimplifyLabels :: Set T.Text
operandSimplifyLabels =
    Set.fromList
        [ "simplify-unary_expression"
        , "simplify-binary_expression"
        , "simplify-in_list"
        , "simplify-in_subquery"
        , "simplify-between"
        , "simplify-like"
        , "simplify-is_null"
        ]

-- | Replacement constants that make an operand simplification destructive.
destructiveExpressionReplacements :: Set ByteString
destructiveExpressionReplacements = Set.fromList ["0", "1", "'0'"]

-- | Prefix used by the frontend for operand-unwrapping edits.
unwrapExpressionPrefix :: T.Text
unwrapExpressionPrefix = "unwrap-"

classifyClauseEdit :: ParserEdit -> ClauseEditClass
classifyClauseEdit edit
    | label `Set.member` safeClauseLabels = SafeClauseEdit
    | label `Set.member` fragileClauseLabels = FragileClauseEdit
    | otherwise = StructuralClauseEdit
    where
        label = parserEditLabel edit

classifyExpressionEdit :: ParserEdit -> ExpressionEditClass
classifyExpressionEdit edit
    | label `Set.member` unwrappedExpressionLabels = UnwrapExpressionEdit
    | label `Set.member` operandSimplifyLabels =
        if parserEditReplacement edit `Set.member` destructiveExpressionReplacements
            then AggressiveExpressionEdit
            else UnwrapExpressionEdit
    | unwrapExpressionPrefix `T.isPrefixOf` label = UnwrapExpressionEdit
    | otherwise = AggressiveExpressionEdit
    where
        label = parserEditLabel edit

-- | Named rank constants: lower ranks are tested first.
safeClauseRank, structuralClauseRank, fragileClauseRank :: Int
safeClauseRank = 0
structuralClauseRank = 1
fragileClauseRank = 2

unwrappedExpressionRank, aggressiveExpressionRank :: Int
unwrappedExpressionRank = 0
aggressiveExpressionRank = 1

clauseEditRank :: ParserEdit -> Int
clauseEditRank edit = case classifyClauseEdit edit of
    SafeClauseEdit -> safeClauseRank
    StructuralClauseEdit -> structuralClauseRank
    FragileClauseEdit -> fragileClauseRank

expressionEditRank :: ParserEdit -> Int
expressionEditRank edit = case classifyExpressionEdit edit of
    UnwrapExpressionEdit -> unwrappedExpressionRank
    AggressiveExpressionEdit -> aggressiveExpressionRank

-- | Column edits only participate when the column mode is enabled.
syntaxCategoryOrder :: ColumnMode -> [ParserEditCategory]
syntaxCategoryOrder ColumnOff = filter (/= EditColumn) allEditCategories
syntaxCategoryOrder _ = allEditCategories

syntaxCategoryOrderName :: ColumnMode -> T.Text
syntaxCategoryOrderName = T.intercalate "," . map editCategoryName . syntaxCategoryOrder

reduceSyntaxWithDialect ::
    Logger ->
    ParseOptions ->
    OraclePool ->
    (ParserEdit -> Bool) ->
    ColumnMode ->
    ByteString ->
    IO ByteString
reduceSyntaxWithDialect logger options pool shouldRun mode initialSQL = do
    let context =
            SyntaxContext
                { syntaxLogger = logger
                , syntaxStore = poolMetricsStore pool
                , syntaxPool = pool
                , syntaxShouldRun = shouldRun
                , syntaxMode = mode
                , syntaxOptions = options
                }
    logDebug'
        logger
        "reducer"
        "Using ordinal-forward syntax candidate schedule"
        [ ("search_policy", toJSON (syntaxPolicyName mode))
        , ("category_order", toJSON (syntaxCategoryOrderName mode))
        ]
    stats <- newIORef (SyntaxStats 0 0 0)
    result <- runSyntaxFixedPoint context stats initialSQL
    logSyntaxStats logger =<< readIORef stats
    pure result

-- | Reparse after each accepted edit and resweep until a full cycle makes no progress.
runSyntaxFixedPoint :: SyntaxContext -> IORef SyntaxStats -> ByteString -> IO ByteString
runSyntaxFixedPoint context stats currentSQL = do
    (startUnits, phases, bundles) <- parseSyntaxSnapshot context currentSQL
    sweepSyntaxCategories
        context
        stats
        currentSQL
        startUnits
        phases
        bundles
        (syntaxCategoryOrder (syntaxMode context))
        False

sweepSyntaxCategories ::
    SyntaxContext ->
    IORef SyntaxStats ->
    ByteString ->
    [StatementUnit] ->
    [(ParserEditCategory, [ParserEdit])] ->
    [ColumnTarget] ->
    [ParserEditCategory] ->
    Bool ->
    IO ByteString
sweepSyntaxCategories context stats currentSQL _ _ _ [] shrunk
    | shrunk = do
        (freshUnits, freshPhases, freshBundles) <- parseSyntaxSnapshot context currentSQL
        sweepSyntaxCategories
            context
            stats
            currentSQL
            freshUnits
            freshPhases
            freshBundles
            (syntaxCategoryOrder (syntaxMode context))
            False
    | otherwise = pure currentSQL
sweepSyntaxCategories context stats currentSQL currentUnits phases bundles (category : rest) shrunk = do
    (nextSQL, nextUnits, nextPhases, nextBundles, categoryShrunk) <-
        exhaustSyntaxCategory context stats currentSQL currentUnits phases bundles category
    sweepSyntaxCategories
        context
        stats
        nextSQL
        nextUnits
        nextPhases
        nextBundles
        rest
        (shrunk || categoryShrunk)

-- | Shift statement spans by a byte offset.
shiftStatementUnit :: Int -> StatementUnit -> StatementUnit
shiftStatementUnit delta unit =
    unit
        { unitStatement = shiftStatement delta (unitStatement unit)
        , unitFacts = fmap (shiftStatementFacts delta) (unitFacts unit)
        }

shiftStatement :: Int -> Statement -> Statement
shiftStatement delta stmt =
    stmt {stmtSpan = shiftSpan delta (stmtSpan stmt)}

shiftSpan :: Int -> Span -> Span
shiftSpan delta (Span start end) =
    Span (start + delta) (end + delta)

shiftStatementFacts :: Int -> StatementFacts -> StatementFacts
shiftStatementFacts delta facts =
    facts {factEdits = map (shiftParserEdit delta) (factEdits facts)}

shiftParserEdit :: Int -> ParserEdit -> ParserEdit
shiftParserEdit delta edit =
    edit
        { parserEditStart = parserEditStart edit + delta
        , parserEditEnd = parserEditEnd edit + delta
        }

shiftColumnTarget :: Int -> Int -> ColumnTarget -> ColumnTarget
shiftColumnTarget threshold delta target =
    target {targetSites = map shiftSite (targetSites target)}
    where
        shiftSite site
            | siteStart site >= threshold =
                site
                    { siteStart = siteStart site + delta
                    , siteEnd = siteEnd site + delta
                    }
            | otherwise = site

-- | Parse a single modified statement in isolation and shift its spans back to global coordinates.
parseSingleUnit ::
    SyntaxContext ->
    StatementUnit ->
    ParserEdit ->
    IO (Maybe (StatementUnit, Int))
parseSingleUnit context oldUnit edit = do
    let Span start_k end_k = stmtSpan (unitStatement oldUnit)
        oldRaw = stmtRaw (unitStatement oldUnit)
        localStart = parserEditStart edit - start_k
        localEnd = parserEditEnd edit - start_k
    if localStart < 0 || localEnd > BS.length oldRaw || localStart >= localEnd
        then pure Nothing
        else do
            let newRaw =
                    BS.concat
                        [ BS.take localStart oldRaw
                        , parserEditReplacement edit
                        , BS.drop localEnd oldRaw
                        ]
                delta = BS.length newRaw - (end_k - start_k)
            parsed <-
                parseSQLTimed
                    (syntaxStore context)
                    (kindOfOptions (syntaxOptions context))
                    (syntaxOptions context)
                    newRaw
            case parsed of
                Right result
                    | [singleUnit] <- parsedUnits result -> do
                        let shifted =
                                shiftStatementUnit
                                    start_k
                                    ( singleUnit
                                        { unitStatement =
                                            (unitStatement singleUnit)
                                                { stmtId = stmtId (unitStatement oldUnit)
                                                }
                                        }
                                    )
                        pure (Just (shifted, delta))
                _ -> pure Nothing

-- | Incrementally update syntax snapshot by reparsing only the modified statement.
updateSyntaxSnapshot ::
    SyntaxContext ->
    [StatementUnit] ->
    [ColumnTarget] ->
    ParserEdit ->
    ByteString ->
    IO ([StatementUnit], [(ParserEditCategory, [ParserEdit])], [ColumnTarget])
updateSyntaxSnapshot context currentUnits currentBundles edit acceptedBytes = do
    let editStart = parserEditStart edit
        editEnd = parserEditEnd edit
        targetUnit =
            find
                ( \u ->
                    let Span s e = stmtSpan (unitStatement u)
                     in editStart >= s && editEnd <= e
                )
                currentUnits
    case targetUnit of
        Nothing -> parseSyntaxSnapshot context acceptedBytes
        Just oldUnit -> do
            singleResult <- parseSingleUnit context oldUnit edit
            case singleResult of
                Nothing -> parseSyntaxSnapshot context acceptedBytes
                Just (updatedUnit, delta) -> do
                    let targetId = stmtId (unitStatement oldUnit)
                        updateUnit u
                            | stmtId (unitStatement u) == targetId = updatedUnit
                            | spanStartByte (stmtSpan (unitStatement u)) > spanStartByte (stmtSpan (unitStatement oldUnit)) =
                                shiftStatementUnit delta u
                            | otherwise = u
                        freshUnits = map updateUnit currentUnits
                        freshEdits =
                            concatMap
                                (filter (syntaxShouldRun context) . maybe [] factEdits . unitFacts)
                                freshUnits
                        oldEnd = spanEndByte (stmtSpan (unitStatement oldUnit))
                        freshBundles = map (shiftColumnTarget oldEnd delta) currentBundles
                    pure
                        ( freshUnits
                        , groupSyntaxEdits freshEdits
                        , freshBundles
                        )

-- | Parse a full snapshot into units, ordered edit buckets, and unblocked column bundles.
parseSyntaxSnapshot ::
    SyntaxContext ->
    ByteString ->
    IO ([StatementUnit], [(ParserEditCategory, [ParserEdit])], [ColumnTarget])
parseSyntaxSnapshot context currentSQL = do
    (_, units, columns) <-
        parseUnitsOrThrow (syntaxStore context) (syntaxOptions context) currentSQL
    let edits =
            concatMap
                (filter (syntaxShouldRun context) . maybe [] factEdits . unitFacts)
                units
    -- Record skipped metrics for blocked bundles.
    mapM_
        (maybe (pure ()) (recordGeneratedSkipped (syntaxStore context)) . targetBlocked)
        columns
    -- Fold into ordered buckets once; category transitions reuse this snapshot.
    pure
        ( units
        , groupSyntaxEdits edits
        , [target | target <- columns, isNothing (targetBlocked target)]
        )

groupSyntaxEdits :: [ParserEdit] -> [(ParserEditCategory, [ParserEdit])]
groupSyntaxEdits edits =
    let buckets = foldr (\e -> Map.insertWith (++) (parserEditCategory e) [e]) Map.empty edits
     in [(cat, sortSyntaxEdits cat (Map.findWithDefault [] cat buckets)) | cat <- allEditCategories]

sortSyntaxEdits :: ParserEditCategory -> [ParserEdit] -> [ParserEdit]
sortSyntaxEdits category edits = case category of
    EditClause ->
        -- Prioritize high-probability/safe clause edits (ORDER BY, LIMIT, OFFSET, FETCH)
        -- before structural clauses (GROUP BY, HAVING, WITH), and fragile WHERE last.
        sortOn clauseEditRank edits
    EditExpression ->
        -- Prioritize expression unwrapping (parens, subquery collapse, operand unwrap)
        -- before destructive constant replacements (0, 1, '0', exists).
        sortOn expressionEditRank edits
    _ -> edits

-- | Exhaustively apply edits of one category until no further reductions succeed.
exhaustSyntaxCategory ::
    SyntaxContext ->
    IORef SyntaxStats ->
    ByteString ->
    [StatementUnit] ->
    [(ParserEditCategory, [ParserEdit])] ->
    [ColumnTarget] ->
    ParserEditCategory ->
    IO (ByteString, [StatementUnit], [(ParserEditCategory, [ParserEdit])], [ColumnTarget], Bool)
exhaustSyntaxCategory context stats sourceSQL sourceUnits phases bundles category =
    sweep
        False
        0
        sourceSQL
        sourceUnits
        phases
        bundles
        (initialSyntaxStream context phases bundles category)
    where
        sweep changed _ currentSQL currentUnits currentPhases currentBundles [] =
            pure (currentSQL, currentUnits, currentPhases, currentBundles, changed)
        sweep changed ordinal currentSQL currentUnits currentPhases currentBundles candidates = do
            (windowTasks, nextOrdinal, rest) <- fillSyntaxWindow context currentSQL ordinal candidates
            if null windowTasks
                then sweep changed nextOrdinal currentSQL currentUnits currentPhases currentBundles rest
                else do
                    (tested, elapsed, accepted) <- testSyntaxWindow context (length currentUnits) windowTasks
                    modifyIORef' stats (recordSyntaxTest tested elapsed accepted)
                    case accepted of
                        Just (acceptedOrdinal, candidate, acceptedBytes) -> do
                            logAcceptedSyntaxEdit context candidate currentSQL acceptedBytes
                            (freshUnits, freshPhases, freshBundles) <- case candidate of
                                SingleCandidate edit
                                    | category /= EditColumn ->
                                        updateSyntaxSnapshot context currentUnits currentBundles edit acceptedBytes
                                _ -> parseSyntaxSnapshot context acceptedBytes
                            let freshCandidates =
                                    drop acceptedOrdinal (initialSyntaxStream context freshPhases freshBundles category)
                            sweep
                                True
                                acceptedOrdinal
                                acceptedBytes
                                freshUnits
                                freshPhases
                                freshBundles
                                freshCandidates
                        Nothing ->
                            sweep changed nextOrdinal currentSQL currentUnits currentPhases currentBundles rest

-- | Run one window of candidates and return the first accepted candidate.
testSyntaxWindow ::
    SyntaxContext ->
    Int ->
    [(Int, SyntaxCandidate, ByteString)] ->
    IO (Int, Word64, Maybe (Int, SyntaxCandidate, ByteString))
testSyntaxWindow context currentCount windowTasks = do
    mapM_ (\(_, _, bytes) -> reportCandidateCount (syntaxPool context) bytes currentCount) windowTasks
    started <- getMonotonicTimeNSec
    outcomes <-
        testOrderedTagged
            (syntaxPool context)
            [(candidateTag candidate, bytes) | (_, candidate, bytes) <- windowTasks]
    finished <- getMonotonicTimeNSec
    let retired = zip windowTasks outcomes
    pure
        ( length outcomes
        , finished - started
        , listToMaybe
            [ (ordinal, candidate, bytes)
            | ((ordinal, candidate, bytes), (outcome, _)) <- retired
            , outcome == OracleAccepted
            ]
        )

recordSyntaxTest :: Int -> Word64 -> Maybe a -> SyntaxStats -> SyntaxStats
recordSyntaxTest tested elapsed accepted stats =
    stats
        { syntaxAttempts = syntaxAttempts stats + tested
        , syntaxAccepted = syntaxAccepted stats + maybe 0 (const 1) accepted
        , syntaxElapsed = syntaxElapsed stats + elapsed
        }

-- | Initial candidate stream for the given syntax category.
initialSyntaxStream ::
    SyntaxContext ->
    [(ParserEditCategory, [ParserEdit])] ->
    [ColumnTarget] ->
    ParserEditCategory ->
    [SyntaxCandidate]
initialSyntaxStream context phases bundles category
    | category == EditColumn && syntaxMode context == ColumnAtomic =
        map BundleCandidate bundles ++ map SingleCandidate (phaseCandidates category phases)
    | otherwise = map SingleCandidate (phaseCandidates category phases)

phaseCandidates :: ParserEditCategory -> [(ParserEditCategory, [ParserEdit])] -> [ParserEdit]
phaseCandidates category = fromMaybe [] . lookup category

-- | Fill one oracle window, skipping candidates that do not strictly shrink.
fillSyntaxWindow ::
    SyntaxContext ->
    ByteString ->
    Int ->
    [SyntaxCandidate] ->
    IO ([(Int, SyntaxCandidate, ByteString)], Int, [SyntaxCandidate])
fillSyntaxWindow context currentSQL startOrdinal = fill (poolJobCount (syntaxPool context)) startOrdinal []
    where
        fill _ ordinal acc [] = pure (reverse acc, ordinal, [])
        fill 0 ordinal acc rest = pure (reverse acc, ordinal, rest)
        fill remaining ordinal acc (candidate : rest) = do
            (built, buildMicros) <- timeSyntaxCandidateBuild context currentSQL candidate
            case built of
                Nothing -> do
                    recordGeneratedSkipped (syntaxStore context) "nonshrinking"
                    fill remaining (ordinal + 1) acc rest
                Just bytes -> do
                    recordCandidateBuildTiming (syntaxStore context) (candidateTag candidate) buildMicros
                    fill (remaining - 1) (ordinal + 1) ((ordinal, candidate, bytes) : acc) rest

-- | Assemble candidate bytes with optional build timing.
timeSyntaxCandidateBuild ::
    SyntaxContext -> ByteString -> SyntaxCandidate -> IO (Maybe ByteString, Int)
timeSyntaxCandidateBuild context currentSQL candidate = case syntaxStore context of
    -- Unmeasured: no clocks, no extra forcing, same engine.
    Nothing -> pure (buildCandidate, 0)
    Just _ -> do
        started <- getMonotonicTimeNSec
        -- Force the assembled bytes before stopping the clock; an O(1) length
        -- touch, never a deepseq.
        let forced = maybe 0 BS.length buildCandidate
        forced `seq` pure ()
        finished <- getMonotonicTimeNSec
        pure (buildCandidate, fromIntegral ((finished - started) `div` 1000))
    where
        buildCandidate = case candidate of
            SingleCandidate edit -> singleBytes currentSQL edit
            BundleCandidate target -> bundleBytes currentSQL target

logAcceptedSyntaxEdit :: SyntaxContext -> SyntaxCandidate -> ByteString -> ByteString -> IO ()
logAcceptedSyntaxEdit context (SingleCandidate edit) currentSQL acceptedBytes =
    let editStart = parserEditStart edit
        editEnd = parserEditEnd edit
        replacement = parserEditReplacement edit
     in logInfo'
            (syntaxLogger context)
            "reducer"
            "Accepted byte-preserving syntax rewrite"
            [ ("rewrite_kind", toJSON (parserEditLabel edit))
            , ("rewrite_category", toJSON (parserEditCategory edit))
            , ("span_start", toJSON editStart)
            , ("span_end", toJSON editEnd)
            , ("replacement", toJSON (TE.decodeUtf8Lenient replacement))
            , ("bytes_before", toJSON (BS.length currentSQL))
            , ("bytes_after", toJSON (BS.length acceptedBytes))
            ]
logAcceptedSyntaxEdit context (BundleCandidate target) currentSQL acceptedBytes =
    logInfo'
        (syntaxLogger context)
        "reducer"
        "Accepted atomic column co-reduction"
        [ ("rewrite_kind", toJSON ("remove-column-bundle" :: T.Text))
        , ("rewrite_category", toJSON EditColumn)
        , ("table", toJSON (nameText (targetTable target)))
        , ("column", toJSON (identText (targetColumn target)))
        , ("sites", toJSON (length (targetSites target)))
        , ("bytes_before", toJSON (BS.length currentSQL))
        , ("bytes_after", toJSON (BS.length acceptedBytes))
        ]

logSyntaxStats :: Logger -> SyntaxStats -> IO ()
logSyntaxStats logger finalStats =
    let accepted = syntaxAccepted finalStats
     in if accepted > 0
            then
                logInfo'
                    logger
                    "reducer"
                    "Syntax rewrites accepted"
                    [ ("accepted_edits", toJSON accepted)
                    , ("attempts", toJSON (syntaxAttempts finalStats))
                    , ("oracle_seconds", toJSON (seconds (syntaxElapsed finalStats)))
                    ]
            else
                logDebug'
                    logger
                    "reducer"
                    "Syntax replacement completed with no accepted edits"
                    [ ("attempts", toJSON (syntaxAttempts finalStats))
                    , ("oracle_seconds", toJSON (seconds (syntaxElapsed finalStats)))
                    ]

replaceSpan :: Int -> Int -> ByteString -> ByteString -> ByteString
replaceSpan start end replacement source
    | BS.null replacement =
        let !prefix = BS.take start source
            !suffix = BS.drop end source
         in BS.append prefix suffix
    | start == 0 =
        BS.append replacement (BS.drop end source)
    | end == BS.length source =
        BS.append (BS.take start source) replacement
    | otherwise =
        BS.concat [BS.take start source, replacement, BS.drop end source]

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

-- | Apply non-overlapping deletion spans to the source bytes in a single pass.
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

-- | Record statement assembly time if metrics are enabled.
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
