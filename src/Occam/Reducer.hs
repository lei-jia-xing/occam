module Occam.Reducer (
    ReductionConfig (..),
    defaultReductionConfig,
    isValidClosure,
    closeSet,
    closureDdmin,
    cutRemovedStatements,
    cutRemovedStatementIds,
    partitionChunks,
    pruneClauses,
    simplifyExpressions,
    reduceSQL,
    reduceSQLWithLogger,
    reduceSQLWithConfig,
    statementDdmin,
    statementAudit,
    statementAuditWithLogger,
    AuditStats (..),
    findInterestingPrefix,
) where

import Data.Aeson (toJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Core (
    OracleOutcome (..),
    SessionInit (..),
    Span (..),
    Statement (..),
    StatementId (..),
    oracleAccepts,
 )
import Occam.Dependency (
    DependencyGraph (..),
    analyzeStatements,
 )
import Occam.Logger (Logger, logInfo', logWarn, nullLogger)
import Occam.Parser (
    ParseOptions (..),
    ParsedSQL (..),
    ParserEdit (..),
    ParserEditCategory (..),
    SQLDialect (..),
    StatementFacts (..),
    StatementUnit (..),
    defaultParseOptions,
    defaultSQLDialect,
    parseSQL,
 )
import Occam.Pool (OraclePool, poolJobCount, poolOracle, testOrdered)
import Occam.Slicer (backwardSlice, backwardSliceMany)

data ReductionConfig = ReductionConfig
    { reductionSlicing :: !Bool
    , reductionClosure :: !Bool
    , reductionIntraStatement :: !Bool
    , reductionUnrestricted :: !Bool
    , reductionDialect :: !SQLDialect
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
        }

seconds :: Word64 -> Double
seconds ns = fromIntegral ns / 1000000000

logStage :: Logger -> T.Text -> Word64 -> IO ()
logStage logger stage elapsed =
    logInfo'
        logger
        "occam::reducer"
        ("Finished " <> stage)
        [("stage", toJSON stage), ("duration_seconds", toJSON (seconds elapsed))]

timedStage :: Logger -> T.Text -> IO a -> IO a
timedStage logger stage action = do
    started <- getMonotonicTimeNSec
    result <- action
    finished <- getMonotonicTimeNSec
    logStage logger stage (finished - started)
    pure result

parseUnitsOrThrow :: ParseOptions -> ByteString -> IO (SessionInit, [StatementUnit])
parseUnitsOrThrow options sql = do
    parsed <- parseSQL options sql
    case parsed of
        Left parserError -> ioError (userError ("SQL parser transport failure: " ++ show parserError))
        Right result -> pure (parsedInitialSession result, parsedUnits result)

{- | The syntax pass alone opts into list-chunk deletions; statement-only
parses keep the legacy payload without chunk materialization.
-}
syntaxParseOptions :: SQLDialect -> ParseOptions
syntaxParseOptions dialect = (defaultParseOptions dialect) {parseChunkEdits = True}

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
    { fbEvaluated :: IORef (Map.Map ByteString CandidateVerdict)
    }

newFeedback :: IO Feedback
newFeedback = Feedback <$> newIORef Map.empty
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
                pull seeds = collect jobs [] Set.empty seeds
                collect _ acc _ [] = testAndContinue (reverse acc) [] Nothing
                collect 0 acc _ rest = testAndContinue (reverse acc) rest Nothing
                collect n acc seen (seedIds : rest) = do
                    token <- classifySeed current currentIds currentLen seedIds
                    case token of
                        Nothing -> collect (n - 1) acc seen rest
                        Just (SeedAccepted kept) -> testAndContinue (reverse acc) rest (Just kept)
                        Just (SeedFresh kept sql)
                            | Set.member sql seen -> collect (n - 1) acc seen rest
                            | otherwise -> collect (n - 1) ((kept, sql) : acc) (Set.insert sql seen) rest
                testAndContinue batch rest barrier = do
                    accepted <- testBatch batch
                    case accepted of
                        Just kept -> pure (Just kept)
                        Nothing -> case barrier of
                            Just kept -> pure (Just kept)
                            Nothing -> pull rest

        testBatch [] = pure Nothing
        testBatch batch = do
            outcomes <- testOrdered pool [sql | (_, sql) <- batch]
            let retired = zip batch outcomes
            mapM_ recordRetired retired
            pure $ listToMaybe [kept | ((kept, _), (outcome, _)) <- retired, outcome == OracleAccepted]

        recordRetired ((_, sql), (outcome, _)) =
            let verdict = if outcome == OracleAccepted then CandidateAccepted else CandidateRejected
             in modifyIORef' (fbEvaluated feedback) (Map.insert sql verdict)

        classifySeed current currentIds currentLen seedIds
            | seedIds == currentIds = pure Nothing
            | otherwise = do
                maybeKept <- buildCandidate current seedIds
                case maybeKept of
                    Nothing -> pure Nothing
                    Just kept -> do
                        let candidateSQL = statementSQL baseSQL allStatements kept
                        if BS.length candidateSQL >= currentLen
                            then pure Nothing
                            else do
                                evaluated <- readIORef (fbEvaluated feedback)
                                case Map.lookup candidateSQL evaluated of
                                    Just CandidateAccepted -> pure (Just (SeedAccepted kept))
                                    Just CandidateRejected -> pure Nothing
                                    Nothing -> pure (Just (SeedFresh kept candidateSQL))

-- | Graph-free statement ddmin over original top-level statement byte spans.
statementDdmin :: SQLDialect -> OraclePool -> ByteString -> IO ByteString
statementDdmin dialect pool initialSQL = do
    initialOK <- oracleAccepts (poolOracle pool) initialSQL
    if not initialOK
        then pure initialSQL
        else do
            (_, units) <- parseUnitsOrThrow (defaultParseOptions dialect) initialSQL
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
                (_, units) <- parseUnitsOrThrow (defaultParseOptions dialect) initialSQL
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
                                candidates =
                                    [ (statement, renderKept baseSQL allStatements (Set.delete (stmtId statement) currentKept))
                                    | statement <- batch
                                    ]
                                testables =
                                    [(statement, sql) | (statement, sql) <- candidates, BS.length sql < currentLen]
                            if null testables
                                then go currentSQL currentKept (afterStatement (last batch) statements) attempts removals
                                else do
                                    outcomes <- testOrdered pool [sql | (_, sql) <- testables]
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
                    candidate = statementSQL baseSQL statements prefix
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

analyzeSQL :: Logger -> SQLDialect -> ByteString -> IO SQLAnalysis
analyzeSQL logger dialect sql = do
    started <- getMonotonicTimeNSec
    (initialSession, units) <- parseUnitsOrThrow (defaultParseOptions dialect) sql
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
                (lastRetained, lastCandidate) = sliceFor lastStatement
            lastAccepted <-
                if BS.length lastCandidate < BS.length sql
                    then oracleAccepts (poolOracle pool) lastCandidate
                    else pure False
            let tryPrefix bestCandidate = do
                    (maybeTarget, prefixTests) <- findInterestingPrefix pool sql statements
                    case maybeTarget of
                        Nothing -> pure bestCandidate
                        Just (targetStatement, prefixSQL, prefixLength) -> do
                            let (retained, candidate) = sliceFor targetStatement
                            if BS.length candidate >= BS.length sql
                                then pure bestCandidate
                                else do
                                    let reusedPrefixResult = candidate == prefixSQL
                                    ok <- if reusedPrefixResult then pure True else oracleAccepts (poolOracle pool) candidate
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
reduceSQLWithConfig config logger pool initialSQL = do
    initialOK <- oracleAccepts (poolOracle pool) initialSQL
    if not initialOK
        then do
            logWarn logger "occam::reducer" "Initial SQL candidate failed oracle check"
            pure initialSQL
        else loop (1 :: Int) initialSQL
    where
        loop iteration currentSQL = do
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
            nextSQL <- reduceIterationWithConfig config logger pool currentSQL
            if BS.length nextSQL < BS.length currentSQL
                then loop (iteration + 1) nextSQL
                else do
                    logInfo'
                        logger
                        "occam::reducer"
                        "Rewrite fixed point reached"
                        [ ("total_iterations", toJSON iteration)
                        , ("final_bytes", toJSON (BS.length currentSQL))
                        ]
                    pure currentSQL

reduceIterationWithConfig ::
    ReductionConfig -> Logger -> OraclePool -> ByteString -> IO ByteString
reduceIterationWithConfig config logger pool initialSQL = do
    (sliced, reusableAnalysis) <-
        if reductionSlicing config
            then timedStage logger "backward-slicing" do
                analysis <- analyzeSQL logger (reductionDialect config) initialSQL
                sliceWithAnalysis logger pool analysis initialSQL
            else pure (initialSQL, Nothing)
    searched <-
        if reductionClosure config
            then timedStage logger "closure-search" do
                analysis <-
                    case reusableAnalysis of
                        Just cached -> pure cached
                        Nothing -> analyzeSQL logger (reductionDialect config) sliced
                closureSearchWithAnalysis logger pool sliced analysis
            else
                timedStage
                    logger
                    "graph-free-statement-search"
                    (statementDdmin (reductionDialect config) pool sliced)
    fallback <-
        if reductionClosure config && reductionUnrestricted config
            then
                timedStage
                    logger
                    "unrestricted-fallback"
                    (statementDdmin (reductionDialect config) pool searched)
            else pure searched
    interleaveStages fallback
    where
        interleaveStages base =
            case (reductionUnrestricted config, reductionIntraStatement config) of
                (True, True) -> interleaveFixedPoint (1 :: Int) base
                (True, False) -> do
                    started <- getMonotonicTimeNSec
                    (result, _) <- statementAuditWithLogger logger (reductionDialect config) pool base
                    finished <- getMonotonicTimeNSec
                    logStage logger "statement-audit" (finished - started)
                    pure result
                (False, True) ->
                    timedStage
                        logger
                        "intra-statement-rewrites"
                        ( reduceSyntaxWithDialect
                            logger
                            (syntaxParseOptions (reductionDialect config))
                            pool
                            (const True)
                            base
                        )
                (False, False) -> pure base

        interleaveFixedPoint iter current = do
            started <- getMonotonicTimeNSec
            (afterAudit, _) <- statementAuditWithLogger logger (reductionDialect config) pool current
            finished <- getMonotonicTimeNSec
            logStage logger "statement-audit" (finished - started)
            afterSyntax <-
                timedStage
                    logger
                    "intra-statement-rewrites"
                    ( reduceSyntaxWithDialect
                        logger
                        (syntaxParseOptions (reductionDialect config))
                        pool
                        (const True)
                        afterAudit
                    )
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
                    pure afterSyntax

pruneClauses :: SQLDialect -> OraclePool -> ByteString -> IO ByteString
pruneClauses dialect pool =
    reduceSyntaxWithDialect
        nullLogger
        (defaultParseOptions dialect)
        pool
        ((== EditClause) . parserEditCategory)

-- Expression-only public pass.
simplifyExpressions :: SQLDialect -> OraclePool -> ByteString -> IO ByteString
simplifyExpressions dialect pool =
    reduceSyntaxWithDialect
        nullLogger
        (defaultParseOptions dialect)
        pool
        ((== EditExpression) . parserEditCategory)

data SyntaxStats = SyntaxStats
    { syntaxAttempts :: !Int
    , syntaxAccepted :: !Int
    , syntaxElapsed :: !Word64
    }

reduceSyntaxWithDialect ::
    Logger -> ParseOptions -> OraclePool -> (ParserEdit -> Bool) -> ByteString -> IO ByteString
reduceSyntaxWithDialect logger options pool shouldRun initialSQL = do
    logInfo'
        logger
        "occam::reducer"
        "Using ordinal-forward syntax candidate schedule"
        [ ("search_policy", toJSON ("phase-forward-sweep-list-chunks" :: T.Text))
        , ("category_order", toJSON ("clause,list,constraint,type,expression,literal" :: T.Text))
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
        , ("search_policy", toJSON ("phase-forward-sweep-list-chunks" :: T.Text))
        , ("category_order", toJSON ("clause,list,constraint,type,expression,literal" :: T.Text))
        ]
    pure result
    where
        categoryOrder =
            [ EditClause
            , EditList
            , EditConstraint
            , EditType
            , EditExpression
            , EditLiteral
            ]
        parseEdits currentSQL = do
            (_, units) <- parseUnitsOrThrow options currentSQL
            let edits =
                    concatMap
                        (filter shouldRun . maybe [] factEdits . unitFacts)
                        units
            -- Fold into ordered buckets once; category transitions reuse this snapshot.
            pure (groupEdits edits)

        groupEdits edits =
            let (clauses, lists, constraints, types, expressions, literals) =
                    foldr distribute ([], [], [], [], [], []) edits
             in [ (EditClause, clauses)
                , (EditList, lists)
                , (EditConstraint, constraints)
                , (EditType, types)
                , (EditExpression, expressions)
                , (EditLiteral, literals)
                ]

        distribute edit (clauses, lists, constraints, types, expressions, literals) =
            case parserEditCategory edit of
                EditClause -> (edit : clauses, lists, constraints, types, expressions, literals)
                EditList -> (clauses, edit : lists, constraints, types, expressions, literals)
                EditConstraint -> (clauses, lists, edit : constraints, types, expressions, literals)
                EditType -> (clauses, lists, constraints, edit : types, expressions, literals)
                EditExpression -> (clauses, lists, constraints, types, edit : expressions, literals)
                EditLiteral -> (clauses, lists, constraints, types, expressions, edit : literals)

        go stats currentSQL = do
            phases <- parseEdits currentSQL
            visitPhases stats currentSQL phases categoryOrder False

        visitPhases stats currentSQL phases [] shrunk
            | shrunk = visitPhases stats currentSQL phases categoryOrder False
            | otherwise = pure currentSQL
        visitPhases stats currentSQL phases (category : rest) shrunk = do
            (nextSQL, nextPhases, categoryShrunk) <-
                exhaustCategory stats currentSQL phases category
            visitPhases
                stats
                nextSQL
                nextPhases
                rest
                (shrunk || categoryShrunk)
        -- After a strict shrink, reparse and resume at the accepted ordinal.
        -- Any candidates before that cursor are revisited in the next full cycle.
        -- Each window pulls testable edits from the unchanged snapshot and
        -- retires them in parser-emitted order.
        jobs = poolJobCount pool
        exhaustCategory stats sourceSQL phases category =
            sweep False 0 sourceSQL phases (phaseCandidates category phases)
            where
                sweep changed _ currentSQL currentPhases [] =
                    pure (currentSQL, currentPhases, changed)
                sweep changed ordinal currentSQL currentPhases edits = do
                    let (windowTasks, nextOrdinal, rest) = fillWindow currentSQL ordinal edits
                    if null windowTasks
                        then sweep changed nextOrdinal currentSQL currentPhases rest
                        else do
                            started <- getMonotonicTimeNSec
                            outcomes <- testOrdered pool [candidate | (_, _, candidate) <- windowTasks]
                            finished <- getMonotonicTimeNSec
                            let elapsed = finished - started
                                retired = zip windowTasks outcomes
                                tested = length outcomes
                            case listToMaybe [(o, e, c) | ((o, e, c), (outcome, _)) <- retired, outcome == OracleAccepted] of
                                Just (acceptedOrdinal, edit, candidate) -> do
                                    let editStart = parserEditStart edit
                                        editEnd = parserEditEnd edit
                                        replacement = parserEditReplacement edit
                                    modifyIORef'
                                        stats
                                        ( \s ->
                                            s
                                                { syntaxAttempts = syntaxAttempts s + tested
                                                , syntaxAccepted = syntaxAccepted s + 1
                                                , syntaxElapsed = syntaxElapsed s + elapsed
                                                }
                                        )
                                    logInfo'
                                        logger
                                        "occam::reducer"
                                        "Accepted byte-preserving syntax rewrite"
                                        [ ("rewrite_kind", toJSON (parserEditLabel edit))
                                        , ("rewrite_category", toJSON (parserEditCategory edit))
                                        , ("span_start", toJSON editStart)
                                        , ("span_end", toJSON editEnd)
                                        , ("replacement", toJSON (TE.decodeUtf8Lenient replacement))
                                        , ("bytes_before", toJSON (BS.length currentSQL))
                                        , ("bytes_after", toJSON (BS.length candidate))
                                        ]
                                    freshPhases <- parseEdits candidate
                                    let freshCandidates =
                                            drop acceptedOrdinal (phaseCandidates category freshPhases)
                                    sweep
                                        True
                                        acceptedOrdinal
                                        candidate
                                        freshPhases
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
                                    sweep changed nextOrdinal currentSQL currentPhases rest

                fillWindow currentSQL ordinal = fill jobs ordinal []
                    where
                        fill _ o acc [] = (reverse acc, o, [])
                        fill 0 o acc rest = (reverse acc, o, rest)
                        fill n o acc (edit : rest)
                            | BS.length (parserEditReplacement edit) >= parserEditEnd edit - parserEditStart edit =
                                fill n (o + 1) acc rest
                            | otherwise =
                                fill
                                    (n - 1)
                                    (o + 1)
                                    ( ( o
                                      , edit
                                      , replaceSpan (parserEditStart edit) (parserEditEnd edit) (parserEditReplacement edit) currentSQL
                                      )
                                        : acc
                                    )
                                    rest

        phaseCandidates category phases =
            fromMaybe [] (lookup category phases)

replaceSpan :: Int -> Int -> ByteString -> ByteString -> ByteString
replaceSpan start end replacement source =
    BS.concat [BS.take start source, replacement, BS.drop end source]
