module Occam.Reducer (
    Oracle,
    SearchStrategy (..),
    ReductionConfig (..),
    defaultReductionConfig,
    isValidClosure,
    closeSet,
    closureDdmin,
    cutRemovedStatements,
    partitionChunks,
    pruneClauses,
    simplifyExpressions,
    reduceSQL,
    reduceSQLWithLogger,
    reduceSQLWithConfig,
    statementDdmin,
    statementAudit,
) where

import Data.Aeson (toJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (foldl', sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64, Word8)
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Core (Span (..), Statement (..), StatementId (..))
import Occam.Dependency (
    DependencyGraph (..),
    SQLDialect (..),
    analyzeStatementsWithDialect,
    defaultSQLDialect,
 )
import Occam.Logger (Logger, logInfo', logWarn, nullLogger)
import Occam.Slicer (backwardSlice)
import Occam.TreeSitter (
    Parser,
    TSNode,
    collectDescendants,
    cutSpan,
    extractStatementNodes,
    nodeNamedChildren,
    nodeSpan,
    nodeText,
    nodeType,
    replaceSpan,
    rootNode,
    withParsedSQL,
    withParser,
 )

-- | The oracle returns True exactly when the candidate preserves the target crash.
type Oracle = ByteString -> IO Bool

data SearchStrategy
    = CountDD
    | WeightedDD
    | CDD
    deriving (Eq, Show)

data ReductionConfig = ReductionConfig
    { reductionSlicing :: !Bool
    , reductionClosure :: !Bool
    , reductionIntraStatement :: !Bool
    , reductionUnrestricted :: !Bool
    , reductionSearch :: !SearchStrategy
    , reductionDialect :: !SQLDialect
    }
    deriving (Eq, Show)

defaultReductionConfig :: ReductionConfig
defaultReductionConfig =
    ReductionConfig
        { reductionSlicing = True
        , reductionClosure = True
        , reductionIntraStatement = True
        , reductionUnrestricted = True
        , reductionSearch = CountDD
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
closeSet graph statementIds =
    Set.unions [backwardSlice sid graph | sid <- Set.toList statementIds]

-- | Cut removed statement spans without unparsing or changing retained bytes.
cutRemovedStatements :: ByteString -> [Statement] -> [Statement] -> ByteString
cutRemovedStatements baseSQL allStatements keptStatements =
    cutSpans (map (absorbTrailing baseSQL) removedSpans) baseSQL
    where
        keptIds = Set.fromList (map stmtId keptStatements)
        removedSpans =
            [ stmtSpan statement
            | statement <- allStatements
            , Set.notMember (stmtId statement) keptIds
            ]

absorbTrailing :: ByteString -> Span -> Span
absorbTrailing source (Span start end) =
    let sourceLength = BS.length source
        afterSemicolon =
            if end < sourceLength && BS.index source end == 59
                then end + 1
                else end
        afterNewline =
            if afterSemicolon < sourceLength && BS.index source afterSemicolon == 10
                then afterSemicolon + 1
                else afterSemicolon
     in Span start afterNewline

cutSpans :: [Span] -> ByteString -> ByteString
cutSpans spans source =
    foldl' (\acc (Span start end) -> cutSpan start end acc) source (sortOn (Down . spanStartByte) spans)

-- | Split a list into at most n approximately equal, contiguous chunks.
partitionChunks :: Int -> [a] -> [[a]]
partitionChunks n xs
    | n <= 1 = [xs]
    | null xs = []
    | otherwise =
        let chunkSize = (length xs + n - 1) `div` n
            (chunk, rest) = splitAt chunkSize xs
         in chunk : partitionChunks (n - 1) rest

statementByteWeight :: ByteString -> Statement -> Int
-- WeightedDD balances contiguous groups by each statement's byte-span weight.
-- Its partitioning is inspired by Wddmin (Zhou et al., arXiv:2411.19410), not a token-weight claim.
statementByteWeight source statement =
    let Span start end = absorbTrailing source (stmtSpan statement)
     in end - start

weightedChunks :: Int -> (a -> Int) -> [a] -> [[a]]
weightedChunks n weight items
    | null items = []
    | n <= 1 = [items]
    | otherwise = split (min n (length items)) items
    where
        split parts remaining
            | parts <= 1 = [remaining]
            | null remaining = []
            | otherwise =
                let totalWeight = sum (map (toInteger . max 1 . weight) remaining)
                    maxCut = length remaining - (parts - 1)
                    cutAt = closestWeightCut parts totalWeight maxCut remaining
                    (chunk, rest) = splitAt cutAt remaining
                 in chunk : split (parts - 1) rest

        closestWeightCut parts totalWeight maxCut remaining =
            let (_, _, bestCut, _) =
                    foldl'
                        (chooseCut parts totalWeight)
                        (0, 0, 1, Nothing)
                        (zip [1 .. maxCut] (take maxCut remaining))
             in bestCut

        chooseCut parts totalWeight (_, previousWeight, bestCut, bestScore) (index, item) =
            let prefixWeight = previousWeight + toInteger (max 1 (weight item))
                score = abs (prefixWeight * toInteger parts - totalWeight)
             in case bestScore of
                    Nothing -> (index, prefixWeight, index, Just score)
                    Just previous
                        | score < previous -> (index, prefixWeight, index, Just score)
                    _ -> (index, prefixWeight, bestCut, bestScore)

chunksFor :: SearchStrategy -> Int -> ByteString -> [Statement] -> [[Statement]]
chunksFor CountDD count _ = partitionChunks count
chunksFor WeightedDD count source = weightedChunks count (statementByteWeight source)
chunksFor CDD count _ = partitionChunks count

statementIds :: [Statement] -> Set StatementId
statementIds = Set.fromList . map stmtId

statementSQL :: ByteString -> [Statement] -> [Statement] -> ByteString
statementSQL = cutRemovedStatements

-- | Closure-constrained delta debugging, retaining the historical CountDD API.
closureDdmin :: Oracle -> ByteString -> [Statement] -> DependencyGraph -> IO [Statement]
closureDdmin = closureDdminWithLogger nullLogger CountDD

closureDdminWithLogger ::
    Logger -> SearchStrategy -> Oracle -> ByteString -> [Statement] -> DependencyGraph -> IO [Statement]
closureDdminWithLogger logger strategy oracle baseSQL initialStatements graph = do
    closureCache <- newIORef Map.empty
    closureStats <- newIORef (ClosureStats 0 0 0 0)
    let weights = Map.fromList [(stmtId s, statementByteWeight baseSQL s) | s <- initialStatements]
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
        searchStatements strategy buildCandidate oracle baseSQL initialStatements initialStatements
    stats <- readIORef closureStats
    logInfo'
        logger
        "occam::reducer"
        "Closure search summary"
        [ ("strategy", toJSON (T.pack (show strategy)))
        ,
            ( "partition_weight"
            , toJSON (if strategy == WeightedDD then ("statement-bytes" :: T.Text) else "statement-count")
            )
        , ("closure_requests", toJSON (closureRequests stats))
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

searchStatements ::
    SearchStrategy ->
    CandidateBuilder ->
    Oracle ->
    ByteString ->
    [Statement] ->
    [Statement] ->
    IO [Statement]
searchStatements strategy buildCandidate oracle baseSQL allStatements initialStatements =
    case strategy of
        CDD -> searchCDD buildCandidate oracle baseSQL allStatements initialStatements
        _ -> searchDDMin strategy buildCandidate oracle baseSQL allStatements initialStatements

type CandidateBuilder = [Statement] -> Set StatementId -> IO (Maybe [Statement])

plainCandidate :: CandidateBuilder
plainCandidate current keptIds =
    pure (Just (filter (\s -> Set.member (stmtId s) keptIds) current))

searchDDMin ::
    SearchStrategy ->
    CandidateBuilder ->
    Oracle ->
    ByteString ->
    [Statement] ->
    [Statement] ->
    IO [Statement]
searchDDMin strategy buildCandidate oracle baseSQL allStatements = go 2
    where
        go _ [] = pure []
        go granularity current = do
            let n = min (length current) (max 1 granularity)
                chunks = chunksFor strategy n baseSQL current
            subset <- trySubsets current chunks
            case subset of
                Just reduced -> go 2 reduced
                Nothing -> do
                    complement <- tryComplements current chunks
                    case complement of
                        Just reduced -> go (max 2 (n - 1)) reduced
                        Nothing
                            | n < length current -> go (min (length current) (2 * n)) current
                            | otherwise -> pure current

        trySubsets _ [] = pure Nothing
        trySubsets current (chunk : rest) = do
            let seedIds = statementIds chunk
            candidate <- testSeed current seedIds
            case candidate of
                Just reduced -> pure (Just reduced)
                Nothing -> trySubsets current rest

        tryComplements _ [] = pure Nothing
        tryComplements current (chunk : rest) = do
            let seedIds = statementIds current `Set.difference` statementIds chunk
            candidate <- testSeed current seedIds
            case candidate of
                Just reduced -> pure (Just reduced)
                Nothing -> tryComplements current rest

        testSeed current seedIds
            | seedIds == statementIds current = pure Nothing
            | otherwise = do
                maybeKept <- buildCandidate current seedIds
                case maybeKept of
                    Nothing -> pure Nothing
                    Just kept -> do
                        let currentSQL = statementSQL baseSQL allStatements current
                            candidateSQL = statementSQL baseSQL allStatements kept
                        if BS.length candidateSQL >= BS.length currentSQL
                            then pure Nothing
                            else do
                                ok <- oracle candidateSQL
                                pure (if ok then Just kept else Nothing)

searchCDD ::
    CandidateBuilder -> Oracle -> ByteString -> [Statement] -> [Statement] -> IO [Statement]
searchCDD buildCandidate oracle baseSQL allStatements = rounds 0
    where
        rounds _ [] = pure []
        rounds roundNumber current = do
            let subsetSize = cddSubsetSize roundNumber (length current)
                partitions = chunksOf subsetSize current
            afterRound <- tryPartitions current partitions
            if subsetSize <= 1
                then pure afterRound
                else rounds (roundNumber + 1) afterRound

        tryPartitions current [] = pure current
        tryPartitions current (partition : rest)
            | null current = pure []
            | otherwise = do
                let retainedIds = statementIds current `Set.difference` statementIds partition
                maybeKept <- buildCandidate current retainedIds
                case maybeKept of
                    Nothing -> tryPartitions current rest
                    Just kept -> do
                        let currentSQL = statementSQL baseSQL allStatements current
                            candidateSQL = statementSQL baseSQL allStatements kept
                        if BS.length candidateSQL < BS.length currentSQL
                            then do
                                ok <- oracle candidateSQL
                                tryPartitions (if ok then kept else current) rest
                            else tryPartitions current rest

        chunksOf size xs
            | null xs = []
            | otherwise =
                let (chunk, rest) = splitAt (max 1 size) xs
                 in chunk : chunksOf size rest

{- | Counter-Based Delta Debugging (CDD), following Algorithm 2 of Zhang et al.,
"Toward a Better Understanding of Probabilistic Delta Debugging",
arXiv:2408.04735v4 (2025), https://arxiv.org/abs/2408.04735v4.
This uses p0 = 0.25 and the paper's approximate round multiplier 1.582.
-}
cddSubsetSize :: Int -> Int -> Int
cddSubsetSize roundNumber count
    | count <= 1 = 1
    | estimatedProbability >= 1 = 1
    | otherwise = bestSize [1 .. count] 1
    where
        estimatedProbability = 0.25 * (1.582 ** fromIntegral roundNumber)
        gain size = fromIntegral size * ((1 - estimatedProbability) ** fromIntegral size)
        bestSize [] currentBest = currentBest
        bestSize (size : sizes) currentBest
            | gain size > gain currentBest = bestSize sizes size
            | otherwise = bestSize sizes currentBest

-- | Graph-free statement ddmin over original top-level statement byte spans.
statementDdmin :: SearchStrategy -> Oracle -> ByteString -> IO ByteString
statementDdmin strategy oracle initialSQL =
    withParser $ \parser -> statementDdminWithParser strategy oracle parser initialSQL

statementDdminWithParser :: SearchStrategy -> Oracle -> Parser -> ByteString -> IO ByteString
statementDdminWithParser strategy oracle parser initialSQL = do
    initialOK <- oracle initialSQL
    if not initialOK
        then pure initialSQL
        else do
            parsed <- withParsedSQL parser initialSQL $ \tree -> extractStatementNodes tree initialSQL
            case parsed of
                Nothing -> pure initialSQL
                Just pairs -> do
                    let allStatements = map fst pairs
                    reduced <- searchStatements strategy plainCandidate oracle initialSQL allStatements allStatements
                    pure (statementSQL initialSQL allStatements reduced)

-- | Sequential one-statement deletion to a fixed point; the empty candidate is allowed.
statementAudit :: Oracle -> ByteString -> IO ByteString
statementAudit oracle initialSQL = withParser $ \parser -> do
    (result, _) <- statementAuditWithParser nullLogger oracle parser initialSQL
    pure result

statementAuditWithParser :: Logger -> Oracle -> Parser -> ByteString -> IO (ByteString, AuditStats)
statementAuditWithParser logger oracle parser initialSQL = do
    initialOK <- oracle initialSQL
    outcome <-
        if initialOK
            then loop initialSQL 0 0 0
            else pure (initialSQL, AuditStats 0 0 0 False)
    logAuditStats logger (snd outcome)
    pure outcome
    where
        loop current passCount attemptCount removalCount = do
            parsed <- withParsedSQL parser current $ \tree -> extractStatementNodes tree current
            case parsed of
                Nothing -> pure (current, AuditStats passCount attemptCount removalCount False)
                Just pairs -> do
                    let statements = map fst pairs
                    if null statements
                        then pure (current, AuditStats passCount attemptCount removalCount True)
                        else do
                            (nextSQL, attempts, removals) <- auditPass current statements statements
                            let nextPassCount = passCount + 1
                            if removals > 0
                                then loop nextSQL nextPassCount (attemptCount + attempts) (removalCount + removals)
                                else pure (current, AuditStats nextPassCount (attemptCount + attempts) removalCount True)

        auditPass baseSQL allStatements candidates = go (statementSQL baseSQL allStatements allStatements) allStatements candidates 0 0
            where
                go currentSQL current [] attempts removals = pure (currentSQL, attempts, removals)
                go currentSQL current (statement : rest) attempts removals
                    | Set.notMember (stmtId statement) (statementIds current) =
                        go currentSQL current rest attempts removals
                    | otherwise = do
                        let kept = filter ((/= stmtId statement) . stmtId) current
                            candidateSQL = statementSQL baseSQL allStatements kept
                        if BS.length candidateSQL >= BS.length currentSQL
                            then go currentSQL current rest attempts removals
                            else do
                                ok <- oracle candidateSQL
                                if ok
                                    then go candidateSQL kept rest (attempts + 1) (removals + 1)
                                    else go currentSQL current rest (attempts + 1) removals

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

-- Prefix tests are linear: oracle results are not assumed to be monotone.
findCrashStatement ::
    Oracle -> ByteString -> [Statement] -> IO (Maybe (Statement, ByteString, Int), Int)
findCrashStatement oracle baseSQL statements = go 1
    where
        go prefixLength
            | prefixLength > length statements = pure (Nothing, prefixLength - 1)
            | otherwise = do
                let prefix = take prefixLength statements
                    candidate = statementSQL baseSQL statements prefix
                ok <- oracle candidate
                if ok
                    then case reverse prefix of
                        statement : _ -> pure (Just (statement, candidate, prefixLength), prefixLength)
                        [] -> pure (Nothing, prefixLength)
                    else go (prefixLength + 1)

analyzeSQL ::
    Logger -> SQLDialect -> Parser -> ByteString -> IO (Maybe ([Statement], DependencyGraph))
analyzeSQL logger dialect parser sql = do
    started <- getMonotonicTimeNSec
    result <- withParsedSQL parser sql $ \tree -> do
        pairs <- extractStatementNodes tree sql
        let statements = map fst pairs
            indexed = [(stmtId statement, node) | (statement, node) <- pairs]
        graph <- analyzeStatementsWithDialect dialect indexed sql
        pure (statements, graph)
    finished <- getMonotonicTimeNSec
    let elapsed = finished - started
        statementCount = maybe 0 (length . fst) result
        edgeCount = maybe 0 (graphEdgeCount . snd) result
        barrierActions = maybe 0 (graphBarrierActions . snd) result
        barrierStatements = maybe 0 (graphBarrierStatements . snd) result
        barrierEdges = maybe 0 (graphBarrierEdges . snd) result
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
    pure result

sliceSQL :: Logger -> SQLDialect -> Oracle -> Parser -> ByteString -> IO ByteString
sliceSQL logger dialect oracle parser sql = do
    analyzed <- analyzeSQL logger dialect parser sql
    case analyzed of
        Nothing -> pure sql
        Just ([], _) -> pure sql
        Just (statements, graph) -> do
            (maybeCrash, prefixTests) <- findCrashStatement oracle sql statements
            case maybeCrash of
                Nothing -> do
                    logInfo'
                        logger
                        "occam::reducer"
                        "No crash-preserving statement prefix found"
                        [("prefix_tests", toJSON prefixTests)]
                    pure sql
                Just (crashStatement, prefixSQL, prefixLength) -> do
                    let slice = backwardSlice (stmtId crashStatement) graph
                        retained = filter (\s -> Set.member (stmtId s) slice) statements
                        candidate = statementSQL sql statements retained
                    if BS.length candidate >= BS.length sql
                        then pure sql
                        else do
                            let reusedPrefixResult = candidate == prefixSQL
                            ok <- if reusedPrefixResult then pure True else oracle candidate
                            logInfo'
                                logger
                                "occam::reducer"
                                "Prefix oracle result considered for backward slice"
                                [ ("prefix_tests", toJSON prefixTests)
                                , ("prefix_length", toJSON prefixLength)
                                , ("prefix_result_reused", toJSON reusedPrefixResult)
                                ]
                            if ok
                                then do
                                    let StatementId crashId = stmtId crashStatement
                                    logInfo'
                                        logger
                                        "occam::reducer"
                                        "Backward slicing accepted"
                                        [ ("crash_statement_id", toJSON crashId)
                                        , ("statements_before", toJSON (length statements))
                                        , ("statements_after", toJSON (length retained))
                                        , ("bytes_before", toJSON (BS.length sql))
                                        , ("bytes_after", toJSON (BS.length candidate))
                                        ]
                                    pure candidate
                                else pure sql

closureSearchSQL ::
    Logger -> SQLDialect -> SearchStrategy -> Oracle -> Parser -> ByteString -> IO ByteString
closureSearchSQL logger dialect strategy oracle parser sql = do
    analyzed <- analyzeSQL logger dialect parser sql
    case analyzed of
        Nothing -> pure sql
        Just (statements, graph)
            | null statements -> pure sql
            | otherwise -> do
                retained <- closureDdminWithLogger logger strategy oracle sql statements graph
                pure (statementSQL sql statements retained)

reduceSQL :: Oracle -> ByteString -> IO ByteString
reduceSQL = reduceSQLWithLogger nullLogger

reduceSQLWithLogger :: Logger -> Oracle -> ByteString -> IO ByteString
reduceSQLWithLogger = reduceSQLWithConfig defaultReductionConfig

reduceSQLWithConfig :: ReductionConfig -> Logger -> Oracle -> ByteString -> IO ByteString
reduceSQLWithConfig config logger oracle initialSQL = do
    initialOK <- oracle initialSQL
    if not initialOK
        then do
            logWarn logger "occam::reducer" "Initial SQL candidate failed oracle check"
            pure initialSQL
        else withParser $ \parser -> loop parser (1 :: Int) initialSQL
    where
        loop parser iteration currentSQL = do
            logInfo'
                logger
                "occam::reducer"
                "Starting reduction iteration"
                [ ("iteration", toJSON iteration)
                , ("bytes", toJSON (BS.length currentSQL))
                , ("strategy", toJSON (T.pack (show (reductionSearch config))))
                ,
                    ( "partition_weight"
                    , toJSON
                        (if reductionSearch config == WeightedDD then ("statement-bytes" :: T.Text) else "statement-count")
                    )
                , ("slicing", toJSON (reductionSlicing config))
                , ("closure", toJSON (reductionClosure config))
                , ("unrestricted", toJSON (reductionUnrestricted config))
                , ("intra_statement", toJSON (reductionIntraStatement config))
                , ("dialect", toJSON (T.pack (show (reductionDialect config))))
                ]
            nextSQL <- reduceIterationWithConfig config logger oracle parser currentSQL
            if BS.length nextSQL < BS.length currentSQL
                then loop parser (iteration + 1) nextSQL
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
    ReductionConfig -> Logger -> Oracle -> Parser -> ByteString -> IO ByteString
reduceIterationWithConfig config logger oracle parser initialSQL = do
    sliced <-
        if reductionSlicing config
            then
                timedStage
                    logger
                    "backward-slicing"
                    (sliceSQL logger (reductionDialect config) oracle parser initialSQL)
            else pure initialSQL
    searched <-
        if reductionClosure config
            then
                timedStage
                    logger
                    "closure-search"
                    (closureSearchSQL logger (reductionDialect config) (reductionSearch config) oracle parser sliced)
            else
                timedStage
                    logger
                    "graph-free-statement-search"
                    (statementDdminWithParser (reductionSearch config) oracle parser sliced)
    fallback <-
        if reductionClosure config && reductionUnrestricted config
            then
                timedStage
                    logger
                    "unrestricted-fallback"
                    (statementDdminWithParser (reductionSearch config) oracle parser searched)
            else pure searched
    audited <-
        if reductionUnrestricted config
            then do
                started <- getMonotonicTimeNSec
                (result, _) <- statementAuditWithParser logger oracle parser fallback
                finished <- getMonotonicTimeNSec
                logStage logger "statement-audit" (finished - started)
                pure result
            else pure fallback
    if reductionIntraStatement config
        then
            timedStage
                logger
                "intra-statement-rewrites"
                (reduceSyntaxWithParser logger oracle parser (const True) audited)
        else pure audited

pruneClauses :: Oracle -> ByteString -> IO ByteString
pruneClauses oracle sql = withParser (\parser -> pruneClausesWithParser oracle parser sql)

pruneClausesWithParser :: Oracle -> Parser -> ByteString -> IO ByteString
pruneClausesWithParser oracle parser = reduceSyntaxWithParser nullLogger oracle parser ((== "clause") . editCategory)

-- Expression-only public pass.
simplifyExpressions :: Oracle -> ByteString -> IO ByteString
simplifyExpressions oracle sql = withParser (\parser -> simplifyExpressionsWithParser oracle parser sql)

simplifyExpressionsWithParser :: Oracle -> Parser -> ByteString -> IO ByteString
simplifyExpressionsWithParser oracle parser = reduceSyntaxWithParser nullLogger oracle parser ((== "expression") . editCategory)

data SyntaxEdit = SyntaxEdit
    { editStart :: !Int
    , editEnd :: !Int
    , editReplacement :: !ByteString
    , editCategory :: !T.Text
    , editLabel :: !T.Text
    }

data LocatedNode = LocatedNode
    { locatedType :: !T.Text
    , locatedSpan :: !Span
    }

reduceSyntaxWithParser ::
    Logger -> Oracle -> Parser -> (SyntaxEdit -> Bool) -> ByteString -> IO ByteString
reduceSyntaxWithParser logger oracle parser shouldRun initialSQL = do
    stats <- newIORef (0 :: Int, 0 :: Int, 0 :: Word64)
    result <- go stats initialSQL
    (attempts, accepted, oracleElapsed) <- readIORef stats
    logInfo'
        logger
        "occam::reducer"
        "Syntax replacement summary"
        [ ("replacement_attempts", toJSON attempts)
        , ("replacement_accepted", toJSON accepted)
        , ("replacement_oracle_seconds", toJSON (seconds oracleElapsed))
        ]
    pure result
    where
        go stats currentSQL = do
            parsed <- withParsedSQL parser currentSQL $ \tree -> do
                root <- rootNode tree
                edits <- collectSyntaxEdits currentSQL root
                tryEdits stats currentSQL (filter shouldRun edits)
            case parsed of
                Nothing -> pure currentSQL
                Just Nothing -> pure currentSQL
                Just (Just reduced) -> go stats reduced

        tryEdits _ _ [] = pure Nothing
        tryEdits stats currentSQL (edit : rest) =
            let candidate = replaceSpan (editStart edit) (editEnd edit) (editReplacement edit) currentSQL
             in if BS.length candidate >= BS.length currentSQL
                    then tryEdits stats currentSQL rest
                    else do
                        started <- getMonotonicTimeNSec
                        ok <- oracle candidate
                        finished <- getMonotonicTimeNSec
                        modifyIORef'
                            stats
                            ( \(attempts, accepted, elapsed) ->
                                (attempts + 1, accepted + fromEnum ok, elapsed + (finished - started))
                            )
                        if ok
                            then do
                                logInfo'
                                    logger
                                    "occam::reducer"
                                    "Accepted byte-preserving syntax rewrite"
                                    [ ("rewrite_kind", toJSON (editLabel edit))
                                    , ("rewrite_category", toJSON (editCategory edit))
                                    , ("span_start", toJSON (editStart edit))
                                    , ("span_end", toJSON (editEnd edit))
                                    , ("replacement", toJSON (TE.decodeUtf8Lenient (editReplacement edit)))
                                    , ("bytes_before", toJSON (BS.length currentSQL))
                                    , ("bytes_after", toJSON (BS.length candidate))
                                    ]
                                pure (Just candidate)
                            else tryEdits stats currentSQL rest

collectSyntaxEdits :: ByteString -> TSNode -> IO [SyntaxEdit]
collectSyntaxEdits source root = do
    nodes <- collectDescendants (const True) root
    concat <$> mapM (nodeSyntaxEdits source) nodes

nodeSyntaxEdits :: ByteString -> TSNode -> IO [SyntaxEdit]
nodeSyntaxEdits source node = do
    nodeName <- nodeType node
    Span start end <- nodeSpan node
    children <- nodeNamedChildren node
    childInfo <- mapM locate children
    let clauseEdits =
            [ SyntaxEdit (precedingSpace source start) end BS.empty "clause" ("remove-" <> nodeName)
            | nodeName `elem` ["where", "order_by", "group_by", "having", "limit"]
            ]
    listEdits <- listSyntaxEdits source nodeName (Span start end) childInfo
    expressionEdits <- expressionSyntaxEdits source nodeName (Span start end) children
    scalarEdits <- scalarSyntaxEdits source nodeName (Span start end)
    pure (clauseEdits <> listEdits <> expressionEdits <> scalarEdits)
    where
        locate child = LocatedNode <$> nodeType child <*> nodeSpan child

precedingSpace :: ByteString -> Int -> Int
precedingSpace source start
    | start <= 0 = start
    | isSQLSpace (BS.index source (start - 1)) = start - 1
    | otherwise = start

isSQLSpace :: Word8 -> Bool
isSQLSpace byte = byte == 32 || byte == 9 || byte == 10 || byte == 13

listSyntaxEdits :: ByteString -> T.Text -> Span -> [LocatedNode] -> IO [SyntaxEdit]
listSyntaxEdits source parentName parentSpan children =
    case parentName of
        "from" -> pure (fromListEdits source children)
        _ ->
            case listItemTypes parentName of
                Nothing -> pure []
                Just itemTypes ->
                    let items = filter ((`elem` itemTypes) . locatedType) children
                     in pure (listEditsForItems source parentName parentSpan items)

listItemTypes :: T.Text -> Maybe [T.Text]
listItemTypes parentName =
    case parentName of
        "select" -> Just ["select_expression", "all_fields"]
        "column_definitions" -> Just ["column_definition", "constraint", "constraints"]
        "constraints" -> Just ["constraint"]
        "assignment_list" -> Just ["assignment"]
        "enum_elements" -> Just ["literal", "identifier"]
        "_cte" -> Just ["cte"]
        "cte" -> Just ["cte"]
        "values" -> Just ["list"]
        "list" -> Just expressionNodeTypes
        "ordered_columns" -> Just ["column"]
        "index_fields" -> Just ["field"]
        "function_arguments" -> Just ["function_argument"]
        "_column_list" -> Just ["column"]
        _ -> Nothing

expressionNodeTypes :: [T.Text]
expressionNodeTypes =
    [ "literal"
    , "identifier"
    , "field"
    , "object_reference"
    , "invocation"
    , "binary_expression"
    , "unary_expression"
    , "parenthesized_expression"
    , "subquery"
    , "list"
    , "case"
    , "cast"
    , "implicit_cast"
    , "exists"
    , "between_expression"
    , "not_in"
    , "select_expression"
    , "term"
    ]

fromListEdits :: ByteString -> [LocatedNode] -> [SyntaxEdit]
fromListEdits source children =
    let relations = filter ((== "relation") . locatedType) children
        joins =
            filter
                ((`elem` ["join", "cross_join", "lateral_join", "lateral_cross_join"]) . locatedType)
                children
        relationEdits =
            if length relations > 1
                then listEditsForItems source "from-relation" (Span 0 (BS.length source)) relations
                else []
        joinEdits =
            [ SyntaxEdit start end BS.empty "list" ("remove-" <> locatedType child)
            | child <- joins
            , let Span start end = locatedSpan child
            ]
     in relationEdits <> joinEdits

listEditsForItems :: ByteString -> T.Text -> Span -> [LocatedNode] -> [SyntaxEdit]
listEditsForItems source parentName _ items
    | length items <= 1 = []
    | otherwise =
        [ SyntaxEdit start end BS.empty "list" ("remove-" <> parentName <> "-" <> locatedType item)
        | (index, item) <- zip [0 ..] items
        , Just (Span start end) <- [itemRemovalSpan source items index]
        ]

itemRemovalSpan :: ByteString -> [LocatedNode] -> Int -> Maybe Span
itemRemovalSpan source items index =
    case splitAt index items of
        (before, item : after) ->
            let Span itemStart itemEnd = locatedSpan item
             in case (reverse before, after) of
                    (previous : _, _) ->
                        let Span _ previousEnd = locatedSpan previous
                         in fmap (`Span` itemEnd) (findLastByte 44 source previousEnd itemStart)
                    ([], next : _) ->
                        let Span nextStart _ = locatedSpan next
                         in fmap (\comma -> Span itemStart (comma + 1)) (findByte 44 source itemEnd nextStart)
                    ([], []) -> Nothing
        _ -> Nothing

findByte :: Word8 -> ByteString -> Int -> Int -> Maybe Int
findByte byte source start end = go start
    where
        go index
            | index >= end = Nothing
            | BS.index source index == byte = Just index
            | otherwise = go (index + 1)

findLastByte :: Word8 -> ByteString -> Int -> Int -> Maybe Int
findLastByte byte source start end = go (end - 1)
    where
        go index
            | index < start = Nothing
            | BS.index source index == byte = Just index
            | otherwise = go (index - 1)

expressionSyntaxEdits :: ByteString -> T.Text -> Span -> [TSNode] -> IO [SyntaxEdit]
expressionSyntaxEdits source nodeName (Span start end) children
    | nodeName `notElem` ["binary_expression", "unary_expression", "parenthesized_expression"] = pure []
    | otherwise = do
        replacements <- expressionReplacements source nodeName children
        pure
            [ SyntaxEdit start end replacement "expression" ("simplify-" <> nodeName)
            | replacement <- replacements
            , BS.length replacement < end - start
            ]

expressionReplacements :: ByteString -> T.Text -> [TSNode] -> IO [ByteString]
expressionReplacements source nodeName children =
    case nodeName of
        "binary_expression" ->
            case children of
                left : rest@(_ : _) -> do
                    leftText <- nodeText source left
                    rightText <- nodeText source (last rest)
                    pure [leftText, rightText, "0", "1", "'0'"]
                _ -> pure []
        "unary_expression" ->
            case reverse children of
                operand : _ -> do
                    operandText <- nodeText source operand
                    pure [operandText, "0", "1", "'0'"]
                [] -> pure []
        "parenthesized_expression" ->
            case children of
                inner : _ -> do
                    innerText <- nodeText source inner
                    pure [innerText]
                [] -> pure []
        _ -> pure []

scalarSyntaxEdits :: ByteString -> T.Text -> Span -> IO [SyntaxEdit]
scalarSyntaxEdits source nodeName (Span start end)
    | nodeName == "literal" = do
        let raw = BS.take (end - start) (BS.drop start source)
        pure
            [ SyntaxEdit start end replacement "literal" "simplify-literal"
            | replacement <- literalReplacements raw
            , BS.length replacement < end - start
            ]
    | Just replacement <- typeReplacement nodeName =
        pure
            [ SyntaxEdit start end replacement "type" ("simplify-type-" <> nodeName)
            | BS.length replacement < end - start
            ]
    | otherwise = pure []

literalReplacements :: ByteString -> [ByteString]
literalReplacements raw
    | not (BS.null raw) && BS.head raw == 39 = ["''", "'0'", "NULL", "0"]
    | otherwise = ["0", "1", "NULL", "FALSE", "TRUE", "''"]

typeReplacement :: T.Text -> Maybe ByteString
typeReplacement nodeName =
    case nodeName of
        "bigint" -> Just "INT"
        "mediumint" -> Just "INT"
        "smallint" -> Just "INT"
        "decimal" -> Just "INT"
        "numeric" -> Just "INT"
        "double" -> Just "FLOAT"
        "varchar" -> Just "TEXT"
        "nvarchar" -> Just "TEXT"
        "varbinary" -> Just "BLOB"
        "boolean" -> Just "BOOL"
        _ -> Nothing
