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
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Core (Span (..), Statement (..), StatementId (..))
import Occam.Dependency (
    DependencyGraph (..),
    analyzeStatements,
 )
import Occam.Logger (Logger, logInfo', logWarn, nullLogger)
import Occam.Parser (
    ParsedSQL (..),
    ParserEdit (..),
    SQLDialect (..),
    StatementFacts (..),
    StatementUnit (..),
    defaultSQLDialect,
    parseSQL,
 )
import Occam.Slicer (backwardSlice)

-- | The oracle returns True exactly when the complete candidate satisfies the user's predicate.
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

parseUnitsOrThrow :: SQLDialect -> ByteString -> IO [StatementUnit]
parseUnitsOrThrow dialect sql = do
    parsed <- parseSQL dialect sql
    case parsed of
        Left parserError -> ioError (userError ("SQL parser transport failure: " ++ show parserError))
        Right result -> pure (parsedUnits result)

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
closeSet graph roots =
    Set.unions [backwardSlice sid graph | sid <- Set.toList roots]

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
cutSpan :: Int -> Int -> ByteString -> ByteString
cutSpan start end source = BS.take start source <> BS.drop end source

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
        estimatedProbability :: Double
        estimatedProbability = 0.25 * (1.582 ** fromIntegral roundNumber)
        gain size = fromIntegral size * ((1 - estimatedProbability) ** fromIntegral size)
        bestSize [] currentBest = currentBest
        bestSize (size : sizes) currentBest
            | gain size > gain currentBest = bestSize sizes size
            | otherwise = bestSize sizes currentBest

-- | Graph-free statement ddmin over original top-level statement byte spans.
statementDdmin :: SQLDialect -> SearchStrategy -> Oracle -> ByteString -> IO ByteString
statementDdmin dialect strategy oracle initialSQL = do
    initialOK <- oracle initialSQL
    if not initialOK
        then pure initialSQL
        else do
            units <- parseUnitsOrThrow dialect initialSQL
            let allStatements = map unitStatement units
            reduced <- searchStatements strategy plainCandidate oracle initialSQL allStatements allStatements
            pure (statementSQL initialSQL allStatements reduced)

-- | Sequential one-statement deletion to a fixed point; the empty candidate is allowed.
statementAudit :: SQLDialect -> Oracle -> ByteString -> IO ByteString
statementAudit dialect oracle initialSQL = fst <$> statementAuditWithLogger nullLogger dialect oracle initialSQL

statementAuditWithLogger ::
    Logger -> SQLDialect -> Oracle -> ByteString -> IO (ByteString, AuditStats)
statementAuditWithLogger logger dialect oracle initialSQL = do
    initialOK <- oracle initialSQL
    outcome <-
        if initialOK
            then loop initialSQL 0 0 0
            else pure (initialSQL, AuditStats 0 0 0 False)
    logAuditStats logger (snd outcome)
    pure outcome
    where
        loop current passCount attemptCount removalCount = do
            units <- parseUnitsOrThrow dialect current
            let statements = map unitStatement units
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
                go currentSQL _ [] attempts removals = pure (currentSQL, attempts, removals)
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
findInterestingPrefix ::
    Oracle -> ByteString -> [Statement] -> IO (Maybe (Statement, ByteString, Int), Int)
findInterestingPrefix oracle baseSQL statements = go 1
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

analyzeSQL :: Logger -> SQLDialect -> ByteString -> IO ([Statement], DependencyGraph)
analyzeSQL logger dialect sql = do
    started <- getMonotonicTimeNSec
    units <- parseUnitsOrThrow dialect sql
    let statements = map unitStatement units
        graph = analyzeStatements units
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
    pure (statements, graph)

sliceSQL :: Logger -> SQLDialect -> Oracle -> ByteString -> IO ByteString
sliceSQL logger dialect oracle sql = do
    (statements, graph) <- analyzeSQL logger dialect sql
    if null statements
        then pure sql
        else do
            let sliceFor target =
                    let slice = backwardSlice (stmtId target) graph
                        retained = filter (\s -> Set.member (stmtId s) slice) statements
                     in (retained, statementSQL sql statements retained)
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
                    then oracle lastCandidate
                    else pure False
            if lastAccepted
                then acceptSlice "last-statement" lastStatement lastRetained lastCandidate
                else do
                    (maybeTarget, prefixTests) <- findInterestingPrefix oracle sql statements
                    case maybeTarget of
                        Nothing -> do
                            logInfo'
                                logger
                                "occam::reducer"
                                "No predicate-preserving statement prefix found"
                                [("prefix_tests", toJSON prefixTests)]
                            pure sql
                        Just (targetStatement, prefixSQL, prefixLength) -> do
                            let (retained, candidate) = sliceFor targetStatement
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
                                        then acceptSlice "first-interesting-prefix" targetStatement retained candidate
                                        else pure sql

closureSearchSQL ::
    Logger -> SQLDialect -> SearchStrategy -> Oracle -> ByteString -> IO ByteString
closureSearchSQL logger dialect strategy oracle sql = do
    (statements, graph) <- analyzeSQL logger dialect sql
    if null statements
        then pure sql
        else do
            retained <- closureDdminWithLogger logger strategy oracle sql statements graph
            pure (statementSQL sql statements retained)

reduceSQL :: SQLDialect -> Oracle -> ByteString -> IO ByteString
reduceSQL dialect = reduceSQLWithLogger dialect nullLogger

reduceSQLWithLogger :: SQLDialect -> Logger -> Oracle -> ByteString -> IO ByteString
reduceSQLWithLogger dialect =
    reduceSQLWithConfig (defaultReductionConfig {reductionDialect = dialect})

reduceSQLWithConfig :: ReductionConfig -> Logger -> Oracle -> ByteString -> IO ByteString
reduceSQLWithConfig config logger oracle initialSQL = do
    initialOK <- oracle initialSQL
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
            nextSQL <- reduceIterationWithConfig config logger oracle currentSQL
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
    ReductionConfig -> Logger -> Oracle -> ByteString -> IO ByteString
reduceIterationWithConfig config logger oracle initialSQL = do
    sliced <-
        if reductionSlicing config
            then
                timedStage
                    logger
                    "backward-slicing"
                    (sliceSQL logger (reductionDialect config) oracle initialSQL)
            else pure initialSQL
    searched <-
        if reductionClosure config
            then
                timedStage
                    logger
                    "closure-search"
                    (closureSearchSQL logger (reductionDialect config) (reductionSearch config) oracle sliced)
            else
                timedStage
                    logger
                    "graph-free-statement-search"
                    (statementDdmin (reductionDialect config) (reductionSearch config) oracle sliced)
    fallback <-
        if reductionClosure config && reductionUnrestricted config
            then
                timedStage
                    logger
                    "unrestricted-fallback"
                    (statementDdmin (reductionDialect config) (reductionSearch config) oracle searched)
            else pure searched
    audited <-
        if reductionUnrestricted config
            then do
                started <- getMonotonicTimeNSec
                (result, _) <- statementAuditWithLogger logger (reductionDialect config) oracle fallback
                finished <- getMonotonicTimeNSec
                logStage logger "statement-audit" (finished - started)
                pure result
            else pure fallback
    if reductionIntraStatement config
        then
            timedStage
                logger
                "intra-statement-rewrites"
                (reduceSyntaxWithDialect logger (reductionDialect config) oracle (const True) audited)
        else pure audited

pruneClauses :: SQLDialect -> Oracle -> ByteString -> IO ByteString
pruneClauses dialect oracle =
    reduceSyntaxWithDialect nullLogger dialect oracle ((== "clause") . parserEditCategory)

-- Expression-only public pass.
simplifyExpressions :: SQLDialect -> Oracle -> ByteString -> IO ByteString
simplifyExpressions dialect oracle =
    reduceSyntaxWithDialect nullLogger dialect oracle ((== "expression") . parserEditCategory)

reduceSyntaxWithDialect ::
    Logger -> SQLDialect -> Oracle -> (ParserEdit -> Bool) -> ByteString -> IO ByteString
reduceSyntaxWithDialect logger dialect oracle shouldRun initialSQL = do
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
            units <- parseUnitsOrThrow dialect currentSQL
            let edits = concatMap (filter shouldRun . maybe [] factEdits . unitFacts) units
            reduced <- tryEdits stats currentSQL edits
            maybe (pure currentSQL) (go stats) reduced

        tryEdits _ _ [] = pure Nothing
        tryEdits stats currentSQL (edit : rest) =
            let candidate =
                    replaceSpan
                        (parserEditStart edit)
                        (parserEditEnd edit)
                        (parserEditReplacement edit)
                        currentSQL
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
                                    [ ("rewrite_kind", toJSON (parserEditLabel edit))
                                    , ("rewrite_category", toJSON (parserEditCategory edit))
                                    , ("span_start", toJSON (parserEditStart edit))
                                    , ("span_end", toJSON (parserEditEnd edit))
                                    , ("replacement", toJSON (TE.decodeUtf8Lenient (parserEditReplacement edit)))
                                    , ("bytes_before", toJSON (BS.length currentSQL))
                                    , ("bytes_after", toJSON (BS.length candidate))
                                    ]
                                pure (Just candidate)
                            else tryEdits stats currentSQL rest

replaceSpan :: Int -> Int -> ByteString -> ByteString -> ByteString
replaceSpan start end replacement source =
    BS.take start source <> replacement <> BS.drop end source
