-- | Literal, distributed-literal, and independent-AST edit batching strategies.
module Occam.Reducer.Syntax.Batches (
    runCategoryBatches,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (evalContT)
import Data.Aeson (toJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable as Foldable (foldl', for_)
import Data.HashSet qualified as HashSet
import Data.IORef (IORef, modifyIORef')
import Data.IntSet qualified as IntSet
import Data.List (partition, sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Occam.Certificate (checkEarlyRejectionBytesTracked)
import Occam.Core (Span (..), Statement (..), StatementId (..))
import Occam.Logger (logInfo')
import Occam.Metrics (FamilyTag (..), recordCandidateBuildTiming)
import Occam.Parser (
    ColumnTarget,
    ParserEdit (..),
    ParserEditCategory (..),
    StatementFacts (..),
    StatementUnit (..),
 )
import Occam.Reducer.Config (DistributedBatchConfig (..), LiteralDeferralMode (..))
import Occam.Reducer.Support (earlyExit, recordEarlyRejectSkip)
import Occam.Reducer.Syntax.Execute (
    logAcceptedSyntaxEdit,
    recordSyntaxTest,
    testSyntaxCandidates,
    testSyntaxWindow,
 )
import Occam.Reducer.Syntax.Index (
    SyntaxCandidate (..),
    SyntaxContext (..),
    SyntaxIndex,
    SyntaxStats,
    isPrimaryLiteralEdit,
    literalSavings,
    phaseCandidates,
 )
import Occam.Reducer.Syntax.Snapshot (
    applyParserEdits,
    independentEditBatches,
    updateSyntaxBatchSnapshot,
    updateSyntaxSnapshot,
 )

runCategoryBatches ::
    SyntaxContext ->
    IORef SyntaxStats ->
    ByteString ->
    [StatementUnit] ->
    SyntaxIndex ->
    [ColumnTarget] ->
    ParserEditCategory ->
    IO (ByteString, [StatementUnit], SyntaxIndex, [ColumnTarget])
runCategoryBatches context stats currentSQL currentUnits phases bundles category
    | syntaxAstBatching context
    , category `elem` [EditClause, EditConstraint, EditType, EditExpression, EditLiteral] =
        exhaustIndependentAstBatches context stats currentSQL currentUnits phases bundles category
    | category == EditLiteral = do
        (distSQL, distUnits, distPhases, distBundles) <-
            if syntaxDistributedLiteralBatches context
                then exhaustDistributedLiteralBatches context stats currentSQL currentUnits phases bundles
                else pure (currentSQL, currentUnits, phases, bundles)
        if syntaxLiteralBatches context
            then exhaustLiteralBatches context stats distSQL distUnits distPhases distBundles
            else pure (distSQL, distUnits, distPhases, distBundles)
    | otherwise = pure (currentSQL, currentUnits, phases, bundles)

unitNonRecursiveLiterals :: SyntaxContext -> StatementUnit -> [ParserEdit]
unitNonRecursiveLiterals context unit =
    [ edit
    | facts <- maybe [] pure (unitFacts unit)
    , edit <- factEdits facts
    , parserEditCategory edit == EditLiteral
    , not (syntaxDeferRecursivePredicates context && parserEditRecursivePredicate edit)
    , syntaxShouldRun context edit
    ]

allNonRecursiveLiterals :: SyntaxContext -> [StatementUnit] -> [ParserEdit]
allNonRecursiveLiterals context = concatMap (unitNonRecursiveLiterals context)

{- | Try literal batches before the regular single-edit sweep. A rejection of
a batch leaves its smaller partitions and the single-edit sweep eligible.
-}
exhaustLiteralBatches ::
    SyntaxContext ->
    IORef SyntaxStats ->
    ByteString ->
    [StatementUnit] ->
    SyntaxIndex ->
    [ColumnTarget] ->
    IO (ByteString, [StatementUnit], SyntaxIndex, [ColumnTarget])
exhaustLiteralBatches context stats sql units phases bundles = search 0 (8 :: Int) candidates
    where
        literals = allNonRecursiveLiterals context units
        candidates =
            [ SingleCandidate (combineLiteralEdits sql first rest)
            | unit <- units
            , group <-
                sortOn (Down . sum . map literalSavings) (literalGroups (stmtSpan (unitStatement unit)) literals)
            , first : rest@(_ : _) <- batchPartitions group
            ]
        search _ misses _ | misses <= 0 = pure (sql, units, phases, bundles)
        search _ _ [] = pure (sql, units, phases, bundles)
        search ordinal misses remaining = do
            let (eligible, deferred) = splitAt misses remaining
            (tested, elapsed, accepted) <- testSyntaxCandidates context units sql ordinal eligible
            modifyIORef' stats (recordSyntaxTest tested elapsed accepted)
            case accepted of
                Just (_, SingleCandidate edit, bytes) -> do
                    logInfo'
                        (syntaxLogger context)
                        "reducer"
                        "Accepted literal batch"
                        [ ("bytes_before", toJSON (BS.length sql))
                        , ("bytes_after", toJSON (BS.length bytes))
                        ]
                    (nextUnits, nextPhases, nextBundles) <-
                        updateSyntaxSnapshot context EditLiteral units phases bundles edit bytes
                    exhaustLiteralBatches context stats bytes nextUnits nextPhases nextBundles
                _ -> search (ordinal + length eligible) (misses - tested) deferred

{- | Group equal replacements while retaining order within each group. This
keeps edits to result-bearing literals from blocking batches of noise values.
-}
literalGroups :: Span -> [ParserEdit] -> [[ParserEdit]]
literalGroups (Span start end) = groupReplacements . filter eligible
    where
        eligible edit =
            parserEditStart edit >= start
                && parserEditEnd edit <= end
                && literalSavings edit > 0
        groupReplacements [] = []
        groupReplacements (first : rest) =
            let (matching, others) = partition ((== parserEditReplacement first) . parserEditReplacement) rest
             in group (first : matching) ++ groupReplacements others
        group [] = []
        group (first : rest) = collect (15 :: Int) [first] rest
        collect capacity edits (next : rest)
            | any (overlaps next) edits = collect capacity edits rest
            | capacity > 0 = collect (capacity - 1) (next : edits) rest
        collect _ edits rest = reverse edits : group rest
        overlaps a b = parserEditStart a < parserEditEnd b && parserEditStart b < parserEditEnd a

batchPartitions :: [ParserEdit] -> [[ParserEdit]]
batchPartitions edits@(_ : _ : _) =
    let (left, right) = splitAt (length edits `div` 2) edits
     in edits : (batchPartitions left ++ batchPartitions right)
batchPartitions _ = []

{- | Represent the disjoint replacements as one statement-local patch so the
existing incremental snapshot path can refresh it after oracle acceptance.
-}
combineLiteralEdits :: ByteString -> ParserEdit -> [ParserEdit] -> ParserEdit
combineLiteralEdits sql first rest =
    first
        { parserEditStart = start
        , parserEditEnd = end
        , parserEditLabel = "simplify-literal-batch"
        , parserEditReplacement = BS.concat (reverse parts)
        }
    where
        ordered = sortOn parserEditStart (first : rest)
        start = minimum (map parserEditStart ordered)
        (end, parts) = Foldable.foldl' append (start, []) ordered
        append (previous, acc) edit =
            ( parserEditEnd edit
            , parserEditReplacement edit : BS.take (parserEditStart edit - previous) (BS.drop previous sql) : acc
            )

primaryUnitLiterals :: SyntaxContext -> StatementUnit -> [ParserEdit]
primaryUnitLiterals context unit
    | deferLiterals == DeferLiteralsOff = eligible
    | otherwise = reverse (fst (Foldable.foldl' step ([], Set.empty) eligible))
    where
        deferLiterals = syntaxDeferLiteralVariants context
        eligible = unitNonRecursiveLiterals context unit
        step (!acc, !seenLitSpans) edit
            | isPrimaryLiteralEdit deferLiterals seenLitSpans edit =
                (edit : acc, Set.insert (parserEditStart edit, parserEditEnd edit) seenLitSpans)
            | otherwise = (acc, seenLitSpans)

unitEligibleLiterals :: SyntaxContext -> StatementUnit -> [ParserEdit]
unitEligibleLiterals context unit =
    [ edit
    | edit <- primaryUnitLiterals context unit
    , parserEditLabel edit == "simplify-literal"
    , literalSavings edit > 0
    ]

distributedLiteralGroups ::
    SyntaxContext ->
    Int ->
    Map.Map (ByteString, Bool) IntSet.IntSet ->
    Maybe (ByteString, Bool) ->
    [StatementUnit] ->
    [((ByteString, Bool), [[(Int, ParserEdit)]])]
distributedLiteralGroups context maxBatch rejectedByRepl preferredRepl units =
    sortOn groupOrder nonEmptyGroups
    where
        byRepl = Foldable.foldl' addUnit Map.empty units
        addUnit groups unit =
            let sid = unStatementId (stmtId (unitStatement unit))
                bestPerRepl =
                    Map.fromListWith
                        (\new old -> if literalSavings new > literalSavings old then new else old)
                        [((parserEditReplacement e, parserEditStringLiteral e), e) | e <- unitEligibleLiterals context unit]
                addEntry acc key edit
                    | IntSet.member sid (Map.findWithDefault IntSet.empty key rejectedByRepl) = acc
                    | otherwise = Map.insertWith (++) key [(sid, edit)] acc
             in Map.foldlWithKey' addEntry groups bestPerRepl

        chunkEdits xs@(_ : _ : _) =
            let (chunk, rest) = splitAt (max 2 maxBatch) xs
             in chunk : chunkEdits rest
        chunkEdits _ = []

        chunkSavings = sum . map (literalSavings . snd)

        nonEmptyGroups =
            [ (key, sortOn (Down . chunkSavings) chunks)
            | (key, revPairs) <- Map.toList byRepl
            , let ordered = sortOn (Down . literalSavings . snd) (reverse revPairs)
                  chunks = chunkEdits ordered
            , not (null chunks)
            ]

        groupOrder (key@(repl, isStr), firstChunk : _) =
            ( if preferredRepl == Just key then (0 :: Int) else 1
            , deferLowYield && any (parserEditLowYield . snd) firstChunk
            , Down (chunkSavings firstChunk)
            , BS.length repl
            , isStr
            , repl
            )
        groupOrder ((repl, isStr), []) = (2, False, Down 0, BS.length repl, isStr, repl)
        deferLowYield = syntaxDeferLiteralVariants context /= DeferLiteralsOff

data BatchProbeOutcome
    = BatchSkipped !(HashSet.HashSet ByteString)
    | BatchRejected !(HashSet.HashSet ByteString)
    | BatchAccepted !ByteString

-- | Build, early-reject check, and test a single syntax batch candidate.
probeSyntaxBatch ::
    SyntaxContext ->
    IORef SyntaxStats ->
    Int ->
    ByteString ->
    HashSet.HashSet ByteString ->
    ParserEditCategory ->
    [ParserEdit] ->
    SyntaxCandidate ->
    IO BatchProbeOutcome
probeSyntaxBatch context stats unitCount sql seenRejected category batch candidate = evalContT do
    candidateBytes <- case applyParserEdits sql batch of
        Just b
            | BS.length b < BS.length sql && not (HashSet.member b seenRejected) -> pure b
        _ -> earlyExit (BatchSkipped seenRejected)
    liftIO $ recordCandidateBuildTiming (syntaxStore context) (TagSyntax category label) 0
    earlyBytes <-
        liftIO $
            checkEarlyRejectionBytesTracked (syntaxTracker context) (syntaxCert context) True candidateBytes
    for_ earlyBytes \reason -> do
        liftIO $ recordEarlyRejectSkip (syntaxStore context) reason
        earlyExit (BatchSkipped (HashSet.insert candidateBytes seenRejected))
    liftIO do
        (tested, elapsed, accepted) <-
            testSyntaxWindow context unitCount [(0, candidate, candidateBytes)]
        modifyIORef' stats (recordSyntaxTest tested elapsed accepted)
        case accepted of
            Just (_, _, bytes) -> do
                logAcceptedSyntaxEdit context candidate sql bytes
                pure (BatchAccepted bytes)
            Nothing -> pure (BatchRejected (HashSet.insert candidateBytes seenRejected))
    where
        label = case candidate of
            BatchCandidate _ lbl _ -> lbl
            _ -> "syntax-batch"

{- | Try atomic cross-statement literal batches (at most one literal per statement
per batch, sharing the same replacement) before the single-edit literal sweep.
Refreshes accepted batches by reparsing only the touched statements through the
Rust parser with a full parse fallback.
-}
exhaustDistributedLiteralBatches ::
    SyntaxContext ->
    IORef SyntaxStats ->
    ByteString ->
    [StatementUnit] ->
    SyntaxIndex ->
    [ColumnTarget] ->
    IO (ByteString, [StatementUnit], SyntaxIndex, [ColumnTarget])
exhaustDistributedLiteralBatches context stats initSQL initUnits initPhases initBundles
    | null (drop (minStmts - 1) (filter (not . null . unitEligibleLiterals context) initUnits)) =
        pure (initSQL, initUnits, initPhases, initBundles)
    | otherwise = go initSQL initUnits initPhases initBundles HashSet.empty Map.empty Nothing
    where
        cfg = syntaxDistributedBatchConfig context
        minStmts = max 2 (distributedMinStatements cfg)
        maxBatch = max 2 (distributedMaxBatchSize cfg)
        maxMisses = max 1 (distributedMaxMisses cfg)

        go sql units phases bundles seenRejected rejectedByRepl preferredRepl =
            let groups = distributedLiteralGroups context maxBatch rejectedByRepl preferredRepl units
                firstPerGroup = [(k, b) | (k, b : _) <- groups]
                nextChunks = [(k, b) | (k, _ : b : _) <- take 1 groups]
                queue = take 2 firstPerGroup ++ nextChunks
             in search sql units phases bundles seenRejected rejectedByRepl maxMisses queue

        search sql units phases bundles _ _ misses _ | misses <= 0 = pure (sql, units, phases, bundles)
        search sql units phases bundles _ _ _ [] = pure (sql, units, phases, bundles)
        search sql units phases bundles seenRejected rejectedByRepl misses ((key, chunk) : rest) = do
            let batch = map snd chunk
                candidate = BatchCandidate EditLiteral "distributed-literal-batch" batch
            probeSyntaxBatch context stats (length units) sql seenRejected EditLiteral batch candidate >>= \case
                BatchSkipped seen' ->
                    search sql units phases bundles seen' rejectedByRepl misses rest
                BatchRejected seen' ->
                    let sids = IntSet.fromList (map fst chunk)
                        rejectedByRepl' = Map.insertWith IntSet.union key sids rejectedByRepl
                     in search sql units phases bundles seen' rejectedByRepl' (misses - 1) rest
                BatchAccepted bytes -> do
                    (nextUnits, nextPhases, nextBundles) <-
                        updateSyntaxBatchSnapshot context EditLiteral units phases bundles batch bytes
                    let rejectedByRepl' = Map.filterWithKey (\k _ -> k == key) rejectedByRepl
                    go bytes nextUnits nextPhases nextBundles seenRejected rejectedByRepl' (Just key)

-- | Try independent AST edit batches with DDMin-style recursive subdivision on rejection.
exhaustIndependentAstBatches ::
    SyntaxContext ->
    IORef SyntaxStats ->
    ByteString ->
    [StatementUnit] ->
    SyntaxIndex ->
    [ColumnTarget] ->
    ParserEditCategory ->
    IO (ByteString, [StatementUnit], SyntaxIndex, [ColumnTarget])
exhaustIndependentAstBatches context stats initSQL initUnits initPhases initBundles category =
    go initSQL initUnits initPhases initBundles HashSet.empty
    where
        go sql units phases bundles seenRejected =
            let categoryEdits
                    | category == EditLiteral = allNonRecursiveLiterals context units
                    | otherwise = phaseCandidates category phases
                batches = independentEditBatches category categoryEdits
             in search sql units phases bundles seenRejected (8 :: Int) batches

        search sql units phases bundles _ misses _ | misses <= 0 = pure (sql, units, phases, bundles)
        search sql units phases bundles _ _ [] = pure (sql, units, phases, bundles)
        search sql units phases bundles seenRejected misses (batch : rest) = do
            let candidate = BatchCandidate category "independent-ast-batch" batch
            probeSyntaxBatch context stats (length units) sql seenRejected category batch candidate >>= \case
                BatchSkipped seen' ->
                    search sql units phases bundles seen' misses rest
                BatchRejected seen' ->
                    let (left, right) = splitAt (length batch `div` 2) batch
                        subdivisions = [sub | sub <- [left, right], length sub >= 2]
                     in search sql units phases bundles seen' (misses - 1) (subdivisions ++ rest)
                BatchAccepted bytes -> do
                    (nextUnits, nextPhases, nextBundles) <-
                        updateSyntaxBatchSnapshot context category units phases bundles batch bytes
                    go bytes nextUnits nextPhases nextBundles seenRejected
