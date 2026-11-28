-- | Intra-statement syntax rewrite fixed-point driver.
module Occam.Reducer.Syntax (
    reduceSyntaxWithDialect,
) where

import Data.Aeson (toJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, newIORef, readIORef)
import Occam.Certificate (EarlyRejectTracker, emptyEarlyRejectCertifier)
import Occam.Core.Types (ResultTrace)
import Occam.Logger (Logger, logDebug', logInfo')
import Occam.Parser (
    ColumnTarget,
    ParseOptions,
    ParserEdit,
    ParserEditCategory (..),
    StatementUnit,
 )
import Occam.Pool (OraclePool, poolMetricsStore)
import Occam.Reducer.Config (
    LiteralDeferralMode (..),
    ReductionConfig (..),
    astBatchPolicy,
    deferLiteralVariantsPolicy,
    deferRecursivePredicatesPolicy,
    distributedLiteralBatchPolicy,
    literalBatchPolicy,
    syntaxPolicyName,
    valuesChunkPolicy,
 )
import Occam.Reducer.Support (buildCertifierTracked, seconds)
import Occam.Reducer.Syntax.Batches (runCategoryBatches)
import Occam.Reducer.Syntax.Execute (
    exhaustSyntaxCategory,
    exhaustSyntaxStream,
    valuesChunkEdits,
 )
import Occam.Reducer.Syntax.Index (
    EditBucket (..),
    SyntaxCandidate (..),
    SyntaxContext (..),
    SyntaxIndex,
    SyntaxStats (..),
    bucketCandidatesFrom,
    phaseCandidates,
    syntaxCategoryOrder,
    syntaxCategoryOrderName,
 )
import Occam.Reducer.Syntax.Snapshot (parseSyntaxSnapshot)

-- | Intra-statement AST\/syntax rewrite pass honoring column bundles, deferral policies, and early rejection.
reduceSyntaxWithDialect ::
    ReductionConfig ->
    EarlyRejectTracker ->
    (ByteString -> IO (Maybe ResultTrace)) ->
    Logger ->
    ParseOptions ->
    OraclePool ->
    (ParserEdit -> Bool) ->
    ByteString ->
    IO ByteString
reduceSyntaxWithDialect config tracker lookupTrace logger options pool shouldRun initialSQL = do
    let mode = reductionColumnMode config
        context =
            SyntaxContext
                { syntaxLogger = logger
                , syntaxStore = poolMetricsStore pool
                , syntaxPool = pool
                , syntaxShouldRun = shouldRun
                , syntaxMode = mode
                , syntaxLiteralBatches = reductionLiteralBatches config
                , syntaxDistributedLiteralBatches = reductionDistributedLiteralBatches config
                , syntaxDistributedBatchConfig = reductionDistributedBatchConfig config
                , syntaxDeferLiteralVariants = reductionDeferLiteralVariants config
                , syntaxDeferRecursivePredicates = reductionDeferRecursivePredicates config
                , syntaxAstBatching = reductionAstBatching config
                , syntaxAdaptiveValues = reductionAdaptiveValues config
                , syntaxOptions = options
                , syntaxEarlyReject = reductionEarlyReject config
                , syntaxTracker = tracker
                , syntaxCert = emptyEarlyRejectCertifier
                , syntaxLookupTrace = lookupTrace
                }
    logDebug'
        logger
        "reducer"
        "Using ordinal-forward syntax candidate schedule"
        [
            ( "search_policy"
            , toJSON
                ( syntaxPolicyName mode
                    <> literalBatchPolicy (reductionLiteralBatches config)
                    <> distributedLiteralBatchPolicy (reductionDistributedLiteralBatches config)
                    <> deferLiteralVariantsPolicy (reductionDeferLiteralVariants config)
                    <> deferRecursivePredicatesPolicy (reductionDeferRecursivePredicates config)
                    <> astBatchPolicy (reductionAstBatching config)
                    <> valuesChunkPolicy (reductionAdaptiveValues config)
                )
            )
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
    mTrace <- syntaxLookupTrace context currentSQL
    cert <-
        buildCertifierTracked
            (syntaxEarlyReject context)
            (syntaxTracker context)
            currentSQL
            startUnits
            mTrace
    let context' = context {syntaxCert = cert}
    sweepSyntaxCategories
        context'
        stats
        currentSQL
        startUnits
        phases
        bundles
        (syntaxCategoryOrder (syntaxMode context'))
        False

sweepSyntaxCategories ::
    SyntaxContext ->
    IORef SyntaxStats ->
    ByteString ->
    [StatementUnit] ->
    SyntaxIndex ->
    [ColumnTarget] ->
    [ParserEditCategory] ->
    Bool ->
    IO ByteString
sweepSyntaxCategories context stats currentSQL units phases bundles [] shrunk
    | shrunk = runSyntaxFixedPoint context stats currentSQL
    | otherwise = runDeferredSyntaxPasses context stats currentSQL units phases bundles
sweepSyntaxCategories context stats currentSQL currentUnits phases bundles (category : rest) shrunk = do
    (batchSQL, batchUnits, batchPhases, batchBundles) <-
        runCategoryBatches context stats currentSQL currentUnits phases bundles category
    (nextSQL, nextUnits, nextPhases, nextBundles, categoryShrunk) <-
        exhaustSyntaxCategory context stats batchSQL batchUnits batchPhases batchBundles category
    sweepSyntaxCategories
        context
        stats
        nextSQL
        nextUnits
        nextPhases
        nextBundles
        rest
        (shrunk || categoryShrunk || BS.length batchSQL < BS.length currentSQL)

runDeferredSyntaxPasses ::
    SyntaxContext ->
    IORef SyntaxStats ->
    ByteString ->
    [StatementUnit] ->
    SyntaxIndex ->
    [ColumnTarget] ->
    IO ByteString
runDeferredSyntaxPasses context stats = go deferredSpecs
    where
        deferredValuesStream buckets _ ordinal =
            map SingleCandidate (drop ordinal (valuesChunkEdits True (phaseCandidates EditList buckets)))
        deferredLiteralsStream buckets _ ordinal =
            map SingleCandidate (bucketCandidatesFrom [DeferredLiterals] buckets ordinal)
        deferredRecursiveStream buckets _ ordinal =
            map SingleCandidate (bucketCandidatesFrom [DeferredRecursivePredicates] buckets ordinal)
        deferredSpecs =
            [(Just EditList, deferredValuesStream) | syntaxAdaptiveValues context]
                ++ [ (Just EditLiteral, deferredLiteralsStream) | syntaxDeferLiteralVariants context /= DeferLiteralsOff
                   ]
                ++ [(Nothing, deferredRecursiveStream) | syntaxDeferRecursivePredicates context]
        go [] sql _ _ _ = pure sql
        go ((mCategory, stream) : more) sql units phases bundles = do
            (nextSQL, nextUnits, nextPhases, nextBundles, accepted) <-
                exhaustSyntaxStream context stats sql units phases bundles mCategory stream
            if accepted
                then runSyntaxFixedPoint context stats nextSQL
                else go more nextSQL nextUnits nextPhases nextBundles

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
                    , ("window_seconds", toJSON (seconds (syntaxElapsed finalStats)))
                    ]
            else
                logDebug'
                    logger
                    "reducer"
                    "Syntax replacement completed with no accepted edits"
                    [ ("attempts", toJSON (syntaxAttempts finalStats))
                    , ("window_seconds", toJSON (seconds (syntaxElapsed finalStats)))
                    ]
