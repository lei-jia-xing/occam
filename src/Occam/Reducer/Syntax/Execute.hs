-- | Syntax candidate stream execution, window probing, and snapshot refresh.
module Occam.Reducer.Syntax.Execute (
    exhaustSyntaxCategory,
    exhaustSyntaxStream,
    testSyntaxWindow,
    recordSyntaxTest,
    testSyntaxCandidates,
    logAcceptedSyntaxEdit,
    valuesChunkEdits,
) where

import Control.Applicative (empty)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson (toJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.IORef (IORef, modifyIORef')
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Certificate (
    checkEarlyRejectionBytesTracked,
    checkEarlyRejectionSyntaxEditTracked,
 )
import Occam.Core (Ident (..), OracleOutcome (..), nameText)
import Occam.Logger (logInfo')
import Occam.Metrics (
    FamilyTag (..),
    recordCandidateBuildTiming,
    recordGeneratedSkipped,
 )
import Occam.Parser (
    ColumnTarget (..),
    ParserEdit (..),
    ParserEditCategory (..),
    StatementUnit,
 )
import Occam.Pool (
    reportCandidateCount,
    testOrderedGenerated,
    testOrderedTagged,
 )
import Occam.Reducer.Config (ColumnMode (..))
import Occam.Reducer.Support (rejectIfEarly)
import Occam.Reducer.Syntax.Index (
    SyntaxCandidate (..),
    SyntaxContext (..),
    SyntaxIndex,
    SyntaxStats (..),
    phaseCandidates,
    phaseCandidatesFrom,
 )
import Occam.Reducer.Syntax.Snapshot (
    applyParserEdits,
    bundleBytes,
    parseSyntaxSnapshot,
    singleBytes,
    updateSyntaxSnapshot,
 )

-- | Exhaustively apply edits of one category until no further reductions succeed.
exhaustSyntaxCategory ::
    SyntaxContext ->
    IORef SyntaxStats ->
    ByteString ->
    [StatementUnit] ->
    SyntaxIndex ->
    [ColumnTarget] ->
    ParserEditCategory ->
    IO (ByteString, [StatementUnit], SyntaxIndex, [ColumnTarget], Bool)
exhaustSyntaxCategory context stats sql units phases bundles category =
    exhaustSyntaxStream
        context
        stats
        sql
        units
        phases
        bundles
        (Just category)
        (\buckets targets -> initialSyntaxStream context buckets targets category)

exhaustSyntaxStream ::
    SyntaxContext ->
    IORef SyntaxStats ->
    ByteString ->
    [StatementUnit] ->
    SyntaxIndex ->
    [ColumnTarget] ->
    Maybe ParserEditCategory ->
    (SyntaxIndex -> [ColumnTarget] -> Int -> [SyntaxCandidate]) ->
    IO (ByteString, [StatementUnit], SyntaxIndex, [ColumnTarget], Bool)
exhaustSyntaxStream context stats sourceSQL sourceUnits phases bundles mCategory stream =
    sweep False 0 sourceSQL sourceUnits phases bundles (stream phases bundles 0)
    where
        sweep changed _ currentSQL currentUnits currentPhases currentBundles [] =
            pure (currentSQL, currentUnits, currentPhases, currentBundles, changed)
        sweep changed ordinal currentSQL currentUnits currentPhases currentBundles candidates = do
            (tested, elapsed, accepted) <-
                testSyntaxCandidates context currentUnits currentSQL ordinal candidates
            modifyIORef' stats (recordSyntaxTest tested elapsed accepted)
            case accepted of
                Nothing -> pure (currentSQL, currentUnits, currentPhases, currentBundles, changed)
                Just (acceptedOrdinal, candidate, acceptedBytes) -> do
                    logAcceptedSyntaxEdit context candidate currentSQL acceptedBytes
                    (freshUnits, freshPhases, freshBundles) <-
                        refreshAcceptedSyntaxSnapshot
                            context
                            mCategory
                            currentUnits
                            currentPhases
                            currentBundles
                            candidate
                            acceptedBytes
                    let freshCandidates = stream freshPhases freshBundles acceptedOrdinal
                    sweep True acceptedOrdinal acceptedBytes freshUnits freshPhases freshBundles freshCandidates

refreshAcceptedSyntaxSnapshot ::
    SyntaxContext ->
    Maybe ParserEditCategory ->
    [StatementUnit] ->
    SyntaxIndex ->
    [ColumnTarget] ->
    SyntaxCandidate ->
    ByteString ->
    IO ([StatementUnit], SyntaxIndex, [ColumnTarget])
refreshAcceptedSyntaxSnapshot context mCategory currentUnits currentPhases currentBundles (SingleCandidate edit) acceptedBytes
    | let editCat = fromMaybe (parserEditCategory edit) mCategory
    , editCat /= EditColumn =
        updateSyntaxSnapshot context editCat currentUnits currentPhases currentBundles edit acceptedBytes
refreshAcceptedSyntaxSnapshot context _ _ _ _ _ acceptedBytes =
    parseSyntaxSnapshot context acceptedBytes

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
    SyntaxIndex ->
    [ColumnTarget] ->
    ParserEditCategory ->
    Int ->
    [SyntaxCandidate]
initialSyntaxStream context phases bundles category ordinal
    | category == EditColumn && syntaxMode context == ColumnAtomic =
        map BundleCandidate (drop ordinal bundles)
            ++ map SingleCandidate (phaseCandidatesFrom category phases (max 0 (ordinal - length bundles)))
    | category == EditList && syntaxAdaptiveValues context =
        map SingleCandidate (drop ordinal (valuesChunkEdits False (phaseCandidates category phases)))
    | otherwise = map SingleCandidate (phaseCandidatesFrom category phases ordinal)

-- The frontend emits each VALUES list's chunks coarse-to-fine, then singles.
-- Leave its coarse pair in the regular sweep and revisit the finer candidates
-- only when an entire cycle finds no reductions.
valuesChunkEdits :: Bool -> [ParserEdit] -> [ParserEdit]
valuesChunkEdits _ [] = []
valuesChunkEdits deferred edits@(edit : rest)
    | isChunk edit =
        let (chunks, remaining) = span isChunk edits
            (coarse, fine) = splitAt 2 chunks
         in (if deferred then fine else coarse) ++ valuesChunkEdits deferred remaining
    | deferred = valuesChunkEdits deferred rest
    | otherwise = edit : valuesChunkEdits deferred rest
    where
        isChunk candidate = parserEditLabel candidate == "remove-values_row-chunk"

-- | Build and execute eligible syntax candidates as the ordered pool asks for them.
testSyntaxCandidates ::
    SyntaxContext ->
    [StatementUnit] ->
    ByteString ->
    Int ->
    [SyntaxCandidate] ->
    IO (Int, Word64, Maybe (Int, SyntaxCandidate, ByteString))
testSyntaxCandidates context currentUnits currentSQL startOrdinal candidates = do
    started <- getMonotonicTimeNSec
    (tested, accepted) <-
        testOrderedGenerated pool nextCandidate retire (0, Nothing) (startOrdinal, candidates)
    finished <- getMonotonicTimeNSec
    pure (tested, finished - started, accepted)
    where
        pool = syntaxPool context
        currentCount = length currentUnits
        nextCandidate (_, []) = pure Nothing
        nextCandidate (ordinal, candidate : rest) = do
            let next = (ordinal + 1, rest)
            stepCandidate candidate >>= \case
                Nothing -> nextCandidate next
                Just bytes -> do
                    reportCandidateCount pool bytes currentCount
                    pure (Just ((candidateTag candidate, bytes, (ordinal, candidate)), next))

        retire (!tested, _) (ordinal, candidate) bytes (outcome, _) =
            pure (tested + 1, if outcome == OracleAccepted then Just (ordinal, candidate, bytes) else Nothing)

        stepCandidate candidate = runMaybeT do
            let store = syntaxStore context
            for_ (singleEdit candidate) \edit ->
                rejectIfEarly store
                    =<< liftIO
                        ( checkEarlyRejectionSyntaxEditTracked
                            (syntaxTracker context)
                            currentUnits
                            (syntaxCert context)
                            edit
                        )
            (built, buildMicros) <- liftIO $ timeSyntaxCandidateBuild context currentSQL candidate
            bytes <- case built of
                Nothing -> liftIO (recordGeneratedSkipped store "nonshrinking") >> empty
                Just b -> pure b
            liftIO $ recordCandidateBuildTiming store (candidateTag candidate) buildMicros
            rejectIfEarly store
                =<< liftIO (checkEarlyRejectionBytesTracked (syntaxTracker context) (syntaxCert context) True bytes)
            pure bytes

        singleEdit (SingleCandidate edit) = Just edit
        singleEdit _ = Nothing

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
            BatchCandidate _ _ edits -> applyParserEdits currentSQL edits

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
logAcceptedSyntaxEdit context (BatchCandidate category label edits) currentSQL acceptedBytes =
    logInfo'
        (syntaxLogger context)
        "reducer"
        "Accepted syntax edit batch"
        [ ("rewrite_kind", toJSON label)
        , ("rewrite_category", toJSON category)
        , ("edits_count", toJSON (length edits))
        , ("bytes_before", toJSON (BS.length currentSQL))
        , ("bytes_after", toJSON (BS.length acceptedBytes))
        ]

candidateTag :: SyntaxCandidate -> FamilyTag
candidateTag (SingleCandidate edit) = TagSyntax (parserEditCategory edit) (parserEditLabel edit)
candidateTag (BundleCandidate _) = TagBundle "remove-column-bundle"
candidateTag (BatchCandidate category label _) = TagSyntax category label
