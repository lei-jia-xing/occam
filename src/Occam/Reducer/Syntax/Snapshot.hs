-- | Syntax byte-edit application, independent batch selection, and incremental snapshot refresh.
module Occam.Reducer.Syntax.Snapshot (
    replaceSpan,
    applyParserEdits,
    selectIndependentEdits,
    independentEditBatches,
    singleBytes,
    bundleBytes,
    parseSyntaxSnapshot,
    updateSyntaxSnapshot,
    updateSyntaxBatchSnapshot,
) where

import Control.Applicative (empty)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List (partition, sortOn)
import Data.Maybe (isNothing)
import Data.Ord (Down (..))
import Occam.Core (Span (..), Statement (..))
import Occam.Metrics (recordGeneratedSkipped)
import Occam.Parser (
    ColumnSite (..),
    ColumnTarget (..),
    ParseOptions (..),
    ParsedSQL (..),
    ParserEdit (..),
    ParserEditCategory (..),
    StatementUnit (..),
    parseSQLTimed,
 )
import Occam.Reducer.Support (hoistMaybe, kindOfOptions, parseUnitsOrThrow)
import Occam.Reducer.Syntax.Index (
    SyntaxContext (..),
    SyntaxIndex,
    buildSyntaxIndex,
    clauseEditRank,
    expressionEditRank,
    indexUnitEdits,
    literalSavings,
    replaceIndexedUnit,
    safeClauseRank,
    shiftColumnTarget,
    shiftParserEdit,
    shiftStatementUnit,
    unwrappedExpressionRank,
 )

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

-- | Select maximal non-interfering edits with disjoint, non-gluing spans.
selectIndependentEdits :: [ParserEdit] -> [ParserEdit]
selectIndependentEdits =
    sortOn parserEditStart
        . foldl' pick []
        . sortOn (\e -> (Down (literalSavings e), parserEditStart e))
    where
        pick acc edit
            | literalSavings edit <= 0 = acc
            | any (interferes edit) acc = acc
            | otherwise = edit : acc
        interferes a b =
            overlaps a b
                || (adjacent a b && not (BS.null (parserEditReplacement a) || BS.null (parserEditReplacement b)))
        overlaps a b = parserEditStart a < parserEditEnd b && parserEditStart b < parserEditEnd a
        adjacent a b = parserEditEnd a == parserEditStart b || parserEditEnd b == parserEditStart a

-- | Build independent edit batches (>= 2 edits) grouped by semantic rank/category.
independentEditBatches :: ParserEditCategory -> [ParserEdit] -> [[ParserEdit]]
independentEditBatches category edits =
    sortOn (Down . sum . map literalSavings) (filter (\g -> length g >= 2) rawGroups)
    where
        rawGroups = case category of
            EditClause ->
                partitionMax 16 (selectIndependentEdits [e | e <- edits, clauseEditRank e == safeClauseRank])
            EditExpression ->
                partitionMax
                    16
                    (selectIndependentEdits [e | e <- edits, expressionEditRank e == unwrappedExpressionRank])
            EditLiteral ->
                concatMap (partitionMax 16 . selectIndependentEdits) (groupByReplacement edits)
            _ ->
                partitionMax 16 (selectIndependentEdits edits)
        groupByReplacement [] = []
        groupByReplacement (first : rest) =
            let (same, others) = partition ((== parserEditReplacement first) . parserEditReplacement) rest
             in (first : same) : groupByReplacement others
        partitionMax _ [] = []
        partitionMax cap xs =
            let (chunk, rest) = splitAt cap xs
             in chunk : partitionMax cap rest

-- | Apply sorted disjoint parser edits to source bytes, enforcing strict byte shrink.
applyParserEdits :: ByteString -> [ParserEdit] -> Maybe ByteString
applyParserEdits source edits = case sortOn parserEditStart edits of
    [] -> Nothing
    first : rest
        | not (validEdit first) -> Nothing
        | otherwise -> go first rest [parserEditReplacement first, BS.take (parserEditStart first) source]
    where
        sourceLen = BS.length source
        validEdit e =
            let s = parserEditStart e
                eEnd = parserEditEnd e
             in s >= 0 && s < eEnd && eEnd <= sourceLen && BS.length (parserEditReplacement e) < eEnd - s
        go prev [] acc =
            let candidate = BS.concat (reverse (BS.drop (parserEditEnd prev) source : acc))
             in if BS.length candidate < sourceLen then Just candidate else Nothing
        go prev (e : more) acc
            | not (validEdit e) = Nothing
            | parserEditStart e < parserEditEnd prev = Nothing
            | parserEditStart e == parserEditEnd prev
            , not (BS.null (parserEditReplacement prev))
            , not (BS.null (parserEditReplacement e)) =
                Nothing
            | otherwise =
                let gap = BS.take (parserEditStart e - parserEditEnd prev) (BS.drop (parserEditEnd prev) source)
                 in go e more (parserEditReplacement e : gap : acc)

{- | Parse a single modified statement in isolation after splicing one or more
local edits, then shift its spans back to global coordinates.
-}
parseModifiedUnit ::
    SyntaxContext ->
    ParserEditCategory ->
    StatementUnit ->
    [ParserEdit] ->
    IO (Maybe (StatementUnit, Int))
parseModifiedUnit context category oldUnit edits = runMaybeT do
    let Span start_k end_k = stmtSpan (unitStatement oldUnit)
        oldRaw = stmtRaw (unitStatement oldUnit)
        localEdits = [shiftParserEdit (negate start_k) edit | edit <- edits]
        -- After EditList has completed in the current sweep, any accepted edit
        -- forces a full cycle restart (runSyntaxFixedPoint), so chunk edits are
        -- never consumed for the rest of this cycle.
        singleOptions
            | category `elem` [EditConstraint, EditType, EditExpression, EditLiteral] =
                (syntaxOptions context) {parseChunkEdits = False}
            | otherwise = syntaxOptions context
    newRaw <- hoistMaybe (applyParserEdits oldRaw localEdits)
    let delta = BS.length newRaw - (end_k - start_k)
    parsed <-
        liftIO $
            parseSQLTimed (syntaxStore context) (kindOfOptions singleOptions) singleOptions newRaw
    singleUnit <- hoistMaybe (singleParsedUnit parsed)
    let renamed =
            singleUnit
                { unitStatement = (unitStatement singleUnit) {stmtId = stmtId (unitStatement oldUnit)}
                }
    pure (shiftStatementUnit start_k renamed, delta)
    where
        singleParsedUnit (Right result)
            | [unit] <- parsedUnits result = Just unit
        singleParsedUnit _ = Nothing

-- | Incrementally update syntax snapshot by reparsing only the modified statement.
updateSyntaxSnapshot ::
    SyntaxContext ->
    ParserEditCategory ->
    [StatementUnit] ->
    SyntaxIndex ->
    [ColumnTarget] ->
    ParserEdit ->
    ByteString ->
    IO ([StatementUnit], SyntaxIndex, [ColumnTarget])
updateSyntaxSnapshot context category currentUnits index currentBundles edit =
    updateSyntaxBatchSnapshot context category currentUnits index currentBundles [edit]

{- | Incrementally refresh an accepted single- or multi-edit syntax batch by
reparsing only the touched statements through the Rust parser and shifting
untouched statement spans by the cumulative byte delta, with full reparse fallback.
-}
updateSyntaxBatchSnapshot ::
    SyntaxContext ->
    ParserEditCategory ->
    [StatementUnit] ->
    SyntaxIndex ->
    [ColumnTarget] ->
    [ParserEdit] ->
    ByteString ->
    IO ([StatementUnit], SyntaxIndex, [ColumnTarget])
updateSyntaxBatchSnapshot context _ _ _ _ _ acceptedBytes
    | syntaxFullReparse context = parseSyntaxSnapshot context acceptedBytes
updateSyntaxBatchSnapshot context category currentUnits index currentBundles batch acceptedBytes = do
    incremental <- runMaybeT do
        guard (category /= EditColumn)
        let sortedEdits = sortOn parserEditStart batch
        guard (validBatch sortedEdits)
        (freshUnits, freshIndex, unitDeltasRev) <-
            updateBatchUnits context category currentBundles 0 0 [] index [] currentUnits sortedEdits
        let freshBundles =
                foldl' (\bs (oldEnd, delta) -> map (shiftColumnTarget oldEnd delta) bs) currentBundles unitDeltasRev
        pure (freshUnits, freshIndex, freshBundles)
    maybe (parseSyntaxSnapshot context acceptedBytes) pure incremental
    where
        validEdit e = parserEditStart e >= 0 && literalSavings e > 0
        validBatch [] = False
        validBatch [e] = validEdit e
        validBatch (a : b : rest) =
            validEdit a
                && ( parserEditEnd a < parserEditStart b
                        || ( parserEditEnd a == parserEditStart b
                                && (BS.null (parserEditReplacement a) || BS.null (parserEditReplacement b))
                           )
                   )
                && validBatch (b : rest)

updateBatchUnits ::
    SyntaxContext ->
    ParserEditCategory ->
    [ColumnTarget] ->
    Int ->
    Int ->
    [StatementUnit] ->
    SyntaxIndex ->
    [(Int, Int)] ->
    [StatementUnit] ->
    [ParserEdit] ->
    MaybeT IO ([StatementUnit], SyntaxIndex, [(Int, Int)])
updateBatchUnits _ _ _ _ _ accUnitsRev accIndex unitDeltasRev [] [] =
    pure (reverse accUnitsRev, accIndex, unitDeltasRev)
updateBatchUnits _ _ _ _ _ _ _ _ [] (_ : _) = empty
updateBatchUnits context category currentBundles !unitIdx !cumDelta accUnitsRev accIndex unitDeltasRev (unit : moreUnits) remEdits
    | null touching =
        let shiftedUnit = if cumDelta == 0 then unit else shiftStatementUnit cumDelta unit
         in updateBatchUnits
                context
                category
                currentBundles
                (unitIdx + 1)
                cumDelta
                (shiftedUnit : accUnitsRev)
                accIndex
                unitDeltasRev
                moreUnits
                restEdits
    | otherwise = do
        guard (all (`containsEdit` unit) touching)
        guard (not (hasAffectedBundleSite currentBundles unit))
        (updatedLocal, unitDelta) <- MaybeT (parseModifiedUnit context category unit touching)
        let finalUnit = if cumDelta == 0 then updatedLocal else shiftStatementUnit cumDelta updatedLocal
            accIndex' = replaceIndexedUnit unitIdx unitDelta (indexUnitEdits context finalUnit) accIndex
            oldEnd = spanEndByte (stmtSpan (unitStatement unit))
            unitDeltasRev' = if unitDelta == 0 then unitDeltasRev else (oldEnd, unitDelta) : unitDeltasRev
        updateBatchUnits
            context
            category
            currentBundles
            (unitIdx + 1)
            (cumDelta + unitDelta)
            (finalUnit : accUnitsRev)
            accIndex'
            unitDeltasRev'
            moreUnits
            restEdits
    where
        Span _ unitEnd = stmtSpan (unitStatement unit)
        (touching, restEdits) = span (\e -> parserEditStart e < unitEnd) remEdits

hasAffectedBundleSite :: [ColumnTarget] -> StatementUnit -> Bool
hasAffectedBundleSite currentBundles u =
    let Span oldStart oldEnd = stmtSpan (unitStatement u)
     in any
            (any (\site -> siteStart site >= oldStart && siteStart site < oldEnd) . targetSites)
            currentBundles

containsEdit :: ParserEdit -> StatementUnit -> Bool
containsEdit edit unit =
    let Span start end = stmtSpan (unitStatement unit)
     in parserEditStart edit >= start && parserEditEnd edit <= end

-- | Parse a full snapshot into units, ordered edit buckets, and unblocked column bundles.
parseSyntaxSnapshot ::
    SyntaxContext ->
    ByteString ->
    IO ([StatementUnit], SyntaxIndex, [ColumnTarget])
parseSyntaxSnapshot context currentSQL = do
    (_, units, columns) <-
        parseUnitsOrThrow (syntaxStore context) (syntaxOptions context) currentSQL
    -- Record skipped metrics for blocked bundles.
    mapM_
        (maybe (pure ()) (recordGeneratedSkipped (syntaxStore context)) . targetBlocked)
        columns
    -- Fold into ordered buckets once; category transitions reuse this snapshot.
    pure
        ( units
        , buildSyntaxIndex context units
        , [target | target <- columns, isNothing (targetBlocked target)]
        )
