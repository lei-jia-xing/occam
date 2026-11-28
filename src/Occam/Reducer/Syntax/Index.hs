-- | Syntax edit classification, coordinate shifting, and balanced edit-index tree.
module Occam.Reducer.Syntax.Index (
    SyntaxStats (..),
    allEditCategories,
    SyntaxContext (..),
    SyntaxCandidate (..),
    safeClauseRank,
    unwrappedExpressionRank,
    clauseEditRank,
    expressionEditRank,
    syntaxCategoryOrder,
    syntaxCategoryOrderName,
    EditBucket (..),
    IndexedUnitEdits,
    SyntaxIndex (..),
    indexUnitEdits,
    buildSyntaxIndex,
    replaceIndexedUnit,
    bucketCandidatesFrom,
    phaseCandidatesFrom,
    phaseCandidates,
    isPrimaryLiteralEdit,
    shiftStatementUnit,
    shiftParserEdit,
    shiftColumnTarget,
    literalSavings,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortOn)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Word (Word64)
import Occam.Certificate (EarlyRejectCertifier, EarlyRejectTracker)
import Occam.Core.Types (ResultTrace, Span (..), Statement (..))
import Occam.Facts (ClauseTier (..), EditTier (..), ExprTier (..), editCategoryName)
import Occam.Logger (Logger)
import Occam.Metrics (MetricsStore)
import Occam.Parser (
    ColumnSite (..),
    ColumnTarget (..),
    ParseOptions,
    ParserEdit (..),
    ParserEditCategory (..),
    StatementFacts (..),
    StatementUnit (..),
 )
import Occam.Pool (OraclePool)
import Occam.Reducer.Config (ColumnMode (..), DistributedBatchConfig, LiteralDeferralMode (..))

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
    , syntaxLiteralBatches :: !Bool
    , syntaxDistributedLiteralBatches :: !Bool
    , syntaxDistributedBatchConfig :: !DistributedBatchConfig
    , syntaxDeferLiteralVariants :: !LiteralDeferralMode
    , syntaxDeferRecursivePredicates :: !Bool
    , syntaxAstBatching :: !Bool
    , syntaxAdaptiveValues :: !Bool
    , syntaxFullReparse :: !Bool
    , syntaxOptions :: !ParseOptions
    , syntaxEarlyReject :: !Bool
    , syntaxTracker :: !EarlyRejectTracker
    , syntaxCert :: !EarlyRejectCertifier
    , syntaxLookupTrace :: ByteString -> IO (Maybe ResultTrace)
    }

data SyntaxCandidate
    = SingleCandidate !ParserEdit
    | BundleCandidate !ColumnTarget
    | BatchCandidate !ParserEditCategory !T.Text ![ParserEdit]

classifyClauseEdit :: ParserEdit -> ClauseTier
classifyClauseEdit edit = case parserEditTier edit of
    TierClause tier -> tier
    _ -> StructuralClause

classifyExpressionEdit :: ParserEdit -> ExprTier
classifyExpressionEdit edit
    | parserEditLowYield edit = AggressiveExpr
    | TierExpr tier <- parserEditTier edit = tier
    | otherwise = AggressiveExpr

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
    SafeClause -> safeClauseRank
    StructuralClause -> structuralClauseRank
    FragileClause -> fragileClauseRank

-- | Priority ranking for expression edits (preferring unwrap transforms before aggressive replacements).
expressionEditRank :: ParserEdit -> Int
expressionEditRank edit = case classifyExpressionEdit edit of
    UnwrapExpr -> unwrappedExpressionRank
    AggressiveExpr -> aggressiveExpressionRank

-- | Column edits only participate when the column mode is enabled.
syntaxCategoryOrder :: ColumnMode -> [ParserEditCategory]
syntaxCategoryOrder ColumnOff = filter (/= EditColumn) allEditCategories
syntaxCategoryOrder _ = allEditCategories

-- | Comma-separated canonical names of the active rewrite categories for telemetry logging.
syntaxCategoryOrderName :: ColumnMode -> T.Text
syntaxCategoryOrderName = T.intercalate "," . map editCategoryName . syntaxCategoryOrder

-- | Priority buckets preserving category ordering across statement boundaries.
data EditBucket
    = SafeClauses
    | StructuralClauses
    | FragileClauses
    | Lists
    | Columns
    | Constraints
    | Types
    | UnwrapExpressions
    | AggressiveExpressions
    | PrimaryLiterals
    | DeferredLiterals
    | DeferredRecursivePredicates
    deriving (Eq, Ord, Enum)

-- | Per-statement map from 'EditBucket' ordinal to queued syntax edits.
type IndexedUnitEdits = IntMap.IntMap (Seq ParserEdit)

{- | A fixed-shape tree of statement units. Branches cache edit counts for
ordinal seeking and the sum of byte changes made below them. Unchanged
right subtrees stay in their original coordinates after an earlier edit.
-}
data SyntaxIndex
    = EmptySyntaxIndex
    | IndexedUnit !Int !IndexedUnitEdits
    | IndexedUnits !Int !Int !(IntMap.IntMap Int) !SyntaxIndex !SyntaxIndex

-- | Bucket all syntax edits extracted from a single statement unit.
indexUnitEdits :: SyntaxContext -> StatementUnit -> IndexedUnitEdits
indexUnitEdits context unit =
    IntMap.mapWithKey finalize (fst (Foldable.foldl' insertEdit (IntMap.empty, Set.empty) filtered))
    where
        edits = maybe [] factEdits (unitFacts unit)
        filtered = filter (syntaxShouldRun context) edits
        deferLiterals = syntaxDeferLiteralVariants context
        deferRecursive = syntaxDeferRecursivePredicates context
        deferredRecKey = fromEnum DeferredRecursivePredicates
        finalize key revEdits
            | key == deferredRecKey = Seq.fromList (sortOn (fromEnum . baseEditBucket) (reverse revEdits))
            | otherwise = Seq.fromList (reverse revEdits)
        insertEdit (!buckets, !seenLitSpans) edit
            | deferRecursive && parserEditRecursivePredicate edit =
                (IntMap.insertWith (++) deferredRecKey [edit] buckets, seenLitSpans)
            | deferLiterals /= DeferLiteralsOff && parserEditCategory edit == EditLiteral =
                if isPrimaryLiteralEdit deferLiterals seenLitSpans edit
                    then
                        ( IntMap.insertWith (++) (fromEnum PrimaryLiterals) [edit] buckets
                        , Set.insert (parserEditStart edit, parserEditEnd edit) seenLitSpans
                        )
                    else (IntMap.insertWith (++) (fromEnum DeferredLiterals) [edit] buckets, seenLitSpans)
            | otherwise =
                (IntMap.insertWith (++) (fromEnum (baseEditBucket edit)) [edit] buckets, seenLitSpans)
        baseEditBucket edit = case parserEditCategory edit of
            EditClause -> case classifyClauseEdit edit of
                SafeClause -> SafeClauses
                StructuralClause -> StructuralClauses
                FragileClause -> FragileClauses
            EditList -> Lists
            EditColumn -> Columns
            EditConstraint -> Constraints
            EditType -> Types
            EditExpression -> case classifyExpressionEdit edit of
                UnwrapExpr -> UnwrapExpressions
                AggressiveExpr -> AggressiveExpressions
            EditLiteral -> PrimaryLiterals

-- | Construct a balanced 'SyntaxIndex' segment tree over parsed statement units.
buildSyntaxIndex :: SyntaxContext -> [StatementUnit] -> SyntaxIndex
buildSyntaxIndex context = build . Seq.fromList
    where
        build units = case Seq.viewl units of
            Seq.EmptyL -> EmptySyntaxIndex
            unit Seq.:< rest
                | Seq.null rest -> IndexedUnit 0 (indexUnitEdits context unit)
                | otherwise ->
                    let (left, right) = Seq.splitAt (Seq.length units `div` 2) units
                     in joinSyntaxIndex (build left) (build right)

joinSyntaxIndex :: SyntaxIndex -> SyntaxIndex -> SyntaxIndex
joinSyntaxIndex left right =
    IndexedUnits
        (indexUnitCount left + indexUnitCount right)
        (indexDelta left + indexDelta right)
        (IntMap.unionWith (+) (counts left) (counts right))
        left
        right
    where
        counts EmptySyntaxIndex = IntMap.empty
        counts (IndexedUnit _ edits) = IntMap.map Seq.length edits
        counts (IndexedUnits _ _ totals _ _) = totals

indexUnitCount :: SyntaxIndex -> Int
indexUnitCount EmptySyntaxIndex = 0
indexUnitCount IndexedUnit {} = 1
indexUnitCount (IndexedUnits count _ _ _ _) = count

indexDelta :: SyntaxIndex -> Int
indexDelta EmptySyntaxIndex = 0
indexDelta (IndexedUnit shift _) = shift
indexDelta (IndexedUnits _ shift _ _ _) = shift

replaceIndexedUnit :: Int -> Int -> IndexedUnitEdits -> SyntaxIndex -> SyntaxIndex
replaceIndexedUnit unitOrdinal delta edits = update unitOrdinal 0
    where
        update _ _ EmptySyntaxIndex = EmptySyntaxIndex
        update _ prefix (IndexedUnit previous _) =
            let local
                    | prefix == 0 = edits
                    | otherwise = IntMap.map (fmap (shiftParserEdit (negate prefix))) edits
             in IndexedUnit (previous + delta) local
        update position prefix (IndexedUnits _ _ _ left right)
            | position < leftCount =
                joinSyntaxIndex (update position prefix left) right
            | otherwise =
                joinSyntaxIndex left (update (position - leftCount) (prefix + indexDelta left) right)
            where
                leftCount = indexUnitCount left

bucketCandidatesFrom :: [EditBucket] -> SyntaxIndex -> Int -> [ParserEdit]
bucketCandidatesFrom buckets index = visitBuckets buckets
    where
        visitBuckets [] _ = []
        visitBuckets (bucket : rest) ordinal
            | ordinal >= count = visitBuckets rest (ordinal - count)
            | otherwise = visit key ordinal 0 index (visitBuckets rest 0)
            where
                key = fromEnum bucket
                count = bucketCount key index

        bucketCount _ EmptySyntaxIndex = 0
        bucketCount key (IndexedUnit _ edits) = maybe 0 Seq.length (IntMap.lookup key edits)
        bucketCount key (IndexedUnits _ _ totals _ _) = IntMap.findWithDefault 0 key totals

        visit _ _ _ EmptySyntaxIndex rest = rest
        visit key ordinal shift (IndexedUnit _ edits) rest =
            case IntMap.lookup key edits of
                Nothing -> rest
                Just bucket ->
                    foldr
                        (\edit -> (:) (if shift == 0 then edit else shiftParserEdit shift edit))
                        rest
                        (Seq.drop ordinal bucket)
        visit key ordinal shift (IndexedUnits _ _ _ left right) rest
            | ordinal >= leftCount = visit key (ordinal - leftCount) rightShift right rest
            | otherwise = visit key ordinal shift left (visit key 0 rightShift right rest)
            where
                leftCount = bucketCount key left
                rightShift = shift + indexDelta left

phaseCandidatesFrom :: ParserEditCategory -> SyntaxIndex -> Int -> [ParserEdit]
phaseCandidatesFrom category = bucketCandidatesFrom (phaseBuckets category)
    where
        phaseBuckets EditClause = [SafeClauses, StructuralClauses, FragileClauses]
        phaseBuckets EditList = [Lists]
        phaseBuckets EditColumn = [Columns]
        phaseBuckets EditConstraint = [Constraints]
        phaseBuckets EditType = [Types]
        phaseBuckets EditExpression = [UnwrapExpressions, AggressiveExpressions]
        phaseBuckets EditLiteral = [PrimaryLiterals]

phaseCandidates :: ParserEditCategory -> SyntaxIndex -> [ParserEdit]
phaseCandidates category index = phaseCandidatesFrom category index 0

isPrimaryLiteralEdit :: LiteralDeferralMode -> Set (Int, Int) -> ParserEdit -> Bool
isPrimaryLiteralEdit DeferLiteralsOff _ _ = True
isPrimaryLiteralEdit DeferLiteralsZeroOne _ edit =
    not (parserEditLowYield edit)

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

literalSavings :: ParserEdit -> Int
literalSavings edit = parserEditEnd edit - parserEditStart edit - BS.length (parserEditReplacement edit)
