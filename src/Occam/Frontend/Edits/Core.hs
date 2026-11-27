-- | Core edit accumulator, list/chunk helpers, clause/type emission, and recursive predicate tracking.
module Occam.Frontend.Edits.Core (
    EditEnv (..),
    EditM,
    runEditM,
    askCtx,
    askTokens,
    askSource,
    askUnitSpan,
    absSpan,
    absTuple,
    sliceAbs,

    -- * Edit emission
    addEditBytes,
    addClauseEdit,
    addExprEdit,
    addLowYieldExprEdit,
    addEditBytesWithFlags,
    markEditsTopProjection,
    withRecursivePredicates,
    addRecursivePredicateSpans,
    getRecursivePredicateSpans,
    markLastEditRecursiveIf,

    -- * List, chunk, clause, and type helpers
    listEdits,
    listEditRanges,
    listChunkRanges,
    chunkLabel,
    clauseEditComplete,
    clauseEditRange,
    addTypeEdits,
) where

import Control.Monad (forM_, when)
import Control.Monad.Trans.State.Strict (State, execState, get, gets, modify', put)
import Data.Bits (shiftL)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Vector.Unboxed qualified as VU
import Occam.Facts (
    ClauseTier (..),
    EditTier (..),
    ExprTier (..),
    ParserEdit (..),
    ParserEditCategory (..),
 )
import Occam.Frontend.Context (AnalysisContext (..), toAbsoluteSpan)
import Occam.Frontend.Edits.Tokens (
    TokenIndex,
    buildTokenIndex,
    clauseEndAfter,
    findComma,
    firstWordInRange,
    keywordBefore,
    precedingSpace,
 )
import Occam.Frontend.Span (ByteSpan (..), Located (..), isUtf8Boundary, sliceSpan)

-- | Mutable state threaded through the edit-generation traversal.
data EditEnv = EditEnv
    { editCtx :: !AnalysisContext
    , editTokens :: !TokenIndex
    , editAcc :: ![ParserEdit]
    -- ^ Reversed accumulator of emitted edits.
    , editRecursivePreds :: ![(Int, Int)]
    -- ^ Absolute @(start, end)@ spans of predicates inside recursive CTE members.
    }

-- | Strict state monad collecting intra-statement 'ParserEdit' candidates.
type EditM = State EditEnv

-- | Execute an 'EditM' traversal for a single statement unit.
runEditM :: AnalysisContext -> EditM () -> [ParserEdit]
runEditM ctx action =
    reverse (editAcc (execState action initialEnv))
    where
        !tokIdx = buildTokenIndex (ctxAstSpanShift ctx) (ctxUnitTokens ctx)
        initialEnv =
            EditEnv
                { editCtx = ctx
                , editTokens = tokIdx
                , editAcc = []
                , editRecursivePreds = []
                }

{-# INLINE askCtx #-}
askCtx :: EditM AnalysisContext
askCtx = gets editCtx

{-# INLINE askTokens #-}
askTokens :: EditM TokenIndex
askTokens = gets editTokens

{-# INLINE askSource #-}
askSource :: EditM ByteString
askSource = gets (ctxSource . editCtx)

{-# INLINE askUnitSpan #-}
askUnitSpan :: EditM ByteSpan
askUnitSpan = gets (ctxUnitSpan . editCtx)

-- | Convert an AST 'ByteSpan' to an absolute 'ByteSpan' in 'ctxSource'.
{-# INLINE absSpan #-}
absSpan :: AnalysisContext -> ByteSpan -> ByteSpan
absSpan = toAbsoluteSpan

-- | Convert an AST 'ByteSpan' to an absolute @(start, end)@ pair.
{-# INLINE absTuple #-}
absTuple :: AnalysisContext -> ByteSpan -> (Int, Int)
absTuple ctx sp =
    let ByteSpan s e = toAbsoluteSpan ctx sp
     in (s, e)

-- | Checked slice of 'ctxSource' at an absolute @(start, end)@ range.
sliceAbs :: ByteString -> (Int, Int) -> Maybe ByteString
sliceAbs src (s, e) = sliceSpan (ByteSpan s e) src

-- ---------------------------------------------------------------------------
-- Edit emission
-- ---------------------------------------------------------------------------

-- | Emit a standard (default-tier, non-string-literal, non-low-yield) edit.
addEditBytes :: Int -> Int -> ByteString -> ParserEditCategory -> Text -> EditM ()
addEditBytes start end replacement category label =
    addEditBytesWithFlags start end replacement category TierDefault label False False

-- | Emit a typed clause-removal edit.
addClauseEdit :: ClauseTier -> Int -> Int -> ByteString -> Text -> EditM ()
addClauseEdit tier start end replacement label =
    addEditBytesWithFlags start end replacement EditClause (TierClause tier) label False False

-- | Emit a typed expression-simplification edit.
addExprEdit :: ExprTier -> Int -> Int -> ByteString -> Text -> EditM ()
addExprEdit tier start end replacement label =
    addEditBytesWithFlags start end replacement EditExpression (TierExpr tier) label False False

-- | Emit a low-yield aggressive expression-simplification edit.
addLowYieldExprEdit :: Int -> Int -> ByteString -> Text -> EditM ()
addLowYieldExprEdit start end replacement label =
    addEditBytesWithFlags
        start
        end
        replacement
        EditExpression
        (TierExpr AggressiveExpr)
        label
        False
        True

{- | Emit an edit with explicit priority tier, @stringLiteral@, and @lowYield@ flags.

Validates byte bounds against 'ctxUnitSpan' and UTF-8 char boundaries, and sets
'parserEditRecursivePredicate' when the edit span falls within any active
recursive CTE predicate span.
-}
addEditBytesWithFlags ::
    Int ->
    Int ->
    ByteString ->
    ParserEditCategory ->
    EditTier ->
    Text ->
    Bool ->
    Bool ->
    EditM ()
addEditBytesWithFlags start end replacement category tier label isString lowYield = do
    env <- get
    let ctx = editCtx env
        src = ctxSource ctx
        ByteSpan unitStart unitEnd = ctxUnitSpan ctx
    when
        ( ctxSyntaxEdits ctx
            && start >= unitStart
            && start < end
            && end <= unitEnd
            && end <= BS.length src
            && isUtf8Boundary src start
            && isUtf8Boundary src end
        )
        do
            let recPreds = editRecursivePreds env
                !isRec = not (null recPreds) && any (\(s, e) -> s <= start && end <= e) recPreds
                !edit =
                    ParserEdit
                        { parserEditStart = start
                        , parserEditEnd = end
                        , parserEditReplacement = replacement
                        , parserEditCategory = category
                        , parserEditTier = tier
                        , parserEditLabel = label
                        , parserEditTopProjection = False
                        , parserEditStringLiteral = isString
                        , parserEditRecursivePredicate = isRec
                        , parserEditLowYield = lowYield
                        }
            put env {editAcc = edit : editAcc env}

-- | Run @action@ and mark every edit emitted during @action@ with 'parserEditTopProjection'.
markEditsTopProjection :: EditM () -> EditM ()
markEditsTopProjection action = do
    before <- gets (length . editAcc)
    action
    modify' \env ->
        let acc = editAcc env
            !added = length acc - before
         in if added <= 0
                then env
                else
                    let (newEdits, oldEdits) = splitAt added acc
                     in env {editAcc = map (\e -> e {parserEditTopProjection = True}) newEdits ++ oldEdits}

-- | Append recursive-member predicate spans to the active set.
addRecursivePredicateSpans :: [(Int, Int)] -> EditM ()
addRecursivePredicateSpans spans =
    unlessNull spans $
        modify' \env -> env {editRecursivePreds = editRecursivePreds env ++ spans}
    where
        unlessNull [] _ = pure ()
        unlessNull _ m = m

-- | Run @action@ with additional recursive predicate spans (which persist in visitor state, matching Rust).
withRecursivePredicates :: [(Int, Int)] -> EditM () -> EditM ()
withRecursivePredicates spans action = do
    addRecursivePredicateSpans spans
    action

getRecursivePredicateSpans :: EditM [(Int, Int)]
getRecursivePredicateSpans = gets editRecursivePreds

{- | If @action@ emitted an edit and @cond@ holds on the recursive predicate set,
force 'parserEditRecursivePredicate' on the edit emitted by @action@ (mirrors Rust
tagging @remove-where@ when the WHERE expression span matches a recursive predicate).
-}
markLastEditRecursiveIf :: ([(Int, Int)] -> Bool) -> EditM () -> EditM ()
markLastEditRecursiveIf cond action = do
    before <- gets (length . editAcc)
    action
    env <- get
    let acc = editAcc env
        !added = length acc - before
    when (added > 0 && cond (editRecursivePreds env)) do
        case acc of
            e : rest -> put env {editAcc = e {parserEditRecursivePredicate = True} : rest}
            [] -> pure ()

-- ---------------------------------------------------------------------------
-- List & chunk edits
-- ---------------------------------------------------------------------------

-- | Emit comma-separated list item deletions (and chunks when enabled) when every item resolves to a span.
listEdits :: Text -> [a] -> (a -> EditM (Maybe (Int, Int))) -> EditM ()
listEdits label items getSpan
    | length items < 2 = pure ()
    | otherwise = do
        mSpans <- sequence <$> mapM getSpan items
        forM_ mSpans (listEditRanges label)

-- | Emit hierarchical chunk deletions (if enabled) followed by single-item deletions.
listEditRanges :: Text -> [(Int, Int)] -> EditM ()
listEditRanges label spans
    | length spans < 2 = pure ()
    | otherwise = do
        listChunkRanges label spans
        ti <- askTokens
        let go [] = pure ()
            go [_] = pure ()
            go (first : rest@(second : _)) = do
                -- Index 0 deletes [first.start, second.start)
                let !rStart = fst first
                    !rEnd = fst second
                case findComma ti rStart rEnd of
                    Just _ -> addEditBytes rStart rEnd "" EditList label
                    Nothing -> pure ()
                goTail first rest
            goTail _ [] = pure ()
            goTail prev (cur : more) = do
                let !rStart = snd prev
                    !rEnd = snd cur
                case findComma ti rStart rEnd of
                    Just _ -> addEditBytes rStart rEnd "" EditList label
                    Nothing -> pure ()
                goTail cur more
        go spans

{- | Emit coarse-to-fine hierarchical chunk deletions for a comma-separated list,
preserving at least one survivor. Exact port of Rust @list_chunk_ranges@.
-}
listChunkRanges :: Text -> [(Int, Int)] -> EditM ()
listChunkRanges label spans = do
    ctx <- askCtx
    let !total = length spans
    when (ctxChunkEdits ctx && total >= 3 && spansOrdered spans) do
        ti <- askTokens
        let !clabel = chunkLabel label
            !indexedSpans = VU.fromListN total spans
            spanAt = (indexedSpans VU.!)
            loopLevel !level =
                let !groups = (1 :: Int) `shiftL` level
                 in when (groups > 0 && groups <= total) do
                        let !base = total `div` groups
                            !extra = total `mod` groups
                            loopGroup !group !startIdx
                                | group >= groups = pure ()
                                | otherwise =
                                    let !size = base + (if group < extra then 1 else 0)
                                     in when (size >= 2) do
                                            let !endIdx = startIdx + size
                                                (gapStart, gapEnd, rangeStart, rangeEnd) =
                                                    if startIdx == 0
                                                        then
                                                            ( snd (spanAt (endIdx - 1))
                                                            , fst (spanAt endIdx)
                                                            , fst (spanAt 0)
                                                            , fst (spanAt endIdx)
                                                            )
                                                        else
                                                            ( snd (spanAt (startIdx - 1))
                                                            , fst (spanAt startIdx)
                                                            , snd (spanAt (startIdx - 1))
                                                            , snd (spanAt (endIdx - 1))
                                                            )
                                            when (gapStart < gapEnd) $
                                                case findComma ti gapStart gapEnd of
                                                    Just _ -> addEditBytes rangeStart rangeEnd "" EditList clabel
                                                    Nothing -> pure ()
                                            loopGroup (group + 1) endIdx
                        loopGroup 0 0
                        loopLevel (level + 1)
        loopLevel 1

spansOrdered :: [(Int, Int)] -> Bool
spansOrdered [] = True
spansOrdered [_] = True
spansOrdered ((s1, e1) : rest@((s2, e2) : _)) =
    s1 < e1 && e1 <= s2 && s2 < e2 && spansOrdered rest

-- | Chunk label matching Rust's static @chunk_label@ mapping.
chunkLabel :: Text -> Text
chunkLabel = \case
    "remove-cte" -> "remove-cte-chunk"
    "remove-values_row" -> "remove-values_row-chunk"
    "remove-select_expression" -> "remove-select_expression-chunk"
    "remove-from-relation" -> "remove-from-relation-chunk"
    "remove-in_list" -> "remove-in_list-chunk"
    "remove-tuple_item" -> "remove-tuple_item-chunk"
    "remove-function_argument" -> "remove-function_argument-chunk"
    _ -> "remove-list-chunk"

-- ---------------------------------------------------------------------------
-- Clause & data-type edits
-- ---------------------------------------------------------------------------

-- | Emit a complete clause removal spanning from @precedingSpace(kw)@ through @clauseEndAfter@.
clauseEditComplete ::
    ClauseTier ->
    ByteString ->
    (Int, Int) ->
    (Int, Int) ->
    [ByteString] ->
    Text ->
    EditM ()
clauseEditComplete tier kw (bodyStart, bodyEnd) (scopeStart, scopeEnd) followingClauses label = do
    src <- askSource
    ti <- askTokens
    case keywordBefore ti kw scopeStart bodyStart of
        Nothing -> pure ()
        Just kwTok ->
            case clauseEndAfter src ti bodyEnd scopeEnd followingClauses of
                Nothing -> pure ()
                Just endPos ->
                    addClauseEdit
                        tier
                        (precedingSpace src (spanStart (locSpan kwTok)))
                        endPos
                        ""
                        label

-- | Emit a clause removal from @precedingSpace(kw)@ through @bodyEnd@.
clauseEditRange :: ClauseTier -> ByteString -> (Int, Int) -> Int -> Text -> EditM ()
clauseEditRange tier kw (bodyStart, bodyEnd) scopeStart label = do
    src <- askSource
    ti <- askTokens
    case keywordBefore ti kw scopeStart bodyStart of
        Nothing -> pure ()
        Just kwTok ->
            addClauseEdit
                tier
                (precedingSpace src (spanStart (locSpan kwTok)))
                bodyEnd
                ""
                label

-- | Emit data-type simplification edits strictly shrinking @(start, end)@.
addTypeEdits :: (Int, Int) -> EditM ()
addTypeEdits (start, end)
    | start >= end = pure ()
    | otherwise = do
        let !len = end - start
        forM_ ["INT", "TEXT", "REAL", "BOOLEAN"] \repl ->
            when (BS.length repl < len) $
                addEditBytes start end repl EditType "simplify-data_type"
        src <- askSource
        ti <- askTokens
        case firstWordInRange ti start end of
            Just (ByteSpan ws we) ->
                case sliceAbs src (ws, we) of
                    Just repl
                        | BS.length repl < len ->
                            addEditBytes start end repl EditType "simplify-data_type"
                    _ -> pure ()
            Nothing -> pure ()
