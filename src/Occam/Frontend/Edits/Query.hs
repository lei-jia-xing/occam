-- | Query, CTE, SELECT, FROM/JOIN, and query-tail edit generation.
module Occam.Frontend.Edits.Query (
    visitQuery,
    selectItemSpan,
    tableFactorSpan,
    tableRefSpan,
    joinEnd,
    orderByExprsSpan,
    isTopLevelSelectableQuery,
    visitTableRef,
    withClauseEdits,
    visitWithClause,
) where

import Control.Monad (foldM, forM_, unless, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.Maybe (isJust, isNothing, maybeToList)
import Data.Text (Text)
import Data.Vector qualified as V
import Occam.Facts (ClauseTier (..), ExprTier (..), ParserEditCategory (..))
import Occam.Frontend.AST.Expr qualified as AST
import Occam.Frontend.AST.Query qualified as AST
import Occam.Frontend.AST.Statement qualified as AST
import Occam.Frontend.Context (AnalysisContext (..), IdentifierClass (..), canonicalIdentText)
import Occam.Frontend.Edits.Core (
    EditM,
    absTuple,
    addClauseEdit,
    addEditBytes,
    addExprEdit,
    addRecursivePredicateSpans,
    askCtx,
    askSource,
    askTokens,
    clauseEditComplete,
    clauseEditRange,
    listEdits,
    markEditsTopProjection,
    markLastEditRecursiveIf,
    sliceAbs,
 )
import Occam.Frontend.Edits.Expr (exprSpan, visitExpr, visitWindowSpec)
import Occam.Frontend.Edits.Tokens (
    TokenIndex (..),
    findKeywordBetween,
    isWordCI,
    keywordBefore,
    matchingParentheses,
    nextSignificant,
    partitionPoint,
    precedingSpace,
    querySourceScope,
    selectScope,
    topLevelKeyword,
    valuesRowSpans,
 )
import Occam.Frontend.Lexer (Token (..), asciiEqCI)
import Occam.Frontend.Span (ByteSpan (..), Located (..))
import Occam.Parser.Types (SQLDialect (..))

-- | True when a top-level @StatementQuery@ qualifies for @parserEditTopProjection@ marking.
isTopLevelSelectableQuery :: AST.Query -> Bool
isTopLevelSelectableQuery q = case locValue (AST.queryBody q) of
    AST.QueryBodySelect core -> checkCore core
    AST.QueryBodyFrom core -> checkCore core
    _ -> False
    where
        checkCore core =
            isNothing (AST.selectInto core)
                && length (AST.selectItems core) >= 2
                && not (any isWildcardItem (AST.selectItems core))
        isWildcardItem (L _ item) = case locValue (AST.selectItemExpr item) of
            AST.ExprStar -> True
            AST.ExprQualifiedStar _ -> True
            AST.ExprFieldStar _ -> True
            _ -> False

-- | Traverse an 'AST.Query', flagging direct 'AST.SelectCore' projections when @isTopProj@ is 'True'.
visitQuery :: (Located AST.Statement -> EditM ()) -> Bool -> Located AST.Query -> EditM ()
visitQuery visitStmt isTopProj (L qSp q) = do
    ctx <- askCtx
    ti <- askTokens
    let ByteSpan unitStart unitEnd = ctxUnitSpan ctx
        qAbs = absTuple ctx qSp
        mQueryScope = querySourceScope unitStart unitEnd ti qAbs
    forM_ (AST.queryWith q) withClauseEdits
    let orderBy = AST.queryOrderBy q
    unless (null orderBy) do
        forM_ ((,) <$> orderByExprsSpan ctx ti orderBy <*> mQueryScope) \(body, scope) ->
            clauseEditComplete
                SafeClause
                "ORDER"
                body
                scope
                ["LIMIT", "OFFSET", "FETCH", "FOR", "SETTINGS", "FORMAT"]
                "remove-order_by"
        when (length orderBy > 1) $
            listEdits "remove-order_by_expression" orderBy (pure . exprSpan ctx ti . AST.orderExpr . locValue)
    limitAndOffsetEdits mQueryScope (AST.queryLimit q) (AST.queryOffset q)
    forM_ (AST.queryFetch q) \(L _ fetch) ->
        forM_ mQueryScope \scope ->
            forM_ (fetchSourceSpan ctx ti fetch scope) \body ->
                clauseEditComplete SafeClause "FETCH" body scope ["FOR", "SETTINGS", "FORMAT"] "remove-fetch"
    case AST.queryBody q of
        L bodySp (AST.QueryBodyValues rows)
            | length rows > 1 ->
                let (bStart, bEnd) = absTuple ctx bodySp
                 in forM_ (findKeywordBetween ti "VALUES" bStart bEnd) \(_, valTok) ->
                        forM_ (valuesRowSpans ti (spanEnd (locSpan valTok)) bEnd (length rows)) \spans ->
                            listEdits "remove-values_row" (zip rows spans) (pure . Just . snd)
        _ -> pure ()
    setOperationEdits ctx ti (AST.queryBody q)
    -- Visit children in Rust sqlparser Visit order.
    forM_ (AST.queryWith q) (visitWithClause visitStmt)
    visitQueryBody visitStmt isTopProj (AST.queryBody q)
    forM_ orderBy (visitExpr (visitQuery visitStmt False) . AST.orderExpr . locValue)
    forM_ (AST.queryLimit q) \(L _ lc) -> do
        visitExpr (visitQuery visitStmt False) (AST.limitFirst lc)
        traverse_ (visitExpr (visitQuery visitStmt False)) (AST.limitSecond lc)
    traverse_ (visitExpr (visitQuery visitStmt False)) (AST.queryOffset q)
    forM_ (AST.queryFetch q) \(L _ fc) ->
        traverse_ (visitExpr (visitQuery visitStmt False)) (AST.fetchCount fc)
    forM_ (AST.querySettings q) \(L setSp items) -> do
        forM_ items (visitExpr (visitQuery visitStmt False) . AST.settingValue . locValue)
        let (s, e) = absTuple ctx setSp
         in addClauseEdit SafeClause s e "" "remove-settings"

-- | Emit @remove-cte@, @remove-with@, and register recursive CTE predicates for a WITH clause.
withClauseEdits :: Located AST.WithClause -> EditM ()
withClauseEdits (L withSp withClause) = do
    ctx <- askCtx
    ti <- askTokens
    let !srcLen = BS.length (ctxSource ctx)
        ctes = AST.withCtes withClause
    when (AST.withRecursive withClause) $
        addRecursivePredicateSpans (collectRecursivePredicateSpans ctx ti ctes)
    listEdits "remove-cte" ctes (pure . Just . absTuple ctx . locSpan)
    case reverse ctes of
        lastCte : _ ->
            let (_, lastEnd) = absTuple ctx (locSpan lastCte)
                !afterIdx = partitionPoint (\t -> spanEnd (locSpan t) <= lastEnd) (tiTokens ti)
                (withStart, _) = absTuple ctx withSp
             in forM_ (nextSignificant ti afterIdx srcLen) \(_, nextTok) ->
                    addEditBytes withStart (spanStart (locSpan nextTok)) "" EditList "remove-with"
        [] -> pure ()

-- | Traverse CTE bodies inside a WITH clause.
visitWithClause :: (Located AST.Statement -> EditM ()) -> Located AST.WithClause -> EditM ()
visitWithClause visitStmt (L _ withClause) =
    forM_ (AST.withCtes withClause) \(L _ cte) -> case locValue (AST.cteBody cte) of
        AST.CteQuery inner -> visitQuery visitStmt False inner
        AST.CteStatement stmt -> visitStmt stmt

limitAndOffsetEdits ::
    Maybe (Int, Int) ->
    Maybe (Located AST.LimitClause) ->
    Maybe (Located AST.Expr) ->
    EditM ()
limitAndOffsetEdits Nothing _ _ = pure ()
limitAndOffsetEdits (Just (scopeStart, scopeEnd)) mLimit mOffset = do
    ctx <- askCtx
    src <- askSource
    ti <- askTokens
    case mLimit of
        Just (L _ (AST.LimitClause firstExpr (Just secondExpr) True)) ->
            forM_ ((,) <$> exprSpan ctx ti firstExpr <*> exprSpan ctx ti secondExpr) \((offStart, offEnd), (_, limEnd)) ->
                forM_ (keywordBefore ti "LIMIT" scopeStart offStart) \kwTok ->
                    addClauseEdit
                        SafeClause
                        (precedingSpace src (spanStart (locSpan kwTok)))
                        (max offEnd limEnd)
                        ""
                        "remove-limit"
        _ -> do
            let limitRange = mLimit >>= \(L _ (AST.LimitClause firstExpr _ _)) -> exprSpan ctx ti firstExpr
                offsetRange = mOffset >>= exprSpan ctx ti
                limitTok = limitRange >>= \(s, _) -> keywordBefore ti "LIMIT" scopeStart s
                offsetTok = offsetRange >>= \(s, _) -> keywordBefore ti "OFFSET" scopeStart s
            forM_ limitTok \tok -> do
                let end0 = maybe scopeEnd snd limitRange
                    endPos = maybe end0 (precedingSpace src . spanStart . locSpan) offsetTok
                addClauseEdit
                    SafeClause
                    (precedingSpace src (spanStart (locSpan tok)))
                    endPos
                    ""
                    "remove-limit"
            forM_ ((,) <$> offsetTok <*> offsetRange) \(tok, (_, offEnd)) -> do
                let !afterIdx = partitionPoint (\t -> spanStart (locSpan t) < offEnd) (tiTokens ti)
                    endPos = case nextSignificant ti afterIdx (BS.length src) of
                        Just (_, nextTok)
                            | isWordCI "ROW" nextTok || isWordCI "ROWS" nextTok ->
                                spanEnd (locSpan nextTok)
                        _ -> offEnd
                addClauseEdit
                    SafeClause
                    (precedingSpace src (spanStart (locSpan tok)))
                    endPos
                    ""
                    "remove-offset"

fetchSourceSpan ::
    AnalysisContext -> TokenIndex -> AST.FetchClause -> (Int, Int) -> Maybe (Int, Int)
fetchSourceSpan ctx ti fetch scope = do
    fetchTok <- topLevelKeyword ti "FETCH" scope
    case AST.fetchCount fetch of
        Nothing ->
            let !e = spanEnd (locSpan fetchTok)
             in Just (e, e)
        Just qty -> exprSpan ctx ti qty

setExprSourceSpan :: AnalysisContext -> TokenIndex -> Located AST.QueryBody -> Maybe (Int, Int)
setExprSourceSpan ctx ti (L bodySp body) =
    let ByteSpan unitStart unitEnd = ctxUnitSpan ctx
        (start, end) = case body of
            AST.QueryBodyParen (L innerSp _) -> absTuple ctx innerSp
            _ -> absTuple ctx bodySp
     in if start < unitStart || end > unitEnd
            then Nothing
            else
                let toks = tiTokens ti
                    parens = tiParens ti
                    !before = partitionPoint (\t -> spanStart (locSpan t) < start) toks
                    scanEnclosing !i
                        | i < 0 = (start, end)
                        | otherwise =
                            let tok = V.unsafeIndex toks i
                             in case (locValue tok, V.unsafeIndex parens i) of
                                    (TokLParen, Just pair) | spanEnd pair >= end -> (spanStart pair, spanEnd pair)
                                    _ -> scanEnclosing (i - 1)
                 in Just (scanEnclosing (before - 1))

setOperationEdits :: AnalysisContext -> TokenIndex -> Located AST.QueryBody -> EditM ()
setOperationEdits ctx ti (L _ (AST.QueryBodySet _ _ left right)) = do
    forM_ ((,) <$> setExprSourceSpan ctx ti left <*> setExprSourceSpan ctx ti right) \((lStart, lEnd), (rStart, rEnd)) -> do
        when (lEnd < rEnd) $
            addClauseEdit StructuralClause lEnd rEnd "" "simplify-set_operation_left"
        when (lStart < rStart) $
            addClauseEdit StructuralClause lStart rStart "" "simplify-set_operation_right"
    setOperationEdits ctx ti left
    setOperationEdits ctx ti right
setOperationEdits _ _ _ = pure ()

visitQueryBody :: (Located AST.Statement -> EditM ()) -> Bool -> Located AST.QueryBody -> EditM ()
visitQueryBody visitStmt isTopProj (L bodySp body) = case body of
    AST.QueryBodySelect core -> visitSelectCore visitStmt isTopProj bodySp core
    AST.QueryBodyFrom core -> visitSelectCore visitStmt isTopProj bodySp core
    AST.QueryBodySet _ _ left right -> do
        visitQueryBody visitStmt False left
        visitQueryBody visitStmt False right
    AST.QueryBodyParen inner -> visitQuery visitStmt False inner
    AST.QueryBodyValues rows -> mapM_ (mapM_ (visitExpr (visitQuery visitStmt False))) rows
    AST.QueryBodyTable _ -> pure ()

visitSelectCore ::
    (Located AST.Statement -> EditM ()) -> Bool -> ByteSpan -> AST.SelectCore -> EditM ()
visitSelectCore visitStmt isTopProj bodySp core = do
    ctx <- askCtx
    ti <- askTokens
    let ByteSpan _ unitEnd = ctxUnitSpan ctx
        (bodyStart, _) = absTuple ctx bodySp
        visitQ = visitQuery visitStmt False
        fromItems = maybe [] AST.fromItems (AST.selectFrom core)
    if isTopProj
        then
            markEditsTopProjection
                (listEdits "remove-select_expression" (AST.selectItems core) (pure . selectItemSpan ctx ti))
        else listEdits "remove-select_expression" (AST.selectItems core) (pure . selectItemSpan ctx ti)
    distinctEdits bodyStart core
    listEdits "remove-from-relation" fromItems (pure . tableRefSpan ctx ti)
    forM_ fromItems \(L _ tr) -> do
        let goJoins _ [] = pure ()
            goJoins prevEnd (j : js) = case (prevEnd, joinEnd ctx ti j) of
                (Just s, Just e) | s < e -> do
                    addEditBytes s e "" EditList "remove-join"
                    goJoins (Just e) js
                _ -> pure ()
        goJoins (snd <$> tableFactorSpan ctx ti tr) (AST.tableRefJoins tr)
    forM_ (AST.selectWhere core) \sel ->
        markLastEditRecursiveIf
            (\preds -> maybe False (`elem` preds) (exprSpan ctx ti sel))
            ( forM_ (exprSpan ctx ti sel) \rng -> clauseEditRange FragileClause "WHERE" rng bodyStart "remove-where"
            )
    forM_ (AST.selectHaving core) \hav ->
        forM_ (exprSpan ctx ti hav) \rng ->
            clauseEditRange StructuralClause "HAVING" rng bodyStart "remove-having"
    groupByEdits bodyStart unitEnd (AST.selectGroupBy core)
    -- Visit children in Rust sqlparser Visit order.
    case AST.selectQuantifier core of
        AST.SelectDistinctOn exprs -> mapM_ (visitExpr visitQ) exprs
        _ -> pure ()
    forM_ (AST.selectItems core) (visitExpr visitQ . AST.selectItemExpr . locValue)
    mapM_ (visitTableRef visitQ) fromItems
    forM_ (AST.selectPrewhere core) (visitExpr visitQ . locValue)
    traverse_ (visitExpr visitQ) (AST.selectWhere core)
    mapM_ (visitGroupByItem (visitExpr visitQ)) (AST.selectGroupBy core)
    traverse_ (visitExpr visitQ) (AST.selectHaving core)
    forM_ (AST.selectWindows core) (visitWindowSpec visitQ . AST.windowDefSpec . locValue)
    forM_ (AST.selectQualify core) (visitExpr visitQ . locValue)
    forM_ (AST.selectPrewhere core) \(L preSp _) ->
        let (s, e) = absTuple ctx preSp
         in addClauseEdit FragileClause s e "" "remove-prewhere"
    forM_ (AST.selectQualify core) \(L qSp _) ->
        let (s, e) = absTuple ctx qSp
         in addClauseEdit FragileClause s e "" "remove-qualify"

distinctEdits :: Int -> AST.SelectCore -> EditM ()
distinctEdits bodyStart core = do
    ctx <- askCtx
    ti <- askTokens
    let ByteSpan _ unitEnd = ctxUnitSpan ctx
        limitPos = case AST.selectItems core of
            firstItem : _ -> maybe unitEnd fst (selectItemSpan ctx ti firstItem)
            [] -> unitEnd
    forM_ (findKeywordBetween ti "SELECT" bodyStart unitEnd) \(_, selTok) ->
        let !selEnd = spanEnd (locSpan selTok)
         in case AST.selectQuantifier core of
                AST.SelectDefault -> pure ()
                AST.SelectDistinct ->
                    forM_ (findKeywordBetween ti "DISTINCT" selEnd limitPos) \(dIdx, dTok) ->
                        forM_ (nextSignificant ti (dIdx + 1) unitEnd) \(_, nextTok) ->
                            addClauseEdit
                                SafeClause
                                (spanStart (locSpan dTok))
                                (spanStart (locSpan nextTok))
                                ""
                                "remove-distinct"
                AST.SelectAll ->
                    forM_ (findKeywordBetween ti "ALL" selEnd limitPos) \(aIdx, aTok) ->
                        forM_ (nextSignificant ti (aIdx + 1) unitEnd) \(_, nextTok) ->
                            addClauseEdit
                                SafeClause
                                (spanStart (locSpan aTok))
                                (spanStart (locSpan nextTok))
                                ""
                                "remove-distinct"
                AST.SelectDistinctOn exprs -> do
                    forM_ (findKeywordBetween ti "DISTINCT" selEnd limitPos) \(dIdx, dTok) ->
                        forM_ (nextSignificant ti (dIdx + 1) limitPos) \(onIdx, onTok) ->
                            when (isWordCI "ON" onTok) $
                                forM_ (nextSignificant ti (onIdx + 1) limitPos) \(pIdx, pTok) ->
                                    case (locValue pTok, tiParens ti V.!? pIdx) of
                                        (TokLParen, Just (Just pair)) ->
                                            let !afterIdx = partitionPoint (\t -> spanStart (locSpan t) < spanEnd pair) (tiTokens ti)
                                             in forM_ (nextSignificant ti afterIdx unitEnd) \(_, nextTok) ->
                                                    addClauseEdit
                                                        SafeClause
                                                        (spanStart (locSpan dTok))
                                                        (spanStart (locSpan nextTok))
                                                        ""
                                                        "remove-distinct"
                                        _ -> pure ()
                    when (length exprs > 1) $
                        listEdits "remove-distinct_on_expression" exprs (pure . exprSpan ctx ti)

groupByEdits :: Int -> Int -> [Located AST.GroupByItem] -> EditM ()
groupByEdits _ _ [] = pure ()
groupByEdits bodyStart unitEnd items = do
    ctx <- askCtx
    ti <- askTokens
    case items of
        [L allSp (AST.GroupByExpr (L _ (AST.ExprColumn (L _ (AST.Name [L _ (AST.NamePart raw False)])))))]
            | asciiEqCI "ALL" raw ->
                forM_ (selectScope unitEnd ti bodyStart) \scope ->
                    clauseEditComplete
                        StructuralClause
                        "GROUP"
                        (absTuple ctx allSp)
                        scope
                        followingGroupByClauses
                        "remove-group_by"
        _ -> do
            when (length items > 1) $
                listEdits "remove-group_by_expression" items (pure . groupByItemSpan ctx ti)
            forM_ ((,) <$> groupBySpan ctx ti items <*> selectScope unitEnd ti bodyStart) \(body, scope) ->
                clauseEditComplete StructuralClause "GROUP" body scope followingGroupByClauses "remove-group_by"
    where
        followingGroupByClauses :: [ByteString]
        followingGroupByClauses =
            [ "HAVING"
            , "WINDOW"
            , "QUALIFY"
            , "SORT"
            , "ORDER"
            , "LIMIT"
            , "OFFSET"
            , "FETCH"
            , "FOR"
            , "CONNECT"
            , "CLUSTER"
            , "DISTRIBUTE"
            , "SETTINGS"
            , "FORMAT"
            ]

groupBySpan :: AnalysisContext -> TokenIndex -> [Located AST.GroupByItem] -> Maybe (Int, Int)
groupBySpan ctx ti items = case (items, reverse items) of
    (firstItem : _, lastItem : _) -> do
        (s, _) <- groupByItemSpan ctx ti firstItem
        (_, e) <- groupByItemSpan ctx ti lastItem
        pure (s, e)
    _ -> Nothing

groupByItemSpan :: AnalysisContext -> TokenIndex -> Located AST.GroupByItem -> Maybe (Int, Int)
groupByItemSpan ctx ti (L sp item) = case item of
    AST.GroupByExpr e -> exprSpan ctx ti e
    AST.GroupByParens inner -> case (inner, reverse inner) of
        (firstItem : _, lastItem : _) -> do
            _ <- groupByItemSpan ctx ti firstItem
            _ <- groupByItemSpan ctx ti lastItem
            Just (absTuple ctx sp)
        _ -> Nothing
    AST.GroupByGroupingSets _ -> Nothing
    AST.GroupByRollup _ -> Nothing
    AST.GroupByCube _ -> Nothing

visitGroupByItem :: (Located AST.Expr -> EditM ()) -> Located AST.GroupByItem -> EditM ()
visitGroupByItem visitE (L _ item) = case item of
    AST.GroupByExpr e -> visitE e
    AST.GroupByParens inner -> mapM_ (visitGroupByItem visitE) inner
    AST.GroupByGroupingSets inner -> mapM_ (visitGroupingSetItem visitE) inner
    AST.GroupByRollup inner -> mapM_ (visitGroupByItem visitE) inner
    AST.GroupByCube inner -> mapM_ (visitGroupByItem visitE) inner

visitGroupingSetItem :: (Located AST.Expr -> EditM ()) -> Located AST.GroupByItem -> EditM ()
visitGroupingSetItem visitE (L sp item) = case item of
    AST.GroupByRollup inner -> emitGroupingFunc sp inner
    AST.GroupByCube inner -> emitGroupingFunc sp inner
    AST.GroupByGroupingSets inner -> mapM_ (visitGroupingSetItem visitE) inner
    AST.GroupByParens inner -> mapM_ (visitGroupingSetItem visitE) inner
    AST.GroupByExpr e -> visitE e
    where
        emitGroupingFunc callSp items = do
            ctx <- askCtx
            ti <- askTokens
            let src = ctxSource ctx
                (start, end) = absTuple ctx callSp
            listEdits "remove-function_argument" items (pure . groupByItemSpan ctx ti)
            when (start < end) do
                forM_ items \arg ->
                    forM_ (groupByItemSpan ctx ti arg) \aSpan ->
                        forM_ (sliceAbs src aSpan) \repl ->
                            addExprEdit UnwrapExpr start end repl "simplify-function_to_argument"
                forM_ ["0", "1"] \repl ->
                    addExprEdit AggressiveExpr start end repl "simplify-function_literal"
            forM_ items \sub -> case sub of
                L pSp (AST.GroupByParens [L _ (AST.GroupByExpr e)]) -> do
                    let (ps, pe) = absTuple ctx pSp
                    forM_ (exprSpan ctx ti e >>= sliceAbs src) \repl ->
                        addExprEdit UnwrapExpr ps pe repl "simplify-parenthesized_expression"
                    visitE e
                _ -> visitGroupingSetItem visitE sub

-- | Source span for a 'AST.SelectItem' (including optional alias).
selectItemSpan :: AnalysisContext -> TokenIndex -> Located AST.SelectItem -> Maybe (Int, Int)
selectItemSpan ctx ti (L itemSp item) = case AST.selectItemAlias item of
    Nothing -> case AST.selectItemExpr item of
        L _ AST.ExprStar -> Just (absTuple ctx itemSp)
        L _ (AST.ExprQualifiedStar _) -> Just (absTuple ctx itemSp)
        L _ (AST.ExprFieldStar base) -> exprSpan ctx ti base >> Just (absTuple ctx itemSp)
        e -> exprSpan ctx ti e
    Just alias -> do
        (s, _) <- exprSpan ctx ti (AST.selectItemExpr item)
        let (_, aEnd) = absTuple ctx (locSpan alias)
        pure (s, aEnd)

-- | Source span for a single table factor (without joins), matching Rust @table_factor_source_span@.
tableFactorSpan :: AnalysisContext -> TokenIndex -> AST.TableRef -> Maybe (Int, Int)
tableFactorSpan ctx ti tr
    | AST.tableRefOnly tr
        || AST.tableRefIncludeDescendants tr
        || AST.tableRefFinal tr
        || isJust (AST.tableRefSample tr) =
        Nothing
    | otherwise = do
        (s, e0) <- sourceSpan (AST.tableRefSource tr)
        let e = maybe e0 (snd . absTuple ctx . locSpan) (AST.tableRefAlias tr)
        pure (s, e)
    where
        sourceSpan (L srcSp src) = case src of
            AST.TableSourceName _ -> Just (absTuple ctx srcSp)
            AST.TableSourceSubquery _ -> Just (absTuple ctx srcSp)
            AST.TableSourceParens (AST.FromClause items) -> do
                traverse_ (tableRefSpan ctx ti) items
                Just (absTuple ctx srcSp)
            AST.TableSourceLateral (L _ (AST.TableSourceSubquery _)) -> Just (absTuple ctx srcSp)
            _ -> Nothing

-- | Source span for a 'AST.TableRef' including all its chained joins.
tableRefSpan :: AnalysisContext -> TokenIndex -> Located AST.TableRef -> Maybe (Int, Int)
tableRefSpan ctx ti (L _ tr) = do
    (s, end0) <- tableFactorSpan ctx ti tr
    endFinal <- foldM (\_ j -> joinEnd ctx ti j) end0 (AST.tableRefJoins tr)
    pure (s, endFinal)

-- | End byte offset of a 'AST.JoinClause' (relation + ON/USING constraint).
joinEnd :: AnalysisContext -> TokenIndex -> Located AST.JoinClause -> Maybe Int
joinEnd ctx ti (L _ jc) = case AST.joinTarget jc of
    AST.JoinTargetArray arrItems -> case reverse arrItems of
        lastItem@(L _ item) : _ -> do
            traverse_ (exprSpan ctx ti . AST.arrayJoinExpr . locValue) arrItems
            let itemEnd = case AST.arrayJoinAlias item of
                    Just aliasPart -> snd (absTuple ctx (locSpan aliasPart))
                    Nothing -> snd (absTuple ctx (locSpan lastItem))
            Just itemEnd
        [] -> Nothing
    AST.JoinTargetTable tRef -> do
        (_, relEnd) <- tableRefSpan ctx ti tRef
        cEnd <- case AST.joinConstraint jc of
            Nothing -> Just relEnd
            Just (AST.JoinOn e) -> snd <$> exprSpan ctx ti e
            Just (AST.JoinUsing cols) -> case (cols, reverse cols) of
                (c0 : _, cN : _) ->
                    let (s0, _) = absTuple ctx (locSpan c0)
                        (_, eN) = absTuple ctx (locSpan cN)
                     in spanEnd <$> matchingParentheses ti s0 eN relEnd
                _ -> Nothing
        pure (max relEnd cEnd)

-- | Span from first ORDER BY expression start to last ORDER BY expression end.
orderByExprsSpan :: AnalysisContext -> TokenIndex -> [Located AST.OrderItem] -> Maybe (Int, Int)
orderByExprsSpan ctx ti items = case (items, reverse items) of
    (firstItem : _, lastItem : _) -> do
        (s, _) <- exprSpan ctx ti (AST.orderExpr (locValue firstItem))
        (_, e) <- exprSpan ctx ti (AST.orderExpr (locValue lastItem))
        pure (s, e)
    _ -> Nothing

visitTableRef :: (Located AST.Query -> EditM ()) -> Located AST.TableRef -> EditM ()
visitTableRef visitQ (L _ tr) = do
    visitTableSource (AST.tableRefSource tr)
    forM_ (AST.tableRefSample tr) \(L _ sample) -> do
        visitExpr visitQ (AST.tableSampleAmount sample)
        traverse_ (visitExpr visitQ) (AST.tableSampleRepeatable sample)
        traverse_ (visitExpr visitQ) (AST.tableSampleOffset sample)
    forM_ (AST.tableRefJoins tr) \(L _ jc) -> do
        case AST.joinTarget jc of
            AST.JoinTargetTable targetRef -> visitTableRef visitQ targetRef
            AST.JoinTargetArray arrItems ->
                forM_ arrItems (visitExpr visitQ . AST.arrayJoinExpr . locValue)
        case AST.joinConstraint jc of
            Just (AST.JoinOn e) -> visitExpr visitQ e
            _ -> pure ()
    where
        visitTableSource (L _ src) = case src of
            AST.TableSourceName _ -> pure ()
            AST.TableSourceSubquery q -> visitQ q
            AST.TableSourceValues rows -> mapM_ (mapM_ (visitExpr visitQ)) rows
            AST.TableSourceFunction (L _ fc) -> do
                ctx <- askCtx
                forM_ (AST.functionArgs fc) \(L _ arg) -> case arg of
                    AST.FunctionArgExpr
                        (L _ (AST.ExprBinary AST.OpEq (L _ (AST.ExprColumn (L _ (AST.Name [_])))) rhs))
                            | ctxDialect ctx == DuckDB ->
                                visitExpr visitQ rhs
                    AST.FunctionArgExpr e -> visitExpr visitQ e
                    AST.FunctionArgNamed _ e -> visitExpr visitQ e
                    AST.FunctionArgStar -> pure ()
                forM_ (AST.functionOrderBy fc) (visitExpr visitQ . AST.orderExpr . locValue)
                forM_ (AST.functionWithinGroup fc) (visitExpr visitQ . AST.orderExpr . locValue)
                traverse_ (visitExpr visitQ) (AST.functionFilter fc)
                forM_ (AST.functionOver fc) \case
                    AST.WindowRefSpec spec -> visitWindowSpec visitQ spec
                    AST.WindowRefName _ -> pure ()
            AST.TableSourceParens (AST.FromClause items) -> mapM_ (visitTableRef visitQ) items
            AST.TableSourceLateral inner -> visitTableSource inner

-- ---------------------------------------------------------------------------
-- Recursive CTE predicate collection
-- ---------------------------------------------------------------------------

collectRecursivePredicateSpans :: AnalysisContext -> TokenIndex -> [Located AST.Cte] -> [(Int, Int)]
collectRecursivePredicateSpans ctx ti ctes =
    concat
        [ case locValue (AST.cteBody cte) of
            AST.CteQuery q -> collectQueryPredicates q (drop idx names)
            AST.CteStatement _ -> []
        | (idx, L _ cte) <- zip [0 ..] ctes
        ]
    where
        names :: [Text]
        names = [canonicalIdentText ctx IdentRelation (locValue (AST.cteName cte)) | L _ cte <- ctes]

        collectQueryPredicates :: Located AST.Query -> [Text] -> [(Int, Int)]
        collectQueryPredicates (L _ q) targets
            | null targets = []
            | otherwise =
                let active = case AST.queryWith q of
                        Nothing -> targets
                        Just (L _ w) ->
                            let shadowed =
                                    [ canonicalIdentText ctx IdentRelation (locValue (AST.cteName cte))
                                    | L _ cte <- AST.withCtes w
                                    ]
                             in filter (`notElem` shadowed) targets
                 in if null active
                        then []
                        else collectBodyPredicates (AST.queryBody q) active

        collectBodyPredicates :: Located AST.QueryBody -> [Text] -> [(Int, Int)]
        collectBodyPredicates (L _ body) targets = case body of
            AST.QueryBodySelect core -> collectSelectPredicates core targets
            AST.QueryBodyFrom core -> collectSelectPredicates core targets
            AST.QueryBodyParen inner -> collectQueryPredicates inner targets
            AST.QueryBodySet _ _ left right ->
                collectBodyPredicates left targets ++ collectBodyPredicates right targets
            _ -> []

        collectSelectPredicates :: AST.SelectCore -> [Text] -> [(Int, Int)]
        collectSelectPredicates core targets =
            let fromItems = maybe [] AST.fromItems (AST.selectFrom core)
             in if not (any (`tableRefRefsAny` targets) fromItems)
                    then []
                    else
                        concatMap (`collectTableRefPredicates` targets) fromItems
                            ++ maybeToList (AST.selectWhere core >>= exprSpan ctx ti)

        collectTableRefPredicates :: Located AST.TableRef -> [Text] -> [(Int, Int)]
        collectTableRefPredicates (L _ tr) targets =
            collectTableSourcePredicates (AST.tableRefSource tr) targets
                ++ concatMap (`collectJoinPredicates` targets) (AST.tableRefJoins tr)

        collectJoinPredicates :: Located AST.JoinClause -> [Text] -> [(Int, Int)]
        collectJoinPredicates (L _ jc) targets =
            let constraintPreds = case AST.joinConstraint jc of
                    Just (AST.JoinOn e) -> maybeToList (exprSpan ctx ti e)
                    _ -> []
                targetPreds = case AST.joinTarget jc of
                    AST.JoinTargetTable tRef -> collectTableRefPredicates tRef targets
                    AST.JoinTargetArray _ -> []
             in constraintPreds ++ targetPreds

        collectTableSourcePredicates :: Located AST.TableSource -> [Text] -> [(Int, Int)]
        collectTableSourcePredicates (L _ src) targets = case src of
            AST.TableSourceParens (AST.FromClause items) ->
                concatMap (`collectTableRefPredicates` targets) items
            AST.TableSourceSubquery q -> collectQueryPredicates q targets
            AST.TableSourceLateral inner -> collectTableSourcePredicates inner targets
            _ -> []

        tableRefRefsAny :: Located AST.TableRef -> [Text] -> Bool
        tableRefRefsAny (L _ tr) targets =
            tableSourceRefsAny (AST.tableRefSource tr) targets
                || any
                    ( \case
                        L _ (AST.JoinClause {AST.joinTarget = AST.JoinTargetTable tRef}) ->
                            tableRefRefsAny tRef targets
                        _ -> False
                    )
                    (AST.tableRefJoins tr)

        tableSourceRefsAny :: Located AST.TableSource -> [Text] -> Bool
        tableSourceRefsAny (L _ src) targets = case src of
            AST.TableSourceName (L _ (AST.Name [L _ part])) ->
                canonicalIdentText ctx IdentRelation part `elem` targets
            AST.TableSourceParens (AST.FromClause items) ->
                any (`tableRefRefsAny` targets) items
            AST.TableSourceSubquery q -> queryRefsAny q targets
            AST.TableSourceLateral inner -> tableSourceRefsAny inner targets
            _ -> False

        queryRefsAny :: Located AST.Query -> [Text] -> Bool
        queryRefsAny (L _ q) targets
            | null targets = False
            | otherwise =
                let active = case AST.queryWith q of
                        Nothing -> targets
                        Just (L _ w) ->
                            let shadowed =
                                    [ canonicalIdentText ctx IdentRelation (locValue (AST.cteName cte))
                                    | L _ cte <- AST.withCtes w
                                    ]
                             in filter (`notElem` shadowed) targets
                 in not (null active) && bodyRefsAny (AST.queryBody q) active

        bodyRefsAny :: Located AST.QueryBody -> [Text] -> Bool
        bodyRefsAny (L _ body) targets = case body of
            AST.QueryBodySelect core ->
                any (`tableRefRefsAny` targets) (maybe [] AST.fromItems (AST.selectFrom core))
            AST.QueryBodyFrom core ->
                any (`tableRefRefsAny` targets) (maybe [] AST.fromItems (AST.selectFrom core))
            AST.QueryBodyParen inner -> queryRefsAny inner targets
            AST.QueryBodySet _ _ left right ->
                bodyRefsAny left targets || bodyRefsAny right targets
            _ -> False
