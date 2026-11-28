-- | Top-level and nested statement edit generation.
module Occam.Frontend.Edits.Stmt (
    visitStatement,
) where

import Control.Monad (forM_, unless, when)
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.Vector qualified as V
import Occam.Facts (ClauseTier (..), ParserEditCategory (..))
import Occam.Frontend.AST.DML qualified as AST
import Occam.Frontend.AST.Expr qualified as AST
import Occam.Frontend.AST.Query qualified as AST
import Occam.Frontend.AST.Statement qualified as AST
import Occam.Frontend.Context (AnalysisContext (..))
import Occam.Frontend.Edits.Core (
    EditM,
    absTuple,
    addEditBytes,
    askCtx,
    askTokens,
    clauseEditComplete,
    clauseEditRange,
    listEdits,
 )
import Occam.Frontend.Edits.DDL (visitAlterStmt, visitCreateStmt)
import Occam.Frontend.Edits.Expr (addScalarEdit, exprSpan, visitExpr)
import Occam.Frontend.Edits.Query (
    isTopLevelSelectableQuery,
    joinEnd,
    orderByExprsSpan,
    selectItemSpan,
    tableFactorSpan,
    visitQuery,
    visitTableRef,
    visitWithClause,
    withClauseEdits,
 )
import Occam.Frontend.Edits.Tokens (
    TokenIndex,
    findKeywordBetween,
    isSignificantToken,
    valuesRowSpans,
 )
import Occam.Frontend.Lexer (LiteralKind (..), Token (..), asciiEqCI)
import Occam.Frontend.Span (ByteSpan (..), Located (..))

-- | Traverse a top-level statement, emitting all non-column edits.
visitStatement :: AST.Statement -> EditM ()
visitStatement = \case
    AST.StatementQuery q@(L _ query) ->
        visitQuery visitNestedStmt (isTopLevelSelectableQuery query) q
    stmt -> visitStatementAt False stmt

visitNestedStmt :: Located AST.Statement -> EditM ()
visitNestedStmt = visitStatementAt True . locValue

visitStatementAt :: Bool -> AST.Statement -> EditM ()
visitStatementAt isNested stmt = do
    ctx <- askCtx
    ti <- askTokens
    let visitQ = visitQuery visitNestedStmt False
        visitE = visitExpr visitQ
    case stmt of
        AST.StatementQuery q ->
            visitQuery visitNestedStmt (not isNested && isTopLevelSelectableQuery (locValue q)) q
        AST.StatementInsert (L insSp ins) -> do
            forM_ (AST.insertWith ins) withClauseEdits
            forM_ (AST.insertWith ins) (visitWithClause visitNestedStmt)
            case AST.insertSource ins of
                AST.InsertValues rows -> do
                    when (length rows > 1) do
                        let (_, insEnd) = absTuple ctx insSp
                            afterCols = case reverse (AST.insertColumns ins) of
                                lastCol : _ -> snd (absTuple ctx (locSpan lastCol))
                                [] ->
                                    maybe
                                        (snd (absTuple ctx (locSpan (AST.insertTable ins))))
                                        (snd . absTuple ctx . locSpan)
                                        (AST.insertAlias ins)
                        forM_ (findKeywordBetween ti "VALUES" afterCols insEnd) \(_, valTok) ->
                            forM_ (valuesRowSpans ti (spanEnd (locSpan valTok)) insEnd (length rows)) \spans ->
                                listEdits "remove-values_row" (zip rows spans) (pure . Just . snd)
                    mapM_ (mapM_ visitE) rows
                AST.InsertQuery q -> visitQ q
                AST.InsertDefaultValues -> pure ()
            forM_ (AST.insertOnConflict ins) \(L _ oc) -> do
                forM_ (AST.onConflictKeys oc) \(L _ kp) -> do
                    visitE (AST.indexKeyExpr kp)
                    traverse_ visitE (AST.indexKeyPrefixLength kp)
                traverse_ visitE (AST.onConflictWhere oc)
                case AST.onConflictAction oc of
                    AST.OnConflictDoNothing -> pure ()
                    AST.OnConflictDoUpdate assigns mWhere -> do
                        forM_ assigns (visitE . AST.assignValue . locValue)
                        traverse_ visitE mWhere
            forM_ (AST.insertOnDuplicate ins) $
                mapM_ (visitE . AST.assignValue . locValue)
            forM_ (AST.insertReturning ins) (visitE . AST.selectItemExpr . locValue)
        AST.StatementUpdate (L updSp upd) -> do
            let updAbs@(updStart, _) = absTuple ctx updSp
                targets = AST.fromItems (AST.updateTargets upd)
                orderBy = AST.updateOrderBy upd
                returning = AST.updateReturning upd
            forM_ (AST.updateWith upd) withClauseEdits
            listEdits "remove-update_assignment" (AST.updateAssignments upd) (pure . assignmentSpan ctx ti)
            forM_ targets \(L _ tr) ->
                emitJoinRemovals ti ctx (snd <$> tableFactorSpan ctx ti tr) (AST.tableRefJoins tr)
            forM_ (AST.updateWhere upd) \whereExpr ->
                forM_ (exprSpan ctx ti whereExpr) \rng ->
                    clauseEditRange FragileClause "WHERE" rng updStart "remove-where"
            unless (null orderBy) do
                forM_ (orderByExprsSpan ctx ti orderBy) \body ->
                    clauseEditComplete SafeClause "ORDER" body updAbs ["LIMIT", "RETURNING"] "remove-order_by"
                when (length orderBy > 1) $
                    listEdits "remove-order_by_expression" orderBy (pure . exprSpan ctx ti . AST.orderExpr . locValue)
            emitDmlLimitEdit ctx ti updStart (AST.updateLimit upd)
            unless (null returning) do
                listEdits "remove-returning_expression" returning (pure . selectItemSpan ctx ti)
                case (returning, reverse returning) of
                    (firstItem : _, lastItem : _) ->
                        forM_ ((,) <$> selectItemSpan ctx ti firstItem <*> selectItemSpan ctx ti lastItem) \((s, _), (_, e)) ->
                            clauseEditComplete StructuralClause "RETURNING" (s, e) updAbs [] "remove-returning"
                    _ -> pure ()
            -- Visit children in Rust sqlparser Visit order.
            forM_ (AST.updateWith upd) (visitWithClause visitNestedStmt)
            mapM_ (visitTableRef visitQ) targets
            forM_ (AST.updateAssignments upd) (visitE . AST.assignValue . locValue)
            forM_ (AST.updateFrom upd) (mapM_ (visitTableRef visitQ) . AST.fromItems)
            traverse_ visitE (AST.updateWhere upd)
            forM_ returning (visitE . AST.selectItemExpr . locValue)
            forM_ orderBy (visitE . AST.orderExpr . locValue)
            forM_ (AST.updateLimit upd) \(L _ lc) -> do
                visitE (AST.limitFirst lc)
                traverse_ visitE (AST.limitSecond lc)
        AST.StatementDelete (L delSp del) -> do
            let delAbs@(delStart, _) = absTuple ctx delSp
                orderBy = AST.deleteOrderBy del
                returning = AST.deleteReturning del
            forM_ (AST.deleteWith del) withClauseEdits
            forM_ (AST.deleteWhere del) \whereExpr ->
                forM_ (exprSpan ctx ti whereExpr) \rng ->
                    clauseEditRange FragileClause "WHERE" rng delStart "remove-where"
            unless (null orderBy) do
                forM_ (orderByExprsSpan ctx ti orderBy) \body ->
                    clauseEditComplete SafeClause "ORDER" body delAbs ["LIMIT", "RETURNING"] "remove-order_by"
                when (length orderBy > 1) $
                    listEdits "remove-order_by_expression" orderBy (pure . exprSpan ctx ti . AST.orderExpr . locValue)
            emitDmlLimitEdit ctx ti delStart (AST.deleteLimit del)
            unless (null returning) do
                listEdits "remove-returning_expression" returning (pure . selectItemSpan ctx ti)
                case (returning, reverse returning) of
                    (firstItem : _, lastItem : _) ->
                        forM_ ((,) <$> selectItemSpan ctx ti firstItem <*> selectItemSpan ctx ti lastItem) \((s, _), (_, e)) ->
                            clauseEditComplete StructuralClause "RETURNING" (s, e) delAbs [] "remove-returning"
                    _ -> pure ()
            -- Visit children in Rust sqlparser Visit order.
            forM_ (AST.deleteWith del) (visitWithClause visitNestedStmt)
            forM_ (AST.deleteFromTables del) (mapM_ (visitTableRef visitQ) . AST.fromItems)
            forM_ (AST.deleteUsing del) (mapM_ (visitTableRef visitQ) . AST.fromItems)
            traverse_ visitE (AST.deleteWhere del)
            forM_ returning (visitE . AST.selectItemExpr . locValue)
            forM_ orderBy (visitE . AST.orderExpr . locValue)
            forM_ (AST.deleteLimit del) \(L _ lc) -> do
                visitE (AST.limitFirst lc)
                traverse_ visitE (AST.limitSecond lc)
        AST.StatementMerge (L _ merge) -> do
            forM_ (AST.mergeWith merge) withClauseEdits
            forM_ (AST.mergeWith merge) (visitWithClause visitNestedStmt)
            visitTableRef visitQ (AST.mergeSource merge)
            visitE (AST.mergeOn merge)
            forM_ (AST.mergeClauses merge) \(L _ mc) -> do
                traverse_ visitE (AST.mergeClauseCondition mc)
                case AST.mergeClauseAction mc of
                    AST.MergeUpdate assigns -> forM_ assigns (visitE . AST.assignValue . locValue)
                    AST.MergeInsert _ vals -> mapM_ visitE vals
                    AST.MergeInsertDefaultValues -> pure ()
                    AST.MergeDelete -> pure ()
                    AST.MergeDoNothing -> pure ()
            forM_ (AST.mergeReturning merge) (visitE . AST.selectItemExpr . locValue)
        AST.StatementCreate (L _ cr) -> visitCreateStmt visitNestedStmt cr
        AST.StatementAlter (L _ alt) -> visitAlterStmt visitNestedStmt alt
        AST.StatementExplain (L _ ex) -> visitNestedStmt (AST.explainStatement ex)
        AST.StatementCopy (L _ cp) -> case AST.copySource cp of
            AST.CopySourceQuery q -> visitQ q
            AST.CopySourceTable _ _ -> pure ()
        AST.StatementPrepare (L _ pr) -> case AST.prepareSource pr of
            AST.PrepareStatement inner -> visitNestedStmt inner
            AST.PrepareExpression e -> visitE e
        AST.StatementExecute (L _ ex) -> mapM_ visitE (AST.executeArgs ex)
        AST.StatementPivot (L _ piv) -> do
            visitTableRef visitQ (AST.pivotSource piv)
            mapM_ visitE (AST.pivotOn piv)
            mapM_ visitE (AST.pivotIn piv)
            forM_ (AST.pivotUsing piv) (visitE . AST.selectItemExpr . locValue)
            mapM_ visitE (AST.pivotGroupBy piv)
        AST.StatementSet (L _ s) -> do
            forM_ (AST.setItems s) \(L _ item) -> case AST.setItemTarget item of
                AST.SetTargetSpecial (L _ kw) | asciiEqCI "NAMES" kw -> pure ()
                _ -> mapM_ visitE (AST.setItemValue item)
            forM_ (AST.setTransactionModes s) \(L (ByteSpan mStart mEnd) modeRaw) ->
                when (BS.isPrefixOf "SNAPSHOT " modeRaw) $
                    forM_
                        [ (litSp, kind, raw)
                        | t@(L litSp (TokLiteral kind raw)) <- V.toList (ctxUnitTokens ctx)
                        , isSignificantToken t
                        , spanStart litSp >= mStart
                        , spanEnd litSp <= mEnd
                        ]
                        \(litSp, kind, raw) ->
                            addScalarEdit litSp (kind == LitString || BS.isPrefixOf "'" raw)
        AST.StatementPragma (L _ p) -> forM_ (AST.pragmaValue p) \case
            AST.PragmaValueExpr e -> visitE e
            AST.PragmaBare (L bareSp _) -> addScalarEdit bareSp False
        AST.StatementNotify (L _ n) -> traverse_ visitE (AST.notifyPayload n)
        AST.StatementVacuum (L _ v) -> traverse_ visitE (AST.vacuumInto v)
        AST.StatementAttach (L _ a) -> case a of
            AST.AttachDatabase e _ _ -> visitE e
            AST.AttachObject {} -> pure ()
        AST.StatementOptimize (L _ o) -> traverse_ visitE (AST.optimizePartition o)
        AST.StatementShow (L _ s) -> do
            traverse_ visitE (AST.showLike s)
            traverse_ visitE (AST.showWhere s)
        AST.StatementDelimiter _ -> pure ()
        AST.StatementDrop _ -> pure ()
        AST.StatementReset _ -> pure ()
        AST.StatementLockTables _ -> pure ()
        AST.StatementUnlockTables -> pure ()
        AST.StatementGrant _ -> pure ()
        AST.StatementUse _ -> pure ()
        AST.StatementTransaction _ -> pure ()
        AST.StatementAnalyze _ -> pure ()
        AST.StatementDetach _ -> pure ()
        AST.StatementDeallocate _ -> pure ()
        AST.StatementTruncate _ -> pure ()
        AST.StatementRenameTable _ -> pure ()
        AST.StatementRefreshMaterializedView _ -> pure ()

assignmentSpan :: AnalysisContext -> TokenIndex -> Located AST.Assignment -> Maybe (Int, Int)
assignmentSpan ctx ti (L _ a) = do
    let (s, _) = absTuple ctx (locSpan (AST.assignTarget a))
    (_, e) <- exprSpan ctx ti (AST.assignValue a)
    if s < e then Just (s, e) else Nothing

emitJoinRemovals ::
    TokenIndex -> AnalysisContext -> Maybe Int -> [Located AST.JoinClause] -> EditM ()
emitJoinRemovals _ _ _ [] = pure ()
emitJoinRemovals ti ctx prevEnd (j : js) = case (prevEnd, joinEnd ctx ti j) of
    (Just s, Just e) | s < e -> do
        addEditBytes s e "" EditList "remove-join"
        emitJoinRemovals ti ctx (Just e) js
    _ -> pure ()

emitDmlLimitEdit ::
    AnalysisContext -> TokenIndex -> Int -> Maybe (Located AST.LimitClause) -> EditM ()
emitDmlLimitEdit _ _ _ Nothing = pure ()
emitDmlLimitEdit ctx ti scopeStart (Just (L _ (AST.LimitClause firstExpr mSecond _))) = case mSecond of
    Nothing ->
        forM_ (exprSpan ctx ti firstExpr) \rng ->
            clauseEditRange SafeClause "LIMIT" rng scopeStart "remove-limit"
    Just secondExpr ->
        forM_ ((,) <$> exprSpan ctx ti firstExpr <*> exprSpan ctx ti secondExpr) \((s1, _), (_, e2)) ->
            clauseEditRange SafeClause "LIMIT" (s1, e2) scopeStart "remove-limit"
