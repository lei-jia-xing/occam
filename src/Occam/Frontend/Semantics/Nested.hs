-- | Traversal of nested statements and ordered merging of their facts.
module Occam.Frontend.Semantics.Nested (
    analyzeNested,
    mergeNested,
    isDmlStatement,
) where

import Data.Foldable qualified as Foldable
import Occam.Core.Semantics qualified as Core
import Occam.Frontend.AST.DDL qualified as AST
import Occam.Frontend.AST.DML qualified as AST
import Occam.Frontend.AST.Expr qualified as AST
import Occam.Frontend.AST.Query qualified as AST
import Occam.Frontend.AST.Statement qualified as AST
import Occam.Frontend.Context (AnalysisContext (..))
import Occam.Frontend.Span (Located (..))

-- | True for DML statements (@INSERT@, @UPDATE@, @DELETE@, @MERGE@).
isDmlStatement :: AST.Statement -> Bool
isDmlStatement = \case
    AST.StatementInsert _ -> True
    AST.StatementUpdate _ -> True
    AST.StatementDelete _ -> True
    AST.StatementMerge _ -> True
    _ -> False

-- | Analyze outermost nested DML statements inside an AST subtree.
analyzeNested ::
    (AnalysisContext -> AST.Statement -> Core.SemanticFacts) ->
    AnalysisContext ->
    Bool ->
    AST.Statement ->
    [Core.SemanticFacts]
analyzeNested analyzeStmt ctx skipRoot rootStmt =
    reverse (collectStmt skipRoot (0 :: Int) [] rootStmt)
    where
        childCtx = ctx {ctxColumnIR = False, ctxSyntaxEdits = False}
        collectStmt skip depth acc stmt
            | not skip && isDmlStatement stmt =
                let acc' = if depth == 0 then analyzeStmt childCtx stmt : acc else acc
                 in collectStmtBody (depth + 1) acc' stmt
            | otherwise = collectStmtBody depth acc stmt

        collectStmtBody depth acc = \case
            AST.StatementQuery (L _ q) -> collectQuery depth acc q
            AST.StatementInsert (L _ ins) ->
                let a1 = maybe acc (collectWith depth acc . locValue) (AST.insertWith ins)
                    a2 = case AST.insertSource ins of
                        AST.InsertValues rows ->
                            Foldable.foldl' (Foldable.foldl' (\a (L _ e) -> collectExpr depth a e)) a1 rows
                        AST.InsertQuery (L _ q) -> collectQuery depth a1 q
                        AST.InsertDefaultValues -> a1
                    a3 = maybe a2 (collectOnConflict depth a2 . locValue) (AST.insertOnConflict ins)
                    a4 =
                        maybe
                            a3
                            (Foldable.foldl' (\a (L _ asgn) -> collectExpr depth a (locValue (AST.assignValue asgn))) a3)
                            (AST.insertOnDuplicate ins)
                 in Foldable.foldl'
                        (\a (L _ si) -> collectExpr depth a (locValue (AST.selectItemExpr si)))
                        a4
                        (AST.insertReturning ins)
            AST.StatementUpdate (L _ upd) ->
                let a1 = maybe acc (collectWith depth acc . locValue) (AST.updateWith upd)
                    a2 = collectFrom depth a1 (AST.updateTargets upd)
                    a3 =
                        Foldable.foldl'
                            (\a (L _ asgn) -> collectExpr depth a (locValue (AST.assignValue asgn)))
                            a2
                            (AST.updateAssignments upd)
                    a4 = maybe a3 (collectFrom depth a3) (AST.updateFrom upd)
                    a5 = maybe a4 (collectExpr depth a4 . locValue) (AST.updateWhere upd)
                    a6 =
                        Foldable.foldl'
                            (\a (L _ oi) -> collectExpr depth a (locValue (AST.orderExpr oi)))
                            a5
                            (AST.updateOrderBy upd)
                    a7 = maybe a6 (collectLimit depth a6 . locValue) (AST.updateLimit upd)
                 in Foldable.foldl'
                        (\a (L _ si) -> collectExpr depth a (locValue (AST.selectItemExpr si)))
                        a7
                        (AST.updateReturning upd)
            AST.StatementDelete (L _ del) ->
                let a1 = maybe acc (collectWith depth acc . locValue) (AST.deleteWith del)
                    a2 = maybe a1 (collectFrom depth a1) (AST.deleteFromTables del)
                    a3 = maybe a2 (collectFrom depth a2) (AST.deleteUsing del)
                    a4 = maybe a3 (collectExpr depth a3 . locValue) (AST.deleteWhere del)
                    a5 =
                        Foldable.foldl'
                            (\a (L _ oi) -> collectExpr depth a (locValue (AST.orderExpr oi)))
                            a4
                            (AST.deleteOrderBy del)
                    a6 = maybe a5 (collectLimit depth a5 . locValue) (AST.deleteLimit del)
                 in Foldable.foldl'
                        (\a (L _ si) -> collectExpr depth a (locValue (AST.selectItemExpr si)))
                        a6
                        (AST.deleteReturning del)
            AST.StatementMerge (L _ m) ->
                let a1 = maybe acc (collectWith depth acc . locValue) (AST.mergeWith m)
                    a2 = collectTableRef depth a1 (locValue (AST.mergeSource m))
                    a3 = collectExpr depth a2 (locValue (AST.mergeOn m))
                    a4 = Foldable.foldl' (collectMergeClause depth) a3 (AST.mergeClauses m)
                 in Foldable.foldl'
                        (\a (L _ si) -> collectExpr depth a (locValue (AST.selectItemExpr si)))
                        a4
                        (AST.mergeReturning m)
            AST.StatementCreate (L _ cr) -> case locValue (AST.createObject cr) of
                AST.CreateTableObject ct ->
                    maybe acc (collectQuery depth acc . locValue) (AST.createTableAs ct)
                AST.CreateViewObject cv ->
                    case AST.createViewBody cv of
                        AST.ViewBodyQuery (L _ q) -> collectQuery depth acc q
                        AST.ViewBodyOpaque _ -> acc
                _ -> acc
            AST.StatementCopy (L _ cp) -> case AST.copySource cp of
                AST.CopySourceQuery (L _ q) -> collectQuery depth acc q
                _ -> acc
            AST.StatementSet (L _ st) ->
                Foldable.foldl'
                    (\a (L _ it) -> Foldable.foldl' (\a' (L _ e) -> collectExpr depth a' e) a (AST.setItemValue it))
                    acc
                    (AST.setItems st)
            _ -> acc

        collectOnConflict depth a occ =
            let a1 =
                    Foldable.foldl'
                        (\a' (L _ kp) -> collectExpr depth a' (locValue (AST.indexKeyExpr kp)))
                        a
                        (AST.onConflictKeys occ)
                a2 = maybe a1 (collectExpr depth a1 . locValue) (AST.onConflictWhere occ)
             in case AST.onConflictAction occ of
                    AST.OnConflictDoNothing -> a2
                    AST.OnConflictDoUpdate assigns mWhere ->
                        let a3 =
                                Foldable.foldl'
                                    (\a' (L _ asgn) -> collectExpr depth a' (locValue (AST.assignValue asgn)))
                                    a2
                                    assigns
                         in maybe a3 (collectExpr depth a3 . locValue) mWhere

        collectMergeClause depth a (L _ cl) =
            let a1 = maybe a (collectExpr depth a . locValue) (AST.mergeClauseCondition cl)
             in case AST.mergeClauseAction cl of
                    AST.MergeUpdate assigns ->
                        Foldable.foldl'
                            (\a' (L _ asgn) -> collectExpr depth a' (locValue (AST.assignValue asgn)))
                            a1
                            assigns
                    AST.MergeInsert _ es -> Foldable.foldl' (\a' (L _ e) -> collectExpr depth a' e) a1 es
                    _ -> a1

        collectWith depth a w =
            Foldable.foldl'
                (\a' (L _ cte) -> collectCteBody depth a' (locValue (AST.cteBody cte)))
                a
                (AST.withCtes w)
        collectCteBody depth a (AST.CteQuery (L _ q)) = collectQuery depth a q
        collectCteBody depth a (AST.CteStatement (L _ s)) = collectStmt False depth a s

        collectQuery depth a q =
            let a1 = maybe a (collectWith depth a . locValue) (AST.queryWith q)
                a2 = collectQueryBody depth a1 (locValue (AST.queryBody q))
                a3 =
                    Foldable.foldl'
                        (\a' (L _ oi) -> collectExpr depth a' (locValue (AST.orderExpr oi)))
                        a2
                        (AST.queryOrderBy q)
                a4 = maybe a3 (collectLimit depth a3 . locValue) (AST.queryLimit q)
                a5 = maybe a4 (collectExpr depth a4 . locValue) (AST.queryOffset q)
                a6 =
                    maybe a5 (maybe a5 (collectExpr depth a5 . locValue) . AST.fetchCount . locValue) (AST.queryFetch q)
             in maybe
                    a6
                    ( Foldable.foldl' (\a' (L _ si) -> collectExpr depth a' (locValue (AST.settingValue si))) a6
                        . locValue
                    )
                    (AST.querySettings q)

        collectLimit depth a lim =
            let a1 = collectExpr depth a (locValue (AST.limitFirst lim))
             in maybe a1 (collectExpr depth a1 . locValue) (AST.limitSecond lim)

        collectQueryBody depth a = \case
            AST.QueryBodySelect sc -> collectSelect depth a sc
            AST.QueryBodyFrom sc -> collectSelect depth a sc
            AST.QueryBodySet _ _ (L _ l) (L _ r) -> collectQueryBody depth (collectQueryBody depth a l) r
            AST.QueryBodyValues rows ->
                Foldable.foldl' (Foldable.foldl' (\a' (L _ e) -> collectExpr depth a' e)) a rows
            AST.QueryBodyTable _ -> a
            AST.QueryBodyParen (L _ iq) -> collectQuery depth a iq

        collectSelect depth a sc =
            let a1 = case AST.selectQuantifier sc of
                    AST.SelectDistinctOn es -> Foldable.foldl' (\a' (L _ e) -> collectExpr depth a' e) a es
                    _ -> a
                a2 =
                    Foldable.foldl'
                        (\a' (L _ si) -> collectExpr depth a' (locValue (AST.selectItemExpr si)))
                        a1
                        (AST.selectItems sc)
                a3 = maybe a2 (collectFrom depth a2) (AST.selectFrom sc)
                a4 = maybe a3 (collectExpr depth a3 . locValue . locValue) (AST.selectPrewhere sc)
                a5 = maybe a4 (collectExpr depth a4 . locValue) (AST.selectWhere sc)
                a6 = Foldable.foldl' (collectGroupBy depth) a5 (AST.selectGroupBy sc)
                a7 = maybe a6 (collectExpr depth a6 . locValue) (AST.selectHaving sc)
                a8 =
                    Foldable.foldl'
                        (\a' (L _ wd) -> collectWindow depth a' (AST.windowDefSpec wd))
                        a7
                        (AST.selectWindows sc)
             in maybe a8 (collectExpr depth a8 . locValue . locValue) (AST.selectQualify sc)

        collectGroupBy depth a (L _ item) = case item of
            AST.GroupByExpr (L _ e) -> collectExpr depth a e
            AST.GroupByParens xs -> Foldable.foldl' (collectGroupBy depth) a xs
            AST.GroupByGroupingSets xs -> Foldable.foldl' (collectGroupBy depth) a xs
            AST.GroupByRollup xs -> Foldable.foldl' (collectGroupBy depth) a xs
            AST.GroupByCube xs -> Foldable.foldl' (collectGroupBy depth) a xs

        collectFrom depth a (AST.FromClause refs) = Foldable.foldl' (\a' (L _ tr) -> collectTableRef depth a' tr) a refs

        collectTableRef depth a tr =
            let a1 = collectTableSource depth a (locValue (AST.tableRefSource tr))
                a2 = maybe a1 (collectSample depth a1 . locValue) (AST.tableRefSample tr)
             in Foldable.foldl' (collectJoin depth) a2 (AST.tableRefJoins tr)

        collectTableSource depth a = \case
            AST.TableSourceName _ -> a
            AST.TableSourceSubquery (L _ q) -> collectQuery depth a q
            AST.TableSourceValues rows ->
                Foldable.foldl' (Foldable.foldl' (\a' (L _ e) -> collectExpr depth a' e)) a rows
            AST.TableSourceFunction (L _ fn) -> collectFn depth a fn
            AST.TableSourceParens fc -> collectFrom depth a fc
            AST.TableSourceLateral (L _ inner) -> collectTableSource depth a inner

        collectSample depth a smp =
            let a1 = collectExpr depth a (locValue (AST.tableSampleAmount smp))
                a2 = maybe a1 (collectExpr depth a1 . locValue) (AST.tableSampleRepeatable smp)
             in maybe a2 (collectExpr depth a2 . locValue) (AST.tableSampleOffset smp)

        collectJoin depth a (L _ j) =
            let a1 = case AST.joinTarget j of
                    AST.JoinTargetTable (L _ tr) -> collectTableRef depth a tr
                    AST.JoinTargetArray items ->
                        Foldable.foldl' (\a' (L _ it) -> collectExpr depth a' (locValue (AST.arrayJoinExpr it))) a items
             in case AST.joinConstraint j of
                    Just (AST.JoinOn (L _ e)) -> collectExpr depth a1 e
                    _ -> a1

        collectFn depth a fn =
            let a1 = Foldable.foldl' collectArg a (AST.functionArgs fn)
                a2 =
                    Foldable.foldl'
                        (\a' (L _ oi) -> collectExpr depth a' (locValue (AST.orderExpr oi)))
                        a1
                        (AST.functionOrderBy fn)
                a3 =
                    Foldable.foldl'
                        (\a' (L _ oi) -> collectExpr depth a' (locValue (AST.orderExpr oi)))
                        a2
                        (AST.functionWithinGroup fn)
                a4 = maybe a3 (collectExpr depth a3 . locValue) (AST.functionFilter fn)
             in case AST.functionOver fn of
                    Just (AST.WindowRefSpec spec) -> collectWindow depth a4 spec
                    _ -> a4
            where
                collectArg a' (L _ arg) = case arg of
                    AST.FunctionArgExpr (L _ e) -> collectExpr depth a' e
                    AST.FunctionArgNamed _ (L _ e) -> collectExpr depth a' e
                    AST.FunctionArgStar -> a'

        collectWindow depth a spec =
            let a1 = Foldable.foldl' (\a' (L _ e) -> collectExpr depth a' e) a (AST.windowPartitionBy spec)
                a2 =
                    Foldable.foldl'
                        (\a' (L _ oi) -> collectExpr depth a' (locValue (AST.orderExpr oi)))
                        a1
                        (AST.windowOrderBy spec)
             in case AST.windowFrame spec of
                    Nothing -> a2
                    Just frame ->
                        let a3 = collectBound (AST.windowFrameStart frame) a2
                         in maybe a3 (`collectBound` a3) (AST.windowFrameEnd frame)
            where
                collectBound (AST.BoundPreceding (L _ e)) a' = collectExpr depth a' e
                collectBound (AST.BoundFollowing (L _ e)) a' = collectExpr depth a' e
                collectBound _ a' = a'

        collectExpr depth a = \case
            AST.ExprUnary _ (L _ e) -> collectExpr depth a e
            AST.ExprBinary _ (L _ l) (L _ r) -> collectExpr depth (collectExpr depth a l) r
            AST.ExprCast _ (L _ e) _ -> collectExpr depth a e
            AST.ExprCastExpr (L _ e1) (L _ e2) -> collectExpr depth (collectExpr depth a e1) e2
            AST.ExprLambda _ (L _ e) -> collectExpr depth a e
            AST.ExprSubscript (L _ e) m1 m2 ->
                let a1 = collectExpr depth a e
                    a2 = maybe a1 (collectExpr depth a1 . locValue) m1
                 in maybe a2 (collectExpr depth a2 . locValue) m2
            AST.ExprField (L _ e) _ -> collectExpr depth a e
            AST.ExprFieldIndex (L _ e) _ -> collectExpr depth a e
            AST.ExprFieldStar (L _ e) -> collectExpr depth a e
            AST.ExprCall (L _ fn) -> collectFn depth a fn
            AST.ExprCase (AST.CaseExpr mOp whens mElse) ->
                let a1 = maybe a (collectExpr depth a . locValue) mOp
                    a2 = Foldable.foldl' (\a' (L _ c, L _ r) -> collectExpr depth (collectExpr depth a' c) r) a1 whens
                 in maybe a2 (collectExpr depth a2 . locValue) mElse
            AST.ExprExists (L _ q) -> collectQuery depth a q
            AST.ExprIsNull _ (L _ e) -> collectExpr depth a e
            AST.ExprIsTrue _ (L _ e) -> collectExpr depth a e
            AST.ExprIsFalse _ (L _ e) -> collectExpr depth a e
            AST.ExprIsUnknown _ (L _ e) -> collectExpr depth a e
            AST.ExprIsDistinct _ (L _ l) (L _ r) -> collectExpr depth (collectExpr depth a l) r
            AST.ExprBetween _ (L _ e) (L _ l) (L _ h) ->
                collectExpr depth (collectExpr depth (collectExpr depth a e) l) h
            AST.ExprInList _ (L _ e) xs ->
                Foldable.foldl' (\a' (L _ x) -> collectExpr depth a' x) (collectExpr depth a e) xs
            AST.ExprInSubquery _ (L _ e) (L _ q) -> collectQuery depth (collectExpr depth a e) q
            AST.ExprLike _ _ (L _ e) (L _ pat) mEsc ->
                let a1 = collectExpr depth (collectExpr depth a e) pat
                 in maybe a1 (collectExpr depth a1 . locValue) mEsc
            AST.ExprQuantified _ _ (L _ l) (L _ r) -> collectExpr depth (collectExpr depth a l) r
            AST.ExprArray xs -> Foldable.foldl' (\a' (L _ x) -> collectExpr depth a' x) a xs
            AST.ExprStruct pairs ->
                Foldable.foldl' (\a' (L _ k, L _ v) -> collectExpr depth (collectExpr depth a' k) v) a pairs
            AST.ExprTuple xs -> Foldable.foldl' (\a' (L _ x) -> collectExpr depth a' x) a xs
            AST.ExprSubquery (L _ q) -> collectQuery depth a q
            AST.ExprParen (L _ e) -> collectExpr depth a e
            AST.ExprCollate (L _ e) _ -> collectExpr depth a e
            _ -> a

-- | Merge nested DML facts into the enclosing statement's facts in source order.
mergeNested :: Core.SemanticFacts -> [Core.SemanticFacts] -> Core.SemanticFacts
mergeNested = Foldable.foldl' step
    where
        step acc nested =
            acc
                { Core.factOps = Core.factOps acc ++ Core.factOps nested
                , Core.factSession = Core.factSession acc ++ Core.factSession nested
                , Core.factPreTx = Core.factPreTx acc ++ Core.factPreTx nested
                , Core.factPostTx = Core.factPostTx acc ++ Core.factPostTx nested
                , Core.factBarriers =
                    Core.factBarriers acc
                        ++ filter (not . isDupUserVar (Core.factBarriers acc)) (Core.factBarriers nested)
                }
        isDupUserVar existing b = case Core.barrierScope b of
            Core.UserVariableScope name ->
                any (\ex -> Core.barrierScope ex == Core.UserVariableScope name) existing
            _ -> False
