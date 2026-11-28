-- | Column-reference extraction helpers for expressions, upserts, and view lineage.
module Occam.Frontend.Semantics.Expr (
    exprColumns,
    conflictExprColumns,
    duplicateExprColumns,
    collectExprColParts,
) where

import Control.Monad (foldM)
import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Occam.Core.Schema qualified as Core
import Occam.Frontend.AST.Expr qualified as AST
import Occam.Frontend.AST.Query qualified as AST
import Occam.Frontend.Context (
    AnalysisContext (..),
    IdentifierClass (..),
    canonicalIdent,
    isVariableIdent,
    unquoteNamePart,
 )
import Occam.Frontend.Semantics.Pure (isNiladicSqlFunction, isValuesMarker)
import Occam.Frontend.Span (Located (..))

-- | Extract distinct column identifiers referenced in expression positions within @expr@.
exprColumns :: AnalysisContext -> AST.Expr -> [Core.Ident]
exprColumns ctx root = reverse (fst (go root ([], Set.empty)))
    where
        addCol part (acc, seen) =
            let ident = canonicalIdent ctx IdentColumn part
             in if Set.member ident seen then (acc, seen) else (ident : acc, Set.insert ident seen)
        go expr st = case expr of
            AST.ExprColumn (L _ (AST.Name parts)) -> case parts of
                [L _ (AST.NamePart raw False)] | isNiladicSqlFunction (TE.decodeUtf8 raw) -> st
                L _ first : _ | isVariableIdent first -> st
                _ -> case reverse parts of
                    L _ lastPart : _ -> addCol lastPart st
                    [] -> st
            _ -> foldExprChildren go expr st

-- | Extract column references in an @ON CONFLICT@ expression that read stored target rows (excluding @EXCLUDED.*@).
conflictExprColumns :: AnalysisContext -> AST.Expr -> [Core.Ident]
conflictExprColumns ctx root =
    [col | col <- reverse order, Map.findWithDefault False col stored]
    where
        (order, stored) = go root ([], Map.empty)
        record part isExcluded (ord, m) =
            let col = canonicalIdent ctx IdentColumn part
             in case Map.lookup col m of
                    Just _ -> (ord, Map.adjust (|| not isExcluded) col m)
                    Nothing -> (col : ord, Map.insert col (not isExcluded) m)
        go expr st = case expr of
            AST.ExprColumn (L _ (AST.Name parts)) -> case parts of
                [L _ (AST.NamePart raw False)] | isNiladicSqlFunction (TE.decodeUtf8 raw) -> st
                L _ first : _ | isVariableIdent first -> st
                [L _ single] -> record single False st
                L _ first : _ ->
                    let excluded =
                            not (AST.namePartQuoted first)
                                && T.toLower (unquoteNamePart first) == "excluded"
                     in case reverse parts of
                            L _ lastPart : _ -> record lastPart excluded st
                            [] -> st
                [] -> st
            _ -> foldExprChildren go expr st

-- | Extract column references in a MySQL @ON DUPLICATE KEY UPDATE@ expression that read stored target rows.
duplicateExprColumns :: AnalysisContext -> AST.Expr -> Maybe Text -> [Core.Ident]
duplicateExprColumns ctx root mRowAlias =
    [col | col <- reverse order, Map.findWithDefault False col stored]
    where
        (order, stored) = go (0 :: Int) root ([], Map.empty)
        record part isProposed (ord, m) =
            let col = canonicalIdent ctx IdentColumn part
             in case Map.lookup col m of
                    Just _ -> (ord, Map.adjust (|| not isProposed) col m)
                    Nothing -> (col : ord, Map.insert col (not isProposed) m)
        go !valuesDepth expr st = case expr of
            AST.ExprCall (L _ fn) ->
                let depth' = if isValuesMarker fn then valuesDepth + 1 else valuesDepth
                 in foldFunctionCall (go depth') fn st
            AST.ExprColumn (L _ (AST.Name parts)) -> case parts of
                [L _ (AST.NamePart raw False)] | isNiladicSqlFunction (TE.decodeUtf8 raw) -> st
                L _ first : _ | isVariableIdent first -> st
                [L _ single] -> record single (valuesDepth > 0) st
                L _ first : _ ->
                    let aliasMatch =
                            maybe
                                False
                                (\al -> T.toLower (unquoteNamePart first) == T.toLower al)
                                mRowAlias
                        proposed = valuesDepth > 0 || aliasMatch
                     in case reverse parts of
                            L _ lastPart : _ -> record lastPart proposed st
                            [] -> st
                [] -> st
            _ -> foldExprChildren (go valuesDepth) expr st

-- | Extract column name-part paths for view column lineage; returns 'Nothing' on unmodeled expressions.
collectExprColParts :: AST.Expr -> Maybe [[AST.NamePart]]
collectExprColParts = fmap reverse . go []
    where
        go acc = \case
            AST.ExprColumn (L _ (AST.Name parts)) -> case parts of
                [L _ (AST.NamePart raw False)] | isNiladicSqlFunction (TE.decodeUtf8 raw) -> Just acc
                L _ first : _ | isVariableIdent first -> Nothing
                [] -> Nothing
                _ -> Just (map locValue parts : acc)
            AST.ExprLiteral _ -> Just acc
            AST.ExprTypedString {} -> Just acc
            AST.ExprNull -> Just acc
            AST.ExprBoolean _ -> Just acc
            AST.ExprUnary _ (L _ e) -> go acc e
            AST.ExprBinary _ (L _ l) (L _ r) -> go acc l >>= (`go` r)
            AST.ExprParen (L _ e) -> go acc e
            AST.ExprCast style (L _ e) _
                | style `elem` [AST.CastFunction, AST.CastDoubleColon, AST.CastTryFunction] ->
                    go acc e
            AST.ExprCollate (L _ e) _ -> go acc e
            AST.ExprIsNull _ (L _ e) -> go acc e
            AST.ExprIsTrue _ (L _ e) -> go acc e
            AST.ExprIsFalse _ (L _ e) -> go acc e
            AST.ExprIsUnknown _ (L _ e) -> go acc e
            AST.ExprBetween _ (L _ e) (L _ l) (L _ h) -> go acc e >>= (`go` l) >>= (`go` h)
            AST.ExprInList _ (L _ e) xs -> go acc e >>= \a1 -> foldM (\a (L _ x) -> go a x) a1 xs
            AST.ExprCase (AST.CaseExpr mOp whens mElse) -> do
                a1 <- maybe (Just acc) (go acc . locValue) mOp
                a2 <- foldM (\a (L _ c, L _ r) -> go a c >>= (`go` r)) a1 whens
                maybe (Just a2) (go a2 . locValue) mElse
            AST.ExprCall (L _ fn)
                | Nothing <- AST.functionOver fn
                , Nothing <- AST.functionFilter fn ->
                    foldM visitArg acc (AST.functionArgs fn)
                where
                    visitArg a (L _ arg) = case arg of
                        AST.FunctionArgExpr (L _ e) -> go a e
                        AST.FunctionArgNamed _ (L _ e) -> go a e
                        AST.FunctionArgStar -> Nothing
            AST.ExprLike _ likeOp (L _ e) (L _ pat) _
                | likeOp `elem` [AST.LikeLike, AST.LikeILike] ->
                    go acc e >>= (`go` pat)
            _ -> Nothing

foldExprChildren :: (AST.Expr -> s -> s) -> AST.Expr -> s -> s
foldExprChildren step expr st = case expr of
    AST.ExprUnary _ (L _ e) -> step e st
    AST.ExprBinary _ (L _ l) (L _ r) -> step r (step l st)
    AST.ExprCast _ (L _ e) _ -> step e st
    AST.ExprCastExpr (L _ e1) (L _ e2) -> step e2 (step e1 st)
    AST.ExprLambda _ (L _ e) -> step e st
    AST.ExprSubscript (L _ e) m1 m2 ->
        let s1 = step e st
            s2 = maybe s1 (flip step s1 . locValue) m1
         in maybe s2 (flip step s2 . locValue) m2
    AST.ExprField (L _ e) _ -> step e st
    AST.ExprFieldIndex (L _ e) _ -> step e st
    AST.ExprFieldStar (L _ e) -> step e st
    AST.ExprCall (L _ fn) -> foldFunctionCall step fn st
    AST.ExprCase (AST.CaseExpr mOp whens mElse) ->
        let s1 = maybe st (flip step st . locValue) mOp
            s2 = Foldable.foldl' (\a (L _ c, L _ r) -> step r (step c a)) s1 whens
         in maybe s2 (flip step s2 . locValue) mElse
    AST.ExprExists (L _ q) -> foldQueryExprs step q st
    AST.ExprIsNull _ (L _ e) -> step e st
    AST.ExprIsTrue _ (L _ e) -> step e st
    AST.ExprIsFalse _ (L _ e) -> step e st
    AST.ExprIsUnknown _ (L _ e) -> step e st
    AST.ExprIsDistinct _ (L _ l) (L _ r) -> step r (step l st)
    AST.ExprBetween _ (L _ e) (L _ l) (L _ h) -> step h (step l (step e st))
    AST.ExprInList _ (L _ e) xs -> Foldable.foldl' (\a (L _ x) -> step x a) (step e st) xs
    AST.ExprInSubquery _ (L _ e) (L _ q) -> foldQueryExprs step q (step e st)
    AST.ExprLike _ _ (L _ e) (L _ pat) mEsc ->
        let s1 = step pat (step e st)
         in maybe s1 (flip step s1 . locValue) mEsc
    AST.ExprQuantified _ _ (L _ l) (L _ r) -> step r (step l st)
    AST.ExprArray xs -> Foldable.foldl' (\a (L _ x) -> step x a) st xs
    AST.ExprStruct pairs -> Foldable.foldl' (\a (L _ k, L _ v) -> step v (step k a)) st pairs
    AST.ExprTuple xs -> Foldable.foldl' (\a (L _ x) -> step x a) st xs
    AST.ExprSubquery (L _ q) -> foldQueryExprs step q st
    AST.ExprParen (L _ e) -> step e st
    AST.ExprCollate (L _ e) _ -> step e st
    _ -> st

foldFunctionCall :: (AST.Expr -> s -> s) -> AST.FunctionCall -> s -> s
foldFunctionCall step fn st0 =
    let s1 = Foldable.foldl' foldArg st0 (AST.functionArgs fn)
        s2 = Foldable.foldl' (\a (L _ oi) -> step (locValue (AST.orderExpr oi)) a) s1 (AST.functionOrderBy fn)
        s3 =
            Foldable.foldl'
                (\a (L _ oi) -> step (locValue (AST.orderExpr oi)) a)
                s2
                (AST.functionWithinGroup fn)
        s4 = maybe s3 (flip step s3 . locValue) (AST.functionFilter fn)
     in case AST.functionOver fn of
            Just (AST.WindowRefSpec spec) -> foldWindowSpec step spec s4
            _ -> s4
    where
        foldArg a (L _ arg) = case arg of
            AST.FunctionArgExpr (L _ e) -> step e a
            AST.FunctionArgNamed _ (L _ e) -> step e a
            AST.FunctionArgStar -> a

foldWindowSpec :: (AST.Expr -> s -> s) -> AST.WindowSpec -> s -> s
foldWindowSpec step spec st0 =
    let s1 = Foldable.foldl' (\a (L _ e) -> step e a) st0 (AST.windowPartitionBy spec)
        s2 = Foldable.foldl' (\a (L _ oi) -> step (locValue (AST.orderExpr oi)) a) s1 (AST.windowOrderBy spec)
     in case AST.windowFrame spec of
            Nothing -> s2
            Just frame ->
                let s3 = foldBound (AST.windowFrameStart frame) s2
                 in maybe s3 (`foldBound` s3) (AST.windowFrameEnd frame)
    where
        foldBound (AST.BoundPreceding (L _ e)) a = step e a
        foldBound (AST.BoundFollowing (L _ e)) a = step e a
        foldBound _ a = a

foldQueryExprs :: (AST.Expr -> s -> s) -> AST.Query -> s -> s
foldQueryExprs step q st0 =
    let s1 = maybe st0 (flip foldWith st0 . locValue) (AST.queryWith q)
        s2 = foldQueryBody (locValue (AST.queryBody q)) s1
        s3 = Foldable.foldl' (\a (L _ oi) -> step (locValue (AST.orderExpr oi)) a) s2 (AST.queryOrderBy q)
        s4 = maybe s3 (flip foldLimit s3 . locValue) (AST.queryLimit q)
        s5 = maybe s4 (flip step s4 . locValue) (AST.queryOffset q)
        s6 = maybe s5 (maybe s5 (flip step s5 . locValue) . AST.fetchCount . locValue) (AST.queryFetch q)
     in maybe
            s6
            (Foldable.foldl' (\a (L _ si) -> step (locValue (AST.settingValue si)) a) s6 . locValue)
            (AST.querySettings q)
    where
        foldWith w a =
            Foldable.foldl' (\acc (L _ cte) -> foldCteBody (locValue (AST.cteBody cte)) acc) a (AST.withCtes w)
        foldCteBody (AST.CteQuery (L _ iq)) a = foldQueryExprs step iq a
        foldCteBody (AST.CteStatement _) a = a
        foldQueryBody body a = case body of
            AST.QueryBodySelect sc -> foldSelectCore sc a
            AST.QueryBodyFrom sc -> foldSelectCore sc a
            AST.QueryBodySet _ _ (L _ l) (L _ r) -> foldQueryBody r (foldQueryBody l a)
            AST.QueryBodyValues rows -> Foldable.foldl' (Foldable.foldl' (\acc (L _ e) -> step e acc)) a rows
            AST.QueryBodyTable _ -> a
            AST.QueryBodyParen (L _ iq) -> foldQueryExprs step iq a
        foldLimit lim a =
            let a1 = step (locValue (AST.limitFirst lim)) a
             in maybe a1 (flip step a1 . locValue) (AST.limitSecond lim)
        foldSelectCore sc a =
            let a1 = case AST.selectQuantifier sc of
                    AST.SelectDistinctOn exprs -> Foldable.foldl' (\acc (L _ e) -> step e acc) a exprs
                    _ -> a
                a2 =
                    Foldable.foldl'
                        (\acc (L _ si) -> step (locValue (AST.selectItemExpr si)) acc)
                        a1
                        (AST.selectItems sc)
                a3 = maybe a2 (foldFrom a2) (AST.selectFrom sc)
                a4 = maybe a3 (flip step a3 . locValue) (AST.selectWhere sc)
                a5 = maybe a4 (flip step a4 . locValue . locValue) (AST.selectPrewhere sc)
                a6 = case AST.selectGroupBy sc of
                    [L _ (AST.GroupByExpr (L _ (AST.ExprColumn (L _ (AST.Name [L _ (AST.NamePart raw False)])))))]
                        | T.toUpper (TE.decodeUtf8 raw) == "ALL" -> a5
                    items -> Foldable.foldl' foldGroupBy a5 items
                a7 = maybe a6 (flip step a6 . locValue) (AST.selectHaving sc)
                a8 =
                    Foldable.foldl'
                        (\acc (L _ wd) -> foldWindowSpec step (AST.windowDefSpec wd) acc)
                        a7
                        (AST.selectWindows sc)
             in maybe a8 (flip step a8 . locValue . locValue) (AST.selectQualify sc)
        foldGroupBy a (L _ item) = case item of
            AST.GroupByExpr (L _ e) -> step e a
            AST.GroupByParens xs -> Foldable.foldl' foldGroupBy a xs
            AST.GroupByGroupingSets xs -> Foldable.foldl' foldGroupBy a xs
            AST.GroupByRollup xs -> Foldable.foldl' foldGroupBy a xs
            AST.GroupByCube xs -> Foldable.foldl' foldGroupBy a xs
        foldFrom a (AST.FromClause refs) = Foldable.foldl' (\acc (L _ r) -> foldTableRef acc r) a refs
        foldTableRef a r =
            let a1 = foldTableSource a (locValue (AST.tableRefSource r))
                a2 = maybe a1 (foldSample a1 . locValue) (AST.tableRefSample r)
             in Foldable.foldl' foldJoin a2 (AST.tableRefJoins r)
        foldTableSource a = \case
            AST.TableSourceName _ -> a
            AST.TableSourceSubquery (L _ iq) -> foldQueryExprs step iq a
            AST.TableSourceValues rows -> Foldable.foldl' (Foldable.foldl' (\acc (L _ e) -> step e acc)) a rows
            AST.TableSourceFunction (L _ fn) -> foldFunctionCall step fn a
            AST.TableSourceParens fc -> foldFrom a fc
            AST.TableSourceLateral (L _ inner) -> foldTableSource a inner
        foldSample a smp =
            let a1 = step (locValue (AST.tableSampleAmount smp)) a
                a2 = maybe a1 (flip step a1 . locValue) (AST.tableSampleRepeatable smp)
             in maybe a2 (flip step a2 . locValue) (AST.tableSampleOffset smp)
        foldJoin a (L _ j) =
            let a1 = case AST.joinTarget j of
                    AST.JoinTargetTable (L _ tr) -> foldTableRef a tr
                    AST.JoinTargetArray items ->
                        Foldable.foldl' (\acc (L _ it) -> step (locValue (AST.arrayJoinExpr it)) acc) a items
             in case AST.joinConstraint j of
                    Just (AST.JoinOn (L _ e)) -> step e a1
                    _ -> a1
