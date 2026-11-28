-- | PostgreSQL sequence-function extraction and unknown-call fencing.
module Occam.Frontend.Semantics.Sequence (
    SequenceEffect (..),
    SequenceState (..),
    initialSequenceState,
    sequenceRefFromExpr,
    sequenceFunctionEffect,
    sequenceExprEffect,
    inIgnoredFunctionRoot,
    recordSequenceExpr,
    sequenceEffectsForStatement,
) where

import Data.ByteString (ByteString)
import Data.Foldable qualified as Foldable
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Occam.Core.Schema qualified as Core
import Occam.Frontend.AST.DDL qualified as AST
import Occam.Frontend.AST.Expr qualified as AST
import Occam.Frontend.AST.Query qualified as AST
import Occam.Frontend.AST.Statement qualified as AST
import Occam.Frontend.Context (
    AnalysisContext (..),
    IdentifierRules,
    literalRelationName,
    unquoteNamePart,
 )
import Occam.Frontend.Semantics.Pure (
    decodeStringLiteral,
    isNiladicSqlFunction,
    isUnconditionalCountExpr,
    isValuesMarker,
 )
import Occam.Frontend.Span (ByteSpan (..), Located (..))
import Occam.Parser.Types (SQLDialect (..))

-- | Effect of a PostgreSQL or DuckDB sequence-function call.
data SequenceEffect
    = SeqWrite !Core.NameRef
    | SeqRead !Core.NameRef
    | SeqUnknown
    deriving (Eq, Show)

-- | Accumulated sequence reads/writes and unknown-call flag for a statement.
data SequenceState = SequenceState
    { seqWrites :: ![Core.NameRef]
    , seqReads :: ![Core.NameRef]
    , seqUnknown :: !Bool
    , seqSeen :: !(Set Core.NameRef)
    , seqIgnoredRoots :: ![ByteSpan]
    }
    deriving (Eq, Show)

-- | Initial empty 'SequenceState' with optional ignored function spans.
initialSequenceState :: [ByteSpan] -> SequenceState
initialSequenceState ignoredRoots =
    SequenceState
        { seqWrites = []
        , seqReads = []
        , seqUnknown = False
        , seqSeen = Set.empty
        , seqIgnoredRoots = ignoredRoots
        }

-- | True when @sp@ falls inside any ignored function subtree root.
inIgnoredFunctionRoot :: [ByteSpan] -> ByteSpan -> Bool
inIgnoredFunctionRoot roots sp =
    any (\root -> spanStart root <= spanStart sp && spanEnd sp <= spanEnd root) roots

-- | Extract a sequence 'Core.NameRef' from a literal or @regclass@/@text@ cast expression.
sequenceRefFromExpr :: IdentifierRules -> AST.Expr -> Maybe Core.NameRef
sequenceRefFromExpr rules = \case
    AST.ExprLiteral lit -> do
        txt <- decodeStringLiteral lit
        name <- literalRelationName rules txt
        pure (Core.NameRef name False)
    AST.ExprCast style (L _ inner) (L _ ty)
        | style `elem` [AST.CastFunction, AST.CastDoubleColon]
        , isSequenceCastType ty ->
            sequenceRefFromExpr rules inner
    AST.ExprParen (L _ inner) -> sequenceRefFromExpr rules inner
    _ -> Nothing
    where
        isSequenceCastType ty =
            null (AST.typeNameArgs ty)
                && null (AST.typeNameSuffix ty)
                && AST.typeNameArrayDims ty == 0
                && case AST.typeNameParts ty of
                    [L _ (AST.NamePart raw False)] ->
                        T.toLower (TE.decodeUtf8 raw) `elem` ["regclass", "text"]
                    _ -> False

-- | Classify a function call as a PostgreSQL or DuckDB sequence read, write, or unknown call.
sequenceFunctionEffect ::
    SQLDialect -> Bool -> IdentifierRules -> AST.FunctionCall -> Maybe SequenceEffect
sequenceFunctionEffect dialect pureEnabled rules fn = do
    L _ identPart <- case AST.nameParts (locValue (AST.functionName fn)) of
        [] -> Nothing
        parts -> Just (last parts)
    let rawLower = T.toLower (unquoteNamePart identPart)
    seqOp <- case rawLower of
        "nextval" -> Just ("nextval" :: Text)
        "currval" -> Just "currval"
        "setval" | dialect == PostgreSQL -> Just "setval"
        "lastval" | dialect == PostgreSQL -> Just "lastval"
        _ -> Nothing
    let builtinIdent part expected
            | AST.namePartQuoted part = unquoteNamePart part == expected
            | otherwise = T.toLower (unquoteNamePart part) == expected
        isBuiltin = case dialect of
            PostgreSQL ->
                builtinIdent identPart seqOp
                    && case AST.nameParts (locValue (AST.functionName fn)) of
                        [L _ schema, _] -> builtinIdent schema "pg_catalog"
                        _ -> False
            DuckDB ->
                pureEnabled
                    && case AST.nameParts (locValue (AST.functionName fn)) of
                        [_] -> True
                        [L _ schema, _] ->
                            T.toLower (unquoteNamePart schema) `elem` ["main", "system"]
                        _ -> False
            _ -> False
    let args = AST.functionArgs fn
        validArity = case seqOp of
            "nextval" -> length args == 1
            "currval" -> length args == 1
            "setval" -> length args `elem` [2, 3]
            _ -> False
        hasDisallowedModifiers =
            AST.functionDistinct fn
                || not (null (AST.functionOrderBy fn))
                || not (null (AST.functionWithinGroup fn))
                || isJust (AST.functionFilter fn)
                || isJust (AST.functionNullTreatment fn)
                || isJust (AST.functionOver fn)
    if not isBuiltin || hasDisallowedModifiers || not validArity
        then Just SeqUnknown
        else case args of
            L _ (AST.FunctionArgExpr (L _ argExpr)) : _ ->
                case sequenceRefFromExpr rules argExpr of
                    Just ref
                        | seqOp == "currval" -> Just (SeqRead ref)
                        | otherwise -> Just (SeqWrite ref)
                    Nothing -> Just SeqUnknown
            _ -> Just SeqUnknown

-- | Classify an expression as a PostgreSQL or DuckDB sequence effect if applicable.
sequenceExprEffect :: AnalysisContext -> AST.Expr -> Maybe SequenceEffect
sequenceExprEffect ctx = \case
    AST.ExprCall (L _ fn)
        | ctxDialect ctx `elem` [PostgreSQL, DuckDB] ->
            sequenceFunctionEffect (ctxDialect ctx) (ctxPureTableEnabled ctx) (ctxIdentRules ctx) fn
    _ -> Nothing

-- | Update 'SequenceState' when visiting an expression node.
recordSequenceExpr :: AnalysisContext -> ByteSpan -> AST.Expr -> SequenceState -> SequenceState
recordSequenceExpr ctx sp expr st = case sequenceExprEffect ctx expr of
    Just (SeqWrite ref)
        | Set.notMember ref (seqSeen st) ->
            st {seqWrites = seqWrites st ++ [ref], seqSeen = Set.insert ref (seqSeen st)}
        | otherwise -> st
    Just (SeqRead ref) -> st {seqReads = seqReads st ++ [ref]}
    Just SeqUnknown -> st {seqUnknown = True}
    Nothing -> case expr of
        AST.ExprCall _ ->
            let isCount = isUnconditionalCountExpr ctx expr (ctxPureTableEnabled ctx)
                ignored = isCount || inIgnoredFunctionRoot (seqIgnoredRoots st) sp
             in if ignored then st else st {seqUnknown = True}
        AST.ExprColumn (L _ (AST.Name [L _ (AST.NamePart raw False)]))
            | isNiladicSqlFunction (TE.decodeUtf8 raw)
            , not (inIgnoredFunctionRoot (seqIgnoredRoots st) sp) ->
                st {seqUnknown = True}
        _ -> st

-- | Extract sequence effects for evaluated DDL statements (CTAS, matviews, ADD COLUMN DEFAULT).
sequenceEffectsForStatement :: AnalysisContext -> AST.Statement -> SequenceState
sequenceEffectsForStatement ctx stmt = case stmt of
    AST.StatementCreate (L _ create) -> case locValue (AST.createObject create) of
        AST.CreateTableObject ct
            | AST.createTableWithData ct /= Just False
            , Just (L _ query) <- AST.createTableAs ct ->
                visitQuerySeq ctx query (initialSequenceState [])
        AST.CreateViewObject cv
            | AST.createViewMaterialized cv
            , AST.ViewBodyQuery (L _ query) <- AST.createViewBody cv ->
                visitQuerySeq ctx query (initialSequenceState [])
        _ -> initialSequenceState []
    AST.StatementAlter (L _ alter)
        | AST.alterObject alter == AST.AlterObjectTable ->
            Foldable.foldl' visitAlterAction (initialSequenceState []) (AST.alterActions alter)
    _ -> initialSequenceState []
    where
        visitAlterAction st (L _ action) = case action of
            AST.AlterAddColumn _ cols _ -> Foldable.foldl' visitColDefaults st cols
            _ -> st
        visitColDefaults st (L _ col) =
            Foldable.foldl' visitColConstraint st (AST.columnConstraints col)
        visitColConstraint st (L _ cc) = case cc of
            AST.ColumnConstraintNamed _ inner -> visitColConstraint st (L (ByteSpan 0 0) inner)
            AST.ColumnConstraintOnConflict inner _ -> visitColConstraint st (L (ByteSpan 0 0) inner)
            AST.ColumnConstraintDefault (L sp expr) -> visitExprSeq ctx sp expr st
            _ -> st

visitExprSeq :: AnalysisContext -> ByteSpan -> AST.Expr -> SequenceState -> SequenceState
visitExprSeq ctx sp expr st0 =
    let st1 = recordSequenceExpr ctx sp expr st0
     in case expr of
            AST.ExprUnary _ (L s e) -> visitExprSeq ctx s e st1
            AST.ExprBinary _ (L s1 e1) (L s2 e2) -> visitExprSeq ctx s2 e2 (visitExprSeq ctx s1 e1 st1)
            AST.ExprCast _ (L s e) _ -> visitExprSeq ctx s e st1
            AST.ExprCastExpr (L s1 e1) (L s2 e2) -> visitExprSeq ctx s2 e2 (visitExprSeq ctx s1 e1 st1)
            AST.ExprLambda _ (L s e) -> visitExprSeq ctx s e st1
            AST.ExprSubscript (L s e) m1 m2 ->
                let st2 = visitExprSeq ctx s e st1
                    st3 = maybe st2 (\(L sa ea) -> visitExprSeq ctx sa ea st2) m1
                 in maybe st3 (\(L sb eb) -> visitExprSeq ctx sb eb st3) m2
            AST.ExprField (L s e) _ -> visitExprSeq ctx s e st1
            AST.ExprFieldIndex (L s e) _ -> visitExprSeq ctx s e st1
            AST.ExprFieldStar (L s e) -> visitExprSeq ctx s e st1
            AST.ExprCall (L _ fn) -> visitFunctionCallSeq ctx fn st1
            AST.ExprCase (AST.CaseExpr mOp whens mElse) ->
                let st2 = maybe st1 (\(L s e) -> visitExprSeq ctx s e st1) mOp
                    st3 =
                        Foldable.foldl'
                            (\acc (L sw ew, L sr er) -> visitExprSeq ctx sr er (visitExprSeq ctx sw ew acc))
                            st2
                            whens
                 in maybe st3 (\(L s e) -> visitExprSeq ctx s e st3) mElse
            AST.ExprExists (L _ q) -> visitQuerySeq ctx q st1
            AST.ExprIsNull _ (L s e) -> visitExprSeq ctx s e st1
            AST.ExprIsTrue _ (L s e) -> visitExprSeq ctx s e st1
            AST.ExprIsFalse _ (L s e) -> visitExprSeq ctx s e st1
            AST.ExprIsUnknown _ (L s e) -> visitExprSeq ctx s e st1
            AST.ExprIsDistinct _ (L s1 e1) (L s2 e2) -> visitExprSeq ctx s2 e2 (visitExprSeq ctx s1 e1 st1)
            AST.ExprBetween _ (L s e) (L s1 e1) (L s2 e2) ->
                visitExprSeq ctx s2 e2 (visitExprSeq ctx s1 e1 (visitExprSeq ctx s e st1))
            AST.ExprInList _ (L s e) xs ->
                Foldable.foldl' (\acc (L sx ex) -> visitExprSeq ctx sx ex acc) (visitExprSeq ctx s e st1) xs
            AST.ExprInSubquery _ (L s e) (L _ q) -> visitQuerySeq ctx q (visitExprSeq ctx s e st1)
            AST.ExprLike _ _ (L s1 e1) (L s2 e2) mEsc ->
                let st2 = visitExprSeq ctx s2 e2 (visitExprSeq ctx s1 e1 st1)
                 in maybe st2 (\(L s3 e3) -> visitExprSeq ctx s3 e3 st2) mEsc
            AST.ExprQuantified _ _ (L s1 e1) (L s2 e2) -> visitExprSeq ctx s2 e2 (visitExprSeq ctx s1 e1 st1)
            AST.ExprArray xs -> Foldable.foldl' (\acc (L sx ex) -> visitExprSeq ctx sx ex acc) st1 xs
            AST.ExprStruct pairs ->
                Foldable.foldl'
                    (\acc (L sk ek, L sv ev) -> visitExprSeq ctx sv ev (visitExprSeq ctx sk ek acc))
                    st1
                    pairs
            AST.ExprTuple xs -> Foldable.foldl' (\acc (L sx ex) -> visitExprSeq ctx sx ex acc) st1 xs
            AST.ExprSubquery (L _ q) -> visitQuerySeq ctx q st1
            AST.ExprParen (L s e) -> visitExprSeq ctx s e st1
            AST.ExprCollate (L s e) _ -> visitExprSeq ctx s e st1
            _ -> st1

visitFunctionCallSeq :: AnalysisContext -> AST.FunctionCall -> SequenceState -> SequenceState
visitFunctionCallSeq ctx fn st0 =
    let st1 = Foldable.foldl' visitArg st0 (AST.functionArgs fn)
        st2 =
            Foldable.foldl'
                (\acc (L _ oi) -> let L s e = AST.orderExpr oi in visitExprSeq ctx s e acc)
                st1
                (AST.functionOrderBy fn)
        st3 =
            Foldable.foldl'
                (\acc (L _ oi) -> let L s e = AST.orderExpr oi in visitExprSeq ctx s e acc)
                st2
                (AST.functionWithinGroup fn)
        st4 = maybe st3 (\(L s e) -> visitExprSeq ctx s e st3) (AST.functionFilter fn)
     in case AST.functionOver fn of
            Just (AST.WindowRefSpec spec) -> visitWindowSpecSeq ctx spec st4
            _ -> st4
    where
        visitArg acc (L _ arg) = case arg of
            AST.FunctionArgExpr (L s e) -> visitExprSeq ctx s e acc
            AST.FunctionArgNamed _ (L s e) -> visitExprSeq ctx s e acc
            AST.FunctionArgStar -> acc

visitWindowSpecSeq :: AnalysisContext -> AST.WindowSpec -> SequenceState -> SequenceState
visitWindowSpecSeq ctx spec st0 =
    let st1 = Foldable.foldl' (\acc (L s e) -> visitExprSeq ctx s e acc) st0 (AST.windowPartitionBy spec)
        st2 =
            Foldable.foldl'
                (\acc (L _ oi) -> let L s e = AST.orderExpr oi in visitExprSeq ctx s e acc)
                st1
                (AST.windowOrderBy spec)
     in case AST.windowFrame spec of
            Nothing -> st2
            Just frame ->
                let st3 = visitBound (AST.windowFrameStart frame) st2
                 in maybe st3 (`visitBound` st3) (AST.windowFrameEnd frame)
    where
        visitBound (AST.BoundPreceding (L s e)) acc = visitExprSeq ctx s e acc
        visitBound (AST.BoundFollowing (L s e)) acc = visitExprSeq ctx s e acc
        visitBound _ acc = acc

visitQuerySeq :: AnalysisContext -> AST.Query -> SequenceState -> SequenceState
visitQuerySeq ctx q st0 =
    let st1 = maybe st0 (visitWithSeq . locValue) (AST.queryWith q)
        st2 = visitQueryBodySeq (locValue (AST.queryBody q)) st1
        st3 =
            Foldable.foldl'
                (\acc (L _ oi) -> let L s e = AST.orderExpr oi in visitExprSeq ctx s e acc)
                st2
                (AST.queryOrderBy q)
        st4 = maybe st3 (\(L _ lim) -> visitLimitSeq lim st3) (AST.queryLimit q)
        st5 = maybe st4 (\(L s e) -> visitExprSeq ctx s e st4) (AST.queryOffset q)
        st6 = maybe st5 (\(L _ f) -> visitFetchSeq f st5) (AST.queryFetch q)
     in maybe
            st6
            ( Foldable.foldl' (\acc (L _ si) -> let L s e = AST.settingValue si in visitExprSeq ctx s e acc) st6
                . locValue
            )
            (AST.querySettings q)
    where
        visitWithSeq w =
            Foldable.foldl'
                (\acc (L _ cte) -> visitCteBodySeq (locValue (AST.cteBody cte)) acc)
                st0
                (AST.withCtes w)
        visitCteBodySeq (AST.CteQuery (L _ iq)) acc = visitQuerySeq ctx iq acc
        visitCteBodySeq (AST.CteStatement _) acc = acc
        visitQueryBodySeq body acc = case body of
            AST.QueryBodySelect sc -> visitSelectCoreSeq sc acc
            AST.QueryBodyFrom sc -> visitSelectCoreSeq sc acc
            AST.QueryBodySet _ _ (L _ l) (L _ r) -> visitQueryBodySeq r (visitQueryBodySeq l acc)
            AST.QueryBodyValues rows -> Foldable.foldl' (Foldable.foldl' (\a (L s e) -> visitExprSeq ctx s e a)) acc rows
            AST.QueryBodyTable _ -> acc
            AST.QueryBodyParen (L _ iq) -> visitQuerySeq ctx iq acc
        visitLimitSeq lim acc =
            let a1 = let L s e = AST.limitFirst lim in visitExprSeq ctx s e acc
             in maybe a1 (\(L s e) -> visitExprSeq ctx s e a1) (AST.limitSecond lim)
        visitFetchSeq f acc = maybe acc (\(L s e) -> visitExprSeq ctx s e acc) (AST.fetchCount f)
        visitSelectCoreSeq sc acc =
            let a1 = case AST.selectQuantifier sc of
                    AST.SelectDistinctOn exprs -> Foldable.foldl' (\a (L s e) -> visitExprSeq ctx s e a) acc exprs
                    _ -> acc
                a2 =
                    Foldable.foldl'
                        (\a (L _ si) -> let L s e = AST.selectItemExpr si in visitExprSeq ctx s e a)
                        a1
                        (AST.selectItems sc)
                a3 = maybe a2 (visitFromSeq a2) (AST.selectFrom sc)
                a4 = maybe a3 (\(L s e) -> visitExprSeq ctx s e a3) (AST.selectWhere sc)
                a5 = maybe a4 (\(L _ (L s e)) -> visitExprSeq ctx s e a4) (AST.selectPrewhere sc)
                a6 = Foldable.foldl' visitGroupBy a5 (AST.selectGroupBy sc)
                a7 = maybe a6 (\(L s e) -> visitExprSeq ctx s e a6) (AST.selectHaving sc)
                a8 =
                    Foldable.foldl'
                        (\a (L _ wd) -> visitWindowSpecSeq ctx (AST.windowDefSpec wd) a)
                        a7
                        (AST.selectWindows sc)
             in maybe a8 (\(L _ (L s e)) -> visitExprSeq ctx s e a8) (AST.selectQualify sc)
        visitGroupBy acc (L _ item) = case item of
            AST.GroupByExpr (L s e) -> visitExprSeq ctx s e acc
            AST.GroupByParens xs -> Foldable.foldl' visitGroupBy acc xs
            AST.GroupByGroupingSets xs -> Foldable.foldl' visitGroupBy acc xs
            AST.GroupByRollup xs -> Foldable.foldl' visitGroupBy acc xs
            AST.GroupByCube xs -> Foldable.foldl' visitGroupBy acc xs
        visitFromSeq acc (AST.FromClause refs) = Foldable.foldl' (\a (L _ r) -> visitTableRefSeq a r) acc refs
        visitTableRefSeq acc r =
            let a1 = visitTableSourceSeq acc (locValue (AST.tableRefSource r))
                a2 = maybe a1 (visitSampleSeq a1 . locValue) (AST.tableRefSample r)
             in Foldable.foldl' visitJoinSeq a2 (AST.tableRefJoins r)
        visitTableSourceSeq acc = \case
            AST.TableSourceName _ -> acc
            AST.TableSourceSubquery (L _ iq) -> visitQuerySeq ctx iq acc
            AST.TableSourceValues rows -> Foldable.foldl' (Foldable.foldl' (\a (L s e) -> visitExprSeq ctx s e a)) acc rows
            AST.TableSourceFunction (L _ fn) -> visitFunctionCallSeq ctx fn acc
            AST.TableSourceParens fc -> visitFromSeq acc fc
            AST.TableSourceLateral (L _ inner) -> visitTableSourceSeq acc inner
        visitSampleSeq acc smp =
            let a1 = let L s e = AST.tableSampleAmount smp in visitExprSeq ctx s e acc
                a2 = maybe a1 (\(L s e) -> visitExprSeq ctx s e a1) (AST.tableSampleRepeatable smp)
             in maybe a2 (\(L s e) -> visitExprSeq ctx s e a2) (AST.tableSampleOffset smp)
        visitJoinSeq acc (L _ j) =
            let a1 = case AST.joinTarget j of
                    AST.JoinTargetTable (L _ tr) -> visitTableRefSeq acc tr
                    AST.JoinTargetArray items ->
                        Foldable.foldl'
                            (\a (L _ it) -> let L s e = AST.arrayJoinExpr it in visitExprSeq ctx s e a)
                            acc
                            items
             in case AST.joinConstraint j of
                    Just (AST.JoinOn (L s e)) -> visitExprSeq ctx s e a1
                    _ -> a1

-- Keep unused import check satisfied if isValuesMarker is re-exported / used by DML
_unusedValuesMarker :: AST.FunctionCall -> Bool
_unusedValuesMarker = isValuesMarker

_unusedByteString :: ByteString -> ByteString
_unusedByteString = id
