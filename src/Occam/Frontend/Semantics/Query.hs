-- | Scoped query traversal, alias/CTE resolution, read modes, and view lineage.
module Occam.Frontend.Semantics.Query (
    CteScope (..),
    AliasScope (..),
    RelationUse (..),
    WholeRowUse (..),
    VisitorState (..),
    TableSource (..),
    runAnalysisVisitor,
    visitorRelations,
    visitorReads,
    updateVisitorReads,
    hasUnmodeledRelation,
    addUnmodeledBarriers,
    relationVisible,
    resolveAlias,
    relationQualifierMatches,
    relationWholeRow,
    relationColumns,
    selectOutputColumns,
    queryBodyOutputColumns,
    queryOutputColumns,
    classifyQueryReadMode,
    selectIntoDefinition,
    isMySqlUserVarSelectInto,
    extractViewLineage,
    tableFactorSource,
    tableWithJoinsSources,
    fromSources,
    findSource,
    sourceFromName,
) where

import Control.Monad (forM_, unless, when)
import Control.Monad.Trans.State.Strict (State, execState, gets, modify')
import Data.Foldable as Foldable (find, foldl', traverse_)
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Occam.Core.Schema qualified as Core
import Occam.Core.Semantics qualified as Core
import Occam.Frontend.AST.DDL qualified as AST
import Occam.Frontend.AST.DML qualified as AST
import Occam.Frontend.AST.Expr qualified as AST
import Occam.Frontend.AST.Query qualified as AST
import Occam.Frontend.AST.Statement qualified as AST
import Occam.Frontend.Context (
    AnalysisContext (..),
    IdentifierClass (..),
    canonicalIdent,
    canonicalIdentText,
    definitionName,
    isVariableIdent,
    nameParts,
    relationFromParts,
    relationRef,
    unquoteNamePart,
 )
import Occam.Frontend.Semantics.Expr (collectExprColParts)
import Occam.Frontend.Semantics.Nested (isDmlStatement)
import Occam.Frontend.Semantics.Pure (
    TableFunctionKind (..),
    classifyTableFunction,
    dedupPreserve,
    isEmptyClickHouseTuple,
    isNiladicSqlFunction,
    isNonNullScalarLiteral,
    isUnconditionalCountExpr,
 )
import Occam.Frontend.Semantics.Sequence (
    SequenceState,
    inIgnoredFunctionRoot,
    recordSequenceExpr,
 )
import Occam.Frontend.Span (ByteSpan (..), Located (..))
import Occam.Parser.Dialect (DialectProfile (..), SemanticsProfile (..))
import Occam.Parser.Types (SQLDialect (..))
import Text.Read (readMaybe)

-- | Lexical CTE scope introduced by a @WITH@ clause.
data CteScope = CteScope
    { cteScopeRecursive :: !Bool
    , cteScopeNames :: ![Text]
    , cteScopeBodySpans :: ![Maybe ByteSpan]
    }
    deriving (Eq, Show)

-- | One lexical query scope for table aliases and @LATERAL@ sibling limits.
data AliasScope = AliasScope
    { asParent :: !(Maybe Int)
    , asAliases :: ![(Text, Maybe [Text])]
    , asParentAliasLimit :: !(Maybe Int)
    , asParentFactorLimit :: !(Maybe Int)
    , asFactors :: !Int
    }
    deriving (Eq, Show)

-- | A relation reference recorded during statement traversal.
data RelationUse = RelationUse
    { relName :: !(Maybe Core.NameRef)
    , relSpan :: !ByteSpan
    , relIsCte :: !Bool
    , relScope :: !Int
    , relAlias :: !(Maybe Text)
    , relFactor :: !Int
    , relOnly :: !Bool
    }
    deriving (Eq, Show)

-- | Wildcard or standalone @TABLE@ whole-row reference.
data WholeRowUse
    = WholeRowSource !ByteSpan
    | WholeRowProjection ![Text] !Int
    deriving (Eq, Show)

-- | One physical table factor named by a DML statement.
data TableSource = TableSource
    { tsName :: !AST.Name
    , tsSpan :: !ByteSpan
    , tsAlias :: !(Maybe Text)
    , tsParts :: ![Text]
    , tsOnly :: !Bool
    }
    deriving (Eq, Show)

-- | Accumulated traversal state mirroring Rust @AnalysisVisitor@.
data VisitorState = VisitorState
    { vsContext :: !AnalysisContext
    , vsGateNestedDml :: !Bool
    , vsStatementDepth :: !Int
    , vsNestedDmlDepth :: !Int
    , vsCteScopes :: ![CteScope]
    , vsRelations :: ![RelationUse]
    , vsUnmodeledRelationSpans :: ![ByteSpan]
    , vsUnmodeledTableSource :: !Bool
    , vsHasFunction :: !Bool
    , vsIgnoredFunctionRoots :: ![ByteSpan]
    , vsIgnoredRelationSpans :: ![ByteSpan]
    , vsIgnoredUserVarSpans :: ![ByteSpan]
    , vsSelectIntoCount :: !Int
    , vsColumns :: ![([Text], Int)]
    , vsWholeRows :: ![WholeRowUse]
    , vsAliases :: !(Seq AliasScope)
    , vsAliasScopeStack :: ![Int]
    , vsSelectScopes :: ![Maybe Int]
    , vsPendingDerivedLimits :: !(Maybe (ByteSpan, Int, Int))
    , vsSequences :: !(Maybe SequenceState)
    , vsUserVars :: ![Core.Ident]
    }

type VisitM = State VisitorState

initialVisitorState ::
    AnalysisContext ->
    Bool ->
    [ByteSpan] ->
    [ByteSpan] ->
    Maybe SequenceState ->
    VisitorState
initialVisitorState ctx gateNestedDml ignoredFnRoots ignoredVarSpans mSeq =
    VisitorState
        { vsContext = ctx
        , vsGateNestedDml = gateNestedDml
        , vsStatementDepth = 0
        , vsNestedDmlDepth = 0
        , vsCteScopes = []
        , vsRelations = []
        , vsUnmodeledRelationSpans = []
        , vsUnmodeledTableSource = False
        , vsHasFunction = False
        , vsIgnoredFunctionRoots = ignoredFnRoots
        , vsIgnoredRelationSpans = []
        , vsIgnoredUserVarSpans = ignoredVarSpans
        , vsSelectIntoCount = 0
        , vsColumns = []
        , vsWholeRows = []
        , vsAliases = Seq.singleton (AliasScope Nothing [] Nothing Nothing 0)
        , vsAliasScopeStack = [0]
        , vsSelectScopes = []
        , vsPendingDerivedLimits = Nothing
        , vsSequences = mSeq
        , vsUserVars = []
        }

-- | Run the scoped AST visitor over a top-level statement.
runAnalysisVisitor ::
    AnalysisContext ->
    Bool ->
    [ByteSpan] ->
    [ByteSpan] ->
    Maybe SequenceState ->
    AST.Statement ->
    VisitorState
runAnalysisVisitor ctx gateNestedDml ignoredFnRoots ignoredVarSpans mSeq stmt =
    let st =
            execState
                (visitStatement stmt)
                (initialVisitorState ctx gateNestedDml ignoredFnRoots ignoredVarSpans mSeq)
     in st
            { vsRelations = reverse (vsRelations st)
            , vsColumns = reverse (vsColumns st)
            , vsWholeRows = reverse (vsWholeRows st)
            , vsUserVars = reverse (vsUserVars st)
            , vsUnmodeledRelationSpans = reverse (vsUnmodeledRelationSpans st)
            }

currentAliasScope :: VisitM Int
currentAliasScope = gets (fromMaybe 0 . listToMaybe . vsAliasScopeStack)

nextFactor :: VisitM Int
nextFactor = do
    scopeIdx <- currentAliasScope
    aliases <- gets vsAliases
    case Seq.lookup scopeIdx aliases of
        Nothing -> pure 0
        Just entry -> do
            let factor = asFactors entry
            modify' (\s -> s {vsAliases = Seq.update scopeIdx (entry {asFactors = factor + 1}) (vsAliases s)})
            pure factor

recordAlias :: AST.Alias -> Maybe [Text] -> VisitM ()
recordAlias alias target = do
    ctx <- gets vsContext
    let name = canonicalIdentText ctx IdentRelation (locValue (AST.aliasName alias))
    scopeIdx <- currentAliasScope
    modify' \s ->
        case Seq.lookup scopeIdx (vsAliases s) of
            Nothing -> s
            Just entry ->
                s
                    { vsAliases =
                        Seq.update scopeIdx (entry {asAliases = asAliases entry ++ [(name, target)]}) (vsAliases s)
                    }

recordAliasOpt :: Maybe (Located AST.Alias) -> Maybe [Text] -> VisitM ()
recordAliasOpt mAlias target = traverse_ (\(L _ al) -> recordAlias al target) mAlias

isCteReference :: [Core.Ident] -> Int -> VisitM Bool
isCteReference parts start = case parts of
    [Core.Ident name] -> do
        scopes <- gets vsCteScopes
        pure (any (matchesScope name) scopes)
    _ -> pure False
    where
        matchesScope name scope =
            let visibleCount =
                    if cteScopeRecursive scope
                        then length (cteScopeNames scope)
                        else
                            fromMaybe
                                (length (cteScopeNames scope))
                                (findIndexIn (cteScopeBodySpans scope))
             in name `elem` take visibleCount (cteScopeNames scope)
        findIndexIn [] = Nothing
        findIndexIn spans =
            let matches (Just (ByteSpan a b)) = a <= start && start < b
                matches Nothing = False
             in fmap fst (find (matches . snd) (zip [0 ..] spans))

recordRelationWith :: Maybe Core.NameRef -> ByteSpan -> Int -> Maybe Text -> Bool -> VisitM ()
recordRelationWith mRef sp factor mAlias only = do
    ignored <- gets (elem sp . vsIgnoredRelationSpans)
    nested <- gets vsNestedDmlDepth
    when (nested == 0 && not ignored) do
        isCte <- case mRef of
            Just ref
                | not (Core.nameRefTemp ref) ->
                    isCteReference (Core.nameParts (Core.nameRefName ref)) (spanStart sp)
            _ -> pure False
        scopeIdx <- currentAliasScope
        let use =
                RelationUse
                    { relName = mRef
                    , relSpan = sp
                    , relIsCte = isCte
                    , relScope = scopeIdx
                    , relAlias = mAlias
                    , relFactor = factor
                    , relOnly = only
                    }
        modify' (\s -> s {vsRelations = use : vsRelations s})

recordUserVar :: Core.Ident -> VisitM ()
recordUserVar var =
    modify' \s ->
        if var `elem` vsUserVars s
            then s
            else s {vsUserVars = var : vsUserVars s}

pushCteScope :: Maybe (Located AST.WithClause) -> VisitM ()
pushCteScope Nothing =
    modify' (\s -> s {vsCteScopes = CteScope False [] [] : vsCteScopes s})
pushCteScope (Just (L _ withClause)) = do
    ctx <- gets vsContext
    let names =
            map
                (canonicalIdentText ctx IdentRelation . locValue . AST.cteName . locValue)
                (AST.withCtes withClause)
        spans = map (Just . locSpan . AST.cteBody . locValue) (AST.withCtes withClause)
    modify'
        (\s -> s {vsCteScopes = CteScope (AST.withRecursive withClause) names spans : vsCteScopes s})

popCteScope :: VisitM ()
popCteScope = modify' (\s -> s {vsCteScopes = drop 1 (vsCteScopes s)})

-- ---------------------------------------------------------------------------
-- Statement / Query / Expression Visitors
-- ---------------------------------------------------------------------------

visitStatement :: AST.Statement -> VisitM ()
visitStatement stmt = do
    depth <- gets vsStatementDepth
    gate <- gets vsGateNestedDml
    let nestedDml = depth > 0 && isDmlStatement stmt
    modify' \s ->
        s
            { vsStatementDepth = depth + 1
            , vsNestedDmlDepth = if nestedDml && gate then vsNestedDmlDepth s + 1 else vsNestedDmlDepth s
            }
    visitStatementBody stmt
    modify' \s ->
        s
            { vsNestedDmlDepth =
                if gate && depth > 0 && isDmlStatement stmt
                    then max 0 (vsNestedDmlDepth s - 1)
                    else vsNestedDmlDepth s
            , vsStatementDepth = max 0 (vsStatementDepth s - 1)
            }

visitStatementBody :: AST.Statement -> VisitM ()
visitStatementBody = \case
    AST.StatementQuery (L _ q) -> visitQuery q
    AST.StatementPivot (L _ piv) -> visitPivot piv
    AST.StatementInsert (L _ ins) -> visitInsert ins
    AST.StatementUpdate (L _ upd) -> visitUpdate upd
    AST.StatementDelete (L _ del) -> visitDelete del
    AST.StatementMerge (L _ m) -> visitMerge m
    AST.StatementCreate (L _ cr) -> visitCreate cr
    AST.StatementAlter (L _ alt) -> visitAlter alt
    AST.StatementSet (L _ setStmt) -> visitSet setStmt
    AST.StatementCopy (L _ cp) -> case AST.copySource cp of
        AST.CopySourceQuery (L _ q) -> visitQuery q
        AST.CopySourceTable _ _ -> pure ()
    AST.StatementExplain (L _ ex) -> visitStatement (locValue (AST.explainStatement ex))
    _ -> pure ()

visitInsert :: AST.InsertStmt -> VisitM ()
visitInsert ins = do
    ctx <- gets vsContext
    when (isJust (AST.insertWith ins)) (pushCteScope (AST.insertWith ins))
    traverse_ (visitWithCteBody . locValue) (maybe [] (AST.withCtes . locValue) (AST.insertWith ins))
    let L tblSpan tblName = AST.insertTable ins
    recordRelationWith (relationRef ctx tblName) tblSpan 0 Nothing False
    case AST.insertSource ins of
        AST.InsertValues rows -> traverse_ (traverse_ visitLocatedExpr) rows
        AST.InsertQuery (L _ q) -> visitQuery q
        AST.InsertDefaultValues -> pure ()
    forM_ (AST.insertOnConflict ins) \(L _ occ) -> do
        traverse_ (visitLocatedExpr . AST.indexKeyExpr . locValue) (AST.onConflictKeys occ)
        traverse_ visitLocatedExpr (AST.onConflictWhere occ)
        case AST.onConflictAction occ of
            AST.OnConflictDoNothing -> pure ()
            AST.OnConflictDoUpdate assigns mWhere -> do
                traverse_ (visitLocatedExpr . AST.assignValue . locValue) assigns
                traverse_ visitLocatedExpr mWhere
    forM_ (AST.insertOnDuplicate ins) $
        traverse_ (visitLocatedExpr . AST.assignValue . locValue)
    traverse_ (visitLocatedExpr . AST.selectItemExpr . locValue) (AST.insertReturning ins)
    when (isJust (AST.insertWith ins)) popCteScope

visitUpdate :: AST.UpdateStmt -> VisitM ()
visitUpdate upd = do
    when (isJust (AST.updateWith upd)) (pushCteScope (AST.updateWith upd))
    traverse_ (visitWithCteBody . locValue) (maybe [] (AST.withCtes . locValue) (AST.updateWith upd))
    visitFromClause (AST.updateTargets upd)
    traverse_ (visitLocatedExpr . AST.assignValue . locValue) (AST.updateAssignments upd)
    traverse_ visitFromClause (AST.updateFrom upd)
    traverse_ visitLocatedExpr (AST.updateWhere upd)
    traverse_ (visitLocatedExpr . AST.orderExpr . locValue) (AST.updateOrderBy upd)
    forM_ (AST.updateLimit upd) \(L _ lim) -> do
        visitLocatedExpr (AST.limitFirst lim)
        traverse_ visitLocatedExpr (AST.limitSecond lim)
    traverse_ (visitLocatedExpr . AST.selectItemExpr . locValue) (AST.updateReturning upd)
    when (isJust (AST.updateWith upd)) popCteScope

visitDelete :: AST.DeleteStmt -> VisitM ()
visitDelete del = do
    ctx <- gets vsContext
    when (isJust (AST.deleteWith del)) (pushCteScope (AST.deleteWith del))
    traverse_ (visitWithCteBody . locValue) (maybe [] (AST.withCtes . locValue) (AST.deleteWith del))
    if null (AST.deleteTargets del)
        then do
            nested <- gets vsNestedDmlDepth
            when (nested == 0) do
                factorIdx <- nextFactor
                let L tblSpan tblName = AST.deleteTable del
                    mRef = relationRef ctx tblName
                    aliasTxt =
                        canonicalIdentText ctx IdentRelation . locValue . AST.aliasName . locValue <$> AST.deleteAlias del
                    targetParts = map Core.identText . Core.nameParts . Core.nameRefName <$> mRef
                recordRelationWith mRef tblSpan factorIdx aliasTxt (AST.deleteOnly del)
                recordAliasOpt (AST.deleteAlias del) targetParts
        else forM_ (AST.deleteTargets del) \(L _ dt) -> do
            let L sp name = AST.deleteTargetName dt
            recordRelationWith (relationRef ctx name) sp 0 Nothing False
    traverse_ visitFromClause (AST.deleteFromTables del)
    traverse_ visitFromClause (AST.deleteUsing del)
    traverse_ visitLocatedExpr (AST.deleteWhere del)
    traverse_ (visitLocatedExpr . AST.orderExpr . locValue) (AST.deleteOrderBy del)
    forM_ (AST.deleteLimit del) \(L _ lim) -> do
        visitLocatedExpr (AST.limitFirst lim)
        traverse_ visitLocatedExpr (AST.limitSecond lim)
    traverse_ (visitLocatedExpr . AST.selectItemExpr . locValue) (AST.deleteReturning del)
    when (isJust (AST.deleteWith del)) popCteScope

visitMerge :: AST.MergeStmt -> VisitM ()
visitMerge m = do
    ctx <- gets vsContext
    when (isJust (AST.mergeWith m)) (pushCteScope (AST.mergeWith m))
    traverse_ (visitWithCteBody . locValue) (maybe [] (AST.withCtes . locValue) (AST.mergeWith m))
    nested <- gets vsNestedDmlDepth
    when (nested == 0) do
        factorIdx <- nextFactor
        let L tblSpan tblName = AST.mergeTarget m
            mRef = relationRef ctx tblName
            aliasTxt =
                canonicalIdentText ctx IdentRelation . locValue . AST.aliasName . locValue
                    <$> AST.mergeTargetAlias m
            targetParts = map Core.identText . Core.nameParts . Core.nameRefName <$> mRef
        recordRelationWith mRef tblSpan factorIdx aliasTxt False
        recordAliasOpt (AST.mergeTargetAlias m) targetParts
    visitTableRef (locValue (AST.mergeSource m))
    visitLocatedExpr (AST.mergeOn m)
    forM_ (AST.mergeClauses m) \(L _ cl) -> do
        traverse_ visitLocatedExpr (AST.mergeClauseCondition cl)
        case AST.mergeClauseAction cl of
            AST.MergeUpdate assigns -> traverse_ (visitLocatedExpr . AST.assignValue . locValue) assigns
            AST.MergeInsert _ exprs -> traverse_ visitLocatedExpr exprs
            AST.MergeInsertDefaultValues -> pure ()
            AST.MergeDelete -> pure ()
            AST.MergeDoNothing -> pure ()
    traverse_ (visitLocatedExpr . AST.selectItemExpr . locValue) (AST.mergeReturning m)
    when (isJust (AST.mergeWith m)) popCteScope

visitCreate :: AST.CreateStmt -> VisitM ()
visitCreate cr = case locValue (AST.createObject cr) of
    AST.CreateTableObject ct -> do
        ctx <- gets vsContext
        let L tblSpan tblName = AST.createTableName ct
        recordRelationWith (relationRef ctx tblName) tblSpan 0 Nothing False
        forM_ (AST.createTablePartitionBy ct) \(L pbSpan _) ->
            modify' (\s -> s {vsIgnoredFunctionRoots = pbSpan : vsIgnoredFunctionRoots s})
        when (ctxDialect ctx == ClickHouse) $
            forM_ (AST.createTableOptions ct) \(L _ opt) ->
                case map (T.toUpper . unquoteNamePart . locValue) (AST.tableOptionName opt) of
                    ws | ws `elem` [["ORDER", "BY"], ["PRIMARY", "KEY"], ["ORDER"], ["PRIMARY"]] ->
                        case AST.tableOptionValue opt of
                            Just (L sp e)
                                | isEmptyClickHouseTuple e ->
                                    modify' (\s -> s {vsIgnoredFunctionRoots = sp : vsIgnoredFunctionRoots s})
                            _ -> pure ()
                    _ -> pure ()
        forM_ (AST.createTableElements ct) \(L _ el) -> case el of
            AST.TableElementColumn (L _ col) -> visitColumnDef col
            AST.TableElementConstraint (L _ tc) -> visitTableConstraint tc
            AST.TableElementProjection (L _ proj) -> visitQuery (locValue (AST.projectionQuery proj))
            AST.TableElementSkippingIndex (L _ si) -> visitLocatedExpr (AST.skippingIndexExpr si)
        forM_ (AST.createTableOptions ct) \(L _ opt) ->
            traverse_ visitLocatedExpr (AST.tableOptionValue opt)
        forM_ (AST.createTableSettings ct) \(L _ si) ->
            visitLocatedExpr (AST.settingValue si)
        forM_ (AST.createTablePartitionBy ct) \(L _ pb) ->
            traverse_ visitLocatedExpr (AST.partitionByExprs pb)
        forM_ (AST.createTablePartitionOf ct) \(L _ tp) -> do
            let L pSpan pName = AST.tablePartitionParent tp
            recordRelationWith (relationRef ctx pName) pSpan 0 Nothing False
            case AST.tablePartitionBound tp of
                AST.PartitionBoundDefault -> pure ()
                AST.PartitionBoundIn es -> traverse_ visitLocatedExpr es
                AST.PartitionBoundFromTo es1 es2 -> traverse_ visitLocatedExpr es1 >> traverse_ visitLocatedExpr es2
                AST.PartitionBoundWith ps -> traverse_ (visitLocatedExpr . snd) ps
        forM_ (AST.createTableAs ct) \(L _ q) -> visitQuery q
    AST.CreateViewObject cv -> do
        ctx <- gets vsContext
        let L vSpan vName = AST.createViewName cv
        recordRelationWith (relationRef ctx vName) vSpan 0 Nothing False
        forM_ (AST.createViewSettings cv) \(L _ si) -> visitLocatedExpr (AST.settingValue si)
        case AST.createViewBody cv of
            AST.ViewBodyQuery (L _ q) -> visitQuery q
            AST.ViewBodyOpaque _ -> pure ()
    AST.CreateIndexObject ci -> do
        ctx <- gets vsContext
        let L tSpan tName = AST.createIndexTable ci
        recordRelationWith (relationRef ctx tName) tSpan 0 Nothing False
        traverse_ (visitLocatedExpr . AST.indexKeyExpr . locValue) (AST.createIndexKeys ci)
        traverse_ visitLocatedExpr (AST.createIndexWhere ci)
    AST.CreateVirtualTableObject cvt -> do
        ctx <- gets vsContext
        let L tSpan tName = AST.createVirtualTableName cvt
        recordRelationWith (relationRef ctx tName) tSpan 0 Nothing False
        forM_ (AST.createVirtualTableArgs cvt) \(L _ arg) -> case arg of
            AST.VirtualTableColumn (L _ col) -> visitColumnDef col
            AST.VirtualTableExpression e -> visitLocatedExpr e
    AST.CreateMacroObject cm -> do
        ctx <- gets vsContext
        let L mSpan mName = AST.createMacroName cm
        recordRelationWith (relationRef ctx mName) mSpan 0 Nothing False
        traverse_ (traverse_ visitLocatedExpr . AST.macroParamDefault . locValue) (AST.createMacroParams cm)
        case AST.createMacroBody cm of
            Left (L _ q) -> visitQuery q
            Right e -> visitLocatedExpr e
    AST.CreateRuleObject crRule -> do
        ctx <- gets vsContext
        let L tSpan tName = AST.createRuleTable crRule
        recordRelationWith (relationRef ctx tName) tSpan 0 Nothing False
        traverse_ visitLocatedExpr (AST.createRuleWhere crRule)
    _ -> pure ()

visitColumnDef :: AST.ColumnDef -> VisitM ()
visitColumnDef col = do
    forM_ (AST.columnConstraints col) \(L _ cc) -> visitColConstraint cc
    forM_ (AST.columnSettings col) \(L _ si) -> visitLocatedExpr (AST.settingValue si)
    where
        visitColConstraint = \case
            AST.ColumnConstraintNamed _ inner -> visitColConstraint inner
            AST.ColumnConstraintOnConflict inner _ -> visitColConstraint inner
            AST.ColumnConstraintDefault e -> visitLocatedExpr e
            AST.ColumnConstraintCheck e -> visitLocatedExpr e
            AST.ColumnConstraintGenerated _ e -> visitLocatedExpr e
            AST.ColumnConstraintOnUpdate e -> visitLocatedExpr e
            AST.ColumnConstraintEphemeral mE -> traverse_ visitLocatedExpr mE
            _ -> pure ()

visitTableConstraint :: AST.TableConstraint -> VisitM ()
visitTableConstraint = \case
    AST.TableConstraintCheck _ e -> visitLocatedExpr e
    AST.TableConstraintPrimaryKey _ keys -> traverse_ (visitLocatedExpr . AST.indexKeyExpr . locValue) keys
    AST.TableConstraintUnique _ keys -> traverse_ (visitLocatedExpr . AST.indexKeyExpr . locValue) keys
    AST.TableConstraintIndex _ keys -> traverse_ (visitLocatedExpr . AST.indexKeyExpr . locValue) keys
    _ -> pure ()

visitAlter :: AST.AlterStmt -> VisitM ()
visitAlter alt = case AST.alterObject alt of
    AST.AlterObjectView -> do
        ctx <- gets vsContext
        let L vSpan vName = AST.alterTable alt
        recordRelationWith (relationRef ctx vName) vSpan 0 Nothing False
        forM_ (AST.alterActions alt) \(L _ action) -> case action of
            AST.AlterViewAsQuery (L _ q) -> visitQuery q
            _ -> pure ()
    AST.AlterObjectTable -> do
        ctx <- gets vsContext
        nested <- gets vsNestedDmlDepth
        let isMutationAction = \case
                AST.AlterUpdate {} -> True
                AST.AlterDelete {} -> True
                _ -> False
        when (nested == 0 && any (isMutationAction . locValue) (AST.alterActions alt)) do
            let L tSpan tName = AST.alterTable alt
                mRef = relationRef ctx tName
                targetParts = map Core.identText . Core.nameParts . Core.nameRefName <$> mRef
            factorIdx <- nextFactor
            recordRelationWith mRef tSpan factorIdx Nothing False
            recordAliasOpt Nothing targetParts
        forM_ (AST.alterActions alt) \(L _ action) -> case action of
            AST.AlterAddColumn _ cols _ -> traverse_ (visitColumnDef . locValue) cols
            AST.AlterAddConstraint (L _ tc) -> visitTableConstraint tc
            AST.AlterAddIndex _ keys -> traverse_ (visitLocatedExpr . AST.indexKeyExpr . locValue) keys
            AST.AlterModifyColumn cols _ -> traverse_ (visitColumnDef . locValue) cols
            AST.AlterChangeColumn _ (L _ col) _ -> visitColumnDef col
            AST.AlterAlterColumn _ colAct -> case colAct of
                AST.AlterSetDefault e -> visitLocatedExpr e
                AST.AlterType _ mUsing -> traverse_ visitLocatedExpr mUsing
                _ -> pure ()
            AST.AlterUpdate assigns parts whereExpr -> do
                traverse_ (visitLocatedExpr . AST.assignValue . locValue) assigns
                traverse_ visitLocatedExpr parts
                visitLocatedExpr whereExpr
            AST.AlterDelete parts whereExpr -> do
                traverse_ visitLocatedExpr parts
                visitLocatedExpr whereExpr
            _ -> pure ()
        forM_ (AST.alterSettings alt) \(L _ si) ->
            visitLocatedExpr (AST.settingValue si)

visitPivot :: AST.PivotStmt -> VisitM ()
visitPivot piv = do
    visitTableRef (locValue (AST.pivotSource piv))
    traverse_ visitLocatedExpr (AST.pivotOn piv)
    traverse_ visitLocatedExpr (AST.pivotIn piv)
    traverse_ (visitLocatedExpr . AST.selectItemExpr . locValue) (AST.pivotUsing piv)
    traverse_ visitLocatedExpr (AST.pivotGroupBy piv)

visitSet :: AST.SetStmt -> VisitM ()
visitSet stmt =
    forM_ (AST.setItems stmt) \(L _ item) ->
        traverse_ visitLocatedExpr (AST.setItemValue item)

visitWithCteBody :: AST.Cte -> VisitM ()
visitWithCteBody cte = case locValue (AST.cteBody cte) of
    AST.CteQuery (L _ q) -> visitQuery q
    AST.CteStatement (L _ s) -> visitStatement s

visitQuery :: AST.Query -> VisitM ()
visitQuery q = do
    parent <- currentAliasScope
    pending <- gets vsPendingDerivedLimits
    let (pAliasLim, pFactorLim, newPending) = case pending of
            Just (sp, aLim, fLim) | sp == locSpan (AST.queryBody q) -> (Just aLim, Just fLim, Nothing)
            _ -> (Nothing, Nothing, pending)
    aliases <- gets vsAliases
    let newScope = Seq.length aliases
        newEntry = AliasScope (Just parent) [] pAliasLim pFactorLim 0
    modify' \s ->
        s
            { vsAliases = aliases Seq.|> newEntry
            , vsAliasScopeStack = newScope : vsAliasScopeStack s
            , vsSelectScopes = Nothing : vsSelectScopes s
            , vsPendingDerivedLimits = newPending
            }
    pushCteScope (AST.queryWith q)
    nested <- gets vsNestedDmlDepth
    when (nested == 0) (recordTableCommands (locSpan (AST.queryBody q)) (locValue (AST.queryBody q)))
    traverse_ (visitWithCteBody . locValue) (maybe [] (AST.withCtes . locValue) (AST.queryWith q))
    visitQueryBody (locValue (AST.queryBody q))
    traverse_ (visitLocatedExpr . AST.orderExpr . locValue) (AST.queryOrderBy q)
    forM_ (AST.queryLimit q) \(L _ lim) -> do
        visitLocatedExpr (AST.limitFirst lim)
        traverse_ visitLocatedExpr (AST.limitSecond lim)
    traverse_ visitLocatedExpr (AST.queryOffset q)
    forM_ (AST.queryFetch q) \(L _ f) -> traverse_ visitLocatedExpr (AST.fetchCount f)
    forM_ (AST.querySettings q) \(L _ items) ->
        traverse_ (visitLocatedExpr . AST.settingValue . locValue) items
    popCteScope
    selScopes <- gets vsSelectScopes
    case selScopes of
        (mSelScope : restSel) -> do
            modify' (\s -> s {vsSelectScopes = restSel})
            case mSelScope of
                Just selScope -> do
                    stk <- gets vsAliasScopeStack
                    when (listToMaybe stk == Just selScope) $
                        modify' (\s -> s {vsAliasScopeStack = drop 1 (vsAliasScopeStack s)})
                Nothing -> pure ()
        [] -> pure ()
    modify' (\s -> s {vsAliasScopeStack = drop 1 (vsAliasScopeStack s)})

recordTableCommands :: ByteSpan -> AST.QueryBody -> VisitM ()
recordTableCommands sp = \case
    AST.QueryBodyTable (L tblSp name) -> do
        ctx <- gets vsContext
        recordRelationWith (relationRef ctx name) tblSp 0 Nothing False
        modify' (\s -> s {vsWholeRows = WholeRowSource tblSp : vsWholeRows s})
    AST.QueryBodySet _ _ (L spL l) (L spR r) -> do
        recordTableCommands spL l
        recordTableCommands spR r
    _ -> pure ()
    where
        _unusedSp = sp

visitQueryBody :: AST.QueryBody -> VisitM ()
visitQueryBody = \case
    AST.QueryBodySelect sc -> visitSelectCore sc
    AST.QueryBodyFrom sc -> visitSelectCore sc
    AST.QueryBodySet _ _ (L _ l) (L _ r) -> visitQueryBody l >> visitQueryBody r
    AST.QueryBodyValues rows -> traverse_ (traverse_ visitLocatedExpr) rows
    AST.QueryBodyTable _ -> pure ()
    AST.QueryBodyParen (L _ iq) -> visitQuery iq

visitSelectCore :: AST.SelectCore -> VisitM ()
visitSelectCore sc = do
    ctx <- gets vsContext
    selScopes <- gets vsSelectScopes
    case selScopes of
        (Just prev : _) -> do
            stk <- gets vsAliasScopeStack
            when (listToMaybe stk == Just prev) $
                modify' (\s -> s {vsAliasScopeStack = drop 1 (vsAliasScopeStack s)})
        _ -> pure ()
    parent <- currentAliasScope
    unless (null selScopes) do
        aliases <- gets vsAliases
        let scope = Seq.length aliases
            entry = AliasScope (Just parent) [] Nothing Nothing 0
        modify' \s ->
            s
                { vsAliases = aliases Seq.|> entry
                , vsAliasScopeStack = scope : vsAliasScopeStack s
                , vsSelectScopes = Just scope : drop 1 (vsSelectScopes s)
                }
    nested <- gets vsNestedDmlDepth
    when (nested == 0) do
        when (isJust (AST.selectInto sc)) $
            modify' (\s -> s {vsSelectIntoCount = vsSelectIntoCount s + 1})
        scopeIdx <- currentAliasScope
        forM_ (AST.selectItems sc) \(L _ si) ->
            case locValue (AST.selectItemExpr si) of
                AST.ExprStar ->
                    modify' (\s -> s {vsWholeRows = WholeRowProjection [] scopeIdx : vsWholeRows s})
                AST.ExprQualifiedStar (L _ name) ->
                    case nameParts ctx IdentRelation name of
                        Just parts ->
                            modify'
                                (\s -> s {vsWholeRows = WholeRowProjection (map Core.identText parts) scopeIdx : vsWholeRows s})
                        Nothing ->
                            modify' (\s -> s {vsUnmodeledTableSource = True})
                AST.ExprFieldStar _ ->
                    modify' (\s -> s {vsUnmodeledTableSource = True})
                _ -> pure ()
    case AST.selectQuantifier sc of
        AST.SelectDistinctOn exprs -> traverse_ visitLocatedExpr exprs
        _ -> pure ()
    traverse_ (visitLocatedExpr . AST.selectItemExpr . locValue) (AST.selectItems sc)
    case AST.selectInto sc of
        Just (L _ (AST.SelectIntoUserVars vars)) ->
            forM_ vars \(L _ raw) ->
                recordUserVar (canonicalIdent ctx IdentOther (AST.NamePart raw False))
        _ -> pure ()
    traverse_ visitFromClause (AST.selectFrom sc)
    traverse_ (visitLocatedExpr . locValue) (AST.selectPrewhere sc)
    traverse_ visitLocatedExpr (AST.selectWhere sc)
    case AST.selectGroupBy sc of
        [L _ (AST.GroupByExpr (L _ (AST.ExprColumn (L _ (AST.Name [L _ (AST.NamePart raw False)])))))]
            | T.toUpper (TE.decodeUtf8 raw) == "ALL" -> pure ()
        items -> traverse_ (visitGroupByItem . locValue) items
    traverse_ visitLocatedExpr (AST.selectHaving sc)
    forM_ (AST.selectWindows sc) \(L _ wd) -> visitWindowSpec (AST.windowDefSpec wd)
    traverse_ (visitLocatedExpr . locValue) (AST.selectQualify sc)

visitGroupByItem :: AST.GroupByItem -> VisitM ()
visitGroupByItem = \case
    AST.GroupByExpr e -> visitLocatedExpr e
    AST.GroupByParens xs -> traverse_ (visitGroupByItem . locValue) xs
    AST.GroupByGroupingSets xs -> traverse_ (visitGroupByItem . locValue) xs
    AST.GroupByRollup xs -> traverse_ (visitGroupByItem . locValue) xs
    AST.GroupByCube xs -> traverse_ (visitGroupByItem . locValue) xs

visitFromClause :: AST.FromClause -> VisitM ()
visitFromClause (AST.FromClause refs) = traverse_ (visitTableRef . locValue) refs

visitTableRef :: AST.TableRef -> VisitM ()
visitTableRef ref = do
    nested <- gets vsNestedDmlDepth
    when (nested == 0) do
        factorIdx <- nextFactor
        visitTableSource
            False
            factorIdx
            (AST.tableRefOnly ref)
            (AST.tableRefAlias ref)
            (AST.tableRefSource ref)
    forM_ (AST.tableRefSample ref) \(L _ smp) -> do
        visitLocatedExpr (AST.tableSampleAmount smp)
        traverse_ visitLocatedExpr (AST.tableSampleRepeatable smp)
        traverse_ visitLocatedExpr (AST.tableSampleOffset smp)
    forM_ (AST.tableRefJoins ref) \(L _ j) -> do
        case AST.joinTarget j of
            AST.JoinTargetTable (L _ tr) -> visitTableRef tr
            AST.JoinTargetArray items -> traverse_ (visitLocatedExpr . AST.arrayJoinExpr . locValue) items
        case AST.joinConstraint j of
            Just (AST.JoinOn e) -> visitLocatedExpr e
            _ -> pure ()

visitTableSource ::
    Bool -> Int -> Bool -> Maybe (Located AST.Alias) -> Located AST.TableSource -> VisitM ()
visitTableSource lateral factorIdx onlyFlag mAlias (L srcSpan src) = do
    ctx <- gets vsContext
    case src of
        AST.TableSourceLateral (L _ inner) ->
            visitTableSource True factorIdx onlyFlag mAlias (L srcSpan inner)
        AST.TableSourceName (L nameSpan name) -> do
            let mRef = relationRef ctx name
                aliasTxt = canonicalIdentText ctx IdentRelation . locValue . AST.aliasName . locValue <$> mAlias
                targetParts = map Core.identText . Core.nameParts . Core.nameRefName <$> mRef
            recordRelationWith mRef nameSpan factorIdx aliasTxt onlyFlag
            recordAliasOpt mAlias targetParts
        AST.TableSourceFunction (L fnSpan fn) -> do
            let fnNameSpan = locSpan (AST.functionName fn)
            case classifyTableFunction (locValue (AST.functionName fn)) (map locValue (AST.functionArgs fn)) of
                PureGenerator -> do
                    modify' (\s -> s {vsIgnoredRelationSpans = fnNameSpan : vsIgnoredRelationSpans s})
                    recordAliasOpt mAlias Nothing
                FileTable paths -> do
                    modify' (\s -> s {vsIgnoredRelationSpans = fnNameSpan : vsIgnoredRelationSpans s})
                    forM_ paths \path -> do
                        let parts = [Core.Ident path]
                        recordRelationWith (Just (relationFromParts (ctxDialect ctx) parts)) (ByteSpan 0 0) 0 Nothing False
                    recordAliasOpt mAlias (fmap (: []) (listToMaybe paths))
                Unmodeled -> do
                    modify' (\s -> s {vsUnmodeledRelationSpans = fnNameSpan : vsUnmodeledRelationSpans s})
                    recordAliasOpt mAlias Nothing
            visitFunctionCallArgs fn
            where
                _unusedFnSpan = fnSpan
        AST.TableSourceSubquery (L _ q) -> do
            scopeIdx <- currentAliasScope
            aliases <- gets vsAliases
            let aliasLimit = maybe 0 (length . asAliases) (Seq.lookup scopeIdx aliases)
                factorLimit = if lateral then factorIdx else 0
                bodySpan = locSpan (AST.queryBody q)
            modify' (\s -> s {vsPendingDerivedLimits = Just (bodySpan, aliasLimit, factorLimit)})
            recordAliasOpt mAlias Nothing
            visitQuery q
            modify' (\s -> s {vsPendingDerivedLimits = Nothing})
        AST.TableSourceValues rows -> do
            recordAliasOpt mAlias Nothing
            traverse_ (traverse_ visitLocatedExpr) rows
        AST.TableSourceParens fc -> do
            recordAliasOpt mAlias Nothing
            visitFromClause fc

visitFunctionCallArgs :: AST.FunctionCall -> VisitM ()
visitFunctionCallArgs fn = do
    forM_ (AST.functionArgs fn) \(L _ arg) -> case arg of
        AST.FunctionArgExpr e -> visitLocatedExpr e
        AST.FunctionArgNamed _ e -> visitLocatedExpr e
        AST.FunctionArgStar -> pure ()
    traverse_ (visitLocatedExpr . AST.orderExpr . locValue) (AST.functionOrderBy fn)
    traverse_ (visitLocatedExpr . AST.orderExpr . locValue) (AST.functionWithinGroup fn)
    traverse_ visitLocatedExpr (AST.functionFilter fn)
    case AST.functionOver fn of
        Just (AST.WindowRefSpec spec) -> visitWindowSpec spec
        _ -> pure ()

visitWindowSpec :: AST.WindowSpec -> VisitM ()
visitWindowSpec spec = do
    traverse_ visitLocatedExpr (AST.windowPartitionBy spec)
    traverse_ (visitLocatedExpr . AST.orderExpr . locValue) (AST.windowOrderBy spec)
    forM_ (AST.windowFrame spec) \frame -> do
        visitBound (AST.windowFrameStart frame)
        traverse_ visitBound (AST.windowFrameEnd frame)
    where
        visitBound (AST.BoundPreceding e) = visitLocatedExpr e
        visitBound (AST.BoundFollowing e) = visitLocatedExpr e
        visitBound _ = pure ()

visitLocatedExpr :: Located AST.Expr -> VisitM ()
visitLocatedExpr (L sp expr) = do
    ctx <- gets vsContext
    mSeq <- gets vsSequences
    forM_ mSeq \seqSt ->
        modify' (\s -> s {vsSequences = Just (recordSequenceExpr ctx sp expr seqSt)})
    roots <- gets vsIgnoredFunctionRoots
    let ignoredFn = inIgnoredFunctionRoot roots sp
    case expr of
        AST.ExprCall _ | not ignoredFn -> modify' (\s -> s {vsHasFunction = True})
        AST.ExprColumn (L _ (AST.Name [L _ (AST.NamePart raw False)]))
            | not ignoredFn && isNiladicSqlFunction (TE.decodeUtf8 raw) ->
                modify' (\s -> s {vsHasFunction = True})
        _ -> pure ()
    ignoredVar <- gets (elem sp . vsIgnoredUserVarSpans)
    unless ignoredVar $
        case expr of
            AST.ExprParam AST.ParamUser raw ->
                recordUserVar (canonicalIdent ctx IdentOther (AST.NamePart raw False))
            AST.ExprColumn (L _ (AST.Name (L _ first : _)))
                | isVariableIdent first && not (T.isPrefixOf "@@" (unquoteNamePart first)) ->
                    recordUserVar (canonicalIdent ctx IdentOther first)
            _ -> pure ()
    nested <- gets vsNestedDmlDepth
    when (nested == 0) do
        scopeIdx <- currentAliasScope
        case expr of
            AST.ExprColumn (L _ (AST.Name parts)) -> case parts of
                [L _ (AST.NamePart raw False)] | isNiladicSqlFunction (TE.decodeUtf8 raw) -> pure ()
                L _ first : _ | isVariableIdent first -> pure ()
                [] -> pure ()
                _ -> do
                    let n = length parts
                        foldPart idx (L _ p) =
                            canonicalIdentText ctx (if idx + 1 < n then IdentRelation else IdentColumn) p
                        folded = zipWith foldPart [0 ..] parts
                    modify' (\s -> s {vsColumns = (folded, scopeIdx) : vsColumns s})
            _ -> pure ()
    case expr of
        AST.ExprUnary _ e -> visitLocatedExpr e
        AST.ExprBinary _ l r -> visitLocatedExpr l >> visitLocatedExpr r
        AST.ExprCast _ e _ -> visitLocatedExpr e
        AST.ExprCastExpr e1 e2 -> visitLocatedExpr e1 >> visitLocatedExpr e2
        AST.ExprLambda _ e -> visitLocatedExpr e
        AST.ExprSubscript e m1 m2 -> visitLocatedExpr e >> traverse_ visitLocatedExpr m1 >> traverse_ visitLocatedExpr m2
        AST.ExprField e _ -> visitLocatedExpr e
        AST.ExprFieldIndex e _ -> visitLocatedExpr e
        AST.ExprFieldStar e -> visitLocatedExpr e
        AST.ExprCall (L _ fn) -> visitFunctionCallArgs fn
        AST.ExprCase (AST.CaseExpr mOp whens mElse) -> do
            traverse_ visitLocatedExpr mOp
            forM_ whens \(c, r) -> visitLocatedExpr c >> visitLocatedExpr r
            traverse_ visitLocatedExpr mElse
        AST.ExprExists (L _ q) -> visitQuery q
        AST.ExprIsNull _ e -> visitLocatedExpr e
        AST.ExprIsTrue _ e -> visitLocatedExpr e
        AST.ExprIsFalse _ e -> visitLocatedExpr e
        AST.ExprIsUnknown _ e -> visitLocatedExpr e
        AST.ExprIsDistinct _ l r -> visitLocatedExpr l >> visitLocatedExpr r
        AST.ExprBetween _ e l h -> visitLocatedExpr e >> visitLocatedExpr l >> visitLocatedExpr h
        AST.ExprInList _ e xs -> visitLocatedExpr e >> traverse_ visitLocatedExpr xs
        AST.ExprInSubquery _ e (L _ q) -> visitLocatedExpr e >> visitQuery q
        AST.ExprLike _ _ e pat mEsc -> visitLocatedExpr e >> visitLocatedExpr pat >> traverse_ visitLocatedExpr mEsc
        AST.ExprQuantified _ _ l r -> visitLocatedExpr l >> visitLocatedExpr r
        AST.ExprArray xs -> traverse_ visitLocatedExpr xs
        AST.ExprStruct pairs -> forM_ pairs \(k, v) -> visitLocatedExpr k >> visitLocatedExpr v
        AST.ExprTuple xs -> traverse_ visitLocatedExpr xs
        AST.ExprSubquery (L _ q) -> visitQuery q
        AST.ExprParen e -> visitLocatedExpr e
        AST.ExprCollate e _ -> visitLocatedExpr e
        _ -> pure ()

-- ---------------------------------------------------------------------------
-- Visibility, Alias Resolution & Read Fact Construction
-- ---------------------------------------------------------------------------

-- | True when a relation declared in @relationScope@ at @relationFactor@ is visible from @columnScope@.
relationVisible :: Int -> Int -> Int -> Seq AliasScope -> Bool
relationVisible relScopeIdx relFactorIdx colScopeIdx aliases = go (Just colScopeIdx) Nothing
    where
        go Nothing _ = False
        go (Just scope) incomingLimit
            | scope == relScopeIdx = case incomingLimit of
                Just limit -> relFactorIdx < limit
                Nothing -> True
            | otherwise = case Seq.lookup scope aliases of
                Nothing -> False
                Just entry -> go (asParent entry) (asParentFactorLimit entry)

-- | Resolve a single-part qualifier through lexical alias scopes.
resolveAlias :: Text -> Int -> Seq AliasScope -> Maybe (Int, Maybe [Text])
resolveAlias qualifier fromScope aliases = go (Just fromScope) Nothing
    where
        go Nothing _ = Nothing
        go (Just scope) incomingLimit = case Seq.lookup scope aliases of
            Nothing -> Nothing
            Just entry ->
                let visibleCount = maybe (length (asAliases entry)) (min (length (asAliases entry))) incomingLimit
                    visible = take visibleCount (asAliases entry)
                 in case find (\(name, _) -> name == qualifier) (reverse visible) of
                        Just (_, target) -> Just (scope, target)
                        Nothing -> go (asParent entry) (asParentAliasLimit entry)

-- | True when a column qualifier matches @relation@ / @usage@ from @columnScope@.
relationQualifierMatches :: Core.NameRef -> RelationUse -> [Text] -> Int -> Seq AliasScope -> Bool
relationQualifierMatches relation usage qualifier colScope aliases = case qualifier of
    [] -> relationVisible (relScope usage) (relFactor usage) colScope aliases
    [single] -> case resolveAlias single colScope aliases of
        Just (aliasScope, Just target) ->
            aliasScope == relScope usage
                && target == map Core.identText (Core.nameParts (Core.nameRefName relation))
        Just (_, Nothing) -> False
        Nothing ->
            isNothing (relAlias usage)
                && relationVisible (relScope usage) (relFactor usage) colScope aliases
                && (Core.identText <$> listToMaybe (reverse (Core.nameParts (Core.nameRefName relation))))
                    == Just single
    _ ->
        isNothing (relAlias usage)
            && relationVisible (relScope usage) (relFactor usage) colScope aliases
            && qualifier == map Core.identText (Core.nameParts (Core.nameRefName relation))

-- | True when @usage@ has a whole-row (@*@ or @TABLE@) read.
relationWholeRow :: Core.NameRef -> RelationUse -> [WholeRowUse] -> Seq AliasScope -> Bool
relationWholeRow relation usage wholeRows aliases = any matches wholeRows
    where
        matches = \case
            WholeRowSource sp -> sp == relSpan usage
            WholeRowProjection qualifier scope ->
                if null qualifier
                    then relScope usage == scope
                    else relationQualifierMatches relation usage qualifier scope aliases

-- | Unshadowed column references attributed to @relation@ at @usage@.
relationColumns ::
    Core.NameRef -> RelationUse -> [([Text], Int)] -> [WholeRowUse] -> Seq AliasScope -> [Core.Ident]
relationColumns relation usage columns wholeRows aliases
    | relationWholeRow relation usage wholeRows aliases = []
    | otherwise = reverse (fst (Foldable.foldl' step ([], Set.empty) columns))
    where
        step (acc, seen) (parts, colScope) = case reverse parts of
            col : revQual
                | relationQualifierMatches relation usage (reverse revQual) colScope aliases ->
                    let ident = Core.Ident col
                     in if Set.member ident seen then (acc, seen) else (ident : acc, Set.insert ident seen)
            _ -> (acc, seen)

-- | Deduplicated non-CTE physical relation uses from @visitor@, excluding @excluded@ spans.
visitorRelations :: VisitorState -> [ByteSpan] -> [(Core.NameRef, RelationUse)]
visitorRelations st excluded = reverse (fst (Foldable.foldl' step ([], Set.empty) (vsRelations st)))
    where
        step (acc, seen) rel
            | relIsCte rel
                || relSpan rel `elem` excluded
                || relSpan rel `elem` vsUnmodeledRelationSpans st =
                (acc, seen)
            | Just ref <- relName rel =
                let key = (ref, relScope rel, relAlias rel, relFactor rel)
                 in if Set.member key seen then (acc, seen) else ((ref, rel) : acc, Set.insert key seen)
            | otherwise = (acc, seen)

-- | True when any unmodeled table source or unmodeled relation span remains unexcluded.
hasUnmodeledRelation :: VisitorState -> [ByteSpan] -> Bool
hasUnmodeledRelation st excluded =
    vsUnmodeledTableSource st
        || any (`notElem` excluded) (vsUnmodeledRelationSpans st)
        || any
            (\rel -> not (relIsCte rel) && relSpan rel `notElem` excluded && isNothing (relName rel))
            (vsRelations st)

-- | Append 'Core.UnmodeledRelationSourceBarrier' when 'hasUnmodeledRelation' holds.
addUnmodeledBarriers :: VisitorState -> [ByteSpan] -> [Core.ScopedBarrier] -> [Core.ScopedBarrier]
addUnmodeledBarriers st excluded barriers
    | hasUnmodeledRelation st excluded =
        barriers ++ [Core.ScopedBarrier Core.GlobalScope Core.UnmodeledRelationSourceBarrier Nothing]
    | otherwise = barriers

-- | Convert visited physical relations into 'Core.ReadFact's.
visitorReads :: VisitorState -> [ByteSpan] -> Core.ReadMode -> [Core.ReadFact]
visitorReads st excluded mode =
    [ let cols =
            if mode `elem` [Core.ReadRowCount, Core.ReadRowExistence]
                then []
                else relationColumns ref usage (vsColumns st) (vsWholeRows st) (vsAliases st)
       in Core.ReadFact ref mode cols (relOnly usage)
    | (ref, usage) <- visitorRelations st excluded
    ]

-- | Convert visited physical relations into 'Core.ReadFact's for @UPDATE@ (skipping unreferenced unconditional targets).
updateVisitorReads ::
    VisitorState -> [ByteSpan] -> [ByteSpan] -> Bool -> Core.ReadMode -> [Core.ReadFact]
updateVisitorReads st excluded targetSpans isAll mode =
    mapMaybe build (visitorRelations st excluded)
    where
        build (ref, usage) =
            let isWholeRow = relationWholeRow ref usage (vsWholeRows st) (vsAliases st)
                cols = relationColumns ref usage (vsColumns st) (vsWholeRows st) (vsAliases st)
             in if isAll && relSpan usage `elem` targetSpans && null cols && not isWholeRow
                    then Nothing
                    else Just (Core.ReadFact ref mode cols (relOnly usage))

-- ---------------------------------------------------------------------------
-- Query Output Columns, Read Modes, SELECT INTO, and View Lineage
-- ---------------------------------------------------------------------------

-- | Closed-world output columns of a single @SELECT@ projection, or 'Nothing' on wildcards/expressions without alias.
selectOutputColumns :: AnalysisContext -> AST.SelectCore -> Maybe [Core.Ident]
selectOutputColumns ctx sc = traverse itemIdent (AST.selectItems sc)
    where
        itemIdent (L _ si) = case AST.selectItemAlias si of
            Just (L _ al) -> Just (canonicalIdent ctx IdentColumn (locValue (AST.aliasName al)))
            Nothing -> case locValue (AST.selectItemExpr si) of
                AST.ExprColumn (L _ (AST.Name parts)) ->
                    canonicalIdent ctx IdentColumn . locValue <$> listToMaybe (reverse parts)
                _ -> Nothing

-- | Closed-world output columns of a 'AST.QueryBody'.
queryBodyOutputColumns :: AnalysisContext -> AST.QueryBody -> Maybe [Core.Ident]
queryBodyOutputColumns ctx = \case
    AST.QueryBodySelect sc -> selectOutputColumns ctx sc
    AST.QueryBodyParen (L _ q) -> queryOutputColumns ctx q
    AST.QueryBodySet _ _ (L _ left) (L _ right) -> do
        cols <- queryBodyOutputColumns ctx left
        case queryBodyOutputColumns ctx right of
            Just rCols | length rCols /= length cols -> Nothing
            _ -> Just cols
    _ -> Nothing

-- | Closed-world output columns of a 'AST.Query'.
queryOutputColumns :: AnalysisContext -> AST.Query -> Maybe [Core.Ident]
queryOutputColumns ctx = queryBodyOutputColumns ctx . locValue . AST.queryBody

isPlainTableRef :: AST.TableRef -> Bool
isPlainTableRef tr =
    isNothing (AST.tableRefSample tr)
        && not (AST.tableRefFinal tr)
        && case locValue (AST.tableRefSource tr) of
            AST.TableSourceName _ -> True
            _ -> False

selectItemNonStarExpr :: AST.SelectItem -> Maybe AST.Expr
selectItemNonStarExpr si = case locValue (AST.selectItemExpr si) of
    AST.ExprStar -> Nothing
    AST.ExprQualifiedStar _ -> Nothing
    AST.ExprFieldStar _ -> Nothing
    e -> Just e

-- | Classify a standalone query's 'Core.ReadMode' ('ReadRowCount', 'ReadRowExistence', or 'ReadData').
classifyQueryReadMode :: AnalysisContext -> AST.Query -> VisitorState -> Core.ReadMode
classifyQueryReadMode ctx query st
    | isJust (AST.queryWith query)
        || not (null (AST.queryOrderBy query))
        || isJust (AST.queryFetch query)
        || isJust (AST.queryFor query)
        || isJust (AST.querySettings query)
        || isJust (AST.queryFormat query)
        || vsUnmodeledTableSource st
        || not (null (vsUnmodeledRelationSpans st))
        || length (vsRelations st) /= 1 =
        Core.ReadData
    | AST.QueryBodySelect sc <- locValue (AST.queryBody query) =
        if isNothing (AST.queryLimit query)
            && isNothing (AST.queryOffset query)
            && isUnconditionalRowCountSelect sc
            then Core.ReadRowCount
            else
                if isPureExistenceSelectWithLimit sc || isPureExistenceExistsQuery sc
                    then Core.ReadRowExistence
                    else Core.ReadData
    | otherwise = Core.ReadData
    where
        isPlainSingleFrom sc = case AST.selectFrom sc of
            Just (AST.FromClause [L _ tr]) -> null (AST.tableRefJoins tr) && isPlainTableRef tr
            _ -> False

        isBasePlainSelect sc =
            isNothing (AST.selectInto sc)
                && isNothing (AST.selectWhere sc)
                && isNothing (AST.selectHaving sc)
                && isNothing (AST.selectQualify sc)
                && AST.selectQuantifier sc == AST.SelectDefault
                && isNothing (AST.selectPrewhere sc)
                && null (AST.selectWindows sc)
                && null (AST.selectGroupBy sc)

        isUnconditionalRowCountSelect sc =
            isBasePlainSelect sc
                && isPlainSingleFrom sc
                && not (null (AST.selectItems sc))
                && checkItems False (AST.selectItems sc)
            where
                checkItems hasCount [] = hasCount
                checkItems hasCount (L _ item : rest) = case selectItemNonStarExpr item of
                    Nothing -> False
                    Just expr
                        | isUnconditionalCountExpr ctx expr (ctxPureTableEnabled ctx) -> checkItems True rest
                        | isNonNullScalarLiteral expr -> checkItems hasCount rest
                        | otherwise -> False

        isPureExistenceSelectWithLimit sc =
            queryHasPositiveLimit query
                && isBasePlainSelect sc
                && isPlainSingleFrom sc
                && not (null (AST.selectItems sc))
                && all (maybe False isNonNullScalarLiteral . selectItemNonStarExpr . locValue) (AST.selectItems sc)

        isPureExistenceExistsQuery sc =
            isNothing (AST.queryLimit query)
                && isNothing (AST.queryOffset query)
                && isNothing (AST.selectFrom sc)
                && isBasePlainSelect sc
                && case AST.selectItems sc of
                    [L _ item]
                        | Just (AST.ExprExists (L _ subq)) <- selectItemNonStarExpr item
                        , isNothing (AST.queryWith subq)
                        , null (AST.queryOrderBy subq)
                        , isNothing (AST.queryLimit subq)
                        , isNothing (AST.queryOffset subq)
                        , isNothing (AST.queryFetch subq)
                        , isNothing (AST.queryFor subq)
                        , isNothing (AST.querySettings subq)
                        , isNothing (AST.queryFormat subq)
                        , AST.QueryBodySelect inner <- locValue (AST.queryBody subq) ->
                            isBasePlainSelect inner && isPlainSingleFrom inner
                    _ -> False

queryHasPositiveLimit :: AST.Query -> Bool
queryHasPositiveLimit query =
    isNothing (AST.queryOffset query)
        && case AST.queryLimit query of
            Just (L _ (AST.LimitClause (L _ (AST.ExprLiteral (AST.Literal _ raw))) Nothing False)) ->
                maybe False (> (0 :: Integer)) (readMaybe (T.unpack (TE.decodeUtf8 raw)))
            _ -> False

-- | Extract 'Core.TableDef' for PostgreSQL @SELECT ... INTO [TEMP] table@.
selectIntoDefinition :: AnalysisContext -> AST.SelectCore -> Maybe Core.TableDef
selectIntoDefinition ctx sc
    | ctxDialect ctx /= PostgreSQL = Nothing
    | Just (L _ (AST.SelectInto (L _ targetName) scope _)) <- AST.selectInto sc = do
        let declaredTemp = scope == AST.SelectIntoTemporary
        (name, temporary) <- definitionName ctx declaredTemp targetName
        let cols = fromMaybe [] (selectOutputColumns ctx sc)
            colDefs = [Core.ColumnDef col False False False [] | col <- cols]
            transactional = semTableTransactional (dialectSemantics (ctxProfile ctx)) temporary
        pure
            Core.TableDef
                { Core.tableName = name
                , Core.tableTemporary = temporary
                , Core.tableTransactional = transactional
                , Core.tableColumns = colDefs
                , Core.tableConstraints = []
                , Core.tableSources = []
                , Core.tableLike = Nothing
                , Core.tablePartitionParent = Nothing
                , Core.tableInheritParents = []
                , Core.tableOnCommit = Nothing
                , Core.tableIfNotExists = False
                , Core.tableOrReplace = False
                }
    | otherwise = Nothing

-- | True when @query@ is a MySQL/MariaDB @SELECT ... INTO \@user_var@.
isMySqlUserVarSelectInto :: AnalysisContext -> AST.Query -> Bool
isMySqlUserVarSelectInto ctx query =
    ctxDialect ctx `elem` [MySQL, MariaDB]
        && case locValue (AST.queryBody query) of
            AST.QueryBodySelect sc
                | Just (L _ (AST.SelectIntoUserVars vars)) <- AST.selectInto sc -> not (null vars)
            _ -> False

-- | Extract output column lineage and condition columns for @CREATE VIEW ... AS SELECT@.
extractViewLineage ::
    AnalysisContext ->
    AST.Query ->
    Maybe [Core.Ident] ->
    VisitorState ->
    [ByteSpan] ->
    ([Core.ViewColumnLineage], [Core.SourceColumn])
extractViewLineage ctx query mExplicitCols visitor excluded
    | isJust (AST.queryWith query) = ([], [])
    | AST.QueryBodySelect sc <- locValue (AST.queryBody query)
    , Just (AST.FromClause fromRefs) <- AST.selectFrom sc
    , not (null fromRefs)
    , all isPlainFromItem fromRefs
    , not (null (AST.selectItems sc))
    , all (isJust . selectItemNonStarExpr . locValue) (AST.selectItems sc) =
        let outputNames = case mExplicitCols of
                Just cols | not (null cols) -> cols
                _ -> fromMaybe [] (selectOutputColumns ctx sc)
            relations = visitorRelations visitor excluded
         in case relations of
                (_, firstRel) : _
                    | length outputNames == length (AST.selectItems sc) ->
                        let colScope = relScope firstRel
                            lineage = zipWith (itemLineage relations colScope) outputNames (AST.selectItems sc)
                         in case collectCondParts sc of
                                Nothing -> ([], [])
                                Just condParts ->
                                    case traverse (resolvePartList relations colScope) condParts of
                                        Nothing -> ([], [])
                                        Just resolvedLists -> (lineage, dedupPreserve (concat resolvedLists))
                _ -> ([], [])
    | otherwise = ([], [])
    where
        isPlainFromItem (L _ tr) =
            isPlainTableRef tr
                && all isPlainJoin (AST.tableRefJoins tr)
        isPlainJoin (L _ j) = case AST.joinTarget j of
            AST.JoinTargetTable (L _ tr) -> isPlainTableRef tr
            AST.JoinTargetArray _ -> False

        itemLineage relations colScope outCol (L _ item) =
            case selectItemNonStarExpr item >>= collectExprColParts of
                Nothing -> Core.ViewColumnLineage outCol Nothing
                Just parts ->
                    case traverse (resolvePartList relations colScope) parts of
                        Nothing -> Core.ViewColumnLineage outCol Nothing
                        Just resolved -> Core.ViewColumnLineage outCol (Just (dedupPreserve (concat resolved)))

        resolvePartList relations colScope parts = case reverse parts of
            colPart : revQual ->
                let qual = map (canonicalIdentText ctx IdentRelation) (reverse revQual)
                    colIdent = canonicalIdent ctx IdentColumn colPart
                    matches =
                        [ Core.SourceColumn ref colIdent
                        | (ref, usage) <- relations
                        , relationQualifierMatches ref usage qual colScope (vsAliases visitor)
                        ]
                 in if null matches then Nothing else Just matches
            [] -> Nothing

        collectCondParts sc = do
            p1 <- maybe (Just []) (collectExprColParts . locValue) (AST.selectWhere sc)
            p2 <- case AST.selectFrom sc of
                Nothing -> Just []
                Just (AST.FromClause refs) -> concat <$> traverse collectFromJoinParts refs
            p3 <- concat <$> traverse (collectGroupPart . locValue) (AST.selectGroupBy sc)
            p4 <- maybe (Just []) (collectExprColParts . locValue) (AST.selectHaving sc)
            p5 <-
                concat
                    <$> traverse (collectExprColParts . locValue . AST.orderExpr . locValue) (AST.queryOrderBy query)
            pure (p1 ++ p2 ++ p3 ++ p4 ++ p5)

        collectFromJoinParts (L _ tr) = concat <$> traverse (collectJoinParts . locValue) (AST.tableRefJoins tr)
        collectJoinParts j = case AST.joinConstraint j of
            Just (AST.JoinOn (L _ e)) -> collectExprColParts e
            Just (AST.JoinUsing cols) -> Just [[locValue c] | c <- cols]
            Nothing -> Just []

        collectGroupPart = \case
            AST.GroupByExpr (L _ e) -> collectExprColParts e
            _ -> Nothing

-- ---------------------------------------------------------------------------
-- DML Target Source Helpers
-- ---------------------------------------------------------------------------

-- | Extract a 'TableSource' from a 'AST.TableRef' when it names a physical table.
tableFactorSource :: AnalysisContext -> AST.TableRef -> Maybe TableSource
tableFactorSource ctx tr = case locValue (AST.tableRefSource tr) of
    AST.TableSourceName (L sp name) -> do
        parts <- nameParts ctx IdentRelation name
        let aliasTxt =
                canonicalIdentText ctx IdentRelation . locValue . AST.aliasName . locValue <$> AST.tableRefAlias tr
        pure
            TableSource
                { tsName = name
                , tsSpan = sp
                , tsAlias = aliasTxt
                , tsParts = map Core.identText parts
                , tsOnly = AST.tableRefOnly tr
                }
    _ -> Nothing

-- | Collect all 'TableSource's across a 'AST.TableRef' and its join chain.
tableWithJoinsSources :: AnalysisContext -> AST.TableRef -> [TableSource]
tableWithJoinsSources ctx tr =
    maybeToList (tableFactorSource ctx tr)
        ++ concatMap joinSource (AST.tableRefJoins tr)
    where
        joinSource (L _ j) = case AST.joinTarget j of
            AST.JoinTargetTable (L _ jtr) -> tableWithJoinsSources ctx jtr
            AST.JoinTargetArray _ -> []

-- | Collect all 'TableSource's in a 'AST.FromClause'.
fromSources :: AnalysisContext -> AST.FromClause -> [TableSource]
fromSources ctx (AST.FromClause refs) = concatMap (tableWithJoinsSources ctx . locValue) refs

-- | Build an unaliased 'TableSource' directly from an 'AST.Name'.
sourceFromName :: AnalysisContext -> ByteSpan -> AST.Name -> Maybe TableSource
sourceFromName ctx sp name = do
    parts <- nameParts ctx IdentRelation name
    pure (TableSource name sp Nothing (map Core.identText parts) False)

-- | Resolve a target name against a list of 'TableSource's (by alias first, then table name).
findSource :: AnalysisContext -> AST.Name -> [TableSource] -> Maybe TableSource
findSource ctx name sources = do
    parts <- map Core.identText <$> nameParts ctx IdentRelation name
    case parts of
        [single] ->
            find (\s -> tsAlias s == Just single) sources
                `orElse` find (\s -> isNothing (tsAlias s) && listToMaybe (reverse (tsParts s)) == Just single) sources
                `orElse` find (\s -> isNothing (tsAlias s) && tsParts s == parts) sources
        _ ->
            find (\s -> isNothing (tsAlias s) && tsParts s == parts) sources
    where
        orElse (Just x) _ = Just x
        orElse Nothing y = y
