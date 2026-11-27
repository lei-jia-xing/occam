-- | DDL edit generation for CREATE and ALTER statements.
module Occam.Frontend.Edits.DDL (
    visitCreateStmt,
    visitAlterStmt,
) where

import Control.Monad (forM_, guard, unless, when)
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Occam.Facts (ExprTier (..), ParserEditCategory (..))
import Occam.Frontend.AST qualified as AST
import Occam.Frontend.Context (AnalysisContext (..))
import Occam.Frontend.Edits.Core (
    EditM,
    absTuple,
    addEditBytes,
    addExprEdit,
    addTypeEdits,
    askCtx,
    askSource,
    askTokens,
    listEdits,
 )
import Occam.Frontend.Edits.Expr (addScalarEdit, exprSpan, sliceExpr, visitExpr)
import Occam.Frontend.Edits.Query (visitQuery)
import Occam.Frontend.Edits.Tokens (
    TokenIndex (..),
    columnOptionsSpan,
    enclosingParentheses,
    isSignificantToken,
    isWordCI,
    nextSignificant,
    partitionPoint,
    precedingWhitespace,
    previousSignificant,
    tableBodyItem,
 )
import Occam.Frontend.Lexer (LiteralKind (..), SpannedToken, Token (..), WordToken (..), asciiEqCI)
import Occam.Frontend.Parser.Core (parseParenList, runParser)
import Occam.Frontend.Parser.Expr (parseExpr)
import Occam.Frontend.Parser.Query (parseQuery)
import Occam.Frontend.Parser.Stmt (parseStatement)
import Occam.Frontend.Span (ByteSpan (..), Located (..))
import Occam.Parser.Dialect (dialectProfile)
import Occam.Parser.Types (SQLDialect (..))

-- | Emit column type/constraint edits for a single 'AST.ColumnDef'.
addColumnDefEdits :: AST.ColumnDef -> EditM ()
addColumnDefEdits col = do
    ctx <- askCtx
    src <- askSource
    ti <- askTokens
    let extraConstraints = case AST.columnType col of
            Nothing -> []
            Just ty ->
                let (effTypeSpan, extras) = splitColumnTypeSuffixes ctx ti ty
                 in effTypeSpan `seq` extras
    forM_ (AST.columnType col) \ty -> do
        let (effTypeSpan, _) = splitColumnTypeSuffixes ctx ti ty
            (_, typeEnd) = effTypeSpan
        addTypeEdits effTypeSpan
        forM_ (columnOptionsSpan ti typeEnd) \(optStart, optEnd) ->
            addEditBytes optStart optEnd "" EditConstraint "remove-column_constraint"
    forM_ extraConstraints \(s, e) ->
        let !editStart = precedingWhitespace src s
         in addEditBytes editStart e "" EditConstraint "remove-column_constraint"
    forM_ (AST.columnConstraints col) \(L _ opt) ->
        forM_ (colConstraintSpan ctx opt) \(s, e) ->
            let !editStart = precedingWhitespace src s
             in addEditBytes editStart e "" EditConstraint "remove-column_constraint"

{- | Split trailing @COLLATE <name>@ or @CHARACTER SET <name>@ suffixes out of a
column's 'AST.TypeName' so type edits match Rust @DataType@ and the detached
suffix emits a @ColumnOption@ span.
-}
splitColumnTypeSuffixes ::
    AnalysisContext ->
    TokenIndex ->
    Located AST.TypeName ->
    ((Int, Int), [(Int, Int)])
splitColumnTypeSuffixes ctx ti (L tySp ty) =
    let (tyStart, tyEnd) = absTuple ctx tySp
     in case findSplitSuffix (AST.typeNameSuffix ty) of
            Nothing -> ((tyStart, tyEnd), [])
            Just (kwSp, extras) ->
                let (kwStart, _) = absTuple ctx kwSp
                    effEnd = case previousSignificant ti kwStart of
                        Just (_, prevTok) -> min tyEnd (spanEnd (locSpan prevTok))
                        Nothing -> tyEnd
                 in ((tyStart, max tyStart effEnd), extras)
    where
        findSplitSuffix [] = Nothing
        findSplitSuffix (L kwSp (AST.NamePart raw False) : rest)
            | asciiEqCI "COLLATE" raw = case rest of
                (firstPart : _) -> Just (kwSp, collectCollateParts firstPart rest)
                [] -> Nothing
            | asciiEqCI "CHARACTER" raw = case rest of
                (L _ (AST.NamePart setRaw False) : firstPart : more)
                    | asciiEqCI "SET" setRaw ->
                        Just (kwSp, collectCollateParts firstPart more)
                _ -> findSplitSuffix rest
        findSplitSuffix (_ : rest) = findSplitSuffix rest

        collectCollateParts firstPart rest =
            let (s, _) = absTuple ctx (locSpan firstPart)
                (_, e) = case takeNameParts rest of
                    [] -> absTuple ctx (locSpan firstPart)
                    lastPart : _ -> absTuple ctx (locSpan lastPart)
             in [(s, e)]

        takeNameParts [] = []
        takeNameParts (p@(L _ (AST.NamePart raw False)) : ps)
            | asciiEqCI "COLLATE" raw || asciiEqCI "CHARACTER" raw = []
            | otherwise = p : takeNameParts ps
        takeNameParts (p : ps) = p : takeNameParts ps

{- | Constraint span matching Rust @ColumnOption::span()@ (inner expression/name
span rather than keyword span; returns 'Nothing' for @NOT NULL@, @PRIMARY KEY@, etc.).
-}
colConstraintSpan :: AnalysisContext -> AST.ColumnConstraint -> Maybe (Int, Int)
colConstraintSpan ctx = \case
    AST.ColumnConstraintNamed namePart inner ->
        let nSpan@(ns, ne) = absTuple ctx (locSpan namePart)
         in case colConstraintSpan ctx inner of
                Just (is, ie) -> Just (min ns is, max ne ie)
                Nothing -> Just nSpan
    AST.ColumnConstraintDefault e -> Just (exprSpanForConstraint e)
    AST.ColumnConstraintCheck e -> Just (exprSpanForConstraint e)
    AST.ColumnConstraintReferences (L _ fk) ->
        let (s, e0) = absTuple ctx (locSpan (AST.foreignKeyTable fk))
            e = case reverse (AST.foreignKeyColumns fk) of
                c : _ -> snd (absTuple ctx (locSpan c))
                [] -> e0
         in Just (s, e)
    AST.ColumnConstraintCollate (L sp _) -> Just (absTuple ctx sp)
    AST.ColumnConstraintCharacterSet (L sp _) -> Just (absTuple ctx sp)
    AST.ColumnConstraintOnUpdate e -> Just (exprSpanForConstraint e)
    AST.ColumnConstraintEphemeral mE -> fmap exprSpanForConstraint mE
    _ -> Nothing
    where
        exprSpanForConstraint (L sp node) = case node of
            AST.ExprParen inner -> exprSpanForConstraint inner
            AST.ExprCast _ inner _ -> exprSpanForConstraint inner
            AST.ExprCall (L _ fc) ->
                let (ns, ne) = absTuple ctx (locSpan (AST.functionName fc))
                    endPos = case reverse (AST.functionArgs fc) of
                        L _ (AST.FunctionArgExpr e) : _ -> max ne (snd (exprSpanForConstraint e))
                        L _ (AST.FunctionArgNamed _ e) : _ -> max ne (snd (exprSpanForConstraint e))
                        L argSp AST.FunctionArgStar : _ -> max ne (snd (absTuple ctx argSp))
                        [] -> ne
                 in (ns, endPos)
            _ -> absTuple ctx sp

tableConstraintSpan :: AnalysisContext -> TokenIndex -> ByteSpan -> Maybe (Int, Int)
tableConstraintSpan ctx ti tcSp = do
    let (innerStart, innerEnd) = absTuple ctx tcSp
    (openPos, closePos) <- enclosingParentheses ti innerStart innerEnd
    let !bodyStart = openPos + 1
        !bodyEnd = closePos - 1
    guard (bodyStart <= bodyEnd)
    (itemStart, itemEnd) <- tableBodyItem ti bodyStart bodyEnd innerStart
    if itemStart > bodyStart
        then
            let !commaStart = itemStart - 1
                !editStart = precedingWhitespace (ctxSource ctx) commaStart
                !editEnd = min itemEnd bodyEnd
             in Just (editStart, editEnd)
        else
            if itemEnd < bodyEnd
                then Just (itemStart, itemEnd + 1)
                else Nothing

-- | Traverse a 'AST.CreateStmt', emitting DDL edits and visiting nested queries/expressions.
visitCreateStmt :: (Located AST.Statement -> EditM ()) -> AST.CreateStmt -> EditM ()
visitCreateStmt visitStmt create = do
    ctx <- askCtx
    ti <- askTokens
    let visitQ = visitQuery visitStmt False
        visitE = visitExpr visitQ
    case locValue (AST.createObject create) of
        AST.CreateTableObject ct -> do
            forM_ (AST.createTableElements ct) \case
                L _ (AST.TableElementColumn (L _ col)) -> addColumnDefEdits col
                _ -> pure ()
            forM_ (AST.createTableElements ct) \case
                L _ (AST.TableElementConstraint (L tcSp _)) ->
                    forM_ (tableConstraintSpan ctx ti tcSp) \(s, e) ->
                        addEditBytes s e "" EditConstraint "remove-table_constraint"
                _ -> pure ()
            forM_ (AST.createTableElements ct) \case
                L _ (AST.TableElementColumn (L _ col)) -> visitColumnExprs visitE col
                L _ (AST.TableElementConstraint (L _ tc)) -> visitTableConstraintExprs visitE tc
                L _ (AST.TableElementProjection (L _ proj)) -> visitQ (AST.projectionQuery proj)
                L _ (AST.TableElementSkippingIndex (L _ idx)) -> do
                    visitE (AST.skippingIndexExpr idx)
                    traverse_ visitE (AST.skippingIndexGranularity idx)
            forM_ (AST.createTableOptions ct) (visitTableOption visitE . locValue)
            traverse_ (visitOpaqueEngine visitE) (AST.createTableEngine ct)
            forM_ (AST.createTableSettings ct) (visitE . AST.settingValue . locValue)
            forM_ (AST.createTablePartitionOf ct) \(L _ part) ->
                visitPartitionBound visitE (AST.tablePartitionBound part)
            forM_ (AST.createTablePartitionBy ct) \(L pbSp pb) -> do
                let (stratStart, _) = absTuple ctx (locSpan (AST.partitionByStrategy pb))
                    (_, pbEnd) = absTuple ctx pbSp
                    exprs = AST.partitionByExprs pb
                listEdits "remove-function_argument" exprs (pure . exprSpan ctx ti)
                when (stratStart < pbEnd) do
                    forM_ exprs \arg ->
                        forM_ (sliceExpr ctx ti arg) \repl ->
                            addExprEdit UnwrapExpr stratStart pbEnd repl "simplify-function_to_argument"
                    forM_ ["0", "1"] \repl ->
                        addExprEdit AggressiveExpr stratStart pbEnd repl "simplify-function_literal"
                mapM_ visitE exprs
            traverse_ visitQ (AST.createTableAs ct)
        AST.CreateIndexObject ci -> do
            mapM_ (visitIndexKeyExprs visitE) (AST.createIndexKeys ci)
            traverse_ visitE (AST.createIndexWhere ci)
        AST.CreateViewObject cv -> case AST.createViewBody cv of
            AST.ViewBodyQuery q -> visitQ q
            AST.ViewBodyOpaque _ -> pure ()
        AST.CreateSequenceObject cs ->
            forM_ (AST.createSequenceOptions cs) (visitTableOption visitE . locValue)
        AST.CreateDatabaseObject cd ->
            forM_ (AST.createDatabaseOptions cd) (visitTableOption visitE . locValue)
        AST.CreateStatisticsObject cs ->
            mapM_ visitE (AST.createStatisticsColumns cs)
        AST.CreateMacroObject cm -> do
            forM_ (AST.createMacroParams cm) (traverse_ visitE . AST.macroParamDefault . locValue)
            case AST.createMacroBody cm of
                Left q -> visitQ q
                Right e -> visitE e
        AST.CreatePolicyObject cp -> do
            traverse_ visitE (AST.createPolicyUsing cp)
            traverse_ visitE (AST.createPolicyWithCheck cp)
        AST.CreateRuleObject cr -> do
            traverse_ visitE (AST.createRuleWhere cr)
            traverse_ visitStmt (AST.createRuleAction cr)
        AST.CreateTriggerObject ctr -> do
            traverse_ visitE (AST.createTriggerWhen ctr)
            mapM_ visitE (AST.createTriggerExecuteArgs ctr)
            when (ctxDialect ctx == SQLite) $
                visitSqliteTriggerBody visitStmt (AST.createTriggerBody ctr)
        AST.CreateFunctionObject cr -> do
            forM_ (AST.createRoutineParams cr) (traverse_ visitE . AST.routineParamDefault . locValue)
            visitOpaqueBodyLiteral (AST.createRoutineBody cr)
        AST.CreateProcedureObject cr -> do
            forM_ (AST.createRoutineParams cr) (traverse_ visitE . AST.routineParamDefault . locValue)
            visitOpaqueBodyLiteral (AST.createRoutineBody cr)
        AST.CreateAggregateObject ca -> do
            forM_ (AST.createAggregateParams ca) (traverse_ visitE . AST.routineParamDefault . locValue)
            forM_ (AST.createAggregateOptions ca) (visitTableOption visitE . locValue)
        AST.CreateVirtualTableObject cvt ->
            forM_ (AST.createVirtualTableArgs cvt) \case
                L _ (AST.VirtualTableColumn (L _ col)) -> visitColumnExprs visitE col
                L _ (AST.VirtualTableExpression e) -> visitE e
        AST.CreateSchemaObject _ -> pure ()
        AST.CreateExtensionObject _ -> pure ()

visitTableOption :: (Located AST.Expr -> EditM ()) -> AST.TableOption -> EditM ()
visitTableOption visitE opt = do
    ctx <- askCtx
    case (reverse (AST.tableOptionName opt), AST.tableOptionValue opt) of
        ([L _ (AST.NamePart raw False)], _)
            | asciiEqCI "INHERITS" raw -> pure ()
        (L nameSp (AST.NamePart raw False) : _, Just (L valSp (AST.ExprTuple [])))
            | asciiEqCI "tuple" raw -> do
                let (ns, _) = absTuple ctx nameSp
                    (_, ve) = absTuple ctx valSp
                forM_ ["0", "1"] \repl ->
                    addExprEdit AggressiveExpr ns ve repl "simplify-function_literal"
        _ -> traverse_ visitE (AST.tableOptionValue opt)

visitOpaqueEngine :: (Located AST.Expr -> EditM ()) -> AST.OpaqueBody -> EditM ()
visitOpaqueEngine visitE (AST.OpaqueBody engineSp AST.OpaqueEngineExpression) = do
    ctx <- askCtx
    let ByteSpan eStart eEnd = engineSp
        sigToks =
            V.filter
                (\t -> isSignificantToken t && spanStart (locSpan t) >= eStart && spanEnd (locSpan t) <= eEnd)
                (ctxUnitTokens ctx)
    case findOrderByTail sigToks of
        Just tailToks
            | not (V.null tailToks) ->
                let profile = dialectProfile (ctxDialect ctx)
                    !endOff = spanEnd (locSpan (V.last tailToks))
                    parser = case locValue (V.head tailToks) of
                        TokLParen -> parseParenList parseExpr
                        _ -> (: []) <$> parseExpr
                 in case runParser profile tailToks endOff parseQuery parseStatement parser of
                        Right exprs -> mapM_ visitE exprs
                        Left _ -> visitOpaqueEngineTuple engineSp
        _ -> visitOpaqueEngineTuple engineSp
    where
        findOrderByTail :: Vector SpannedToken -> Maybe (Vector SpannedToken)
        findOrderByTail toks = go 0 (0 :: Int)
            where
                !n = V.length toks
                go !i !depth
                    | i >= n = Nothing
                    | otherwise =
                        let tok = V.unsafeIndex toks i
                         in case locValue tok of
                                TokLParen -> go (i + 1) (depth + 1)
                                TokRParen -> go (i + 1) (max 0 (depth - 1))
                                _
                                    | depth == 0 && isWordCI "ORDER" tok && i + 1 < n && isWordCI "BY" (V.unsafeIndex toks (i + 1)) ->
                                        Just (V.drop (i + 2) toks)
                                    | otherwise -> go (i + 1) depth
visitOpaqueEngine _ _ = pure ()

visitOpaqueEngineTuple :: ByteSpan -> EditM ()
visitOpaqueEngineTuple engineSp = do
    ctx <- askCtx
    ti <- askTokens
    let (eStart, eEnd) = absTuple ctx engineSp
        toks = tiTokens ti
        !startIdx = partitionPoint (\t -> spanStart (locSpan t) < eStart) toks
        go !idx = case nextSignificant ti idx eEnd of
            Nothing -> pure ()
            Just (tIdx, L wSp (TokWord wt))
                | Nothing <- wordQuote wt
                , asciiEqCI "tuple" (wordRaw wt) ->
                    case nextSignificant ti (tIdx + 1) eEnd of
                        Just (lpIdx, L _ TokLParen) -> case nextSignificant ti (lpIdx + 1) eEnd of
                            Just (rpIdx, L rpSp TokRParen) -> do
                                forM_ ["0", "1"] \repl ->
                                    addExprEdit AggressiveExpr (spanStart wSp) (spanEnd rpSp) repl "simplify-function_literal"
                                go (rpIdx + 1)
                            _ -> go (tIdx + 1)
                        _ -> go (tIdx + 1)
            Just (tIdx, _) -> go (tIdx + 1)
    go startIdx

visitSqliteTriggerBody :: (Located AST.Statement -> EditM ()) -> AST.OpaqueBody -> EditM ()
visitSqliteTriggerBody visitStmt (AST.OpaqueBody bodySp AST.OpaqueTriggerBody) = do
    ctx <- askCtx
    let ByteSpan bStart bEnd = bodySp
        sigToks =
            V.filter
                (\t -> isSignificantToken t && spanStart (locSpan t) >= bStart && spanEnd (locSpan t) <= bEnd)
                (ctxUnitTokens ctx)
        !n = V.length sigToks
    when (n >= 2 && isWordCI "BEGIN" (V.head sigToks) && isWordCI "END" (V.last sigToks)) do
        let innerToks = V.slice 1 (n - 2) sigToks
            profile = dialectProfile SQLite
        forM_ (splitTopLevelSemicolons innerToks) \stmtToks ->
            unless (V.null stmtToks) do
                let !endOff = spanEnd (locSpan (V.last stmtToks))
                case runParser profile stmtToks endOff parseQuery parseStatement parseStatement of
                    Right stmt -> visitStmt stmt
                    Left _ -> pure ()
    where
        splitTopLevelSemicolons :: Vector SpannedToken -> [Vector SpannedToken]
        splitTopLevelSemicolons toks = go 0 0 (0 :: Int) []
            where
                !len = V.length toks
                go !start !i !depth acc
                    | i >= len =
                        let final = if start < len then V.slice start (len - start) toks : acc else acc
                         in reverse final
                    | otherwise = case locValue (V.unsafeIndex toks i) of
                        TokLParen -> go start (i + 1) (depth + 1) acc
                        TokRParen -> go start (i + 1) (max 0 (depth - 1)) acc
                        TokSemicolon
                            | depth == 0 ->
                                let chunk = if start < i then V.slice start (i - start) toks : acc else acc
                                 in go (i + 1) (i + 1) 0 chunk
                        _ -> go start (i + 1) depth acc
visitSqliteTriggerBody _ _ = pure ()

visitOpaqueBodyLiteral :: AST.OpaqueBody -> EditM ()
visitOpaqueBodyLiteral (AST.OpaqueBody bodySp AST.OpaqueFunctionBody) = do
    ctx <- askCtx
    let ByteSpan bStart bEnd = bodySp
        sigToks =
            [ t
            | t <- V.toList (ctxUnitTokens ctx)
            , isSignificantToken t
            , spanStart (locSpan t) >= bStart
            , spanEnd (locSpan t) <= bEnd
            ]
    case sigToks of
        [L litSp (TokLiteral kind raw)] ->
            addScalarEdit litSp (kind == LitString || BS.isPrefixOf "'" raw)
        _ -> pure ()
visitOpaqueBodyLiteral _ = pure ()

-- | Traverse an 'AST.AlterStmt', emitting column-def edits for @ADD COLUMN@ and visiting nested nodes.
visitAlterStmt :: (Located AST.Statement -> EditM ()) -> AST.AlterStmt -> EditM ()
visitAlterStmt visitStmt alter = do
    let visitQ = visitQuery visitStmt False
        visitE = visitExpr visitQ
    when (AST.alterObject alter == AST.AlterObjectTable) $
        forM_ (AST.alterActions alter) \case
            L _ (AST.AlterAddColumn _ cols _) -> mapM_ (addColumnDefEdits . locValue) cols
            _ -> pure ()
    forM_ (AST.alterActions alter) \(L _ action) -> case action of
        AST.AlterAddColumn _ cols _ -> mapM_ (visitColumnExprs visitE . locValue) cols
        AST.AlterAddConstraint (L _ tc) -> visitTableConstraintExprs visitE tc
        AST.AlterAddIndex _ keys -> mapM_ (visitIndexKeyExprs visitE) keys
        AST.AlterModifyColumn cols _ -> mapM_ (visitColumnExprs visitE . locValue) cols
        AST.AlterChangeColumn _ (L _ col) _ -> visitColumnExprs visitE col
        AST.AlterAlterColumn _ colAct -> case colAct of
            AST.AlterSetDefault e -> visitE e
            AST.AlterType _ mUsing -> traverse_ visitE mUsing
            AST.AlterAddIdentity _ opts ->
                forM_ opts (visitTableOption visitE . locValue)
            AST.AlterColumnOptions _ -> pure ()
            _ -> pure ()
        AST.AlterTableOption (L _ opt) -> visitTableOption visitE opt
        AST.AlterModifySetting items -> forM_ items (visitE . AST.settingValue . locValue)
        AST.AlterAttachPartition _ bound -> visitPartitionBound visitE bound
        AST.AlterUpdate assigns parts whereExpr -> do
            forM_ assigns (visitE . AST.assignValue . locValue)
            mapM_ visitE parts
            visitE whereExpr
        AST.AlterDelete parts whereExpr -> do
            mapM_ visitE parts
            visitE whereExpr
        AST.AlterViewSetOptions items -> forM_ items (visitE . AST.settingValue . locValue)
        AST.AlterViewAsQuery q -> visitQ q
        _ -> pure ()

visitColumnExprs :: (Located AST.Expr -> EditM ()) -> AST.ColumnDef -> EditM ()
visitColumnExprs visitE col = do
    forM_ (AST.columnConstraints col) (visitColConstraint . locValue)
    forM_ (AST.columnSettings col) (visitE . AST.settingValue . locValue)
    where
        visitColConstraint = \case
            AST.ColumnConstraintNamed _ inner -> visitColConstraint inner
            AST.ColumnConstraintDefault e -> visitE e
            AST.ColumnConstraintCheck e -> visitE e
            AST.ColumnConstraintGenerated _ e -> visitE e
            AST.ColumnConstraintOnConflict inner _ -> visitColConstraint inner
            AST.ColumnConstraintOnUpdate e -> visitE e
            AST.ColumnConstraintComment e -> visitE e
            AST.ColumnConstraintEphemeral mE -> traverse_ visitE mE
            AST.ColumnConstraintIdentity _ opts ->
                forM_ opts (visitTableOption visitE . locValue)
            _ -> pure ()

visitTableConstraintExprs :: (Located AST.Expr -> EditM ()) -> AST.TableConstraint -> EditM ()
visitTableConstraintExprs visitE = \case
    AST.TableConstraintPrimaryKey _ keys -> mapM_ (visitIndexKeyExprs visitE) keys
    AST.TableConstraintUnique _ keys -> mapM_ (visitIndexKeyExprs visitE) keys
    AST.TableConstraintIndex _ keys -> mapM_ (visitIndexKeyExprs visitE) keys
    AST.TableConstraintCheck _ e -> visitE e
    AST.TableConstraintForeignKey {} -> pure ()
    AST.TableConstraintUsingIndex {} -> pure ()
    AST.TableConstraintLike {} -> pure ()

visitIndexKeyExprs :: (Located AST.Expr -> EditM ()) -> Located AST.IndexKeyPart -> EditM ()
visitIndexKeyExprs visitE (L _ kp) = do
    visitE (AST.indexKeyExpr kp)
    traverse_ visitE (AST.indexKeyPrefixLength kp)

visitPartitionBound :: (Located AST.Expr -> EditM ()) -> AST.PartitionBound -> EditM ()
visitPartitionBound visitE = \case
    AST.PartitionBoundDefault -> pure ()
    AST.PartitionBoundIn exprs -> mapM_ visitE exprs
    AST.PartitionBoundFromTo froms tos -> mapM_ visitE froms >> mapM_ visitE tos
    AST.PartitionBoundWith items -> forM_ items (visitE . snd)
