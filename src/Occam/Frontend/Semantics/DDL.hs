-- | Semantic fact extraction for DDL statements (CREATE, DROP, ALTER, RENAME, ANALYZE).
module Occam.Frontend.Semantics.DDL (
    implicitCommit,
    indexSchemaPlacement,
    indexRef,
    triggerRef,
    qualifyRename,
    referentialAction,
    foreignKey,
    constraintFromTable,
    columnDefinition,
    tableDefinition,
    createTableFacts,
    createViewFacts,
    createIndexFacts,
    createTriggerFacts,
    createSequenceFacts,
    createSchemaFacts,
    createDatabaseFacts,
    createMacroFacts,
    createVirtualTableFacts,
    createRuleFacts,
    createStatisticsFacts,
    dropFacts,
    alterFacts,
    renameTableFacts,
    analyzeFacts,
) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Occam.Core qualified as Core
import Occam.Frontend.AST qualified as AST
import Occam.Frontend.Context (
    AnalysisContext (..),
    IdentifierClass (..),
    canonicalIdent,
    definitionName,
    nameFromAST,
    nameParts,
    relationRef,
    sliceSource,
    unitSource,
    unquoteNamePart,
    unsafeSliceSource,
 )
import Occam.Frontend.Lexer (
    LexResult (..),
    Token (..),
    WordToken (..),
    asciiEqCI,
    isTriviaToken,
    lexSQL,
 )
import Occam.Frontend.Parser (ParsedScript (..), parseScript)
import Occam.Frontend.Semantics.Expr (exprColumns)
import Occam.Frontend.Semantics.Pure (indexColumnIdent)
import Occam.Frontend.Semantics.Query (
    VisitorState (..),
    addUnmodeledBarriers,
    analyzeNested,
    extractViewLineage,
    mergeNested,
    queryOutputColumns,
    runAnalysisVisitor,
    visitorReads,
 )
import Occam.Frontend.Span (ByteSpan (..), Located (..), spanEnd, spanStart)
import Occam.Parser.Dialect (DialectProfile (..), SemanticsProfile (..))
import Occam.Parser.Types (SQLDialect (..))

-- | True when @stmt@ forces an implicit commit in MySQL/MariaDB.
implicitCommit :: AnalysisContext -> AST.Statement -> Bool
implicitCommit ctx stmt
    | not (semImplicitDdlCommit (dialectSemantics (ctxProfile ctx))) = False
    | otherwise = case stmt of
        AST.StatementCreate (L _ cr) -> case locValue (AST.createObject cr) of
            AST.CreateTableObject _ -> not (AST.createTemp cr)
            _ -> True
        AST.StatementDrop (L _ dr) -> not (AST.dropTemp dr)
        AST.StatementAlter _ -> True
        AST.StatementRenameTable _ -> True
        AST.StatementTruncate _ -> True
        AST.StatementGrant _ -> True
        AST.StatementLockTables _ -> True
        AST.StatementUnlockTables -> True
        AST.StatementAnalyze _ -> True
        AST.StatementOptimize _ -> True
        _ -> False

indexSchemaPlacement :: AnalysisContext -> Core.IndexSchemaPlacement
indexSchemaPlacement _ = Core.IndexTableSchema

indexRef :: AnalysisContext -> Core.Name -> Maybe Core.NameRef -> Core.IndexRef
indexRef ctx name mTable
    | ctxDialect ctx `elem` [MySQL, MariaDB]
    , Just table <- mTable
    , Just lastPart <- listToMaybe (reverse (Core.nameParts name)) =
        Core.TableIndex table lastPart
    | otherwise = Core.NamedIndex name

triggerRef :: AnalysisContext -> Core.Name -> Core.NameRef -> Core.TriggerRef
triggerRef ctx name table
    | ctxDialect ctx == PostgreSQL
    , Just lastPart <- listToMaybe (reverse (Core.nameParts name)) =
        Core.TableTrigger table lastPart
    | otherwise = Core.NamedTrigger name

qualifyRename :: Core.Name -> Core.Name -> Core.Name
qualifyRename old new
    | length (Core.nameParts new) == 1 && length (Core.nameParts old) > 1 =
        Core.Name (init (Core.nameParts old) ++ Core.nameParts new)
    | otherwise = new

referentialAction :: Maybe (Located AST.ForeignKeyAction) -> Core.ReferentialAction
referentialAction Nothing = Core.NoAction
referentialAction (Just (L _ act)) = case AST.foreignKeyActionKind act of
    AST.ForeignKeyCascade -> Core.Cascade
    AST.ForeignKeySetNull -> Core.SetNull
    AST.ForeignKeySetDefault -> Core.SetDefault
    AST.ForeignKeyRestrict -> Core.Restrict
    AST.ForeignKeyNoAction -> Core.NoAction

foreignKey :: AnalysisContext -> AST.ForeignKeyRef -> [Core.Ident] -> Maybe Core.ForeignKey
foreignKey ctx fk childCols = do
    parent <- relationRef ctx (locValue (AST.foreignKeyTable fk))
    let parentCols = map (canonicalIdent ctx IdentColumn . locValue) (AST.foreignKeyColumns fk)
    pure
        Core.ForeignKey
            { Core.foreignKeyName = Nothing
            , Core.foreignKeyChildColumns = childCols
            , Core.foreignKeyParent = parent
            , Core.foreignKeyParentColumns = parentCols
            , Core.foreignKeyOnDelete = referentialAction (AST.foreignKeyOnDelete fk)
            , Core.foreignKeyOnUpdate = referentialAction (AST.foreignKeyOnUpdate fk)
            , Core.foreignKeyNotValid = False
            , Core.foreignKeyDeferrable = fromMaybe False (AST.foreignKeyDeferrable fk)
            }

constraintFromTable :: AnalysisContext -> AST.TableConstraint -> Maybe Core.Constraint
constraintFromTable ctx = \case
    AST.TableConstraintPrimaryKey opts keys ->
        Just $
            Core.PrimaryKeyConstraint
                (optName opts)
                (mapMaybe (indexColumnIdent ctx . locValue) keys)
    AST.TableConstraintUnique opts keys ->
        Just $
            Core.UniqueConstraint
                (optName opts)
                (mapMaybe (indexColumnIdent ctx . locValue) keys)
    AST.TableConstraintForeignKey opts cols (L _ fkRef) -> do
        let childCols = map (canonicalIdent ctx IdentColumn . locValue) cols
        fk <- foreignKey ctx fkRef childCols
        let fk' = fk {Core.foreignKeyName = optName opts}
        Just (Core.ForeignKeyConstraint fk')
    AST.TableConstraintCheck mName (L _ expr) ->
        Just $
            Core.CheckConstraint
                (fmap (canonicalIdent ctx IdentOther . locValue) mName)
                (exprColumns ctx expr)
                Core.ConstraintValid
    AST.TableConstraintIndex opts keys ->
        Just $
            Core.IndexConstraint
                ( fmap
                    (canonicalIdent ctx IdentOther . locValue)
                    (AST.constraintName opts <|> AST.constraintIndexName opts)
                )
                (mapMaybe (indexColumnIdent ctx . locValue) keys)
                Core.IndexNonUnique
    AST.TableConstraintUsingIndex mName isPrimary (L _ idxPart) ->
        Just $
            Core.UsingIndexConstraint
                (fmap (canonicalIdent ctx IdentOther . locValue) mName)
                (canonicalIdent ctx IdentOther idxPart)
                (if isPrimary then Core.PrimaryIndex else Core.SecondaryIndex)
                (indexSchemaPlacement ctx)
    AST.TableConstraintLike _ _ -> Nothing
    where
        optName opts = fmap (canonicalIdent ctx IdentOther . locValue) (AST.constraintName opts)

columnDefinition :: AnalysisContext -> AST.ColumnDef -> (Core.ColumnDef, [Core.Constraint])
columnDefinition ctx col =
    let colName = canonicalIdent ctx IdentColumn (locValue (AST.columnName col))
        (notNull, hasDefault, generated, refs, cons) =
            foldl' (step colName Nothing) (False, False, False, [], []) (AST.columnConstraints col)
     in ( Core.ColumnDef
            { Core.columnName = colName
            , Core.columnNotNull = notNull
            , Core.columnHasDefault = hasDefault
            , Core.columnGenerated = generated
            , Core.columnReferences = reverse refs
            }
        , reverse cons
        )
    where
        step colName mConstraintName acc@(nn, hd, gen, refs, cons) (L _ cc) = case cc of
            AST.ColumnConstraintNamed (L _ namePart) inner ->
                step colName (Just (canonicalIdent ctx IdentOther namePart)) acc (L (ByteSpan 0 0) inner)
            AST.ColumnConstraintOnConflict inner _ ->
                step colName mConstraintName acc (L (ByteSpan 0 0) inner)
            AST.ColumnConstraintNotNull -> (True, hd, gen, refs, cons)
            AST.ColumnConstraintDefault _ -> (nn, True, gen, refs, cons)
            AST.ColumnConstraintGenerated _ _ -> (nn, True, True, refs, cons)
            AST.ColumnConstraintIdentity _ _ -> (nn, True, True, refs, cons)
            AST.ColumnConstraintEphemeral mDef -> (nn, hd || isJust mDef, gen, refs, cons)
            AST.ColumnConstraintPrimaryKey _ ->
                (True, hd, gen, refs, Core.PrimaryKeyConstraint mConstraintName [colName] : cons)
            AST.ColumnConstraintKey ->
                (True, hd, gen, refs, Core.PrimaryKeyConstraint mConstraintName [colName] : cons)
            AST.ColumnConstraintUnique ->
                (nn, hd, gen, refs, Core.UniqueConstraint mConstraintName [colName] : cons)
            AST.ColumnConstraintReferences (L _ fkRef) ->
                case foreignKey ctx fkRef [colName] of
                    Just fk ->
                        let fk' = fk {Core.foreignKeyName = mConstraintName}
                         in (nn, hd, gen, fk' : refs, cons)
                    Nothing -> acc
            AST.ColumnConstraintCheck (L _ expr) ->
                ( nn
                , hd
                , gen
                , refs
                , Core.CheckConstraint mConstraintName (exprColumns ctx expr) Core.ConstraintValid : cons
                )
            _ -> acc

tableDefinition ::
    AnalysisContext ->
    AST.CreateStmt ->
    AST.CreateTable ->
    [Core.ReadFact] ->
    Maybe Core.NameRef ->
    Maybe Core.NameRef ->
    [Core.NameRef] ->
    Maybe Core.TableDef
tableDefinition ctx createStmt ct sources like partitionParent inheritParents = do
    (name, temporary) <-
        definitionName ctx (AST.createTemp createStmt) (locValue (AST.createTableName ct))
    let (revCols, revCons) = foldl' stepElem ([], []) (AST.createTableElements ct)
        cols0 = reverse revCols
        cons = reverse revCons
        cols =
            if null cols0
                then case AST.createTableAs ct of
                    Just (L _ q) -> case queryOutputColumns ctx q of
                        Just out -> [Core.ColumnDef c False False False [] | c <- out]
                        Nothing -> []
                    Nothing -> []
                else cols0
        transactional = semTableTransactional (dialectSemantics (ctxProfile ctx)) temporary
    pure
        Core.TableDef
            { Core.tableName = name
            , Core.tableTemporary = temporary
            , Core.tableTransactional = transactional
            , Core.tableColumns = cols
            , Core.tableConstraints = cons
            , Core.tableSources = sources
            , Core.tableLike = like
            , Core.tablePartitionParent = partitionParent
            , Core.tableInheritParents = inheritParents
            , Core.tableOnCommit = parseOnCommit (AST.createTableOptions ct)
            , Core.tableIfNotExists = AST.createTableIfNotExists ct
            , Core.tableOrReplace = AST.createOrReplace createStmt
            }
    where
        stepElem (colsAcc, consAcc) (L _ el) = case el of
            AST.TableElementColumn (L _ cdef) ->
                let (c, ccons) = columnDefinition ctx cdef
                 in (c : colsAcc, reverse ccons ++ consAcc)
            AST.TableElementConstraint (L _ tc) ->
                case constraintFromTable ctx tc of
                    Just c -> (colsAcc, c : consAcc)
                    Nothing -> (colsAcc, consAcc)
            _ -> (colsAcc, consAcc)

parseOnCommit :: [Located AST.TableOption] -> Maybe Core.OnCommitAction
parseOnCommit opts = go (concatMap optWords opts)
    where
        optWords (L _ opt) = map (T.toUpper . unquoteNamePart . locValue) (AST.tableOptionName opt)
        go ("ON" : "COMMIT" : "DELETE" : "ROWS" : _) = Just Core.OnCommitDeleteRows
        go ("ON" : "COMMIT" : "DROP" : _) = Just Core.OnCommitDrop
        go (_ : rest) = go rest
        go [] = Nothing

createTableFacts ::
    (AnalysisContext -> AST.Statement -> Core.SemanticFacts) ->
    AnalysisContext ->
    VisitorState ->
    AST.CreateStmt ->
    AST.CreateTable ->
    Core.SemanticFacts ->
    Core.SemanticFacts
createTableFacts analyzeStmt ctx visitor createStmt ct baseFacts =
    let (partParent, unmodeledPart) = case AST.createTablePartitionOf ct of
            Nothing -> (Nothing, False)
            Just (L _ tp) -> case relationRef ctx (locValue (AST.tablePartitionParent tp)) of
                Just ref -> (Just ref, False)
                Nothing -> (Nothing, True)
        (inheritParents, unmodeledInherit) = extractInheritParents (AST.createTableOptions ct)
        unmodeledParent = unmodeledPart || unmodeledInherit
        parentBarriers =
            [ Core.ScopedBarrier
                Core.GlobalScope
                Core.UnknownDDLBarrier
                (Just "unmodeled inherited or partitioned table effects")
            | unmodeledParent
            ]
        withData = AST.createTableWithData ct /= Just False
        nested =
            if withData
                then maybe [] (analyzeNested analyzeStmt ctx False . AST.StatementQuery) (AST.createTableAs ct)
                else []
        excluded = [locSpan (AST.createTableName ct)]
        sourceMode = if withData then Core.ReadData else Core.ReadSchema
        sources0 = visitorReads visitor excluded sourceMode
        likeRef = listToMaybe $ mapMaybe extractLike (AST.createTableElements ct)
        parentSources =
            [ Core.ReadFact ref Core.ReadSchema [] False
            | ref <- maybeToList partParent ++ inheritParents
            ]
        sources = sources0 ++ parentSources
        (ddlOps, ddlBarriers) = case tableDefinition ctx createStmt ct sources likeRef partParent inheritParents of
            Just tbl ->
                let fkConstraintReads =
                        [ Core.ReadOp
                            (Core.ReadFact (Core.foreignKeyParent fk) Core.ReadSchema (Core.foreignKeyParentColumns fk) False)
                        | Core.ForeignKeyConstraint fk <- Core.tableConstraints tbl
                        ]
                    fkColReads =
                        [ Core.ReadOp
                            (Core.ReadFact (Core.foreignKeyParent fk) Core.ReadSchema (Core.foreignKeyParentColumns fk) False)
                        | col <- Core.tableColumns tbl
                        , fk <- Core.columnReferences col
                        ]
                 in (fkConstraintReads ++ fkColReads ++ [Core.DefineTableOp tbl], [])
            Nothing ->
                ( []
                ,
                    [ Core.ScopedBarrier
                        Core.GlobalScope
                        Core.UnknownDDLBarrier
                        (Just (TE.decodeUtf8 (unsafeSliceSource ctx (locSpan (AST.createTableName ct)))))
                    ]
                )
        barriers' =
            addUnmodeledBarriers visitor excluded (Core.factBarriers baseFacts ++ parentBarriers ++ ddlBarriers)
        facts1 =
            baseFacts
                { Core.factOps = Core.factOps baseFacts ++ ddlOps
                , Core.factBarriers = barriers'
                }
     in mergeNested facts1 nested
    where
        extractLike (L _ (AST.TableElementConstraint (L _ (AST.TableConstraintLike (L _ likeName) _)))) =
            relationRef ctx likeName
        extractLike _ = Nothing

        extractInheritParents opts =
            let exprs =
                    [ e
                    | L _ opt <- opts
                    , map (T.toUpper . unquoteNamePart . locValue) (AST.tableOptionName opt) == ["INHERITS"]
                    , Just (L _ val) <- [AST.tableOptionValue opt]
                    , e <- unwrapExprList val
                    ]
             in foldr step ([], False) exprs
            where
                unwrapExprList (AST.ExprTuple es) = map locValue es
                unwrapExprList (AST.ExprParen (L _ e)) = [e]
                unwrapExprList e = [e]
                step (AST.ExprColumn (L _ name)) (acc, failed) = case relationRef ctx name of
                    Just ref -> (ref : acc, failed)
                    Nothing -> (acc, True)
                step _ (acc, _) = (acc, True)

createViewFacts ::
    (AnalysisContext -> AST.Statement -> Core.SemanticFacts) ->
    AnalysisContext ->
    VisitorState ->
    AST.CreateStmt ->
    AST.CreateView ->
    Core.SemanticFacts ->
    Core.SemanticFacts
createViewFacts analyzeStmt ctx visitor createStmt cv baseFacts =
    let excluded = [locSpan (AST.createViewName cv)]
        materialized = AST.createViewMaterialized cv
        sourceMode = if materialized then Core.ReadData else Core.ReadSchema
        nested = case AST.createViewBody cv of
            AST.ViewBodyQuery q -> analyzeNested analyzeStmt ctx False (AST.StatementQuery q)
            AST.ViewBodyOpaque _ -> []
        nonMatCteBarriers =
            [ Core.ScopedBarrier
                Core.GlobalScope
                Core.UnknownViewBarrier
                (Just "data-modifying CTE in a non-materialized view")
            | not materialized && not (null nested)
            ]
        sources = visitorReads visitor excluded sourceMode
        (ops', defBarriers) = case definitionName ctx (AST.createTemp createStmt) (locValue (AST.createViewName cv)) of
            Just (name, temporary) ->
                let explicitCols =
                        if null (AST.createViewColumns cv)
                            then Nothing
                            else Just (map (canonicalIdent ctx IdentColumn . locValue) (AST.createViewColumns cv))
                    (cols, lineage, condCols, opaqueBarriers) = case AST.createViewBody cv of
                        AST.ViewBodyQuery (L _ q) ->
                            let c = fromMaybe (fromMaybe [] (queryOutputColumns ctx q)) explicitCols
                                (lin, cond) = extractViewLineage ctx q explicitCols visitor excluded
                             in (c, lin, cond, [])
                        AST.ViewBodyOpaque _ ->
                            ( fromMaybe [] explicitCols
                            , []
                            , []
                            ,
                                [ Core.ScopedBarrier Core.GlobalScope Core.UnknownViewBarrier (Just (TE.decodeUtf8 (unitSource ctx)))
                                ]
                            )
                    viewDef =
                        Core.ViewDef
                            { Core.viewName = name
                            , Core.viewTemporary = temporary
                            , Core.viewMaterialized = materialized
                            , Core.viewColumns = cols
                            , Core.viewSources = sources
                            , Core.viewIfNotExists = AST.createViewIfNotExists cv
                            , Core.viewOrReplace = AST.createOrReplace createStmt
                            , Core.viewColumnLineage = lineage
                            , Core.viewConditionColumns = condCols
                            }
                    toBarrier =
                        [ Core.ScopedBarrier
                            Core.GlobalScope
                            Core.WriteThroughViewBarrier
                            (Just "unmodeled persistent CREATE VIEW ... TO routing")
                        | isJust (AST.createViewTo cv)
                        ]
                 in ([Core.DefineViewOp viewDef], opaqueBarriers ++ toBarrier)
            Nothing ->
                ( []
                ,
                    [ Core.ScopedBarrier
                        Core.GlobalScope
                        Core.UnknownViewBarrier
                        (Just (TE.decodeUtf8 (unsafeSliceSource ctx (locSpan (AST.createViewName cv)))))
                    ]
                )
        barriers' =
            addUnmodeledBarriers
                visitor
                excluded
                (Core.factBarriers baseFacts ++ nonMatCteBarriers ++ defBarriers)
        facts1 =
            baseFacts
                { Core.factOps = Core.factOps baseFacts ++ ops'
                , Core.factBarriers = barriers'
                }
     in if materialized then mergeNested facts1 nested else facts1

createIndexFacts ::
    AnalysisContext ->
    VisitorState ->
    AST.CreateStmt ->
    AST.CreateIndex ->
    Core.SemanticFacts ->
    Core.SemanticFacts
createIndexFacts ctx visitor createStmt ci baseFacts =
    let mTable = relationRef ctx (locValue (AST.createIndexTable ci))
        mIndexName = AST.createIndexName ci >>= nameFromAST ctx IdentOther . locValue
        (ops', idxBarriers) = case (mIndexName, mTable) of
            (Just idxName, Just tbl) ->
                let cols = mapMaybe (indexColumnIdent ctx . locValue) (AST.createIndexKeys ci)
                    hasExpr = any (isNothing . indexColumnIdent ctx . locValue) (AST.createIndexKeys ci)
                    idxDef =
                        Core.IndexDef
                            { Core.indexRef = indexRef ctx idxName (Just tbl)
                            , Core.indexTable = tbl
                            , Core.indexColumns = cols
                            , Core.indexUnique = AST.createUnique createStmt
                            , Core.indexPredicate = isJust (AST.createIndexWhere ci)
                            , Core.indexExpression = hasExpr
                            , Core.indexIfNotExists = False
                            , Core.indexSchemaPlacement = indexSchemaPlacement ctx
                            }
                 in ([Core.DefineIndexOp idxDef], [])
            _ ->
                ( []
                ,
                    [ Core.ScopedBarrier Core.GlobalScope Core.UnknownIndexBarrier (Just (TE.decodeUtf8 (unitSource ctx)))
                    ]
                )
        barriers' =
            addUnmodeledBarriers
                visitor
                [locSpan (AST.createIndexTable ci)]
                (Core.factBarriers baseFacts ++ idxBarriers)
     in baseFacts {Core.factOps = Core.factOps baseFacts ++ ops', Core.factBarriers = barriers'}

createTriggerFacts ::
    (AnalysisContext -> AST.Statement -> Core.SemanticFacts) ->
    AnalysisContext ->
    AST.CreateStmt ->
    AST.CreateTrigger ->
    Core.SemanticFacts ->
    Core.SemanticFacts
createTriggerFacts analyzeStmt ctx createStmt tr baseFacts =
    let mTable = relationRef ctx (locValue (AST.createTriggerTable tr))
        mTriggerName = nameFromAST ctx IdentOther (locValue (AST.createTriggerName tr))
     in case (mTriggerName, mTable) of
            (Just trgName, Just tbl) ->
                let trg = triggerRef ctx trgName tbl
                    timing = case fmap (T.toUpper . TE.decodeUtf8 . locValue) (AST.createTriggerTiming tr) of
                        Just "AFTER" -> Core.TriggerAfter
                        Just "INSTEAD" -> Core.TriggerInsteadOf
                        _ -> Core.TriggerBefore
                    events = map (triggerEvent . locValue) (AST.createTriggerEvents tr)
                    forEachRow = case fmap locValue (AST.createTriggerGranularity tr) of
                        Just AST.TriggerForEachRow -> True
                        Just AST.TriggerForEachStatement -> False
                        Nothing -> semDefaultTriggerForEachRow (dialectSemantics (ctxProfile ctx))
                    whenReads = case AST.createTriggerWhen tr of
                        Nothing -> []
                        Just (L _ cond) ->
                            let st = runAnalysisVisitor ctx True [] [] Nothing (wrapCondQuery cond)
                             in visitorReads st [] Core.ReadData
                    body = triggerBody analyzeStmt ctx tr
                    trgDef =
                        Core.TriggerDef
                            { Core.triggerRef = trg
                            , Core.triggerTable = tbl
                            , Core.triggerTiming = timing
                            , Core.triggerEvents = events
                            , Core.triggerForEachRow = forEachRow
                            , Core.triggerWhenReads = whenReads
                            , Core.triggerBody = body
                            , Core.triggerReferencedTable = Nothing
                            , Core.triggerTemporary = AST.createTemp createStmt
                            , Core.triggerIfNotExists = False
                            , Core.triggerOrReplace = AST.createOrReplace createStmt
                            }
                 in baseFacts {Core.factOps = Core.factOps baseFacts ++ [Core.DefineTriggerOp trgDef]}
            _ ->
                baseFacts
                    { Core.factBarriers =
                        Core.factBarriers baseFacts
                            ++ [ Core.ScopedBarrier
                                    Core.GlobalScope
                                    Core.UnknownTriggerBarrier
                                    (Just (TE.decodeUtf8 (unsafeSliceSource ctx (locSpan (AST.createTriggerName tr)))))
                               ]
                    }
    where
        triggerEvent raw = case T.toUpper (TE.decodeUtf8 raw) of
            "INSERT" -> Core.TriggerInsert
            "UPDATE" -> Core.TriggerUpdate []
            "DELETE" -> Core.TriggerDelete
            "TRUNCATE" -> Core.TriggerTruncate
            _ -> Core.TriggerInsert
        wrapCondQuery cond =
            AST.StatementQuery
                ( L
                    (ByteSpan 0 0)
                    AST.Query
                        { AST.queryWith = Nothing
                        , AST.queryBody =
                            L
                                (ByteSpan 0 0)
                                ( AST.QueryBodySelect
                                    AST.SelectCore
                                        { AST.selectQuantifier = AST.SelectDefault
                                        , AST.selectItems = []
                                        , AST.selectInto = Nothing
                                        , AST.selectFrom = Nothing
                                        , AST.selectWhere = Just (L (ByteSpan 0 0) cond)
                                        , AST.selectPrewhere = Nothing
                                        , AST.selectGroupBy = []
                                        , AST.selectHaving = Nothing
                                        , AST.selectWindows = []
                                        , AST.selectQualify = Nothing
                                        }
                                )
                        , AST.queryOrderBy = []
                        , AST.queryLimit = Nothing
                        , AST.queryOffset = Nothing
                        , AST.queryFetch = Nothing
                        , AST.queryFor = Nothing
                        , AST.querySettings = Nothing
                        , AST.queryFormat = Nothing
                        }
                )

triggerBody ::
    (AnalysisContext -> AST.Statement -> Core.SemanticFacts) ->
    AnalysisContext ->
    AST.CreateTrigger ->
    Core.TriggerBody
triggerBody analyzeStmt ctx tr
    | Just (L _ fnName) <- AST.createTriggerExecute tr =
        case nameFromAST ctx IdentOther fnName of
            Just fn ->
                Core.TriggerFunction
                    fn
                    (map (TE.decodeUtf8 . unsafeSliceSource ctx . locSpan) (AST.createTriggerExecuteArgs tr))
            Nothing -> Core.TriggerOpaque
    | otherwise =
        let bodySpan = AST.opaqueBodySpan (AST.createTriggerBody tr)
            bodyBytes = fromMaybe BS.empty (sliceSource ctx bodySpan)
            interiorBytes = stripTriggerBlock (ctxProfile ctx) bodyBytes
            subCtx =
                ctx
                    { ctxSource = interiorBytes
                    , ctxUnitSpan = ByteSpan 0 (BS.length interiorBytes)
                    , ctxUnitOffset = 0
                    , ctxAstSpanShift = 0
                    , ctxUnitTokens = V.empty
                    , ctxColumnIR = False
                    , ctxSyntaxEdits = False
                    }
         in case parseScript (ctxProfile ctx) interiorBytes of
                Right (ParsedScript stmts) ->
                    Core.TriggerInline (concatMap (triggerEffectsFromFacts . analyzeStmt subCtx . locValue) stmts)
                Left _ -> Core.TriggerOpaque

-- | Strip outer @BEGIN ... END@ block tokens if present in a trigger body slice.
stripTriggerBlock :: DialectProfile -> ByteString -> ByteString
stripTriggerBlock profile bytes =
    let res = lexSQL profile bytes
     in case lexError res of
            Just _ -> bytes
            Nothing ->
                let toks = V.filter (not . isTriviaToken . locValue) (lexTokens res)
                    n = V.length toks
                 in if n >= 2
                        && isWord "BEGIN" (V.head toks)
                        && isWord "END" (V.last toks)
                        then
                            let start = spanEnd (locSpan (V.head toks))
                                end = spanStart (locSpan (V.last toks))
                             in if start <= end then BS.take (end - start) (BS.drop start bytes) else bytes
                        else bytes
    where
        isWord kw (L _ (TokWord wt)) = isNothing (wordQuote wt) && asciiEqCI kw (wordRaw wt)
        isWord _ _ = False

triggerEffectsFromFacts :: Core.SemanticFacts -> [Core.TriggerEffect]
triggerEffectsFromFacts facts =
    concatMap opEffects (Core.factOps facts)
        ++ map Core.TriggerSession (Core.factSession facts)
        ++ map Core.TriggerBarrier (Core.factBarriers facts)
        ++ [ Core.TriggerBarrier
                ( Core.ScopedBarrier
                    Core.GlobalScope
                    Core.UnsupportedTriggerBodyBarrier
                    (Just "transaction control in trigger body")
                )
           | not (null (Core.factPreTx facts)) || not (null (Core.factPostTx facts))
           ]
    where
        opEffects = \case
            Core.ReadOp r -> [Core.TriggerRead r]
            Core.WriteDataOp w ->
                let readEffs = map Core.TriggerRead (Core.dataWriteReads w)
                    dmlEv = case Core.dataWriteKind w of
                        Core.InsertWrite _ -> Core.DmlInsert (Core.dataWriteTarget w) (Core.dataWriteColumns w)
                        Core.UpdateWrite _ -> Core.DmlUpdate (Core.dataWriteTarget w) (Core.dataWriteColumns w)
                        Core.MergeWrite -> Core.DmlMerge (Core.dataWriteTarget w) (Core.dataWriteColumns w)
                        Core.DeleteWrite scope -> Core.DmlDelete (Core.dataWriteTarget w) scope
                        Core.TruncateWrite beh _ -> Core.DmlTruncate (Core.dataWriteTarget w) beh
                 in readEffs ++ [Core.TriggerDml dmlEv]
            Core.WriteSequenceOp sw -> [Core.TriggerWriteSequence sw]
            _ ->
                [ Core.TriggerBarrier
                    ( Core.ScopedBarrier
                        Core.GlobalScope
                        Core.UnsupportedTriggerBodyBarrier
                        (Just "DDL in trigger body")
                    )
                ]

createSequenceFacts ::
    AnalysisContext -> AST.CreateStmt -> AST.CreateSequence -> Core.SemanticFacts -> Core.SemanticFacts
createSequenceFacts ctx createStmt cs baseFacts =
    case definitionName ctx (AST.createTemp createStmt) (locValue (AST.createSequenceName cs)) of
        Just (name, temp) ->
            let seqDef =
                    Core.SequenceDef
                        { Core.sequenceName = name
                        , Core.sequenceTemporary = temp
                        , Core.sequenceTransactional = ctxDialect ctx `notElem` [MySQL, MariaDB, ClickHouse]
                        }
             in baseFacts {Core.factOps = Core.factOps baseFacts ++ [Core.DefineSequenceOp seqDef]}
        Nothing ->
            baseFacts
                { Core.factBarriers =
                    Core.factBarriers baseFacts
                        ++ [Core.ScopedBarrier Core.GlobalScope Core.UnknownDDLBarrier (Just (TE.decodeUtf8 (unitSource ctx)))]
                }

createSchemaFacts :: AnalysisContext -> AST.CreateSchema -> Core.SemanticFacts -> Core.SemanticFacts
createSchemaFacts ctx cs baseFacts =
    case AST.createSchemaName cs >>= nameFromAST ctx IdentRelation . locValue of
        Just name ->
            baseFacts {Core.factOps = Core.factOps baseFacts ++ [Core.DefineSchemaOp (Core.SchemaDef name)]}
        Nothing ->
            baseFacts
                { Core.factBarriers =
                    Core.factBarriers baseFacts
                        ++ [Core.ScopedBarrier Core.GlobalScope Core.UnknownDDLBarrier (Just (TE.decodeUtf8 (unitSource ctx)))]
                }

createDatabaseFacts ::
    AnalysisContext -> AST.CreateStmt -> AST.CreateDatabase -> Core.SemanticFacts -> Core.SemanticFacts
createDatabaseFacts ctx createStmt db baseFacts
    | ctxDialect ctx `elem` [MySQL, MariaDB, ClickHouse]
    , not (AST.createOrReplace createStmt)
    , null (AST.createDatabaseOptions db) =
        case nameFromAST ctx IdentRelation (locValue (AST.createDatabaseName db)) of
            Just name ->
                baseFacts {Core.factOps = Core.factOps baseFacts ++ [Core.DefineSchemaOp (Core.SchemaDef name)]}
            Nothing ->
                baseFacts
                    { Core.factBarriers =
                        Core.factBarriers baseFacts
                            ++ [Core.ScopedBarrier Core.GlobalScope Core.UnknownDDLBarrier (Just (TE.decodeUtf8 (unitSource ctx)))]
                    }
    | otherwise =
        baseFacts
            { Core.factBarriers =
                Core.factBarriers baseFacts
                    ++ [ Core.ScopedBarrier
                            Core.GlobalScope
                            Core.UnknownStatementBarrier
                            (Just (TE.decodeUtf8 (unitSource ctx)))
                       ]
            }

createMacroFacts ::
    AnalysisContext -> VisitorState -> AST.CreateMacro -> Core.SemanticFacts -> Core.SemanticFacts
createMacroFacts _ctx visitor cm baseFacts =
    let excluded = [locSpan (AST.createMacroName cm)]
        readFacts = visitorReads visitor excluded Core.ReadData
        barriers' = addUnmodeledBarriers visitor excluded (Core.factBarriers baseFacts)
     in baseFacts
            { Core.factOps = Core.factOps baseFacts ++ map Core.ReadOp readFacts
            , Core.factBarriers = barriers'
            }

createVirtualTableFacts ::
    AnalysisContext ->
    VisitorState ->
    AST.CreateVirtualTable ->
    Core.SemanticFacts ->
    Core.SemanticFacts
createVirtualTableFacts ctx visitor cvt baseFacts =
    let excluded = [locSpan (AST.createVirtualTableName cvt)]
        mDef = definitionName ctx False (locValue (AST.createVirtualTableName cvt))
        mRef = relationRef ctx (locValue (AST.createVirtualTableName cvt))
        modSuffixes = virtualTableShadowSuffixes (locValue (AST.createVirtualTableModule cvt))
        (ops', trgBarriers) = case (mDef, mRef) of
            (Just (tblName, temp), Just target) ->
                let txFlag = semTableTransactional (dialectSemantics (ctxProfile ctx)) temp
                    tbl =
                        Core.TableDef
                            { Core.tableName = tblName
                            , Core.tableTemporary = temp
                            , Core.tableTransactional = txFlag
                            , Core.tableColumns = []
                            , Core.tableConstraints = []
                            , Core.tableSources = []
                            , Core.tableLike = Nothing
                            , Core.tablePartitionParent = Nothing
                            , Core.tableInheritParents = []
                            , Core.tableOnCommit = Nothing
                            , Core.tableIfNotExists = AST.createVirtualTableIfNotExists cvt
                            , Core.tableOrReplace = False
                            }
                    shadowOps =
                        [ Core.DefineTableOp
                            Core.TableDef
                                { Core.tableName = shadowName
                                , Core.tableTemporary = temp
                                , Core.tableTransactional = txFlag
                                , Core.tableColumns = []
                                , Core.tableConstraints = []
                                , Core.tableSources = [Core.ReadFact target Core.ReadSchema [] False]
                                , Core.tableLike = Nothing
                                , Core.tablePartitionParent = Just target
                                , Core.tableInheritParents = []
                                , Core.tableOnCommit = Nothing
                                , Core.tableIfNotExists = AST.createVirtualTableIfNotExists cvt
                                , Core.tableOrReplace = False
                                }
                        | suffix <- modSuffixes
                        , Just shadowName <- [appendShadowSuffix tblName suffix]
                        ]
                 in ( Core.DefineTableOp tbl : shadowOps
                    ,
                        [ Core.ScopedBarrier
                            (Core.RelationScope target)
                            Core.UnknownStatementBarrier
                            (Just (TE.decodeUtf8 (unitSource ctx)))
                        ]
                    )
            _ ->
                ( []
                ,
                    [ Core.ScopedBarrier
                        Core.GlobalScope
                        Core.UnknownStatementBarrier
                        (Just (TE.decodeUtf8 (unitSource ctx)))
                    ]
                )
        barriers' = addUnmodeledBarriers visitor excluded (Core.factBarriers baseFacts ++ trgBarriers)
     in baseFacts {Core.factOps = Core.factOps baseFacts ++ ops', Core.factBarriers = barriers'}
    where
        virtualTableShadowSuffixes modName = case listToMaybe (reverse (AST.nameParts modName)) of
            Just (L _ part) -> case T.toLower (unquoteNamePart part) of
                "fts3" -> ["content", "segments", "segdir", "docsize", "stat"]
                "fts4" -> ["content", "segments", "segdir", "docsize", "stat"]
                "fts5" -> ["data", "idx", "content", "docsize", "config"]
                "rtree" -> ["rowid", "node", "parent"]
                "rtree_i32" -> ["rowid", "node", "parent"]
                "geopoly" -> ["rowid", "node", "parent"]
                _ -> []
            Nothing -> []
        appendShadowSuffix (Core.Name parts) suffix = case reverse parts of
            Core.Ident lastTxt : revPrefix ->
                Just (Core.Name (reverse (Core.Ident (lastTxt <> "_" <> suffix) : revPrefix)))
            [] -> Nothing

-- | Extract scoped semantic facts for PostgreSQL @CREATE [OR REPLACE] RULE@.
createRuleFacts ::
    (AnalysisContext -> AST.Statement -> Core.SemanticFacts) ->
    AnalysisContext ->
    VisitorState ->
    AST.CreateRule ->
    Core.SemanticFacts ->
    Core.SemanticFacts
createRuleFacts analyzeStmt ctx visitor crRule baseFacts =
    let L tblSpan tblName = AST.createRuleTable crRule
        rawTable = TE.decodeUtf8 (unsafeSliceSource ctx tblSpan)
        excluded = [tblSpan]
        whereCols = maybe [] (exprColumns ctx . locValue) (AST.createRuleWhere crRule)
     in case relationRef ctx tblName of
            Nothing ->
                baseFacts
                    { Core.factBarriers =
                        Core.factBarriers baseFacts
                            ++ [ Core.ScopedBarrier
                                    Core.GlobalScope
                                    Core.UnknownStatementBarrier
                                    (Just (TE.decodeUtf8 (unitSource ctx)))
                               ]
                    }
            Just target ->
                let targetRead = Core.ReadOp (Core.ReadFact target Core.ReadSchema whereCols False)
                    whereReads = map Core.ReadOp (visitorReads visitor excluded Core.ReadData)
                    targetBarrier =
                        Core.ScopedBarrier
                            (Core.RelationScope target)
                            Core.UnknownDDLBarrier
                            (Just ("CREATE RULE on " <> rawTable))
                    (actionOps, actionBarriers) = case AST.createRuleAction crRule of
                        Nothing -> ([], [])
                        Just (L _ (AST.StatementNotify _)) -> ([], [])
                        Just (L _ actStmt) ->
                            let af = analyzeStmt ctx actStmt
                                touchedRefs = concatMap opRelationRefs (Core.factOps af)
                                extraRefBarriers =
                                    [ Core.ScopedBarrier
                                        (Core.RelationScope r)
                                        Core.UnknownDDLBarrier
                                        (Just ("CREATE RULE action on " <> rawTable))
                                    | r <- dedupRefs (filter (/= target) touchedRefs)
                                    ]
                                extraReadOps =
                                    [ Core.ReadOp (Core.ReadFact r Core.ReadSchema [] False)
                                    | r <- dedupRefs (filter (/= target) touchedRefs)
                                    ]
                                propagatedBarriers =
                                    [ b
                                    | b <- Core.factBarriers af
                                    , case Core.barrierScope b of
                                        Core.RelationScope r -> r /= target
                                        _ -> True
                                    ]
                                txOrSessionBarrier =
                                    [ Core.ScopedBarrier
                                        Core.GlobalScope
                                        Core.UnknownDDLBarrier
                                        (Just ("CREATE RULE side-effect action on " <> rawTable))
                                    | not (null (Core.factSession af))
                                        || not (null (Core.factPreTx af))
                                        || not (null (Core.factPostTx af))
                                    ]
                             in (extraReadOps, extraRefBarriers ++ propagatedBarriers ++ txOrSessionBarrier)
                 in baseFacts
                        { Core.factOps = Core.factOps baseFacts ++ (targetRead : whereReads) ++ actionOps
                        , Core.factBarriers =
                            addUnmodeledBarriers
                                visitor
                                excluded
                                (Core.factBarriers baseFacts ++ [targetBarrier] ++ actionBarriers)
                        }
    where
        opRelationRefs = \case
            Core.ReadOp r -> [Core.readSource r]
            Core.WriteDataOp w -> Core.dataWriteTarget w : map Core.readSource (Core.dataWriteReads w)
            _ -> []
        dedupRefs =
            reverse
                . fst
                . foldl'
                    (\(acc, seen) r -> if Set.member r seen then (acc, seen) else (r : acc, Set.insert r seen))
                    ([], Set.empty)

createStatisticsFacts ::
    AnalysisContext -> AST.CreateStatistics -> Core.SemanticFacts -> Core.SemanticFacts
createStatisticsFacts ctx cs baseFacts = case AST.createStatisticsFrom cs of
    Just (L sp tblName) -> case relationRef ctx tblName of
        Just target ->
            let cols = concatMap (exprColumns ctx . locValue) (AST.createStatisticsColumns cs)
                dedupCols = reverse (fst (foldl' dedup ([], Set.empty) cols))
             in baseFacts
                    { Core.factOps =
                        Core.factOps baseFacts ++ [Core.ReadOp (Core.ReadFact target Core.ReadSchema dedupCols False)]
                    , Core.factBarriers =
                        Core.factBarriers baseFacts
                            ++ [ Core.ScopedBarrier
                                    (Core.RelationScope target)
                                    Core.UnknownDDLBarrier
                                    (Just ("CREATE STATISTICS on " <> TE.decodeUtf8 (unsafeSliceSource ctx sp)))
                               ]
                    }
        Nothing ->
            baseFacts
                { Core.factBarriers =
                    Core.factBarriers baseFacts
                        ++ [ Core.ScopedBarrier
                                Core.GlobalScope
                                Core.UnknownStatementBarrier
                                (Just (TE.decodeUtf8 (unitSource ctx)))
                           ]
                }
    Nothing ->
        baseFacts
            { Core.factBarriers =
                Core.factBarriers baseFacts
                    ++ [ Core.ScopedBarrier
                            Core.GlobalScope
                            Core.UnknownStatementBarrier
                            (Just (TE.decodeUtf8 (unitSource ctx)))
                       ]
            }
    where
        dedup (acc, seen) x
            | Set.member x seen = (acc, seen)
            | otherwise = (x : acc, Set.insert x seen)

dropFacts :: AnalysisContext -> AST.DropStmt -> Core.SemanticFacts -> Core.SemanticFacts
dropFacts ctx stmt baseFacts = case locValue (AST.dropObject stmt) of
    AST.DropTrigger ->
        let mTable = AST.dropOnTable stmt >>= relationRef ctx . locValue
            mTriggers =
                traverse
                    ( \n -> do
                        tName <- nameFromAST ctx IdentOther (locValue n)
                        case (AST.dropOnTable stmt, mTable) of
                            (Just _, Nothing) -> Nothing
                            (Just _, Just tbl) -> Just (Core.DropTrigger (triggerRef ctx tName tbl))
                            (Nothing, _) -> Just (Core.DropTrigger (Core.NamedTrigger tName))
                    )
                    (AST.dropNames stmt)
         in case mTriggers of
                Just objs
                    | not (null objs) ->
                        baseFacts
                            { Core.factOps = Core.factOps baseFacts ++ [Core.DropCatalogOp (Core.DropOp objs Core.DropRestrict)]
                            }
                _ ->
                    baseFacts
                        { Core.factBarriers =
                            Core.factBarriers baseFacts
                                ++ [ Core.ScopedBarrier Core.GlobalScope Core.UnknownDropBarrier (Just (TE.decodeUtf8 (unitSource ctx)))
                                   ]
                        }
    objKind
        | objKind
            `elem` [ AST.DropFunction
                   , AST.DropProcedure
                   , AST.DropMacro
                   , AST.DropAggregate
                   , AST.DropConversion
                   , AST.DropPolicy
                   , AST.DropRule
                   , AST.DropStatistics
                   ] ->
            baseFacts
                { Core.factBarriers =
                    Core.factBarriers baseFacts
                        ++ [ Core.ScopedBarrier
                                Core.GlobalScope
                                Core.UnknownStatementBarrier
                                (Just (TE.decodeUtf8 (unitSource ctx)))
                           ]
                }
    objKind ->
        let mTable = AST.dropOnTable stmt >>= relationRef ctx . locValue
            mObjects = traverse (dropObj mTable objKind . locValue) (AST.dropNames stmt)
            mode
                | AST.dropBehavior stmt == Just AST.DropCascade = Core.DropCascade
                | objKind `elem` [AST.DropSchema, AST.DropDatabase]
                    && ctxDialect ctx `elem` [MySQL, MariaDB, ClickHouse] =
                    Core.DropCascade
                | otherwise = Core.DropRestrict
         in case mObjects of
                Just objs
                    | not (null objs) ->
                        baseFacts {Core.factOps = Core.factOps baseFacts ++ [Core.DropCatalogOp (Core.DropOp objs mode)]}
                _ ->
                    baseFacts
                        { Core.factBarriers =
                            Core.factBarriers baseFacts
                                ++ [ Core.ScopedBarrier Core.GlobalScope Core.UnknownDropBarrier (Just (TE.decodeUtf8 (unitSource ctx)))
                                   ]
                        }
    where
        dropObj mTable kind name = case kind of
            AST.DropTable -> flip Core.DropRelation Core.TableRelation . markTemp <$> relationRef ctx name
            AST.DropView -> flip Core.DropRelation Core.ViewRelation . markTemp <$> relationRef ctx name
            AST.DropMaterializedView ->
                flip Core.DropRelation Core.MaterializedViewRelation . markTemp <$> relationRef ctx name
            AST.DropIndex -> do
                idxName <- nameFromAST ctx IdentOther name
                pure (Core.DropIndex (indexRef ctx idxName mTable))
            AST.DropSequence -> Core.DropSequence . markTemp <$> relationRef ctx name
            AST.DropSchema -> Core.DropSchema <$> nameFromAST ctx IdentRelation name
            AST.DropDatabase -> Core.DropSchema <$> nameFromAST ctx IdentRelation name
            _ -> Nothing
        markTemp ref = ref {Core.nameRefTemp = Core.nameRefTemp ref || AST.dropTemp stmt}

alterFacts ::
    AnalysisContext -> VisitorState -> AST.AlterStmt -> Core.SemanticFacts -> Core.SemanticFacts
alterFacts ctx visitor stmt baseFacts = case AST.alterObject stmt of
    AST.AlterObjectView -> alterViewFacts ctx visitor stmt baseFacts
    AST.AlterObjectTable -> case relationRef ctx (locValue (AST.alterTable stmt)) of
        Nothing ->
            baseFacts
                { Core.factBarriers =
                    Core.factBarriers baseFacts
                        ++ [ Core.ScopedBarrier
                                Core.GlobalScope
                                Core.UnknownAlterBarrier
                                (Just (TE.decodeUtf8 (unsafeSliceSource ctx (locSpan (AST.alterTable stmt)))))
                           ]
                }
        Just table ->
            let (changes, ops, barriers) = foldl' (stepAction table) ([], [], []) (AST.alterActions stmt)
                alterOp = [Core.AlterTableOp (Core.AlterTable table changes) | not (null changes)]
                hasMutation = any (isMutation . locValue) (AST.alterActions stmt)
                finalBarriers =
                    if hasMutation
                        then addUnmodeledBarriers visitor [] (Core.factBarriers baseFacts ++ barriers)
                        else Core.factBarriers baseFacts ++ barriers
             in baseFacts
                    { Core.factOps = Core.factOps baseFacts ++ alterOp ++ ops
                    , Core.factBarriers = finalBarriers
                    }
    where
        isMutation = \case
            AST.AlterUpdate {} -> True
            AST.AlterDelete {} -> True
            _ -> False
        stepAction table (chAcc, opAcc, bAcc) (L sp action) =
            let (chs, ops, bs) = alterChange ctx visitor table sp action
             in (chAcc ++ chs, opAcc ++ ops, bAcc ++ bs)

alterViewFacts ::
    AnalysisContext -> VisitorState -> AST.AlterStmt -> Core.SemanticFacts -> Core.SemanticFacts
alterViewFacts ctx visitor stmt baseFacts = case map locValue (AST.alterActions stmt) of
    [AST.AlterViewAsQuery (L _ q)] ->
        let excluded = [locSpan (AST.alterTable stmt)]
            sources = visitorReads visitor excluded Core.ReadSchema
         in case definitionName ctx False (locValue (AST.alterTable stmt)) of
                Just (name, temp) ->
                    let cols = fromMaybe [] (queryOutputColumns ctx q)
                        (lineage, condCols) = extractViewLineage ctx q Nothing visitor excluded
                        viewDef =
                            Core.ViewDef
                                { Core.viewName = name
                                , Core.viewTemporary = temp
                                , Core.viewMaterialized = False
                                , Core.viewColumns = cols
                                , Core.viewSources = sources
                                , Core.viewIfNotExists = False
                                , Core.viewOrReplace = True
                                , Core.viewColumnLineage = lineage
                                , Core.viewConditionColumns = condCols
                                }
                     in baseFacts
                            { Core.factOps = Core.factOps baseFacts ++ [Core.DefineViewOp viewDef]
                            , Core.factBarriers = addUnmodeledBarriers visitor excluded (Core.factBarriers baseFacts)
                            }
                Nothing ->
                    baseFacts
                        { Core.factBarriers =
                            Core.factBarriers baseFacts
                                ++ [ Core.ScopedBarrier Core.GlobalScope Core.UnknownViewBarrier (Just (TE.decodeUtf8 (unitSource ctx)))
                                   ]
                        }
    [AST.AlterRenameTable (L _ newName)] ->
        case (relationRef ctx (locValue (AST.alterTable stmt)), nameFromAST ctx IdentRelation newName) of
            (Just fromRef, Just toName) ->
                baseFacts
                    { Core.factOps =
                        Core.factOps baseFacts
                            ++ [Core.RenameOp (Core.RenameRelation fromRef toName (indexSchemaPlacement ctx))]
                    }
            _ ->
                baseFacts
                    { Core.factBarriers =
                        Core.factBarriers baseFacts
                            ++ [ Core.ScopedBarrier Core.GlobalScope Core.UnknownViewBarrier (Just (TE.decodeUtf8 (unitSource ctx)))
                               ]
                    }
    [AST.AlterRenameColumn (L _ oldPart) (L _ newPart)] ->
        case relationRef ctx (locValue (AST.alterTable stmt)) of
            Just table ->
                baseFacts
                    { Core.factOps =
                        Core.factOps baseFacts
                            ++ [ Core.RenameOp
                                    ( Core.RenameColumnOp
                                        table
                                        (canonicalIdent ctx IdentColumn oldPart)
                                        (canonicalIdent ctx IdentColumn newPart)
                                    )
                               ]
                    }
            Nothing ->
                baseFacts
                    { Core.factBarriers =
                        Core.factBarriers baseFacts
                            ++ [ Core.ScopedBarrier Core.GlobalScope Core.UnknownViewBarrier (Just (TE.decodeUtf8 (unitSource ctx)))
                               ]
                    }
    _ ->
        baseFacts
            { Core.factBarriers =
                Core.factBarriers baseFacts
                    ++ [ Core.ScopedBarrier Core.GlobalScope Core.UnknownViewBarrier (Just (TE.decodeUtf8 (unitSource ctx)))
                       ]
            }

alterChange ::
    AnalysisContext ->
    VisitorState ->
    Core.NameRef ->
    ByteSpan ->
    AST.AlterAction ->
    ([Core.AlterChange], [Core.CatalogOp], [Core.ScopedBarrier])
alterChange ctx visitor table actionSpan action =
    let detail = TE.decodeUtf8 (unsafeSliceSource ctx actionSpan)
     in case action of
            AST.AlterAddColumn _ cols _ ->
                let chs =
                        [ let (col, cons) = columnDefinition ctx cdef
                           in Core.AddColumn col cons
                        | L _ cdef <- cols
                        ]
                 in (chs, [], [])
            AST.AlterDropColumn _ (L _ part) mBehavior ->
                ( [Core.DropColumn (canonicalIdent ctx IdentColumn part) (mBehavior == Just AST.DropCascade)]
                , []
                , []
                )
            AST.AlterRenameColumn (L _ oldPart) (L _ newPart) ->
                ( []
                ,
                    [ Core.RenameOp
                        ( Core.RenameColumnOp
                            table
                            (canonicalIdent ctx IdentColumn oldPart)
                            (canonicalIdent ctx IdentColumn newPart)
                        )
                    ]
                , []
                )
            AST.AlterAddConstraint (L _ tc) -> case constraintFromTable ctx tc of
                Just c -> ([Core.AddConstraint c], [], [])
                Nothing -> ([], [], [Core.ScopedBarrier Core.GlobalScope Core.UnknownAlterBarrier (Just detail)])
            AST.AlterAddIndex mName keys ->
                let idxConstraint =
                        Core.IndexConstraint
                            (fmap (canonicalIdent ctx IdentOther . locValue) mName)
                            (mapMaybe (indexColumnIdent ctx . locValue) keys)
                            Core.IndexNonUnique
                 in ([Core.AddConstraint idxConstraint], [], [])
            AST.AlterDropConstraint _ (L _ part) mBehavior ->
                ( [Core.DropConstraint (canonicalIdent ctx IdentOther part) (mBehavior == Just AST.DropCascade)]
                , []
                , []
                )
            AST.AlterDropIndex _ (L _ part) _ ->
                ([Core.DropIndexChange (canonicalIdent ctx IdentOther part)], [], [])
            AST.AlterAlterColumn (L colSp colPart) colAct ->
                let col = canonicalIdent ctx IdentColumn colPart
                    refs = alterOpColumns ctx colAct
                    opDetail =
                        let sliced = T.strip (TE.decodeUtf8 (unsafeSliceSource ctx (ByteSpan (spanEnd colSp) (spanEnd actionSpan))))
                         in if T.null sliced then detail else sliced
                 in ([Core.AlterColumn col opDetail refs False], [], [])
            AST.AlterChangeColumn (L _ oldPart) (L _ cdef) mPos ->
                let oldName = canonicalIdent ctx IdentColumn oldPart
                    newName = canonicalIdent ctx IdentColumn (locValue (AST.columnName cdef))
                    renameOps =
                        [ Core.RenameOp (Core.RenameColumnOp table oldName newName)
                        | oldName /= newName
                        ]
                 in ([changeModifyChange ctx newName cdef mPos detail table], renameOps, [])
            AST.AlterModifyColumn cols mPos ->
                let chs =
                        [ let colName = canonicalIdent ctx IdentColumn (locValue (AST.columnName cdef))
                           in changeModifyChange ctx colName cdef mPos detail table
                        | L _ cdef <- cols
                        ]
                 in (chs, [], [])
            AST.AlterRenameTable (L _ newName) -> case nameFromAST ctx IdentRelation newName of
                Just n ->
                    ( []
                    ,
                        [ Core.RenameOp
                            (Core.RenameRelation table (qualifyRename (Core.nameRefName table) n) (indexSchemaPlacement ctx))
                        ]
                    , []
                    )
                Nothing -> ([], [], [Core.ScopedBarrier Core.GlobalScope Core.UnknownAlterBarrier (Just detail)])
            AST.AlterAttachPartition (L _ partName) _ -> case relationRef ctx partName of
                Just child -> ([], [Core.AttachPartitionOp (Core.PartitionLink table child)], [])
                Nothing -> ([], [], [Core.ScopedBarrier Core.GlobalScope Core.UnknownAlterBarrier (Just detail)])
            AST.AlterDetachPartition (L _ partName) _ _ -> case relationRef ctx partName of
                Just child -> ([], [Core.DetachPartitionOp (Core.PartitionLink table child)], [])
                Nothing -> ([], [], [Core.ScopedBarrier Core.GlobalScope Core.UnknownAlterBarrier (Just detail)])
            AST.AlterInherit (L _ parentName) -> case relationRef ctx parentName of
                Just parent -> ([], [Core.AttachInheritOp (Core.PartitionLink parent table)], [])
                Nothing -> ([], [], [Core.ScopedBarrier Core.GlobalScope Core.UnknownAlterBarrier (Just detail)])
            AST.AlterNoInherit (L _ parentName) -> case relationRef ctx parentName of
                Just parent -> ([], [Core.DetachInheritOp (Core.PartitionLink parent table)], [])
                Nothing -> ([], [], [Core.ScopedBarrier Core.GlobalScope Core.UnknownAlterBarrier (Just detail)])
            AST.AlterTableOption (L _ opt)
                | ctxDialect ctx `elem` [MySQL, MariaDB]
                , map (T.toUpper . unquoteNamePart . locValue) (AST.tableOptionName opt)
                    `elem` [["ALGORITHM"], ["LOCK"]] ->
                    ([], [], [Core.ScopedBarrier (Core.RelationScope table) Core.UnknownAlterBarrier (Just detail)])
            AST.AlterMaterializeTtl ->
                ([], [], [Core.ScopedBarrier (Core.RelationScope table) Core.UnknownAlterBarrier (Just detail)])
            AST.AlterModifySetting _ ->
                ([], [], [Core.ScopedBarrier (Core.RelationScope table) Core.UnknownAlterBarrier (Just detail)])
            AST.AlterSetLogged _ ->
                ([], [], [Core.ScopedBarrier (Core.RelationScope table) Core.UnknownAlterBarrier (Just detail)])
            AST.AlterRowLevelSecurity _ ->
                ([], [], [Core.ScopedBarrier (Core.RelationScope table) Core.UnknownAlterBarrier (Just detail)])
            AST.AlterUpdate assigns _ _ ->
                let writtenCols =
                        dedupCols
                            [ col
                            | L _ asgn <- assigns
                            , Just name <- [nameFromAST ctx IdentColumn (locValue (AST.assignTarget asgn))]
                            , Just col <- [listToMaybe (reverse (Core.nameParts name))]
                            ]
                    readFacts = visitorReads visitor [] Core.ReadData
                    dw =
                        Core.DataWrite
                            { Core.dataWriteTarget = table
                            , Core.dataWriteKind = Core.UpdateWrite Core.UpdateFilteredRows
                            , Core.dataWriteColumns = []
                            , Core.dataWriteUnqualifiedColumns = writtenCols
                            , Core.dataWriteReads = readFacts
                            , Core.dataWriteOnly = False
                            }
                 in ([], [Core.WriteDataOp dw], [])
            AST.AlterDelete _ _ ->
                let readFacts = visitorReads visitor [] Core.ReadData
                    dw =
                        Core.DataWrite
                            { Core.dataWriteTarget = table
                            , Core.dataWriteKind = Core.DeleteWrite Core.DeleteFilteredRows
                            , Core.dataWriteColumns = []
                            , Core.dataWriteUnqualifiedColumns = []
                            , Core.dataWriteReads = readFacts
                            , Core.dataWriteOnly = False
                            }
                 in ([], [Core.WriteDataOp dw], [])
            _ -> ([], [], [Core.ScopedBarrier Core.GlobalScope Core.UnknownAlterBarrier (Just detail)])
    where
        dedupCols =
            reverse
                . fst
                . foldl'
                    (\(acc, seen) c -> if Set.member c seen then (acc, seen) else (c : acc, Set.insert c seen))
                    ([], Set.empty)

alterOpColumns :: AnalysisContext -> AST.AlterColumnAction -> [Core.Ident]
alterOpColumns ctx = \case
    AST.AlterSetDefault (L _ e) -> exprColumns ctx e
    AST.AlterType _ (Just (L _ u)) -> exprColumns ctx u
    AST.AlterAddIdentity _ opts ->
        dedupCols [c | L _ opt <- opts, Just (L _ e) <- [AST.tableOptionValue opt], c <- exprColumns ctx e]
    _ -> []
    where
        dedupCols xs =
            reverse
                ( fst
                    ( foldl'
                        (\(acc, seen) x -> if Set.member x seen then (acc, seen) else (x : acc, Set.insert x seen))
                        ([], Set.empty)
                        xs
                    )
                )

changeModifyChange ::
    AnalysisContext ->
    Core.Ident ->
    AST.ColumnDef ->
    Maybe (Located AST.ColumnPosition) ->
    Text ->
    Core.NameRef ->
    Core.AlterChange
changeModifyChange ctx colName cdef mPos detail table =
    case changeModifyRefs ctx cdef table of
        Just refs -> Core.AlterColumn colName detail refs (isJust mPos)
        Nothing -> Core.OtherChange detail

changeModifyRefs :: AnalysisContext -> AST.ColumnDef -> Core.NameRef -> Maybe [Core.Ident]
changeModifyRefs ctx cdef table =
    fmap (reverse . fst) (foldl' step (Just ([], Set.empty)) (AST.columnConstraints cdef))
    where
        pushList = foldl' pushOne
        pushOne (acc, seen) c
            | Set.member c seen = (acc, seen)
            | otherwise = (c : acc, Set.insert c seen)
        step Nothing _ = Nothing
        step (Just st) (L _ cc) = case cc of
            AST.ColumnConstraintNamed _ inner -> step (Just st) (L (ByteSpan 0 0) inner)
            AST.ColumnConstraintOnConflict inner _ -> step (Just st) (L (ByteSpan 0 0) inner)
            AST.ColumnConstraintDefault (L _ e) -> Just (pushList st (exprColumns ctx e))
            AST.ColumnConstraintCheck (L _ e) -> Just (pushList st (exprColumns ctx e))
            AST.ColumnConstraintGenerated _ (L _ e) -> Just (pushList st (exprColumns ctx e))
            AST.ColumnConstraintOnUpdate (L _ e) -> Just (pushList st (exprColumns ctx e))
            AST.ColumnConstraintEphemeral (Just (L _ e)) -> Just (pushList st (exprColumns ctx e))
            AST.ColumnConstraintIdentity _ opts ->
                Just
                    ( pushList
                        st
                        [c | L _ opt <- opts, Just (L _ e) <- [AST.tableOptionValue opt], c <- exprColumns ctx e]
                    )
            AST.ColumnConstraintAutoIncrement -> Nothing
            AST.ColumnConstraintReferences (L _ fk) ->
                if alterParentMayBeSelf ctx (locValue (AST.foreignKeyTable fk)) table
                    then case AST.foreignKeyColumns fk of
                        [] -> Nothing
                        cols -> Just (pushList st (map (canonicalIdent ctx IdentColumn . locValue) cols))
                    else Just st
            _ -> Just st

alterParentMayBeSelf :: AnalysisContext -> AST.Name -> Core.NameRef -> Bool
alterParentMayBeSelf ctx parent table =
    case nameParts ctx IdentRelation parent of
        Just parts -> listToMaybe (reverse parts) == listToMaybe (reverse (Core.nameParts (Core.nameRefName table)))
        Nothing -> True

renameTableFacts ::
    AnalysisContext ->
    [(Located AST.Name, Located AST.Name)] ->
    Core.SemanticFacts ->
    Core.SemanticFacts
renameTableFacts ctx renames baseFacts = foldl' step baseFacts renames
    where
        step acc (L spOld oldName, L spNew newName) =
            case (relationRef ctx oldName, nameFromAST ctx IdentRelation newName) of
                (Just fromRef, Just toName) ->
                    let mkRename =
                            if ctxDialect ctx == MySQL
                                then Core.RenamePermanentRelation
                                else Core.RenameRelation
                     in acc
                            { Core.factOps =
                                Core.factOps acc
                                    ++ [ Core.RenameOp
                                            ( mkRename
                                                fromRef
                                                (qualifyRename (Core.nameRefName fromRef) toName)
                                                (indexSchemaPlacement ctx)
                                            )
                                       ]
                            }
                _ ->
                    let detail =
                            TE.decodeUtf8 (unsafeSliceSource ctx spOld)
                                <> " TO "
                                <> TE.decodeUtf8 (unsafeSliceSource ctx spNew)
                     in acc
                            { Core.factBarriers =
                                Core.factBarriers acc
                                    ++ [Core.ScopedBarrier Core.GlobalScope Core.UnknownAlterBarrier (Just detail)]
                            }

analyzeFacts :: AnalysisContext -> AST.AnalyzeStmt -> Core.SemanticFacts -> Core.SemanticFacts
analyzeFacts ctx stmt baseFacts
    | null (AST.analyzeTables stmt) =
        baseFacts
            { Core.factBarriers =
                Core.factBarriers baseFacts
                    ++ [Core.ScopedBarrier Core.GlobalScope Core.UnknownDDLBarrier (Just (TE.decodeUtf8 (unitSource ctx)))]
            }
    | otherwise = foldl' step baseFacts (AST.analyzeTables stmt)
    where
        step acc (L _ (AST.AnalyzeTable (L sp name) cols)) =
            let rawName = TE.decodeUtf8 (unsafeSliceSource ctx sp)
             in case relationRef ctx name of
                    Just target ->
                        let colIdents = map (canonicalIdent ctx IdentColumn . locValue) cols
                         in acc
                                { Core.factOps =
                                    Core.factOps acc ++ [Core.ReadOp (Core.ReadFact target Core.ReadData colIdents False)]
                                , Core.factBarriers =
                                    Core.factBarriers acc
                                        ++ [ Core.ScopedBarrier
                                                (Core.RelationScope target)
                                                Core.UnknownDDLBarrier
                                                (Just ("ANALYZE on relation " <> rawName))
                                           ]
                                }
                    Nothing ->
                        acc
                            { Core.factBarriers =
                                Core.factBarriers acc
                                    ++ [Core.ScopedBarrier Core.GlobalScope Core.UnknownRelationBarrier (Just rawName)]
                            }
