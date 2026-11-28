-- | Semantic fact extraction for DROP, ALTER, RENAME, and ANALYZE DDL statements.
module Occam.Frontend.Semantics.DDL.AlterDrop (
    dropFacts,
    alterFacts,
    renameTableFacts,
    analyzeFacts,
) where

import Data.Foldable qualified as Foldable
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Occam.Core.Schema qualified as Core
import Occam.Core.Semantics qualified as Core
import Occam.Frontend.AST.DDL qualified as AST
import Occam.Frontend.AST.DML qualified as AST
import Occam.Frontend.AST.Expr qualified as AST
import Occam.Frontend.AST.Statement qualified as AST
import Occam.Frontend.Context (
    AnalysisContext (..),
    IdentifierClass (..),
    canonicalIdent,
    definitionName,
    nameFromAST,
    nameParts,
    relationRef,
    unitSource,
    unquoteNamePart,
    unsafeSliceSource,
 )
import Occam.Frontend.Semantics.DDL.Create (
    columnDefinition,
    constraintFromTable,
    indexRef,
    indexSchemaPlacement,
    qualifyRename,
    triggerRef,
 )
import Occam.Frontend.Semantics.Expr (exprColumns)
import Occam.Frontend.Semantics.Pure (dedupPreserve, indexColumnIdent)
import Occam.Frontend.Semantics.Query (
    VisitorState (..),
    addUnmodeledBarriers,
    extractViewLineage,
    queryOutputColumns,
    visitorReads,
 )
import Occam.Frontend.Span (ByteSpan (..), Located (..), spanEnd)
import Occam.Parser.Types (SQLDialect (..))

-- | Extract catalog drop operations and dependency barriers from a @DROP@ statement.
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

-- | Extract catalog alterations, FK/constraint updates, and barriers from an @ALTER@ statement.
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
            let (changes, ops, barriers) = Foldable.foldl' (stepAction table) ([], [], []) (AST.alterActions stmt)
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
                        dedupPreserve
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

alterOpColumns :: AnalysisContext -> AST.AlterColumnAction -> [Core.Ident]
alterOpColumns ctx = \case
    AST.AlterSetDefault (L _ e) -> exprColumns ctx e
    AST.AlterType _ (Just (L _ u)) -> exprColumns ctx u
    AST.AlterAddIdentity _ opts ->
        dedupPreserve
            [c | L _ opt <- opts, Just (L _ e) <- [AST.tableOptionValue opt], c <- exprColumns ctx e]
    _ -> []

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
    fmap (reverse . fst) (Foldable.foldl' step (Just ([], Set.empty)) (AST.columnConstraints cdef))
    where
        pushList = Foldable.foldl' pushOne
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

-- | Extract pairwise relation renames from MySQL\/ClickHouse @RENAME TABLE@ statements.
renameTableFacts ::
    AnalysisContext ->
    [(Located AST.Name, Located AST.Name)] ->
    Core.SemanticFacts ->
    Core.SemanticFacts
renameTableFacts ctx renames baseFacts = Foldable.foldl' step baseFacts renames
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

-- | Extract relation\/column read facts and conservative barriers from an @ANALYZE@ statement.
analyzeFacts :: AnalysisContext -> AST.AnalyzeStmt -> Core.SemanticFacts -> Core.SemanticFacts
analyzeFacts ctx stmt baseFacts
    | null (AST.analyzeTables stmt) =
        baseFacts
            { Core.factBarriers =
                Core.factBarriers baseFacts
                    ++ [Core.ScopedBarrier Core.GlobalScope Core.UnknownDDLBarrier (Just (TE.decodeUtf8 (unitSource ctx)))]
            }
    | otherwise = Foldable.foldl' step baseFacts (AST.analyzeTables stmt)
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
