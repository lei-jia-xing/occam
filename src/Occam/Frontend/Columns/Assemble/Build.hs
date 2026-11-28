-- | Single forward pass that gathers script-wide column references and fences.
module Occam.Frontend.Columns.Assemble.Build (buildAssembly) where

import Data.Foldable qualified as Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Occam.Core.Schema qualified as Core
import Occam.Core.Semantics qualified as Core
import Occam.Frontend.Columns.Analysis (
    ColumnUnit (..),
    InsertColumns (..),
    collectReads,
 )
import Occam.Frontend.Columns.Assemble.Types (
    Assembly (..),
    RelRename (..),
    UnitRefs (..),
    UnitView (..),
 )

data BuildAcc = BuildAcc
    { baRefsRev :: ![Maybe UnitRefs]
    , baOpaqueAll :: !Bool
    , baInTx :: !Bool
    , baDependent :: !(Set Core.Name)
    , baRelation :: !(Set Core.Name)
    , baOpaque :: !(Set Core.Name)
    , baSchema :: !(Set Core.Name)
    , baIndexCols :: !(Map Core.Name [Core.Ident])
    , baIndexOpaque :: !(Set Core.Name)
    , baParentCols :: !(Map Core.Name (Set Core.Ident))
    , baParentOpaque :: !(Set Core.Name)
    , baRelNames :: !(Set Core.Name)
    , baDdlNames :: !(Set Core.Name)
    , baDefineNames :: !(Set Core.Name)
    , baRenamesRev :: ![RelRename]
    , baDrops :: !(Set Core.Name)
    }

-- | Gather script-wide references and conservative fences in source order.
buildAssembly :: [UnitView] -> Assembly
buildAssembly views =
    let acc0 =
            BuildAcc
                { baRefsRev = []
                , baOpaqueAll = False
                , baInTx = False
                , baDependent = Set.empty
                , baRelation = Set.empty
                , baOpaque = Set.empty
                , baSchema = Set.empty
                , baIndexCols = Map.empty
                , baIndexOpaque = Set.empty
                , baParentCols = Map.empty
                , baParentOpaque = Set.empty
                , baRelNames = Set.empty
                , baDdlNames = Set.empty
                , baDefineNames = Set.empty
                , baRenamesRev = []
                , baDrops = Set.empty
                }
        acc = Foldable.foldl' stepUnit acc0 views
     in Assembly
            { asmViews = views
            , asmRefs = reverse (baRefsRev acc)
            , asmOpaqueAll = baOpaqueAll acc
            , asmDependentFenced = baDependent acc
            , asmRelationFenced = baRelation acc
            , asmOpaqueFenced = baOpaque acc
            , asmSchemaFenced = baSchema acc
            , asmIndexColumns = baIndexCols acc
            , asmIndexOpaque = baIndexOpaque acc
            , asmParentColumns = baParentCols acc
            , asmParentOpaque = baParentOpaque acc
            , asmRelationNames = baRelNames acc
            , asmDdlNames = baDdlNames acc
            , asmDefineNames = baDefineNames acc
            , asmRelRenames = reverse (baRenamesRev acc)
            , asmDrops = baDrops acc
            }

stepUnit :: BuildAcc -> UnitView -> BuildAcc
stepUnit acc view = case uvFacts view of
    Nothing ->
        acc
            { baRefsRev = Nothing : baRefsRev acc
            , baOpaqueAll = True
            }
    Just facts ->
        let unitRefs = collectRefs facts (uvColumn view)
            !opaqueAll' =
                baOpaqueAll acc
                    || barrierIsGlobalSchemaMutation facts
                    || sessionChangesNameResolution facts
            !inTxBefore =
                applyTxBegin (Core.factPreTx facts)
                    . applyTxClear (Core.factPreTx facts)
                    $ baInTx acc
            !unitInTx =
                inTxBefore
                    || not (null (Core.factPreTx facts))
                    || not (null (Core.factPostTx facts))
                    || any ((== Core.TransactionStateScope) . Core.barrierScope) (Core.factBarriers facts)
            !opaqueSet' =
                if urOpaque unitRefs || unitInTx
                    then Set.union (baOpaque acc) (urTouched unitRefs)
                    else baOpaque acc
            !inTxAfter =
                applyTxBegin (Core.factPostTx facts)
                    . applyTxClear (Core.factPostTx facts)
                    $ inTxBefore
            !relNamesFromRefs =
                Set.unions
                    [ Set.fromList [Core.nameRefName (Core.readSource r) | r <- urReads unitRefs]
                    , Set.fromList [t | (t, _) <- urWrites unitRefs]
                    , Set.fromList [Core.nameRefName nr | ins <- cuInserts (uvColumn view), Just nr <- [insertTarget ins]]
                    ]
            (depF, relF, schF) = collectTableFences facts (baDependent acc, baRelation acc, baSchema acc)
            accWithFences =
                acc
                    { baRefsRev = Just unitRefs : baRefsRev acc
                    , baOpaqueAll = opaqueAll'
                    , baInTx = inTxAfter
                    , baDependent = depF
                    , baRelation = relF
                    , baOpaque = opaqueSet'
                    , baSchema = schF
                    , baRelNames = Set.union (baRelNames acc) relNamesFromRefs
                    }
         in Foldable.foldl' (stepOp (uvIndex view)) accWithFences (Core.factOps facts)
    where
        applyTxClear txs b
            | any isEndTx txs = False
            | otherwise = b
        applyTxBegin txs b
            | any isBeginTx txs = True
            | otherwise = b
        isEndTx = \case
            Core.TxCommit False -> True
            Core.TxRollbackAll -> True
            _ -> False
        isBeginTx = \case
            Core.TxBegin _ -> True
            _ -> False

stepOp :: Int -> BuildAcc -> Core.CatalogOp -> BuildAcc
stepOp unitIdx acc = \case
    Core.DefineIndexOp idx ->
        let tbl = Core.nameRefName (Core.indexTable idx)
            acc' =
                acc
                    { baRelNames = Set.insert tbl (baRelNames acc)
                    , baDdlNames = Set.insert tbl (baDdlNames acc)
                    }
         in if Core.indexExpression idx || Core.indexPredicate idx
                then acc' {baIndexOpaque = Set.insert tbl (baIndexOpaque acc')}
                else
                    acc'
                        { baIndexCols =
                            Map.insertWith (flip (++)) tbl (Core.indexColumns idx) (baIndexCols acc')
                        }
    Core.DefineTableOp tbl ->
        let tName = Core.tableName tbl
            parents =
                concatMap Core.columnReferences (Core.tableColumns tbl)
                    ++ [fk | Core.ForeignKeyConstraint fk <- Core.tableConstraints tbl]
            acc' =
                acc
                    { baRelNames = Set.insert tName (baRelNames acc)
                    , baDdlNames = Set.insert tName (baDdlNames acc)
                    , baDefineNames = Set.insert tName (baDefineNames acc)
                    }
         in Foldable.foldl' recordParentFk acc' parents
    Core.DefineTriggerOp tr ->
        let tbl = Core.nameRefName (Core.triggerTable tr)
         in acc
                { baRelNames = Set.insert tbl (baRelNames acc)
                , baDdlNames = Set.insert tbl (baDdlNames acc)
                }
    Core.AlterTableOp alt ->
        let tbl = Core.nameRefName (Core.alterTableTarget alt)
            parents = concatMap changeParents (Core.alterTableChanges alt)
            acc' =
                acc
                    { baRelNames = Set.insert tbl (baRelNames acc)
                    , baDdlNames = Set.insert tbl (baDdlNames acc)
                    }
         in Foldable.foldl' recordParentFk acc' parents
    Core.DropCatalogOp dr ->
        Foldable.foldl' recordDrop acc (Core.dropObjects dr)
    Core.RenameOp (Core.RenameRelation fromRef toName _) ->
        let fromName = Core.nameRefName fromRef
         in acc
                { baRelNames = Set.insert fromName (Set.insert toName (baRelNames acc))
                , baDdlNames = Set.insert fromName (Set.insert toName (baDdlNames acc))
                , baRenamesRev = RelRename unitIdx fromRef toName : baRenamesRev acc
                }
    Core.RenameOp (Core.RenamePermanentRelation fromRef toName _) ->
        let fromName = Core.nameRefName fromRef
         in acc
                { baRelNames = Set.insert fromName (Set.insert toName (baRelNames acc))
                , baDdlNames = Set.insert fromName (Set.insert toName (baDdlNames acc))
                , baRenamesRev = RelRename unitIdx (fromRef {Core.nameRefTemp = False}) toName : baRenamesRev acc
                }
    Core.RenameOp (Core.RenameColumnOp tbl _ _) ->
        let tName = Core.nameRefName tbl
         in acc
                { baRelNames = Set.insert tName (baRelNames acc)
                , baDdlNames = Set.insert tName (baDdlNames acc)
                }
    _ -> acc
    where
        changeParents = \case
            Core.AddColumn col constraints ->
                Core.columnReferences col
                    ++ [fk | Core.ForeignKeyConstraint fk <- constraints]
            Core.AddConstraint (Core.ForeignKeyConstraint fk) -> [fk]
            _ -> []
        recordParentFk a fk =
            let pName = Core.nameRefName (Core.foreignKeyParent fk)
                pCols = Core.foreignKeyParentColumns fk
                a' = a {baRelNames = Set.insert pName (baRelNames a)}
             in if null pCols
                    then a' {baParentOpaque = Set.insert pName (baParentOpaque a')}
                    else
                        a'
                            { baParentCols =
                                Map.insertWith Set.union pName (Set.fromList pCols) (baParentCols a')
                            }
        recordDrop a = \case
            Core.DropRelation nr _ ->
                let rName = Core.nameRefName nr
                 in a
                        { baRelNames = Set.insert rName (baRelNames a)
                        , baDdlNames = Set.insert rName (baDdlNames a)
                        , baDrops = Set.insert rName (baDrops a)
                        }
            _ -> a

collectWrites :: Core.SemanticFacts -> [(Core.Name, [Core.Ident])]
collectWrites facts = concatMap opWrites (Core.factOps facts)
    where
        opWrites = \case
            Core.WriteDataOp w -> case Core.dataWriteKind w of
                Core.InsertWrite _ -> []
                _ -> [(Core.nameRefName (Core.dataWriteTarget w), Core.dataWriteColumns w)]
            Core.DefineTriggerOp tr -> case Core.triggerBody tr of
                Core.TriggerInline effects ->
                    [ eventPair ev
                    | Core.TriggerDml ev <- effects
                    ]
                _ -> []
            _ -> []
        eventPair = \case
            Core.DmlInsert t cols -> (Core.nameRefName t, cols)
            Core.DmlUpdate t cols -> (Core.nameRefName t, cols)
            Core.DmlMerge t cols -> (Core.nameRefName t, cols)
            Core.DmlDelete t _ -> (Core.nameRefName t, [])
            Core.DmlTruncate t _ -> (Core.nameRefName t, [])

collectTouched :: Core.SemanticFacts -> ColumnUnit -> [Core.ReadFact] -> Set Core.Name
collectTouched facts colUnit readsList =
    Set.unions
        [ Set.fromList [Core.nameRefName (Core.readSource r) | r <- readsList]
        , Set.fromList (concatMap opNames (Core.factOps facts))
        , Set.fromList [Core.nameRefName nr | ins <- cuInserts colUnit, Just nr <- [insertTarget ins]]
        , Set.fromList (mapMaybe (barrierName . Core.barrierScope) (Core.factBarriers facts))
        ]
    where
        opNames = \case
            Core.DefineTableOp t -> [Core.tableName t]
            Core.DefineViewOp v -> [Core.viewName v]
            Core.DefineIndexOp i -> [Core.nameRefName (Core.indexTable i)]
            Core.WriteDataOp w -> [Core.nameRefName (Core.dataWriteTarget w)]
            Core.AlterTableOp a -> [Core.nameRefName (Core.alterTableTarget a)]
            Core.DefineTriggerOp tr -> [Core.nameRefName (Core.triggerTable tr)]
            Core.RenameOp (Core.RenameRelation from to _) -> [Core.nameRefName from, to]
            Core.RenameOp (Core.RenamePermanentRelation from to _) -> [Core.nameRefName from, to]
            Core.RenameOp (Core.RenameColumnOp tbl _ _) -> [Core.nameRefName tbl]
            Core.DropCatalogOp d -> [Core.nameRefName nr | Core.DropRelation nr _ <- Core.dropObjects d]
            _ -> []
        barrierName = \case
            Core.RelationScope nr -> Just (Core.nameRefName nr)
            Core.ObjectScope obj -> case obj of
                Core.ObjectRelation nr -> Just (Core.nameRefName nr)
                Core.ObjectConstraint cr -> Just (Core.nameRefName (Core.constraintTable cr))
                Core.ObjectIndex (Core.TableIndex tbl _) -> Just (Core.nameRefName tbl)
                Core.ObjectTrigger (Core.TableTrigger tbl _) -> Just (Core.nameRefName tbl)
                _ -> Nothing
            _ -> Nothing

collectRefs :: Core.SemanticFacts -> ColumnUnit -> UnitRefs
collectRefs facts colUnit =
    UnitRefs
        { urReads = readsList
        , urWrites = writesList
        , urTouched = collectTouched facts colUnit readsList
        , urStars = cuStars colUnit
        , urUnknownSource = cuUnknownSource colUnit
        , urOpaque = opaque
        }
    where
        readsList = collectReads facts
        writesList = collectWrites facts
        opaque =
            not (null (Core.factPreTx facts))
                || not (null (Core.factPostTx facts))
                || any (isOpaqueScope . Core.barrierScope) (Core.factBarriers facts)
        isOpaqueScope = \case
            Core.GlobalScope -> True
            Core.TransactionStateScope -> True
            Core.SessionStateScope -> True
            Core.SchemaScope _ -> True
            _ -> False

barrierIsGlobalSchemaMutation :: Core.SemanticFacts -> Bool
barrierIsGlobalSchemaMutation facts = any check (Core.factBarriers facts)
    where
        check b =
            Core.barrierScope b == Core.GlobalScope && case Core.barrierReason b of
                Core.SyntaxErrorBarrier -> True
                Core.UnknownDDLBarrier -> True
                Core.UnknownAlterBarrier -> True
                Core.UnknownDropBarrier -> True
                Core.UnknownRelationBarrier -> True
                Core.UnknownTruncateBarrier -> True
                Core.UnknownSessionEffectBarrier -> True
                Core.UnsupportedTransactionBarrier -> True
                Core.UnknownStatementBarrier ->
                    Core.barrierDetail b /= Just "unmodeled routine or deferred expression effects"
                Core.UnknownDMLBarrier -> null (Core.factOps facts)
                Core.UnknownViewBarrier -> null (Core.factOps facts)
                _ -> False

sessionChangesNameResolution :: Core.SemanticFacts -> Bool
sessionChangesNameResolution facts = any check (Core.factSession facts)
    where
        check = \case
            Core.SetSearchPath {} -> True
            Core.SetNamespace {} -> True
            Core.ResetAll {} -> True
            _ -> False

collectTableFences ::
    Core.SemanticFacts ->
    (Set Core.Name, Set Core.Name, Set Core.Name) ->
    (Set Core.Name, Set Core.Name, Set Core.Name)
collectTableFences facts (dep0, rel0, sch0) =
    let dep1 = Foldable.foldl' addTrig dep0 (Core.factOps facts)
     in Foldable.foldl' addBarrier (dep1, rel0, sch0) (Core.factBarriers facts)
    where
        addTrig dep = \case
            Core.DefineTriggerOp tr -> Set.insert (Core.nameRefName (Core.triggerTable tr)) dep
            _ -> dep
        addBarrier (dep, rel, sch) b = case Core.barrierScope b of
            Core.RelationScope nr -> (dep, Set.insert (Core.nameRefName nr) rel, sch)
            Core.SchemaScope s -> (dep, rel, Set.insert s sch)
            Core.ObjectScope obj -> case obj of
                Core.ObjectRelation nr -> (dep, Set.insert (Core.nameRefName nr) rel, sch)
                Core.ObjectConstraint cr -> (dep, Set.insert (Core.nameRefName (Core.constraintTable cr)) rel, sch)
                Core.ObjectIndex (Core.TableIndex tbl _) -> (dep, Set.insert (Core.nameRefName tbl) rel, sch)
                Core.ObjectTrigger (Core.TableTrigger tbl _) -> (dep, Set.insert (Core.nameRefName tbl) rel, sch)
                Core.ObjectSchema s -> (dep, rel, Set.insert s sch)
                _ -> (dep, rel, sch)
            _ -> (dep, rel, sch)
