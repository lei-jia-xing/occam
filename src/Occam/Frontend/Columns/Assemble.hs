{- | Script-wide column co-reduction bundle assembly.

Tracks table generation chains across renames, sibling-spelling ambiguity,
schema/relation/dependent fences, transaction opacity, constraint and reference
fences, positional @INSERT@/@VALUES@ binding, and site validation.
-}
module Occam.Frontend.Columns.Assemble (
    assembleColumns,
    assembleColumnsWith,
    blockConstraint,
    blockRead,
    blockStar,
    blockAmbiguous,
    blockUnknown,
    blockGeneration,
    blockShape,
    blockDependent,
    blockSpan,
) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Char (isAsciiUpper, toLower)
import Data.Foldable qualified as Foldable
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Occam.Core.Schema qualified as Core
import Occam.Core.Semantics qualified as Core
import Occam.Core.Types qualified as Core
import Occam.Facts (
    ColumnSite (..),
    ColumnSiteKind (..),
    ColumnTarget (..),
    StatementFacts (..),
    StatementUnit (..),
 )
import Occam.Frontend.Columns.Analysis (
    ColumnUnit (..),
    CreateColumns (..),
    InsertColumns (..),
    InsertList (..),
    InsertShape (..),
    PreParsedUnit (..),
    analyzeColumnUnitWith,
    collectReads,
 )
import Occam.Frontend.Span (isUtf8Boundary)

minBundleSites :: Int
minBundleSites = 2

blockConstraint :: Text
blockConstraint = "column-constraint-ref"

blockRead :: Text
blockRead = "column-read-elsewhere"

blockStar :: Text
blockStar = "column-star-projection"

blockAmbiguous :: Text
blockAmbiguous = "column-ambiguous-binding"

blockUnknown :: Text
blockUnknown = "column-unknown-source"

blockGeneration :: Text
blockGeneration = "column-generation-ambiguous"

blockShape :: Text
blockShape = "column-insert-shape"

blockDependent :: Text
blockDependent = "column-dependent-object"

blockSpan :: Text
blockSpan = "column-span-incomplete"

data UnitView = UnitView
    { uvIndex :: !Int
    , uvStart :: !Int
    , uvEnd :: !Int
    , uvFacts :: !(Maybe Core.SemanticFacts)
    , uvColumn :: !ColumnUnit
    }

data UnitRefs = UnitRefs
    { urReads :: ![Core.ReadFact]
    , urWrites :: ![(Core.Name, [Core.Ident])]
    , urTouched :: !(Set Core.Name)
    , urStars :: ![Core.NameRef]
    , urUnknownSource :: !Bool
    , urOpaque :: !Bool
    }

data RelRename = RelRename
    { rrUnit :: !Int
    , rrFrom :: !Core.NameRef
    , rrTo :: !Core.Name
    }

data FenceSet = FenceSet
    { fsAll :: !Bool
    , fsTouched :: !(Set Core.Ident)
    }

emptyFenceSet :: FenceSet
emptyFenceSet = FenceSet False Set.empty

data TableScope = TableScope
    { tsChain :: ![Core.Name]
    , tsEdgeUnits :: ![Int]
    , tsGeneration :: !FenceSet
    , tsAmb :: !FenceSet
    }

activeGenerationName :: TableScope -> Int -> Core.Name
activeGenerationName scope unitIdx =
    go (headOrErr (tsChain scope)) (zip (tsEdgeUnits scope) (drop 1 (tsChain scope)))
    where
        headOrErr (x : _) = x
        headOrErr [] = Core.Name []
        go cur [] = cur
        go cur ((edge, toName) : rest)
            | edge < unitIdx = go toName rest
            | otherwise = cur

data TablePlan = TablePlan
    { tpName :: !Core.Name
    , tpDefines :: ![(Int, Core.TableDef, Maybe CreateColumns)]
    }

data Assembly = Assembly
    { asmViews :: ![UnitView]
    , asmRefs :: ![Maybe UnitRefs]
    , asmOpaqueAll :: !Bool
    , asmDependentFenced :: !(Set Core.Name)
    , asmRelationFenced :: !(Set Core.Name)
    , asmOpaqueFenced :: !(Set Core.Name)
    , asmSchemaFenced :: !(Set Core.Name)
    , asmIndexColumns :: !(Map Core.Name [Core.Ident])
    , asmIndexOpaque :: !(Set Core.Name)
    , asmParentColumns :: !(Map Core.Name (Set Core.Ident))
    , asmParentOpaque :: !(Set Core.Name)
    , asmRelationNames :: !(Set Core.Name)
    , asmDdlNames :: !(Set Core.Name)
    , asmDefineNames :: !(Set Core.Name)
    , asmRelRenames :: ![RelRename]
    , asmDrops :: !(Set Core.Name)
    }

-- | Assemble script-wide atomic 'ColumnTarget' bundles from parsed statement units.
assembleColumns :: ByteString -> [StatementUnit] -> [ColumnTarget]
assembleColumns source units = assembleColumnsWith source [(u, Nothing) | u <- units]

-- | Assemble script-wide 'ColumnTarget' bundles reusing pre-parsed ASTs and token streams.
assembleColumnsWith :: ByteString -> [(StatementUnit, Maybe PreParsedUnit)] -> [ColumnTarget]
assembleColumnsWith source unitsWithAst =
    concatMap planTargets (planTables (asmViews assembly))
    where
        views =
            [ UnitView
                { uvIndex = idx
                , uvStart = Core.spanStartByte (Core.stmtSpan (unitStatement unit))
                , uvEnd = Core.spanEndByte (Core.stmtSpan (unitStatement unit))
                , uvFacts = fmap factSemantics (unitFacts unit)
                , uvColumn = analyzeColumnUnitWith mPreParsed source unit
                }
            | (idx, (unit, mPreParsed)) <- zip [0 :: Int ..] unitsWithAst
            ]
        assembly = buildAssembly views

        defineTargets plan mScope (unitIdx, tblDef, mGeom) =
            let cols = Core.tableColumns tblDef
                dupNames = hasDuplicateNames cols
                arity = length cols
                geometries = maybe [] createColumns mGeom
                resolveGeometry defined = \case
                    Just (name, spanForColumn) | name == Core.columnName defined -> spanForColumn
                    _ -> Nothing
                buildTarget ordinal defined geometry =
                    let createSpan = resolveGeometry defined geometry
                     in case mScope of
                            Nothing ->
                                let sites = createSites unitIdx createSpan
                                 in Just (blockedTarget (tpName plan) (Core.columnName defined) ordinal sites blockGeneration)
                            Just scope ->
                                columnTarget
                                    assembly
                                    source
                                    (tpName plan)
                                    (unitIdx, tblDef)
                                    arity
                                    dupNames
                                    scope
                                    (ordinal, defined, createSpan)
             in catMaybes (zipWith3 buildTarget [0 ..] cols (map Just geometries ++ repeat Nothing))

        planTargets plan =
            let unique = length (tpDefines plan) == 1
                mScope = case tpDefines plan of
                    [(defUnit, _, _)] | unique -> Just (buildScope assembly (tpName plan) defUnit)
                    _ -> Nothing
             in concatMap (defineTargets plan mScope) (tpDefines plan)

createSites :: Int -> Maybe (Int, Int) -> [ColumnSite]
createSites unitIdx = \case
    Just (s, e) -> [ColumnSite (unitIdx + 1) s e CreateDefSite]
    Nothing -> []

-- ---------------------------------------------------------------------------
-- Assembly construction (single script pass)
-- ---------------------------------------------------------------------------

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

tableInSchema :: Core.Name -> Core.Name -> Bool
tableInSchema (Core.Name parts) (Core.Name schemaParts) = case (parts, schemaParts) of
    ([], _) -> False
    (_, []) -> False
    (_, [singleSchema])
        | length parts >= 2 ->
            let prefix = init parts
             in prefix == [singleSchema]
                    || case reverse prefix of
                        (lastPref : _) -> identAsciiEq lastPref singleSchema
                        [] -> False
    (_, schemas)
        | length parts >= 2 && length parts - 1 == length schemas ->
            init parts == schemas
    ([_], [singleSchema]) ->
        let s = Core.identText singleSchema
         in textAsciiEq s "public" || textAsciiEq s "main" || textAsciiEq s "dbo"
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

-- ---------------------------------------------------------------------------
-- Constraints, generation chains, and table scopes
-- ---------------------------------------------------------------------------

constraintTouches :: Core.Constraint -> Core.Ident -> Bool
constraintTouches c col = case c of
    Core.PrimaryKeyConstraint _ cols -> col `elem` cols
    Core.UniqueConstraint _ cols -> col `elem` cols
    Core.CheckConstraint _ cols _ -> col `elem` cols
    Core.IndexConstraint _ cols _ -> col `elem` cols
    Core.ForeignKeyConstraint fk -> col `elem` Core.foreignKeyChildColumns fk
    Core.UsingIndexConstraint {} -> False
    Core.OtherConstraint _ -> False

constraintIsOpaque :: Core.Constraint -> Bool
constraintIsOpaque = \case
    Core.UsingIndexConstraint {} -> True
    Core.OtherConstraint _ -> True
    _ -> False

buildChain :: Assembly -> Core.Name -> Int -> Maybe ([Core.Name], [Int])
buildChain asm define = go [define] [] (Set.singleton define)
    where
        go chainRev edgeUnitsRev visited prevUnit =
            let current = case chainRev of
                    (c : _) -> c
                    [] -> define
                outgoing =
                    [ edge
                    | edge <- asmRelRenames asm
                    , not (Core.nameRefTemp (rrFrom edge))
                    , Core.nameRefName (rrFrom edge) == current
                    ]
             in case outgoing of
                    [] -> finalize (reverse chainRev) (reverse edgeUnitsRev)
                    [firstEdge]
                        | rrUnit firstEdge <= prevUnit -> Nothing
                        | rrTo firstEdge == current -> Nothing
                        | Set.member (rrTo firstEdge) visited -> Nothing
                        | Set.member (rrTo firstEdge) (asmDefineNames asm) -> Nothing
                        | otherwise ->
                            go
                                (rrTo firstEdge : chainRev)
                                (rrUnit firstEdge : edgeUnitsRev)
                                (Set.insert (rrTo firstEdge) visited)
                                (rrUnit firstEdge)
                    _ -> Nothing

        finalize chain edgeUnits
            | any badRename (asmRelRenames asm) = Nothing
            | any (`Set.member` asmDrops asm) chain = Nothing
            | any sameUnitAltered edgeUnits = Nothing
            | otherwise = Just (chain, edgeUnits)
            where
                chainSet = Set.fromList chain
                inChain n = Set.member n chainSet
                badRename edge
                    | Core.nameRefTemp (rrFrom edge) =
                        inChain (Core.nameRefName (rrFrom edge)) || inChain (rrTo edge)
                    | otherwise =
                        inChain (rrTo edge) && not (inChain (Core.nameRefName (rrFrom edge)))
                sameUnitAltered unitIdx = case drop unitIdx (asmViews asm) of
                    (v : _) -> case uvFacts v of
                        Just f ->
                            any
                                (\case Core.AlterTableOp a -> inChain (Core.nameRefName (Core.alterTableTarget a)); _ -> False)
                                (Core.factOps f)
                        Nothing -> False
                    [] -> False

foldGenerationChange :: [Core.Name] -> Bool -> Core.AlterChange -> FenceSet -> FenceSet
foldGenerationChange chain implicit change gen = case change of
    Core.AddColumn col constraints ->
        let gen1 =
                gen
                    { fsTouched = Set.insert (Core.columnName col) (fsTouched gen)
                    , fsAll = fsAll gen || Core.columnGenerated col || implicit
                    }
            gen2 = Foldable.foldl' (flip (foldConstraint chain)) gen1 constraints
         in Foldable.foldl' addSelfFk gen2 (Core.columnReferences col)
    Core.DropColumn col _ ->
        gen
            { fsTouched = Set.insert col (fsTouched gen)
            , fsAll = fsAll gen || implicit
            }
    Core.AddConstraint c -> foldConstraint chain c gen
    Core.AlterColumn col _ referenced repositions ->
        gen
            { fsTouched = Set.union (fsTouched gen) (Set.fromList (col : referenced))
            , fsAll = fsAll gen || repositions
            }
    _ -> gen {fsAll = True}
    where
        addSelfFk g fk
            | Core.nameRefName (Core.foreignKeyParent fk) `elem` chain =
                case Core.foreignKeyParentColumns fk of
                    [] -> g {fsAll = True}
                    cols -> g {fsTouched = Set.union (fsTouched g) (Set.fromList cols)}
            | otherwise = g

foldConstraint :: [Core.Name] -> Core.Constraint -> FenceSet -> FenceSet
foldConstraint chain c gen = case c of
    Core.PrimaryKeyConstraint _ cols -> touch cols
    Core.UniqueConstraint _ cols -> touch cols
    Core.CheckConstraint _ cols _ -> touch cols
    Core.IndexConstraint _ cols _ -> touch cols
    Core.ForeignKeyConstraint fk ->
        let g1 = touch (Core.foreignKeyChildColumns fk)
         in if Core.nameRefName (Core.foreignKeyParent fk) `elem` chain
                then case Core.foreignKeyParentColumns fk of
                    [] -> g1 {fsAll = True}
                    pCols -> g1 {fsTouched = Set.union (fsTouched g1) (Set.fromList pCols)}
                else g1
    Core.UsingIndexConstraint {} -> gen {fsAll = True}
    Core.OtherConstraint _ -> gen {fsAll = True}
    where
        touch cols = gen {fsTouched = Set.union (fsTouched gen) (Set.fromList cols)}

buildScope :: Assembly -> Core.Name -> Int -> TableScope
buildScope asm define defineUnit = case buildChain asm define defineUnit of
    Nothing ->
        TableScope
            { tsChain = [define]
            , tsEdgeUnits = []
            , tsGeneration = FenceSet True Set.empty
            , tsAmb = buildSiblingScope asm [define]
            }
    Just (chain, edgeUnits) ->
        let implicit = any (hasImplicitForChain chain) (asmViews asm)
            gen = Foldable.foldl' (stepView chain implicit) emptyFenceSet (asmViews asm)
         in TableScope
                { tsChain = chain
                , tsEdgeUnits = edgeUnits
                , tsGeneration = gen
                , tsAmb = buildSiblingScope asm chain
                }
    where
        hasImplicitForChain chain view =
            isJust (uvFacts view) && any (isImplicitIns chain) (cuInserts (uvColumn view))
        isImplicitIns chain ins =
            maybe False ((`elem` chain) . Core.nameRefName) (insertTarget ins)
                && insertShape ins == ShapePlainValues
                && isNothing (insertExplicit ins)
        stepView chain implicit acc view = case uvFacts view of
            Nothing -> acc
            Just facts -> Foldable.foldl' (stepScopeOp chain implicit) acc (Core.factOps facts)
        stepScopeOp chain implicit acc = \case
            Core.AlterTableOp alt
                | Core.nameRefName (Core.alterTableTarget alt) `elem` chain ->
                    Foldable.foldl' (flip (foldGenerationChange chain implicit)) acc (Core.alterTableChanges alt)
            Core.RenameOp (Core.RenameColumnOp tbl fromCol toCol)
                | Core.nameRefName tbl `elem` chain ->
                    acc {fsTouched = Set.insert fromCol (Set.insert toCol (fsTouched acc))}
            _ -> acc

buildSiblingScope :: Assembly -> [Core.Name] -> FenceSet
buildSiblingScope asm chain
    | Set.null siblings = emptyFenceSet
    | any isSibling (Set.toList (asmDdlNames asm)) = FenceSet True Set.empty
    | otherwise =
        let amb0 = Foldable.foldl' stepRef emptyFenceSet (catMaybes (asmRefs asm))
            amb1 = Foldable.foldl' stepViewInserts amb0 (asmViews asm)
            amb2 = Map.foldlWithKey' stepParentCols amb1 (asmParentColumns asm)
         in if any isSibling (Set.toList (asmParentOpaque asm))
                then amb2 {fsAll = True}
                else amb2
    where
        trailings = Set.fromList (mapMaybe lastNamePart chain)
        siblings =
            Set.filter
                (\n -> n `notElem` chain && maybe False (`Set.member` trailings) (lastNamePart n))
                (asmRelationNames asm)
        isSibling n = Set.member n siblings
        stepRef amb refs =
            let amb1 =
                    if any (isSibling . Core.nameRefName) (urStars refs)
                        || (urUnknownSource refs && any isSibling (Set.toList (urTouched refs)))
                        || (urOpaque refs && any isSibling (Set.toList (urTouched refs)))
                        then amb {fsAll = True}
                        else amb
                amb2 =
                    Foldable.foldl'
                        ( \a r ->
                            if isSibling (Core.nameRefName (Core.readSource r))
                                then a {fsTouched = Set.union (fsTouched a) (Set.fromList (Core.readColumns r))}
                                else a
                        )
                        amb1
                        (urReads refs)
             in Foldable.foldl'
                    ( \a (tgt, cols) ->
                        if isSibling tgt
                            then a {fsTouched = Set.union (fsTouched a) (Set.fromList cols)}
                            else a
                    )
                    amb2
                    (urWrites refs)
        stepViewInserts amb view = case uvFacts view of
            Nothing -> amb
            Just _ -> Foldable.foldl' stepIns amb (cuInserts (uvColumn view))
        stepIns amb ins = case insertTarget ins of
            Just tgt | isSibling (Core.nameRefName tgt) -> case insertShape ins of
                ShapeExotic -> amb {fsAll = True}
                ShapeNeutral -> amb
                ShapeSelectSource -> touchExplicit amb (insertExplicit ins)
                ShapePlainValues -> touchExplicit amb (insertExplicit ins)
            _ -> amb
        touchExplicit amb = \case
            Just list -> amb {fsTouched = Set.union (fsTouched amb) (Map.keysSet (insertListColumns list))}
            Nothing -> amb {fsAll = True}
        stepParentCols amb pName cols
            | isSibling pName = amb {fsTouched = Set.union (fsTouched amb) cols}
            | otherwise = amb

lastNamePart :: Core.Name -> Maybe Core.Ident
lastNamePart (Core.Name parts) = listToMaybe (reverse parts)

hasDuplicateNames :: [Core.ColumnDef] -> Bool
hasDuplicateNames = go Set.empty
    where
        go _ [] = False
        go seen (c : rest)
            | Set.member (Core.columnName c) seen = True
            | otherwise = go (Set.insert (Core.columnName c) seen) rest

planTables :: [UnitView] -> [TablePlan]
planTables views =
    [ TablePlan name (reverse defsRev)
    | name <- reverse orderRev
    , Just defsRev <- [Map.lookup name tablesMap]
    ]
    where
        (orderRev, tablesMap) = Foldable.foldl' stepView ([], Map.empty) views
        stepView acc view = case uvFacts view of
            Nothing -> acc
            Just facts ->
                let creates = cuCreates (uvColumn view)
                 in snd $ Foldable.foldl' (stepOpDef (uvIndex view)) (creates, acc) (Core.factOps facts)
        stepOpDef unitIdx (creates, acc@(ordRev, m)) = \case
            Core.DefineTableOp tbl ->
                let (mGeom, createsRest) = case creates of
                        (g : gs) | not (null (Core.tableColumns tbl)) -> (Just g, gs)
                        _ -> (Nothing, creates)
                    tName = Core.tableName tbl
                    ordRev' = if Map.member tName m then ordRev else tName : ordRev
                    m' = Map.insertWith (++) tName [(unitIdx, tbl, mGeom)] m
                 in (createsRest, (ordRev', m'))
            _ -> (creates, acc)

-- ---------------------------------------------------------------------------
-- Bundle validation & per-column target evaluation
-- ---------------------------------------------------------------------------

validateSites :: [UnitView] -> ByteString -> [ColumnSite] -> Maybe [ColumnSite]
validateSites views source rawSites =
    let sorted = sortOn (\s -> (siteStmt s, siteStart s)) rawSites
     in if checkSites Nothing sorted then Just sorted else Nothing
    where
        checkSites _ [] = True
        checkSites prev (s : rest)
            | siteStart s >= siteEnd s = False
            | siteStmt s < 1 = False
            | otherwise = case drop (siteStmt s - 1) views of
                (v : _)
                    | siteStart s >= uvStart v
                    , siteEnd s <= uvEnd v
                    , isUtf8Boundary source (siteStart s)
                    , isUtf8Boundary source (siteEnd s)
                    , maybe True (\(pStmt, pEnd) -> pStmt /= siteStmt s || pEnd <= siteStart s) prev ->
                        checkSites (Just (siteStmt s, siteEnd s)) rest
                _ -> False

referenceFence :: Assembly -> [Core.Name] -> Core.Ident -> Maybe Text
referenceFence asm chain col = Foldable.foldl' (<|>) Nothing (map checkRefs (catMaybes (asmRefs asm)))
    where
        inChain n = n `elem` chain
        checkRefs refs
            | any (inChain . Core.nameRefName) (urStars refs) = Just blockStar
            | any
                (\r -> inChain (Core.nameRefName (Core.readSource r)) && col `elem` Core.readColumns r)
                (urReads refs) =
                Just blockRead
            | any (\(t, cols) -> inChain t && col `elem` cols) (urWrites refs) =
                Just blockRead
            | urUnknownSource refs && any inChain (Set.toList (urTouched refs)) =
                Just blockUnknown
            | urOpaque refs && any inChain (Set.toList (urTouched refs)) =
                Just blockUnknown
            | otherwise = Nothing

insertContribution ::
    InsertColumns ->
    Core.Ident ->
    Int ->
    Int ->
    Int ->
    Either Text [ColumnSite]
insertContribution ins col ordinal createArity stmtId = case insertShape ins of
    ShapeExotic -> Left blockShape
    ShapeNeutral -> Right []
    ShapeSelectSource -> case insertExplicit ins of
        Just list | Map.member col (insertListColumns list) -> Left blockShape
        Nothing -> Left blockShape
        _ -> Right []
    ShapePlainValues
        | null (insertRows ins) -> Left blockSpan
        | otherwise -> case insertExplicit ins of
            Just list -> case Map.lookup col (insertListColumns list) of
                Nothing -> Right []
                Just Nothing -> Left blockAmbiguous
                Just (Just (pos, Just (s, e)))
                    | insertRowWidth ins == Just (insertListWidth list) ->
                        (ColumnSite stmtId s e InsertListSite :) <$> traverse (rowSiteAt pos) (insertRows ins)
                Just (Just _) -> Left blockSpan
            Nothing
                | insertRowWidth ins /= Just createArity -> Left blockSpan
                | otherwise -> traverse (rowSiteAt ordinal) (insertRows ins)
    where
        rowSiteAt idx row = case row V.!? idx of
            Just (Just (s, e)) -> Right (ColumnSite stmtId s e ValuesItemSite)
            _ -> Left blockSpan

blockedTarget :: Core.Name -> Core.Ident -> Int -> [ColumnSite] -> Text -> ColumnTarget
blockedTarget tbl col ordinal sites cause =
    ColumnTarget
        { targetTable = tbl
        , targetColumn = col
        , targetOrdinal = ordinal
        , targetSites = sites
        , targetBlocked = Just cause
        }

columnTarget ::
    Assembly ->
    ByteString ->
    Core.Name ->
    (Int, Core.TableDef) ->
    Int ->
    Bool ->
    TableScope ->
    (Int, Core.ColumnDef, Maybe (Int, Int)) ->
    Maybe ColumnTarget
columnTarget asm source tbl (unitIdx, tblDef) createArity createDupNames scope (ordinal, defined, createSpan)
    | asmOpaqueAll asm = Just (blockedTarget tbl col ordinal initSites blockUnknown)
    | isNothing blockedAfterInserts =
        case validateSites (asmViews asm) source finalSites of
            Nothing -> Just (blockedTarget tbl col ordinal finalSites blockSpan)
            Just validSites
                | length validSites < minBundleSites -> Nothing
                | otherwise ->
                    Just
                        ColumnTarget
                            { targetTable = tbl
                            , targetColumn = col
                            , targetOrdinal = ordinal
                            , targetSites = validSites
                            , targetBlocked = Nothing
                            }
    | otherwise =
        Just
            ColumnTarget
                { targetTable = tbl
                , targetColumn = col
                , targetOrdinal = ordinal
                , targetSites = finalSites
                , targetBlocked = blockedAfterInserts
                }
    where
        col = Core.columnName defined
        initSites = createSites unitIdx createSpan

        blockedStatic =
            Foldable.foldl'
                (<|>)
                Nothing
                [ blockSpan <$ guardMaybe (isNothing createSpan)
                , blockAmbiguous <$ guardMaybe createDupNames
                , blockGeneration
                    <$ guardMaybe
                        ( isJust (Core.tableLike tblDef)
                            || not (null (Core.tableSources tblDef))
                            || isJust (Core.tableOnCommit tblDef)
                            || Core.tableTemporary tblDef
                        )
                , blockGeneration
                    <$ guardMaybe (fsAll (tsGeneration scope) || Set.member col (fsTouched (tsGeneration scope)))
                , blockAmbiguous <$ guardMaybe (fsAll (tsAmb scope) || Set.member col (fsTouched (tsAmb scope)))
                , blockUnknown <$ guardMaybe (any (`Set.member` asmRelationFenced asm) (tsChain scope))
                , blockUnknown <$ guardMaybe (any (`Set.member` asmOpaqueFenced asm) (tsChain scope))
                , blockUnknown
                    <$ guardMaybe
                        (any (\s -> any (`tableInSchema` s) (tsChain scope)) (Set.toList (asmSchemaFenced asm)))
                , blockDependent <$ guardMaybe (any (`Set.member` asmDependentFenced asm) (tsChain scope))
                , blockDependent <$ guardMaybe (any (`Set.member` asmIndexOpaque asm) (tsChain scope))
                , blockConstraint <$ guardMaybe (any constraintIsOpaque (Core.tableConstraints tblDef))
                , blockConstraint
                    <$ guardMaybe
                        ( Core.columnHasDefault defined
                            || Core.columnGenerated defined
                            || not (null (Core.columnReferences defined))
                            || any (`constraintTouches` col) (Core.tableConstraints tblDef)
                        )
                , blockDependent
                    <$ guardMaybe
                        (any (maybe False (col `elem`) . (`Map.lookup` asmIndexColumns asm)) (tsChain scope))
                , blockDependent <$ guardMaybe (any (`Set.member` asmParentOpaque asm) (tsChain scope))
                , blockDependent
                    <$ guardMaybe
                        (any (maybe False (Set.member col) . (`Map.lookup` asmParentColumns asm)) (tsChain scope))
                ]

        blockedWithRefs = blockedStatic <|> referenceFence asm (tsChain scope) col

        (finalSites, blockedAfterInserts) = case blockedWithRefs of
            Just cause -> (initSites, Just cause)
            Nothing -> collectInserts initSites (asmViews asm)

        collectInserts accSites [] = (accSites, Nothing)
        collectInserts accSites (v : vs) = case uvFacts v of
            Nothing -> collectInserts accSites vs
            Just _ -> stepViewInserts accSites (cuInserts (uvColumn v)) vs
            where
                activeName = activeGenerationName scope (uvIndex v)
                stepViewInserts acc [] rest = collectInserts acc rest
                stepViewInserts acc (ins : inss) rest = case insertTarget ins of
                    Just tgt
                        | Core.nameRefName tgt == activeName ->
                            if uvIndex v < unitIdx
                                then (acc, Just blockGeneration)
                                else case insertContribution ins col ordinal createArity (uvIndex v + 1) of
                                    Left cause -> (acc, Just cause)
                                    Right newSites -> stepViewInserts (acc ++ newSites) inss rest
                    _ -> stepViewInserts acc inss rest

        guardMaybe True = Just ()
        guardMaybe False = Nothing

-- ---------------------------------------------------------------------------
-- ASCII helpers
-- ---------------------------------------------------------------------------

asciiLowerText :: Text -> Text
asciiLowerText = T.map (\c -> if isAsciiUpper c then toLower c else c)

textAsciiEq :: Text -> Text -> Bool
textAsciiEq a b = asciiLowerText a == asciiLowerText b

identAsciiEq :: Core.Ident -> Core.Ident -> Bool
identAsciiEq (Core.Ident a) (Core.Ident b) = textAsciiEq a b
