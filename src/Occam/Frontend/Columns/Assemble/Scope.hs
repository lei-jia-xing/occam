-- | Relation generation chains, table scopes, and definition planning.
module Occam.Frontend.Columns.Assemble.Scope (
    activeGenerationName,
    buildScope,
    hasDuplicateNames,
    planTables,
    tableInSchema,
) where

import Data.Char (isAsciiUpper, toLower)
import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Occam.Core.Schema qualified as Core
import Occam.Core.Semantics qualified as Core
import Occam.Frontend.Columns.Analysis (
    ColumnUnit (..),
    InsertColumns (..),
    InsertList (..),
    InsertShape (..),
 )
import Occam.Frontend.Columns.Assemble.Types (
    Assembly (..),
    FenceSet (..),
    RelRename (..),
    TablePlan (..),
    TableScope (..),
    UnitRefs (..),
    UnitView (..),
    emptyFenceSet,
 )

-- | Resolve the relation name reached by rename edges before a unit index.
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

-- | Match a relation to a fenced schema, including common unqualified defaults.
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

-- ---------------------------------------------------------------------------
-- Constraints, generation chains, and table scopes
-- ---------------------------------------------------------------------------

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

-- | Build the generation chain and conservative fences for one definition.
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

-- | Detect duplicate names that make ordinal column bindings ambiguous.
hasDuplicateNames :: [Core.ColumnDef] -> Bool
hasDuplicateNames = go Set.empty
    where
        go _ [] = False
        go seen (c : rest)
            | Set.member (Core.columnName c) seen = True
            | otherwise = go (Set.insert (Core.columnName c) seen) rest

-- | Group table definitions while preserving first-seen and source order.
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
-- ASCII helpers
-- ---------------------------------------------------------------------------

asciiLowerText :: Text -> Text
asciiLowerText = T.map (\c -> if isAsciiUpper c then toLower c else c)

textAsciiEq :: Text -> Text -> Bool
textAsciiEq a b = asciiLowerText a == asciiLowerText b

identAsciiEq :: Core.Ident -> Core.Ident -> Bool
identAsciiEq (Core.Ident a) (Core.Ident b) = textAsciiEq a b
