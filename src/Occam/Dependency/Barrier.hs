-- | Barrier keys: matching, touch-key expansion, and scope lookup.
module Occam.Dependency.Barrier (
    barrierKeyMatches,
    relationSchemaBarrierKeys,
    dedupe,
    relationTouchKeys,
    indexTouchKeys,
    triggerTouchKeys,
    sequenceTouchKeys,
    typeTouchKeys,
    schemaTouchKeys,
    barrierKeysForScope,
    lastBarrierDeps,
    touchersForBarrier,
) where

import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Occam.Core.Schema (Name (..))
import Occam.Core.Semantics (BarrierScope (..))
import Occam.Core.Types (StatementId)
import Occam.Dependency.Resolve (resolveObjectKeys, resolveRelationKeys)
import Occam.Dependency.State (
    BarrierKey (..),
    Catalog,
    IndexKey,
    ObjectKey (..),
    RelationKey (..),
    ScanState (..),
    SequenceKey,
    SessionState,
    TriggerKey,
    nameLast,
    nameSuffix,
 )

-- Key matching and touch-key expansion

-- | Test whether two barrier keys conflict (symmetric suffix match for nested schemas).
barrierKeyMatches :: BarrierKey -> BarrierKey -> Bool
barrierKeyMatches (SchemaBarrierKey left) (SchemaBarrierKey right) =
    nameSuffix left right || nameSuffix right left
barrierKeyMatches left right = left == right

-- | Schema barrier keys inferred from multi-part relation qualifiers.
relationSchemaBarrierKeys :: RelationKey -> [BarrierKey]
relationSchemaBarrierKeys (RelationKey _ name) =
    case nameParts name of
        [] -> []
        [_] -> []
        parts ->
            let prefixes = [Name (take n parts) | n <- [1 .. length parts - 1]]
                lastPrefix = [Name [last (init parts)]]
             in map SchemaBarrierKey (dedupe (prefixes ++ lastPrefix))

-- | Deduplicate an 'Ord' list via set conversion.
dedupe :: (Ord a) => [a] -> [a]
dedupe = Set.toList . Set.fromList

-- | Expand a 'RelationKey' into all barrier keys touched by DDL\/DML on that relation.
relationTouchKeys :: RelationKey -> [BarrierKey]
relationTouchKeys key =
    RelationBarrierKey key
        : ObjectBarrierKey (BarrierRelationKey key)
        : relationSchemaBarrierKeys key

-- | Expand an index and its optional parent table into barrier touch keys.
indexTouchKeys :: IndexKey -> Maybe RelationKey -> [BarrierKey]
indexTouchKeys key table =
    ObjectBarrierKey (BarrierIndexKey key)
        : maybe [] relationTouchKeys table

-- | Expand a trigger and its optional parent table into barrier touch keys.
triggerTouchKeys :: TriggerKey -> Maybe RelationKey -> [BarrierKey]
triggerTouchKeys key table =
    ObjectBarrierKey (BarrierTriggerKey key)
        : maybe [] relationTouchKeys table

-- | Barrier touch keys affected by sequence DDL.
sequenceTouchKeys :: SequenceKey -> [BarrierKey]
sequenceTouchKeys key = [ObjectBarrierKey (BarrierSequenceKey key)]

-- | Barrier touch keys affected by custom type DDL.
typeTouchKeys :: Name -> [BarrierKey]
typeTouchKeys name = [ObjectBarrierKey (BarrierTypeKey name)]

-- | Barrier touch keys affected by schema DDL.
schemaTouchKeys :: Name -> [BarrierKey]
schemaTouchKeys name = [ObjectBarrierKey (BarrierSchemaKey name), SchemaBarrierKey name]

-- | Resolve a 'BarrierScope' under the active session and catalog into concrete 'BarrierKey' values.
barrierKeysForScope :: SessionState -> Catalog -> BarrierScope -> [BarrierKey]
barrierKeysForScope session catalog scope =
    case scope of
        GlobalScope -> [GlobalBarrierKey]
        SessionStateScope -> [SessionBarrierKey]
        TransactionStateScope -> [TransactionBarrierKey]
        RelationScope relation ->
            map RelationBarrierKey (resolveRelationKeys session catalog relation)
        SchemaScope name -> [SchemaBarrierKey name]
        ObjectScope objectRef ->
            map ObjectBarrierKey (resolveObjectKeys session catalog objectRef)
        UserVariableScope ident -> [UserVariableBarrierKey ident]

-- | Collect the statement IDs of the most recent barriers matching any key in the supplied list.
lastBarrierDeps :: ScanState -> [BarrierKey] -> Set StatementId
lastBarrierDeps state = Foldable.foldl' addLast Set.empty
    where
        barriers = scanLastBarriers state
        addLast !acc key = case Map.lookup key barriers of
            Nothing -> acc
            Just sid -> Set.insert sid acc

touchersForBarrier :: ScanState -> BarrierKey -> Set StatementId
touchersForBarrier state = \case
    SchemaBarrierKey left -> case nameLast left of
        Nothing ->
            Set.unions
                ( scanSchemaTouchersEmpty state
                    : [ touchers
                      | subMap <- Map.elems (scanSchemaTouchersByLast state)
                      , touchers <- Map.elems subMap
                      ]
                )
        Just ident ->
            let !matchingByLast = case Map.lookup ident (scanSchemaTouchersByLast state) of
                    Nothing -> []
                    Just subMap ->
                        [ touchers
                        | (right, touchers) <- Map.toList subMap
                        , nameSuffix left right || nameSuffix right left
                        ]
             in Set.unions (scanSchemaTouchersEmpty state : matchingByLast)
    key -> Map.findWithDefault Set.empty key (scanTouchers state)
