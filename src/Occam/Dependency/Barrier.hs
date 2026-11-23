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

import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Occam.Core (
    BarrierScope (..),
    Name (..),
    StatementId,
 )
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
    nameSuffix,
 )

-- Key matching and touch-key expansion
barrierKeyMatches :: BarrierKey -> BarrierKey -> Bool
barrierKeyMatches (SchemaBarrierKey left) (SchemaBarrierKey right) =
    nameSuffix left right || nameSuffix right left
barrierKeyMatches left right = left == right

relationSchemaBarrierKeys :: RelationKey -> [BarrierKey]
relationSchemaBarrierKeys (RelationKey _ name) =
    case nameParts name of
        [] -> []
        [_] -> []
        parts ->
            let prefixes = [Name (take n parts) | n <- [1 .. length parts - 1]]
                lastPrefix = [Name [last (init parts)]]
             in map SchemaBarrierKey (dedupe (prefixes ++ lastPrefix))

dedupe :: (Ord a) => [a] -> [a]
dedupe = Set.toList . Set.fromList

relationTouchKeys :: RelationKey -> [BarrierKey]
relationTouchKeys key =
    RelationBarrierKey key
        : ObjectBarrierKey (BarrierRelationKey key)
        : relationSchemaBarrierKeys key

indexTouchKeys :: IndexKey -> Maybe RelationKey -> [BarrierKey]
indexTouchKeys key table =
    ObjectBarrierKey (BarrierIndexKey key)
        : maybe [] relationTouchKeys table

triggerTouchKeys :: TriggerKey -> Maybe RelationKey -> [BarrierKey]
triggerTouchKeys key table =
    ObjectBarrierKey (BarrierTriggerKey key)
        : maybe [] relationTouchKeys table

sequenceTouchKeys :: SequenceKey -> [BarrierKey]
sequenceTouchKeys key = [ObjectBarrierKey (BarrierSequenceKey key)]

typeTouchKeys :: Name -> [BarrierKey]
typeTouchKeys name = [ObjectBarrierKey (BarrierTypeKey name)]

schemaTouchKeys :: Name -> [BarrierKey]
schemaTouchKeys name = [ObjectBarrierKey (BarrierSchemaKey name), SchemaBarrierKey name]

-- Scope lookup
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

lastBarrierDeps :: ScanState -> [BarrierKey] -> Set StatementId
lastBarrierDeps state = foldl' addLast Set.empty
    where
        barriers = scanLastBarriers state
        addLast !acc key = case Map.lookup key barriers of
            Nothing -> acc
            Just sid -> Set.insert sid acc

touchersForBarrier :: ScanState -> BarrierKey -> Set StatementId
touchersForBarrier state = \case
    SchemaBarrierKey left ->
        Set.unions
            [ touchers
            | (SchemaBarrierKey right, touchers) <- Map.toList (scanTouchers state)
            , nameSuffix left right || nameSuffix right left
            ]
    key -> Map.findWithDefault Set.empty key (scanTouchers state)
