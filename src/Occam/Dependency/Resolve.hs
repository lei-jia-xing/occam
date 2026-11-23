-- | Name resolution: namespace prefixes, search paths, and fallback keys.
module Occam.Dependency.Resolve (
    qualifyDefinitionName,
    qualifyObjectName,
    qualifyReferenceName,
    qualifyRenameName,
    relationFallbackKey,
    sequenceFallbackKey,
    resolveRelationKeys,
    resolveSequenceKeys,
    resolveIndexKeys,
    resolveTriggerKeys,
    resolveSchemaObjectName,
    schemaIndexNames,
    schemaTriggerNames,
    resolveConstraintKeys,
    resolveObjectKeys,
) where

import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Occam.Core (
    ConstraintRef (..),
    IndexRef (..),
    Name (..),
    NameRef (..),
    TriggerRef (..),
 )
import Occam.Core qualified as Core
import Occam.Dependency.State (
    Catalog (..),
    ConstraintIdentity (..),
    ConstraintKey (..),
    IndexKey (..),
    ObjectKey (..),
    RelationKey (..),
    SequenceKey (..),
    SessionState,
    TriggerKey (..),
    effectiveNamespacePrefix,
    effectiveSearchPath,
    nameLast,
    nameSuffix,
 )

qualifyDefinitionName :: SessionState -> Name -> Bool -> Name
qualifyDefinitionName session name@(Name parts) temporary
    | temporary = name
    | length parts /= 1 = name
    | otherwise =
        case effectiveNamespacePrefix session of
            prefix@(_ : _) -> Name (prefix ++ parts)
            [] ->
                case effectiveSearchPath session of
                    Name searchParts : _ | not (null searchParts) -> Name (searchParts ++ parts)
                    _ -> name

qualifyObjectName :: SessionState -> Name -> Name
qualifyObjectName session name = qualifyDefinitionName session name False

qualifyReferenceName :: SessionState -> Name -> Name
qualifyReferenceName = qualifyObjectName

qualifyRenameName :: Name -> Name -> Name
qualifyRenameName (Name oldParts) newName@(Name newParts)
    | length oldParts > 1 && length newParts == 1 =
        Name (init oldParts ++ newParts)
    | otherwise = newName

relationFallbackKey :: SessionState -> NameRef -> RelationKey
relationFallbackKey session (NameRef name temporary)
    | temporary = RelationKey True name
    | length (nameParts name) > 1 = RelationKey False name
    | otherwise = RelationKey False (qualifyReferenceName session name)

sequenceFallbackKey :: SessionState -> NameRef -> SequenceKey
sequenceFallbackKey session (NameRef name temporary)
    | temporary = SequenceKey True name
    | length (nameParts name) > 1 = SequenceKey False name
    | otherwise = SequenceKey False (qualifyReferenceName session name)

class ScopedKey k where
    scopedKey :: Bool -> Name -> k
    scopedKeyParts :: k -> Maybe (Bool, Name)

instance ScopedKey RelationKey where
    scopedKey = RelationKey
    scopedKeyParts (RelationKey temporary name) = Just (temporary, name)

instance ScopedKey SequenceKey where
    scopedKey = SequenceKey
    scopedKeyParts (SequenceKey temporary name) = Just (temporary, name)

-- | Resolve namespaced catalog keys for relations and sequences.
resolveNamespacedKeys ::
    (Ord k, ScopedKey k) =>
    (Catalog -> Map.Map k v) ->
    (Catalog -> Name -> [k]) ->
    (SessionState -> NameRef -> k) ->
    SessionState ->
    Catalog ->
    NameRef ->
    [k]
resolveNamespacedKeys entriesOf wildcardOf fallbackKeyOf session catalog (NameRef name temporary)
    | temporary = [temporaryKey]
    | isQualified = [scopedKey False name]
    | Map.member temporaryKey entries = [temporaryKey]
    | not (null prefixKeys) = prefixKeys
    | null (effectiveSearchPath session) =
        if Map.member directKey entries
            then [directKey]
            else case wildcardMatches of
                [] -> [fallbackKey]
                matches -> matches
    | otherwise =
        case firstExisting searchKeys of
            Just key -> [key]
            Nothing -> [fallbackKey]
    where
        entries = entriesOf catalog
        isQualified = case nameParts name of
            (_ : _ : _) -> True
            _ -> False
        temporaryKey = scopedKey True name
        directKey = scopedKey False name
        fallbackKey = fallbackKeyOf session (NameRef name temporary)
        prefixKeys =
            case effectiveNamespacePrefix session of
                [] -> []
                prefix -> [scopedKey False (Name (prefix ++ nameParts name))]
        searchKeys =
            prefixKeys
                ++ [ scopedKey False (Name (nameParts entry ++ nameParts name))
                   | entry <- effectiveSearchPath session
                   ]
                ++ [directKey]
        firstExisting = find (`Map.member` entries)
        wildcardMatches = wildcardOf catalog name

wildcardScan :: (ScopedKey k) => (Catalog -> Map.Map k v) -> Catalog -> Name -> [k]
wildcardScan entriesOf catalog name
    | Map.null entries = []
    | otherwise =
        Map.foldrWithKey
            ( \key _ acc ->
                case scopedKeyParts key of
                    Just (False, stored) | nameSuffix name stored -> key : acc
                    _ -> acc
            )
            []
            entries
    where
        entries = entriesOf catalog

resolveRelationWildcard :: Catalog -> Name -> [RelationKey]
resolveRelationWildcard catalog name =
    case nameLast name of
        Nothing -> wildcardScan catalogRelations catalog name
        Just ident ->
            case Map.lookup ident (catalogRelationsByLast catalog) of
                Nothing -> []
                Just keys ->
                    [ key
                    | key <- Set.toList keys
                    , nameSuffix name (relationKeyName key)
                    ]

resolveRelationKeys :: SessionState -> Catalog -> NameRef -> [RelationKey]
resolveRelationKeys = resolveNamespacedKeys catalogRelations resolveRelationWildcard relationFallbackKey

resolveSequenceKeys :: SessionState -> Catalog -> NameRef -> [SequenceKey]
resolveSequenceKeys = resolveNamespacedKeys catalogSequences (wildcardScan catalogSequences) sequenceFallbackKey

resolveIndexKeys :: SessionState -> Catalog -> IndexRef -> [IndexKey]
resolveIndexKeys session catalog indexRef =
    case indexRef of
        NamedIndex name ->
            [ NamedIndexKey resolved
            | resolved <- resolveSchemaObjectName session (schemaIndexNames catalog) name
            ]
        TableIndex table ident ->
            [ TableIndexKey relationKey ident
            | relationKey <- resolveRelationKeys session catalog table
            ]

resolveTriggerKeys :: SessionState -> Catalog -> TriggerRef -> [TriggerKey]
resolveTriggerKeys session catalog triggerRef =
    case triggerRef of
        NamedTrigger name ->
            [ NamedTriggerKey resolved
            | resolved <- resolveSchemaObjectName session (schemaTriggerNames catalog) name
            ]
        TableTrigger table ident ->
            [ TableTriggerKey relationKey ident
            | relationKey <- resolveRelationKeys session catalog table
            ]

-- | Resolve a schema-level object name using namespace and search-path rules.
resolveSchemaObjectName :: SessionState -> [Name] -> Name -> [Name]
resolveSchemaObjectName session existing name@(Name parts)
    | length parts > 1 = [name]
    | not (null prefixKeys) = prefixKeys
    | null (effectiveSearchPath session) =
        case wildcardMatches of
            [] -> [fallback]
            matches -> matches
    | otherwise =
        case firstExisting searchKeys of
            Just matched -> [matched]
            Nothing -> [fallback]
    where
        fallback = qualifyObjectName session name
        prefixKeys =
            case effectiveNamespacePrefix session of
                [] -> []
                prefix -> [Name (prefix ++ parts)]
        searchKeys =
            prefixKeys
                ++ [ Name (nameParts entry ++ parts)
                   | entry <- effectiveSearchPath session
                   ]
                ++ [name]
        existingSet = Set.fromList existing
        firstExisting = find (`Set.member` existingSet)
        wildcardMatches =
            [ stored
            | stored <- Set.toList existingSet
            , nameSuffix name stored
            ]

schemaIndexNames :: Catalog -> [Name]
schemaIndexNames catalog =
    [ name
    | NamedIndexKey name <- Map.keys (catalogIndexes catalog)
    ]

schemaTriggerNames :: Catalog -> [Name]
schemaTriggerNames catalog =
    [ name
    | NamedTriggerKey name <- Map.keys (catalogTriggers catalog)
    ]

resolveConstraintKeys :: SessionState -> Catalog -> ConstraintRef -> [ConstraintKey]
resolveConstraintKeys session catalog (ConstraintRef table name) =
    [ ConstraintKey relationKey (NamedConstraint name)
    | relationKey <- resolveRelationKeys session catalog table
    ]

resolveObjectKeys :: SessionState -> Catalog -> Core.ObjectRef -> [ObjectKey]
resolveObjectKeys session catalog objectRef =
    case objectRef of
        Core.ObjectRelation relation ->
            map BarrierRelationKey (resolveRelationKeys session catalog relation)
        Core.ObjectIndex index ->
            map BarrierIndexKey (resolveIndexKeys session catalog index)
        Core.ObjectTrigger trigger ->
            map BarrierTriggerKey (resolveTriggerKeys session catalog trigger)
        Core.ObjectSequence sequenceRef ->
            map BarrierSequenceKey (resolveSequenceKeys session catalog sequenceRef)
        Core.ObjectType name -> [BarrierTypeKey name]
        Core.ObjectSchema name -> [BarrierSchemaKey name]
        Core.ObjectConstraint constraintRef ->
            map BarrierConstraintKey (resolveConstraintKeys session catalog constraintRef)
