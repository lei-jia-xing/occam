-- | Name resolution: namespace prefixes, search paths, and fallback keys.
module Occam.Dependency.Resolve (
    qualifyDefinitionName,
    qualifyObjectName,
    qualifyReferenceName,
    qualifyRenameName,
    relationFallbackKey,
    sequenceFallbackKey,
    resolveRelationKeys,
    resolvePermanentRelationKeys,
    resolveViewSourceKeys,
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
import Occam.Core.Schema (
    ConstraintRef (..),
    IndexRef (..),
    Name (..),
    NameRef (..),
    TriggerRef (..),
 )
import Occam.Core.Semantics qualified as Core
import Occam.Dependency.State (
    Catalog (..),
    ConstraintIdentity (..),
    ConstraintKey (..),
    IndexKey (..),
    ObjectKey (..),
    RelationKey (..),
    SequenceKey (..),
    SessionState (..),
    TriggerKey (..),
    effectiveNamespacePrefix,
    effectiveSearchPath,
    nameLast,
    nameSuffix,
 )

-- | Qualify a newly defined object name according to session namespace prefix and @search_path@.
qualifyDefinitionName :: SessionState -> Name -> Bool -> Name
qualifyDefinitionName session name@(Name parts) temporary
    | length parts /= 1 = name
    | temporary && not (sessTempInNamespace session) = name
    | otherwise =
        case effectiveNamespacePrefix session of
            prefix@(_ : _) -> Name (prefix ++ parts)
            []
                | temporary -> name
                | otherwise ->
                    case effectiveSearchPath session of
                        Name searchParts : _ | not (null searchParts) -> Name (searchParts ++ parts)
                        _ -> name

-- | Qualify a permanent object name using active session schema rules.
qualifyObjectName :: SessionState -> Name -> Name
qualifyObjectName session name = qualifyDefinitionName session name False

-- | Alias for 'qualifyObjectName' when resolving object references.
qualifyReferenceName :: SessionState -> Name -> Name
qualifyReferenceName = qualifyObjectName

-- | Qualify the target name of a @RENAME@ operation, preserving the old schema when unqualified.
qualifyRenameName :: Name -> Name -> Name
qualifyRenameName (Name oldParts) newName@(Name newParts)
    | length oldParts > 1 && length newParts == 1 =
        Name (init oldParts ++ newParts)
    | otherwise = newName

-- | Fallback 'RelationKey' derived solely from session rules when no matching catalog entry exists.
relationFallbackKey :: SessionState -> NameRef -> RelationKey
relationFallbackKey session (NameRef name temporary)
    | length (nameParts name) > 1 = RelationKey temporary name
    | temporary = RelationKey True (qualifyDefinitionName session name True)
    | otherwise = RelationKey False (qualifyReferenceName session name)

-- | Fallback 'SequenceKey' derived solely from session rules when no matching sequence exists.
sequenceFallbackKey :: SessionState -> NameRef -> SequenceKey
sequenceFallbackKey session (NameRef name temporary)
    | length (nameParts name) > 1 = SequenceKey temporary name
    | temporary = SequenceKey True (qualifyDefinitionName session name True)
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
    Bool ->
    (Catalog -> Map.Map k v) ->
    (Catalog -> Name -> [k]) ->
    (SessionState -> NameRef -> k) ->
    SessionState ->
    Catalog ->
    NameRef ->
    [k]
resolveNamespacedKeys allowTemp entriesOf wildcardOf fallbackKeyOf session catalog (NameRef name temporary)
    | temporary && not allowTemp = []
    | temporary = [qualifiedTempKey]
    | isQualified =
        if allowTemp && sessTempInNamespace session && Map.member temporaryKey entries
            then [temporaryKey]
            else [scopedKey False name]
    | allowTemp && Map.member qualifiedTempKey entries = [qualifiedTempKey]
    | not (null prefixKeys) = prefixKeys
    | null (effectiveSearchPath session) =
        if Map.member directKey entries
            then [directKey]
            else case wildcardMatches of
                [] -> [permFallbackKey]
                matches -> matches
    | otherwise =
        case firstExisting searchKeys of
            Just key -> [key]
            Nothing -> [permFallbackKey]
    where
        entries = entriesOf catalog
        isQualified = case nameParts name of
            (_ : _ : _) -> True
            _ -> False
        temporaryKey = scopedKey True name
        qualifiedTempKey =
            if sessTempInNamespace session && not isQualified
                then case effectiveNamespacePrefix session of
                    [] -> temporaryKey
                    prefix -> scopedKey True (Name (prefix ++ nameParts name))
                else temporaryKey
        directKey = scopedKey False name
        permFallbackKey = fallbackKeyOf session (NameRef name False)
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

-- | Resolve all matching permanent and temporary relation keys in the catalog.
resolveRelationKeys :: SessionState -> Catalog -> NameRef -> [RelationKey]
resolveRelationKeys = resolveNamespacedKeys True catalogRelations resolveRelationWildcard relationFallbackKey

-- | Resolve only permanent (non-temporary) relation keys in the catalog.
resolvePermanentRelationKeys :: SessionState -> Catalog -> NameRef -> [RelationKey]
resolvePermanentRelationKeys = resolveNamespacedKeys False catalogRelations resolveRelationWildcard relationFallbackKey

-- | Resolve relation references inside a view definition, honoring temp-shadowing rules.
resolveViewSourceKeys :: SessionState -> Bool -> Catalog -> NameRef -> [RelationKey]
resolveViewSourceKeys session viewIsTemp catalog ref
    | not viewIsTemp && sessPermViewIgnoresTemp session =
        resolvePermanentRelationKeys session catalog ref
    | otherwise = resolveRelationKeys session catalog ref

-- | Resolve all matching temporary and permanent sequence keys in the catalog.
resolveSequenceKeys :: SessionState -> Catalog -> NameRef -> [SequenceKey]
resolveSequenceKeys = resolveNamespacedKeys True catalogSequences (wildcardScan catalogSequences) sequenceFallbackKey

-- | Resolve index references (named or table-qualified) against catalog metadata.
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

-- | Resolve trigger references (named or table-qualified) against catalog metadata.
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
