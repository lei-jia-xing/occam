-- | Cascade-write expansion for cross-statement foreign key effects.
module Occam.Dependency.Enforcement (
    cascadeWrites,
) where

import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Occam.Core (
    AlterChange (..),
    AlterTable (..),
    CatalogOp (..),
    ColumnDef (..),
    Constraint (..),
    DataWrite (..),
    DeleteScope (..),
    DropObject (..),
    DropOp (..),
    ForeignKey (..),
    Ident (..),
    Name (..),
    NameRef (..),
    ReferentialAction (..),
    RenameOp (..),
    SearchPathEntry (..),
    SemanticFacts (..),
    SessionEffect (..),
    SessionInit (..),
    SessionScope (..),
    Statement (..),
    StatementId,
    TableDef (..),
    UpdateScope (..),
    WriteKind (..),
 )
import Occam.Facts (StatementFacts (..), StatementUnit (..))

-- | Temp flag plus lowercased qualified name components.
data TableKey = TableKey !Bool ![Text]
    deriving (Eq, Ord, Show)

tableKeyOf :: NameRef -> TableKey
tableKeyOf (NameRef (Name parts) temporary) =
    TableKey temporary (map lowerPart parts)
    where
        lowerPart = T.toLower . identText

-- | Resolve a table reference against the accumulated catalog.
resolveTableKey :: SessionInit -> Map TableKey TableInfo -> NameRef -> Maybe TableKey
resolveTableKey initial catalog ref
    | Map.member direct catalog = Just direct
    | nameRefTemp ref = Nothing
    | length parts > 1 = Nothing
    | otherwise = firstExisting `orElse` uniqueSuffix
    where
        direct = tableKeyOf ref
        parts = nameParts (nameRefName ref)
        lowerParts = map (T.toLower . identText) parts
        namespaceKeys =
            [ TableKey False (map (T.toLower . identText) (sessionNamespacePrefix initial) ++ lowerParts)
            | not (null (sessionNamespacePrefix initial))
            ]
        searchPathKeys =
            [ TableKey False (pathParts ++ lowerParts)
            | entry <- sessionSearchPath initial
            , pathParts <- case entry of
                SearchPathName (Name prefix) -> [map (T.toLower . identText) prefix]
                SearchPathCurrentUser -> maybe [] (\user -> [[T.toLower (identText user)]]) (sessionCurrentUser initial)
            ]
        searchKeys = namespaceKeys ++ searchPathKeys ++ [direct]
        firstExisting = find (`Map.member` catalog) searchKeys
        uniqueSuffix
            | null lowerParts = Nothing
            | otherwise =
                case [ key
                     | key@(TableKey False stored) <- Map.keys catalog
                     , not (null stored)
                     , last stored == last lowerParts
                     ] of
                    [key] -> Just key
                    _ -> Nothing
        orElse (Just key) _ = Just key
        orElse Nothing fallback = fallback

lowerIdent :: Ident -> Text
lowerIdent = T.toLower . identText

data TableInfo = TableInfo
    { infoRef :: !NameRef
    , infoPK :: ![Text]
    , infoFKs :: ![ForeignKey]
    }
    deriving (Eq, Show)

collectTable :: TableDef -> TableInfo
collectTable def =
    TableInfo
        { infoRef = NameRef (tableName def) (tableTemporary def)
        , infoPK =
            [ lowerIdent column
            | PrimaryKeyConstraint _ columns <- tableConstraints def
            , column <- columns
            ]
        , infoFKs =
            [foreignKey | ForeignKeyConstraint foreignKey <- tableConstraints def]
                ++ [foreignKey | column <- tableColumns def, foreignKey <- columnReferences column]
        }

-- | Per-statement cascade expansion against accumulated flag and catalog.
cascadeWrites :: SessionInit -> [StatementUnit] -> Map StatementId [DataWrite]
cascadeWrites initial = go (sessionForeignKeyChecks initial) Map.empty
    where
        go _ _ [] = Map.empty
        go enforced catalog (unit : rest) =
            let sid = stmtId (unitStatement unit)
                (ops, effects) = case unitFacts unit of
                    Nothing -> ([], [])
                    Just facts -> let semantics = factSemantics facts in (factOps semantics, factSession semantics)
                extras
                    | enforced = concatMap (expand initial catalog) ops
                    | otherwise = []
                catalog' = foldl' (applyOp initial) catalog ops
                enforced' = foldl' (applySession (sessionForeignKeyChecks initial)) enforced effects
                tailMap = go enforced' catalog' rest
             in if null extras then tailMap else Map.insert sid extras tailMap

-- | Mirrors 'Occam.Dependency.Session.applySessionEffect'.
applySession :: Bool -> Bool -> SessionEffect -> Bool
applySession initial current effect = case effect of
    SetForeignKeyChecks ScopeSession value -> value
    ResetForeignKeyChecks ScopeSession -> initial
    ResetAll ScopeSession -> initial
    _ -> current

applyOp :: SessionInit -> Map TableKey TableInfo -> CatalogOp -> Map TableKey TableInfo
applyOp initial catalog op = case op of
    DefineTableOp def ->
        let info = collectTable def
         in Map.insert (tableKeyOf (infoRef info)) info catalog
    AlterTableOp alter ->
        foldl' (applyChange initial (alterTableTarget alter)) catalog (alterTableChanges alter)
    DropCatalogOp dropOp -> foldl' (applyDrop initial) catalog (dropObjects dropOp)
    RenameOp rename -> applyRename initial rename catalog
    _ -> catalog

ensureTable :: SessionInit -> NameRef -> Map TableKey TableInfo -> Map TableKey TableInfo
ensureTable initial ref catalog =
    let key = fromMaybe (tableKeyOf ref) (resolveTableKey initial catalog ref)
     in if Map.member key catalog then catalog else Map.insert key (TableInfo ref [] []) catalog

applyChange ::
    SessionInit -> NameRef -> Map TableKey TableInfo -> AlterChange -> Map TableKey TableInfo
applyChange initial target catalog change = case change of
    AddConstraint (ForeignKeyConstraint foreignKey) ->
        Map.adjust appendFK key (ensureTable initial target catalog)
        where
            appendFK info = info {infoFKs = infoFKs info ++ [foreignKey]}
    AddConstraint (PrimaryKeyConstraint _ columns) ->
        Map.adjust appendPK key (ensureTable initial target catalog)
        where
            appendPK info = info {infoPK = infoPK info ++ map lowerIdent columns}
    AddColumn column extras ->
        Map.adjust appendBoth key (ensureTable initial target catalog)
        where
            appendBoth info =
                info
                    { infoFKs =
                        infoFKs info
                            ++ [foreignKey | ForeignKeyConstraint foreignKey <- extras]
                            ++ columnReferences column
                    , infoPK =
                        infoPK info
                            ++ [ lowerIdent columnName
                               | PrimaryKeyConstraint _ columns <- extras
                               , columnName <- columns
                               ]
                    }
    DropConstraint name _ -> Map.adjust removeFK key catalog
        where
            removeFK info =
                info {infoFKs = [foreignKey | foreignKey <- infoFKs info, foreignKeyName foreignKey /= Just name]}
    _ -> catalog
    where
        key = fromMaybe (tableKeyOf target) (resolveTableKey initial catalog target)

applyDrop :: SessionInit -> Map TableKey TableInfo -> DropObject -> Map TableKey TableInfo
applyDrop initial catalog (DropRelation ref _) =
    case resolveTableKey initial catalog ref of
        Just key -> Map.delete key catalog
        Nothing -> catalog
applyDrop _ catalog (DropSchema schema) =
    Map.filterWithKey (\key _ -> notInSchema key) catalog
    where
        schemaParts = map lowerIdent (nameParts schema)
        -- Schema-qualified tables carry the schema as their name prefix.
        notInSchema (TableKey False parts) =
            null schemaParts
                || length parts <= length schemaParts
                || take (length schemaParts) parts /= schemaParts
        notInSchema _ = True
applyDrop _ catalog _ = catalog

applyRename :: SessionInit -> RenameOp -> Map TableKey TableInfo -> Map TableKey TableInfo
applyRename initial (RenameRelation old new _) catalog =
    case resolveTableKey initial catalog old of
        Nothing -> catalog
        Just oldKey ->
            case Map.lookup oldKey catalog of
                Nothing -> catalog
                Just info ->
                    Map.insert newKey (info {infoRef = NameRef new (nameRefTemp old)}) (Map.delete oldKey catalog)
    where
        newKey = tableKeyOf (NameRef new (nameRefTemp old))
applyRename initial (RenameColumnOp target old new) catalog =
    renameColumn initial target old new catalog

renameColumn ::
    SessionInit -> NameRef -> Ident -> Ident -> Map TableKey TableInfo -> Map TableKey TableInfo
renameColumn initial targetRef old new catalog = Map.mapWithKey fix catalog
    where
        target = fromMaybe (tableKeyOf targetRef) (resolveTableKey initial catalog targetRef)
        oldLower = lowerIdent old
        fix key info
            | key == target =
                info
                    { infoPK = renameLowered (infoPK info)
                    , infoFKs =
                        [ foreignKey {foreignKeyChildColumns = renameIdents (foreignKeyChildColumns foreignKey)}
                        | foreignKey <- infoFKs info
                        ]
                    }
            | otherwise =
                info
                    { infoFKs =
                        [ if resolveTableKey initial catalog (foreignKeyParent foreignKey) == Just target
                            then foreignKey {foreignKeyParentColumns = renameIdents (foreignKeyParentColumns foreignKey)}
                            else foreignKey
                        | foreignKey <- infoFKs info
                        ]
                    }
        renameLowered = map (\column -> if column == oldLower then lowerIdent new else column)
        renameIdents = map (\column -> if lowerIdent column == oldLower then new else column)

expand :: SessionInit -> Map TableKey TableInfo -> CatalogOp -> [DataWrite]
expand initial catalog (WriteDataOp write) = expandWrite initial catalog write
expand _ _ _ = []

-- | Breadth-first fan-out; visited guard is per (table, write flavor, columns).
expandWrite :: SessionInit -> Map TableKey TableInfo -> DataWrite -> [DataWrite]
expandWrite initial catalog write = case dataWriteKind write of
    DeleteWrite _ -> bfs Set.empty [(keyOfTarget, True, [])]
    UpdateWrite _ -> bfs Set.empty [(keyOfTarget, False, writeColumns)]
    _ -> []
    where
        keyOfTarget =
            fromMaybe
                (tableKeyOf (dataWriteTarget write))
                (resolveTableKey initial catalog (dataWriteTarget write))
        writeColumns = map lowerIdent (dataWriteColumns write ++ dataWriteUnqualifiedColumns write)

        bfs :: Set (TableKey, Bool, Set Text) -> [(TableKey, Bool, [Text])] -> [DataWrite]
        bfs _ [] = []
        bfs visited ((key, isDelete, columns) : rest)
            | Set.member guard visited = bfs visited rest
            | otherwise =
                let children = cascadeChildren key isDelete columns
                    writes =
                        [ childWrite ref childIsDelete childColumns
                        | (_, ref, childIsDelete, childColumns) <- children
                        ]
                    queued =
                        [ (childKey, childIsDelete, map lowerIdent childColumns)
                        | (childKey, _, childIsDelete, childColumns) <- children
                        ]
                 in writes ++ bfs (Set.insert guard visited) (rest ++ queued)
            where
                guard = (key, isDelete, Set.fromList columns)

        childWrite :: NameRef -> Bool -> [Ident] -> DataWrite
        childWrite ref True _ = DataWrite ref (DeleteWrite DeleteAllRows) [] [] [] False
        childWrite ref False childColumns = DataWrite ref (UpdateWrite UpdateFilteredRows) childColumns [] [] False

        cascadeChildren :: TableKey -> Bool -> [Text] -> [(TableKey, NameRef, Bool, [Ident])]
        cascadeChildren parentKey isDelete columns =
            [ (childKey, infoRef info, childIsDelete, foreignKeyChildColumns foreignKey)
            | (childKey, info) <- Map.toList catalog
            , foreignKey <- infoFKs info
            , resolveTableKey initial catalog (foreignKeyParent foreignKey) == Just parentKey
            , Just childIsDelete <- [matchAction foreignKey]
            ]
            where
                matchAction :: ForeignKey -> Maybe Bool
                matchAction foreignKey
                    | isDelete = case foreignKeyOnDelete foreignKey of
                        Cascade -> Just True
                        SetNull -> Just False
                        SetDefault -> Just False
                        _ -> Nothing
                    | otherwise = case foreignKeyOnUpdate foreignKey of
                        Cascade -> overlap
                        SetNull -> overlap
                        SetDefault -> overlap
                        _ -> Nothing
                    where
                        overlap
                            | null columns = Just False
                            | null parentColumns = Just False
                            | any (`elem` parentColumns) columns = Just False
                            | otherwise = Nothing
                        parentColumns
                            | not (null (foreignKeyParentColumns foreignKey)) =
                                map lowerIdent (foreignKeyParentColumns foreignKey)
                            | otherwise = maybe [] infoPK (Map.lookup parentKey catalog)
