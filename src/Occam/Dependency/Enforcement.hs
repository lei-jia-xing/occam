-- | Cascade-write expansion for cross-statement foreign key effects.
module Occam.Dependency.Enforcement (
    cascadeWrites,
) where

import Data.Foldable qualified as Foldable
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Occam.Core.Schema (
    AlterChange (..),
    AlterTable (..),
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
    TableDef (..),
    UpdateScope (..),
    WriteKind (..),
 )
import Occam.Core.Semantics (
    CatalogOp (..),
    SearchPathEntry (..),
    SemanticFacts (..),
    SessionEffect (..),
    SessionInit (..),
    SessionScope (..),
 )
import Occam.Core.Types (Statement (..), StatementId)
import Occam.Dependency.State (initialScanState, transactionActive)
import Occam.Dependency.Transaction (applyTxEffects, finishStatement)
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
cascadeWrites initial = go (sessionForeignKeyChecks initial) Map.empty (initialScanState initial)
    where
        go _ _ _ [] = Map.empty
        go enforced catalog transaction (unit : rest) =
            let sid = stmtId (unitStatement unit)
                (ops, effects, preTx, postTx) = case unitFacts unit of
                    Nothing -> ([], [], [], [])
                    Just facts ->
                        let semantics = factSemantics facts
                         in (factOps semantics, factSession semantics, factPreTx semantics, factPostTx semantics)
                (before, _) = applyTxEffects sid preTx transaction
                (after, _) = applyTxEffects sid postTx before
                extras
                    | enforced = concatMap (expand initial catalog) ops
                    | otherwise = []
                catalog' = Foldable.foldl' (applyOp initial) catalog ops
                enforced' =
                    Foldable.foldl'
                        (applySession (sessionForeignKeyChecks initial) (transactionActive before))
                        enforced
                        effects
                tailMap = go enforced' catalog' (finishStatement sid after) rest
             in if null extras then tailMap else Map.insert sid extras tailMap

-- | Mirror the enforcement flag rules in the graph's session scan.
applySession :: Bool -> Bool -> Bool -> SessionEffect -> Bool
applySession initial active current effect = case effect of
    SetForeignKeyChecks ScopeSession value -> value
    SetSqliteForeignKeys value | not active -> value
    ResetForeignKeyChecks ScopeSession -> initial
    ResetAll ScopeSession -> initial
    _ -> current

applyOp :: SessionInit -> Map TableKey TableInfo -> CatalogOp -> Map TableKey TableInfo
applyOp initial catalog op = case op of
    DefineTableOp def ->
        let info = collectTable def
         in Map.insert (tableKeyOf (infoRef info)) info catalog
    AlterTableOp alter ->
        Foldable.foldl' (applyChange initial (alterTableTarget alter)) catalog (alterTableChanges alter)
    DropCatalogOp dropOp -> Foldable.foldl' (applyDrop initial) catalog (dropObjects dropOp)
    RenameOp rename -> applyRename initial rename catalog
    _ -> catalog

ensureTable :: SessionInit -> NameRef -> Map TableKey TableInfo -> Map TableKey TableInfo
ensureTable initial ref catalog =
    let key = fromMaybe (tableKeyOf ref) (resolveTableKey initial catalog ref)
     in if Map.member key catalog then catalog else Map.insert key (TableInfo ref [] []) catalog

applyChange ::
    SessionInit -> NameRef -> Map TableKey TableInfo -> AlterChange -> Map TableKey TableInfo
applyChange initial target catalog = \case
    AddConstraint (ForeignKeyConstraint foreignKey) ->
        Map.adjust (\info -> info {infoFKs = infoFKs info ++ [foreignKey]}) key ensured
    AddConstraint (PrimaryKeyConstraint _ columns) ->
        Map.adjust (\info -> info {infoPK = infoPK info ++ map lowerIdent columns}) key ensured
    AddColumn column extras ->
        Map.adjust (appendColumn column extras) key ensured
    DropConstraint name _ ->
        Map.adjust (removeNamedFK name) key catalog
    _ -> catalog
    where
        key = fromMaybe (tableKeyOf target) (resolveTableKey initial catalog target)
        ensured = ensureTable initial target catalog
        appendColumn column extras info =
            info
                { infoFKs =
                    infoFKs info
                        ++ [foreignKey | ForeignKeyConstraint foreignKey <- extras]
                        ++ columnReferences column
                , infoPK =
                    infoPK info
                        ++ [lowerIdent columnName | PrimaryKeyConstraint _ columns <- extras, columnName <- columns]
                }
        removeNamedFK name info =
            info {infoFKs = [fk | fk <- infoFKs info, foreignKeyName fk /= Just name]}

applyDrop :: SessionInit -> Map TableKey TableInfo -> DropObject -> Map TableKey TableInfo
applyDrop initial catalog (DropRelation ref _) =
    maybe catalog (`Map.delete` catalog) (resolveTableKey initial catalog ref)
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
    applyRenameRelation initial old new catalog
applyRename initial (RenamePermanentRelation old new _) catalog =
    applyRenameRelation initial (old {nameRefTemp = False}) new catalog
applyRename initial (RenameColumnOp target old new) catalog =
    renameColumn initial target old new catalog

applyRenameRelation ::
    SessionInit -> NameRef -> Name -> Map TableKey TableInfo -> Map TableKey TableInfo
applyRenameRelation initial old new catalog =
    fromMaybe catalog do
        oldKey <- resolveTableKey initial catalog old
        info <- Map.lookup oldKey catalog
        let newRef = NameRef new (nameRefTemp old)
        pure (Map.insert (tableKeyOf newRef) (info {infoRef = newRef}) (Map.delete oldKey catalog))

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
                    , infoFKs = map renameChildCols (infoFKs info)
                    }
            | otherwise =
                info {infoFKs = map renameParentCols (infoFKs info)}
        renameChildCols fk = fk {foreignKeyChildColumns = renameIdents (foreignKeyChildColumns fk)}
        renameParentCols fk
            | resolveTableKey initial catalog (foreignKeyParent fk) == Just target =
                fk {foreignKeyParentColumns = renameIdents (foreignKeyParentColumns fk)}
            | otherwise = fk
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
            | Set.member guardKey visited = bfs visited rest
            | otherwise = writes ++ bfs (Set.insert guardKey visited) (rest ++ queued)
            where
                guardKey = (key, isDelete, Set.fromList columns)
                children = cascadeChildren initial catalog key isDelete columns
                writes = [childWrite ref childIsDelete childColumns | (_, ref, childIsDelete, childColumns) <- children]
                queued =
                    [ (childKey, childIsDelete, map lowerIdent childColumns)
                    | (childKey, _, childIsDelete, childColumns) <- children
                    ]

        childWrite :: NameRef -> Bool -> [Ident] -> DataWrite
        childWrite ref True _ = DataWrite ref (DeleteWrite DeleteFilteredRows) [] [] [] False
        childWrite ref False childColumns = DataWrite ref (UpdateWrite UpdateFilteredRows) childColumns [] [] False

cascadeChildren ::
    SessionInit ->
    Map TableKey TableInfo ->
    TableKey ->
    Bool ->
    [Text] ->
    [(TableKey, NameRef, Bool, [Ident])]
cascadeChildren initial catalog parentKey isDelete columns =
    [ (childKey, infoRef info, childIsDelete, foreignKeyChildColumns foreignKey)
    | (childKey, info) <- Map.toList catalog
    , foreignKey <- infoFKs info
    , resolveTableKey initial catalog (foreignKeyParent foreignKey) == Just parentKey
    , Just childIsDelete <- [matchCascadeAction catalog parentKey isDelete columns foreignKey]
    ]

matchCascadeAction ::
    Map TableKey TableInfo -> TableKey -> Bool -> [Text] -> ForeignKey -> Maybe Bool
matchCascadeAction catalog parentKey isDelete columns foreignKey
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
        parentColumns
            | not (null (foreignKeyParentColumns foreignKey)) =
                map lowerIdent (foreignKeyParentColumns foreignKey)
            | otherwise = maybe [] infoPK (Map.lookup parentKey catalog)
        overlap
            | null columns || null parentColumns || any (`elem` parentColumns) columns = Just False
            | otherwise = Nothing
