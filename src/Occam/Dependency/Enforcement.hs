{- | FK-enforcement cascade-write expansion for @--dump-facts@ cross-validation.

The dependency graph already models cascade writes conditional on the
threaded enforcement flag ('Occam.Dependency.Write.applyForeignKeys'):
with enforcement OFF a parent write touches only its target, with it ON
@ON DELETE/UPDATE CASCADE@ (and @SET NULL@/@SET DEFAULT@) fan out to
children, transitively. @--dump-facts@, however, reports raw per-statement
facts with no cross-statement session or catalog state, so under @--fk-on@
validation the engine reports cascade writes the dump never mentions.
This module replays just enough scan state (enforcement flag plus FK
catalog) to expand the cascade writes each statement performs.

== Rule (mirrors 'applyParentActions')

Statements are scanned in file order. Each statement expands against the
flag and catalog accumulated from earlier statements; its own session
effects and schema changes apply afterwards, exactly as in
'Occam.Dependency.step':

* enforcement OFF: no expansion;
* enforcement ON: @DELETE@ expands through @ON DELETE CASCADE@ and
  @SET NULL@/@SET DEFAULT@ (the latter two as child @UPDATE@s, matching
  the engine's authorizer output); @UPDATE@ expands through the matching
  @ON UPDATE@ actions only when the written columns overlap the referenced
  parent columns (explicit list, else the parent primary key), and an
  empty write-column list cascades conservatively. Expansion recurses
  multi-level with a visited guard, so self-referential and cyclic FKs
  terminate. @NoAction@ and @Restrict@ add only reads and are skipped.

== Mode-dependent precision

The initial flag is the frontend-resolved session ('SessionInit'): the
dialect default (@OFF@ for SQLite) unless @--dump-facts-fk-on@ (or an
explicit server setting) overrides it. In-file @PRAGMA foreign_keys@ and
@SET foreign_key_checks@ assignments then thread forward exactly as the
engine replay executes them, so occam's per-statement flag agrees with the
engine's in both harness modes. Default mode (initial OFF, no in-file
toggles in the corpus) expands nothing and stays byte-identical; @--fk-on@
(initial ON) gains exactly the engine-reported cascade writes.

Deliberately unmodeled: FK check __reads__. The engine's read set includes
implementation scan columns no static fact pins down (e.g. a cascaded
delete also reports the child's primary key alongside its FK columns), so
reads stay raw in both modes and remain triaged mismatches under @--fk-on@.
@TRUNCATE@ cascades are skipped as well: SQLite (the validated dialect)
has no @TRUNCATE@, and the harness flags any @truncate@ write outright.
-}
module Occam.Dependency.Enforcement (
    cascadeWrites,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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
    SemanticFacts (..),
    SessionEffect (..),
    SessionInit (..),
    SessionScope (..),
    Statement (..),
    StatementId,
    TableDef (..),
    WriteKind (..),
 )
import Occam.Facts (StatementFacts (..), StatementUnit (..))

{- | Table identity for enforcement replay: temp flag plus the lowercased
last name component, matching the harness's case-insensitive
last-component comparison.
-}
data TableKey = TableKey !Bool !Text
    deriving (Eq, Ord, Show)

tableKeyOf :: NameRef -> TableKey
tableKeyOf ref = TableKey (nameRefTemp ref) (lastComponent (nameRefName ref))
    where
        lastComponent (Name parts) = case parts of
            [] -> T.empty
            _ -> T.toLower (identText (last parts))

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

{- | Scan statements in order, expanding cascade writes against the
enforcement flag and FK catalog in effect at each writing statement.
-}
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
                    | enforced = concatMap (expand catalog) ops
                    | otherwise = []
                catalog' = foldl' applyOp catalog ops
                enforced' = foldl' (applySession (sessionForeignKeyChecks initial)) enforced effects
                tailMap = go enforced' catalog' rest
             in if null extras then tailMap else Map.insert sid extras tailMap

{- | Session threading mirrors 'Occam.Dependency.Session.applySessionEffect':
only session-scoped assignments take effect; global and transactional
scopes leave the current session untouched.
-}
applySession :: Bool -> Bool -> SessionEffect -> Bool
applySession initial current effect = case effect of
    SetForeignKeyChecks ScopeSession value -> value
    ResetForeignKeyChecks ScopeSession -> initial
    ResetAll ScopeSession -> initial
    _ -> current

applyOp :: Map TableKey TableInfo -> CatalogOp -> Map TableKey TableInfo
applyOp catalog op = case op of
    DefineTableOp def ->
        let info = collectTable def
         in Map.insert (tableKeyOf (infoRef info)) info catalog
    AlterTableOp alter ->
        foldl' (applyChange (alterTableTarget alter)) catalog (alterTableChanges alter)
    DropCatalogOp dropOp -> foldl' applyDrop catalog (dropObjects dropOp)
    RenameOp rename -> applyRename rename catalog
    _ -> catalog

ensureTable :: NameRef -> Map TableKey TableInfo -> Map TableKey TableInfo
ensureTable ref catalog =
    let key = tableKeyOf ref
     in if Map.member key catalog then catalog else Map.insert key (TableInfo ref [] []) catalog

applyChange :: NameRef -> Map TableKey TableInfo -> AlterChange -> Map TableKey TableInfo
applyChange target catalog change = case change of
    AddConstraint (ForeignKeyConstraint foreignKey) ->
        Map.adjust appendFK key (ensureTable target catalog)
        where
            appendFK info = info {infoFKs = infoFKs info ++ [foreignKey]}
    AddConstraint (PrimaryKeyConstraint _ columns) ->
        Map.adjust appendPK key (ensureTable target catalog)
        where
            appendPK info = info {infoPK = infoPK info ++ map lowerIdent columns}
    AddColumn column extras ->
        Map.adjust appendBoth key (ensureTable target catalog)
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
        key = tableKeyOf target

applyDrop :: Map TableKey TableInfo -> DropObject -> Map TableKey TableInfo
applyDrop catalog (DropRelation ref _) = Map.delete (tableKeyOf ref) catalog
applyDrop catalog _ = catalog

applyRename :: RenameOp -> Map TableKey TableInfo -> Map TableKey TableInfo
applyRename (RenameRelation old new _) catalog =
    case Map.lookup oldKey catalog of
        Nothing -> catalog
        Just info ->
            Map.insert newKey (info {infoRef = NameRef new (nameRefTemp old)}) (Map.delete oldKey catalog)
    where
        oldKey = tableKeyOf old
        newKey = tableKeyOf (NameRef new (nameRefTemp old))
applyRename (RenameColumnOp target old new) catalog = renameColumn (tableKeyOf target) old new catalog

renameColumn :: TableKey -> Ident -> Ident -> Map TableKey TableInfo -> Map TableKey TableInfo
renameColumn target old new = Map.mapWithKey fix
    where
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
                        [ if tableKeyOf (foreignKeyParent foreignKey) == target
                            then foreignKey {foreignKeyParentColumns = renameIdents (foreignKeyParentColumns foreignKey)}
                            else foreignKey
                        | foreignKey <- infoFKs info
                        ]
                    }
        renameLowered = map (\column -> if column == oldLower then lowerIdent new else column)
        renameIdents = map (\column -> if lowerIdent column == oldLower then new else column)

expand :: Map TableKey TableInfo -> CatalogOp -> [DataWrite]
expand catalog (WriteDataOp write) = expandWrite catalog write
expand _ _ = []

{- | Breadth-first cascade fan-out from one direct write. The queue carries
the lowered write columns for @UPDATE@ overlap checks; deletes carry none.
The visited guard is per (table, write flavor), matching the graph's
duplicate-write suppression.
-}
expandWrite :: Map TableKey TableInfo -> DataWrite -> [DataWrite]
expandWrite catalog write = case dataWriteKind write of
    DeleteWrite _ -> bfs Set.empty [(keyOfTarget, True, [])]
    UpdateWrite -> bfs Set.empty [(keyOfTarget, False, writeColumns)]
    _ -> []
    where
        keyOfTarget = tableKeyOf (dataWriteTarget write)
        writeColumns = map lowerIdent (dataWriteColumns write ++ dataWriteUnqualifiedColumns write)

        bfs :: Set (TableKey, Bool) -> [(TableKey, Bool, [Text])] -> [DataWrite]
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
                guard = (key, isDelete)

        childWrite :: NameRef -> Bool -> [Ident] -> DataWrite
        childWrite ref True _ = DataWrite ref (DeleteWrite DeleteAllRows) [] [] [] False
        childWrite ref False childColumns = DataWrite ref UpdateWrite childColumns [] [] False

        cascadeChildren :: TableKey -> Bool -> [Text] -> [(TableKey, NameRef, Bool, [Ident])]
        cascadeChildren parentKey isDelete columns =
            [ (childKey, infoRef info, childIsDelete, foreignKeyChildColumns foreignKey)
            | (childKey, info) <- Map.toList catalog
            , foreignKey <- infoFKs info
            , tableKeyOf (foreignKeyParent foreignKey) == parentKey
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
