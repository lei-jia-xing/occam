module Occam.Dependency (
    ObjectName,
    ObjectKind (..),
    DependencyType (..),
    ObjectAction (..),
    DependencyGraph (..),
    emptyGraph,
    isBarrierAction,
    extractActions,
    buildGraph,
    analyzeStatements,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Occam.Core (
    ActionTarget (..),
    BarrierReason (..),
    DependencyType (..),
    ObjectAction (..),
    ObjectKind (..),
    ObjectName (..),
    Statement (..),
    StatementId (..),
    targetObjectName,
 )
import Occam.Parser (StatementFacts (..), StatementUnit (..))

data DependencyGraph = DependencyGraph
    { graphStmts :: !(Set StatementId)
    , graphActions :: !(Map StatementId [ObjectAction])
    , graphEdges :: !(Map StatementId (Set StatementId))
    , graphBarrierActions :: !Int
    , graphBarrierStatements :: !Int
    , graphBarrierEdges :: !Int
    , graphEdgeCount :: !Int
    }
    deriving (Eq, Show)

emptyGraph :: DependencyGraph
emptyGraph =
    DependencyGraph
        { graphStmts = Set.empty
        , graphActions = Map.empty
        , graphEdges = Map.empty
        , graphBarrierActions = 0
        , graphBarrierStatements = 0
        , graphBarrierEdges = 0
        , graphEdgeCount = 0
        }

isBarrierAction :: ObjectAction -> Bool
isBarrierAction act =
    actionKind act `elem` [BarrierObject, SessionVariableObject]
        || actionType act == BarrierDep

isSessionAction :: ObjectAction -> Bool
isSessionAction act = actionKind act == SessionVariableObject

-- Canonical names arrive from the frontend as escaped dot-separated
-- identifiers and are parsed into structured parts at the Parser boundary.
isQualifiedName :: ObjectName -> Bool
isQualifiedName (ObjectName parts) = length parts > 1

lastNamePart :: ObjectName -> ObjectName
lastNamePart name@(ObjectName []) = name
lastNamePart (ObjectName parts) = ObjectName [last parts]

qualifyRename :: ObjectName -> ObjectName -> ObjectName
qualifyRename (ObjectName oldParts) newName@(ObjectName newParts)
    | length oldParts > 1 && length newParts <= 1 =
        ObjectName (init oldParts ++ newParts)
    | otherwise = newName

-- ----------------------------------------------------------------------------
-- sqlparser-derived facts
-- ----------------------------------------------------------------------------

extractActions :: StatementUnit -> [ObjectAction]
extractActions unit =
    maybe
        [ ObjectAction BarrierObject BarrierDep $
            BarrierTarget
                ( if isNothing (unitParseError unit)
                    then UnknownStatementBarrier
                    else SyntaxErrorBarrier
                )
        ]
        factActions
        (unitFacts unit)

data TxCommand
    = TxBegin
    | TxCommit
    | TxRollback
    | TxSavepoint !ObjectName
    | TxRollbackTo !ObjectName
    | TxRelease !ObjectName
    deriving (Eq, Show)

-- Facts were parsed once with the selected dialect; graph construction stays
-- independent of SQL syntax and retains object generations and transaction snapshots.
analyzeStatements :: [StatementUnit] -> DependencyGraph
analyzeStatements units =
    buildGraph
        [ (stmtId (unitStatement unit), extractActions unit)
        | unit <- units
        ]

-- ----------------------------------------------------------------------------
-- Dependency DAG Construction
-- ----------------------------------------------------------------------------

{- | Builds dependencies between definitions, reads, writes, and conservative
   sequence barriers. Definitions replace the current object generation.
-}
buildGraph :: [(StatementId, [ObjectAction])] -> DependencyGraph
buildGraph stmtActions =
    let (_state, edges, barrierEdges, barrierStatements, barrierActions) =
            foldl' processStmt (initialState, Map.empty, 0, 0, 0) stmtActions
     in DependencyGraph
            { graphStmts = Set.fromList (map fst stmtActions)
            , graphActions = Map.fromList stmtActions
            , graphEdges = edges
            , graphBarrierActions = barrierActions
            , graphBarrierStatements = barrierStatements
            , graphBarrierEdges = barrierEdges
            , graphEdgeCount = sum (map Set.size (Map.elems edges))
            }

data ObjectKey
    = ObjectKey !ObjectKind !ObjectName
    | ColumnKey !ObjectKey !Text
    deriving (Eq, Ord, Show)

data ObjectSource
    = UnresolvedSource !ObjectName
    | ResolvedSource !ObjectKey
    deriving (Eq, Ord, Show)

data ObjectLifecycle = ObjectLive | ObjectDropped
    deriving (Eq, Show)

data ObjectState = ObjectState
    { objectDefinition :: !(Maybe StatementId)
    , objectLastWrite :: !(Maybe StatementId)
    , objectRestored :: !(Maybe StatementId)
    , objectSources :: !(Set ObjectSource)
    , objectLifecycle :: !ObjectLifecycle
    }
    deriving (Eq, Show)

data Catalog = Catalog
    { catalogObjects :: !(Map ObjectKey ObjectState)
    , catalogRelations :: !(Map ObjectKey (Set ObjectKey))
    , catalogCascades :: !(Map ObjectKey (Set ObjectKey))
    }
    deriving (Eq, Show)

data TxFrame = TxFrame
    { txBeginSid :: !StatementId
    , txStart :: !Catalog
    , txSavepoints :: ![(ObjectName, Catalog, StatementId)]
    , txImplicitSavepoint :: !Bool
    , txOnCommitDrop :: !(Set ObjectKey)
    , txOnCommitDeleteRows :: !(Set ObjectKey)
    }
    deriving (Eq, Show)

data ScanState = ScanState
    { scanCatalog :: !Catalog
    , scanTransactions :: ![TxFrame]
    , scanLastBarrier :: !(Maybe StatementId)
    , scanPriorStatements :: !(Set StatementId)
    }

initialState :: ScanState
initialState =
    ScanState
        { scanCatalog = Catalog Map.empty Map.empty Map.empty
        , scanTransactions = []
        , scanLastBarrier = Nothing
        , scanPriorStatements = Set.empty
        }

processStmt ::
    (ScanState, Map StatementId (Set StatementId), Int, Int, Int) ->
    (StatementId, [ObjectAction]) ->
    (ScanState, Map StatementId (Set StatementId), Int, Int, Int)
processStmt (state, edgeMap, barrierEdgeCount, barrierStmtCount, barrierActionCount) (sid, actions) =
    let
        declaredBarriers = length (filter isBarrierAction actions)
        catalogBarrier = declaredBarriers == 0 && needsCatalogBarrier state actions
        hasBarrier = declaredBarriers > 0 || catalogBarrier
        isSessionOnly = declaredBarriers > 0 && all isSessionAction (filter isBarrierAction actions)
        barrierDeps =
            if hasBarrier
                then
                    if isSessionOnly
                        then maybe Set.empty Set.singleton (scanLastBarrier state)
                        else scanPriorStatements state
                else maybe Set.empty Set.singleton (scanLastBarrier state)
        (objectDeps, state') = processObjectActions sid actions state
        (controlDeps, state'') = applyTransactionControl sid actions state'
        allDeps = Set.delete sid (Set.unions [barrierDeps, objectDeps, controlDeps])
        edgeMap' = Map.insert sid allDeps edgeMap
        state''' =
            state''
                { scanLastBarrier = if hasBarrier then Just sid else scanLastBarrier state''
                , scanPriorStatements = Set.insert sid (scanPriorStatements state'')
                }
        newBarrierEdges = Set.size (Set.difference barrierDeps (Set.union objectDeps controlDeps))
        actionBarriers = declaredBarriers + fromEnum catalogBarrier
     in
        ( state'''
        , edgeMap'
        , barrierEdgeCount + newBarrierEdges
        , barrierStmtCount + if hasBarrier then 1 else 0
        , barrierActionCount + actionBarriers
        )

needsCatalogBarrier :: ScanState -> [ObjectAction] -> Bool
needsCatalogBarrier state = any uncertain
    where
        uncertain action
            | actionKind action /= TableObject = False
            | actionType action == ModDep =
                any ((== ViewObject) . objectKind) (liveMatches UseDep action)
            | actionType action `elem` [DropDep, RenameFromDep] =
                case liveMatches (actionType action) action of
                    _ : _ : _ -> True
                    _ -> False
            | otherwise = False
        liveMatches dependency action =
            filter (isLiveKey (scanCatalog state)) $
                matchingKeys state TableObject dependency (actionTarget action)

processObjectActions :: StatementId -> [ObjectAction] -> ScanState -> (Set StatementId, ScanState)
processObjectActions sid actions state =
    let
        priorState = state
        nonRename = filter (\act -> actionType act `notElem` [RenameFromDep, RenameToDep]) actions
        (actionDeps, state') =
            foldl' (processAction sid priorState actions) (Set.empty, state) nonRename
        (renameDeps, state'') = processRenames sid priorState actions state'
        catalogWithCascades = bindCascades sid actions (scanCatalog state'')
        state''' = state'' {scanCatalog = catalogWithCascades}
        state4 = recordOnCommit sid actions state'''
     in
        (Set.union actionDeps renameDeps, state4)

processAction ::
    StatementId ->
    ScanState ->
    [ObjectAction] ->
    (Set StatementId, ScanState) ->
    ObjectAction ->
    (Set StatementId, ScanState)
processAction sid priorState stmtActions (deps, state) act
    | actionKind act `elem` [SessionVariableObject, TransactionObject] = (deps, state)
    | otherwise =
        case actionType act of
            DefDep ->
                let catalog = scanCatalog state
                    keys = definitionKeys catalog stmtActions act
                    sources = viewSources (scanCatalog priorState) stmtActions (actionTarget act)
                    newObject = ObjectState (Just sid) Nothing Nothing sources ObjectLive
                    catalog0 =
                        if actionKind act `elem` [TableObject, TemporaryTableObject]
                            then foldl' (prepareDefinition sid) catalog keys
                            else catalog
                    catalog' =
                        catalog0
                            { catalogObjects =
                                foldl' (\objects key -> Map.insert key newObject objects) (catalogObjects catalog0) keys
                            }
                    catalog'' =
                        if actionKind act `elem` [IndexObject, TriggerObject]
                            then foldl' (\current key -> bindRelations sid key stmtActions current) catalog' keys
                            else catalog'
                 in (deps, state {scanCatalog = catalog''})
            UseDep ->
                let objectDeps = dependenciesForAction priorState act
                    indexDeps =
                        if actionKind act /= TableObject
                            || any (\action -> actionKind action == IndexObject && actionType action == DefDep) stmtActions
                            then Set.empty
                            else
                                maybe
                                    Set.empty
                                    (boundIndexDependencies (scanCatalog priorState))
                                    (targetObjectName (actionTarget act))
                 in (Set.union deps (Set.union objectDeps indexDeps), state)
            ModDep ->
                let priorDeps = dependenciesForAction priorState act
                    keys = actionKeys priorState act
                    catalog' = writeObjects sid keys (scanCatalog state)
                 in (Set.union deps priorDeps, state {scanCatalog = catalog'})
            DropDep ->
                let priorDeps = dependenciesForAction priorState act
                    keys = actionKeys priorState act
                    catalog' = dropObjects sid keys (scanCatalog state)
                 in (Set.union deps priorDeps, state {scanCatalog = catalog'})
            BarrierDep -> (deps, state)
            RenameFromDep -> (deps, state)
            RenameToDep -> (deps, state)
            TxBeginDep -> (deps, state)
            TxCommitDep -> (deps, state)
            TxRollbackDep -> (deps, state)
            TxSavepointDep -> (deps, state)
            TxRollbackToDep -> (deps, state)
            TxReleaseDep -> (deps, state)
            CascadeDep -> (deps, state)
            OnCommitDropDep -> (deps, state)
            OnCommitDeleteRowsDep -> (deps, state)

processRenames ::
    StatementId ->
    ScanState ->
    [ObjectAction] ->
    ScanState ->
    (Set StatementId, ScanState)
processRenames sid priorState actions state =
    foldl' rename (Set.empty, state) (zip sources targets)
    where
        sources =
            [ name
            | a <- actions
            , actionKind a == TableObject
            , actionType a == RenameFromDep
            , ObjectTarget name <- [actionTarget a]
            ]
        targets =
            [ name
            | a <- actions
            , actionKind a == TableObject
            , actionType a == RenameToDep
            , ObjectTarget name <- [actionTarget a]
            ]

        rename (deps, current) (oldName, newName) =
            let catalog = scanCatalog priorState
                oldKeys =
                    filter
                        (isLiveKey catalog)
                        (matchingKeys priorState TableObject RenameFromDep (ObjectTarget oldName))
                sourceKeys = if null oldKeys then [ObjectKey TableObject oldName] else oldKeys
                sourceDeps =
                    Set.unions
                        [ Set.union (stateDependencies priorState key) (boundDependencies catalog (const True) key)
                        | key <- sourceKeys
                        ]
                move currentCatalog sourceKey =
                    case sourceKey of
                        ObjectKey kind storedName ->
                            let targetKey = ObjectKey kind (qualifyRename storedName newName)
                                dropped = dropObjects sid [sourceKey] currentCatalog
                                cleanTarget = prepareDefinition sid dropped targetKey
                                withTarget =
                                    cleanTarget
                                        { catalogObjects =
                                            Map.insert
                                                targetKey
                                                (ObjectState (Just sid) Nothing Nothing Set.empty ObjectLive)
                                                (catalogObjects cleanTarget)
                                        , catalogRelations =
                                            moveRelations sourceKey targetKey (catalogRelations catalog) (catalogRelations cleanTarget)
                                        , catalogCascades =
                                            moveCascades sourceKey targetKey (catalogCascades catalog) (catalogCascades cleanTarget)
                                        }
                                withSources = updateViewSources sourceKey targetKey withTarget
                             in moveTableColumns sid sourceKey targetKey catalog withSources
                        ColumnKey _ _ -> currentCatalog
             in (Set.union deps sourceDeps, current {scanCatalog = foldl' move (scanCatalog current) sourceKeys})

dependenciesForAction :: ScanState -> ObjectAction -> Set StatementId
dependenciesForAction state act =
    let keys = matchingKeys state (actionKind act) (actionType act) (actionTarget act)
        catalog = scanCatalog state
        objectDeps = Set.unions [recursiveDependencies catalog Set.empty key | key <- keys]
        relationDeps =
            case (actionKind act, actionType act, actionTarget act) of
                (TableObject, ModDep, ObjectTarget name) -> boundObjectDependencies catalog name
                _ -> Set.empty
     in Set.union objectDeps relationDeps

recursiveDependencies :: Catalog -> Set ObjectKey -> ObjectKey -> Set StatementId
recursiveDependencies catalog seen key
    | Set.member key seen = Set.empty
    | otherwise =
        case Map.lookup key (catalogObjects catalog) of
            Nothing -> Set.empty
            Just object ->
                let here = stateDependenciesInObject object
                    seen' = Set.insert key seen
                    sources =
                        if objectKind key == ViewObject
                            then
                                Set.unions
                                    [ Set.union
                                        (recursiveDependencies catalog seen' sourceKey)
                                        (boundDependencies catalog ((== IndexObject) . objectKind) sourceKey)
                                    | source <- Set.toList (objectSources object)
                                    , sourceKey <- case source of
                                        UnresolvedSource name ->
                                            matchingKeysInCatalog catalog TableObject UseDep (ObjectTarget name)
                                        ResolvedSource resolvedKey -> [resolvedKey]
                                    ]
                            else Set.empty
                 in Set.union here sources

stateDependencies :: ScanState -> ObjectKey -> Set StatementId
stateDependencies state key =
    maybe Set.empty stateDependenciesInObject (Map.lookup key (catalogObjects (scanCatalog state)))

stateDependenciesInObject :: ObjectState -> Set StatementId
stateDependenciesInObject object =
    Set.fromList (catMaybes [objectDefinition object, objectLastWrite object, objectRestored object])

matchingKeys :: ScanState -> ObjectKind -> DependencyType -> ActionTarget -> [ObjectKey]
matchingKeys state = matchingKeysInCatalog (scanCatalog state)

isLiveKey :: Catalog -> ObjectKey -> Bool
isLiveKey catalog key =
    maybe False ((== ObjectLive) . objectLifecycle) (Map.lookup key (catalogObjects catalog))

definitionKeys :: Catalog -> [ObjectAction] -> ObjectAction -> [ObjectKey]
definitionKeys catalog actions action =
    case (actionKind action, actionTarget action) of
        (ColumnObject, ColumnTarget table column) ->
            [ColumnKey owner column | owner <- owners table]
        (_, ObjectTarget name) -> [ObjectKey (actionKind action) name]
        (_, BarrierTarget _) -> []
        (_, ColumnTarget _ _) -> []
    where
        declared table =
            [ ObjectKey (actionKind definition) table
            | definition <- actions
            , actionType definition == DefDep
            , actionKind definition `elem` [TableObject, TemporaryTableObject]
            , actionTarget definition == ObjectTarget table
            ]
        resolved table =
            filter (isLiveKey catalog) (matchingKeysInCatalog catalog TableObject UseDep (ObjectTarget table))
        owners table
            | not (null (declared table)) = declared table
            | not (null (resolved table)) = resolved table
            | otherwise = [ObjectKey TableObject table]

actionKeys :: ScanState -> ObjectAction -> [ObjectKey]
actionKeys state action =
    case matchingKeys state (actionKind action) (actionType action) (actionTarget action) of
        [] -> definitionKeys (scanCatalog state) [] action
        keys -> keys

prepareDefinition :: StatementId -> Catalog -> ObjectKey -> Catalog
prepareDefinition sid catalog key =
    case key of
        ObjectKey kind name ->
            let alias = ObjectKey kind (lastNamePart name)
                retireAlias =
                    isQualifiedName name
                        && maybe False ((== ObjectDropped) . objectLifecycle) (Map.lookup alias (catalogObjects catalog))
                retired =
                    if retireAlias
                        then
                            catalog
                                { catalogObjects = Map.delete alias (catalogObjects catalog)
                                , catalogRelations = Map.delete alias (catalogRelations catalog)
                                }
                        else catalog
                cleared =
                    if Map.member key (catalogObjects catalog)
                        then resetTableColumns sid key retired
                        else retired
             in removeRelationsForTable key cleared
        ColumnKey _ _ -> catalog

matchingKeysInCatalog :: Catalog -> ObjectKind -> DependencyType -> ActionTarget -> [ObjectKey]
matchingKeysInCatalog _ _ _ (BarrierTarget _) = []
matchingKeysInCatalog catalog ColumnObject dependency (ColumnTarget table column) =
    [ key
    | owner <- matchingKeysInCatalog catalog TableObject dependency (ObjectTarget table)
    , let key = ColumnKey owner column
    , Map.member key (catalogObjects catalog)
    ]
matchingKeysInCatalog catalog kind dependency (ColumnTarget table _) =
    matchingKeysInCatalog catalog kind dependency (ObjectTarget table)
matchingKeysInCatalog catalog kind dependency (ObjectTarget name) =
    let available = Map.keys (catalogObjects catalog)
        acceptableKinds = case (kind, dependency) of
            (TableObject, UseDep) -> [TableObject, TemporaryTableObject, ViewObject]
            (TableObject, ModDep) -> [TableObject, TemporaryTableObject, ViewObject]
            (TableObject, _) -> [TableObject, TemporaryTableObject]
            _ -> [kind]
        matches =
            [ key
            | key@(ObjectKey keyKind objectName) <- available
            , keyKind `elem` acceptableKinds
            , not
                ( keyKind == TemporaryTableObject
                    && isQualifiedName name
                    && not (isTemporarySchema name)
                    && objectName /= name
                )
            , namesMayReferToEachOther name objectName
            ]
        tempMatches =
            [ key
            | key@(ObjectKey TemporaryTableObject _) <- matches
            , isLiveKey catalog key
            ]
        shouldPreferTemp =
            kind == TableObject
                && dependency `elem` [UseDep, ModDep, DropDep, RenameFromDep]
                && not (null tempMatches)
     in if shouldPreferTemp then tempMatches else matches

isTemporarySchema :: ObjectName -> Bool
isTemporarySchema (ObjectName (schema : _)) =
    schema == "temp" || schema == "pg_temp" || "pg_temp_" `T.isPrefixOf` schema
isTemporarySchema (ObjectName []) = False

-- Without a known search_path, an unqualified name can resolve in any schema.
namesMayReferToEachOther :: ObjectName -> ObjectName -> Bool
namesMayReferToEachOther requested stored
    | requested == stored = True
    | not (isQualifiedName requested) || not (isQualifiedName stored) =
        lastNamePart requested == lastNamePart stored
    | otherwise = False

objectKind :: ObjectKey -> ObjectKind
objectKind (ObjectKey kind _) = kind
objectKind (ColumnKey _ _) = ColumnObject

writeObjects :: StatementId -> [ObjectKey] -> Catalog -> Catalog
writeObjects sid keys catalog =
    let allKeys = transitiveCascades keys (catalogCascades catalog)
     in catalog
            { catalogObjects =
                foldl' write (catalogObjects catalog) allKeys
            }
    where
        write objects key =
            let old = Map.findWithDefault (ObjectState Nothing Nothing Nothing Set.empty ObjectLive) key objects
             in Map.insert key old {objectLastWrite = Just sid, objectRestored = Nothing} objects

transitiveCascades :: [ObjectKey] -> Map ObjectKey (Set ObjectKey) -> [ObjectKey]
transitiveCascades initial cascades = go (Set.fromList initial) initial
    where
        go visited [] = Set.toList visited
        go visited (k : rest) =
            let children = Map.findWithDefault Set.empty k cascades
                newChildren = Set.difference children visited
                visited' = Set.union visited newChildren
             in go visited' (rest ++ Set.toList newChildren)

dropObjects :: StatementId -> [ObjectKey] -> Catalog -> Catalog
dropObjects sid keys catalog =
    foldl' resetDroppedTable catalog' keys
    where
        catalog' =
            catalog
                { catalogObjects = foldl' dropOne (catalogObjects catalog) keys
                , catalogRelations = foldl' removeObjectBindings (catalogRelations catalog) keys
                , catalogCascades = foldl' removeCascadeBindings (catalogCascades catalog) keys
                }

        dropOne objects key =
            Map.insert key (ObjectState (Just sid) Nothing Nothing Set.empty ObjectDropped) objects

        removeObjectBindings relations key =
            Map.mapMaybe
                (\bound -> let kept = Set.delete key bound in if Set.null kept then Nothing else Just kept)
                relations

        removeCascadeBindings cascades key =
            let withoutKey = Map.delete key cascades
             in Map.mapMaybe
                    (\bound -> let kept = Set.delete key bound in if Set.null kept then Nothing else Just kept)
                    withoutKey

        resetDroppedTable current key
            | objectKind key `elem` [TableObject, TemporaryTableObject] =
                removeCascadesForTable key (removeRelationsForTable key (resetTableColumns sid key current))
            | otherwise = current

removeRelationsForTable :: ObjectKey -> Catalog -> Catalog
removeRelationsForTable table catalog =
    catalog
        { catalogRelations = Map.delete table (catalogRelations catalog)
        }

removeCascadesForTable :: ObjectKey -> Catalog -> Catalog
removeCascadesForTable table catalog =
    catalog
        { catalogCascades = Map.delete table (catalogCascades catalog)
        }

bindCascades :: StatementId -> [ObjectAction] -> Catalog -> Catalog
bindCascades _sid actions catalog =
    let cascadeParents =
            [ parent
            | act <- actions
            , actionType act == CascadeDep
            , parent <- matchingKeysInCatalog catalog TableObject UseDep (actionTarget act)
            ]
        localTables =
            [ key
            | act <- actions
            , actionType act `elem` [DefDep, ModDep]
            , actionKind act `elem` [TableObject, TemporaryTableObject]
            , key <- matchingKeysInCatalog catalog (actionKind act) (actionType act) (actionTarget act)
            ]
     in if null cascadeParents || null localTables
            then catalog
            else
                let cascades' =
                        foldl'
                            ( \cascades pKey ->
                                foldl' (\c locKey -> Map.insertWith Set.union pKey (Set.singleton locKey) c) cascades localTables
                            )
                            (catalogCascades catalog)
                            cascadeParents
                 in catalog {catalogCascades = cascades'}

recordOnCommit :: StatementId -> [ObjectAction] -> ScanState -> ScanState
recordOnCommit _sid actions state =
    case scanTransactions state of
        frame : rest ->
            let dropKeys =
                    [ key
                    | act <- actions
                    , actionType act == OnCommitDropDep
                    , key <- matchingKeysInCatalog (scanCatalog state) TemporaryTableObject DefDep (actionTarget act)
                    ]
                deleteKeys =
                    [ key
                    | act <- actions
                    , actionType act == OnCommitDeleteRowsDep
                    , key <- matchingKeysInCatalog (scanCatalog state) TemporaryTableObject DefDep (actionTarget act)
                    ]
                frame' =
                    frame
                        { txOnCommitDrop = Set.union (txOnCommitDrop frame) (Set.fromList dropKeys)
                        , txOnCommitDeleteRows = Set.union (txOnCommitDeleteRows frame) (Set.fromList deleteKeys)
                        }
             in state {scanTransactions = frame' : rest}
        [] -> state

moveCascades ::
    ObjectKey ->
    ObjectKey ->
    Map ObjectKey (Set ObjectKey) ->
    Map ObjectKey (Set ObjectKey) ->
    Map ObjectKey (Set ObjectKey)
moveCascades oldKey newKey sourceCascades targetCascades =
    let updatedParent =
            case Map.lookup oldKey sourceCascades of
                Nothing -> targetCascades
                Just bound -> Map.insertWith Set.union newKey bound (Map.delete oldKey targetCascades)
     in Map.map (Set.map (\k -> if k == oldKey then newKey else k)) updatedParent

resetTableColumns :: StatementId -> ObjectKey -> Catalog -> Catalog
resetTableColumns sid table catalog =
    catalog
        { catalogObjects =
            Map.filterWithKey keepColumn (catalogObjects catalog)
        }
    where
        keepColumn (ColumnKey owner _) object =
            owner /= table || objectDefinition object == Just sid
        keepColumn _ _ = True

moveTableColumns :: StatementId -> ObjectKey -> ObjectKey -> Catalog -> Catalog -> Catalog
moveTableColumns sid oldKey newKey sourceCatalog targetCatalog =
    let sourceColumns =
            [ column
            | (ColumnKey owner column, object) <- Map.toList (catalogObjects sourceCatalog)
            , owner == oldKey
            , objectLifecycle object == ObjectLive
            ]
        cleanTarget = resetTableColumns sid newKey targetCatalog
        movedObjects =
            foldl'
                ( \objects column ->
                    Map.insert
                        (ColumnKey newKey column)
                        (ObjectState (Just sid) Nothing Nothing Set.empty ObjectLive)
                        objects
                )
                (catalogObjects cleanTarget)
                sourceColumns
     in cleanTarget {catalogObjects = movedObjects}

bindRelations :: StatementId -> ObjectKey -> [ObjectAction] -> Catalog -> Catalog
bindRelations _ key actions catalog =
    let tables =
            [ owner
            | action <- actions
            , actionKind action == TableObject
            , actionType action == UseDep
            , owner <- matchingKeysInCatalog catalog TableObject UseDep (actionTarget action)
            , isLiveKey catalog owner
            ]
        withoutOldBinding =
            Map.mapMaybe
                (\bound -> let kept = Set.delete key bound in if Set.null kept then Nothing else Just kept)
                (catalogRelations catalog)
        relations' =
            foldl'
                (\relations table -> Map.insertWith Set.union table (Set.singleton key) relations)
                withoutOldBinding
                tables
     in catalog {catalogRelations = relations'}

boundIndexDependencies :: Catalog -> ObjectName -> Set StatementId
boundIndexDependencies catalog table =
    Set.unions
        [ boundDependencies catalog ((== IndexObject) . objectKind) owner
        | owner <- matchingKeysInCatalog catalog TableObject UseDep (ObjectTarget table)
        ]

boundObjectDependencies :: Catalog -> ObjectName -> Set StatementId
boundObjectDependencies catalog table =
    Set.unions
        [ boundDependencies catalog (const True) owner
        | owner <- matchingKeysInCatalog catalog TableObject ModDep (ObjectTarget table)
        ]

boundDependencies :: Catalog -> (ObjectKey -> Bool) -> ObjectKey -> Set StatementId
boundDependencies catalog include owner =
    Set.unions
        [ maybe Set.empty stateDependenciesInObject (Map.lookup key (catalogObjects catalog))
        | key <- Set.toList (Map.findWithDefault Set.empty owner (catalogRelations catalog))
        , include key
        ]

viewSources :: Catalog -> [ObjectAction] -> ActionTarget -> Set ObjectSource
viewSources catalog actions viewTarget =
    if any
        (\act -> actionKind act == ViewObject && actionType act == DefDep && actionTarget act == viewTarget)
        actions
        then
            Set.fromList
                [ source
                | act <- actions
                , actionKind act == TableObject
                , actionType act == UseDep
                , ObjectTarget sourceName <- [actionTarget act]
                , source <- case matchingKeysInCatalog catalog TableObject UseDep (actionTarget act) of
                    [] -> [UnresolvedSource sourceName]
                    keys -> map ResolvedSource keys
                ]
        else Set.empty

updateViewSources :: ObjectKey -> ObjectKey -> Catalog -> Catalog
updateViewSources oldKey newKey catalog =
    catalog
        { catalogObjects =
            Map.map
                ( \object -> object {objectSources = Set.unions (map replaceSource (Set.toList (objectSources object)))}
                )
                (catalogObjects catalog)
        }
    where
        replaceSource (ResolvedSource key) =
            Set.singleton (ResolvedSource (if key == oldKey then newKey else key))
        replaceSource source@(UnresolvedSource name) =
            case (oldKey, newKey) of
                (ObjectKey _ oldName, ObjectKey _ newName)
                    | namesMayReferToEachOther oldName name ->
                        Set.fromList [source, UnresolvedSource newName]
                _ -> Set.singleton source

moveRelations ::
    ObjectKey ->
    ObjectKey ->
    Map ObjectKey (Set ObjectKey) ->
    Map ObjectKey (Set ObjectKey) ->
    Map ObjectKey (Set ObjectKey)
moveRelations oldKey newKey sourceRelations targetRelations =
    case Map.lookup oldKey sourceRelations of
        Nothing -> targetRelations
        Just bound -> Map.insertWith Set.union newKey bound targetRelations

restoreCatalog :: StatementId -> Catalog -> Catalog -> Catalog
restoreCatalog sid targetCatalog currentCatalog =
    let modifiedKeys =
            [ key
            | (key, obj) <- Map.toList (catalogObjects targetCatalog)
            , Map.lookup key (catalogObjects currentCatalog) /= Just obj
            ]
        restoredObjects =
            foldl'
                (flip (Map.adjust (\obj -> obj {objectRestored = Just sid})))
                (catalogObjects targetCatalog)
                modifiedKeys
     in targetCatalog {catalogObjects = restoredObjects}

applyTransactionControl ::
    StatementId -> [ObjectAction] -> ScanState -> (Set StatementId, ScanState)
applyTransactionControl _sid actions state =
    case mapMaybe txCommandFromAction actions of
        command : _ -> apply command
        [] -> (Set.empty, state)
    where
        snapshot = scanCatalog state

        apply TxBegin =
            case scanTransactions state of
                [] ->
                    ( Set.empty
                    , state {scanTransactions = [TxFrame _sid snapshot [] False Set.empty Set.empty]}
                    )
                _ -> (Set.empty, state)
        apply TxCommit =
            case scanTransactions state of
                frame : rest ->
                    let catalog = scanCatalog state
                        droppedCatalog = dropObjects _sid (Set.toList (txOnCommitDrop frame)) catalog
                        truncatedCatalog = writeObjects _sid (Set.toList (txOnCommitDeleteRows frame)) droppedCatalog
                     in (Set.singleton (txBeginSid frame), state {scanCatalog = truncatedCatalog, scanTransactions = rest})
                [] -> (Set.empty, state)
        apply TxRollback =
            case scanTransactions state of
                frame : rest ->
                    let restored = restoreCatalog _sid (txStart frame) (scanCatalog state)
                     in ( Set.singleton (txBeginSid frame)
                        , state
                            { scanCatalog = restored
                            , scanTransactions = rest
                            }
                        )
                [] -> (Set.empty, state)
        apply (TxSavepoint name) =
            case scanTransactions state of
                frame : rest ->
                    ( Set.singleton (txBeginSid frame)
                    , state
                        { scanTransactions =
                            frame {txSavepoints = txSavepoints frame ++ [(name, snapshot, _sid)]} : rest
                        }
                    )
                [] ->
                    ( Set.empty
                    , state
                        { scanTransactions =
                            [TxFrame _sid snapshot [(name, snapshot, _sid)] True Set.empty Set.empty]
                        }
                    )
        apply (TxRollbackTo name) =
            case scanTransactions state of
                frame : rest ->
                    case break (\(n, _, _) -> n == name) (reverse (txSavepoints frame)) of
                        (_, (savedName, saved, spSid) : older) ->
                            let retained = reverse older ++ [(savedName, saved, spSid)]
                                restored = restoreCatalog _sid saved (scanCatalog state)
                             in ( Set.singleton spSid
                                , state
                                    { scanCatalog = restored
                                    , scanTransactions = frame {txSavepoints = retained} : rest
                                    }
                                )
                        _ -> (Set.singleton (txBeginSid frame), state)
                [] -> (Set.empty, state)
        apply (TxRelease name) =
            case scanTransactions state of
                frame : rest ->
                    case break (\(n, _, _) -> n == name) (reverse (txSavepoints frame)) of
                        (_, (_, _, spSid) : older) ->
                            let retained = reverse older
                                transactions =
                                    if txImplicitSavepoint frame && null retained
                                        then rest
                                        else frame {txSavepoints = retained} : rest
                             in (Set.singleton spSid, state {scanTransactions = transactions})
                        _ -> (Set.empty, state)
                [] -> (Set.empty, state)
txCommandFromAction :: ObjectAction -> Maybe TxCommand
txCommandFromAction act
    | actionKind act == TransactionObject =
        case actionType act of
            TxBeginDep -> Just TxBegin
            TxCommitDep -> Just TxCommit
            TxRollbackDep -> Just TxRollback
            TxSavepointDep -> Just (TxSavepoint (savepointName act))
            TxRollbackToDep -> Just (TxRollbackTo (savepointName act))
            TxReleaseDep -> Just (TxRelease (savepointName act))
            _ -> Nothing
    | otherwise = Nothing
    where
        savepointName action = fromMaybe (ObjectName []) (targetObjectName (actionTarget action))
