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

import Control.Monad (filterM)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Occam.Core (StatementId (..))
import Occam.TreeSitter (TSNode, collectDescendants, nodeNamedChildren, nodeText, nodeType)

type ObjectName = Text

data ObjectKind
    = TableObject
    | ColumnObject
    | IndexObject
    | ViewObject
    | BarrierObject
    deriving (Eq, Ord, Show)

data DependencyType
    = -- | Creates or declares an object (e.g. CREATE TABLE t)
      DefDep
    | -- | Reads an object (e.g. SELECT FROM t)
      UseDep
    | -- | Modifies or writes to an object (e.g. INSERT INTO t)
      ModDep
    | -- | Sequence barrier: transaction boundary, session setting, error
      BarrierDep
    deriving (Eq, Ord, Show)

data ObjectAction = ObjectAction
    { actionKind :: !ObjectKind
    , actionType :: !DependencyType
    , actionName :: !ObjectName
    }
    deriving (Eq, Ord, Show)

data DependencyGraph = DependencyGraph
    { graphStmts :: !(Set StatementId)
    , graphActions :: !(Map StatementId [ObjectAction])
    , graphEdges :: !(Map StatementId (Set StatementId))
    -- ^ stmt -> set of statements it directly depends on (predecessors)
    }
    deriving (Eq, Show)

emptyGraph :: DependencyGraph
emptyGraph =
    DependencyGraph
        { graphStmts = Set.empty
        , graphActions = Map.empty
        , graphEdges = Map.empty
        }

isBarrierAction :: ObjectAction -> Bool
isBarrierAction act =
    actionKind act == BarrierObject || actionType act == BarrierDep

-- ----------------------------------------------------------------------------
-- CST Pattern Matching & Action Extraction
-- ----------------------------------------------------------------------------

-- | Check if any descendant node has the given type.
hasDescendantType :: Text -> TSNode -> IO Bool
hasDescendantType targetType node = do
    matches <- collectDescendants (== targetType) node
    pure (not (null matches))

normalizeName :: ByteString -> Text
normalizeName bs =
    let txt = T.strip (TE.decodeUtf8Lenient bs)
        segs = filter (not . T.null) (T.splitOn "." txt)
        rawTable = case segs of
            [] -> txt
            _ -> last segs
        unquote t =
            let s = T.strip t
             in T.dropAround (\c -> c == '"' || c == '`' || c == '\'' || c == '[' || c == ']') s
     in T.toLower (unquote rawTable)

extractActions :: TSNode -> ByteString -> IO [ObjectAction]
extractActions stmtNode src = do
    sRootType <- nodeType stmtNode
    isErr <- hasDescendantType "ERROR" stmtNode
    if isErr || sRootType == "ERROR"
        then pure [ObjectAction BarrierObject BarrierDep "__syntax_error_barrier__"]
        else do
            children <- nodeNamedChildren stmtNode
            cteNames <- findCTEAliases stmtNode src
            hasTruncate <- hasDescendantType "keyword_truncate" stmtNode
            actions <-
                if hasTruncate
                    then dmlActions <$> findTableReferences cteNames stmtNode src
                    else concat <$> mapM (inspectChild cteNames src) children
            let txAction = [ObjectAction BarrierObject BarrierDep "__tx_barrier__" | sRootType == "transaction"]
            let allActions = txAction ++ actions
            if null allActions
                then -- Fallback conservative rule: unparsed/unrecognized statements are barriers
                    pure [ObjectAction BarrierObject BarrierDep "__unknown_stmt_barrier__"]
                else pure (dedup allActions)
    where
        dedup = Set.toList . Set.fromList

findDirectTarget :: ByteString -> TSNode -> IO (Maybe ObjectName)
findDirectTarget src node = do
    children <- nodeNamedChildren node
    findTarget children
    where
        findTarget [] = pure Nothing
        findTarget (c : cs) = do
            t <- nodeType c
            if t == "object_reference" || t == "relation"
                then do
                    name <- normalizeName <$> nodeText src c
                    if T.null name then findTarget cs else pure (Just name)
                else findTarget cs

inspectChild :: Set ObjectName -> ByteString -> TSNode -> IO [ObjectAction]
inspectChild cteNames src node = do
    t <- nodeType node
    case t of
        -- DDL: CREATE TABLE
        "create_table" -> do
            mTgt <- findDirectTarget src node
            tables <- findTableReferences cteNames node src
            case mTgt of
                Just target -> do
                    let defAction = [ObjectAction TableObject DefDep target]
                        useActions = [ObjectAction TableObject UseDep tbl | tbl <- tables, tbl /= target]
                    pure (defAction ++ useActions)
                Nothing ->
                    pure [ObjectAction TableObject DefDep tbl | tbl <- tables]
        -- DDL: CREATE VIEW
        "create_view" -> do
            mTgt <- findDirectTarget src node
            tables <- findTableReferences cteNames node src
            case mTgt of
                Just target -> do
                    let defAction = [ObjectAction ViewObject DefDep target]
                        useActions = [ObjectAction TableObject UseDep tbl | tbl <- tables, tbl /= target]
                    pure (defAction ++ useActions)
                Nothing ->
                    pure [ObjectAction ViewObject DefDep tbl | tbl <- tables]
        -- DDL: CREATE INDEX
        "create_index" -> do
            tables <- findTableReferences cteNames node src
            pure [ObjectAction TableObject UseDep tbl | tbl <- tables]
        -- DDL: DROP TABLE / DROP VIEW / ALTER TABLE
        "drop_table" -> do
            tables <- findTableReferences cteNames node src
            pure [ObjectAction TableObject ModDep tbl | tbl <- tables]
        "drop_view" -> do
            tables <- findTableReferences cteNames node src
            pure [ObjectAction ViewObject ModDep tbl | tbl <- tables]
        "alter_table" -> do
            tables <- findTableReferences cteNames node src
            pure [ObjectAction TableObject ModDep tbl | tbl <- tables]
        -- DML: target mutations and source reads, including nested queries.
        "insert" -> dmlActions <$> findTableReferences cteNames node src
        "replace" -> dmlActions <$> findTableReferences cteNames node src
        "update" -> dmlActions <$> findTableReferences cteNames node src
        "delete" -> dmlActions <$> findTableReferences cteNames node src
        "truncate" -> dmlActions <$> findTableReferences cteNames node src
        -- DML: SELECT / FROM
        "select" -> do
            tables <- findTableReferences cteNames node src
            pure [ObjectAction TableObject UseDep tbl | tbl <- tables]
        "from" -> do
            tables <- findTableReferences cteNames node src
            pure [ObjectAction TableObject UseDep tbl | tbl <- tables]
        -- Barriers: Transactions & Session configs
        "transaction" ->
            pure [ObjectAction BarrierObject BarrierDep "__tx_barrier__"]
        "set" ->
            pure [ObjectAction BarrierObject BarrierDep "__session_barrier__"]
        -- Recurse through CTE clauses, set operations, subqueries, and wrappers.
        _ -> do
            subCh <- nodeNamedChildren node
            concat <$> mapM (inspectChild cteNames src) subCh
dmlActions :: [ObjectName] -> [ObjectAction]
dmlActions tables =
    [ ObjectAction TableObject actionType table
    | actionType <- [UseDep, ModDep]
    , table <- tables
    ]

findCTEAliases :: TSNode -> ByteString -> IO (Set ObjectName)
findCTEAliases node src = do
    ctes <- collectDescendants (== "cte") node
    names <- mapM cteName ctes
    pure (Set.fromList (filter (not . T.null) names))
    where
        cteName cte = do
            children <- nodeNamedChildren cte
            case children of
                (nameNode : _) -> normalizeName <$> nodeText src nameNode
                [] -> pure ""

findTableReferences :: Set ObjectName -> TSNode -> ByteString -> IO [ObjectName]
findTableReferences cteNames node src = do
    objRefs <- collectTableRefs (== "object_reference") node
    relations <- collectTableRefs (== "relation") node
    standaloneRelations <- filterM (fmap not . hasDescendantType "object_reference") relations
    names <- mapM (nodeText src) (objRefs ++ standaloneRelations)
    let cleaned =
            Set.fromList
                [ name
                | name <- map normalizeName names
                , not (T.null name)
                , Set.notMember name cteNames
                ]
    pure (Set.toList cleaned)
    where
        -- Invocations (e.g. NOW(), VALUES() in ON DUPLICATE KEY UPDATE) are
        -- not table references.
        collectTableRefs predNode n = do
            t <- nodeType n
            if t == "invocation"
                then pure []
                else do
                    children <- nodeNamedChildren n
                    subMatches <- concat <$> mapM (collectTableRefs predNode) children
                    pure $ if predNode t then n : subMatches else subMatches

-- ----------------------------------------------------------------------------
-- Dependency DAG Construction
-- ----------------------------------------------------------------------------

{- | Builds a strictly directed acyclic graph capturing:
  1. Structural dependency: Use/Mod(T) depends on Def(T).
  2. Data dependency: Use(T) depends on prior Mod(T); Mod(T) depends on prior Mod(T).
  3. Sequence barriers: statements around BarrierDep cannot cross the barrier.
-}
buildGraph :: [(StatementId, [ObjectAction])] -> DependencyGraph
buildGraph stmtActions =
    DependencyGraph
        { graphStmts = Set.fromList (map fst stmtActions)
        , graphActions = Map.fromList stmtActions
        , graphEdges = edges
        }
    where
        -- Track definitions and modifications as we scan in execution order
        edges = snd $ foldl processStmt (initialState, Map.empty) stmtActions

        initialState =
            ScanState
                { defMap = Map.empty -- ObjectName -> StatementId
                , modMap = Map.empty -- ObjectName -> [StatementId]
                , lastBarrier = Nothing -- Maybe StatementId
                }

data ScanState = ScanState
    { defMap :: !(Map ObjectName StatementId)
    , modMap :: !(Map ObjectName [StatementId])
    , lastBarrier :: !(Maybe StatementId)
    }

processStmt ::
    (ScanState, Map StatementId (Set StatementId)) ->
    (StatementId, [ObjectAction]) ->
    (ScanState, Map StatementId (Set StatementId))
processStmt (state, edgeMap) (sid, actions) =
    let
        -- 1. Barrier dependencies
        barrierDeps = maybe Set.empty Set.singleton (lastBarrier state)

        -- 2. Object dependencies
        (objDeps, state') = foldl (processAction sid state) (Set.empty, state) actions

        allDeps = Set.delete sid (Set.union barrierDeps objDeps)
        edgeMap' = Map.insert sid allDeps edgeMap
        -- Update barrier state if this statement is itself a barrier
        hasBarrier = any isBarrierAction actions
        state'' =
            if hasBarrier
                then state' {lastBarrier = Just sid}
                else state'
     in
        (state'', edgeMap')

processAction ::
    StatementId ->
    ScanState ->
    (Set StatementId, ScanState) ->
    ObjectAction ->
    (Set StatementId, ScanState)
processAction sid priorState (deps, state) act =
    case actionType act of
        DefDep ->
            -- Definition: recorded in defMap
            let state' = state {defMap = Map.insert (actionName act) sid (defMap state)}
             in (deps, state')
        UseDep ->
            -- Reading object: depends on Def(T), and depends on the latest Mod(T)
            let obj = actionName act
                defDep = maybe Set.empty Set.singleton (Map.lookup obj (defMap priorState))
                modDeps = case Map.lookup obj (modMap priorState) of
                    Just (latestMod : _) -> Set.singleton latestMod
                    _ -> Set.empty
                deps' = Set.unions [deps, defDep, modDeps]
             in (deps', state)
        ModDep ->
            -- Mutating object: depends on Def(T), and depends on prior Mod(T)
            let obj = actionName act
                defDep = maybe Set.empty Set.singleton (Map.lookup obj (defMap priorState))
                priorMods = Map.findWithDefault [] obj (modMap priorState)
                modDep = case priorMods of
                    (latestMod : _) -> Set.singleton latestMod
                    [] -> Set.empty
                deps' = Set.unions [deps, defDep, modDep]
                state' = state {modMap = Map.insert obj (sid : priorMods) (modMap state)}
             in (deps', state')
        BarrierDep ->
            -- Handled at statement level
            (deps, state)

-- | End-to-end analyzer: given statement nodes, extract actions and build DAG.
analyzeStatements :: [(StatementId, TSNode)] -> ByteString -> IO DependencyGraph
analyzeStatements stmts src = do
    stmtActions <- mapM extractPair stmts
    pure (buildGraph stmtActions)
    where
        extractPair (sid, node) = do
            acts <- extractActions node src
            pure (sid, acts)
