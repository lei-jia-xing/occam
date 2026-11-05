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
            actions <- concat <$> mapM (inspectChild src) children
            let txAction = [ObjectAction BarrierObject BarrierDep "__tx_barrier__" | sRootType == "transaction"]
            let allActions = txAction ++ actions
            if null allActions
                then -- Fallback conservative rule: unparsed/unrecognized statements are barriers
                    pure [ObjectAction BarrierObject BarrierDep "__unknown_stmt_barrier__"]
                else pure (dedup allActions)
    where
        dedup = Set.toList . Set.fromList

inspectChild :: ByteString -> TSNode -> IO [ObjectAction]
inspectChild src node = do
    t <- nodeType node
    case t of
        -- DDL: CREATE TABLE
        "create_table" -> do
            tables <- findTableReferences node src
            pure [ObjectAction TableObject DefDep tbl | tbl <- tables]
        -- DDL: CREATE INDEX
        "create_index" -> do
            tables <- findTableReferences node src
            pure [ObjectAction TableObject UseDep tbl | tbl <- tables]
        -- DDL: DROP TABLE / ALTER TABLE
        "drop_table" -> do
            tables <- findTableReferences node src
            pure [ObjectAction TableObject ModDep tbl | tbl <- tables]
        "alter_table" -> do
            tables <- findTableReferences node src
            pure [ObjectAction TableObject ModDep tbl | tbl <- tables]
        -- DML: INSERT
        "insert" -> do
            tables <- findTableReferences node src
            pure [ObjectAction TableObject ModDep tbl | tbl <- tables]
        "replace" -> do
            tables <- findTableReferences node src
            pure [ObjectAction TableObject ModDep tbl | tbl <- tables]
        "update" -> do
            tables <- findTableReferences node src
            pure [ObjectAction TableObject ModDep tbl | tbl <- tables]
        -- DML: DELETE
        "delete" -> do
            tables <- findTableReferences node src
            pure [ObjectAction TableObject ModDep tbl | tbl <- tables]
        -- DML: SELECT / FROM
        "select" -> do
            tables <- findTableReferences node src
            pure [ObjectAction TableObject UseDep tbl | tbl <- tables]
        "from" -> do
            tables <- findTableReferences node src
            pure [ObjectAction TableObject UseDep tbl | tbl <- tables]
        -- Barriers: Transactions & Session configs
        "transaction" ->
            pure [ObjectAction BarrierObject BarrierDep "__tx_barrier__"]
        "set" ->
            pure [ObjectAction BarrierObject BarrierDep "__session_barrier__"]
        -- General fallback for nested statements
        "statement" -> do
            subCh <- nodeNamedChildren node
            concat <$> mapM (inspectChild src) subCh
        _ -> do
            -- Check for transaction keywords in bare tokens
            isTx <- hasDescendantType "transaction" node
            isSet <- hasDescendantType "keyword_set" node
            if isTx
                then pure [ObjectAction BarrierObject BarrierDep "__tx_barrier__"]
                else
                    if isSet
                        then pure [ObjectAction BarrierObject BarrierDep "__session_barrier__"]
                        else pure []

findTableReferences :: TSNode -> ByteString -> IO [ObjectName]
findTableReferences node src = do
    -- In tree-sitter-sql, table names appear in object_reference nodes.
    -- We also support relation, field (e.g. schema.table), or identifier as fallbacks.
    -- Invocations (e.g. NOW(), VALUES() in ON DUPLICATE KEY UPDATE) are pruned.
    objRefs <- collectTableRefs (== "object_reference") node
    refs <-
        if null objRefs
            then collectTableRefs isFallbackRef node
            else pure objRefs
    names <- mapM (nodeText src) refs
    let cleaned = filter (not . T.null) $ map normalizeName names
    pure (Set.toList (Set.fromList cleaned))
    where
        isFallbackRef nt = nt == "relation" || nt == "field" || nt == "identifier"

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
