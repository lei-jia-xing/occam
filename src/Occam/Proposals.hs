{- | Noncontiguous deletion proposals derived from dependency components,
relation lifecycles, and transaction/savepoint structure.
-}
module Occam.Proposals (
    ProposalGroupKind (..),
    DeletionProposalGroup (..),
    GroupProposalPlan (..),
    isNoncontiguousIn,
    buildRelaxedGraph,
    extractDeletionProposalPlan,
    extractDeletionProposalPlanWithExtraTx,
    extractTransactionGroups,
    projectTransactionBlocks,
) where

import Data.HashSet qualified as HashSet
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Occam.Core (
    AlterTable (alterTableTarget),
    BarrierScope (GlobalScope),
    CatalogOp (..),
    DataWrite (dataWriteTarget),
    DropObject (DropRelation),
    DropOp (dropObjects),
    Ident (identText),
    IndexDef (indexTable),
    Name,
    NameRef (nameRefName),
    PartitionLink (linkChild),
    ReadFact (readSource),
    RefreshOp (refreshTarget),
    RenameOp (..),
    ResultStatement (..),
    ScopedBarrier (barrierScope),
    SemanticFacts (..),
    SessionInit,
    Span (..),
    Statement (..),
    StatementId (..),
    TableDef (tableName),
    TriggerDef (triggerTable),
    TxEffect (..),
    ViewDef (viewName),
    nameText,
 )
import Occam.Dependency (DependencyGraph (..), buildGraph)
import Occam.Slicer (backwardSliceFast)

data ProposalGroupKind
    = GroupWeakComponent
    | GroupWeakRelation
    | GroupTxBlock
    | GroupTxWrapper
    | GroupIndirectRelation
    deriving (Eq, Ord, Show)

data DeletionProposalGroup = DeletionProposalGroup
    { proposalKind :: !ProposalGroupKind
    , proposalLabel :: !Text
    , proposalStatements :: !IntSet
    , proposalBytes :: !Int
    , proposalWeakTie :: !Bool
    , proposalNoncontiguous :: !Bool
    }
    deriving (Eq, Show)

data GroupProposalPlan = GroupProposalPlan
    { planWeakGroups :: ![DeletionProposalGroup]
    , planWeakTxBlocks :: ![DeletionProposalGroup]
    , planStrongTxBlocks :: ![DeletionProposalGroup]
    , planTxWrappers :: ![DeletionProposalGroup]
    , planIndirectRelations :: ![DeletionProposalGroup]
    }
    deriving (Eq, Show)

{- | True when the selected statement IDs span at least one retained statement
in source order, making the group unavailable to any contiguous partition.
-}
isNoncontiguousIn :: [Statement] -> IntSet -> Bool
isNoncontiguousIn statements removed =
    case positions of
        [] -> False
        firstPos : rest ->
            let (lastPos, count) = foldl' (\(_, n) p -> (p, n + 1)) (firstPos, 1 :: Int) rest
             in lastPos - firstPos + 1 > count
    where
        positions =
            [ idx
            | (idx, stmt) <- zip [0 :: Int ..] statements
            , IntSet.member (unStatementId (stmtId stmt)) removed
            ]

{- | Build a relaxed dependency graph by removing global barriers.
Used solely to propose candidate deletions that remain oracle-gated.
-}
buildRelaxedGraph :: SessionInit -> [Statement] -> DependencyGraph -> DependencyGraph
buildRelaxedGraph initialSession statements graph =
    buildGraph
        initialSession
        [ (stmtId stmt, facts {factBarriers = filter ((/= GlobalScope) . barrierScope) (factBarriers facts)})
        | stmt <- statements
        , Just facts <- [Map.lookup (stmtId stmt) (graphFacts graph)]
        ]

{- | Extract tiered deletion proposal groups ordered from weakest ties to the
observed (or trailing) result statement toward coupled transaction/relation moves.
-}
extractDeletionProposalPlan ::
    SessionInit ->
    [Statement] ->
    DependencyGraph ->
    Maybe ResultStatement ->
    GroupProposalPlan
extractDeletionProposalPlan initialSession statements graph observedResult =
    extractDeletionProposalPlanWithExtraTx initialSession statements graph observedResult []

{- | Extract tiered deletion proposal groups, merging additional projected
transaction/savepoint block bodies preserved across relaxed slicing.
-}
extractDeletionProposalPlanWithExtraTx ::
    SessionInit ->
    [Statement] ->
    DependencyGraph ->
    Maybe ResultStatement ->
    [(Text, IntSet)] ->
    GroupProposalPlan
extractDeletionProposalPlanWithExtraTx initialSession statements graph observedResult extraTxBlocks
    | length statements < 2 =
        GroupProposalPlan [] [] [] [] []
    | otherwise =
        let relaxed = buildRelaxedGraph initialSession statements graph
            allIds = IntSet.fromList [unStatementId (stmtId s) | s <- statements]
            weights =
                IntMap.fromList
                    [ (unStatementId (stmtId s), max 0 (spanEndByte (stmtSpan s) - spanStartByte (stmtSpan s)))
                    | s <- statements
                    ]
            bytesOf = IntSet.foldl' (\acc sid -> acc + IntMap.findWithDefault 0 sid weights) 0
            targetId = selectTargetId statements observedResult
            targetSlice = maybe IntSet.empty (`backwardSliceFast` relaxed) targetId
            weakPool = allIds `IntSet.difference` targetSlice
            directTargetRels = case targetId of
                Nothing -> Set.empty
                Just tid ->
                    maybe
                        Set.empty
                        (Set.fromList . statementRelations)
                        (Map.lookup (StatementId tid) (graphFacts graph))
            mkGroup kind label stmts =
                DeletionProposalGroup
                    { proposalKind = kind
                    , proposalLabel = label
                    , proposalStatements = stmts
                    , proposalBytes = bytesOf stmts
                    , proposalWeakTie = IntSet.disjoint stmts targetSlice
                    , proposalNoncontiguous = isNoncontiguousIn statements stmts
                    }
            validGroup stmts =
                not (IntSet.null stmts)
                    && stmts /= allIds
                    && maybe True (`IntSet.notMember` stmts) targetId
            -- Tier 1: weakly-connected components in the weak-tie pool (allIds \ targetSlice),
            -- sorted largest-bytes first.
            weakComps =
                [ mkGroup GroupWeakComponent "weak-component" comp
                | comp <- weaklyConnectedComponents weakPool (graphFastEdges relaxed)
                , validGroup comp
                ]
            relMap = relationStatementMap statements graph
            weakRelGroups =
                [ mkGroup GroupWeakRelation ("relation:" <> rel) stmts
                | (rel, stmts) <- Map.toList relMap
                , not (Set.member rel directTargetRels)
                , validGroup stmts
                , IntSet.disjoint stmts targetSlice
                ]
            (txBlocksRaw, txWrappersRaw) = extractTransactionGroups statements graph targetId
            txBlocks =
                [ mkGroup GroupTxBlock label stmts
                | (label, stmts) <- txBlocksRaw ++ extraTxBlocks
                , validGroup stmts
                ]
            txWrappers =
                [ mkGroup GroupTxWrapper label stmts
                | (label, stmts) <- txWrappersRaw
                , validGroup stmts
                , isNoncontiguousIn statements stmts
                ]
            indirectRels =
                [ mkGroup GroupIndirectRelation ("relation:" <> rel) stmts
                | (rel, stmts) <- Map.toList relMap
                , not (Set.member rel directTargetRels)
                , validGroup stmts
                , not (IntSet.disjoint stmts targetSlice)
                , IntSet.size stmts >= 2
                , isNoncontiguousIn statements stmts
                ]
            sortTier =
                sortOn
                    (\g -> (Down (proposalBytes g), not (proposalNoncontiguous g), IntSet.findMin (proposalStatements g)))
            (weakGroups, seen1) = dedupTier HashSet.empty (sortTier (weakComps ++ weakRelGroups))
            (weakTx, seen2) = dedupTier seen1 (sortTier (filter proposalWeakTie txBlocks))
            (strongTx, seen3) = dedupTier seen2 (sortTier (filter (not . proposalWeakTie) txBlocks))
            (wrappers, seen4) = dedupTier seen3 (sortTier txWrappers)
            (indirect, _) = dedupTier seen4 (sortTier indirectRels)
         in GroupProposalPlan
                { planWeakGroups = weakGroups
                , planWeakTxBlocks = weakTx
                , planStrongTxBlocks = strongTx
                , planTxWrappers = wrappers
                , planIndirectRelations = indirect
                }

selectTargetId :: [Statement] -> Maybe ResultStatement -> Maybe Int
selectTargetId statements observedResult =
    case observedResult of
        Just (ResultStatement count index)
            | count == length statements
            , index >= 0
            , Just stmt <- listToMaybe (drop index statements) ->
                Just (unStatementId (stmtId stmt))
        _ -> unStatementId . stmtId <$> lastMay statements
    where
        lastMay [] = Nothing
        lastMay [x] = Just x
        lastMay (_ : xs) = lastMay xs

dedupTier ::
    HashSet.HashSet IntSet ->
    [DeletionProposalGroup] ->
    ([DeletionProposalGroup], HashSet.HashSet IntSet)
dedupTier seen0 = go seen0 []
    where
        go seen acc [] = (reverse acc, seen)
        go seen acc (g : rest)
            | HashSet.member (proposalStatements g) seen = go seen acc rest
            | otherwise = go (HashSet.insert (proposalStatements g) seen) (g : acc) rest

weaklyConnectedComponents :: IntSet -> IntMap IntSet -> [IntSet]
weaklyConnectedComponents nodes fastEdges = collect IntSet.empty (IntSet.toList nodes)
    where
        adj = IntMap.foldlWithKey' addNode IntMap.empty fastEdges
        addNode acc u preds
            | IntSet.notMember u nodes = acc
            | otherwise = IntSet.foldl' (addUndirectedEdge u) acc preds
        addUndirectedEdge u m v
            | IntSet.notMember v nodes = m
            | otherwise =
                IntMap.insertWith IntSet.union u (IntSet.singleton v) $
                    IntMap.insertWith IntSet.union v (IntSet.singleton u) m
        collect _ [] = []
        collect visited (start : rest)
            | IntSet.member start visited = collect visited rest
            | otherwise =
                let comp = bfs (IntSet.singleton start) [start]
                 in comp : collect (IntSet.union visited comp) rest
        bfs seen [] = seen
        bfs seen (curr : queue)
            | IntSet.null fresh = bfs seen queue
            | otherwise = bfs (IntSet.union seen fresh) (IntSet.foldl' (flip (:)) queue fresh)
            where
                nbrs = IntMap.findWithDefault IntSet.empty curr adj
                fresh = nbrs `IntSet.difference` seen

relationStatementMap :: [Statement] -> DependencyGraph -> Map Text IntSet
relationStatementMap statements graph =
    foldl' addStmt Map.empty statements
    where
        addStmt acc stmt =
            let sid = unStatementId (stmtId stmt)
                rels = maybe [] statementRelations (Map.lookup (stmtId stmt) (graphFacts graph))
             in foldl' (\m rel -> Map.insertWith IntSet.union rel (IntSet.singleton sid) m) acc rels

statementRelations :: SemanticFacts -> [Text]
statementRelations facts = concatMap opRelations (factOps facts)

opRelations :: CatalogOp -> [Text]
opRelations = \case
    DefineTableOp td -> [canonName (tableName td)]
    DefineViewOp vd -> [canonName (viewName vd)]
    DefineIndexOp idx -> [canonName (nameRefName (indexTable idx))]
    DefineTriggerOp tr -> [canonName (nameRefName (triggerTable tr))]
    ReadOp rf -> [canonName (nameRefName (readSource rf))]
    WriteDataOp dw -> [canonName (nameRefName (dataWriteTarget dw))]
    AlterTableOp at -> [canonName (nameRefName (alterTableTarget at))]
    AttachPartitionOp pl -> [canonName (nameRefName (linkChild pl))]
    DetachPartitionOp pl -> [canonName (nameRefName (linkChild pl))]
    AttachInheritOp pl -> [canonName (nameRefName (linkChild pl))]
    DetachInheritOp pl -> [canonName (nameRefName (linkChild pl))]
    DropCatalogOp dropOp -> [canonName (nameRefName nr) | DropRelation nr _ <- dropObjects dropOp]
    RenameOp (RenameRelation nr newName _) -> [canonName (nameRefName nr), canonName newName]
    RenameOp (RenameColumnOp nr _ _) -> [canonName (nameRefName nr)]
    RefreshCatalogOp rf -> [canonName (nameRefName (refreshTarget rf))]
    _ -> []

canonName :: Name -> Text
canonName = T.toLower . nameText

type TxGroupState =
    ([(Int, Int, Bool)], [(Text, Int, Int, Bool)], [(Text, IntSet)], [(Text, IntSet)])

extractTransactionGroups ::
    [Statement] ->
    DependencyGraph ->
    Maybe Int ->
    ([(Text, IntSet)], [(Text, IntSet)])
extractTransactionGroups statements graph targetId =
    (reverse blocks, reverse wrappers)
    where
        indexed = zip [0 :: Int ..] statements
        idsBetween startIdx endIdx =
            IntSet.fromList [unStatementId (stmtId s) | (i, s) <- indexed, i >= startIdx && i <= endIdx]
        (_, _, blocks, wrappers) = foldl' stepTx ([], [], [], []) indexed
        stepTx acc (idx, stmt) =
            let sid = unStatementId (stmtId stmt)
                effects = maybe [] (\f -> factPreTx f ++ factPostTx f) (Map.lookup (stmtId stmt) (graphFacts graph))
             in foldl' (applyTxGroupEffect targetId idsBetween idx sid) acc effects

applyTxGroupEffect ::
    Maybe Int -> (Int -> Int -> IntSet) -> Int -> Int -> TxGroupState -> TxEffect -> TxGroupState
applyTxGroupEffect _ _ idx sid (txStack, spStack, blocks, wrappers) (TxBegin _) =
    ((idx, sid, False) : txStack, spStack, blocks, wrappers)
applyTxGroupEffect _ idsBetween idx sid state@(txStack, _, blocks, wrappers) (TxCommit _) =
    case txStack of
        (startIdx, startSid, sawRb) : restTx ->
            let block = idsBetween startIdx idx
                blocks' = ("tx-commit-block", block) : blocks
                wrappers'
                    | not sawRb && idx - startIdx > 1 =
                        ("tx-commit-wrapper", IntSet.fromList [startSid, sid]) : wrappers
                    | otherwise = wrappers
             in (restTx, [], blocks', wrappers')
        [] -> state
applyTxGroupEffect _ idsBetween idx _ state@(txStack, _, blocks, wrappers) TxRollbackAll =
    case txStack of
        (startIdx, _, _) : restTx ->
            (restTx, [], ("tx-rollback-block", idsBetween startIdx idx) : blocks, wrappers)
        [] -> state
applyTxGroupEffect _ _ idx sid (txStack, spStack, blocks, wrappers) (TxSavepoint sp) =
    (txStack, (T.toLower (identText sp), idx, sid, False) : spStack, blocks, wrappers)
applyTxGroupEffect targetId idsBetween idx _ (txStack, spStack, blocks, wrappers) (TxRollbackTo sp) =
    (txStack', spStack', blocks', wrappers)
    where
        spKey = T.toLower (identText sp)
        txStack' = map (\(i, s, _) -> (i, s, True)) txStack
        (found, spStack') = markSavepoint spKey spStack
        blocks' = maybe blocks prependRollback found
        prependRollback startIdx
            | maybe True (`IntSet.notMember` block) targetId = ("savepoint-rollback-block", block) : blocks
            | otherwise = blocks
            where
                block = idsBetween startIdx idx
applyTxGroupEffect _ idsBetween idx sid state@(txStack, spStack, blocks, wrappers) (TxReleaseSavepoint sp)
    | Just (startIdx, startSid, sawRb, restSp) <- popSavepoint (T.toLower (identText sp)) spStack =
        let block = idsBetween startIdx idx
            blocks' = ("savepoint-release-block", block) : blocks
            wrappers'
                | not sawRb && idx - startIdx > 1 =
                    ("savepoint-release-wrapper", IntSet.fromList [startSid, sid]) : wrappers
                | otherwise = wrappers
         in (txStack, restSp, blocks', wrappers')
    | otherwise = state
applyTxGroupEffect _ _ _ _ state (TxSetReadOnly _) = state

markSavepoint :: Text -> [(Text, Int, Int, Bool)] -> (Maybe Int, [(Text, Int, Int, Bool)])
markSavepoint _ [] = (Nothing, [])
markSavepoint spKey ((k, i, s, rb) : rest)
    | k == spKey = (Just i, (k, i, s, True) : rest)
    | otherwise =
        let (found, rest') = markSavepoint spKey rest
         in (found, (k, i, s, rb) : rest')

popSavepoint :: Text -> [(Text, Int, Int, Bool)] -> Maybe (Int, Int, Bool, [(Text, Int, Int, Bool)])
popSavepoint _ [] = Nothing
popSavepoint spKey ((k, i, s, rb) : rest)
    | k == spKey = Just (i, s, rb, rest)
    | otherwise = popSavepoint spKey rest

{- | Project pre-slice transaction/savepoint blocks onto surviving post-slice
statements when slicing stripped transaction wrappers (e.g., @BEGIN@/@COMMIT@)
while retaining coupled body statements 1-to-1.
-}
projectTransactionBlocks ::
    [Statement] ->
    DependencyGraph ->
    Maybe ResultStatement ->
    [Statement] ->
    [Statement] ->
    [(Text, IntSet)]
projectTransactionBlocks preStatements preGraph preObserved retainedStatements postStatements
    | length retainedStatements /= length postStatements = []
    | otherwise =
        let preTargetId = selectTargetId preStatements preObserved
            (preTxBlocks, _) = extractTransactionGroups preStatements preGraph preTargetId
            idMap =
                IntMap.fromList
                    ( zip
                        (map (unStatementId . stmtId) retainedStatements)
                        (map (unStatementId . stmtId) postStatements)
                    )
            projectSet =
                IntSet.foldl'
                    ( \acc preSid ->
                        case IntMap.lookup preSid idMap of
                            Just postSid -> IntSet.insert postSid acc
                            Nothing -> acc
                    )
                    IntSet.empty
         in [ (label, projected)
            | (label, preIds) <- preTxBlocks
            , let projected = projectSet preIds
            , IntSet.size projected >= 2
            ]
