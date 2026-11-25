module PropertyChecks (tests) where

import Data.Graph qualified as Graph
import Data.IntSet qualified as IntSet
import Data.List (subsequences)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Occam.Core (
    CatalogOp (..),
    ColumnDef (..),
    ConflictAction (..),
    DataWrite (..),
    Ident (..),
    Name (..),
    NameRef (..),
    ReadFact (..),
    ReadMode (..),
    SemanticFacts (..),
    SessionInit (..),
    StatementId (..),
    TableDef (..),
    WriteKind (..),
 )
import Occam.Dependency (DependencyGraph (..), buildGraph)
import Occam.Reducer (closeSet)
import Occam.Slicer (backwardSlice, closeFromBoundary, successorIndex)
import Test.QuickCheck (
    Arbitrary (..),
    Property,
    chooseInt,
    counterexample,
    elements,
    oneof,
    resize,
    vectorOf,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import TestSupport (emptySemanticFacts)

data ArbitraryFact
    = DefineTableFact !T.Text
    | WriteTableFact !T.Text
    | ReadTableFact !T.Text
    deriving (Eq, Show)

instance Arbitrary ArbitraryFact where
    arbitrary = do
        name <- elements ["t1", "t2", "t3"]
        oneof
            [ pure (DefineTableFact name)
            , pure (WriteTableFact name)
            , pure (ReadTableFact name)
            ]

factOp :: ArbitraryFact -> CatalogOp
factOp fact =
    case fact of
        DefineTableFact name ->
            DefineTableOp
                TableDef
                    { tableName = Name [Ident name]
                    , tableTemporary = False
                    , tableTransactional = True
                    , tableColumns = [ColumnDef (Ident "id") False False False []]
                    , tableConstraints = []
                    , tableSources = []
                    , tableLike = Nothing
                    , tablePartitionParent = Nothing
                    , tableInheritParents = []
                    , tableOnCommit = Nothing
                    , tableIfNotExists = False
                    , tableOrReplace = False
                    }
        WriteTableFact name ->
            WriteDataOp
                DataWrite
                    { dataWriteTarget = NameRef (Name [Ident name]) False
                    , dataWriteKind = InsertWrite ConflictError
                    , dataWriteColumns = []
                    , dataWriteUnqualifiedColumns = []
                    , dataWriteReads = []
                    , dataWriteOnly = False
                    }
        ReadTableFact name ->
            ReadOp
                ReadFact
                    { readSource = NameRef (Name [Ident name]) False
                    , readMode = ReadData
                    , readColumns = []
                    , readOnly = False
                    }

newtype SmallFactGraph = SmallFactGraph [[ArbitraryFact]]
    deriving (Show)

instance Arbitrary SmallFactGraph where
    arbitrary = do
        statementCount <- chooseInt (2, 8)
        SmallFactGraph <$> vectorOf statementCount (resize 8 arbitrary)
    shrink (SmallFactGraph statements) =
        [SmallFactGraph smaller | smaller <- shrink statements, not (null smaller)]

buildFactGraph :: [[ArbitraryFact]] -> DependencyGraph
buildFactGraph rawFacts =
    buildGraph
        defaultSessionInit
        [ ( StatementId sid
          , emptySemanticFacts {factOps = map factOp facts}
          )
        | (sid, facts) <- zip [1 ..] rawFacts
        ]

defaultSessionInit :: SessionInit
defaultSessionInit = SessionInit [] [] Nothing True True Map.empty

-- Data.Graph independently computes reachability along each statement's predecessors.
backwardReachabilityOracle ::
    DependencyGraph ->
    Map.Map StatementId (Set.Set StatementId)
backwardReachabilityOracle dependencyGraph =
    let nodes =
            [ (sid, sid, Set.toList (Map.findWithDefault Set.empty sid (graphEdges dependencyGraph)))
            | sid <- Set.toList (graphStmts dependencyGraph)
            ]
        (graph, nodeFromVertex, vertexFromKey) = Graph.graphFromEdges nodes
        reachableFrom target =
            case vertexFromKey target of
                Nothing -> Set.empty
                Just vertex ->
                    Set.fromList
                        [ reached
                        | reachedVertex <- Graph.reachable graph vertex
                        , let (_, reached, _) = nodeFromVertex reachedVertex
                        ]
     in Map.fromSet reachableFrom (graphStmts dependencyGraph)

-- Prop 1: DAG Acyclicity
prop_dagAcyclicity :: [[ArbitraryFact]] -> Property
prop_dagAcyclicity rawFacts =
    let graph = buildFactGraph rawFacts
        edges = Map.toList (graphEdges graph)
        strictlyBackwards (StatementId j, preds) =
            all (\(StatementId i) -> i < j) (Set.toList preds)
     in counterexample
            ("Graph edges violate acyclicity: " ++ show edges)
            (all strictlyBackwards edges)

-- Prop 2: Backward slices exactly match graph reachability
prop_backwardSliceSoundness :: SmallFactGraph -> Property
prop_backwardSliceSoundness (SmallFactGraph rawFacts) =
    let graph = buildFactGraph rawFacts
        expectedByTarget = backwardReachabilityOracle graph
        mismatches =
            [ (target, Set.difference expected actual, Set.difference actual expected)
            | target <- Set.toList (graphStmts graph)
            , let actual = backwardSlice target graph
                  expected = Map.findWithDefault Set.empty target expectedByTarget
            , actual /= expected
            ]
     in counterexample
            ( "Backward slices differ (target, missing ancestors, unrelated extras): "
                ++ show mismatches
                ++ "; graph edges: "
                ++ show (graphEdges graph)
            )
            (null mismatches)

-- Prop 2b: Set closure exactly matches reachability for every valid root subset
prop_closeSetReachability :: SmallFactGraph -> Property
prop_closeSetReachability (SmallFactGraph rawFacts) =
    let graph = buildFactGraph rawFacts
        expectedByTarget = backwardReachabilityOracle graph
        rootSets = map Set.fromList (subsequences (Set.toList (graphStmts graph)))
        predecessors = graphFastEdges graph
        successors = successorIndex predecessors
        asInts = IntSet.fromList . map unStatementId . Set.toList
        allIds = asInts (graphStmts graph)
        mismatches =
            [ (roots, actual, boundaryActual, expected)
            | roots <- rootSets
            , let actual = closeSet graph roots
                  expected =
                    Set.unions
                        [ Map.findWithDefault Set.empty root expectedByTarget
                        | root <- Set.toList roots
                        ]
                  kept = asInts roots
                  boundaryActual =
                    closeFromBoundary predecessors successors kept (allIds `IntSet.difference` kept)
            , actual /= expected || boundaryActual /= asInts expected
            ]
     in counterexample
            ("closeSet differs from reachability (roots, actual, boundary actual, expected): " ++ show mismatches)
            (null mismatches)

tests :: TestTree
tests =
    testGroup
        "Property Tests (QuickCheck)"
        [ testProperty "Prop 1: DAG Acyclicity (j -> i implies i < j)" prop_dagAcyclicity
        , testProperty
            "Prop 2: Backward Slice Soundness (exact predecessor reachability)"
            prop_backwardSliceSoundness
        , testProperty
            "Prop 2b: Set closure matches reachability for every valid root subset"
            prop_closeSetReachability
        ]
