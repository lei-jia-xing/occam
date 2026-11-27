module DependencyChecks.SnapshotModelChecks (prop_catalogTransitions) where

import Data.Bifunctor qualified as Bifunctor
import Data.ByteString.Char8 qualified as BSC
import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import DependencyChecks.Support (analyze, dependenciesFor)
import Occam.Core (StatementId (..))
import Occam.Parser (SQLDialect (..))
import Test.QuickCheck (Gen, Property, arbitrary, chooseInt, counterexample, forAll, listOf, resize)
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)

data CatalogPlan = CatalogPlan
    { planTarget :: Int
    , planReverseInitial :: Bool
    , planTransactionWrites :: [Int]
    , planSavepointWrites :: [Int]
    , planAfterRollbackWrites :: [Int]
    , planReleaseWrites :: [Int]
    , planTransactionWritesFirst :: Bool
    , planSavepointWritesFirst :: Bool
    , planReleaseWritesFirst :: Bool
    }
    deriving (Show)

genCatalogPlan :: Gen CatalogPlan
genCatalogPlan =
    CatalogPlan
        <$> chooseInt (0, 1)
        <*> arbitrary
        <*> boundedWrites
        <*> boundedWrites
        <*> boundedWrites
        <*> boundedWrites
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    where
        boundedWrites :: Gen [Int]
        boundedWrites = resize 3 (listOf (chooseInt (0, 1)))

type TableSnapshot = Map.Map Int (StatementId, Set.Set StatementId)

data CatalogTrace = CatalogTrace
    { traceStatements :: ![String]
    , traceNextStatement :: !Int
    , traceTables :: !TableSnapshot
    , traceReads :: ![(StatementId, Set.Set StatementId)]
    , traceControls :: !(Set.Set StatementId)
    }

emptyTrace :: CatalogTrace
emptyTrace = CatalogTrace [] 1 Map.empty [] Set.empty

emit :: String -> CatalogTrace -> (StatementId, CatalogTrace)
emit sql trace =
    let sid = StatementId (traceNextStatement trace)
     in ( sid
        , trace
            { traceStatements = sql : traceStatements trace
            , traceNextStatement = traceNextStatement trace + 1
            }
        )

control :: String -> CatalogTrace -> CatalogTrace
control sql trace =
    let (sid, trace') = emit sql trace
     in trace' {traceControls = Set.insert sid (traceControls trace')}

snapshotTableName :: Int -> String
snapshotTableName 0 = "a"
snapshotTableName _ = "b"

createTable :: Int -> CatalogTrace -> CatalogTrace
createTable table trace =
    let (sid, trace') = emit ("CREATE TABLE " ++ snapshotTableName table ++ " (id INTEGER);") trace
     in trace' {traceTables = Map.insert table (sid, Set.empty) (traceTables trace')}

writeTable :: Int -> CatalogTrace -> CatalogTrace
writeTable table trace =
    let (sid, trace') = emit ("INSERT INTO " ++ snapshotTableName table ++ " VALUES (1);") trace
     in trace'
            { traceTables =
                Map.adjust
                    (Bifunctor.second (Set.insert sid))
                    table
                    (traceTables trace')
            }

dropTable :: Int -> CatalogTrace -> CatalogTrace
dropTable table trace =
    let (_, trace') = emit ("DROP TABLE " ++ snapshotTableName table ++ ";") trace
     in trace' {traceTables = Map.delete table (traceTables trace')}

readTable :: Int -> CatalogTrace -> CatalogTrace
readTable table trace =
    let (sid, trace') = emit ("SELECT * FROM " ++ snapshotTableName table ++ ";") trace
     in trace'
            { traceReads =
                (sid, tableDependencies table (traceTables trace')) : traceReads trace'
            }

tableDependencies :: Int -> TableSnapshot -> Set.Set StatementId
tableDependencies table tables =
    case Map.lookup table tables of
        Nothing -> Set.empty
        Just (definition, writes) -> Set.insert definition writes

writeMany :: [Int] -> CatalogTrace -> CatalogTrace
writeMany tables trace = Foldable.foldl' (flip writeTable) trace tables

readBoth :: Int -> Int -> CatalogTrace -> CatalogTrace
readBoth first second = readTable second . readTable first

mutateBlock :: Int -> Int -> [Int] -> Bool -> CatalogTrace -> CatalogTrace
mutateBlock target other extraWrites extrasFirst trace =
    let beforeChange = if extrasFirst then writeMany extraWrites trace else trace
        changed =
            writeTable
                other
                (writeTable target (createTable target (dropTable target beforeChange)))
     in if extrasFirst then changed else writeMany extraWrites changed

catalogTrace :: CatalogPlan -> CatalogTrace
catalogTrace plan =
    releaseSavepoint (rollbackSavepoint (rollbackTransaction initial))
    where
        target = planTarget plan
        other = 1 - target
        initialOrder = if planReverseInitial plan then [1, 0] else [0, 1]
        initial =
            Foldable.foldl'
                (\trace table -> writeTable table (createTable table trace))
                emptyTrace
                initialOrder

        rollbackTransaction trace =
            let snapshot = traceTables trace
                changed =
                    readBoth target other $
                        mutateBlock
                            target
                            other
                            (planTransactionWrites plan)
                            (planTransactionWritesFirst plan)
                            (control "BEGIN;" trace)
                restored = (control "ROLLBACK;" changed) {traceTables = snapshot}
             in readBoth target other restored

        rollbackSavepoint trace =
            let snapshot = traceTables trace
                changed =
                    readBoth target other $
                        mutateBlock
                            target
                            other
                            (planSavepointWrites plan)
                            (planSavepointWritesFirst plan)
                            (control "SAVEPOINT s;" trace)
                restored = (control "ROLLBACK TO SAVEPOINT s;" changed) {traceTables = snapshot}
                afterRollback = readBoth target other restored
                released =
                    control
                        "RELEASE SAVEPOINT s;"
                        (writeMany (planAfterRollbackWrites plan) afterRollback)
             in readBoth target other released

        releaseSavepoint trace =
            let changed =
                    mutateBlock
                        target
                        other
                        (planReleaseWrites plan)
                        (planReleaseWritesFirst plan)
                        (control "SAVEPOINT r;" trace)
             in readBoth target other (control "RELEASE SAVEPOINT r;" changed)

prop_catalogTransitions :: Property
prop_catalogTransitions =
    forAll genCatalogPlan $ \plan ->
        let trace = catalogTrace plan
            sql = BSC.unlines (map BSC.pack (reverse (traceStatements trace)))
            expected = reverse (traceReads trace)
            actual graph =
                [ (sid, Set.difference (dependenciesFor sid graph) (traceControls trace))
                | (sid, _) <- expected
                ]
         in counterexample
                ("SQL trace:\n" ++ BSC.unpack sql ++ "\nexpected: " ++ show expected)
                ( monadicIO $ do
                    graph <- run (analyze SQLite sql)
                    monitor (counterexample ("actual data dependencies: " ++ show (actual graph)))
                    assert (actual graph == expected)
                )
