{-# LANGUAGE OverloadedStrings #-}

module MutationalFuzz (
    -- * Seeds
    SeedScenario (..),
    seedPool,

    -- * Mutators
    injectDeadTables,
    bloatExpressions,
    injectOptionalClauses,
    injectLexicalNoise,
    compositeMutate,

    -- * Properties
    prop_mutationRecovery,
    prop_deadTablesPruned,
    prop_bloatExpressionsSimplified,
    prop_optionalClausesPruned,
    prop_lexicalNoiseSoundness,

    -- * Test Tree & Runner
    checkTreeSitterValidity,
    mutationalFuzzTests,
    main,
) where

import Control.Monad (foldM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Char (toUpper)
import Data.Text (Text)
import Data.Text qualified as T
import Occam.Reducer (Oracle, reduceSQL)
import Occam.TreeSitter (
    collectDescendants,
    parseSQL,
    rootNode,
    withParser,
 )
import Test.QuickCheck (
    Arbitrary (..),
    Gen,
    Property,
    chooseInt,
    counterexample,
    elements,
    forAll,
    frequency,
 )
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)
import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)

-- ----------------------------------------------------------------------------
-- Seed Scenarios
-- ----------------------------------------------------------------------------

data SeedScenario = SeedScenario
    { seedName :: !Text
    , seedSQL :: !ByteString
    , seedOracle :: !Oracle
    }

instance Show SeedScenario where
    show s = "SeedScenario<" ++ T.unpack (seedName s) ++ ">"

instance Arbitrary SeedScenario where
    arbitrary = elements seedPool

seedPool :: [SeedScenario]
seedPool =
    [ SeedScenario
        { seedName = "json_crash"
        , seedSQL =
            BSC.unlines
                [ "CREATE TABLE target_json (id INT PRIMARY KEY, payload TEXT);"
                , "INSERT INTO target_json VALUES (1, '{\"trigger\": \"crash_segv\"}');"
                , "SELECT id, payload FROM target_json WHERE payload = '{\"trigger\": \"crash_segv\"}';"
                ]
        , seedOracle = \bs ->
            pure
                ( BSC.isInfixOf "CREATE TABLE target_json" bs
                    && BSC.isInfixOf "INSERT INTO target_json VALUES (1, '{\"trigger\": \"crash_segv\"}')" bs
                    && BSC.isInfixOf "SELECT id, payload FROM target_json" bs
                    && BSC.isInfixOf "payload = '{\"trigger\": \"crash_segv\"}'" bs
                )
        }
    , SeedScenario
        { seedName = "join_crash"
        , seedSQL =
            BSC.unlines
                [ "CREATE TABLE users_tbl (uid INT PRIMARY KEY, name TEXT);"
                , "CREATE TABLE orders_tbl (oid INT, uid INT, amount NUMERIC);"
                , "INSERT INTO users_tbl VALUES (42, 'alice');"
                , "INSERT INTO orders_tbl VALUES (101, 42, 999.99);"
                , "SELECT u.name, o.amount FROM users_tbl u JOIN orders_tbl o ON u.uid = o.uid WHERE o.amount > 500;"
                ]
        , seedOracle = \bs ->
            pure
                ( BSC.isInfixOf "CREATE TABLE users_tbl" bs
                    && BSC.isInfixOf "CREATE TABLE orders_tbl" bs
                    && BSC.isInfixOf "INSERT INTO users_tbl VALUES (42, 'alice')" bs
                    && BSC.isInfixOf "INSERT INTO orders_tbl VALUES (101, 42, 999.99)" bs
                    && BSC.isInfixOf "SELECT u.name, o.amount FROM users_tbl u JOIN orders_tbl o" bs
                    && BSC.isInfixOf "u.uid = o.uid" bs
                    && BSC.isInfixOf "o.amount > 500" bs
                )
        }
    , SeedScenario
        { seedName = "agg_crash"
        , seedSQL =
            BSC.unlines
                [ "CREATE TABLE metric_series (sensor_id INT, reading NUMERIC);"
                , "INSERT INTO metric_series VALUES (1, 10.5);"
                , "INSERT INTO metric_series VALUES (1, 42.0);"
                , "SELECT sensor_id, avg(reading) FROM metric_series WHERE sensor_id = 1 GROUP BY sensor_id;"
                ]
        , seedOracle = \bs ->
            pure
                ( BSC.isInfixOf "CREATE TABLE metric_series" bs
                    && BSC.isInfixOf "INSERT INTO metric_series VALUES (1, 42.0)" bs
                    && BSC.isInfixOf "FROM metric_series" bs
                    && BSC.isInfixOf "avg(reading)" bs
                    && BSC.isInfixOf "GROUP BY sensor_id" bs
                )
        }
    , SeedScenario
        { seedName = "subquery_crash"
        , seedSQL =
            BSC.unlines
                [ "CREATE TABLE main_tasks (task_id INT PRIMARY KEY, name TEXT);"
                , "CREATE TABLE sub_items (item_id INT, task_id INT, status TEXT);"
                , "INSERT INTO main_tasks VALUES (7, 'deploy');"
                , "INSERT INTO sub_items VALUES (99, 7, 'crashed_worker');"
                , "SELECT * FROM main_tasks WHERE task_id IN (SELECT task_id FROM sub_items WHERE status = 'crashed_worker');"
                ]
        , seedOracle = \bs ->
            pure
                ( BSC.isInfixOf "CREATE TABLE main_tasks" bs
                    && BSC.isInfixOf "CREATE TABLE sub_items" bs
                    && BSC.isInfixOf "INSERT INTO main_tasks VALUES (7, 'deploy')" bs
                    && BSC.isInfixOf "INSERT INTO sub_items VALUES (99, 7, 'crashed_worker')" bs
                    && BSC.isInfixOf "SELECT * FROM main_tasks" bs
                    && BSC.isInfixOf "task_id IN (SELECT task_id FROM sub_items" bs
                    && BSC.isInfixOf "status = 'crashed_worker'" bs
                )
        }
    , SeedScenario
        { seedName = "window_crash"
        , seedSQL =
            BSC.unlines
                [ "CREATE TABLE events (id INT, tag TEXT, val INT);"
                , "INSERT INTO events VALUES (1, 'segv_window', 10);"
                , "SELECT tag, row_number() OVER (PARTITION BY tag) FROM events WHERE tag = 'segv_window';"
                ]
        , seedOracle = \bs ->
            pure
                ( BSC.isInfixOf "CREATE TABLE events" bs
                    && BSC.isInfixOf "INSERT INTO events VALUES (1, 'segv_window', 10)" bs
                    && BSC.isInfixOf "FROM events" bs
                    && BSC.isInfixOf "row_number() OVER (PARTITION BY tag)" bs
                    && BSC.isInfixOf "tag = 'segv_window'" bs
                )
        }
    ]

-- ----------------------------------------------------------------------------
-- Statement Splitting & Analysis Helpers
-- ----------------------------------------------------------------------------

splitStatements :: ByteString -> [ByteString]
splitStatements bs = filter (not . BSC.null . BSC.strip) (go 0 False 0 [])
    where
        len = BS.length bs
        go idx inQuote start acc
            | idx >= len =
                let remStmt = BSC.strip (BS.drop start bs)
                 in reverse (if BSC.null remStmt then acc else remStmt : acc)
            | otherwise =
                let b = BS.index bs idx
                 in if b == 39 -- single quote '
                        then
                            if inQuote && idx + 1 < len && BS.index bs (idx + 1) == 39
                                then go (idx + 2) True start acc
                                else go (idx + 1) (not inQuote) start acc
                        else
                            if not inQuote && b == 59 -- semicolon ;
                                then
                                    let stmt = BSC.strip (BS.take (idx + 1 - start) (BS.drop start bs))
                                     in go (idx + 1) False (idx + 1) (stmt : acc)
                                else go (idx + 1) inQuote start acc

-- ----------------------------------------------------------------------------
-- Mutators
-- ----------------------------------------------------------------------------

-- | Mutator a: Injects unreferenced dead table creation and population at random positions.
injectDeadTables :: ByteString -> Gen ByteString
injectDeadTables bs
    | "noise_tbl" `BSC.isInfixOf` bs = pure bs
    | otherwise = do
        let stmts = splitStatements bs
            createStmt = "CREATE TABLE noise_tbl (col INT);"
            insertStmt = "INSERT INTO noise_tbl VALUES (42);"
            n = length stmts
        i <- chooseInt (0, n)
        let stmts1 = take i stmts ++ [createStmt] ++ drop i stmts
        j <- chooseInt (i + 1, n + 1)
        let stmts2 = take j stmts1 ++ [insertStmt] ++ drop j stmts1
        pure (BSC.unlines stmts2)

bloatCondition :: ByteString -> Gen ByteString
bloatCondition cond = do
    wrapper <-
        elements
            [ \c -> "(" <> c <> ") AND (1 = 1)"
            , \c -> "(" <> c <> ") AND (NULLIF(1, 0) IS NOT NULL)"
            , \c -> "(" <> c <> ") OR FALSE"
            , \c -> "(1 = 1) AND (" <> c <> ")"
            , \c -> "(" <> c <> ") AND NOT FALSE"
            , \c -> "(" <> c <> ") AND (0 = 0)"
            ]
    pure (wrapper cond)

findConditionEnd :: ByteString -> Int
findConditionEnd bs = go 0 False 0
    where
        go :: Int -> Bool -> Int -> Int
        len = BS.length bs
        go idx inQuote parenDepth
            | idx >= len = len
            | otherwise =
                let b = BS.index bs idx
                 in if b == 39 -- single quote '
                        then go (idx + 1) (not inQuote) parenDepth
                        else
                            if inQuote
                                then go (idx + 1) inQuote parenDepth
                                else
                                    if b == 40 -- (
                                        then go (idx + 1) inQuote (parenDepth + 1)
                                        else
                                            if b == 41 -- )
                                                then go (idx + 1) inQuote (max 0 (parenDepth - 1))
                                                else
                                                    if parenDepth == 0 && (b == 59 || matchesClause idx) -- ;
                                                        then idx
                                                        else go (idx + 1) inQuote parenDepth

        matchesClause i =
            let rest = BSC.map toUpper (BS.drop i bs)
             in any
                    (`BSC.isPrefixOf` rest)
                    [ "GROUP BY"
                    , "ORDER BY"
                    , "HAVING"
                    , "LIMIT"
                    , "WINDOW"
                    ]

-- | Mutator b: Wraps existing WHERE conditions with redundant expressions.
bloatExpressions :: ByteString -> Gen ByteString
bloatExpressions bs = do
    let stmts = splitStatements bs
    let whereIndices = [i | (i, s) <- zip [0 ..] stmts, "WHERE " `BSC.isInfixOf` s]
    case whereIndices of
        [] -> pure bs
        _ -> do
            targetIdx <- elements whereIndices
            let targetStmt = stmts !! targetIdx
            let (beforeWhere, fromWhere) = BSC.breakSubstring "WHERE " targetStmt
            if BSC.null fromWhere
                then pure bs
                else do
                    let afterWhere = BS.drop 6 fromWhere
                    let condEnd = findConditionEnd afterWhere
                    let rawCond = BS.take condEnd afterWhere
                    let trailing = BS.drop condEnd afterWhere
                    bloatedCond <- bloatCondition (BSC.strip rawCond)
                    let newStmt = beforeWhere <> "WHERE " <> bloatedCond <> " " <> trailing
                    let newStmts = take targetIdx stmts ++ [newStmt] ++ drop (targetIdx + 1) stmts
                    pure (BSC.unlines newStmts)

-- | Mutator c: Appends redundant ORDER BY and LIMIT clauses.
injectOptionalClauses :: ByteString -> Gen ByteString
injectOptionalClauses bs = do
    let stmts = splitStatements bs
    let isSelect s = "SELECT" `BSC.isInfixOf` BSC.map toUpper s
    case filter (isSelect . snd) (zip [0 ..] stmts) of
        [] -> pure bs
        selectPairs -> do
            (targetIdx, targetStmt) <- elements selectPairs
            clauseChoice <- elements ["ORDER BY 1", "LIMIT 9999", "ORDER BY 1 LIMIT 9999"]
            let upper = BSC.map toUpper targetStmt
                hasOrderBy = "ORDER BY" `BSC.isInfixOf` upper
                hasLimit = "LIMIT" `BSC.isInfixOf` upper
            let chosenClause = case (hasOrderBy, hasLimit) of
                    (False, False) -> Just clauseChoice
                    (False, True) -> Nothing
                    (True, False) -> Just "LIMIT 9999"
                    (True, True) -> Nothing
            case chosenClause of
                Nothing -> pure bs
                Just cl -> do
                    let stripped = BSC.strip targetStmt
                        body = if BSC.isSuffixOf ";" stripped then BSC.init stripped else stripped
                        newStmt = body <> " " <> cl <> ";"
                        newStmts = take targetIdx stmts ++ [newStmt] ++ drop (targetIdx + 1) stmts
                    pure (BSC.unlines newStmts)

-- | Mutator d: Injects lexical noise (comments and redundant whitespace).
injectLexicalNoise :: ByteString -> Gen ByteString
injectLexicalNoise bs = do
    let stmts = splitStatements bs
    if null stmts
        then pure bs
        else do
            noiseKind <- chooseInt (1, 3)
            case noiseKind of
                1 -> do
                    idx <- chooseInt (0, length stmts)
                    comment <-
                        elements
                            [ "/* fuzz_mutation_noise */"
                            , "/* noise comment */"
                            , "/* unnecessary_bloat_noise */"
                            ]
                    let (h, t) = splitAt idx stmts
                    pure (BSC.unlines (h ++ [comment] ++ t))
                2 -> do
                    idx <- chooseInt (0, length stmts)
                    comment <-
                        elements
                            [ "-- noise comment\n"
                            , "-- mutational fuzz comment\n"
                            , "-- unnecessary line comment\n"
                            ]
                    let (h, t) = splitAt idx stmts
                    pure (BSC.unlines (h ++ [comment] ++ t))
                _ -> do
                    idx <- chooseInt (0, length stmts)
                    ws <- elements ["\n", "   \n", "\n\n"]
                    let (h, t) = splitAt idx stmts
                    pure (BSC.unlines (h ++ [ws] ++ t))

-- | Mutator e: Combines 5 to 30 random mutations per test.
compositeMutate :: ByteString -> Gen ByteString
compositeMutate initialSQL = do
    numMutations <- chooseInt (5, 30)
    s0 <- injectDeadTables initialSQL
    foldM (\acc _ -> stepMutation acc) s0 [2 .. numMutations]
    where
        stepMutation sql =
            frequency
                [ (3, injectDeadTables sql)
                , (3, bloatExpressions sql)
                , (2, injectOptionalClauses sql)
                , (3, injectLexicalNoise sql)
                ]

-- ----------------------------------------------------------------------------
-- Syntactic Validity Checker
-- ----------------------------------------------------------------------------

checkTreeSitterValidity :: ByteString -> IO Bool
checkTreeSitterValidity sql = withParser $ \p -> do
    mTree <- parseSQL p sql
    case mTree of
        Nothing -> pure False
        Just tree -> do
            root <- rootNode tree
            errs <- collectDescendants (\t -> t == "ERROR" || t == "MISSING") root
            pure (null errs)

-- ----------------------------------------------------------------------------
-- QuickCheck Invariant Properties
-- ----------------------------------------------------------------------------

-- | Invariant 1: Injected dead tables are completely pruned
prop_deadTablesPruned :: Property
prop_deadTablesPruned = forAll (elements seedPool) $ \seed ->
    forAll (injectDeadTables (seedSQL seed)) $ \s_mutated -> monadicIO $ do
        origOk <- run (seedOracle seed s_mutated)
        assert origOk
        s_red <- run (reduceSQL (seedOracle seed) s_mutated)
        crashPreserved <- run (seedOracle seed s_red)
        monitor (counterexample "Crash was not preserved: oracle returned False")
        assert crashPreserved
        assert (not (BSC.isInfixOf "noise_tbl" s_red))

-- | Invariant 2: Bloated expressions are simplified
prop_bloatExpressionsSimplified :: Property
prop_bloatExpressionsSimplified = forAll (elements seedPool) $ \seed ->
    forAll (bloatExpressions (seedSQL seed)) $ \s_mutated -> monadicIO $ do
        origOk <- run (seedOracle seed s_mutated)
        assert origOk
        s_red <- run (reduceSQL (seedOracle seed) s_mutated)
        crashPreserved <- run (seedOracle seed s_red)
        monitor (counterexample "Crash was not preserved: oracle returned False")
        assert crashPreserved
        assert (BS.length s_red <= BS.length s_mutated)

-- | Invariant 3: Optional clauses (ORDER BY, LIMIT) are pruned
prop_optionalClausesPruned :: Property
prop_optionalClausesPruned = forAll (elements seedPool) $ \seed ->
    forAll (injectOptionalClauses (seedSQL seed)) $ \s_mutated -> monadicIO $ do
        origOk <- run (seedOracle seed s_mutated)
        assert origOk
        s_red <- run (reduceSQL (seedOracle seed) s_mutated)
        crashPreserved <- run (seedOracle seed s_red)
        monitor (counterexample "Crash was not preserved: oracle returned False")
        assert crashPreserved
        assert (not (BSC.isInfixOf "ORDER BY 1" s_red))
        assert (not (BSC.isInfixOf "LIMIT 9999" s_red))

-- | Invariant 4: Lexical noise does not compromise reduction or validity
prop_lexicalNoiseSoundness :: Property
prop_lexicalNoiseSoundness = forAll (elements seedPool) $ \seed ->
    forAll (injectLexicalNoise (seedSQL seed)) $ \s_mutated -> monadicIO $ do
        origOk <- run (seedOracle seed s_mutated)
        assert origOk
        s_red <- run (reduceSQL (seedOracle seed) s_mutated)
        crashPreserved <- run (seedOracle seed s_red)
        monitor (counterexample "Crash was not preserved: oracle returned False")
        assert crashPreserved
        valid <- run (checkTreeSitterValidity s_red)
        monitor (counterexample "Reduced SQL failed Tree-sitter syntax validation")
        assert valid

-- | Grand Invariant: Mutational Recovery property asserting all 4 invariants under composite mutation
prop_mutationRecovery :: Property
prop_mutationRecovery = forAll (elements seedPool) $ \seed ->
    forAll (compositeMutate (seedSQL seed)) $ \s_mutated -> monadicIO $ do
        origOk <- run (seedOracle seed s_mutated)
        monitor (counterexample "Mutated SQL failed initial crash oracle check")
        assert origOk
        s_red <- run (reduceSQL (seedOracle seed) s_mutated)
        -- 1. Crash is strictly preserved
        crashPreserved <- run (seedOracle seed s_red)
        monitor (counterexample "Invariant 1 Failed: Crash was not strictly preserved")
        assert crashPreserved
        -- 2. Monotonic length reduction
        let lenRed = BS.length s_red
            lenMut = BS.length s_mutated
        monitor
            (counterexample ("Invariant 2 Failed: Length not monotonic: " ++ show lenRed ++ " > " ++ show lenMut))
        assert (lenRed <= lenMut)
        -- 3. Tree-sitter parses s_red successfully (syntactic validity)
        valid <- run (checkTreeSitterValidity s_red)
        monitor (counterexample "Invariant 3 Failed: Tree-sitter parsing syntax error in reduced SQL")
        assert valid
        -- 4. Injected noise tables (e.g. noise_tbl) are pruned from s_red
        let noisePruned = not (BSC.isInfixOf "noise_tbl" s_red)
        monitor
            ( counterexample
                ("Invariant 4 Failed: Injected noise_tbl was not pruned from reduced SQL:\n" ++ BSC.unpack s_red)
            )
        assert noisePruned

-- ----------------------------------------------------------------------------
-- Test Suite
-- ----------------------------------------------------------------------------

mutationalFuzzTests :: TestTree
mutationalFuzzTests =
    testGroup
        "Mutational Fuzz Testing (Seed-and-Mutate Recovery)"
        [ testProperty
            "MutFuzz 1: Dead Tables Pruning"
            prop_deadTablesPruned
        , testProperty
            "MutFuzz 2: Bloated Expressions Simplification"
            prop_bloatExpressionsSimplified
        , testProperty
            "MutFuzz 3: Optional Clauses Pruning"
            prop_optionalClausesPruned
        , testProperty
            "MutFuzz 4: Lexical Noise Soundness & Reduction"
            prop_lexicalNoiseSoundness
        , localOption (QuickCheckTests 100) $
            testProperty
                "MutFuzz 5: Composite Mutation Recovery (All 4 Invariants)"
                prop_mutationRecovery
        ]

main :: IO ()
main = defaultMain mutationalFuzzTests
