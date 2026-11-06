{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Fuzz (fuzzTests, generativeFuzzTests, main) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import MutationalFuzz (mutationalFuzzTests)
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
    frequency,
    oneof,
    shrinkList,
    shuffle,
    vectorOf,
 )
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)
import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)

-- ----------------------------------------------------------------------------
-- SQL Schema Representation
-- ----------------------------------------------------------------------------

data SqlType
    = SqlInt
    | SqlText
    | SqlNumeric
    | SqlJson
    | SqlBoolean
    | SqlTimestamp
    deriving (Eq, Show)

data ColumnDef = ColumnDef
    { colName :: !Text
    , colType :: !SqlType
    , colConstraints :: ![Text]
    }
    deriving (Eq, Show)

data TableDef = TableDef
    { tableName :: !Text
    , tableCols :: ![ColumnDef]
    }
    deriving (Eq, Show)

data TargetKind
    = TargetTableColRow !Text !Text !ByteString
    | TargetJoin !Text !Text !ByteString
    | TargetTx !Text !ByteString
    | TargetExpr !Text !ByteString
    deriving (Eq, Show)

data FuzzTarget = FuzzTarget
    { targetKind :: !TargetKind
    , targetDesc :: !Text
    }
    deriving (Eq, Show)

data FuzzScript = FuzzScript
    { scriptDdl :: ![Text]
    , scriptDml :: ![Text]
    , scriptTarget :: !FuzzTarget
    }
    deriving (Show)

renderScriptSQL :: FuzzScript -> ByteString
renderScriptSQL FuzzScript {..} =
    TE.encodeUtf8 (T.intercalate "\n" (scriptDdl ++ scriptDml))

-- ----------------------------------------------------------------------------
-- SQL Rendering Helpers
-- ----------------------------------------------------------------------------

renderType :: SqlType -> Text
renderType SqlInt = "INT"
renderType SqlText = "TEXT"
renderType SqlNumeric = "NUMERIC"
renderType SqlJson = "JSON"
renderType SqlBoolean = "BOOLEAN"
renderType SqlTimestamp = "TIMESTAMP"

renderColumnDef :: ColumnDef -> Text
renderColumnDef ColumnDef {..} =
    colName
        <> " "
        <> renderType colType
        <> if null colConstraints then "" else " " <> T.intercalate " " colConstraints

renderTableDef :: TableDef -> Text
renderTableDef TableDef {..} =
    "CREATE TABLE "
        <> tableName
        <> " ("
        <> T.intercalate ", " (map renderColumnDef tableCols)
        <> ");"

-- ----------------------------------------------------------------------------
-- Generator Implementation
-- ----------------------------------------------------------------------------

genLiteral :: SqlType -> Gen Text
genLiteral SqlInt = T.pack . show <$> chooseInt (1, 1000)
genLiteral SqlText =
    elements ["'alpha'", "'beta'", "'sample_val'", "'event_active'", "'standard_item'"]
genLiteral SqlNumeric =
    elements ["12.50", "42.0", "3.1415", "100.0", "-5.5"]
genLiteral SqlJson =
    elements ["'{\"status\": \"ok\", \"count\": 1}'", "'{\"user_id\": 42}'", "'[1, 2, 3]'"]
genLiteral SqlBoolean =
    elements ["TRUE", "FALSE"]
genLiteral SqlTimestamp =
    elements ["'2026-11-05 12:00:00'", "'2025-01-01 00:00:00'"]

genArithExpr :: Int -> [Text] -> Gen Text
genArithExpr 0 numericCols =
    if null numericCols
        then T.pack . show <$> chooseInt (1, 100)
        else oneof [elements numericCols, T.pack . show <$> chooseInt (1, 100)]
genArithExpr depth numericCols =
    let sub = genArithExpr (depth - 1) numericCols
     in oneof
            [ (\a b -> "(" <> a <> " + " <> b <> ")") <$> sub <*> sub
            , (\a b -> "(" <> a <> " - " <> b <> ")") <$> sub <*> sub
            , (\a b -> "(" <> a <> " * " <> b <> ")") <$> sub <*> sub
            , (\a -> "abs(" <> a <> ")") <$> sub
            , (\a -> "round(" <> a <> ", 2)") <$> sub
            ]

genBoolExpr :: Int -> TableDef -> Gen Text
genBoolExpr 0 TableDef {..} = do
    col <- elements tableCols
    case colType col of
        SqlInt -> do
            val <- chooseInt (0, 500)
            elements
                [ colName col <> " > " <> T.pack (show val)
                , colName col <> " = " <> T.pack (show val)
                , colName col <> " IS NOT NULL"
                ]
        SqlNumeric ->
            elements
                [ "abs(" <> colName col <> ") > 5.0"
                , colName col <> " IS NOT NULL"
                , "(" <> colName col <> " + 1.0) > 0.0"
                ]
        SqlText ->
            elements
                [ "lower(" <> colName col <> ") = 'alpha'"
                , "length(" <> colName col <> ") > 2"
                , "coalesce(" <> colName col <> ", 'default') != ''"
                , colName col <> " IS NOT NULL"
                ]
        SqlJson ->
            elements
                [ "json_extract(" <> colName col <> ", '$.status') IS NOT NULL"
                , colName col <> " IS NOT NULL"
                ]
        SqlBoolean ->
            elements
                [ colName col <> " = TRUE"
                , colName col <> " = FALSE"
                , colName col <> " IS NOT NULL"
                ]
        SqlTimestamp ->
            elements
                [ colName col <> " IS NOT NULL"
                , colName col <> " >= '2020-01-01 00:00:00'"
                ]
genBoolExpr depth tbl =
    let sub = genBoolExpr (depth - 1) tbl
     in oneof
            [ (\a b -> "(" <> a <> " AND " <> b <> ")") <$> sub <*> sub
            , (\a b -> "(" <> a <> " OR " <> b <> ")") <$> sub <*> sub
            , (\a -> "NOT (" <> a <> ")") <$> sub
            , (\a -> "(" <> a <> " AND 1 = 1)") <$> sub
            ]

genInsert :: TableDef -> Maybe (Text, Text) -> Gen Text
genInsert TableDef {..} mNeedle = do
    vals <- mapM assignVal tableCols
    let colNames = T.intercalate ", " (map colName tableCols)
        valTexts = T.intercalate ", " vals
    pure $ "INSERT INTO " <> tableName <> " (" <> colNames <> ") VALUES (" <> valTexts <> ");"
    where
        assignVal col = case mNeedle of
            Just (needleCol, needleVal)
                | colName col == needleCol -> pure needleVal
            _ -> genLiteral (colType col)

genUpdate :: TableDef -> Gen Text
genUpdate tbl@TableDef {..} = do
    targetCol <- elements tableCols
    newVal <- case colType targetCol of
        SqlInt -> genArithExpr 1 [colName c | c <- tableCols, colType c == SqlInt]
        SqlNumeric -> genArithExpr 1 [colName c | c <- tableCols, colType c == SqlNumeric]
        t -> genLiteral t
    cond <- genBoolExpr 1 tbl
    pure $
        "UPDATE "
            <> tableName
            <> " SET "
            <> colName targetCol
            <> " = "
            <> newVal
            <> " WHERE "
            <> cond
            <> ";"

genDelete :: TableDef -> Gen Text
genDelete tbl@TableDef {..} = do
    cond <- genBoolExpr 1 tbl
    pure $ "DELETE FROM " <> tableName <> " WHERE " <> cond <> ";"

genSelect :: TableDef -> Gen Text
genSelect tbl@TableDef {..} = do
    cols <- vectorOf 2 (elements tableCols)
    cond <- genBoolExpr 2 tbl
    orderCol <- elements tableCols
    limitN <- chooseInt (1, 20)
    let colList = T.intercalate ", " (nub (map colName cols))
    pure $
        "SELECT "
            <> colList
            <> " FROM "
            <> tableName
            <> " WHERE "
            <> cond
            <> " ORDER BY "
            <> colName orderCol
            <> " LIMIT "
            <> T.pack (show limitN)
            <> ";"

genJoinQuery :: TableDef -> TableDef -> Text -> Gen Text
genJoinQuery t1 t2 refCol = do
    cond <- genBoolExpr 1 t1
    pure $
        "SELECT "
            <> tableName t1
            <> ".id, "
            <> tableName t2
            <> ".id FROM "
            <> tableName t1
            <> " JOIN "
            <> tableName t2
            <> " ON "
            <> tableName t1
            <> ".id = "
            <> tableName t2
            <> "."
            <> refCol
            <> " WHERE "
            <> cond
            <> ";"

genTxBlock :: TableDef -> Maybe (Text, Text) -> Gen Text
genTxBlock tbl mNeedle = do
    ins <- genInsert tbl mNeedle
    upd <- genUpdate tbl
    pure $ "BEGIN;\n" <> ins <> "\n" <> upd <> "\nCOMMIT;"

genAlterAdd :: TableDef -> Int -> Gen Text
genAlterAdd TableDef {..} colId = do
    t <- elements [SqlInt, SqlNumeric, SqlText]
    let newCol = "extra_col_" <> T.pack (show colId)
    pure $ "ALTER TABLE " <> tableName <> " ADD COLUMN " <> newCol <> " " <> renderType t <> ";"

genAlterDrop :: TableDef -> Text -> Gen Text
genAlterDrop TableDef {..} colToDrop =
    pure $ "ALTER TABLE " <> tableName <> " DROP COLUMN " <> colToDrop <> ";"

genCreateIndex :: TableDef -> Int -> Gen Text
genCreateIndex TableDef {..} idxId = do
    col <- elements tableCols
    let idxName = "idx_" <> tableName <> "_" <> colName col <> "_" <> T.pack (show idxId)
    pure $ "CREATE INDEX " <> idxName <> " ON " <> tableName <> " (" <> colName col <> ");"

genDropDummy :: Int -> Gen Text
genDropDummy dummyId =
    pure $ "DROP TABLE IF EXISTS dummy_tbl_" <> T.pack (show dummyId) <> ";"

genTableDef :: Text -> Maybe (Text, Text) -> Gen TableDef
genTableDef name mFk = do
    extraColsCount <- chooseInt (2, 4)
    extraCols <- mapM (genCol mFk) [1 .. extraColsCount]
    let pkCol =
            ColumnDef
                { colName = "id"
                , colType = SqlInt
                , colConstraints = ["PRIMARY KEY"]
                }
    pure
        TableDef
            { tableName = name
            , tableCols = pkCol : extraCols
            }
    where
        genCol (Just (parentTbl, parentCol)) 1 =
            pure
                ColumnDef
                    { colName = "target_ref_id"
                    , colType = SqlInt
                    , colConstraints = ["REFERENCES " <> parentTbl <> "(" <> parentCol <> ")"]
                    }
        genCol _ idx = do
            t <- elements [SqlText, SqlNumeric, SqlJson, SqlBoolean, SqlTimestamp, SqlInt]
            constraints <-
                frequency
                    [ (4, pure [])
                    , (2, pure ["NOT NULL"])
                    ]
            pure
                ColumnDef
                    { colName = "col_" <> T.toLower (renderType t) <> "_" <> T.pack (show idx)
                    , colType = t
                    , colConstraints = constraints
                    }

genDistractor :: [TableDef] -> Int -> Gen Text
genDistractor tbls tag = do
    tbl <- elements tbls
    frequency
        [ (4, genInsert tbl Nothing)
        , (3, genUpdate tbl)
        , (2, genDelete tbl)
        , (4, genSelect tbl)
        , (2, genTxBlock tbl Nothing)
        , (1, genAlterAdd tbl tag)
        ,
            ( 1
            , if length (tableCols tbl) > 2
                then genAlterDrop tbl (colName (last (tableCols tbl)))
                else genAlterAdd tbl tag
            )
        , (1, genCreateIndex tbl tag)
        , (1, genDropDummy tag)
        ]

mutateStmt :: Text -> Gen Text
mutateStmt stmt = do
    choice <- chooseInt (0, 3)
    case choice of
        1 -> pure $ "-- fuzz_stress_comment\n" <> stmt
        2 -> pure $ "/* block comment */ " <> stmt
        _ -> pure stmt

genFuzzScript :: Gen FuzzScript
genFuzzScript = do
    targetStmtTotal <- chooseInt (10, 40)
    tag <- chooseInt (1000, 9999)
    targetChoice <- chooseInt (0, 3)

    let targetTblName = "t_target_" <> T.pack (show tag)
        refTblName = "t_join_" <> T.pack (show tag)
        needleStr = "crash_needle_" <> T.pack (show tag)
        needleQuoted = "'" <> needleStr <> "'"

    targetTblBase <- genTableDef targetTblName Nothing
    let targetCol = colName (last (tableCols targetTblBase))
        targetTbl = targetTblBase

    (target, initialStmts, targetStmts) <- case targetChoice of
        0 -> do
            ins <- genInsert targetTbl (Just (targetCol, needleQuoted))
            cond <- genBoolExpr 1 targetTbl
            let sel =
                    "SELECT "
                        <> targetCol
                        <> " FROM "
                        <> targetTblName
                        <> " WHERE "
                        <> targetCol
                        <> " = "
                        <> needleQuoted
                        <> " AND ("
                        <> cond
                        <> ");"
            let tgt =
                    FuzzTarget
                        { targetKind = TargetTableColRow targetTblName targetCol (TE.encodeUtf8 needleStr)
                        , targetDesc = "Requires table " <> targetTblName <> ", col " <> targetCol <> ", row " <> needleStr
                        }
            pure (tgt, [renderTableDef targetTbl], [ins, sel])
        1 -> do
            refTbl <- genTableDef refTblName (Just (targetTblName, "id"))
            ins1 <- genInsert targetTbl (Just (targetCol, needleQuoted))
            ins2 <- genInsert refTbl Nothing
            joinQ <- genJoinQuery targetTbl refTbl "target_ref_id"
            let tgt =
                    FuzzTarget
                        { targetKind = TargetJoin targetTblName refTblName (TE.encodeUtf8 needleStr)
                        , targetDesc =
                            "Requires join between " <> targetTblName <> " and " <> refTblName <> " with " <> needleStr
                        }
            pure (tgt, [renderTableDef targetTbl, renderTableDef refTbl], [ins1, ins2, joinQ])
        2 -> do
            tx <- genTxBlock targetTbl (Just (targetCol, needleQuoted))
            let tgt =
                    FuzzTarget
                        { targetKind = TargetTx targetTblName (TE.encodeUtf8 needleStr)
                        , targetDesc = "Requires transaction block on " <> targetTblName <> " with " <> needleStr
                        }
            pure (tgt, [renderTableDef targetTbl], [tx])
        _ -> do
            ins <- genInsert targetTbl (Just (targetCol, needleQuoted))
            let exprSnippet = "abs(" <> targetCol <> ")"
            let sel = "SELECT * FROM " <> targetTblName <> " WHERE " <> exprSnippet <> " > 0.0;"
            let tgt =
                    FuzzTarget
                        { targetKind = TargetExpr targetTblName (TE.encodeUtf8 exprSnippet)
                        , targetDesc = "Requires nested expression " <> exprSnippet <> " on " <> targetTblName
                        }
            pure (tgt, [renderTableDef targetTbl], [ins, sel])

    distCount <- chooseInt (1, 3)
    distTbls <-
        mapM
            (\i -> genTableDef ("t_dist_" <> T.pack (show tag) <> "_" <> T.pack (show i)) Nothing)
            [1 .. distCount]
    let allTbls = targetTbl : distTbls
        distCreateStmts = map renderTableDef distTbls

    let currentCount = length initialStmts + length distCreateStmts + length targetStmts
        needed = max 5 (targetStmtTotal - currentCount)

    distStmts <- vectorOf needed (genDistractor allTbls tag)
    mutatedDist <- mapM mutateStmt distStmts
    distShuffled <- shuffle mutatedDist

    let allDdl = initialStmts ++ distCreateStmts
        allDml = targetStmts ++ distShuffled

    pure
        FuzzScript
            { scriptDdl = allDdl
            , scriptDml = allDml
            , scriptTarget = target
            }

instance Arbitrary FuzzScript where
    arbitrary = genFuzzScript
    shrink FuzzScript {..} =
        -- QuickCheck native shrinking: drop distractor DML while preserving minimal target
        [ FuzzScript {scriptDml = dml', ..}
        | dml' <- shrinkList (const []) scriptDml
        , length dml' >= 2
        ]

-- ----------------------------------------------------------------------------
-- Programmatic Crash Oracle Simulation
-- ----------------------------------------------------------------------------

evalTargetCondition :: TargetKind -> ByteString -> Bool
evalTargetCondition (TargetTableColRow tbl col needle) bs =
    let tblBS = TE.encodeUtf8 tbl
        colBS = TE.encodeUtf8 col
     in tblBS `BSC.isInfixOf` bs
            && colBS `BSC.isInfixOf` bs
            && needle `BSC.isInfixOf` bs
evalTargetCondition (TargetJoin tbl1 tbl2 needle) bs =
    let t1BS = TE.encodeUtf8 tbl1
        t2BS = TE.encodeUtf8 tbl2
     in t1BS `BSC.isInfixOf` bs
            && t2BS `BSC.isInfixOf` bs
            && "JOIN" `BSC.isInfixOf` bs
            && needle `BSC.isInfixOf` bs
evalTargetCondition (TargetTx tbl needle) bs =
    let tblBS = TE.encodeUtf8 tbl
     in tblBS `BSC.isInfixOf` bs
            && "BEGIN" `BSC.isInfixOf` bs
            && "COMMIT" `BSC.isInfixOf` bs
            && needle `BSC.isInfixOf` bs
evalTargetCondition (TargetExpr tbl exprSnippet) bs =
    let tblBS = TE.encodeUtf8 tbl
     in tblBS `BSC.isInfixOf` bs
            && exprSnippet `BSC.isInfixOf` bs

makeScriptOracle :: FuzzScript -> Oracle
makeScriptOracle FuzzScript {..} bs =
    pure (evalTargetCondition (targetKind scriptTarget) bs)

-- ----------------------------------------------------------------------------
-- Invariant Checkers
-- ----------------------------------------------------------------------------

checkTreeSitterValidity :: ByteString -> IO Bool
checkTreeSitterValidity sql
    | BSC.null (BSC.filter (\c -> c /= ' ' && c /= '\n' && c /= '\r' && c /= '\t') sql) = pure True
    | otherwise = withParser $ \parser -> do
        mTree <- parseSQL parser sql
        case mTree of
            Nothing -> pure False
            Just tree -> do
                root <- rootNode tree
                errs <- collectDescendants (== "ERROR") root
                pure (null errs)

-- ----------------------------------------------------------------------------
-- QuickCheck Properties
-- ----------------------------------------------------------------------------

-- | Invariant 1: Tree-sitter validity (reduced output must parse cleanly with zero corruption)
prop_treeSitterValidity :: FuzzScript -> Property
prop_treeSitterValidity script = monadicIO $ do
    let initialSQL = renderScriptSQL script
        oracle = makeScriptOracle script
    reducedSQL <- run (reduceSQL oracle initialSQL)
    valid <- run (checkTreeSitterValidity reducedSQL)
    monitor
        ( counterexample
            ( "Tree-sitter parse failed on reduced SQL:\n"
                ++ BSC.unpack reducedSQL
                ++ "\nOracle desc: "
                ++ T.unpack (targetDesc (scriptTarget script))
            )
        )
    assert valid

-- | Synthetic invariant: reduced output must satisfy the generated text predicate.
prop_oracleSoundness :: FuzzScript -> Property
prop_oracleSoundness script = monadicIO $ do
    let initialSQL = renderScriptSQL script
        oracle = makeScriptOracle script
    reducedSQL <- run (reduceSQL oracle initialSQL)
    sound <- run (oracle reducedSQL)
    monitor
        ( counterexample
            ( "Oracle returned False on reduced SQL:\n"
                ++ BSC.unpack reducedSQL
                ++ "\nOracle desc: "
                ++ T.unpack (targetDesc (scriptTarget script))
            )
        )
    assert sound

-- | Invariant 3: Reduction monotonicity (reduced byte length <= initial byte length)
prop_reductionMonotonicity :: FuzzScript -> Property
prop_reductionMonotonicity script = monadicIO $ do
    let initialSQL = renderScriptSQL script
        oracle = makeScriptOracle script
    reducedSQL <- run (reduceSQL oracle initialSQL)
    let initLen = BSC.length initialSQL
        redLen = BSC.length reducedSQL
    monitor
        ( counterexample
            ( "Monotonicity violated: reduced length "
                ++ show redLen
                ++ " > initial length "
                ++ show initLen
            )
        )
    assert (redLen <= initLen)

-- | Invariant 4: Reducer idempotency (running reduceSQL again reaches the same fixed point)
prop_reductionIdempotency :: FuzzScript -> Property
prop_reductionIdempotency script = monadicIO $ do
    let initialSQL = renderScriptSQL script
        oracle = makeScriptOracle script
    reducedSQL <- run (reduceSQL oracle initialSQL)
    secondReducedSQL <- run (reduceSQL oracle reducedSQL)
    monitor
        ( counterexample
            ( "Idempotency violated: fixed-point not reached.\nFirst:\n"
                ++ BSC.unpack reducedSQL
                ++ "\nSecond:\n"
                ++ BSC.unpack secondReducedSQL
            )
        )
    assert (secondReducedSQL == reducedSQL)

-- | Comprehensive Heavy Fuzzing Property (All 4 invariants verified on every candidate)
prop_heavyFuzzAllInvariants :: FuzzScript -> Property
prop_heavyFuzzAllInvariants script = monadicIO $ do
    let initialSQL = renderScriptSQL script
        oracle = makeScriptOracle script
    reducedSQL <- run (reduceSQL oracle initialSQL)

    valid <- run (checkTreeSitterValidity reducedSQL)
    sound <- run (oracle reducedSQL)
    let mono = BSC.length reducedSQL <= BSC.length initialSQL
    secondReducedSQL <- run (reduceSQL oracle reducedSQL)
    let idem = secondReducedSQL == reducedSQL

    monitor
        ( counterexample
            ( "Fuzz invariant failure!\nTarget: "
                ++ T.unpack (targetDesc (scriptTarget script))
                ++ "\nValid CST: "
                ++ show valid
                ++ ", Soundness: "
                ++ show sound
                ++ ", Monotonicity: "
                ++ show mono
                ++ ", Idempotency: "
                ++ show idem
                ++ "\nInitial SQL ("
                ++ show (BSC.length initialSQL)
                ++ " bytes):\n"
                ++ BSC.unpack initialSQL
                ++ "\nReduced SQL ("
                ++ show (BSC.length reducedSQL)
                ++ " bytes):\n"
                ++ BSC.unpack reducedSQL
            )
        )
    assert (valid && sound && mono && idem)

-- ----------------------------------------------------------------------------
-- Test Suite Hierarchy
-- ----------------------------------------------------------------------------

generativeFuzzTests :: TestTree
generativeFuzzTests =
    testGroup
        "Generative Fuzzing (Tasty / QuickCheck)"
        [ testProperty
            "Fuzz 1: Tree-sitter CST Validity (Zero Unparse Corruption)"
            prop_treeSitterValidity
        , testProperty
            "Fuzz 2: Synthetic Predicate Preservation"
            prop_oracleSoundness
        , testProperty
            "Fuzz 3: Reduction Monotonicity (Len Reduced <= Initial)"
            prop_reductionMonotonicity
        , testProperty
            "Fuzz 4: Reducer Idempotency (Fixed-Point Reached)"
            prop_reductionIdempotency
        , localOption (QuickCheckTests 200) $
            testProperty
                "Fuzz 5: Deep Stress Invariants (All 4 Invariants Held under Multi-Statement Fuzzing)"
                prop_heavyFuzzAllInvariants
        ]

fuzzTests :: TestTree
fuzzTests =
    testGroup
        "Occam Fuzz Testing"
        [ generativeFuzzTests
        , mutationalFuzzTests
        ]
main :: IO ()
main = defaultMain fuzzTests
