module FrontendEditsChecks (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.List (find, findIndex, findIndices)
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Occam.Core (Span (spanEndByte, spanStartByte), Statement (stmtSpan))
import Occam.Facts (
    ParserEdit (..),
    ParserEditCategory (..),
    StatementFacts (..),
    StatementUnit (..),
 )
import Occam.Frontend.Pipeline (parseFrontend)
import Occam.Frontend.Span (isUtf8Boundary)
import Occam.Parser (
    ParseOptions (..),
    ParsedSQL (..),
    SQLDialect (..),
    defaultParseOptions,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

-- | Syntax-edit unit tests and frontend contract checks for @Occam.Frontend.Edits@.
tests :: TestTree
tests =
    testGroup
        "Pure-Haskell syntax edits (Occam.Frontend.Edits)"
        [ testCase "maps same-line Unicode and CRLF to exact byte boundaries" testUnicodeAndCrlf
        , testCase "VALUES rows and data types keep exact AST boundaries" testValuesAndTypes
        , testCase "chunked list edits delete contiguous VALUES rows before singles" testChunkedValuesRows
        , testCase "chunked list edits carry comments and preserve Unicode and CRLF" testChunkedCommentsCrlf
        , testCase "clause edits use full clause boundaries" testClauseBoundaries
        , testCase "clause and scalar subquery edits cover parentheses" testParenthesesAndSubquery
        , testCase
            "expression edits cover partial AST spans without malformed candidates"
            testExpressionCandidates
        , testCase "IN-subquery expression edits keep closing parentheses" testInSubqueryParentheses
        , testCase
            "PostgreSQL projection deletion keeps cast and dollar-quote boundaries"
            testPgCastDollarProjection
        , testCase "final statement edits stop before following comments" testFinalStatementCommentBoundary
        , testCase "window function candidates do not cut off OVER" testWindowFunctionOverBoundary
        , testCase
            "DDL constraint and expression edits cover constraints and operators"
            testConstraintAndExprCategories
        , testCase
            "UPDATE and DELETE emit assignment, clause, join, and returning edits"
            testUpdateAndDeleteEdits
        , testCase "set operations emit left and right simplification edits" testSetOperationEdits
        , testCase "DISTINCT, GROUP BY, and ORDER BY emit expected edits" testDistinctGroupOrderEdits
        , testCase "window specification emits empty, clause, and item edits" testWindowSpecEdits
        , testCase "CAST, ::, and TRY_CAST emit simplify-cast edits" testCastSimplificationEdits
        , testCase "window partition deletion skips nested ORDER BY" testWindowPartitionNestedOrderBy
        , testCase
            "gate, category, projection, and recursive flags uphold the contract"
            testFrontendContractFlags
        , testCase
            "ClickHouse PREWHERE/SETTINGS and DuckDB QUALIFY preserve exact clause spans"
            testDialectClauseEdits
        , testCase
            "DDL/trigger/set/dialect edge cases emit expected syntax edits"
            testDialectSpecificEdgeCases
        ]

parseOrFail :: ParseOptions -> ByteString -> IO ParsedSQL
parseOrFail opts sql = either (assertFailure . show) pure (parseFrontend opts sql)

editOpts :: SQLDialect -> Bool -> ParseOptions
editOpts dialect chunks =
    (defaultParseOptions dialect)
        { parseSyntaxOnly = True
        , parseSyntaxEdits = True
        , parseChunkEdits = chunks
        }

firstUnitEdits :: SQLDialect -> Bool -> ByteString -> IO (ParsedSQL, [ParserEdit])
firstUnitEdits dialect chunks sql = do
    parsed <- parseOrFail (editOpts dialect chunks) sql
    case parsedUnits parsed of
        unit : _ -> case unitFacts unit of
            Just facts -> do
                assertEqual "unit parse error" Nothing (unitParseError unit)
                let unitSpan = stmtSpan (unitStatement unit)
                    uStart = spanStartByte unitSpan
                    uEnd = spanEndByte unitSpan
                    edits = factEdits facts
                mapM_
                    ( \e -> do
                        assertBool
                            ("edit outside unitSpan: " <> show (e, uStart, uEnd))
                            (parserEditStart e >= uStart && parserEditStart e < parserEditEnd e && parserEditEnd e <= uEnd)
                        assertBool "edit start UTF-8 boundary" (isUtf8Boundary sql (parserEditStart e))
                        assertBool "edit end UTF-8 boundary" (isUtf8Boundary sql (parserEditEnd e))
                        assertBool "never emits EditColumn" (parserEditCategory e /= EditColumn)
                    )
                    edits
                pure (parsed, edits)
            Nothing -> assertFailure ("missing unitFacts: " <> show (unitParseError unit))
        [] -> assertFailure "expected at least one unit"

applyEdit :: ByteString -> ParserEdit -> IO ByteString
applyEdit sql edit = do
    let s = parserEditStart edit
        e = parserEditEnd edit
    assertBool ("start must be UTF-8 boundary: " <> show s) (isUtf8Boundary sql s)
    assertBool ("end must be UTF-8 boundary: " <> show e) (isUtf8Boundary sql e)
    assertBool ("start <= end: " <> show (s, e)) (s <= e && e <= BS.length sql)
    pure (BS.take s sql <> parserEditReplacement edit <> BS.drop e sql)

sliceEdit :: ByteString -> ParserEdit -> ByteString
sliceEdit sql edit = BS.take (parserEditEnd edit - parserEditStart edit) (BS.drop (parserEditStart edit) sql)

trimAscii :: ByteString -> ByteString
trimAscii = BS.dropWhile isSpaceByte . BS.reverse . BS.dropWhile isSpaceByte . BS.reverse
    where
        isSpaceByte w = w `elem` [0x20, 0x09, 0x0A, 0x0D]

requireEdit :: Text -> [ParserEdit] -> IO ParserEdit
requireEdit label edits = case find ((== label) . parserEditLabel) edits of
    Just edit -> pure edit
    Nothing -> assertFailure ("missing edit with label " <> show label)

assertReparses :: SQLDialect -> ByteString -> Text -> ByteString -> IO ()
assertReparses dialect sql label candidate = do
    reparsed <- parseOrFail (editOpts dialect False) candidate
    let errs = map unitParseError (parsedUnits reparsed)
    assertBool
        ( show label
            <> " emitted unparseable candidate:\n  sql: "
            <> BSC.unpack candidate
            <> "\n  orig: "
            <> BSC.unpack sql
            <> "\n  errs: "
            <> show errs
        )
        (all isNothing errs && not (null errs))

testUnicodeAndCrlf :: IO ()
testUnicodeAndCrlf = do
    let sql = encodeUtf8 "SELECT 'é', 1 + 2;\r\nSELECT 3;"
    (_, edits) <- firstUnitEdits SQLite False sql
    edit <- requireEdit "simplify-binary_expression" edits
    assertEqual "binary expression slice" "1 + 2" (sliceEdit sql edit)
    _ <- applyEdit sql edit
    pure ()

testValuesAndTypes :: IO ()
testValuesAndTypes = do
    let valuesSql = "INSERT INTO t(a,b) VALUES (1,2),(42,43);"
    (_, valEdits) <- firstUnitEdits SQLite False valuesSql
    edit <- requireEdit "remove-values_row" valEdits
    reduced <- applyEdit valuesSql edit
    assertEqual "VALUES first row deletion" "INSERT INTO t(a,b) VALUES (42,43);" reduced

    let typeSql = "CREATE TABLE t (id VARCHAR(255) NOT NULL); SELECT CAST(1 AS BIGINT);"
    parsed <- parseOrFail (editOpts PostgreSQL False) typeSql
    let typeSlices =
            [ sliceEdit typeSql e
            | unit <- parsedUnits parsed
            , Just facts <- [unitFacts unit]
            , e <- factEdits facts
            , parserEditCategory e == EditType
            ]
    assertBool ("expected VARCHAR(255) in " <> show typeSlices) ("VARCHAR(255)" `elem` typeSlices)
    assertBool ("expected BIGINT in " <> show typeSlices) ("BIGINT" `elem` typeSlices)

testChunkedValuesRows :: IO ()
testChunkedValuesRows = do
    let sql = "INSERT INTO t(a,b) VALUES (1,2),(3,4),(5,6),(42,43);"
    (_, edits) <- firstUnitEdits SQLite True sql
    let chunks = filter ((== "remove-values_row-chunk") . parserEditLabel) edits
    applied <- traverse (applyEdit sql) chunks
    assertEqual
        "contiguous row chunk deletions"
        [ "INSERT INTO t(a,b) VALUES (5,6),(42,43);"
        , "INSERT INTO t(a,b) VALUES (1,2),(3,4);"
        ]
        applied
    mapM_ (assertReparses SQLite sql "remove-values_row-chunk") applied
    let mFirstSingle = findIndex ((== "remove-values_row") . parserEditLabel) edits
        mLastChunk = listToMaybe (reverse (findIndices ((== "remove-values_row-chunk") . parserEditLabel) edits))
    assertBool "chunks must precede single-item deletions" $
        case (mLastChunk, mFirstSingle) of
            (Just lc, Just fs) -> lc < fs
            _ -> False
    single <- requireEdit "remove-values_row" edits
    reduced <- applyEdit sql single
    assertEqual "single row deletion" "INSERT INTO t(a,b) VALUES (3,4),(5,6),(42,43);" reduced

testChunkedCommentsCrlf :: IO ()
testChunkedCommentsCrlf = do
    let sql = encodeUtf8 "-- café\nSELECT * FROM t WHERE x IN (1, /* drop */ 2, 3);\r\n"
    (_, edits) <- firstUnitEdits SQLite True sql
    let chunks = filter ((== "remove-in_list-chunk") . parserEditLabel) edits
    applied <- traverse (applyEdit sql) chunks
    assertEqual
        "in_list chunk deletion preserves comment and CRLF"
        [encodeUtf8 "-- café\nSELECT * FROM t WHERE x IN (3);\r\n"]
        applied
    mapM_ (assertReparses SQLite sql "remove-in_list-chunk") applied

testClauseBoundaries :: IO ()
testClauseBoundaries = do
    let sql =
            "WITH c AS (SELECT x FROM t) SELECT a FROM u LEFT JOIN v ON u.id = v.id WHERE a = 1 GROUP BY a, b ORDER BY b LIMIT 5 OFFSET 2;"
    (_, edits) <- firstUnitEdits PostgreSQL False sql
    let checkTrimmed label expected = do
            e <- requireEdit label edits
            assertEqual (show label) expected (trimAscii (sliceEdit sql e))
    checkTrimmed "remove-with" "WITH c AS (SELECT x FROM t)"
    checkTrimmed "remove-join" "LEFT JOIN v ON u.id = v.id"
    checkTrimmed "remove-group_by" "GROUP BY a, b"
    checkTrimmed "remove-order_by" "ORDER BY b"
    checkTrimmed "remove-limit" "LIMIT 5"
    checkTrimmed "remove-offset" "OFFSET 2"

testParenthesesAndSubquery :: IO ()
testParenthesesAndSubquery = do
    let sql1 =
            "SELECT t_target.id, t_join.id FROM t_target JOIN t_join ON t_target.id = t_join.target_ref_id WHERE (col_timestamp_2 IS NOT NULL AND col_timestamp_1 IS NOT NULL);"
    (_, edits1) <- firstUnitEdits PostgreSQL False sql1
    e1 <- requireEdit "remove-where" edits1
    assertEqual
        "WHERE with outer parens"
        " WHERE (col_timestamp_2 IS NOT NULL AND col_timestamp_1 IS NOT NULL)"
        (sliceEdit sql1 e1)
    r1 <- applyEdit sql1 e1
    assertEqual
        "reduced WHERE"
        "SELECT t_target.id, t_join.id FROM t_target JOIN t_join ON t_target.id = t_join.target_ref_id;"
        r1

    let sql2 =
            "SELECT t_target.id, t_join.id FROM t_target JOIN t_join ON t_target.id = t_join.target_ref_id WHERE ((col_numeric_1 + 1.0) > 0.0 AND 1 = 1);"
    (_, edits2) <- firstUnitEdits PostgreSQL False sql2
    e2 <- requireEdit "remove-where" edits2
    r2 <- applyEdit sql2 e2
    assertEqual
        "reduced nested parens WHERE"
        "SELECT t_target.id, t_join.id FROM t_target JOIN t_join ON t_target.id = t_join.target_ref_id;"
        r2

    let sql3 = encodeUtf8 "-- café\nSELECT (SELECT 42);\r\n"
    (_, edits3) <- firstUnitEdits SQLite False sql3
    e3 <- requireEdit "simplify-scalar-subquery" edits3
    assertEqual "scalar subquery slice" "(SELECT 42)" (sliceEdit sql3 e3)
    r3 <- applyEdit sql3 e3
    assertEqual "simplified scalar subquery" (encodeUtf8 "-- café\nSELECT 42;\r\n") r3

testExpressionCandidates :: IO ()
testExpressionCandidates = do
    let sql =
            "SELECT id, payload FROM target_json WHERE ((((payload = '{\"trigger\": \"crash_segv\"}') OR FALSE) AND (NULLIF(1, 0) IS NOT NULL)) AND (1 = 1)) AND (1 = 1) LIMIT 9999;"
    (_, edits) <- firstUnitEdits SQLite False sql
    whereEdit <- requireEdit "remove-where" edits
    reduced <- applyEdit sql whereEdit
    assertEqual
        "WHERE deletion stops before LIMIT"
        "SELECT id, payload FROM target_json LIMIT 9999;"
        reduced
    forM_Labels ["simplify-binary_expression", "simplify-parenthesized_expression"] edits \matching label -> do
        assertBool ("missing " <> show label) (not (null matching))
        candidates <- traverse (applyEdit sql) matching
        mapM_ (assertReparses SQLite sql label) candidates
    where
        forM_Labels labels edits f = mapM_ (\lbl -> f (filter ((== lbl) . parserEditLabel) edits) lbl) labels

testInSubqueryParentheses :: IO ()
testInSubqueryParentheses = do
    let sql =
            "SELECT * FROM main_tasks WHERE ((((task_id IN (SELECT task_id FROM sub_items WHERE status = 'crashed_worker')) AND NOT FALSE) OR FALSE) AND NOT FALSE) AND NOT FALSE ORDER BY 1 LIMIT 9999;"
        subquery = "task_id IN (SELECT task_id FROM sub_items WHERE status = 'crashed_worker')"
    (_, edits) <- firstUnitEdits SQLite False sql
    let exprEdits = filter ((== EditExpression) . parserEditCategory) edits
    candidates <- traverse (applyEdit sql) exprEdits
    mapM_ (assertReparses SQLite sql "EditExpression") candidates
    assertBool
        "binary simplification preserves complete IN-subquery"
        ( any
            ( \e ->
                parserEditLabel e == "simplify-binary_expression" && BS.isInfixOf subquery (parserEditReplacement e)
            )
            exprEdits
        )

testPgCastDollarProjection :: IO ()
testPgCastDollarProjection = do
    let sql = "SELECT (1 + 2)::int AS value, $$crash;marker$$::text AS marker;"
        expected = "SELECT $$crash;marker$$::text AS marker;"
    (_, edits) <- firstUnitEdits PostgreSQL False sql
    let projEdits = filter ((== "remove-select_expression") . parserEditLabel) edits
    candidates <- traverse (applyEdit sql) projEdits
    mapM_ (assertReparses PostgreSQL sql "remove-select_expression") candidates
    assertBool "first projection deletion missing" (expected `elem` candidates)

testFinalStatementCommentBoundary :: IO ()
testFinalStatementCommentBoundary = do
    let sql =
            "SELECT sensor_id, avg(reading) FROM metric_series WHERE (((((1 = 1) AND ((1 = 1) AND ((1 = 1) AND (sensor_id = 1)))) AND NOT FALSE) AND (NULLIF(1, 0) IS NOT NULL)) AND (1 = 1)) AND (NULLIF(1, 0) IS NOT NULL) GROUP BY sensor_id ORDER BY 1;\n/* fuzz_mutation_noise */\n"
    (parsed, edits) <- firstUnitEdits SQLite False sql
    case parsedUnits parsed of
        [finalUnit] -> do
            orderEdit <- requireEdit "remove-order_by" edits
            assertEqual
                "order_by ends at semicolon"
                ";\n/* fuzz_mutation_noise */\n"
                (BS.drop (parserEditEnd orderEdit) sql)
            assertEqual
                "order_by stops at unitEnd"
                (spanEndByte (stmtSpan (unitStatement finalUnit)))
                (parserEditEnd orderEdit)
        other -> assertFailure ("expected 1 unit, got " <> show (length other))

testWindowFunctionOverBoundary :: IO ()
testWindowFunctionOverBoundary = do
    let sql = "SELECT 1 FROM t WHERE 1 < sum(x) OVER (PARTITION BY y) AND TRUE;"
    (_, edits) <- firstUnitEdits PostgreSQL False sql
    let mLit =
            find
                ( \e ->
                    parserEditCategory e == EditLiteral && sliceEdit sql e == "TRUE" && parserEditReplacement e == "0"
                )
                edits
    litEdit <- maybe (assertFailure "missing TRUE -> 0 literal edit") pure mLit
    litCand <- applyEdit sql litEdit
    assertEqual
        "literal replacement preserves OVER"
        "SELECT 1 FROM t WHERE 1 < sum(x) OVER (PARTITION BY y) AND 0;"
        litCand
    candidates <- traverse (applyEdit sql) edits
    mapM_ (assertReparses PostgreSQL sql "window_over") candidates

testConstraintAndExprCategories :: IO ()
testConstraintAndExprCategories = do
    let ddlSql =
            "CREATE TABLE t (id INT CONSTRAINT c1 NOT NULL, CONSTRAINT pk PRIMARY KEY (id), CONSTRAINT fk FOREIGN KEY (id) REFERENCES p(id), name TEXT);"
    (_, ddlEdits) <- firstUnitEdits PostgreSQL False ddlSql
    let constraintLabels = map parserEditLabel (filter ((== EditConstraint) . parserEditCategory) ddlEdits)
    assertBool "missing remove-column_constraint" ("remove-column_constraint" `elem` constraintLabels)
    let tableConstraints = [sliceEdit ddlSql e | e <- ddlEdits, parserEditLabel e == "remove-table_constraint"]
    assertEqual "two table constraints" 2 (length tableConstraints)
    assertBool
        "pk constraint slice"
        (any (BS.isInfixOf "CONSTRAINT pk PRIMARY KEY (id)") tableConstraints)
    assertBool
        "fk constraint slice"
        (any (BS.isInfixOf "CONSTRAINT fk FOREIGN KEY (id) REFERENCES p(id)") tableConstraints)

    let exprSql =
            "SELECT CASE WHEN a = 1 THEN 42 ELSE 0 END, COALESCE(x, y), z IS NOT NULL, b BETWEEN 1 AND 10 FROM t;"
    (_, exprEdits) <- firstUnitEdits PostgreSQL False exprSql
    let exprLabels = map parserEditLabel exprEdits
    assertBool "simplify-case_result" ("simplify-case_result" `elem` exprLabels)
    assertBool "simplify-function_to_argument" ("simplify-function_to_argument" `elem` exprLabels)
    assertBool "simplify-is_null" ("simplify-is_null" `elem` exprLabels)
    assertBool "simplify-between" ("simplify-between" `elem` exprLabels)

testUpdateAndDeleteEdits :: IO ()
testUpdateAndDeleteEdits = do
    let sql1 = "UPDATE t SET a = 1, b = 2, c = 3 WHERE id = 10;"
    (_, edits1) <- firstUnitEdits PostgreSQL False sql1
    assignments <-
        traverse (applyEdit sql1) (filter ((== "remove-update_assignment") . parserEditLabel) edits1)
    assertEqual
        "assignment deletions"
        [ "UPDATE t SET b = 2, c = 3 WHERE id = 10;"
        , "UPDATE t SET a = 1, c = 3 WHERE id = 10;"
        , "UPDATE t SET a = 1, b = 2 WHERE id = 10;"
        ]
        assignments
    whereEdit <- requireEdit "remove-where" edits1
    reducedWhere <- applyEdit sql1 whereEdit
    assertEqual "update where deletion" "UPDATE t SET a = 1, b = 2, c = 3;" reducedWhere

    (_, edits2) <- firstUnitEdits MySQL False "UPDATE t SET a = 1 ORDER BY id LIMIT 5;"
    assertBool "MySQL UPDATE remove-order_by" (any ((== "remove-order_by") . parserEditLabel) edits2)
    assertBool "MySQL UPDATE remove-limit" (any ((== "remove-limit") . parserEditLabel) edits2)

    (_, edits3) <- firstUnitEdits PostgreSQL False "UPDATE t SET a = 1 RETURNING a, b;"
    assertBool
        "PG UPDATE remove-returning_expression"
        (any ((== "remove-returning_expression") . parserEditLabel) edits3)
    assertBool "PG UPDATE remove-returning" (any ((== "remove-returning") . parserEditLabel) edits3)

    (_, edits4) <- firstUnitEdits MySQL False "DELETE FROM t WHERE id = 10 ORDER BY id LIMIT 5;"
    assertBool "MySQL DELETE remove-where" (any ((== "remove-where") . parserEditLabel) edits4)
    assertBool "MySQL DELETE remove-order_by" (any ((== "remove-order_by") . parserEditLabel) edits4)
    assertBool "MySQL DELETE remove-limit" (any ((== "remove-limit") . parserEditLabel) edits4)

    (_, edits5) <- firstUnitEdits PostgreSQL False "DELETE FROM t WHERE id = 10 RETURNING id;"
    assertBool "PG DELETE remove-where" (any ((== "remove-where") . parserEditLabel) edits5)
    assertBool "PG DELETE remove-returning" (any ((== "remove-returning") . parserEditLabel) edits5)

    (_, edits6) <-
        firstUnitEdits
            MySQL
            False
            "UPDATE t1 JOIN t2 ON t1.id = t2.id JOIN t3 ON t1.id = t3.id SET t1.a = 1;"
    assertEqual
        "MySQL multi-JOIN remove-join count"
        2
        (length (filter ((== "remove-join") . parserEditLabel) edits6))

testSetOperationEdits :: IO ()
testSetOperationEdits = do
    let checkSetOps sql expected = do
            (_, edits) <- firstUnitEdits PostgreSQL False sql
            applied <-
                traverse
                    (applyEdit sql)
                    [ e
                    | e <- edits
                    , parserEditLabel e `elem` ["simplify-set_operation_left", "simplify-set_operation_right"]
                    ]
            assertEqual (BSC.unpack sql) expected applied
    checkSetOps "SELECT 1 UNION ALL SELECT 2;" ["SELECT 1;", "SELECT 2;"]
    checkSetOps "(SELECT 1) UNION ALL (SELECT 2);" ["(SELECT 1);", "(SELECT 2);"]
    checkSetOps
        "SELECT 1 UNION ALL SELECT 2 ORDER BY 1 LIMIT 1;"
        ["SELECT 1 ORDER BY 1 LIMIT 1;", "SELECT 2 ORDER BY 1 LIMIT 1;"]
    checkSetOps
        "SELECT 999 AS noise UNION ALL (SELECT 888 AS noise) UNION ALL SELECT 42 AS target;"
        [ "SELECT 999 AS noise UNION ALL (SELECT 888 AS noise);"
        , "SELECT 42 AS target;"
        , "SELECT 999 AS noise UNION ALL SELECT 42 AS target;"
        , "(SELECT 888 AS noise) UNION ALL SELECT 42 AS target;"
        ]

testDistinctGroupOrderEdits :: IO ()
testDistinctGroupOrderEdits = do
    let sql = "SELECT DISTINCT a, b FROM t GROUP BY a, b ORDER BY a, b;"
    (_, edits) <- firstUnitEdits PostgreSQL False sql
    dEdit <- requireEdit "remove-distinct" edits
    dApplied <- applyEdit sql dEdit
    assertEqual "remove DISTINCT" "SELECT a, b FROM t GROUP BY a, b ORDER BY a, b;" dApplied

    grpApplied <-
        traverse (applyEdit sql) (filter ((== "remove-group_by_expression") . parserEditLabel) edits)
    assertEqual
        "GROUP BY expr edits"
        [ "SELECT DISTINCT a, b FROM t GROUP BY b ORDER BY a, b;"
        , "SELECT DISTINCT a, b FROM t GROUP BY a ORDER BY a, b;"
        ]
        grpApplied

    ordApplied <-
        traverse (applyEdit sql) (filter ((== "remove-order_by_expression") . parserEditLabel) edits)
    assertEqual
        "ORDER BY expr edits"
        [ "SELECT DISTINCT a, b FROM t GROUP BY a, b ORDER BY b;"
        , "SELECT DISTINCT a, b FROM t GROUP BY a, b ORDER BY a;"
        ]
        ordApplied

    let sqlAll = "SELECT ALL a FROM t;"
    (_, editsAll) <- firstUnitEdits PostgreSQL False sqlAll
    allApplied <- requireEdit "remove-distinct" editsAll >>= applyEdit sqlAll
    assertEqual "remove ALL" "SELECT a FROM t;" allApplied

    let sqlOn = "SELECT DISTINCT ON (a, b) c FROM t;"
    (_, editsOn) <- firstUnitEdits PostgreSQL False sqlOn
    onApplied <- requireEdit "remove-distinct" editsOn >>= applyEdit sqlOn
    assertEqual "remove DISTINCT ON" "SELECT c FROM t;" onApplied
    onExprs <-
        traverse (applyEdit sqlOn) (filter ((== "remove-distinct_on_expression") . parserEditLabel) editsOn)
    assertEqual
        "DISTINCT ON expr edits"
        [ "SELECT DISTINCT ON (b) c FROM t;"
        , "SELECT DISTINCT ON (a) c FROM t;"
        ]
        onExprs

    let sqlFn = "SELECT count(DISTINCT x) FROM t;"
    (_, editsFn) <- firstUnitEdits PostgreSQL False sqlFn
    fnApplied <- requireEdit "remove-distinct" editsFn >>= applyEdit sqlFn
    assertEqual "function DISTINCT" "SELECT count(x) FROM t;" fnApplied

testWindowSpecEdits :: IO ()
testWindowSpecEdits = do
    let sql =
            "SELECT sum(x) OVER (PARTITION BY a, b ORDER BY c, d ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) FROM t;"
    (_, edits) <- firstUnitEdits PostgreSQL False sql
    let checkSingle lbl expected = do
            applied <- requireEdit lbl edits >>= applyEdit sql
            assertEqual (show lbl) expected applied
    checkSingle "simplify-window_spec_empty" "SELECT sum(x) OVER () FROM t;"
    checkSingle
        "remove-window_partition_by"
        "SELECT sum(x) OVER (ORDER BY c, d ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) FROM t;"
    checkSingle
        "remove-window_order_by"
        "SELECT sum(x) OVER (PARTITION BY a, b ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) FROM t;"
    checkSingle "remove-window_frame" "SELECT sum(x) OVER (PARTITION BY a, b ORDER BY c, d) FROM t;"

    partExprs <-
        traverse
            (applyEdit sql)
            (filter ((== "remove-window_partition_by_expression") . parserEditLabel) edits)
    assertEqual
        "window PARTITION BY item edits"
        [ "SELECT sum(x) OVER (PARTITION BY b ORDER BY c, d ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) FROM t;"
        , "SELECT sum(x) OVER (PARTITION BY a ORDER BY c, d ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) FROM t;"
        ]
        partExprs

    orderExprs <-
        traverse (applyEdit sql) (filter ((== "remove-window_order_by_expression") . parserEditLabel) edits)
    assertEqual
        "window ORDER BY item edits"
        [ "SELECT sum(x) OVER (PARTITION BY a, b ORDER BY d ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) FROM t;"
        , "SELECT sum(x) OVER (PARTITION BY a, b ORDER BY c ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) FROM t;"
        ]
        orderExprs

testCastSimplificationEdits :: IO ()
testCastSimplificationEdits = do
    let checkCast dialect sql expected = do
            (_, edits) <- firstUnitEdits dialect False sql
            applied <- requireEdit "simplify-cast" edits >>= applyEdit sql
            assertEqual (BSC.unpack sql) expected applied
    checkCast PostgreSQL "SELECT CAST(col AS VARCHAR(255)) FROM t;" "SELECT col FROM t;"
    checkCast PostgreSQL "SELECT col::text FROM t;" "SELECT col FROM t;"
    checkCast DuckDB "SELECT TRY_CAST(col AS INT) FROM t;" "SELECT col FROM t;"

testWindowPartitionNestedOrderBy :: IO ()
testWindowPartitionNestedOrderBy = do
    let sql = "SELECT sum(a) OVER (PARTITION BY (SELECT b FROM u ORDER BY b LIMIT 1) ORDER BY c) FROM t"
    (_, edits) <- firstUnitEdits PostgreSQL False sql
    applied <- requireEdit "remove-window_partition_by" edits >>= applyEdit sql
    assertEqual "skips nested ORDER BY inside subquery" "SELECT sum(a) OVER (ORDER BY c) FROM t" applied

testFrontendContractFlags :: IO ()
testFrontendContractFlags = do
    let sql =
            "WITH RECURSIVE r(x) AS (SELECT 1 UNION ALL SELECT x + 1 FROM r WHERE x < 5) SELECT x, (SELECT y, z FROM s) FROM r;"
    (_, edits) <- firstUnitEdits PostgreSQL False sql
    assertBool
        "no EditColumn emitted by Workstream D"
        (all ((/= EditColumn) . parserEditCategory) edits)
    assertBool "top-level select_expression tagged as top projection" $
        any (\e -> parserEditLabel e == "remove-select_expression" && parserEditTopProjection e) edits
    assertBool "recursive predicate tagged as parserEditRecursivePredicate" $
        any (\e -> parserEditLabel e == "remove-where" && parserEditRecursivePredicate e) edits
    assertBool "inner select_expression not tagged as top projection" $
        isJust
            ( find
                (\e -> parserEditLabel e == "remove-select_expression" && not (parserEditTopProjection e))
                edits
            )

    gated <- parseOrFail ((defaultParseOptions PostgreSQL) {parseSyntaxEdits = False}) sql
    assertEqual
        "parseSyntaxEdits=False emits no edits"
        []
        [e | u <- parsedUnits gated, Just f <- [unitFacts u], e <- factEdits f]

    (_, noChunkEdits) <-
        firstUnitEdits SQLite False "INSERT INTO t(a,b) VALUES (1,2),(3,4),(5,6),(42,43);"
    assertBool
        "parseChunkEdits=False suppresses -chunk edits"
        (not (any (T.isSuffixOf "-chunk" . parserEditLabel) noChunkEdits))

testDialectClauseEdits :: IO ()
testDialectClauseEdits = do
    let chSql = "SELECT id FROM events PREWHERE active = 1 WHERE score > 10 SETTINGS max_threads = 2;"
    (_, chEdits) <- firstUnitEdits ClickHouse False chSql
    preEdit <- requireEdit "remove-prewhere" chEdits
    setEdit <- requireEdit "remove-settings" chEdits
    preApplied <- applyEdit chSql preEdit
    setApplied <- applyEdit chSql setEdit
    assertEqual
        "ClickHouse remove-prewhere"
        "SELECT id FROM events  WHERE score > 10 SETTINGS max_threads = 2;"
        preApplied
    assertEqual
        "ClickHouse remove-settings"
        "SELECT id FROM events PREWHERE active = 1 WHERE score > 10 ;"
        setApplied

    let duckSql = "SELECT id, row_number() OVER (ORDER BY id) AS rn FROM t QUALIFY rn = 1;"
    (_, duckEdits) <- firstUnitEdits DuckDB False duckSql
    qualEdit <- requireEdit "remove-qualify" duckEdits
    qualApplied <- applyEdit duckSql qualEdit
    assertEqual
        "DuckDB remove-qualify"
        "SELECT id, row_number() OVER (ORDER BY id) AS rn FROM t ;"
        qualApplied

testDialectSpecificEdgeCases :: IO ()
testDialectSpecificEdgeCases = do
    let trgSql =
            "CREATE TRIGGER tr AFTER INSERT ON t BEGIN UPDATE t SET x = 'a;b' WHERE id = NEW.id; END;"
    (_, trgEdits) <- firstUnitEdits SQLite False trgSql
    assertBool
        "SQLite trigger body emits remove-where"
        (any ((== "remove-where") . parserEditLabel) trgEdits)
    assertBool
        "SQLite trigger body emits literal edits"
        (any (\e -> parserEditCategory e == EditLiteral && sliceEdit trgSql e == "'a;b'") trgEdits)

    let chDdl =
            "CREATE TABLE t (k UInt64, x Nullable(UInt8)) ENGINE = MergeTree ORDER BY (k, CAST(x, 'UInt8'));"
    (_, chDdlEdits) <- firstUnitEdits ClickHouse False chDdl
    assertBool
        "ClickHouse ENGINE ORDER BY emits CAST function edits"
        ( any
            (\e -> parserEditLabel e == "simplify-function_to_argument" && parserEditReplacement e == "x")
            chDdlEdits
        )

    let pgPart = "CREATE TABLE t (d DATE) PARTITION BY RANGE (d);"
    (_, pgPartEdits) <- firstUnitEdits PostgreSQL False pgPart
    assertBool
        "PostgreSQL PARTITION BY emits function_to_argument"
        (any ((== "simplify-function_to_argument") . parserEditLabel) pgPartEdits)

    let pgSnap = "SET TRANSACTION SNAPSHOT 'snap123';"
    (_, pgSnapEdits) <- firstUnitEdits PostgreSQL False pgSnap
    assertBool
        "SET TRANSACTION SNAPSHOT emits literal edits"
        (any (\e -> parserEditCategory e == EditLiteral && sliceEdit pgSnap e == "'snap123'") pgSnapEdits)

    let duckStruct = "SELECT {'a': 1, 'b': 2};"
    (_, duckStructEdits) <- firstUnitEdits DuckDB False duckStruct
    assertEqual "DuckDB struct keys do not emit literal edits" [] duckStructEdits

    let duckCsv = "SELECT * FROM read_csv('data.csv', delim = '|', nullstr = 'NA');"
    (_, duckCsvEdits) <- firstUnitEdits DuckDB False duckCsv
    assertBool
        "DuckDB read_csv named args do not emit binary expr edits"
        (not (any ((== "simplify-binary_expression") . parserEditLabel) duckCsvEdits))

    let mysqlSet = "SET optimizer_switch=\"semijoin=off\";"
    (_, mysqlSetEdits) <- firstUnitEdits MySQL False mysqlSet
    assertBool
        "MySQL double-quoted SET value emits literal edits"
        ( any
            (\e -> parserEditCategory e == EditLiteral && sliceEdit mysqlSet e == "\"semijoin=off\"")
            mysqlSetEdits
        )

    let mariadbNames = "SET NAMES 'utf8mb4';"
    (_, mariadbNamesEdits) <- firstUnitEdits MariaDB False mariadbNames
    assertEqual "MariaDB SET NAMES emits no edits" [] mariadbNamesEdits
