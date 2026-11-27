module FrontendChecks (tests) where

import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Foldable (for_)
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Occam.Frontend.Lexer (
    Keyword (..),
    LexError (..),
    LexErrorKind (..),
    LexResult (..),
    LiteralKind (..),
    QuoteKind (..),
    SpannedToken,
    Token (..),
    TriviaKind (..),
    WordToken (..),
    isCommentToken,
    isWhitespaceToken,
    lexSQL,
    lexSQLStrict,
 )
import Occam.Frontend.OracleProof (
    DuckDBReplayUnit (..),
    duckdbReplayUnits,
    oracleCacheKey,
 )
import Occam.Frontend.Span (
    ByteSpan (..),
    Located (..),
    fromCoreSpan,
    isUtf8Boundary,
    isValidSpan,
    mergeSpan,
    sliceSpan,
    spanIsEmpty,
    spanLength,
    toCoreSpan,
    validateUtf8,
 )
import Occam.Frontend.Splitter (SplitUnit (..), splitSQL)
import Occam.Parser (
    DialectProfile (..),
    LexicalProfile (..),
    SQLDialect (..),
    allDialectProfiles,
    dialectProfile,
    nativeDuckDBReplayUnits,
    nativeOracleCacheKey,
 )
import Test.QuickCheck (
    Arbitrary (..),
    Gen,
    Property,
    counterexample,
    elements,
    forAll,
    listOf,
    oneof,
    (.&&.),
    (===),
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup
        "Pure-Haskell Frontend"
        [ spanTests
        , lexerTests
        , splitterTests
        , boundaryRegressions
        , oracleProofTests
        ]

boundaryRegressions :: TestTree
boundaryRegressions =
    testGroup
        "lexer and splitter regressions"
        [ testCase "PostgreSQL FOR and WHILE both close with END LOOP" $ do
            let sql =
                    "CREATE FUNCTION f() RETURNS void AS BEGIN FOR i IN 1..10 LOOP WHILE x > 0 LOOP SELECT i; END LOOP; END LOOP; END; SELECT 1;"
            units <- either (assertFailure . show) pure (splitSQL (dialectProfile PostgreSQL) sql)
            assertEqual "function and trailing statement" 2 (length units)
            assertEqual "trailing statement" "SELECT 1" (sliceUnit sql (last units))
        , testCase "unquoted end alias and column names do not close blocks" $ do
            let sql =
                    "CREATE TRIGGER tr AFTER INSERT ON t BEGIN SELECT end AS end FROM t; INSERT INTO log VALUES (1); END; SELECT 2;"
            units <- either (assertFailure . show) pure (splitSQL (dialectProfile SQLite) sql)
            assertEqual "trigger and trailing statement" 2 (length units)
            assertEqual "trailing statement" "SELECT 2" (sliceUnit sql (last units))
        , testCase "custom delimiters split attached words without changing ordinary dollar identifiers" $ do
            for_ [MySQL, MariaDB] $ \dialect -> do
                let sql =
                        "SELECT a$$;\nDELIMITER $$\nCREATE PROCEDURE p() BEGIN SELECT '$$'; END$$\nCOMMIT$$\nSELECT a$$\nDELIMITER ;\nSELECT 2;"
                units <- either (assertFailure . show) pure (splitSQL (dialectProfile dialect) sql)
                assertEqual
                    "unit slices"
                    [ "SELECT a$$"
                    , "DELIMITER $$"
                    , "CREATE PROCEDURE p() BEGIN SELECT '$$'; END"
                    , "COMMIT"
                    , "SELECT a"
                    , "DELIMITER ;"
                    , "SELECT 2"
                    ]
                    (map (sliceUnit sql) units)
                assertEqual
                    "delimiter ranges"
                    [Just ";", Nothing, Just "$$", Just "$$", Just "$$", Nothing, Just ";"]
                    (map (splitUnitDelimiterSpan >=> (`sliceSpan` sql)) units)
        , testCase "leading decimals and tuple indices have distinct tokens" $ do
            let LexResult tokens err = lexSQL (dialectProfile ClickHouse) "tup.1.2, .1, 1.2"
            assertEqual "lex error" Nothing err
            assertEqual
                "tokens"
                [ TokWord (WordToken "tup" Nothing Nothing)
                , TokDot
                , TokLiteral LitNumber "1"
                , TokDot
                , TokLiteral LitNumber "2"
                , TokComma
                , TokLiteral LitNumber ".1"
                , TokComma
                , TokLiteral LitNumber "1.2"
                ]
                (map locValue (filter (not . isWhitespaceToken . locValue) (V.toList tokens)))
        , testCase "quoted MySQL variables retain their opening byte and spelling" $ do
            let profile = dialectProfile MySQL
                LexResult tokens err = lexSQL profile "@@`sql_mode` @@global.`time_zone`"
            assertEqual "lex error" Nothing err
            assertEqual
                "variables"
                [TokSysVar "@@`sql_mode`", TokSysVar "@@global.`time_zone`"]
                (map locValue (filter (not . isWhitespaceToken . locValue) (V.toList tokens)))
            for_ ["@'bad", "@\"bad", "@`bad", "@@global.`bad"] $ \broken -> do
                let sql = "SELECT 1; " <> broken
                units <- either (assertFailure . show) pure (splitSQL profile sql)
                case units of
                    [_, failed] -> do
                        assertEqual "trailing unit starts at @" broken (sliceUnit sql failed)
                        assertEqual "error offset" (Just 10) (lexErrorOffset <$> splitUnitLexError failed)
                    _ -> assertFailure (show units)
        , testCase "custom lexical flags and keyword sets are honored" $ do
            let postgres = dialectProfile PostgreSQL
                profile =
                    postgres
                        { dialectKeywords = Set.singleton "CUSTOMWORD"
                        , dialectLexical =
                            (dialectLexical postgres)
                                { lexNamedParameters = True
                                , lexNumberedQuestionParameters = True
                                , lexQuestionOperators = False
                                }
                        }
                LexResult tokens err = lexSQL profile "customword @arg :arg ?12 ?|"
            assertEqual "lex error" Nothing err
            assertEqual
                "tokens from the supplied profile"
                [ TokWord (WordToken "customword" Nothing (Just (KwDialect "CUSTOMWORD")))
                , TokParam "@arg"
                , TokParam ":arg"
                , TokParam "?12"
                , TokQuestion
                , TokOperator "|"
                ]
                (map locValue (filter (not . isWhitespaceToken . locValue) (V.toList tokens)))
        ]

-- ---------------------------------------------------------------------------
-- Span & UTF-8 tests
-- ---------------------------------------------------------------------------

spanTests :: TestTree
spanTests =
    testGroup
        "Occam.Frontend.Span"
        [ testCase "ByteSpan merge, length, emptiness, and Core conversion" $ do
            let a = ByteSpan 2 5
                b = ByteSpan 7 12
                emptySp = ByteSpan 4 4
            assertEqual "mergeSpan convex hull" (ByteSpan 2 12) (mergeSpan a b)
            assertEqual "Semigroup (<>) is commutative" (a <> b) (b <> a)
            assertEqual "spanLength" 3 (spanLength a)
            assertEqual "empty spanLength" 0 (spanLength (ByteSpan 5 2))
            assertBool "spanIsEmpty on zero width" (spanIsEmpty emptySp)
            assertBool "non-empty span" (not (spanIsEmpty a))
            assertEqual "toCoreSpan/fromCoreSpan round-trip" a (fromCoreSpan (toCoreSpan a))
        , testCase "UTF-8 validation accepts valid sequences and pinpoints invalid offsets" $ do
            assertEqual "empty" (Right ()) (validateUtf8 "")
            assertEqual "ASCII + NUL" (Right ()) (validateUtf8 "abc\0xyz")
            assertEqual "2/3/4-byte valid UTF-8" (Right ()) (validateUtf8 (TE.encodeUtf8 "café中😀"))
            -- Stray continuation byte
            assertEqual "stray continuation at 3" (Left 3) (validateUtf8 "abc\x80def")
            -- Overlong 2-byte (0xC0, 0xC1)
            assertEqual "overlong 0xC0 at 1" (Left 1) (validateUtf8 "a\xc0\xafb")
            assertEqual "overlong 0xC1 at 0" (Left 0) (validateUtf8 "\xc1\x80")
            -- Overlong 3-byte (0xE0 0x80..0x9F)
            assertEqual "overlong 3-byte at 2" (Left 2) (validateUtf8 (TE.encodeUtf8 "é" <> "\xe0\x80\xaf"))
            -- UTF-16 surrogate U+D800 (0xED 0xA0 0x80) after 4-byte emoji
            assertEqual
                "surrogate after emoji at 4"
                (Left 4)
                (validateUtf8 (TE.encodeUtf8 "😀" <> "\xed\xa0\x80"))
            -- Overlong 4-byte (0xF0 0x80..0x8F)
            assertEqual "overlong 4-byte at 0" (Left 0) (validateUtf8 "\xf0\x80\x80\x80")
            -- Codepoint > U+10FFFF (0xF4 0x90 0x80 0x80)
            assertEqual "above U+10FFFF at 1" (Left 1) (validateUtf8 "x\xf4\x90\x80\x80")
            -- Truncated 3-byte sequence at EOF
            assertEqual "truncated 3-byte at 3" (Left 3) (validateUtf8 (TE.encodeUtf8 "中" <> "\xe4\xb8"))
            -- Invalid byte 0xFF
            assertEqual "0xFF at 6" (Left 6) (validateUtf8 "SELECT\xff")
        , testCase "isUtf8Boundary and sliceSpan enforce UTF-8 character boundaries" $ do
            let bs = TE.encodeUtf8 "a中😀b"
            -- Bytes: 'a'=[0..1), '中'=[1..4), '😀'=[4..8), 'b'=[8..9)
            assertEqual "valid boundaries" [True, True, False, False, True, False, False, False, True, True] $
                map (isUtf8Boundary bs) [0 .. 9]
            assertEqual "slice 中" (Just (TE.encodeUtf8 "中")) (sliceSpan (ByteSpan 1 4) bs)
            assertEqual "slice 😀" (Just (TE.encodeUtf8 "😀")) (sliceSpan (ByteSpan 4 8) bs)
            assertEqual "mid-char slice fails" Nothing (sliceSpan (ByteSpan 2 4) bs)
            assertEqual "out-of-bounds fails" Nothing (sliceSpan (ByteSpan 0 10) bs)
            assertBool "isValidSpan rejects mid-char" (not (isValidSpan bs (ByteSpan 1 5)))
        ]

-- ---------------------------------------------------------------------------
-- Lexer tests
-- ---------------------------------------------------------------------------

substantiveTokens :: LexResult -> [SpannedToken]
substantiveTokens = filter (not . isWhitespaceToken . locValue) . V.toList . lexTokens

nonTriviaTokens :: LexResult -> [SpannedToken]
nonTriviaTokens =
    filter (not . isWhitespaceToken . locValue)
        . filter (not . isCommentToken . locValue)
        . V.toList
        . lexTokens

lexerTests :: TestTree
lexerTests =
    testGroup
        "Occam.Frontend.Lexer"
        [ testCase "exact ByteSpan round-trip and UTF-8 multi-byte inputs across all dialects" $ do
            let sql = TE.encodeUtf8 "SELECT café, \"naïve😀\", '你好\\n' /* 块注释 */ -- 行注释\nFROM 表名;"
            for_ allDialectProfiles $ \prof -> do
                let res = lexSQL prof sql
                assertEqual ("no lex error for " <> show (dialectId prof)) Nothing (lexError res)
                let toks = V.toList (lexTokens res)
                    reconstructed = BS.concat [unsafeSlice sql sp | L sp _ <- toks]
                assertEqual ("exact byte round-trip for " <> show (dialectId prof)) sql reconstructed
                assertBool
                    ("every span valid UTF-8 for " <> show (dialectId prof))
                    (all (\(L sp _) -> isValidSpan sql sp && not (spanIsEmpty sp)) toks)
        , testCase "lexical error offsets are exact for invalid UTF-8 and unterminated literals/comments" $ do
            assertEqual
                "invalid UTF-8 after multi-byte prefix"
                (Left (LexError LexInvalidUtf8 10 "SQL input is not valid UTF-8"))
                (lexSQLStrict (dialectProfile PostgreSQL) ("SELECT " <> TE.encodeUtf8 "中" <> "\xff;"))
            assertEqual
                "unterminated single-quoted string"
                (Just (LexError LexUnterminatedString 7 "unterminated single-quoted string literal"))
                (lexError (lexSQL (dialectProfile SQLite) "SELECT 'unterminated"))
            assertEqual
                "unterminated escape string in PG"
                (Just (LexError LexUnterminatedString 7 "unterminated escape string literal"))
                (lexError (lexSQL (dialectProfile PostgreSQL) "SELECT E'abc\\'"))
            assertEqual
                "unterminated double-quoted identifier"
                (Just (LexError LexUnterminatedIdent 7 "unterminated double-quoted identifier"))
                (lexError (lexSQL (dialectProfile DuckDB) "SELECT \"col"))
            assertEqual
                "unterminated backtick identifier in MySQL"
                (Just (LexError LexUnterminatedIdent 7 "unterminated backtick-quoted identifier"))
                (lexError (lexSQL (dialectProfile MySQL) "SELECT `col"))
            assertEqual
                "unterminated bracket identifier in SQLite"
                (Just (LexError LexUnterminatedIdent 7 "unterminated bracket-quoted identifier"))
                (lexError (lexSQL (dialectProfile SQLite) "SELECT [col"))
            assertEqual
                "unterminated dollar-quoted string in PG"
                (Just (LexError LexUnterminatedDollarString 7 "unterminated dollar-quoted string literal"))
                (lexError (lexSQL (dialectProfile PostgreSQL) "SELECT $tag$body$wrong$"))
            assertEqual
                "unterminated nested block comment in PG reports outer start"
                (Just (LexError LexUnterminatedComment 7 "unterminated block comment"))
                (lexError (lexSQL (dialectProfile PostgreSQL) "SELECT /* a /* b */ c"))
            assertEqual
                "unterminated conditional comment in MySQL"
                (Just (LexError LexUnterminatedComment 7 "unterminated conditional block comment"))
                (lexError (lexSQL (dialectProfile MySQL) "SELECT /*!50000 1"))
        , testCase "dialect isolation: $$tag$$ dollar-quoting only in PostgreSQL and DuckDB" $ do
            let sql = "SELECT $tag$a;b$tag$, $$x$$;"
            for_ [PostgreSQL, DuckDB] $ \d -> do
                let toks = map locValue (nonTriviaTokens (lexSQL (dialectProfile d) sql))
                assertBool
                    (show d <> " lexes LitDollarString")
                    ( TokLiteral LitDollarString "$tag$a;b$tag$" `elem` toks
                        && TokLiteral LitDollarString "$$x$$" `elem` toks
                    )
            for_ [MySQL, MariaDB, SQLite, ClickHouse] $ \d -> do
                let toks = map locValue (nonTriviaTokens (lexSQL (dialectProfile d) sql))
                assertBool
                    (show d <> " must NOT lex LitDollarString")
                    (not (any isDollarLit toks))
        , testCase "dialect isolation: # line comments only in MySQL and MariaDB" $ do
            let sql = "SELECT 1 # comment\n, 2;"
            for_ [MySQL, MariaDB] $ \d -> do
                let res = lexSQL (dialectProfile d) sql
                    comments = filter (isCommentToken . locValue) (V.toList (lexTokens res))
                    vals = map locValue (nonTriviaTokens res)
                assertEqual
                    (show d <> " sees # as line comment")
                    [TokTrivia TriviaLineComment]
                    (map locValue comments)
                assertBool (show d <> " omits comment word") (not (any (isWordRaw "comment") vals))
            for_ [PostgreSQL, SQLite, DuckDB, ClickHouse] $ \d -> do
                let vals = map locValue (nonTriviaTokens (lexSQL (dialectProfile d) sql))
                assertBool (show d <> " sees # as operator, not comment") (TokOperator "#" `elem` vals)
                assertBool (show d <> " keeps token after #") (any (isWordRaw "comment") vals)
        , testCase "dialect isolation: [bracket] identifiers only in SQLite" $ do
            let sql = "SELECT [my col], X'deadbeef';"
                sqliteToks = map locValue (nonTriviaTokens (lexSQL (dialectProfile SQLite) sql))
            assertBool
                "SQLite lexes [my col] as QuoteBracket and X'...' as LitHexString"
                ( TokWord (WordToken "[my col]" (Just QuoteBracket) Nothing) `elem` sqliteToks
                    && TokLiteral LitHexString "X'deadbeef'" `elem` sqliteToks
                )
            for_ [PostgreSQL, MySQL, MariaDB, DuckDB, ClickHouse] $ \d -> do
                let toks = map locValue (nonTriviaTokens (lexSQL (dialectProfile d) sql))
                assertBool
                    (show d <> " must lex [ and ] separately")
                    (TokLBracket `elem` toks && TokRBracket `elem` toks)
        , testCase
            "dialect isolation: backticks, nested /* */, conditional /*! */, E'...', @user, ClickHouse ops"
            $ do
                -- Backticks
                for_ [MySQL, MariaDB, SQLite, ClickHouse] $ \d -> do
                    let toks = map locValue (nonTriviaTokens (lexSQL (dialectProfile d) "SELECT `a``b`;"))
                    assertBool (show d <> " supports backtick ident") $
                        TokWord (WordToken "`a``b`" (Just QuoteBacktick) Nothing) `elem` toks
                for_ [PostgreSQL, DuckDB] $ \d -> do
                    let toks = map locValue (nonTriviaTokens (lexSQL (dialectProfile d) "SELECT `a`;"))
                    assertBool (show d <> " rejects backtick ident") $
                        TokUnknown "`" `elem` toks
                -- Nested block comments (PG / DuckDB / ClickHouse)
                let nested = "/* outer /* inner */ still */ SELECT 1;"
                for_ [PostgreSQL, DuckDB, ClickHouse] $ \d -> do
                    let toks = map locValue (nonTriviaTokens (lexSQL (dialectProfile d) nested))
                    assertEqual (show d <> " skips entire nested comment") 3 (length toks)
                for_ [MySQL, MariaDB, SQLite] $ \d -> do
                    let toks = map locValue (nonTriviaTokens (lexSQL (dialectProfile d) nested))
                    assertBool (show d <> " closes block comment at first */") (any (isWordRaw "still") toks)
                -- Conditional comments (MySQL / MariaDB only)
                for_ [MySQL, MariaDB] $ \d -> do
                    let toks = map locValue (substantiveTokens (lexSQL (dialectProfile d) "/*!50000 SELECT 1 */"))
                    assertEqual (show d <> " tags conditional comment") [TokTrivia TriviaConditionalComment] toks
                for_ [PostgreSQL, SQLite, DuckDB, ClickHouse] $ \d -> do
                    let toks = map locValue (substantiveTokens (lexSQL (dialectProfile d) "/*!50000 SELECT 1 */"))
                    assertEqual (show d <> " treats /*! as regular block comment") [TokTrivia TriviaBlockComment] toks
                -- E'...' escape strings (PG / DuckDB only)
                for_ [PostgreSQL, DuckDB] $ \d -> do
                    let toks = map locValue (nonTriviaTokens (lexSQL (dialectProfile d) "SELECT E'a\\'b';"))
                    assertBool (show d <> " lexes E'...' as LitEscapeString") $
                        TokLiteral LitEscapeString "E'a\\'b'" `elem` toks
                for_ [MySQL, MariaDB, SQLite, ClickHouse] $ \d -> do
                    let toks = map locValue (nonTriviaTokens (lexSQL (dialectProfile d) "SELECT E'a';"))
                    assertBool (show d <> " lexes E as word before 'a'") $
                        any (isWordRaw "E") toks && TokLiteral LitString "'a'" `elem` toks
                -- MySQL/MariaDB @user / @@sys vs PG $1 / ClickHouse operators
                for_ [MySQL, MariaDB] $ \d -> do
                    let toks = map locValue (nonTriviaTokens (lexSQL (dialectProfile d) "SELECT @u, @@global.mode;"))
                    assertBool (show d <> " lexes @user and @@sys") $
                        TokUserVar "@u" `elem` toks && TokSysVar "@@global.mode" `elem` toks
                let pgToks = map locValue (nonTriviaTokens (lexSQL (dialectProfile PostgreSQL) "SELECT $1, a @> b;"))
                assertBool "PG lexes $1 param and @> operator" $
                    TokParam "$1" `elem` pgToks && TokOperator "@>" `elem` pgToks
                let chToks =
                        map
                            locValue
                            ( nonTriviaTokens
                                (lexSQL (dialectProfile ClickHouse) "SELECT data.:Int64, x::Int32, j->'a', j->>'b', @v := 1;")
                            )
                assertBool "ClickHouse operators ::, .:, ->, ->>, := present" $
                    all (`elem` chToks) [TokDotColon, TokDoubleColon, TokArrow, TokDoubleArrow, TokColonEq]
        , testCase "dialect isolation: dialect-specific keywords are isolated" $ do
            let pgRes = map locValue (nonTriviaTokens (lexSQL (dialectProfile PostgreSQL) "SELECT ilike, qualify;"))
                duckRes = map locValue (nonTriviaTokens (lexSQL (dialectProfile DuckDB) "SELECT ilike, qualify;"))
            assertEqual
                "ILIKE is keyword in PG, QUALIFY is ident in PG"
                [ Just KwSelect
                , Just KwIlike
                , Nothing
                , Nothing
                , Nothing
                ]
                (map wordKw pgRes)
            assertEqual
                "ILIKE is ident in DuckDB, QUALIFY is keyword in DuckDB"
                [ Just KwSelect
                , Nothing
                , Nothing
                , Just KwQualify
                , Nothing
                ]
                (map wordKw duckRes)
        , testCase "numeric, range, hex, and exponent edge cases lex cleanly" $ do
            let toks = map locValue (nonTriviaTokens (lexSQL (dialectProfile PostgreSQL) "1..2, 1.e2, 0x, 1e+a"))
            assertEqual
                "numeric boundary tokens"
                [ TokLiteral LitNumber "1"
                , TokOperator ".."
                , TokLiteral LitNumber "2"
                , TokComma
                , TokLiteral LitNumber "1.e2"
                , TokComma
                , TokLiteral LitNumber "0"
                , TokWord (WordToken "x" Nothing Nothing)
                , TokComma
                , TokLiteral LitNumber "1"
                , TokWord (WordToken "e" Nothing Nothing)
                , TokPlus
                , TokWord (WordToken "a" Nothing Nothing)
                ]
                toks
        , testProperty
            "lexSQL round-trips arbitrary generated SQL snippets across all dialects"
            propLexRoundTrip
        ]
    where
        unsafeSlice bs (ByteSpan s e) = BS.take (e - s) (BS.drop s bs)
        isDollarLit (TokLiteral LitDollarString _) = True
        isDollarLit _ = False
        isWordRaw target (TokWord WordToken {wordRaw = raw}) = raw == target
        isWordRaw _ _ = False
        wordKw (TokWord WordToken {wordKeyword = mKw}) = mKw
        wordKw _ = Nothing

-- ---------------------------------------------------------------------------
-- Splitter tests
-- ---------------------------------------------------------------------------

splitterTests :: TestTree
splitterTests =
    testGroup
        "Occam.Frontend.Splitter"
        [ testCase "ignores empty, comment-only, and semicolon-only inputs" $ do
            for_ ["", "   \n\t ", "; ;; ;", "-- comment\n/* block */"] $ \sql ->
                assertEqual ("empty units for " <> show sql) (Right []) (splitSQL (dialectProfile PostgreSQL) sql)
        , testCase "preserves semicolons inside strings, quoted idents, comments, and parens" $ do
            let sql = "SELECT 'a;b', \"c;d\", /* x;y */ (1; SELECT 2;) /* trail;comment */; -- eol;comment\nSELECT 3;"
            case splitSQL (dialectProfile PostgreSQL) sql of
                Left err -> assertFailure (show err)
                Right [u0, u1] -> do
                    assertEqual
                        "first unit slice excludes trailing comment"
                        "SELECT 'a;b', \"c;d\", /* x;y */ (1; SELECT 2;)"
                        (sliceUnit sql u0)
                    assertEqual "second unit slice" "SELECT 3" (sliceUnit sql u1)
                Right units -> assertFailure ("expected 2 units, got " <> show (length units))
        , testCase "tracks BEGIN ... END, nested CASE ... END, and transactions accurately" $ do
            let sqliteSql =
                    "BEGIN; INSERT INTO t VALUES (1); COMMIT;\n"
                        <> "CREATE TRIGGER tr AFTER INSERT ON t BEGIN "
                        <> "SELECT CASE WHEN NEW.x > 0 THEN 1 ELSE 0 END; "
                        <> "INSERT INTO log VALUES (NEW.x); "
                        <> "END;\n"
                        <> "SELECT 99;"
            case splitSQL (dialectProfile SQLite) sqliteSql of
                Left err -> assertFailure (show err)
                Right [u0, u1, u2, u3, u4] -> do
                    assertEqual "tx begin" "BEGIN" (sliceUnit sqliteSql u0)
                    assertEqual "tx insert" "INSERT INTO t VALUES (1)" (sliceUnit sqliteSql u1)
                    assertEqual "tx commit" "COMMIT" (sliceUnit sqliteSql u2)
                    assertBool
                        "trigger kept whole"
                        ( BSC.isPrefixOf "CREATE TRIGGER tr" (sliceUnit sqliteSql u3)
                            && BSC.isSuffixOf "END" (sliceUnit sqliteSql u3)
                        )
                    assertEqual "trailing select" "SELECT 99" (sliceUnit sqliteSql u4)
                Right units -> assertFailure ("expected 5 units, got " <> show (length units))
            -- MySQL / MariaDB nested IF / CASE / LOOP / WHILE / REPEAT inside BEGIN ... END
            let mysqlSql =
                    "CREATE PROCEDURE p() BEGIN "
                        <> "IF x > 0 THEN SELECT 1; ELSE SELECT 2; END IF; "
                        <> "CASE x WHEN 1 THEN SELECT 3; END CASE; "
                        <> "LOOP SELECT 4; END LOOP; "
                        <> "WHILE x > 0 DO SELECT 5; END WHILE; "
                        <> "REPEAT SELECT 6; UNTIL x > 0 END REPEAT; "
                        <> "END; SELECT 7;"
            case splitSQL (dialectProfile MariaDB) mysqlSql of
                Left err -> assertFailure (show err)
                Right [_, u1] -> assertEqual "trailing select" "SELECT 7" (sliceUnit mysqlSql u1)
                Right units -> assertFailure ("expected 2 units, got " <> show (length units))
            -- BEGIN / END inside parens in CREATE TABLE do not push blocks
            let colsSql = "CREATE TABLE t (begin INT, end INT); SELECT 1;"
            case splitSQL (dialectProfile PostgreSQL) colsSql of
                Left err -> assertFailure (show err)
                Right units -> assertEqual "2 statements" 2 (length units)
            -- Unclosed expression CASE does not swallow following statement
            let brokenCase = "SELECT CASE WHEN 1 THEN 2; SELECT 3;"
            case splitSQL (dialectProfile PostgreSQL) brokenCase of
                Left err -> assertFailure (show err)
                Right units -> assertEqual "broken expr CASE splits at ;" 2 (length units)
        , testCase "does not misclassify BEGIN as compound block or REPEAT()/CURSOR FOR inside procedures" $ do
            for_ [PostgreSQL, MySQL, MariaDB] $ \d -> do
                let sqlId = "SELECT begin FROM t; CREATE TABLE begin (x INT); SELECT 1 AS begin; SELECT 2;"
                case splitSQL (dialectProfile d) sqlId of
                    Left err -> assertFailure (show err)
                    Right units ->
                        assertEqual
                            (show d <> " splits BEGIN identifier statements")
                            [ "SELECT begin FROM t"
                            , "CREATE TABLE begin (x INT)"
                            , "SELECT 1 AS begin"
                            , "SELECT 2"
                            ]
                            (map (sliceUnit sqlId) units)
            for_ [MySQL, MariaDB] $ \d -> do
                let procSql =
                        "CREATE PROCEDURE p() lbl: BEGIN "
                            <> "DECLARE c CURSOR FOR SELECT 1; "
                            <> "SELECT REPEAT('x', 5); "
                            <> "SELECT a FROM t FOR UPDATE; "
                            <> "END lbl; SELECT 42;"
                case splitSQL (dialectProfile d) procSql of
                    Left err -> assertFailure (show err)
                    Right [u0, u1] -> do
                        assertBool
                            (show d <> " proc kept whole")
                            (BSC.isPrefixOf "CREATE PROCEDURE p()" (sliceUnit procSql u0))
                        assertEqual (show d <> " trailing select isolated") "SELECT 42" (sliceUnit procSql u1)
                    Right units -> assertFailure (show d <> " expected 2 units, got " <> show (length units))
            let trigBeginTbl =
                    "CREATE TRIGGER tr AFTER INSERT ON begin BEGIN INSERT INTO log VALUES (1); END; SELECT 2;"
            case splitSQL (dialectProfile SQLite) trigBeginTbl of
                Left err -> assertFailure (show err)
                Right [u0, u1] -> do
                    assertBool
                        "trigger on table named begin kept whole"
                        (BSC.isPrefixOf "CREATE TRIGGER tr" (sliceUnit trigBeginTbl u0))
                    assertEqual "trailing select after trigger on begin" "SELECT 2" (sliceUnit trigBeginTbl u1)
                Right units -> assertFailure ("expected 2 units, got " <> show (length units))
            let pgAtomic =
                    "CREATE FUNCTION f() RETURNS int LANGUAGE SQL BEGIN ATOMIC SELECT 1; RETURN 2; END; SELECT 3;"
            case splitSQL (dialectProfile PostgreSQL) pgAtomic of
                Left err -> assertFailure (show err)
                Right [u0, u1] -> do
                    assertBool
                        "PG BEGIN ATOMIC kept whole"
                        (BSC.isPrefixOf "CREATE FUNCTION f()" (sliceUnit pgAtomic u0))
                    assertEqual "PG trailing select" "SELECT 3" (sliceUnit pgAtomic u1)
                Right units -> assertFailure ("expected 2 units, got " <> show (length units))
            let mariaAnon = "BEGIN NOT ATOMIC SELECT 1; SELECT 2; END; SELECT 3;"
            case splitSQL (dialectProfile MariaDB) mariaAnon of
                Left err -> assertFailure (show err)
                Right [_, u1] -> assertEqual "MariaDB BEGIN NOT ATOMIC trailing select" "SELECT 3" (sliceUnit mariaAnon u1)
                Right units -> assertFailure ("expected 2 units, got " <> show (length units))
        , testCase "supports MySQL/MariaDB client DELIMITER directives cleanly" $ do
            let sql =
                    "DELIMITER //\n"
                        <> "CREATE PROCEDURE p() BEGIN SELECT 1; SELECT 2; END//\n"
                        <> "DELIMITER ;\n"
                        <> "SELECT delimiter FROM t;"
            for_ [MySQL, MariaDB] $ \d ->
                case splitSQL (dialectProfile d) sql of
                    Left err -> assertFailure (show err)
                    Right [u0, u1, u2, u3] -> do
                        assertEqual "delimiter //" "DELIMITER //" (sliceUnit sql u0)
                        assertEqual
                            "procedure body"
                            "CREATE PROCEDURE p() BEGIN SELECT 1; SELECT 2; END"
                            (sliceUnit sql u1)
                        assertEqual "delimiter ;" "DELIMITER ;" (sliceUnit sql u2)
                        assertEqual "ordinary delimiter ident" "SELECT delimiter FROM t" (sliceUnit sql u3)
                    Right units -> assertFailure (show d <> " expected 4 units, got " <> show (length units))
            let wordDelim = "DELIMITER GO\nSELECT 1 GO\nSELECT GOOD FROM t GO\nDELIMITER ;\nSELECT 3;"
            case splitSQL (dialectProfile MySQL) wordDelim of
                Left err -> assertFailure (show err)
                Right [u0, u1, u2, u3, u4] -> do
                    assertEqual "delimiter GO" "DELIMITER GO" (sliceUnit wordDelim u0)
                    assertEqual "stmt 1" "SELECT 1" (sliceUnit wordDelim u1)
                    assertEqual "stmt 2 with GOOD" "SELECT GOOD FROM t" (sliceUnit wordDelim u2)
                    assertEqual "delimiter ;" "DELIMITER ;" (sliceUnit wordDelim u3)
                    assertEqual "stmt 3" "SELECT 3" (sliceUnit wordDelim u4)
                Right units -> assertFailure ("expected 5 units for DELIMITER GO, got " <> show (length units))
        , testCase "lexical-error suffix isolation preserves prior top-level boundaries" $ do
            let sql1 = "SELECT 1; SELECT 'unterminated"
            case splitSQL (dialectProfile SQLite) sql1 of
                Left err -> assertFailure (show err)
                Right [u0, u1] -> do
                    assertEqual "first unit clean" "SELECT 1" (sliceUnit sql1 u0)
                    assertEqual "first unit no error" Nothing (splitUnitLexError u0)
                    assertEqual "second unit covers error tail" "SELECT 'unterminated" (sliceUnit sql1 u1)
                    assertBool "second unit records LexUnterminatedString" $
                        fmap lexErrorKind (splitUnitLexError u1) == Just LexUnterminatedString
                Right units -> assertFailure ("expected 2 units, got " <> show (length units))
            -- Lexical error inside compound BEGIN ... END must roll back to before CREATE TRIGGER, NOT inner semicolon!
            let sql2 =
                    "SELECT 1; CREATE TRIGGER tr AFTER INSERT ON t BEGIN "
                        <> "INSERT INTO a VALUES (1); INSERT INTO b VALUES ('unterminated"
            case splitSQL (dialectProfile SQLite) sql2 of
                Left err -> assertFailure (show err)
                Right [u0, u1] -> do
                    assertEqual "first unit" "SELECT 1" (sliceUnit sql2 u0)
                    assertBool "second unit starts at CREATE TRIGGER" $
                        BSC.isPrefixOf "CREATE TRIGGER" (sliceUnit sql2 u1)
                Right units ->
                    assertFailure
                        ("expected 2 units (does not split inside broken trigger), got " <> show (length units))
        ]

sliceUnit :: ByteString -> SplitUnit -> ByteString
sliceUnit bs u =
    let ByteSpan s e = splitUnitSpan u
     in BS.take (e - s) (BS.drop s bs)

-- ---------------------------------------------------------------------------
-- OracleProof parity tests
-- ---------------------------------------------------------------------------

oracleProofTests :: TestTree
oracleProofTests =
    testGroup
        "Occam.Frontend.OracleProof"
        [ testCase "oracleCacheKey dialect gating, whitespace normalization, and comment sensitivity" $ do
            for_ [PostgreSQL, MySQL, MariaDB, ClickHouse] $ \d ->
                assertEqual (show d <> " returns Nothing") Nothing
                    =<< nativeOracleCacheKey d "SELECT 1 AS a;"
            for_ [SQLite, DuckDB] $ \d -> do
                k1 <-
                    nativeOracleCacheKey
                        d
                        "SELECT 1 AS a, 'x;y' AS b, -42 AS c, +7 AS d, NULL AS e, TRUE AS f, FALSE AS g;"
                k2 <-
                    nativeOracleCacheKey
                        d
                        " \n\tSELECT\t1\r\nAS a , 'x;y' AS b,-42 AS c,+7 AS d,NULL AS e,TRUE AS f,FALSE AS g ; "
                assertBool (show d <> " produces key") (isJust k1)
                assertEqual (show d <> " normalizes inter-token whitespace") k1 k2
                assertEqual (show d <> " rejects line comments") Nothing
                    =<< nativeOracleCacheKey d "SELECT 1 AS a -- c\n;"
                assertEqual (show d <> " rejects block comments") Nothing
                    =<< nativeOracleCacheKey d "SELECT /* c */ 1 AS a;"
                assertEqual (show d <> " rejects quoted alias") Nothing
                    =<< nativeOracleCacheKey d "SELECT 1 AS \"a\";"
                assertEqual (show d <> " rejects keyword alias") Nothing
                    =<< nativeOracleCacheKey d "SELECT 1 AS select;"
                assertEqual (show d <> " rejects non-ASCII alias") Nothing
                    =<< nativeOracleCacheKey d (TE.encodeUtf8 "SELECT 1 AS café;")
                assertEqual (show d <> " rejects decimal number") Nothing
                    =<< nativeOracleCacheKey d "SELECT 1.5 AS a;"
                assertEqual (show d <> " rejects hex/escape strings") Nothing
                    =<< nativeOracleCacheKey d "SELECT X'01' AS a;"
                assertEqual (show d <> " rejects NUL byte") Nothing
                    =<< nativeOracleCacheKey d "SELECT '\0' AS a;"
                assertEqual (show d <> " rejects multi-statement") Nothing
                    =<< nativeOracleCacheKey d "SELECT 1 AS a; SELECT 2 AS b;"
        , testCase "duckdbReplayUnits parity across create/insert/query/other and UTF-8 offsets" $ do
            let sql =
                    TE.encodeUtf8 $
                        "CREATE TABLE My_Tbl (a INT, b BIGINT, c VARCHAR, d BOOLEAN, e TINYINT, f SMALLINT, g INTEGER, h HUGEINT);\n"
                            <> "INSERT INTO My_Tbl VALUES (1, 'café'), (-2, NULL), (TRUE, FALSE);\n"
                            <> "SELECT COUNT(*), SUM(a) AS total FROM My_Tbl;\n"
                            <> "SELECT 42 AS answer;\n"
                            <> "CREATE TABLE raw_txt (s TEXT);\n"
                            <> "SELECT * FROM My_Tbl"
            units <- nativeDuckDBReplayUnits sql
            let expectedOffsets = map (+ 1) (BS.findIndices (== 59) sql)
                expected =
                    zipWith
                        (\off (kind, tbl) -> DuckDBReplayUnit off kind tbl)
                        (map Just expectedOffsets ++ [Nothing])
                        [ ("create", Just "my_tbl")
                        , ("insert", Just "my_tbl")
                        , ("query", Just "my_tbl")
                        , ("query", Nothing)
                        , ("other", Nothing)
                        , ("other", Nothing)
                        ]
            assertEqual "replay units match expected classification and byte offsets" expected units
            for_ (zip [0 ..] units) $ \(idx, u) -> case replayUnitEnd u of
                Nothing -> pure ()
                Just endOff ->
                    assertBool
                        ("unit " <> show (idx :: Int) <> " ends right after ';'")
                        (endOff >= 1 && endOff <= BS.length sql && BS.index sql (endOff - 1) == 59)
            assertEqual "NUL byte returns empty replay units" [] (duckdbReplayUnits "SELECT 1;\0")
            assertEqual "lex error returns empty replay units" [] (duckdbReplayUnits "SELECT 'unterminated")
            assertEqual
                "pure oracleCacheKey matches IO wrapper"
                (oracleCacheKey (dialectProfile SQLite) "SELECT 1 AS a;")
                =<< nativeOracleCacheKey SQLite "SELECT 1 AS a;"
        ]

-- ---------------------------------------------------------------------------
-- QuickCheck Generators & Properties
-- ---------------------------------------------------------------------------

newtype GenDialect = GenDialect SQLDialect
    deriving (Show)

instance Arbitrary GenDialect where
    arbitrary = GenDialect <$> elements [minBound .. maxBound]

genSqlChunk :: SQLDialect -> Gen ByteString
genSqlChunk d =
    oneof $
        [ elements
            [ "SELECT "
            , "FROM "
            , "CREATE TABLE t (a INT, b VARCHAR); "
            , "INSERT INTO t VALUES (1, 'a;b'); "
            , TE.encodeUtf8 "café "
            , TE.encodeUtf8 "表名 "
            , TE.encodeUtf8 "'你好😀' "
            , "\"q\"\"ident\" "
            , "-- line\n"
            , "/* block */ "
            , "123.45e-2 "
            , "0xdeadbeef "
            , "+ - * / = <> <= >= :: := -> ->> "
            , "( ) , ; "
            , "   \t\r\n"
            ]
        ]
            ++ [ elements ["$$body$$ ", "$tag$x;y$tag$ ", "E'a\\'b' ", "/* a /* b */ c */ ", "$1 "]
               | d `elem` [PostgreSQL, DuckDB]
               ]
            ++ [ elements ["# hash\n", "`bt``x` ", "/*!50000 1*/ ", "@u ", "@@version "] | d `elem` [MySQL, MariaDB]
               ]
            ++ [elements ["[br ident] ", "X'01AF' ", "$p ", ":p ", "@p ", "?1 "] | d == SQLite]
            ++ [elements ["`ch` ", "data.:Int64 ", "/* n /* m */ o */ "] | d == ClickHouse]

propLexRoundTrip :: GenDialect -> Property
propLexRoundTrip (GenDialect d) =
    forAll (BS.concat <$> listOf (genSqlChunk d)) $ \sql ->
        let prof = dialectProfile d
            res = lexSQL prof sql
            toks = V.toList (lexTokens res)
            reconstructed = BS.concat [BS.take (e - s) (BS.drop s sql) | L (ByteSpan s e) _ <- toks]
            contiguous = and [spanEnd sp1 == spanStart sp2 | (L sp1 _, L sp2 _) <- zip toks (drop 1 toks)]
            startsAtZero = null toks || spanStart (locSpan (head' toks)) == 0
            allValidUtf8 = all (\(L sp _) -> isValidSpan sql sp && not (spanIsEmpty sp)) toks
         in case lexError res of
                Just err -> counterexample ("unexpected lex error: " <> show err) False
                Nothing ->
                    counterexample ("dialect=" <> show d <> " sql=" <> show sql) $
                        (reconstructed === sql)
                            .&&. counterexample "contiguous" contiguous
                            .&&. counterexample "startsAtZero" startsAtZero
                            .&&. counterexample "allValidUtf8" allValidUtf8
    where
        head' (x : _) = x
        head' [] = error "unreachable"
