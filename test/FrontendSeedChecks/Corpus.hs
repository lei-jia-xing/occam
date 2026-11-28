-- | Seed-derived dialect matrix and source-layout assertions.
module FrontendSeedChecks.Corpus (
    positiveTest,
    invalidTest,
    pgAvailabilityTest,
    pgPositiveTest,
    pgInvalidTest,
    pgCrossRejectTest,
    chPositiveTest,
    chInvalidTest,
    chCrossRejectTest,
    mysqlFamilyPositiveTest,
    mysqlFamilyInvalidTest,
    mysqlFamilyCrossRejectTest,
    sqliteTest,
    sqliteInvalidTest,
    duckdbTest,
    duckdbInvalidTest,
    seedLabel,
    contextLabel,
    spanWithin,
    sliceOrEmpty,
) where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Foldable (for_)
import Data.Foldable qualified as Foldable
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Database.SQLite3 qualified as Sqlite
import ExtraEngineOracles (
    MysqlServer,
    MysqlSyntaxError (..),
    clickhouseSyntaxCheck,
    mysqlClientScriptCheck,
    mysqlCrossRejectCheck,
    mysqlSyntaxCheckIsolated,
 )
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import FrontendAstInvariants (validateStatementInvariants)
import FrontendSeedLoader (
    SeedCase (..),
    UnitExpectation (..),
    expectedUnitDirective,
    expectedUnitSqlOnlyCount,
 )
import Occam.Frontend.Lexer (
    LexError (..),
    LexErrorKind (..),
    LexResult (..),
    SpannedToken,
    isTriviaToken,
    lexSQL,
    lexSQLStrict,
 )
import Occam.Frontend.Parser (
    ParseError (..),
    ParsedScript (..),
    Statement (..),
    parseScript,
    parseScriptUnits,
 )
import Occam.Frontend.Span (
    ByteSpan (..),
    Located (..),
    isNonEmptyValidSpan,
    isUtf8Boundary,
    mergeSpan,
    sliceSpan,
    spanEnd,
    spanIsEmpty,
    spanStart,
    unsafeSliceSpan,
    validateUtf8,
 )
import Occam.Frontend.Splitter (SplitUnit (..), splitSQL)
import Occam.Oracle.DuckDB.FFI (
    DuckDBAPI (..),
    closeDatabase,
    getDuckDBAPI,
    openFreshDatabase,
 )
import Occam.Parser (
    DialectProfile (..),
    allDialectProfiles,
    countStatements,
    dialectProfile,
 )
import PgQueryOracle (PgSyntaxError (..), pgQuerySplitCheck)
import System.Directory (doesDirectoryExist)
import System.Environment (lookupEnv)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

-- The moved matrix assertions follow.
positiveTest :: SeedCase -> TestTree
positiveTest seedCase =
    testCase (seedLabel seedCase) $
        for_ allDialectProfiles \profile ->
            if dialectId profile `elem` seedCaseDialects seedCase
                then do
                    assertAccepted seedCase profile
                    count <- countStatements (dialectId profile) (seedCaseSql seedCase)
                    assertEqual
                        "product counts the annotated SQL statements"
                        (Right (expectedUnitSqlOnlyCount seedCase))
                        count
                else assertRejectedByExactMatrix seedCase profile

invalidTest :: SeedCase -> TestTree
invalidTest seedCase =
    testCase (seedLabel seedCase) $
        for_ (seedCaseDialects seedCase) (assertRejectedInvalid seedCase . dialectProfile)

pgAvailabilityTest :: Bool -> TestTree
pgAvailabilityTest pgAvail =
    testCase "libpg_query dynamic library available when configured" $ do
        mEnv <- lookupEnv "OCCAM_PG_QUERY_LIB"
        nixExists <- doesDirectoryExist "/nix/store"
        case mEnv of
            Just p
                | not (null p) ->
                    assertBool ("OCCAM_PG_QUERY_LIB is set (" <> p <> ") but libpg_query failed to load") pgAvail
            _ ->
                if pgAvail || not nixExists
                    then pure ()
                    else
                        assertFailure
                            "libpg_query was not loaded in a Nix environment; set OCCAM_PG_QUERY_LIB or install libpg_query"

pgPositiveTest :: SeedCase -> TestTree
pgPositiveTest seedCase = testCase (seedLabel seedCase) $ do
    result <- pgQuerySplitCheck (seedCaseSql seedCase)
    case result of
        Left err ->
            assertFailure
                ( seedLabel seedCase
                    <> ": libpg_query rejected positive PostgreSQL seed at pos "
                    <> show (pgErrCursorPos err)
                    <> ": "
                    <> T.unpack (pgErrMessage err)
                )
        Right count ->
            assertEqual
                (seedLabel seedCase <> ": libpg_query statement count")
                (expectedUnitSqlOnlyCount seedCase)
                count

pgInvalidTest :: SeedCase -> TestTree
pgInvalidTest seedCase = testCase (seedLabel seedCase) $ do
    result <- pgQuerySplitCheck (seedCaseSql seedCase)
    case result of
        Left _ -> pure ()
        Right count ->
            assertFailure
                ( seedLabel seedCase
                    <> ": libpg_query accepted invalid PostgreSQL seed ("
                    <> show count
                    <> " statements)"
                )

pgCrossRejectTest :: SeedCase -> TestTree
pgCrossRejectTest seedCase = testCase (seedLabel seedCase) $ do
    result <- pgQuerySplitCheck (seedCaseSql seedCase)
    if seedCaseId seedCase `elem` pgCatalogOnlyCrossSeeds
        then
            -- MySQL/MariaDB `_utf8mb4'...'` introducers are parsed by PG gram.y as
            -- `TypeName 'string'` typed literals and only fail in PG catalog lookup.
            assertBool
                (seedLabel seedCase <> ": expected libpg_query to parse catalog-only introducer syntax")
                (case result of Right _ -> True; Left _ -> False)
        else case result of
            Left _ -> pure ()
            Right count ->
                assertFailure
                    ( seedLabel seedCase
                        <> ": non-PostgreSQL seed was accepted by libpg_query ("
                        <> show count
                        <> " statements) while rejected by Occam PostgreSQL parser"
                    )

pgCatalogOnlyCrossSeeds :: [T.Text]
pgCatalogOnlyCrossSeeds =
    [ "mysql_utf8_introducer_cast"
    , "mariadb_utf8_introducer_cast"
    ]

chPositiveTest :: SeedCase -> TestTree
chPositiveTest seedCase = testCase (seedLabel seedCase) $ do
    result <- clickhouseSyntaxCheck (seedCaseSql seedCase)
    case result of
        Left err ->
            assertFailure
                (seedLabel seedCase <> ": clickhouse format rejected positive ClickHouse seed: " <> T.unpack err)
        Right () -> pure ()

chInvalidTest :: SeedCase -> TestTree
chInvalidTest seedCase = testCase (seedLabel seedCase) $ do
    result <- clickhouseSyntaxCheck (seedCaseSql seedCase)
    case result of
        Left _ -> pure ()
        Right () ->
            assertFailure (seedLabel seedCase <> ": clickhouse format accepted invalid ClickHouse seed")

chCrossRejectTest :: SeedCase -> TestTree
chCrossRejectTest seedCase = testCase (seedLabel seedCase) $ do
    result <- clickhouseSyntaxCheck (seedCaseSql seedCase)
    if seedCaseId seedCase `elem` clickhouseFormatOnlyCrossSeeds
        then
            -- clickhouse format accepts these 3 grammar-quirk forms (ASOF RIGHT JOIN,
            -- VALUES ROW(...), client-side COPY) that clickhouse-local/server reject
            -- semantically (Code 48 / 27 / 78).
            assertBool
                (seedLabel seedCase <> ": expected clickhouse format quirk acceptance")
                (case result of Right () -> True; Left _ -> False)
        else case result of
            Left _ -> pure ()
            Right () ->
                assertFailure
                    ( seedLabel seedCase
                        <> ": non-ClickHouse seed was accepted by clickhouse format while rejected by Occam"
                    )

clickhouseFormatOnlyCrossSeeds :: [Text]
clickhouseFormatOnlyCrossSeeds =
    [ "duckdb_query_asof_join"
    , "mysql_dml_insert_values_row"
    , "pg_utility_copy_shared"
    ]

mysqlFamilyPositiveTest :: String -> MysqlServer -> SeedCase -> TestTree
mysqlFamilyPositiveTest label srv seedCase = testCase (seedLabel seedCase) $ do
    res <- mysqlSyntaxCheckIsolated srv (seedCaseSql seedCase)
    case res of
        Left err ->
            assertFailure
                ( seedLabel seedCase
                    <> ": "
                    <> label
                    <> " rejected positive seed ("
                    <> show (mysqlErrCode err)
                    <> "): "
                    <> T.unpack (mysqlErrMessage err)
                )
        Right () -> pure ()
    when (any (isJust . expectedUnitDirective) (seedCaseUnits seedCase)) $ do
        cliRes <- mysqlClientScriptCheck srv (seedCaseSql seedCase)
        case cliRes of
            Left err ->
                assertFailure
                    ( seedLabel seedCase
                        <> ": "
                        <> label
                        <> " CLI client rejected DELIMITER script: "
                        <> T.unpack err
                    )
            Right () -> pure ()

mysqlFamilyInvalidTest :: String -> MysqlServer -> SeedCase -> TestTree
mysqlFamilyInvalidTest label srv seedCase = testCase (seedLabel seedCase) $ do
    if seedCaseId seedCase == "invalid_mysql_delimiter_no_space"
        then do
            cliRes <- mysqlClientScriptCheck srv (seedCaseSql seedCase)
            case cliRes of
                Left _ -> pure ()
                Right () ->
                    assertFailure
                        (seedLabel seedCase <> ": " <> label <> " CLI client accepted invalid DELIMITER; seed")
        else do
            res <- mysqlSyntaxCheckIsolated srv (seedCaseSql seedCase)
            case res of
                Left _ -> pure ()
                Right () ->
                    assertFailure
                        (seedLabel seedCase <> ": " <> label <> " accepted invalid seed")

mysqlFamilyCrossRejectTest :: String -> MysqlServer -> SeedCase -> TestTree
mysqlFamilyCrossRejectTest label srv seedCase = testCase (seedLabel seedCase) $ do
    res <- mysqlCrossRejectCheck srv (seedCaseSql seedCase)
    case res of
        Left _ -> pure ()
        Right () ->
            assertFailure
                ( seedLabel seedCase
                    <> ": non-"
                    <> label
                    <> " seed was accepted by "
                    <> label
                    <> " while rejected by Occam"
                )

sqliteTest :: SeedCase -> TestTree
sqliteTest seedCase = testCase (seedLabel seedCase) $ do
    result <-
        try
            ( bracket (Sqlite.open ":memory:") Sqlite.close $ \database ->
                Sqlite.exec database (TE.decodeUtf8 (seedCaseSql seedCase))
            ) ::
            IO (Either SomeException ())
    case result of
        Left exception ->
            assertFailure
                (seedLabel seedCase <> ": SQLite rejected the positive SQL: " <> show exception)
        Right () -> pure ()

sqliteInvalidTest :: SeedCase -> TestTree
sqliteInvalidTest seedCase = testCase (seedLabel seedCase) $ do
    result <-
        try
            ( bracket (Sqlite.open ":memory:") Sqlite.close $ \database ->
                Sqlite.exec database (TE.decodeUtf8 (seedCaseSql seedCase))
            ) ::
            IO (Either Sqlite.SQLError ())
    case result of
        Right () ->
            assertFailure (seedLabel seedCase <> ": SQLite accepted invalid SQLite seed")
        Left err ->
            let details = T.toLower (Sqlite.sqlErrorDetails err)
                isSyntaxErr =
                    any
                        (`T.isInfixOf` details)
                        [ "syntax error"
                        , "incomplete input"
                        , "unrecognized token"
                        , "natural join"
                        , "cannot have both on and using"
                        ]
             in assertBool
                    (seedLabel seedCase <> ": expected SQLite syntax error, got: " <> show err)
                    isSyntaxErr

duckdbTest :: SeedCase -> TestTree
duckdbTest seedCase = testCase (seedLabel seedCase) $ do
    result <- duckdbSyntaxCount (seedCaseSql seedCase)
    case result of
        Left message -> assertFailure (seedLabel seedCase <> ": " <> message)
        Right count ->
            assertEqual
                (seedLabel seedCase <> ": DuckDB extracted statement count")
                (expectedUnitSqlOnlyCount seedCase)
                count

duckdbInvalidTest :: SeedCase -> TestTree
duckdbInvalidTest seedCase = testCase (seedLabel seedCase) $ do
    result <- duckdbSyntaxCount (seedCaseSql seedCase)
    case result of
        Left _ -> pure ()
        Right count ->
            assertFailure
                ( seedLabel seedCase
                    <> ": DuckDB accepted invalid DuckDB seed ("
                    <> show count
                    <> " statements)"
                )

seedLabel :: SeedCase -> String
seedLabel seedCase =
    seedCaseFile seedCase
        <> ":"
        <> show (seedCaseLine seedCase)
        <> " ["
        <> T.unpack (seedCaseId seedCase)
        <> "]"

profileLabel :: DialectProfile -> String
profileLabel = T.unpack . dialectCanonicalName

contextLabel :: SeedCase -> DialectProfile -> String
contextLabel seedCase profile = seedLabel seedCase <> " dialect=" <> profileLabel profile

-- ---------------------------------------------------------------------------
-- Positive matrix
-- ---------------------------------------------------------------------------

assertAccepted :: SeedCase -> DialectProfile -> Assertion
assertAccepted seedCase profile = do
    let sql = seedCaseSql seedCase
        label = contextLabel seedCase profile
        expected = seedCaseUnits seedCase
        segments = seedCaseSegments seedCase
        segmentSpans = seedCaseSegmentSpans seedCase
    rows <-
        either (assertFailure . ((label <> ": parseScriptUnits failed: ") <>) . show) pure $
            parseScriptUnits profile sql
    assertEqual (label <> ": unit count") (length expected) (length rows)
    assertEqual (label <> ": segment count") (length expected) (length segments)
    assertEqual (label <> ": segment span count") (length segments) (length segmentSpans)
    for_ (zip3 [0 :: Int ..] (zip expected rows) (zip segments segmentSpans)) $
        \(index, (expectation, (unit, result)), (segment, segmentSpan)) ->
            assertUnitLayout label sql profile expectation unit segment segmentSpan index
                *> assertParsedUnit label sql expectation unit result
    assertCleanLexer label profile sql
    case parseScript profile sql of
        Left err -> assertFailure (label <> ": parseScript failed: " <> show err)
        Right (ParsedScript statements) ->
            assertEqual
                (label <> ": parseScript SQL-only count")
                (expectedUnitSqlOnlyCount seedCase)
                (length statements)

assertRejectedByExactMatrix :: SeedCase -> DialectProfile -> Assertion
assertRejectedByExactMatrix seedCase profile =
    case parseScript profile (seedCaseSql seedCase) of
        Left err -> assertParseErrorLocation (contextLabel seedCase profile) (seedCaseSql seedCase) err
        Right (ParsedScript statements) ->
            assertFailure
                ( contextLabel seedCase profile
                    <> ": dialect is not in the exact accept set but parsed "
                    <> show (length statements)
                    <> " statements"
                )

assertUnitLayout ::
    String ->
    ByteString ->
    DialectProfile ->
    UnitExpectation ->
    SplitUnit ->
    ByteString ->
    ByteSpan ->
    Int ->
    Assertion
assertUnitLayout label sql profile expectation unit segment segmentSpan index = do
    let unitSpan = splitUnitSpan unit
        base = spanStart segmentSpan
    assertBool (label <> ": unit span valid") (isNonEmptyValidSpan sql unitSpan)
    assertBool (label <> ": unit inside expected segment") (spanWithin unitSpan segmentSpan)
    case lexSQL profile segment of
        LexResult tokens lexError' -> do
            assertEqual (label <> ": segment " <> show index <> " lex error") Nothing lexError'
            let tokens' =
                    [ L (ByteSpan (base + spanStart sp) (base + spanEnd sp)) token
                    | L sp token <- V.toList tokens
                    , not (isTriviaToken token)
                    ]
            case tokens' of
                [] -> assertFailure (label <> ": expected unit segment has no SQL tokens")
                firstToken : _ -> do
                    let firstStart = spanStart (locSpan firstToken)
                        coreEnd = spanEnd (hullOf tokens')
                    case expectation of
                        ExpectDirective delimiter -> do
                            let expectedUnit = ByteSpan firstStart coreEnd
                                expectedDirective = ByteSpan (coreEnd - BS.length delimiter) coreEnd
                            assertBool
                                (label <> ": directive argument is the segment suffix")
                                (BS.isSuffixOf delimiter (sliceOrEmpty sql expectedUnit))
                            assertEqual (label <> ": directive unit span") expectedUnit unitSpan
                            assertEqual
                                (label <> ": directive has no statement delimiter")
                                Nothing
                                (splitUnitDelimiterSpan unit)
                            case splitUnitDelimiterDirective unit of
                                Nothing -> assertFailure (label <> ": splitter did not classify DELIMITER")
                                Just (L directiveSpan directiveValue) -> do
                                    assertEqual (label <> ": directive argument bytes") delimiter directiveValue
                                    assertEqual (label <> ": directive argument span") expectedDirective directiveSpan
                                    assertEqual (label <> ": directive source bytes") (Just delimiter) (sliceSpan directiveSpan sql)
                        ExpectNoTerminator -> do
                            assertEqual (label <> ": unterminated unit span") (ByteSpan firstStart coreEnd) unitSpan
                            assertEqual (label <> ": unexpected statement delimiter") Nothing (splitUnitDelimiterSpan unit)
                            assertEqual (label <> ": unexpected directive") Nothing (splitUnitDelimiterDirective unit)
                        ExpectSql delimiter -> do
                            let coreSource = sliceOrEmpty sql (ByteSpan (spanStart segmentSpan) coreEnd)
                            assertBool
                                (label <> ": segment ends with declared delimiter " <> show delimiter)
                                (BS.isSuffixOf delimiter coreSource)
                            assertBool
                                (label <> ": delimiter fits before segment core")
                                (BS.length delimiter <= coreEnd - spanStart segmentSpan)
                            let delimiterStart = coreEnd - BS.length delimiter
                                expectedDelimiter = ByteSpan delimiterStart coreEnd
                                prefixBytes = BS.take (delimiterStart - spanStart segmentSpan) segment
                            case lexSQL profile prefixBytes of
                                LexResult prefixTokens prefixError -> do
                                    assertEqual (label <> ": delimiter prefix lex error") Nothing prefixError
                                    let prefixTokens' =
                                            [ L (ByteSpan (base + spanStart sp) (base + spanEnd sp)) token
                                            | L sp token <- V.toList prefixTokens
                                            , not (isTriviaToken token)
                                            ]
                                        expectedUnit = case prefixTokens' of
                                            [] -> ByteSpan firstStart firstStart
                                            _ -> ByteSpan firstStart (spanEnd (hullOf prefixTokens'))
                                    assertBool (label <> ": expected unit is non-empty") (not (spanIsEmpty expectedUnit))
                                    assertEqual (label <> ": statement unit span") expectedUnit unitSpan
                                    assertEqual
                                        (label <> ": statement delimiter span")
                                        (Just expectedDelimiter)
                                        (splitUnitDelimiterSpan unit)
                                    assertEqual
                                        (label <> ": statement delimiter bytes")
                                        (Just delimiter)
                                        (splitUnitDelimiterSpan unit >>= \delimiterSpan -> sliceSpan delimiterSpan sql)
                                    assertEqual (label <> ": unexpected directive") Nothing (splitUnitDelimiterDirective unit)
                                    assertBool
                                        (label <> ": unit and delimiter must not overlap")
                                        (spanEnd expectedUnit <= spanStart expectedDelimiter)

assertParsedUnit ::
    String ->
    ByteString ->
    UnitExpectation ->
    SplitUnit ->
    Either ParseError (Located Statement) ->
    Assertion
assertParsedUnit label sql expectation unit result = case (expectation, result) of
    ( ExpectDirective delimiter
        , Right statement@(L statementSpan (StatementDelimiter (L parsedSpan parsedValue)))
        ) -> do
            assertEqual (label <> ": parsed directive bytes") delimiter parsedValue
            assertEqual (label <> ": parsed directive span") splitterDirectiveSpan parsedSpan
            assertEqual (label <> ": parsed directive statement span") (splitUnitSpan unit) statementSpan
            validateStatementInvariants sql statement
    (ExpectDirective _, other) ->
        assertFailure (label <> ": expected StatementDelimiter, got " <> showParseResult other)
    (_, Right (L _ (StatementDelimiter _))) ->
        assertFailure (label <> ": unexpected synthetic StatementDelimiter for SQL unit")
    (_, Right statement) -> validateStatementInvariants sql statement
    (_, Left err) -> assertFailure (label <> ": unit parse failed: " <> show err)
    where
        splitterDirectiveSpan = case splitUnitDelimiterDirective unit of
            Just (L span' _) -> span'
            Nothing -> ByteSpan (spanStart (splitUnitSpan unit)) (spanStart (splitUnitSpan unit))

showParseResult :: Either ParseError (Located Statement) -> String
showParseResult = \case
    Left err -> "parse error " <> show err
    Right (L span' _) -> "statement at " <> show span'

assertCleanLexer :: String -> DialectProfile -> ByteString -> Assertion
assertCleanLexer label profile sql = do
    let LexResult tokens lexError' = lexSQL profile sql
        tokens' = V.toList tokens
    assertEqual (label <> ": lex error") Nothing lexError'
    assertBool
        (label <> ": every token span is valid and non-empty")
        (all (isNonEmptyValidSpan sql . locSpan) tokens')
    assertBool
        (label <> ": lexer starts at byte zero")
        (maybe True ((== 0) . spanStart . locSpan) (listToMaybe tokens'))
    assertBool
        (label <> ": lexer tokens are contiguous")
        ( and
            [ spanEnd (locSpan left) == spanStart (locSpan right) | (left, right) <- zip tokens' (drop 1 tokens')
            ]
        )
    assertBool
        (label <> ": lexer consumes the whole input")
        (null tokens' || spanEnd (locSpan (last tokens')) == BS.length sql)
    assertEqual
        (label <> ": lexer byte round-trip")
        sql
        (BS.concat [unsafeSliceSpan (locSpan token) sql | token <- tokens'])

-- ---------------------------------------------------------------------------
-- Invalid matrix
-- ---------------------------------------------------------------------------

assertRejectedInvalid :: SeedCase -> DialectProfile -> Assertion
assertRejectedInvalid seedCase profile = do
    let sql = seedCaseSql seedCase
        label = contextLabel seedCase profile
    case validateUtf8 sql of
        Left invalidOffset -> do
            case lexSQLStrict profile sql of
                Left lexError' -> do
                    assertEqual (label <> ": invalid UTF-8 kind") LexInvalidUtf8 (lexErrorKind lexError')
                    assertEqual (label <> ": invalid UTF-8 offset") invalidOffset (lexErrorOffset lexError')
                Right _ -> assertFailure (label <> ": lexSQLStrict accepted invalid UTF-8")
            case splitSQL profile sql of
                Left lexError' -> do
                    assertEqual (label <> ": splitSQL invalid UTF-8 kind") LexInvalidUtf8 (lexErrorKind lexError')
                    assertEqual (label <> ": splitSQL invalid UTF-8 offset") invalidOffset (lexErrorOffset lexError')
                Right _ -> assertFailure (label <> ": splitSQL accepted invalid UTF-8")
            case parseScript profile sql of
                Left _ -> pure ()
                Right _ -> assertFailure (label <> ": parseScript accepted invalid UTF-8")
        Right () -> do
            case splitSQL profile sql of
                Left lexError' ->
                    assertFailure (label <> ": valid UTF-8 must not make splitSQL return Left: " <> show lexError')
                Right _ -> pure ()
            case parseScript profile sql of
                Left parseError' -> assertParseErrorLocation label sql parseError'
                Right (ParsedScript statements) ->
                    assertFailure
                        (label <> ": reject case parsed " <> show (length statements) <> " statements")

assertParseErrorLocation :: String -> ByteString -> ParseError -> Assertion
assertParseErrorLocation label sql parseError' = do
    let ByteSpan start end = parseErrorSpan parseError'
        len = BS.length sql
    assertBool (label <> ": parse error start in bounds") (start >= 0 && start <= len)
    assertBool (label <> ": parse error end in bounds") (end >= start && end <= len)
    assertBool (label <> ": parse error start on UTF-8 boundary") (isUtf8Boundary sql start)
    assertBool (label <> ": parse error end on UTF-8 boundary") (isUtf8Boundary sql end)

-- ---------------------------------------------------------------------------
-- DuckDB syntax extraction
-- ---------------------------------------------------------------------------

duckdbSyntaxCount :: ByteString -> IO (Either String Int)
duckdbSyntaxCount sql = do
    loaded <- getDuckDBAPI
    case loaded of
        Left err -> pure (Left ("DuckDB API unavailable: " <> show err))
        Right api ->
            bracket (openFreshDatabase api) (closeDatabase api) $ \database ->
                alloca $ \connectionPtr -> do
                    status <- apiConnect api database connectionPtr
                    if status /= 0
                        then pure (Left "DuckDB connection failed")
                        else do
                            connection <- peek connectionPtr
                            bracket (pure connection) (\_ -> apiDisconnect api connectionPtr) $ \_ ->
                                alloca $ \extractedPtr -> do
                                    count <- BSC.useAsCString sql $ \sqlPtr ->
                                        apiExtractStatements api connection sqlPtr extractedPtr
                                    extracted <- peek extractedPtr
                                    bracket (pure extracted) (\_ -> apiDestroyExtracted api extractedPtr) $ \_ -> do
                                        errorPtr <- apiExtractStatementsError api extracted
                                        if errorPtr == nullPtr
                                            then pure (Right (fromIntegral count))
                                            else Left <$> peekCString errorPtr

-- ---------------------------------------------------------------------------
-- Small shared helpers
-- ---------------------------------------------------------------------------

hullOf :: [SpannedToken] -> ByteSpan
hullOf [] = error "hullOf: empty token list"
hullOf (L firstSpan _ : rest) = Foldable.foldl' (\accumulator (L nextSpan _) -> mergeSpan accumulator nextSpan) firstSpan rest

spanWithin :: ByteSpan -> ByteSpan -> Bool
spanWithin child parent =
    spanStart child >= spanStart parent && spanEnd child <= spanEnd parent

sliceOrEmpty :: ByteString -> ByteSpan -> ByteString
sliceOrEmpty sql span' = fromMaybe BS.empty (sliceSpan span' sql)
