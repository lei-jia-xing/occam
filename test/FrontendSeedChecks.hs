-- | Six-dialect frontend conformance and seed-derived mutation checks.
module FrontendSeedChecks (tests) where

import Control.Exception (SomeException, bracket, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Database.SQLite3 qualified as Sqlite
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import FrontendAstInvariants (invariantViolations, validateStatementInvariants)
import FrontendSeedLoader (
    SeedCase (..),
    UnitExpectation (..),
    activeDelimiterAfter,
    expectedUnitSqlOnlyCount,
    loadSeedCorpus,
    seedCorpusPath,
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
    isValidSpan,
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
    LexicalProfile (..),
    SQLDialect (..),
    allDialectProfiles,
    countStatements,
    dialectProfile,
 )
import PgQueryOracle (PgSyntaxError (..), isPgQueryAvailable, pgQuerySplitCheck)
import System.Directory (doesDirectoryExist)
import System.Environment (lookupEnv)
import Test.QuickCheck (
    Gen,
    Property,
    choose,
    counterexample,
    elements,
    forAllBlind,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)

tests :: IO TestTree
tests = do
    corpus <- loadSeedCorpus >>= either (ioError . userError . ("seed corpus: " <>)) pure
    pgAvail <- isPgQueryAvailable
    let positive = filter (not . isInvalidCase) corpus
        invalid = filter isInvalidCase corpus
        multiUnit = filter ((>= 2) . length . seedCaseUnits) positive
        middle = filter (all (== ";") . activeDelimiterAfter) multiUnit
        pgPositive = filter (elem PostgreSQL . seedCaseDialects) positive
        pgInvalid = filter (elem PostgreSQL . seedCaseDialects) invalid
        pgCrossReject = filter (notElem PostgreSQL . seedCaseDialects) positive
        sqlitePositive = filter (elem SQLite . seedCaseDialects) positive
        sqliteInvalid = filter (elem SQLite . seedCaseDialects) invalid
        duckdbPositive = filter (elem DuckDB . seedCaseDialects) positive
        duckdbInvalid = filter (elem DuckDB . seedCaseDialects) invalid
    pure $
        testGroup
            "Pure-Haskell Frontend seed conformance"
            [ testGroup "positive six-dialect matrix" (map positiveTest positive)
            , testGroup "invalid reject matrix" (map invalidTest invalid)
            , testGroup
                "PostgreSQL libpg_query alignment"
                ( pgAvailabilityTest pgAvail
                    : if pgAvail
                        then
                            [ testGroup "positive seeds" (map pgPositiveTest pgPositive)
                            , testGroup "invalid seeds" (map pgInvalidTest pgInvalid)
                            , testGroup "cross-dialect non-PG seeds" (map pgCrossRejectTest pgCrossReject)
                            ]
                        else []
                )
            , testGroup
                "in-process SQLite alignment"
                [ testGroup "positive execution" (map sqliteTest sqlitePositive)
                , testGroup "invalid syntax reject" (map sqliteInvalidTest sqliteInvalid)
                ]
            , testGroup
                "DuckDB syntax extraction alignment"
                [ testGroup "positive seeds" (map duckdbTest duckdbPositive)
                , testGroup "invalid seeds" (map duckdbInvalidTest duckdbInvalid)
                ]
            , testGroup
                "seed-derived QuickCheck mutation"
                [ testProperty "tail truncation keeps the independently closed prefix" (propTailTruncation multiUnit)
                , testProperty "boundary-preserving corruption keeps the intact suffix" (propMiddleCorruption middle)
                , testProperty
                    "quoted/bracket/comment/paren corruption attaches lex errors locally"
                    (propLexicalCorruption multiUnit)
                ]
            , focusedWalkerTests
            ]

isInvalidCase :: SeedCase -> Bool
isInvalidCase seedCase = seedCaseFile seedCase == seedCorpusPath "invalid.sql"

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
-- Seed-derived mutation
-- ---------------------------------------------------------------------------

data Mutation = Mutation
    { mutationName :: !String
    , mutationBytes :: !ByteString
    , mutationLexical :: !Bool
    }

data Lexed = Lexed
    { lexedTokens :: ![SpannedToken]
    , lexedError :: !(Maybe LexError)
    }

lexed :: DialectProfile -> ByteString -> Lexed
lexed profile sql =
    let LexResult tokens lexError' = lexSQL profile sql
     in Lexed (V.toList tokens) lexError'

tokenLayoutIssues :: String -> ByteString -> Lexed -> [String]
tokenLayoutIssues label sql lexedInput =
    [ label <> ": token span invalid " <> show (locSpan token)
    | token <- tokens
    , not (isNonEmptyValidSpan sql (locSpan token))
    ]
        ++ [ label <> ": tokens not contiguous at " <> show (locSpan right)
           | (left, right) <- zip tokens (drop 1 tokens)
           , spanEnd (locSpan left) /= spanStart (locSpan right)
           ]
        ++ [ label <> ": first token does not start at zero"
           | (firstToken : _) <- [tokens]
           , spanStart (locSpan firstToken) /= 0
           ]
        ++ case lexedError lexedInput of
            Nothing ->
                [ label <> ": token bytes do not cover the input"
                | BS.concat [unsafeSliceSpan (locSpan token) sql | token <- tokens] /= sql
                ]
                    ++ [ label <> ": last token does not end at EOF"
                       | (lastToken : _) <- [reverse tokens]
                       , spanEnd (locSpan lastToken) /= BS.length sql
                       ]
            Just err ->
                [ label <> ": lex error offset outside input"
                | lexErrorOffset err < 0 || lexErrorOffset err > BS.length sql
                ]
                    ++ [ label <> ": lex error offset is not a UTF-8 boundary"
                       | not (isUtf8Boundary sql (lexErrorOffset err))
                       ]
    where
        tokens = lexedTokens lexedInput

splitterLayoutIssues :: ByteString -> [SplitUnit] -> [String]
splitterLayoutIssues sql units =
    concatMap unitIssues units
        ++ [ "split units overlap at " <> show (splitUnitSpan right)
           | (left, right) <- zip units (drop 1 units)
           , spanStart (splitUnitSpan right) < spanEnd (splitUnitSpan left)
           ]
    where
        unitIssues unit =
            let unitSpan = splitUnitSpan unit
                hasLexError = isJust (splitUnitLexError unit)
             in [ "split unit span invalid: " <> show unitSpan
                | not (isValidSpan sql unitSpan)
                ]
                    ++ [ "empty split unit span: " <> show unitSpan
                       | not hasLexError && spanIsEmpty unitSpan
                       ]
                    ++ case splitUnitDelimiterSpan unit of
                        Nothing -> directiveIssues unitSpan (splitUnitDelimiterDirective unit)
                        Just delimiterSpan ->
                            [ "invalid statement delimiter span: " <> show delimiterSpan
                            | not (isNonEmptyValidSpan sql delimiterSpan)
                            ]
                                ++ [ "statement delimiter overlaps its unit: " <> show (unitSpan, delimiterSpan)
                                   | spanEnd unitSpan > spanStart delimiterSpan
                                   ]
        directiveIssues unitSpan = \case
            Nothing -> []
            Just (L directiveSpan _) ->
                [ "invalid directive span: " <> show directiveSpan
                | not (isNonEmptyValidSpan sql directiveSpan)
                ]
                    ++ [ "directive span is outside its unit: " <> show (directiveSpan, unitSpan)
                       | not (spanWithin directiveSpan unitSpan)
                       ]

prefixIssues ::
    DialectProfile ->
    ByteString ->
    String ->
    ByteString ->
    Int ->
    [SplitUnit] ->
    [String]
prefixIssues profile actualSql label prefixAnchor expectedCount actualUnits =
    case splitSQL profile prefixAnchor of
        Left err -> [label <> ": independently authored prefix failed to split: " <> show err]
        Right reference ->
            [ label
                <> ": prefix unit count short (actual "
                <> show (length actualUnits)
                <> ", expected "
                <> show expectedCount
                <> ")"
            | length actualUnits < expectedCount
            ]
                ++ [ label
                        <> ": reference prefix unit count differs (reference "
                        <> show (length reference)
                        <> ", expected "
                        <> show expectedCount
                        <> ")"
                   | length reference /= expectedCount
                   ]
                ++ [ label
                        <> ": prefix unit "
                        <> show index
                        <> " differs: actual "
                        <> show actualBytes
                        <> " reference "
                        <> show referenceBytes
                   | (index, (actual, referenceUnit)) <- indexedPairs (take expectedCount actualUnits) reference
                   , let actualBytes = sliceOrEmpty actualSql (splitUnitSpan actual)
                   , let referenceBytes = sliceOrEmpty prefixAnchor (splitUnitSpan referenceUnit)
                   , actualBytes /= referenceBytes || splitUnitSpan actual /= splitUnitSpan referenceUnit
                   ]

suffixIssues ::
    DialectProfile ->
    ByteString ->
    String ->
    ByteString ->
    Int ->
    Int ->
    [SplitUnit] ->
    [String]
suffixIssues profile actualSql label suffixAnchor suffixOffset expectedCount actualUnits =
    case splitSQL profile suffixAnchor of
        Left err -> [label <> ": independently authored suffix failed to split: " <> show err]
        Right reference ->
            [ label
                <> ": suffix unit count short (actual "
                <> show (length actualSuffix)
                <> ", expected "
                <> show expectedCount
                <> ")"
            | length actualSuffix < expectedCount
            ]
                ++ [ label
                        <> ": reference suffix unit count differs (reference "
                        <> show (length reference)
                        <> ", expected "
                        <> show expectedCount
                        <> ")"
                   | length reference /= expectedCount
                   ]
                ++ [ label
                        <> ": suffix unit "
                        <> show index
                        <> " differs: actual "
                        <> show actualBytes
                        <> " reference "
                        <> show referenceBytes
                   | (index, (actual, referenceUnit)) <- indexedPairs actualSuffix reference
                   , let actualBytes = sliceOrEmpty actualSql (splitUnitSpan actual)
                   , let referenceBytes = sliceOrEmpty suffixAnchor (splitUnitSpan referenceUnit)
                   , actualBytes /= referenceBytes
                   ]
                ++ [ label
                        <> ": suffix unit "
                        <> show index
                        <> " span differs: actual "
                        <> show (splitUnitSpan actual)
                        <> " expected "
                        <> show (shiftSpan suffixOffset (splitUnitSpan referenceUnit))
                   | (index, (actual, referenceUnit)) <- indexedPairs actualSuffix reference
                   , splitUnitSpan actual /= shiftSpan suffixOffset (splitUnitSpan referenceUnit)
                   ]
    where
        actualSuffix = drop (max 0 (length actualUnits - expectedCount)) actualUnits

indexedPairs :: [a] -> [b] -> [(Int, (a, b))]
indexedPairs left right = zip [0 :: Int ..] (zip left right)

shiftSpan :: Int -> ByteSpan -> ByteSpan
shiftSpan offset (ByteSpan start end) = ByteSpan (start + offset) (end + offset)

localLexIssues :: String -> ByteString -> Lexed -> [SplitUnit] -> [String]
localLexIssues label sql lexedInput units =
    case lexedError lexedInput of
        Nothing -> []
        Just err -> case reverse units of
            [] -> [label <> ": lexer reported an error but splitter produced no unit"]
            (lastUnit : _) ->
                [ label <> ": lex error was not attached to the last split unit"
                | splitUnitLexError lastUnit /= Just err
                ]
                    ++ [ label <> ": error unit does not reach EOF"
                       | spanEnd (splitUnitSpan lastUnit) /= BS.length sql
                       ]
                    ++ [ label <> ": lex error offset precedes its split unit"
                       | lexErrorOffset err < spanStart (splitUnitSpan lastUnit)
                       ]

expectNoLexIssues :: String -> Lexed -> [String]
expectNoLexIssues label lexedInput =
    case lexedError lexedInput of
        Nothing -> []
        Just err -> [label <> ": unexpected lex error " <> show err]

nonLexicalIssues :: String -> ByteString -> Lexed -> [SplitUnit] -> [String]
nonLexicalIssues label sql lexedInput units =
    case lexedError lexedInput of
        Just err -> [label <> ": non-lexical mutation unexpectedly produced " <> show err]
        Nothing -> case reverse units of
            [] -> [label <> ": no split units"]
            (lastUnit : _) ->
                [ label <> ": non-lexical trailing unit does not reach last substantive token"
                | spanEnd (splitUnitSpan lastUnit) /= lastSubstantiveEnd
                ]
                    ++ [ label <> ": non-lexical trailing unit unexpectedly terminated with a delimiter"
                       | isJust (splitUnitDelimiterSpan lastUnit)
                       ]
                    ++ [ label <> ": non-lexical trailing unit carries a lex error"
                       | isJust (splitUnitLexError lastUnit)
                       ]
    where
        lastSubstantiveEnd =
            maybe
                (BS.length sql)
                (spanEnd . locSpan)
                (listToMaybe (reverse (filter (not . isTriviaToken . locValue) (lexedTokens lexedInput))))

walkerIssues :: String -> ByteString -> DialectProfile -> [String]
walkerIssues label sql profile =
    case parseScriptUnits profile sql of
        Left err -> [label <> ": parseScriptUnits rejected valid UTF-8: " <> show err]
        Right parsedUnits ->
            [ label <> ": AST invariant: " <> violation
            | (_, Right statement) <- parsedUnits
            , violation <- invariantViolations sql statement
            ]

elementsNonEmpty :: String -> [a] -> Gen a
elementsNonEmpty label values = case values of
    [] -> error ("empty QuickCheck domain: " <> label)
    _ -> elements values

genCaseProfile :: String -> [SeedCase] -> Gen (SeedCase, DialectProfile)
genCaseProfile label cases = do
    seedCase <- elementsNonEmpty label cases
    profile <-
        elementsNonEmpty
            (label <> " dialects for " <> seedLabel seedCase)
            (map dialectProfile (seedCaseDialects seedCase))
    pure (seedCase, profile)

genTruncation :: [SeedCase] -> Gen (SeedCase, DialectProfile, Int, Int)
genTruncation cases = do
    (seedCase, profile) <- genCaseProfile "truncation" cases
    let sql = seedCaseSql seedCase
        segments = seedCaseSegments seedCase
        segmentSpans = seedCaseSegmentSpans seedCase
        boundaries segmentSpan first =
            [ offset
            | offset <- [spanStart segmentSpan + spanStart (locSpan first) + 1 .. spanEnd segmentSpan]
            , offset < BS.length sql
            , isUtf8Boundary sql offset
            ]
        candidates =
            [ (index, cuts)
            | (index, (segment, segmentSpan)) <- zip [1 ..] (drop 1 (zip segments segmentSpans))
            , Just first <- [listToMaybe (nonTriviaTokens profile segment)]
            , let cuts = boundaries segmentSpan first
            , not (null cuts)
            ]
    (segmentIndex, cuts) <- elementsNonEmpty "truncation segments" candidates
    cut <- elementsNonEmpty "UTF-8 truncation boundaries" cuts
    pure (seedCase, profile, segmentIndex, cut)

genMiddle :: [SeedCase] -> Gen (SeedCase, DialectProfile, Int, Mutation)
genMiddle cases = do
    (seedCase, profile) <- genCaseProfile "middle corruption" cases
    boundary <- choose (1, length (seedCaseUnits seedCase) - 1)
    mutation <- elements cleanMutations
    pure (seedCase, profile, boundary, mutation)

genLexical :: [SeedCase] -> Gen (SeedCase, DialectProfile, Int, Mutation)
genLexical cases = do
    (seedCase, profile) <- genCaseProfile "lexical corruption" cases
    boundary <- choose (1, length (seedCaseUnits seedCase) - 1)
    mutation <- elements (lexicalMutations profile)
    pure (seedCase, profile, boundary, mutation)

cleanMutations :: [Mutation]
cleanMutations =
    [ Mutation "syntax-noise" "SYNTAX ??? ERROR;" False
    , Mutation "unbalanced-parens" "SELECT ))) ???;" False
    , Mutation "quoted-noise" "SELECT 'x' ???;" False
    ]

lexicalMutations :: DialectProfile -> [Mutation]
lexicalMutations profile =
    [ Mutation "unterminated-single-quote" "SELECT 'unterminated" True
    , Mutation "unterminated-double-quote" "SELECT \"unterminated" True
    , Mutation "unterminated-block-comment" "SELECT /* unterminated" True
    , Mutation "unclosed-paren" "SELECT ((( ???" False
    ]
        ++ [ Mutation "unterminated-backtick" "SELECT `unterminated" True
           | lexBacktickQuotes (dialectLexical profile)
           ]
        ++ [ Mutation "unterminated-bracket" "SELECT [unterminated" True
           | lexBracketQuotes (dialectLexical profile)
           ]
        ++ [ Mutation "unterminated-dollar-string" "SELECT $tag$unterminated" True
           | lexDollarQuoting (dialectLexical profile)
           ]

nonTriviaTokens :: DialectProfile -> ByteString -> [SpannedToken]
nonTriviaTokens profile bytes =
    [ L sp token
    | L sp token <- V.toList (lexTokens (lexSQL profile bytes))
    , not (isTriviaToken token)
    ]

mutationContext :: SeedCase -> DialectProfile -> String -> String
mutationContext seedCase profile detail = contextLabel seedCase profile <> " " <> detail

propTailTruncation :: [SeedCase] -> Property
propTailTruncation cases =
    forAllBlind (genTruncation cases) $ \(seedCase, profile, segmentIndex, cut) ->
        let sql = seedCaseSql seedCase
            truncated = BS.take cut sql
            prefixAnchor = seedCasePrefixes seedCase !! (segmentIndex - 1)
            context =
                mutationContext
                    seedCase
                    profile
                    ("class=tail-truncation segment=" <> show segmentIndex <> " cut=" <> show cut)
            lexedInput = lexed profile truncated
            issues = case splitSQL profile truncated of
                Left err -> [context <> ": splitSQL rejected a UTF-8-safe truncation: " <> show err]
                Right units ->
                    splitterLayoutIssues truncated units
                        ++ prefixIssues profile truncated (context <> " prefix") prefixAnchor segmentIndex units
                        ++ tokenLayoutIssues (context <> " lexer") truncated lexedInput
                        ++ localLexIssues (context <> " lexer") truncated lexedInput units
                        ++ walkerIssues (context <> " AST") truncated profile
         in counterexample (context <> "\n" <> unlines issues) (null issues)

propMiddleCorruption :: [SeedCase] -> Property
propMiddleCorruption cases =
    forAllBlind (genMiddle cases) $ \(seedCase, profile, boundary, mutation) ->
        let prefixAnchor = seedCasePrefixes seedCase !! (boundary - 1)
            suffixAnchor = seedCaseSuffixes seedCase !! (boundary - 1)
            changes = mutationBytes mutation
            corrupt = prefixAnchor <> changes <> "\n" <> suffixAnchor
            suffixOffset = BS.length prefixAnchor + BS.length changes + 1
            unitCount = length (seedCaseUnits seedCase)
            context =
                mutationContext
                    seedCase
                    profile
                    ("class=" <> mutationName mutation <> " boundary=" <> show boundary)
            lexedInput = lexed profile corrupt
            issues = case splitSQL profile corrupt of
                Left err -> [context <> ": splitSQL rejected boundary-preserving corruption: " <> show err]
                Right units ->
                    splitterLayoutIssues corrupt units
                        ++ [ context <> ": expected " <> show (unitCount + 1) <> " units, got " <> show (length units)
                           | length units /= unitCount + 1
                           ]
                        ++ prefixIssues profile corrupt (context <> " prefix") prefixAnchor boundary units
                        ++ suffixIssues
                            profile
                            corrupt
                            (context <> " suffix")
                            suffixAnchor
                            suffixOffset
                            (unitCount - boundary)
                            units
                        ++ expectNoLexIssues (context <> " lexer") lexedInput
                        ++ tokenLayoutIssues (context <> " lexer") corrupt lexedInput
                        ++ walkerIssues (context <> " AST") corrupt profile
         in counterexample (context <> "\n" <> unlines issues) (null issues)

propLexicalCorruption :: [SeedCase] -> Property
propLexicalCorruption cases =
    forAllBlind (genLexical cases) $ \(seedCase, profile, boundary, mutation) ->
        let prefixAnchor = seedCasePrefixes seedCase !! (boundary - 1)
            suffixAnchor = seedCaseSuffixes seedCase !! (boundary - 1)
            corrupt = prefixAnchor <> mutationBytes mutation <> "\n" <> suffixAnchor
            context =
                mutationContext
                    seedCase
                    profile
                    ("class=" <> mutationName mutation <> " boundary=" <> show boundary)
            lexedInput = lexed profile corrupt
            issues = case splitSQL profile corrupt of
                Left err -> [context <> ": splitSQL returned Left for valid UTF-8 corruption: " <> show err]
                Right units ->
                    splitterLayoutIssues corrupt units
                        ++ prefixIssues profile corrupt (context <> " prefix") prefixAnchor boundary units
                        ++ tokenLayoutIssues (context <> " lexer") corrupt lexedInput
                        ++ if mutationLexical mutation
                            then localLexIssues (context <> " lexer") corrupt lexedInput units
                            else
                                nonLexicalIssues (context <> " lexer") corrupt lexedInput units
                                    ++ walkerIssues (context <> " AST") corrupt profile
         in counterexample (context <> "\n" <> unlines issues) (null issues)

-- ---------------------------------------------------------------------------
-- Focused in-memory walker coverage
-- ---------------------------------------------------------------------------

focusedWalkerTests :: TestTree
focusedWalkerTests =
    testGroup
        "located COPY/EXPLAIN payloads"
        [ testCase "COPY bare option span includes its optional value" $ do
            let sql = "COPY seed_copy FROM stdin WITH NULL '\\N'"
            assertSingleStatementValid "COPY" (dialectProfile PostgreSQL) sql
        , testCase "EXPLAIN option name and quoted literal stay source-exact" $ do
            let sql = "EXPLAIN (FORMAT 'json') SELECT 1"
            assertSingleStatementValid "EXPLAIN" (dialectProfile PostgreSQL) sql
        ]

assertSingleStatementValid :: String -> DialectProfile -> ByteString -> Assertion
assertSingleStatementValid label profile sql =
    case parseScript profile sql of
        Left err -> assertFailure (label <> ": parse failed: " <> show err)
        Right (ParsedScript statements) -> do
            assertEqual (label <> ": statement count") 1 (length statements)
            for_ statements (validateStatementInvariants sql)

-- ---------------------------------------------------------------------------
-- Small shared helpers
-- ---------------------------------------------------------------------------

hullOf :: [SpannedToken] -> ByteSpan
hullOf [] = error "hullOf: empty token list"
hullOf (L firstSpan _ : rest) = foldl' (\accumulator (L nextSpan _) -> mergeSpan accumulator nextSpan) firstSpan rest

spanWithin :: ByteSpan -> ByteSpan -> Bool
spanWithin child parent =
    spanStart child >= spanStart parent && spanEnd child <= spanEnd parent

sliceOrEmpty :: ByteString -> ByteSpan -> ByteString
sliceOrEmpty sql span' = fromMaybe BS.empty (sliceSpan span' sql)
