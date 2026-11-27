module FrontendPipelineChecks (tests) where

import Data.ByteString qualified as BS
import Data.Maybe (isJust, isNothing)
import Data.Text.Encoding (encodeUtf8)
import Occam.Core (
    Ident (..),
    Name (..),
    SearchPathEntry (..),
    SemanticFacts (..),
    SessionEffect (..),
    SessionInit (..),
    Span (..),
    Statement (..),
    StatementId (..),
 )
import Occam.Facts (
    ParserEdit (..),
    ParserEditCategory (..),
    StatementFacts (..),
    StatementUnit (..),
 )
import Occam.Frontend.AST qualified as AST
import Occam.Frontend.Context (
    IdentifierClass (..),
    canonicalIdentWith,
    foldFor,
    identifierRulesFor,
    isUserVariableIdent,
    isVariableIdent,
    literalIdentifierPart,
    literalRelationName,
    namePartsWith,
    splitIdentifierList,
    stripTempSchema,
    unquoteNamePart,
 )
import Occam.Frontend.Parser (ParsedScript (..), parseScript)
import Occam.Frontend.Pipeline (isRoutineDDL, parseFrontend, sessionInit)
import Occam.Frontend.Span (ByteSpan (..), Located (..))
import Occam.Parser (
    CaseFolding (..),
    ParseOptions (..),
    ParsedSQL (..),
    ParserError (..),
    ParserErrorKind (..),
    SQLDialect (..),
    ServerConfig (..),
    defaultParseOptions,
    defaultServerConfig,
    dialectProfile,
    spanOnlyOptions,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
    testGroup
        "Pure-Haskell frontend context and pipeline"
        [ identifierRulesTests
        , nameNormalizationTests
        , sessionInitTests
        , pipelineBehaviorTests
        ]

identifierRulesTests :: TestTree
identifierRulesTests =
    testGroup
        "IdentifierRules"
        [ testCase
            "default folding matches all six dialects including MySQL/MariaDB lower_case_table_names=0"
            do
                let pg = identifierRulesFor PostgreSQL defaultServerConfig
                    my0 = identifierRulesFor MySQL defaultServerConfig
                    my1 = identifierRulesFor MySQL defaultServerConfig {serverLowerCaseTableNames = 1}
                    ma0 = identifierRulesFor MariaDB defaultServerConfig
                    sq = identifierRulesFor SQLite defaultServerConfig
                    dk = identifierRulesFor DuckDB defaultServerConfig
                    ch = identifierRulesFor ClickHouse defaultServerConfig
                assertEqual "PG unquoted relation" FoldLower (foldFor pg IdentRelation False)
                assertEqual "PG quoted relation" FoldPreserve (foldFor pg IdentRelation True)
                assertEqual "MySQL lctn=0 unquoted relation" FoldPreserve (foldFor my0 IdentRelation False)
                assertEqual "MySQL lctn=0 unquoted column" FoldLower (foldFor my0 IdentColumn False)
                assertEqual "MySQL lctn=1 unquoted relation" FoldLower (foldFor my1 IdentRelation False)
                assertEqual "MariaDB lctn=0 unquoted relation" FoldPreserve (foldFor ma0 IdentRelation False)
                assertEqual "SQLite quoted" FoldAsciiLower (foldFor sq IdentRelation True)
                assertEqual "DuckDB unquoted" FoldAsciiLower (foldFor dk IdentColumn False)
                assertEqual "ClickHouse unquoted" FoldPreserve (foldFor ch IdentRelation False)
        , testCase "explicit ServerConfig overrides take precedence over dialect defaults" do
            let cfg =
                    defaultServerConfig
                        { serverTableNamesCaseSensitive = Just False
                        , serverColumnNamesCaseSensitive = Just True
                        , serverQuotedIdentifiersCaseSensitive = Just False
                        }
                rules = identifierRulesFor PostgreSQL cfg
            assertEqual "relation forced lower" FoldLower (foldFor rules IdentRelation True)
            assertEqual "column forced preserve" FoldPreserve (foldFor rules IdentColumn False)
            assertEqual "other quoted forced lower" FoldLower (foldFor rules IdentOther True)
        ]

nameNormalizationTests :: TestTree
nameNormalizationTests =
    testGroup
        "Name normalization and literal parsing"
        [ testCase "unquotes double, backtick, and bracket identifiers" do
            assertEqual "double" "Mixed\"Name" (unquoteNamePart (AST.NamePart "\"Mixed\"\"Name\"" True))
            assertEqual "backtick" "a`b" (unquoteNamePart (AST.NamePart "`a``b`" True))
            assertEqual "bracket" "a]b" (unquoteNamePart (AST.NamePart "[a]b]" True))
            assertEqual "unquoted" "Plain" (unquoteNamePart (AST.NamePart "Plain" False))
        , testCase "folds name parts with IdentRelation on prefixes and target class on leaf" do
            let rules = identifierRulesFor MySQL defaultServerConfig
                astName =
                    AST.Name
                        [ L (ByteSpan 0 3) (AST.NamePart "Tbl" False)
                        , L (ByteSpan 4 7) (AST.NamePart "Col" False)
                        ]
            assertEqual
                "MySQL lctn=0 keeps table case and lowers column"
                (Just [Ident "Tbl", Ident "col"])
                (namePartsWith rules IdentColumn astName)
            assertEqual
                "rejects >4 parts"
                Nothing
                ( namePartsWith
                    rules
                    IdentRelation
                    (AST.Name (replicate 5 (L (ByteSpan 0 1) (AST.NamePart "a" False))))
                )
        , testCase "strips dialect temporary schema prefixes" do
            assertEqual
                "PG pg_temp_3"
                ([Ident "t"], True)
                (stripTempSchema PostgreSQL [Ident "pg_temp_3", Ident "t"])
            assertEqual
                "SQLite temp"
                ([Ident "t"], True)
                (stripTempSchema SQLite [Ident "temp", Ident "t"])
            assertEqual
                "MySQL temp is regular schema"
                ([Ident "temp", Ident "t"], False)
                (stripTempSchema MySQL [Ident "temp", Ident "t"])
        , testCase "parses dotted literal relation names and variable identifiers" do
            let pgRules = identifierRulesFor PostgreSQL defaultServerConfig
            assertEqual
                "splitIdentifierList ignores dots in quotes"
                (Just ["tenant", "\"a.b\"", "\"Seq\""])
                (splitIdentifierList '.' "tenant.\"a.b\".\"Seq\"")
            assertEqual
                "literalIdentifierPart unescapes doubled quotes"
                (Just ("Mixed\"User", True))
                (literalIdentifierPart "\"Mixed\"\"User\"")
            assertEqual
                "literalRelationName folds unquoted and preserves quoted"
                (Just (Name [Ident "tenant", Ident "MixedCase", Ident "seq"]))
                (literalRelationName pgRules "Tenant.\"MixedCase\".\"seq\"")
            assertBool "user var" (isUserVariableIdent (AST.NamePart "@x" False))
            assertBool "sys var not user var" (not (isUserVariableIdent (AST.NamePart "@@x" False)))
            assertBool "sys var is variable" (isVariableIdent (AST.NamePart "@@x" False))
            assertEqual
                "canonicalIdentWith"
                (Ident "Mixed")
                (canonicalIdentWith pgRules IdentOther (AST.NamePart "\"Mixed\"" True))
        ]

sessionInitTests :: TestTree
sessionInitTests =
    testCase "sessionInit folds identifiers and applies dialect FK defaults" do
        let pgCfg =
                defaultServerConfig
                    { serverNamespacePrefix = ["\"Tenant\"", "App"]
                    , serverSearchPath = ["$user", "\"$user\"", "\"Mixed Case\"", "Public"]
                    , serverCurrentUser = Just "\"MixedUser\""
                    , serverAutocommit = False
                    }
            pgInit = sessionInit (defaultParseOptions PostgreSQL) {parseServerConfig = pgCfg}
            sqInit = sessionInit (defaultParseOptions SQLite)
        assertEqual "namespace_prefix" [Ident "Tenant", Ident "app"] (sessionNamespacePrefix pgInit)
        assertEqual
            "search_path"
            [ SearchPathCurrentUser
            , SearchPathCurrentUser
            , SearchPathName (Name [Ident "Mixed Case"])
            , SearchPathName (Name [Ident "public"])
            ]
            (sessionSearchPath pgInit)
        assertEqual "current_user" (Just (Ident "MixedUser")) (sessionCurrentUser pgInit)
        assertEqual "autocommit" False (sessionAutocommit pgInit)
        assertEqual "PG default FK checks" True (sessionForeignKeyChecks pgInit)
        assertEqual "SQLite default FK checks" False (sessionForeignKeyChecks sqInit)

pipelineBehaviorTests :: TestTree
pipelineBehaviorTests =
    testGroup
        "parseFrontend"
        [ testCase "rejects invalid UTF-8 with ParserInvalidUtf8" do
            case parseFrontend (defaultParseOptions SQLite) (BS.pack [0xff, 0xfe]) of
                Left (ParserError ParserInvalidUtf8 _) -> pure ()
                other -> assertFailure ("expected ParserInvalidUtf8, got " <> show other)
        , testCase "isolates middle parse errors and lexical-error suffixes with exact spans" do
            let sql = "SELECT 1; SELECT FROM; SELECT 'unterminated"
            parsed <- expectRight (parseFrontend (defaultParseOptions SQLite) sql)
            let units = parsedUnits parsed
            case units of
                [u1, u2, u3] -> do
                    assertEqual
                        "unit ids"
                        [StatementId 1, StatementId 2, StatementId 3]
                        (map (stmtId . unitStatement) units)
                    assertEqual "unit 1 raw" "SELECT 1" (stmtRaw (unitStatement u1))
                    assertEqual "unit 2 raw" "SELECT FROM" (stmtRaw (unitStatement u2))
                    assertEqual "unit 3 raw" "SELECT 'unterminated" (stmtRaw (unitStatement u3))
                    assertBool "unit 1 parsed" (isNothing (unitParseError u1) && isJust (unitFacts u1))
                    assertBool "unit 2 parse error" (isJust (unitParseError u2) && isNothing (unitFacts u2))
                    assertBool "unit 3 lex error" (isJust (unitParseError u3) && isNothing (unitFacts u3))
                    -- Lexical error anywhere disables cannotProduceResult script-wide.
                    assertEqual
                        "cannotProduceResult disabled on lex error"
                        [False, False, False]
                        (map unitCannotProduceResult units)
                _ -> assertFailure ("expected 3 units, got " <> show (length units))
        , testCase "respects spanOnly, syntaxOnly, and syntaxEdits flags" do
            let sql = encodeUtf8 "PRAGMA foreign_keys = ON; BEGIN; SELECT 1;"
            spanParsed <- expectRight (parseFrontend (spanOnlyOptions SQLite) sql)
            assertEqual
                "spanOnly omits facts"
                [Nothing, Nothing, Nothing]
                (map unitFacts (parsedUnits spanParsed))
            assertEqual
                "spanOnly still computes cannotProduceResult"
                [False, True, False]
                (map unitCannotProduceResult (parsedUnits spanParsed))

            let synOpts = (defaultParseOptions SQLite) {parseSyntaxOnly = True, parseSyntaxEdits = True}
            synParsed <- expectRight (parseFrontend synOpts sql)
            let synFacts = [factSemantics f | Just f <- map unitFacts (parsedUnits synParsed)]
            assertEqual
                "syntaxOnly skips semantic extraction"
                (replicate 3 (SemanticFacts [] [] [] [] []))
                synFacts

            -- When columnIR is enabled, syntaxOnly is ignored and semantics are populated.
            let colOpts = synOpts {parseColumnIR = True}
            colParsed <- expectRight (parseFrontend colOpts sql)
            let colSessions = [factSession (factSemantics f) | Just f <- map unitFacts (parsedUnits colParsed)]
                colPreTx = [factPreTx (factSemantics f) | Just f <- map unitFacts (parsedUnits colParsed)]
            assertEqual
                "columnIR overrides syntaxOnly for PRAGMA"
                [[SetSqliteForeignKeys True], [], []]
                colSessions
            assertBool "columnIR overrides syntaxOnly for BEGIN" (not (null (colPreTx !! 1)))
        , testCase "emits exact absolute spans for ClickHouse PREWHERE/SETTINGS and DuckDB QUALIFY" do
            let chSql = encodeUtf8 "SELECT '前置'; SELECT x FROM t PREWHERE /* c */ x > 0 SETTINGS max_threads = 1;"
                chOpts = (defaultParseOptions ClickHouse) {parseSyntaxEdits = True, parseSyntaxOnly = True}
            chParsed <- expectRight (parseFrontend chOpts chSql)
            let chEdits = concat [factEdits f | Just f <- map unitFacts (parsedUnits chParsed)]
                sliced =
                    [ ( parserEditLabel e
                      , BS.take (parserEditEnd e - parserEditStart e) (BS.drop (parserEditStart e) chSql)
                      )
                    | e <- chEdits
                    , parserEditCategory e == EditClause
                    ]
            assertEqual
                "ClickHouse clause edit spans"
                [ ("remove-prewhere", "PREWHERE /* c */ x > 0")
                , ("remove-settings", "SETTINGS max_threads = 1")
                ]
                sliced

            -- Without parseSyntaxEdits, no edits are emitted.
            chNoEdits <- expectRight (parseFrontend (defaultParseOptions ClickHouse) chSql)
            assertEqual
                "no edits when parseSyntaxEdits=False"
                []
                (concat [factEdits f | Just f <- map unitFacts (parsedUnits chNoEdits)])
        , testCase
            "classifies cannotProduceResult strictly including WITH+DML and isRoutineDDL for AGGREGATE"
            do
                let sql =
                        "CREATE TABLE t(x INT); CREATE TABLE c AS SELECT 1; INSERT INTO t VALUES (1); "
                            <> "INSERT INTO t VALUES (2) RETURNING x; WITH c AS (SELECT 1) INSERT INTO t VALUES (3); "
                            <> "UPDATE t SET x = 3; DELETE FROM t; "
                            <> "WITH c AS (SELECT 1) DELETE FROM t; BEGIN; COMMIT; SELECT 1;"
                parsed <- expectRight (parseFrontend (defaultParseOptions PostgreSQL) sql)
                assertEqual
                    "cannotProduceResult allowlist"
                    [True, False, True, False, False, True, True, False, True, True, False]
                    (map unitCannotProduceResult (parsedUnits parsed))
                case parsedUnits parsed of
                    firstUnit : _ -> assertEqual "first unit span" (Span 0 21) (stmtSpan (unitStatement firstUnit))
                    [] -> assertFailure "expected non-empty units"
                aggStmts <-
                    expectRight
                        ( parseScript
                            (dialectProfile PostgreSQL)
                            "CREATE AGGREGATE count(int) (SFUNC = int4pl, STYPE = int); DROP AGGREGATE count;"
                        )
                assertEqual
                    "isRoutineDDL for CREATE/DROP AGGREGATE"
                    [True, True]
                    (map (isRoutineDDL . locValue) (parsedScriptStatements aggStmts))
        ]

expectRight :: (Show e) => Either e a -> IO a
expectRight = either (assertFailure . show) pure
