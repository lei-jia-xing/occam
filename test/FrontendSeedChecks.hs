-- | Six-dialect frontend conformance and seed-derived mutation checks.
module FrontendSeedChecks (tests) where

import ExtraEngineOracles (ExtraOracles (..))
import FrontendSeedChecks.Corpus (
    chCrossRejectTest,
    chInvalidTest,
    chPositiveTest,
    duckdbInvalidTest,
    duckdbTest,
    invalidTest,
    mysqlFamilyCrossRejectTest,
    mysqlFamilyInvalidTest,
    mysqlFamilyPositiveTest,
    pgAvailabilityTest,
    pgCrossRejectTest,
    pgInvalidTest,
    pgPositiveTest,
    positiveTest,
    sqliteInvalidTest,
    sqliteTest,
 )
import FrontendSeedChecks.Mutation (
    focusedWalkerTests,
    propLexicalCorruption,
    propMiddleCorruption,
    propTailTruncation,
 )
import FrontendSeedLoader (SeedCase (..), activeDelimiterAfter, loadSeedCorpus, seedCorpusPath)
import Occam.Parser (SQLDialect (..))
import PgQueryOracle (isPgQueryAvailable)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: ExtraOracles -> IO TestTree
tests extraOracles = do
    corpus <- loadSeedCorpus >>= either (ioError . userError . ("seed corpus: " <>)) pure
    pgAvail <- isPgQueryAvailable
    let chAvail = oracleClickHouse extraOracles
        mMysqlSrv = oracleMySQL extraOracles
        mMariaSrv = oracleMariaDB extraOracles
        positive = filter (not . isInvalidCase) corpus
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
        chPositive = filter (elem ClickHouse . seedCaseDialects) positive
        chInvalid = filter (elem ClickHouse . seedCaseDialects) invalid
        chCrossReject = filter (notElem ClickHouse . seedCaseDialects) positive
        mysqlPositive = filter (elem MySQL . seedCaseDialects) positive
        mysqlInvalid = filter (elem MySQL . seedCaseDialects) invalid
        mysqlCrossReject = filter (notElem MySQL . seedCaseDialects) positive
        mariaPositive = filter (elem MariaDB . seedCaseDialects) positive
        mariaInvalid = filter (elem MariaDB . seedCaseDialects) invalid
        mariaCrossReject = filter (notElem MariaDB . seedCaseDialects) positive
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
                "ClickHouse clickhouse-format alignment"
                ( if chAvail
                    then
                        [ testGroup "positive seeds" (map chPositiveTest chPositive)
                        , testGroup "invalid seeds" (map chInvalidTest chInvalid)
                        , testGroup "cross-dialect non-ClickHouse seeds" (map chCrossRejectTest chCrossReject)
                        ]
                    else []
                )
            , testGroup
                "MySQL 8.4 mysqld alignment"
                ( case mMysqlSrv of
                    Just srv ->
                        [ testGroup "positive seeds" (map (mysqlFamilyPositiveTest "MySQL 8.4" srv) mysqlPositive)
                        , testGroup "invalid seeds" (map (mysqlFamilyInvalidTest "MySQL 8.4" srv) mysqlInvalid)
                        , testGroup
                            "cross-dialect non-MySQL seeds"
                            (map (mysqlFamilyCrossRejectTest "MySQL 8.4" srv) mysqlCrossReject)
                        ]
                    Nothing -> []
                )
            , testGroup
                "MariaDB 11.4 mariadbd alignment"
                ( case mMariaSrv of
                    Just srv ->
                        [ testGroup "positive seeds" (map (mysqlFamilyPositiveTest "MariaDB 11.4" srv) mariaPositive)
                        , testGroup "invalid seeds" (map (mysqlFamilyInvalidTest "MariaDB 11.4" srv) mariaInvalid)
                        , testGroup
                            "cross-dialect non-MariaDB seeds"
                            (map (mysqlFamilyCrossRejectTest "MariaDB 11.4" srv) mariaCrossReject)
                        ]
                    Nothing -> []
                )
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
