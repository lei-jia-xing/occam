{- | Multi-engine in-process & local-server syntax alignment & differential syntax
fuzzing across all six dialects: PostgreSQL (libpg_query 18), DuckDB (libduckdb),
SQLite (direct-sqlite), ClickHouse (clickhouse format), MySQL 8.4 (mysqld), and
MariaDB 11.4 (mariadbd).
-}
module SyntaxAlignmentChecks (tests) where

import ExtraEngineOracles (ExtraOracles)
import PgQueryOracle (isPgQueryAvailable)
import SyntaxAlignmentChecks.Corpus (realFuzzCorpusAlignment, regressionCorpusAlignment)
import SyntaxAlignmentChecks.Differential (differentialFuzzTests)
import Test.Tasty (TestTree, testGroup)

tests :: ExtraOracles -> IO TestTree
tests extraOracles = do
    pgAvail <- isPgQueryAvailable
    pure $
        testGroup
            "Multi-engine syntax alignment"
            [ realFuzzCorpusAlignment pgAvail extraOracles
            , regressionCorpusAlignment pgAvail extraOracles
            , differentialFuzzTests pgAvail extraOracles
            ]
