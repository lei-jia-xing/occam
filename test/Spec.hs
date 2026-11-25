module Main (main) where

import BenchmarkMetricsChecks qualified
import CacheChecks qualified
import CliChecks qualified
import ColumnChecks qualified
import DependencyChecks qualified
import DialectAndControlChecks qualified
import DuckDBOracleChecks qualified
import EvaluationChecks qualified
import Fuzz (generativeFuzzTests)
import MutationalFuzz (mutationalFuzzTests)
import OracleChecks qualified
import PoolChecks qualified
import PoolMetricsChecks qualified
import PropertyChecks qualified
import ReductionChecks qualified
import SqliteOracleChecks qualified
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Occam Test Suite"
        [ DialectAndControlChecks.phase3Tests
        , CliChecks.tests
        , CacheChecks.tests
        , DialectAndControlChecks.phase7Tests
        , DialectAndControlChecks.phase8Tests
        , DependencyChecks.tests
        , EvaluationChecks.tests
        , OracleChecks.tests
        , PoolChecks.tests
        , PoolMetricsChecks.tests
        , ReductionChecks.tests
        , ColumnChecks.tests
        , SqliteOracleChecks.tests
        , DuckDBOracleChecks.tests
        , BenchmarkMetricsChecks.tests
        , PropertyChecks.tests
        , generativeFuzzTests
        , mutationalFuzzTests
        ]
