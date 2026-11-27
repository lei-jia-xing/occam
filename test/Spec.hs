module Main (main) where

import BatchReportChecks qualified
import BenchmarkMetricsChecks qualified
import CacheChecks qualified
import CliChecks qualified
import ColumnChecks qualified
import DependencyChecks qualified
import DialectAndControlChecks qualified
import DialectFuzz qualified
import DuckDBOracleChecks qualified
import EvaluationChecks qualified
import ExtraEngineOracles (withExtraOracles)
import FrontendChecks qualified
import FrontendColumnsChecks qualified
import FrontendCorpusChecks qualified
import FrontendEditsChecks qualified
import FrontendFacadeChecks qualified
import FrontendPipelineChecks qualified
import FrontendSeedChecks qualified
import FrontendSemanticsChecks qualified
import Fuzz (generativeFuzzTests)
import MutationalFuzz (mutationalFuzzTests)
import OracleChecks qualified
import ParserChecks qualified
import PoolChecks qualified
import PoolMetricsChecks qualified
import PropertyChecks qualified
import ReductionChecks qualified
import SemanticAlignmentChecks qualified
import SqliteOracleChecks qualified
import SyntaxAlignmentChecks qualified
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main =
    withExtraOracles \extraOracles -> do
        frontendSeedTests <- FrontendSeedChecks.tests extraOracles
        syntaxAlignmentTests <- SyntaxAlignmentChecks.tests extraOracles
        defaultMain
            (tests frontendSeedTests syntaxAlignmentTests (SemanticAlignmentChecks.tests extraOracles))

tests :: TestTree -> TestTree -> TestTree -> TestTree
tests frontendSeedTests syntaxAlignmentTests semanticAlignmentTests =
    testGroup
        "Occam Test Suite"
        [ DialectAndControlChecks.phase3Tests
        , FrontendChecks.tests
        , FrontendColumnsChecks.tests
        , FrontendCorpusChecks.tests
        , FrontendEditsChecks.tests
        , FrontendFacadeChecks.tests
        , FrontendPipelineChecks.tests
        , FrontendSemanticsChecks.tests
        , ParserChecks.tests
        , frontendSeedTests
        , syntaxAlignmentTests
        , semanticAlignmentTests
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
        , BatchReportChecks.tests
        , PropertyChecks.tests
        , generativeFuzzTests
        , mutationalFuzzTests
        , DialectFuzz.tests
        ]
