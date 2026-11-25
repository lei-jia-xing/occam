module ReductionChecks (tests) where

import ReductionChecks.BatchAndDeferralChecks qualified as BatchAndDeferralChecks
import ReductionChecks.CertificateChecks qualified as CertificateChecks
import ReductionChecks.StatementSearchChecks qualified as StatementSearchChecks
import ReductionChecks.SyntaxAndColumnChecks qualified as SyntaxAndColumnChecks
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
    testGroup
        "Reduction checks"
        ( StatementSearchChecks.checks
            ++ SyntaxAndColumnChecks.checks
            ++ CertificateChecks.checks
            ++ BatchAndDeferralChecks.checks
        )
