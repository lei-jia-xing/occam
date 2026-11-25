module DependencyChecks (tests) where

import DependencyChecks.CascadeAndSessionChecks qualified as CascadeAndSessionChecks
import DependencyChecks.CoreChecks qualified as CoreChecks
import DependencyChecks.NativeSliceChecks (nativeSliceChecks)
import DependencyChecks.PartitionAndDweChecks qualified as PartitionAndDweChecks
import DependencyChecks.ViewAndBarrierChecks qualified as ViewAndBarrierChecks
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
    testGroup
        "Dependency analysis"
        ( CoreChecks.checks
            ++ CascadeAndSessionChecks.checks
            ++ ViewAndBarrierChecks.checks
            ++ PartitionAndDweChecks.checks
            ++ [nativeSliceChecks]
        )
