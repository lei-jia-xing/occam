-- | End-to-end and API checks for native collect, N-replay, and batch orchestration.
module BatchRunnerChecks (
    tests,
) where

import BatchRunnerChecks.Collect (testCollectSources, testCollectVerify)
import BatchRunnerChecks.Embedded (
    testBatchLocalEmbeddedRealOccam,
    testBatchLocalEmbeddedSigintResume,
 )
import BatchRunnerChecks.External (testBatchExternalContainers)
import BatchRunnerChecks.Managed (testBatchManagedRecreatePort, testBatchManagedWorkersAndResume)
import BatchRunnerChecks.Plan (testBatchDryRunAndValidation)
import BatchRunnerChecks.Replay (testReplayApiAndCancel)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
    testGroup
        "Harness Collect, Replay & Batch Runner"
        [ testCase
            "collect imports squirrel & sqlancer artifacts and handles cur-log flags"
            testCollectSources
        , testCase
            "collect --verify filters unstable inputs, dedups signatures, overwrites logs, and validates timeout"
            testCollectVerify
        , testCase
            "Replay API verifies consistency, slippage drift, missing baseline, and live IORef cancellation"
            testReplayApiAndCancel
        , testCase
            "batch --dry-run plans real corpus (71 -> 1278) and validates unsafe occam-jobs / flags"
            testBatchDryRunAndValidation
        , testCase
            "batch --containers drives external workers without docker run or rm"
            testBatchExternalContainers
        , testCase
            "batch managed workers qualify, catch slippage, clean up containers, and resume with zero docker calls"
            testBatchManagedWorkersAndResume
        , testCase
            "batch managed container recreate updates mapped port after reset failure (15432 -> 15433)"
            testBatchManagedRecreatePort
        , testCase
            "batch --local-embedded with real occam covers all 6 profiles, budgets, aggregate-only, and hash resume"
            testBatchLocalEmbeddedRealOccam
        , testCase
            "batch --local-embedded handles SIGINT (exit 130) and resumes cleanly to 2/2 completed"
            testBatchLocalEmbeddedSigintResume
        ]
