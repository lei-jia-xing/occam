-- | Replay API and cancellation checks.
module BatchRunnerChecks.Replay (testReplayApiAndCancel) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (when)
import Data.IORef (newIORef, writeIORef)
import Data.Time (diffUTCTime, getCurrentTime)
import Occam.Harness.Config (HarnessConfig, loadHarnessConfig, resolveHarnessConfigPath)
import Occam.Harness.DbmsExec (ConnectionOverrides (..), emptyConnectionOverrides)
import Occam.Harness.Replay (
    ReplayConfig (..),
    ReplayVerification (..),
    defaultReplayConfig,
    runReplayVerification,
 )
import Occam.Harness.Signature (CrashSignature (..), DbmsKind (..))
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((</>))
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure)

import BatchRunnerChecks.Support

-- ---------------------------------------------------------------------------
-- 2. Replay API tests
-- ---------------------------------------------------------------------------

testReplayApiAndCancel :: IO ()
testReplayApiAndCancel = withTempDir "occam-replay-api" \td -> do
    cfg <- loadDefaultHarnessConfig
    let modeFile = td </> "mode.txt"
        callCount = td </> "calls.txt"
        engDrift = td </> "sqlite_drift"
        engSleep = td </> "sqlite_sleep"
    _ <-
        writeExec
            td
            "sqlite_drift"
            ( unlines
                [ "#!/bin/sh"
                , "mode=$(cat " <> shellSingleQuote modeFile <> ")"
                , "n=0"
                , "if [ -f "
                    <> shellSingleQuote callCount
                    <> " ]; then n=$(cat "
                    <> shellSingleQuote callCount
                    <> "); fi"
                , "n=$((n + 1))"
                , "printf '%s' \"$n\" > " <> shellSingleQuote callCount
                , "if [ \"$mode\" = \"consistent\" ] || [ \"$n\" -eq 1 ]; then"
                , "  printf '==1==ERROR: AddressSanitizer: SEGV on unknown address\\n    #0 0x100 in func_alpha /src/a.c:10\\n' >&2"
                , "else"
                , "  printf '==2==ERROR: AddressSanitizer: SEGV on unknown address\\n    #0 0x200 in func_beta /src/b.c:20\\n' >&2"
                , "fi"
                , "exit 134"
                ]
            )
    _ <-
        writeExec
            td
            "sqlite_sleep"
            "#!/bin/sh\nexec sleep 15\n"

    let mkCfg bin baseSig =
            (defaultReplayConfig DbmsSqlite (emptyConnectionOverrides {coEngineBin = Just bin}))
                { rcBaselineSignature = baseSig
                , rcRejectOnSlippage = False
                , rcProbeTimeout = 10.0
                , rcResetBeforeRun = False
                , rcRequireConsistent = True
                }

    -- Consistent, no baseline -> rvMatchesBaseline Nothing
    writeFile modeFile "consistent"
    removeIfExists callCount
    rvCons <- runReplayVerification cfg Nothing (mkCfg engDrift Nothing) 3 "SELECT 1;"
    assertEqual ("allPassed: " <> show rvCons) True (rvAllPassed rvCons)
    assertEqual ("consistentSignature: " <> show rvCons) True (rvConsistentSignature rvCons)
    assertEqual "no baseline -> matchesBaseline Nothing" Nothing (rvMatchesBaseline rvCons)
    baseSigId <- case rvBaselineSignature rvCons of
        Just s -> pure (csId s)
        Nothing -> assertFailure "expected baseline signature from consistent replay"

    -- Slippage / signature drift across 2 runs against baseSigId
    writeFile modeFile "drift"
    removeIfExists callCount
    rvDrift <- runReplayVerification cfg Nothing (mkCfg engDrift (Just baseSigId)) 2 "SELECT 1;"
    assertEqual ("drift still passes crashes: " <> show rvDrift) True (rvAllPassed rvDrift)
    assertEqual "drift consistentSignature False" False (rvConsistentSignature rvDrift)
    assertEqual "drift matchesBaseline Just False" (Just False) (rvMatchesBaseline rvDrift)
    assertEqual "drift slippage count 1" 1 (rvSlippageCount rvDrift)

    -- Pre-set IORef stop flag cancels immediately with zero runs
    stopRef1 <- newIORef True
    rvPreCancel <- runReplayVerification cfg (Just stopRef1) (mkCfg engSleep Nothing) 3 "SELECT 1;"
    assertEqual "pre-cancel interrupted" True (rvInterrupted rvPreCancel)
    assertEqual "pre-cancel zero runs" 0 (length (rvRuns rvPreCancel))

    -- Live cancellation during sleep 15 subprocess completes quickly (< 5s)
    stopRef2 <- newIORef False
    tStart <- getCurrentTime
    _ <- forkIO do
        threadDelay 200_000
        writeIORef stopRef2 True
    rvLiveCancel <- runReplayVerification cfg (Just stopRef2) (mkCfg engSleep Nothing) 2 "SELECT 1;"
    tEnd <- getCurrentTime
    let elapsed = realToFrac (diffUTCTime tEnd tStart) :: Double
    assertEqual "live cancel interrupted" True (rvInterrupted rvLiveCancel)
    assertBool ("live cancel took too long: " <> show elapsed) (elapsed < 5.0)
    where
        removeIfExists p = do
            ok <- doesFileExist p
            when ok (removeFile p)

loadDefaultHarnessConfig :: IO HarnessConfig
loadDefaultHarnessConfig = do
    cfgPath <- resolveHarnessConfigPath Nothing
    either assertFailure pure =<< loadHarnessConfig cfgPath
