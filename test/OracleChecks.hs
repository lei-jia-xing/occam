{-# LANGUAGE CPP #-}

module OracleChecks (tests) where

import Data.ByteString qualified as BS
import Occam.Oracle
import System.Exit (ExitCode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
#ifndef mingw32_HOST_OS
import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Monad (when)
import Data.Maybe (isJust)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
#endif

tests :: TestTree
tests =
    testGroup
        "Oracle checks"
        $ [ testCase "only an explicit zero predicate exit accepts SQL" $ do
                let cfg command = OracleConfig "sh" ["-c", command] 2000000 Nothing
                mapM_
                    ( \(command, expected) -> do
                        accepted <- runExternalOracle (cfg command) "SELECT 1;"
                        assertEqual ("Unexpected predicate result: " ++ command) expected accepted
                    )
                    [ ("cat >/dev/null; printf '{opaque output'; exit 0", True)
                    , ("exit 1", False)
                    , ("exit 139", False)
                    , ("exit 125", False)
                    , ("kill -TERM $$", False)
                    ]
          , testCase "initial SQL must satisfy the same predicate before reduction" $ do
                let cfg = OracleConfig "sh" ["-c", "grep -q 'SELECT 42;'"] 2000000 Nothing
                accepted <- prepareOracle cfg "SELECT 42;"
                assertBool
                    "accepted original input was refused"
                    ( case accepted of
                        Right details -> detailedObservation details == OracleFinished ExitSuccess
                        Left _ -> False
                    )
                rejected <- prepareOracle cfg "SELECT 1;"
                assertBool
                    "noninteresting original input was accepted"
                    (case rejected of Left _ -> True; Right _ -> False)
          , testCase "reset, timeout, and launch failures remain distinct observations" $ do
                let cfg = OracleConfig "sh" ["-c", "sleep 2"] 100000 Nothing
                timed <- observeExternalOracle cfg BS.empty
                assertEqual "expected oracle timeout" OracleTimedOut timed
                reset <- observeExternalOracle (cfg {oracleResetCommand = Just "false"}) BS.empty
                assertEqual "failed reset must prevent oracle execution" OracleResetFailed reset
                missing <- observeExternalOracle (cfg {oracleCommand = "/no/such/occam-oracle"}) BS.empty
                assertBool "missing executable was not an infrastructure failure" (isInfrastructure missing)
          ]
            ++ posixTests

posixTests :: [TestTree]
#ifdef mingw32_HOST_OS
posixTests = []
#else
posixTests =
    [ testCase "predicates may close stdin early without changing their exit verdict" $
            mapM_
                ( \(exitStatus, expected) -> do
                    let command = "exec 0<&-; sleep 0.05; exit " ++ exitStatus
                        cfg = OracleConfig "sh" ["-c", command] 2000000 Nothing
                        input = BS.replicate (4 * 1024 * 1024) 120
                    observation <- observeExternalOracle cfg input
                    assertEqual "early stdin closure must preserve the predicate exit" expected observation
                )
                [ ("0", OracleFinished ExitSuccess)
                , ("1", OracleFinished (ExitFailure 1))
                ]
        , testCase "large binary output is drained and bounded while stdin is written" $ do
            let command =
                    "import sys; sys.stdout.buffer.write(b'{'*3145728); sys.stdout.flush(); sys.stderr.buffer.write(b'x'*3145728); sys.stderr.flush(); data=sys.stdin.buffer.read(); sys.exit(0 if data == bytes(range(256))*4096 else 1)"
                cfg = OracleConfig "python3" ["-c", command] 5000000 Nothing
                input = BS.concat (replicate 4096 (BS.pack [0 .. 255]))
            details <- observeExternalOracleDetailed cfg input
            assertEqual "output parsing, pipe deadlock, or input corruption changed the predicate" (OracleFinished ExitSuccess) (detailedObservation details)
            assertBool "oracle process start was not counted" (isJust (oracleElapsedMicros details))
        , testCase "relative commands retain caller paths and each candidate gets fresh state" $ do
            let reset = "test \"$PWD\" = \"$OCCAM_ORIGINAL_CWD\" && : > \"$OCCAM_WORKDIR/reset.ready\""
                command =
                    "test \"$PWD\" = \"$OCCAM_ORIGINAL_CWD\" && test -f \"$OCCAM_WORKDIR/reset.ready\""
                        ++ " && test ! -f \"$OCCAM_WORKDIR/seen\" && : > \"$OCCAM_WORKDIR/seen\""
                cfg = OracleConfig "sh" ["-c", command] 2000000 (Just reset)
            mapM_
                (\_ -> runExternalOracle cfg BS.empty >>= assertBool "relative commands or fresh candidate state failed")
                [1 :: Int, 2]
        , testCase "timeout cleanup kills ordinary process-group descendants" $
            withSentinelPath $ \sentinel -> do
                let command = "(sleep 1; printf leaked > " ++ shellQuote sentinel ++ ") & wait"
                    cfg = OracleConfig "sh" ["-c", command] 100000 Nothing
                details <- observeExternalOracleDetailed cfg BS.empty
                assertEqual "expected timeout" OracleTimedOut (detailedObservation details)
                assertBool "timed-out oracle duration was omitted" (isJust (oracleElapsedMicros details))
                threadDelay 1200000
                leaked <- doesFileExist sentinel
                assertBool "a timed-out descendant survived the process-group cleanup" (not leaked)
        ]
#endif

isInfrastructure :: OracleObservation -> Bool
isInfrastructure (OracleIOFailed _) = True
isInfrastructure _ = False

#ifndef mingw32_HOST_OS
withSentinelPath :: (FilePath -> IO a) -> IO a
withSentinelPath action = do
    tempDir <- getTemporaryDirectory
    (path, handle) <- openTempFile tempDir "occam-oracle-child-"
    hClose handle
    removeFile path
    let cleanup = do
            exists <- doesFileExist path
            when exists (removeFile path)
    action path `finally` cleanup
#endif

#ifndef mingw32_HOST_OS
shellQuote :: String -> String
shellQuote value = "'" ++ concatMap quote value ++ "'"
  where
    quote '\'' = "'\\''"
    quote character = [character]
#endif
