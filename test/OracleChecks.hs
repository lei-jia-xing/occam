{-# LANGUAGE CPP #-}

module OracleChecks (tests, checkInPlacePermissions) where

import Data.ByteString qualified as BS
import Occam.Oracle
import System.Exit (ExitCode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import TestSupport (withSentinelPath)
#ifndef mingw32_HOST_OS
import Control.Concurrent (threadDelay)
import Data.Bits ((.&.))
import Data.Maybe (isJust)
import System.Directory (doesFileExist, findExecutable)
import System.Posix.Files (accessModes, fileMode, getFileStatus, setFileMode)
import System.Process (CmdSpec (..))
import System.Timeout (timeout)
#endif

tests :: TestTree
tests =
    testGroup
        "Oracle checks"
        $ [ testCase "only an explicit zero predicate exit accepts SQL" $ do
                let cfg command = shellOracleConfig command 2000000 Nothing
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
                let cfg = shellOracleConfig "grep -q 'SELECT 42;'" 2000000 Nothing
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
                let cfg = shellOracleConfig "sleep 2" 100000 Nothing
                timed <- observeExternalOracle cfg BS.empty
                assertEqual "expected oracle timeout" OracleTimedOut timed
                reset <- observeExternalOracle (cfg {oracleResetCommand = Just "false"}) BS.empty
                assertEqual "failed reset must prevent oracle execution" OracleResetFailed reset
                missing <-
                    observeExternalOracle (rawOracleConfig "/no/such/occam-oracle" [] 2000000 Nothing) BS.empty
                assertBool "missing executable was not an infrastructure failure" (isInfrastructure missing)
          , testCase "parseDirectCommand only accepts metacharacter-free non-builtin commands" $ do
                assertEqual "simple binary" (Just ("/usr/bin/env", [])) (parseDirectCommand "/usr/bin/env")
                assertEqual
                    "binary with flags and equals in arg"
                    (Just ("python3", ["check.py", "--mode=fast"]))
                    (parseDirectCommand "  python3   check.py --mode=fast  ")
                assertEqual "empty command" Nothing (parseDirectCommand "   ")
                assertEqual "pipe requires shell" Nothing (parseDirectCommand "cat | grep x")
                assertEqual "redirection requires shell" Nothing (parseDirectCommand "cat >/dev/null")
                assertEqual "quotes require shell" Nothing (parseDirectCommand "grep -q 'SELECT 42;'")
                assertEqual "semicolon requires shell" Nothing (parseDirectCommand "true; exit 0")
                assertEqual "leading env assignment requires shell" Nothing (parseDirectCommand "FOO=1 ./oracle")
                assertEqual "shell builtin requires shell" Nothing (parseDirectCommand "exit 1")
                assertEqual "test builtin requires shell" Nothing (parseDirectCommand "test -f a")
          ]
            ++ posixTests

posixTests :: [TestTree]
#ifdef mingw32_HOST_OS
posixTests = []
#else
posixTests =
    [ testCase "resolveOracleContext upgrades direct ELF executables to RawCommand" $ do
            mEnv <- findExecutable "env"
            case mEnv of
                Nothing -> pure ()
                Just _ -> do
                    ctx <- resolveOracleContext (shellOracleConfig "env true" 2000000 Nothing)
                    case contextCmdSpec ctx of
                        RawCommand _ ["true"] -> pure ()
                        other -> assertFailure ("expected direct RawCommand for ELF binary, got: " ++ show other)
                    ctxShell <- resolveOracleContext (shellOracleConfig "env true && true" 2000000 Nothing)
                    case contextCmdSpec ctxShell of
                        ShellCommand _ -> pure ()
                        other -> assertFailure ("expected ShellCommand for shell syntax, got: " ++ show other)
        , testCase "predicates may close stdin early without changing their exit verdict" $
            mapM_
                ( \(exitStatus, expected) -> do
                    let command = "exec 0<&-; sleep 0.05; exit " ++ exitStatus
                        cfg = shellOracleConfig command 2000000 Nothing
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
                cfg = rawOracleConfig "python3" ["-c", command] 5000000 Nothing
                input = BS.concat (replicate 4096 (BS.pack [0 .. 255]))
            details <- observeExternalOracleDetailed cfg input
            assertEqual "output parsing, pipe deadlock, or input corruption changed the predicate" (OracleFinished ExitSuccess) (detailedObservation details)
            assertBool "oracle process start was not counted" (isJust (oracleElapsedMicros details))
        , testCase "relative commands retain caller paths and each candidate gets fresh state" $ do
            let reset = "test \"$PWD\" = \"$OCCAM_ORIGINAL_CWD\" && : > \"$OCCAM_WORKDIR/reset.ready\""
                command =
                    "test \"$PWD\" = \"$OCCAM_ORIGINAL_CWD\" && test -f \"$OCCAM_WORKDIR/reset.ready\""
                        ++ " && test ! -f \"$OCCAM_WORKDIR/seen\" && : > \"$OCCAM_WORKDIR/seen\""
                cfg = shellOracleConfig command 2000000 (Just reset)
            mapM_
                (\_ -> runExternalOracle cfg BS.empty >>= assertBool "relative commands or fresh candidate state failed")
                [1 :: Int, 2]
        , testCase "timeout cleanup kills ordinary process-group descendants" $
            withSentinelPath $ \sentinel -> do
                let command = "(sleep 1; printf leaked > " ++ shellQuote sentinel ++ ") & wait"
                    cfg = shellOracleConfig command 100000 Nothing
                details <- observeExternalOracleDetailed cfg BS.empty
                assertEqual "expected timeout" OracleTimedOut (detailedObservation details)
                assertBool "timed-out oracle duration was omitted" (isJust (oracleElapsedMicros details))
                threadDelay 1200000
                leaked <- doesFileExist sentinel
                assertBool "a timed-out descendant survived the process-group cleanup" (not leaked)
        , testCase "unread stdin with a full pipe and flooded output still times out cleanly" $
            withSentinelPath $ \sentinel -> do
                let input = BS.replicate (2 * 1024 * 1024) 65
                    command =
                        "(sleep 2; printf leaked > "
                            ++ shellQuote sentinel
                            ++ ") & yes flooding-output | head -c 20000000 >&2; wait"
                    cfg = shellOracleConfig command 500000 Nothing
                -- Verify process-group termination when stdin is unread and stderr floods.
                result <- timeout 20000000 (observeExternalOracleDetailed cfg input)
                case result of
                    Nothing -> assertFailure "oracle observation hung on unread stdin"
                    Just details ->
                        assertEqual "expected oracle timeout" OracleTimedOut (detailedObservation details)
                threadDelay 2500000
                leaked <- doesFileExist sentinel
                assertBool "a timed-out descendant survived the process-group cleanup" (not leaked)
        ]
#endif

isInfrastructure :: OracleObservation -> Bool
isInfrastructure (OracleIOFailed _) = True
isInfrastructure _ = False

#ifndef mingw32_HOST_OS
shellQuote :: String -> String
shellQuote value = "'" ++ concatMap quote value ++ "'"
  where
    quote '\'' = "'\\''"
    quote character = [character]
#endif

checkInPlacePermissions :: FilePath -> FilePath -> IO a -> IO a
#ifdef mingw32_HOST_OS
checkInPlacePermissions _ _ action = action
#else
checkInPlacePermissions input backup action = do
    setFileMode input 0o600
    setFileMode backup 0o644
    result <- action
    mapM_ checkMode [input, backup]
    pure result
    where
        checkMode path = do
            status <- getFileStatus path
            assertEqual ("permissions for " ++ path) 0o600 (fileMode status .&. accessModes)
#endif
