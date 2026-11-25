module Occam.Oracle (
    OracleConfig (..),
    shellOracleConfig,
    rawOracleConfig,
    parseDirectCommand,
    OracleContext (..),
    resolveOracleContext,
    executeReset,
    runExternalOracle,
    OracleObservation (..),
    OracleObservationDetails (..),
    OracleStatus (..),
    observeExternalOracle,
    observeExternalOracleDetailed,
    observeExternalOracleDetailedWith,
    observeExternalOracleDetailedWithContext,
    oracleObservationMatches,
    classifyOracleObservation,
    observationOutcome,
    OracleReapFailed (..),
    prepareOracle,
    prepareOracleWithContext,
    withCandidateWorkdir,
) where

import Control.Exception (IOException)
import Data.Aeson (
    FromJSON (..),
    FromJSONKey (..),
    FromJSONKeyFunction (..),
    ToJSON (..),
    ToJSONKey (..),
    ToJSONKeyFunction (..),
    Value (String),
    withText,
 )
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.Key qualified as Key
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Core (OracleOutcome (..))
import Occam.Oracle.Process (
    OracleContext (..),
    OracleReapFailed (..),
    baseEnvironment,
    candidateEnvironmentWith,
    cmdSpecToProcess,
    elapsedMicros,
    executeReset,
    parseDirectCommand,
    resolveExecutable,
    resolveShellCommand,
    runProcessGroup,
    runProcessGroupWithStart,
    tryIOException,
    withCandidateWorkdir,
 )
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode (..))
import System.Process (CmdSpec (..), CreateProcess (env), shell)

data OracleObservationDetails = OracleObservationDetails
    { detailedObservation :: !OracleObservation
    , totalElapsedMicros :: !Int
    , resetElapsedMicros :: !Int
    , oracleElapsedMicros :: !(Maybe Int)
    }
    deriving (Eq, Show)

resolveOracleContext :: OracleConfig -> IO OracleContext
resolveOracleContext OracleConfig {..} = do
    spec <- case oracleSpec of
        RawCommand bin args -> do
            resolvedBin <- resolveExecutable bin
            pure (RawCommand resolvedBin args)
        ShellCommand cmd -> resolveShellCommand cmd
    resetSpec <- traverse resolveShellCommand oracleResetCommand
    base <- baseEnvironment
    originalDir <- getCurrentDirectory
    pure
        OracleContext
            { contextCmdSpec = spec
            , contextResetSpec = resetSpec
            , contextBaseEnvironment = base
            , contextOriginalDir = originalDir
            }

observeExternalOracle :: OracleConfig -> ByteString -> IO OracleObservation
observeExternalOracle cfg sql =
    detailedObservation <$> observeExternalOracleDetailed cfg sql

observeExternalOracleDetailed :: OracleConfig -> ByteString -> IO OracleObservationDetails
observeExternalOracleDetailed = observeExternalOracleDetailedWith (pure ())

-- | Count the oracle process only after it has actually been created.
observeExternalOracleDetailedWith ::
    IO () ->
    OracleConfig ->
    ByteString ->
    IO OracleObservationDetails
observeExternalOracleDetailedWith onOracleStarted cfg sql = do
    context <- resolveOracleContext cfg
    observeExternalOracleDetailedWithContext onOracleStarted cfg context sql

-- | One-shot observation against a pre-resolved 't:OracleContext'.
observeExternalOracleDetailedWithContext ::
    IO () ->
    OracleConfig ->
    OracleContext ->
    ByteString ->
    IO OracleObservationDetails
observeExternalOracleDetailedWithContext onOracleStarted cfg@OracleConfig {..} context sql
    | oracleTimeoutMicros <= 0 = do
        started <- getMonotonicTimeNSec
        finishOracleObservation started OracleTimedOut 0 Nothing
    | otherwise = do
        started <- getMonotonicTimeNSec
        let remainingMicros = do
                now <- getMonotonicTimeNSec
                pure (oracleTimeoutMicros - elapsedMicros started now)
        attempt <-
            tryIOException $
                withCandidateWorkdir (runOracleInWorkdir onOracleStarted cfg context sql remainingMicros)
        case attempt of
            Left err -> finishOracleObservation started (OracleIOFailed (T.pack (show err))) 0 Nothing
            Right (observation, resetElapsed, oracleElapsed) ->
                finishOracleObservation started observation resetElapsed oracleElapsed

finishOracleObservation ::
    Word64 -> OracleObservation -> Int -> Maybe Int -> IO OracleObservationDetails
finishOracleObservation started observation resetElapsed oracleElapsed = do
    ended <- getMonotonicTimeNSec
    pure
        OracleObservationDetails
            { detailedObservation = observation
            , totalElapsedMicros = elapsedMicros started ended
            , resetElapsedMicros = resetElapsed
            , oracleElapsedMicros = oracleElapsed
            }

runOracleInWorkdir ::
    IO () ->
    OracleConfig ->
    OracleContext ->
    ByteString ->
    IO Int ->
    FilePath ->
    IO (OracleObservation, Int, Maybe Int)
runOracleInWorkdir onOracleStarted OracleConfig {..} context sql remainingMicros workDir = do
    let childEnv = candidateEnvironmentWith context workDir
    (resetResult, resetElapsed) <-
        runOracleResetStep oracleResetCommand context childEnv remainingMicros
    case resetResult of
        Left err -> pure (OracleIOFailed (T.pack (show err)), resetElapsed, Nothing)
        Right Nothing -> pure (OracleTimedOut, resetElapsed, Nothing)
        Right (Just code)
            | code /= ExitSuccess -> pure (OracleResetFailed, resetElapsed, Nothing)
            | otherwise ->
                runOracleCommandStep onOracleStarted context childEnv sql remainingMicros resetElapsed

runOracleResetStep ::
    Maybe String ->
    OracleContext ->
    [(String, String)] ->
    IO Int ->
    IO (Either IOException (Maybe ExitCode), Int)
runOracleResetStep Nothing _ _ _ = pure (Right (Just ExitSuccess), 0)
runOracleResetStep (Just command) context childEnv remainingMicros = do
    resetStarted <- getMonotonicTimeNSec
    remaining <- remainingMicros
    result <-
        if remaining <= 0
            then pure (Right Nothing)
            else do
                let resetProc = maybe (shell command) cmdSpecToProcess (contextResetSpec context)
                tryIOException $ runProcessGroup remaining (resetProc {env = Just childEnv}) BS.empty
    resetEnded <- getMonotonicTimeNSec
    pure (result, elapsedMicros resetStarted resetEnded)

runOracleCommandStep ::
    IO () ->
    OracleContext ->
    [(String, String)] ->
    ByteString ->
    IO Int ->
    Int ->
    IO (OracleObservation, Int, Maybe Int)
runOracleCommandStep onOracleStarted context childEnv sql remainingMicros resetElapsed = do
    oracleStarted <- getMonotonicTimeNSec
    remaining <- remainingMicros
    if remaining <= 0
        then pure (OracleTimedOut, resetElapsed, Nothing)
        else do
            let cmdProcess = cmdSpecToProcess (contextCmdSpec context)
            oracleResult <-
                tryIOException $
                    runProcessGroupWithStart onOracleStarted remaining (cmdProcess {env = Just childEnv}) sql
            oracleEnded <- getMonotonicTimeNSec
            let oracleElapsed = elapsedMicros oracleStarted oracleEnded
                obs = case oracleResult of
                    Left err -> OracleIOFailed (T.pack (show err))
                    Right (_, Nothing) -> OracleTimedOut
                    Right (_, Just code') -> OracleFinished code'
                mOracleElapsed = case oracleResult of
                    Right (True, _) -> Just oracleElapsed
                    _ -> Nothing
            pure (obs, resetElapsed, mOracleElapsed)

oracleObservationMatches :: OracleObservation -> Bool
oracleObservationMatches (OracleFinished ExitSuccess) = True
oracleObservationMatches _ = False

data OracleStatus
    = Accepted
    | Rejected
    | TimedOut
    | InfrastructureError
    | Interrupted
    deriving (Eq, Ord, Show)

oracleStatusName :: OracleStatus -> Text
oracleStatusName Accepted = "accepted"
oracleStatusName Rejected = "rejected"
oracleStatusName TimedOut = "timed-out"
oracleStatusName InfrastructureError = "infrastructure-error"
oracleStatusName Interrupted = "interrupted"

parseOracleStatusName :: Text -> Maybe OracleStatus
parseOracleStatusName "accepted" = Just Accepted
parseOracleStatusName "rejected" = Just Rejected
parseOracleStatusName "timed-out" = Just TimedOut
parseOracleStatusName "infrastructure-error" = Just InfrastructureError
parseOracleStatusName "interrupted" = Just Interrupted
parseOracleStatusName _ = Nothing

instance ToJSON OracleStatus where
    toJSON :: OracleStatus -> Value
    toJSON = String . oracleStatusName

instance FromJSON OracleStatus where
    parseJSON = withText "OracleStatus" \name ->
        maybe (fail ("unknown oracle status: " <> T.unpack name)) pure (parseOracleStatusName name)

instance ToJSONKey OracleStatus where
    toJSONKey :: ToJSONKeyFunction OracleStatus
    toJSONKey =
        ToJSONKeyText
            (Key.fromText . oracleStatusName)
            (Encoding.text . oracleStatusName)

instance FromJSONKey OracleStatus where
    fromJSONKey :: FromJSONKeyFunction OracleStatus
    fromJSONKey = FromJSONKeyTextParser \name ->
        maybe (fail ("unknown oracle status: " <> T.unpack name)) pure (parseOracleStatusName name)

classifyOracleObservation :: OracleObservation -> OracleStatus
classifyOracleObservation observation = case observation of
    OracleTimedOut -> TimedOut
    OracleResetFailed -> InfrastructureError
    OracleIOFailed _ -> InfrastructureError
    OracleFinished ExitSuccess -> Accepted
    OracleFinished _ -> Rejected

-- | Definite process outcomes decide; timeouts and failures stay inconclusive.
observationOutcome :: OracleObservation -> OracleOutcome
observationOutcome observation = case classifyOracleObservation observation of
    Accepted -> OracleAccepted
    Rejected -> OracleRejected
    TimedOut -> OracleInconclusive
    InfrastructureError -> OracleInconclusive
    Interrupted -> OracleInconclusive

-- ----------------------------------------------------------------------------
-- External interestingness predicate
-- ----------------------------------------------------------------------------

data OracleConfig = OracleConfig
    { oracleSpec :: !CmdSpec
    , oracleTimeoutMicros :: !Int
    , oracleResetCommand :: !(Maybe String)
    }
    deriving (Eq, Show)

-- | Construct a shell-based oracle command (portable across POSIX and Windows).
shellOracleConfig :: String -> Int -> Maybe String -> OracleConfig
shellOracleConfig cmd timeoutMicros reset =
    OracleConfig
        { oracleSpec = ShellCommand cmd
        , oracleTimeoutMicros = timeoutMicros
        , oracleResetCommand = reset
        }

-- | Construct a raw binary-based oracle command.
rawOracleConfig :: FilePath -> [String] -> Int -> Maybe String -> OracleConfig
rawOracleConfig bin args timeoutMicros reset =
    OracleConfig
        { oracleSpec = RawCommand bin args
        , oracleTimeoutMicros = timeoutMicros
        , oracleResetCommand = reset
        }

-- | An observation retains execution failures instead of conflating them with rejection.
data OracleObservation
    = OracleFinished !ExitCode
    | OracleTimedOut
    | OracleResetFailed
    | OracleIOFailed !Text
    deriving (Eq, Show)

-- | Refuse inputs that do not satisfy the supplied predicate before reduction.
prepareOracle :: OracleConfig -> ByteString -> IO (Either Text OracleObservationDetails)
prepareOracle cfg sql = do
    context <- resolveOracleContext cfg
    prepareOracleWithContext cfg context sql

-- | Variant of 'prepareOracle' reusing a pre-resolved 'OracleContext'.
prepareOracleWithContext ::
    OracleConfig -> OracleContext -> ByteString -> IO (Either Text OracleObservationDetails)
prepareOracleWithContext cfg context sql
    | oracleTimeoutMicros cfg <= 0 = pure (Left "--timeout must be positive")
    | otherwise = do
        details <- observeExternalOracleDetailedWithContext (pure ()) cfg context sql
        pure $ case detailedObservation details of
            OracleFinished ExitSuccess -> Right details
            OracleFinished _ -> Left "Initial input does not satisfy the oracle predicate."
            _ -> Left ("Initial oracle failed: " <> T.pack (show (detailedObservation details)))

-- | Only an explicit zero exit status accepts a candidate.
runExternalOracle :: OracleConfig -> ByteString -> IO Bool
runExternalOracle cfg sql =
    oracleObservationMatches <$> observeExternalOracle cfg sql
