-- | Executable entry point for the @occam@ SQL test-case reducer CLI.
module Main (main) where

import Completion (generateCompletion)
import Control.Monad (forM_)
import Data.Aeson (encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Occam.Logger (LogFormat (..))
import Occam.Parser (SQLDialect (..), dialectName)
import Occam.Reducer (columnModeName, statementSearchPolicyName)
import Options (
    Action (..),
    RawOptions (..),
    applyConfigPatch,
    colorModeName,
    defaultLogLevelForFormat,
    defaultRawOptions,
    getSpecified,
    logFormatName,
    parseConfigPatch,
    strategyName,
    validateCliOptions,
 )
import Options.Applicative (
    defaultPrefs,
    execParserPure,
    handleParseResult,
    overFailure,
 )
import Options.Applicative.Help (ParserHelp (helpUsage), isEmpty, stringChunk)
import Options.Parser (optsInfo)
import Runner (runOccam)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (tryIOError)

-- | Parse CLI arguments with compact usage formatting and dispatch the selected action.
main :: IO ()
main = do
    args <- getArgs
    action' <- handleParseResult (overFailure compactUsage (execParserPure defaultPrefs optsInfo args))
    runAction action'
    where
        compactUsage helpText
            | isEmpty (helpUsage helpText) = helpText
            | otherwise =
                helpText
                    { helpUsage =
                        stringChunk "Usage: occam INPUT.sql -d DIALECT (--oracle COMMAND | --result-oracle) [OPTIONS]"
                    }

runAction :: Action -> IO ()
runAction (ActionCompletion shell) = do
    putStr (generateCompletion shell)
    exitSuccess
runAction ActionListDialects = do
    putStrLn "Supported SQL dialects:"
    forM_ ([minBound .. maxBound] :: [SQLDialect]) $ \d ->
        putStrLn ("  " ++ T.unpack (dialectName d))
    exitSuccess
runAction ActionListColumnModes = do
    putStrLn "Supported column co-reduction modes:"
    putStrLn "  off       Disable column co-reduction (default)"
    putStrLn "  single    List per-column single sites (requires --intra-statement)"
    putStrLn "  atomic    List per-column sites and joint bundles (requires --intra-statement)"
    exitSuccess
runAction ActionListStrategies = do
    putStrLn "Supported reduction strategies:"
    putStrLn
        "  full          Full multi-stage reduction: slice -> closure -> intra -> audit -> fallback (default)"
    putStrLn "  ddmin-only    Fast statement ddmin and audit only"
    putStrLn "  audit-only    Statement deletion audit only"
    exitSuccess
runAction ActionPrintDefaults = do
    let opts = defaultRawOptions
        defaults =
            object
                [ "timeout" .= getSpecified (rawTimeoutSecs opts)
                , "jobs" .= getSpecified (rawJobs opts)
                , "strategy" .= strategyName (getSpecified (rawStrategy opts))
                , "statement_order" .= statementSearchPolicyName (getSpecified (rawStatementOrder opts))
                , "column_mode" .= columnModeName (getSpecified (rawColumnMode opts))
                , "slicing" .= getSpecified (rawSlicing opts)
                , "closure" .= getSpecified (rawClosure opts)
                , "intra_statement" .= getSpecified (rawIntraStatement opts)
                , "literal_batches" .= getSpecified (rawLiteralBatches opts)
                , "ast_batching" .= getSpecified (rawAstBatching opts)
                , "adaptive_values" .= getSpecified (rawAdaptiveValues opts)
                , "unrestricted" .= getSpecified (rawUnrestricted opts)
                , "cache" .= getSpecified (rawCache opts)
                , "log_level" .= getSpecified (rawLogLevel opts)
                , "log_level_json" .= defaultLogLevelForFormat FormatJSON
                , "log_format" .= logFormatName (getSpecified (rawLogFormat opts))
                , "color" .= colorModeName (getSpecified (rawColor opts))
                ]
    BSL.putStr (encode defaults <> "\n")
    exitSuccess
runAction (ActionNormal rawOpts) = do
    resolvedOpts <- case rawConfigFile rawOpts of
        Nothing -> pure rawOpts
        Just cfgPath -> do
            eBytes <- tryIOError (BS.readFile cfgPath)
            case eBytes of
                Left ioErr -> dieOpt ("failed to read --config " ++ cfgPath ++ ": " ++ show ioErr)
                Right bytes -> case parseConfigPatch bytes >>= (`applyConfigPatch` rawOpts) of
                    Left err -> dieOpt err
                    Right patched -> pure patched
    case validateCliOptions resolvedOpts of
        Left err -> dieOpt err
        Right validatedOpts -> runOccam validatedOpts
    where
        dieOpt msg = do
            hPutStrLn stderr ("error: " ++ msg)
            exitWith (ExitFailure 2)
