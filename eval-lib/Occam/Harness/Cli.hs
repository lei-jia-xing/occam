-- | Command-line dispatcher for the @occam-harness@ helper executable.
module Occam.Harness.Cli (
    HarnessCommand (..),
    parseHarnessArgs,
    runHarnessCommand,
    runHarnessMain,
) where

import Data.Aeson (encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Occam.AtomicFile (writeAtomicLazyBytes)
import Occam.Benchmark.BatchReport (aggregateOutDir, summarizeDecisionLog)
import Occam.Harness.Config (
    HarnessConfig (..),
    SquirrelRenderOptions (..),
    loadHarnessConfig,
    lookupSquirrelSpec,
    renderSquirrelYaml,
    resolveHarnessConfigPath,
    squirrelFieldText,
 )
import Occam.Harness.SqlancerSanitize (
    SqlancerSanitizeResult (..),
    sanitizeSqlancerLog,
 )
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- | Supported @occam-harness@ subcommands.
data HarnessCommand
    = CmdAggregate !Bool !FilePath
    | CmdSummarizeDecisions !FilePath
    | CmdSanitizeSqlancer !(Maybe FilePath) !(Maybe FilePath) !Bool
    | CmdRenderSquirrelYaml !(Maybe FilePath) !SquirrelRenderOptions
    | CmdSquirrelField !(Maybe FilePath) !Text !Text
    | CmdCheckConfig !(Maybe FilePath)
    deriving (Eq, Show)

-- | Parse command-line arguments purely for @occam-harness@.
parseHarnessArgs :: [String] -> ParserResult HarnessCommand
parseHarnessArgs =
    execParserPure defaultPrefs parserInfo
    where
        parserInfo =
            info
                (commandParser <**> helper)
                ( fullDesc
                    <> progDesc
                        "Typed Haskell harness helper for batch reporting, Dhall config validation, and SQLancer log sanitization"
                    <> header "occam-harness - Occam evaluation harness helper"
                )

commandParser :: Parser HarnessCommand
commandParser =
    subparser
        ( command
            "aggregate"
            (info aggregateParser (progDesc "Aggregate batch runs into aggregated_stats.json and summary.md"))
            <> command
                "summarize-decisions"
                (info summarizeParser (progDesc "Summarize an oracle_decisions.jsonl log as JSON"))
            <> command
                "sanitize-sqlancer"
                (info sanitizeParser (progDesc "Sanitize a SQLancer crash log into replayable SQL and metadata"))
            <> command
                "render-squirrel-yaml"
                (info renderSquirrelParser (progDesc "Render SQUIRREL YAML config from Dhall-compiled harness.json"))
            <> command
                "squirrel-field"
                ( info
                    squirrelFieldParser
                    (progDesc "Print a single scalar SQUIRREL template field from harness.json")
                )
            <> command
                "check-config"
                (info checkConfigParser (progDesc "Validate config/generated/harness.json against typed schemas"))
        )

aggregateParser :: Parser HarnessCommand
aggregateParser =
    CmdAggregate
        <$> switch (long "print-json" <> help "Print the aggregated report JSON to stdout")
        <*> strArgument (metavar "OUT_DIR" <> help "Batch output directory containing runs/ and qualified/")

summarizeParser :: Parser HarnessCommand
summarizeParser =
    CmdSummarizeDecisions
        <$> strArgument (metavar "DECISIONS_JSONL" <> help "Path to oracle_decisions.jsonl")

sanitizeParser :: Parser HarnessCommand
sanitizeParser =
    CmdSanitizeSqlancer
        <$> optional
            ( strOption
                (long "input" <> short 'i' <> metavar "FILE" <> help "Input SQLancer log (defaults to stdin)")
            )
        <*> optional
            (strOption (long "meta-out" <> metavar "FILE" <> help "Write extracted metadata JSON to FILE"))
        <*> switch (long "json" <> help "Print JSON object {\"sql\": ..., \"metadata\": ...} to stdout")

configOption :: Parser (Maybe FilePath)
configOption =
    optional
        ( strOption
            ( long "config"
                <> metavar "FILE"
                <> help "Path to harness.json (defaults to OCCAM_HARNESS_JSON or config/generated/harness.json)"
            )
        )

renderSquirrelParser :: Parser HarnessCommand
renderSquirrelParser =
    CmdRenderSquirrelYaml
        <$> configOption
        <*> ( SquirrelRenderOptions
                <$> strOption (long "dbms" <> metavar "DBMS" <> help "Target DBMS (sqlite, postgres, mysql, mariadb)")
                <*> strOption (long "squirrel-home" <> metavar "DIR" <> help "SQUIRREL installation directory")
                <*> strOption (long "host" <> value "" <> metavar "HOST" <> help "Database host")
                <*> option auto (long "port" <> value 0 <> metavar "PORT" <> help "Database port")
                <*> strOption (long "user" <> value "" <> metavar "USER" <> help "Database user")
                <*> strOption (long "password" <> value "" <> metavar "PASSWORD" <> help "Database password")
                <*> strOption (long "socket" <> value "" <> metavar "PATH" <> help "Unix socket path")
                <*> strOption (long "db-name" <> value "" <> metavar "NAME" <> help "Database name or prefix")
                <*> strOption (long "server-cmd" <> value "" <> metavar "CMD" <> help "Server startup command")
            )

squirrelFieldParser :: Parser HarnessCommand
squirrelFieldParser =
    CmdSquirrelField
        <$> configOption
        <*> strArgument (metavar "DBMS" <> help "Target SQUIRREL DBMS")
        <*> strArgument (metavar "FIELD" <> help "Field name (e.g. target_bin_rel, default_input_subdir)")

checkConfigParser :: Parser HarnessCommand
checkConfigParser =
    CmdCheckConfig
        <$> optional (strArgument (metavar "CONFIG_JSON" <> help "Optional path to harness.json"))

-- | CLI entry point invoked by @occam-harness@.
runHarnessMain :: IO ()
runHarnessMain = do
    cmd <-
        execParser
            ( info
                (commandParser <**> helper)
                ( fullDesc
                    <> progDesc "Typed Haskell harness helper for Occam"
                    <> header "occam-harness"
                )
            )
    runHarnessCommand cmd

-- | Execute one parsed @HarnessCommand@.
runHarnessCommand :: HarnessCommand -> IO ()
runHarnessCommand = \case
    CmdAggregate printJson outDir -> do
        agg <- aggregateOutDir outDir
        whenTrue printJson (BSL.putStr (encode agg <> BSL.pack [10]))
    CmdSummarizeDecisions logPath -> do
        summary <- summarizeDecisionLog logPath
        BSL.putStr (encode summary <> BSL.pack [10])
    CmdSanitizeSqlancer mInput mMetaOut asJson -> do
        rawBytes <- maybe BS.getContents BS.readFile mInput
        let res = sanitizeSqlancerLog rawBytes
        case mMetaOut of
            Nothing -> pure ()
            Just metaPath -> writeAtomicLazyBytes metaPath (encode (sanitizedMeta res) <> BSL.pack [10])
        if asJson
            then BSL.putStr (encode res <> BSL.pack [10])
            else BS.putStr (sanitizedSql res)
    CmdRenderSquirrelYaml mCfg opts -> do
        hc <- loadOrDie mCfg
        case renderSquirrelYaml hc opts of
            Left err -> dieWith (T.unpack err)
            Right rendered -> TIO.putStr rendered
    CmdSquirrelField mCfg dbms field -> do
        hc <- loadOrDie mCfg
        case lookupSquirrelSpec hc dbms of
            Nothing -> dieWith ("Unsupported SQUIRREL DBMS '" <> T.unpack dbms <> "'")
            Just spec -> for_ (squirrelFieldText spec field) TIO.putStr
    CmdCheckConfig mCfg -> do
        hc <- loadOrDie mCfg
        putStrLn $
            unwords
                [ "Valid harness config:"
                , show (length (hcDbms hc))
                , "dbms,"
                , show (length (hcProfiles hc))
                , "profiles,"
                , show (Map.size (hcPlans hc))
                , "plans,"
                , show (length (hcSquirrel hc))
                , "squirrel templates"
                ]
    where
        whenTrue True act = act
        whenTrue False _ = pure ()

        loadOrDie mCfg = do
            cfgPath <- resolveHarnessConfigPath mCfg
            eCfg <- loadHarnessConfig cfgPath
            case eCfg of
                Left err -> dieWith err
                Right hc -> pure hc

        dieWith msg = do
            hPutStrLn stderr ("occam-harness: " <> msg)
            exitFailure
