module Options.Parser (
    actionParser,
    optsInfo,
    rawOptionsParser,
) where

import Completion (parseShellType, supportedShells)
import Control.Applicative ((<|>))
import Data.Char (toLower)
import Data.Text qualified as T
import Occam qualified
import Occam.Evaluation (parseSQLDialect)
import Occam.Logger (LogFormat (..), LogLevel (..))
import Occam.Parser (dialectName)
import Occam.Reducer (
    ColumnMode (..),
    StatementSearchPolicy (..),
    convergedRankedScheduleConfig,
    statementRankedFallback,
 )
import Options (
    Action (..),
    RawOptions (..),
    ReductionStrategy (..),
    Specified (..),
    defaultRawOptions,
    getSpecified,
    parseColorMode,
 )
import Options.Applicative (
    Parser,
    ParserInfo,
    action,
    auto,
    completeWith,
    eitherReader,
    flag',
    fullDesc,
    help,
    helper,
    info,
    infoOption,
    internal,
    long,
    metavar,
    option,
    optional,
    short,
    strArgument,
    strOption,
    switch,
    value,
    (<**>),
 )

actionParser :: Parser Action
actionParser =
    (ActionNormal <$> rawOptionsParser)
        <|> flag'
            ActionListDialects
            ( long "list-dialects"
                <> help "List SQL dialects"
            )
        <|> flag'
            ActionListColumnModes
            ( long "list-column-modes"
                <> help "List column reduction modes"
            )
        <|> flag'
            ActionListStrategies
            ( long "list-strategies"
                <> help "List reduction strategies"
            )
        <|> flag'
            ActionPrintDefaults
            ( long "print-defaults"
                <> help "Print defaults as JSON"
            )
        <|> ( ActionCompletion
                <$> option
                    (eitherReader parseShellType)
                    ( long "completion"
                        <> metavar "SHELL"
                        <> help "Generate completions for bash, zsh, or fish"
                        <> completeWith (map fst supportedShells)
                    )
            )

rawOptionsParser :: Parser RawOptions
rawOptionsParser =
    RawOptions
        <$> strArgument
            ( metavar "INPUT.sql"
                <> help "Input SQL file or '-' for stdin"
                <> action "file"
            )
        <*> optional
            ( strOption
                ( long "oracle"
                    <> short 'c'
                    <> metavar "COMMAND"
                    <> help "Read SQL on stdin. Exit 0 accepts."
                )
            )
        <*> optional
            ( strOption
                ( long "out"
                    <> short 'o'
                    <> metavar "PATH"
                    <> help "Write SQL to PATH, or '-' for stdout (default)"
                    <> action "file"
                )
            )
        <*> switch
            ( long "result-oracle"
                <> help "Preserve query results with SQLite or DuckDB"
            )
        <*> switch
            ( long "in-place"
                <> short 'i'
                <> help "Replace input after validation. Keep a .bak backup."
            )
        <*> optional
            ( strOption
                ( long "stats"
                    <> metavar "FILE"
                    <> help "Write JSON statistics"
                    <> action "file"
                )
            )
        <*> optional
            ( strOption
                ( long "reset"
                    <> metavar "COMMAND"
                    <> help "Run before each oracle execution"
                )
            )
        <*> option
            auto
            ( long "timeout"
                <> metavar "SECONDS"
                <> value (rawTimeoutSecs defaultRawOptions)
                <> help "Timeout per execution (default: 10 seconds)"
            )
        <*> ( ( Explicit
                    <$> option
                        (eitherReader parseLogLevel)
                        ( long "log-level"
                            <> metavar "LEVEL"
                            <> help "debug, info, warn (default), or error"
                            <> completeWith ["debug", "info", "warn", "error"]
                        )
              )
                <|> pure (rawLogLevel defaultRawOptions)
            )
        <*> option
            (eitherReader parseLogFormat)
            ( long "log-format"
                <> metavar "FORMAT"
                <> value (rawLogFormat defaultRawOptions)
                <> help "text (default) or json"
                <> completeWith ["text", "json"]
            )
        <*> switch
            ( long "verbose"
                <> help "Show debug logs and progress"
            )
        <*> switch
            ( long "quiet"
                <> short 'q'
                <> help "Show errors only"
            )
        <*> option
            (eitherReader parseColorMode)
            ( long "color"
                <> metavar "WHEN"
                <> value (rawColor defaultRawOptions)
                <> help "auto (default), always, or never. Honors NO_COLOR."
                <> completeWith ["auto", "always", "never"]
            )
        <*> optional
            ( option
                auto
                ( long "budget"
                    <> long "time-limit"
                    <> metavar "SECONDS"
                    <> help "Total time limit, including oracle checks"
                )
            )
        <*> optional
            ( option
                auto
                ( long "max-oracle-executions"
                    <> long "max-evals"
                    <> metavar "N"
                    <> help "Maximum uncached oracle attempts"
                )
            )
        <*> option
            (eitherReader parseJobs)
            ( short 'j'
                <> long "jobs"
                <> metavar "N"
                <> value (rawJobs defaultRawOptions)
                <> help "Concurrent oracle runs (default: 1)"
            )
        <*> toggleSpecified
            "slicing"
            "no-slicing"
            "dependency slicing"
            (getSpecified (rawSlicing defaultRawOptions))
        <*> toggleSpecified
            "closure"
            "no-closure"
            "dependency closure"
            (getSpecified (rawClosure defaultRawOptions))
        <*> toggleSpecified
            "intra-statement"
            "no-intra-statement"
            "intra-statement reductions"
            (getSpecified (rawIntraStatement defaultRawOptions))
        <*> statementOrderParser
        <*> toggleSpecified
            "literal-batches"
            "no-literal-batches"
            "literal edit batching (requires --intra-statement)"
            (getSpecified (rawLiteralBatches defaultRawOptions))
        <*> toggleSpecified
            "ast-batching"
            "no-ast-batching"
            "independent AST edit batching (requires --intra-statement)"
            (getSpecified (rawAstBatching defaultRawOptions))
        <*> toggleSpecified
            "adaptive-values"
            "no-adaptive-values"
            "deferred fine VALUES chunks (requires --intra-statement, on by default with --result-oracle)"
            (getSpecified (rawAdaptiveValues defaultRawOptions))
        <*> columnModeParser
        <*> toggleSpecified
            "unrestricted"
            "no-unrestricted"
            "unrestricted fallback search"
            (getSpecified (rawUnrestricted defaultRawOptions))
        <*> toggleSpecified "cache" "no-cache" "candidate caching" (getSpecified (rawCache defaultRawOptions))
        <*> optional
            ( option
                (eitherReader parseSQLDialect)
                ( long "dialect"
                    <> short 'd'
                    <> metavar "DIALECT"
                    <> help
                        "SQL dialect (required). See --list-dialects."
                    <> completeWith (map (T.unpack . dialectName) [minBound .. maxBound])
                )
            )
        <*> strategyParser
        <*> switch
            ( long "dump-facts"
                <> internal
            )
        <*> switch
            ( long "dump-facts-fk-on"
                <> internal
            )

toggleSpecified :: String -> String -> String -> Bool -> Parser (Specified Bool)
toggleSpecified enableName disableName description def =
    flag' (Explicit True) (long enableName <> help ("Enable " ++ description))
        <|> flag' (Explicit False) (long disableName <> help ("Disable " ++ description))
        <|> pure (Default def)

statementOrderParser :: Parser (Specified StatementSearchPolicy)
statementOrderParser =
    option
        (Explicit <$> eitherReader parseStatementOrder)
        ( long "statement-order"
            <> metavar "ORDER"
            <> help
                "Statement ddmin search order: ranked-fallback (default), ranked, byte-weighted, complement-first, or subset-first"
            <> completeWith ["ranked-fallback", "ranked", "byte-weighted", "complement-first", "subset-first"]
        )
        <|> pure (rawStatementOrder defaultRawOptions)

strategyParser :: Parser (Specified ReductionStrategy)
strategyParser =
    option
        (Explicit <$> eitherReader parseStrategy)
        ( long "strategy"
            <> metavar "STRATEGY"
            <> help "Default: full. See --list-strategies."
            <> completeWith ["full", "ddmin-only", "audit-only"]
        )
        <|> pure (rawStrategy defaultRawOptions)

columnModeParser :: Parser (Specified ColumnMode)
columnModeParser =
    option
        (Explicit <$> eitherReader parseColumnMode)
        ( long "column-mode"
            <> metavar "MODE"
            <> help "off (default). Other modes need --intra-statement."
            <> completeWith ["off", "single", "atomic"]
        )
        <|> pure (rawColumnMode defaultRawOptions)

parseLogLevel :: String -> Either String LogLevel
parseLogLevel s = case map toLower s of
    "debug" -> Right LevelDebug
    "info" -> Right LevelInfo
    "warn" -> Right LevelWarn
    "warning" -> Right LevelWarn
    "error" -> Right LevelError
    _ -> Left "Expected debug, info, warn, or error"

parseLogFormat :: String -> Either String LogFormat
parseLogFormat s = case map toLower s of
    "text" -> Right FormatText
    "json" -> Right FormatJSON
    _ -> Left "Expected text or json"

parseJobs :: String -> Either String Int
parseJobs s = case reads s of
    [(n, "")]
        | n >= (1 :: Integer)
        , n <= toInteger (maxBound :: Int) ->
            Right (fromInteger n)
    _ -> Left "Expected a positive --jobs value"

parseStatementOrder :: String -> Either String StatementSearchPolicy
parseStatementOrder s = case map toLower s of
    "ranked-fallback" -> Right statementRankedFallback
    "ranked" -> Right (StatementRankedWith convergedRankedScheduleConfig)
    "subset-first" -> Right StatementSubsetFirst
    "complement-first" -> Right StatementComplementFirst
    "byte-weighted" -> Right StatementByteWeighted
    _ -> Left "Expected ranked-fallback, ranked, byte-weighted, complement-first, or subset-first"

parseColumnMode :: String -> Either String ColumnMode
parseColumnMode s = case map toLower s of
    "off" -> Right ColumnOff
    "single" -> Right ColumnSingle
    "atomic" -> Right ColumnAtomic
    _ -> Left "Expected off, single, or atomic"

parseStrategy :: String -> Either String ReductionStrategy
parseStrategy s = case map toLower s of
    "full" -> Right StrategyFull
    "ddmin-only" -> Right StrategyDdminOnly
    "audit-only" -> Right StrategyAuditOnly
    _ -> Left "Expected full, ddmin-only, or audit-only"

versionOption :: Parser (a -> a)
versionOption =
    infoOption
        ("Occam v" ++ Occam.version)
        ( long "version"
            <> short 'v'
            <> help "Show version information"
        )

optsInfo :: ParserInfo Action
optsInfo =
    info (actionParser <**> versionOption <**> helper) fullDesc
