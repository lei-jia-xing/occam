module Options.Parser (
    actionParser,
    optsInfo,
    rawOptionsParser,
) where

import Completion (parseShellType, supportedShells)
import Control.Applicative ((<|>))
import Data.Text qualified as T
import Occam qualified
import Occam.Evaluation (parseSQLDialect)
import Occam.Parser (dialectName)
import Occam.Reducer (ColumnMode, StatementSearchPolicy)
import Options (
    Action (..),
    RawOptions (..),
    ReductionStrategy (..),
    Specified (..),
    defaultRawOptions,
    getSpecified,
    parseColorMode,
    parseColumnMode,
    parseLogFormat,
    parseLogLevel,
    parseStatementOrder,
    parseStrategy,
 )
import Options.Applicative (
    Mod,
    OptionFields,
    Parser,
    ParserInfo,
    ReadM,
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
        <*> switchSpecified
            "result-oracle"
            Nothing
            "Preserve query results with SQLite or DuckDB"
            (getSpecified (rawResultOracle defaultRawOptions))
        <*> switchSpecified
            "in-place"
            (Just 'i')
            "Replace input after validation. Keep a .bak backup."
            (getSpecified (rawInPlace defaultRawOptions))
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
        <*> optional
            ( strOption
                ( long "config"
                    <> metavar "FILE"
                    <> help "Load JSON configuration (CLI flags override)"
                    <> action "file"
                )
            )
        <*> optionSpecified
            auto
            ( long "timeout"
                <> metavar "SECONDS"
                <> help "Timeout per execution (default: 10 seconds)"
            )
            (rawTimeoutSecs defaultRawOptions)
        <*> optionSpecified
            (eitherReader parseLogLevel)
            ( long "log-level"
                <> metavar "LEVEL"
                <> help "debug, info, warn (default), or error"
                <> completeWith ["debug", "info", "warn", "error"]
            )
            (rawLogLevel defaultRawOptions)
        <*> optionSpecified
            (eitherReader parseLogFormat)
            ( long "log-format"
                <> metavar "FORMAT"
                <> help "text (default) or json"
                <> completeWith ["text", "json"]
            )
            (rawLogFormat defaultRawOptions)
        <*> switchSpecified
            "verbose"
            Nothing
            "Show debug logs and progress"
            (getSpecified (rawVerbose defaultRawOptions))
        <*> switchSpecified
            "quiet"
            (Just 'q')
            "Show errors only"
            (getSpecified (rawQuiet defaultRawOptions))
        <*> optionSpecified
            (eitherReader parseColorMode)
            ( long "color"
                <> metavar "WHEN"
                <> help "auto (default), always, or never. Honors NO_COLOR."
                <> completeWith ["auto", "always", "never"]
            )
            (rawColor defaultRawOptions)
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
        <*> optionSpecified
            (eitherReader parseJobs)
            ( short 'j'
                <> long "jobs"
                <> metavar "N"
                <> help "Concurrent oracle runs (default: 1)"
            )
            (rawJobs defaultRawOptions)
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

optionSpecified :: ReadM a -> Mod OptionFields (Specified a) -> Specified a -> Parser (Specified a)
optionSpecified reader mods def = option (Explicit <$> reader) mods <|> pure def

switchSpecified :: String -> Maybe Char -> String -> Bool -> Parser (Specified Bool)
switchSpecified flagName mShort desc def =
    flag' (Explicit True) (long flagName <> maybe mempty short mShort <> help desc)
        <|> pure (Default def)

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

parseJobs :: String -> Either String Int
parseJobs s = case reads s of
    [(n, "")]
        | n >= (1 :: Integer)
        , n <= toInteger (maxBound :: Int) ->
            Right (fromInteger n)
    _ -> Left "Expected a positive --jobs value"

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
