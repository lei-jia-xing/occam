-- | CLI option types, JSON configuration file loading, and semantic option validation.
module Options (
    Action (..),
    ColorMode (..),
    ConfigPatch (..),
    RawOptions (..),
    ReductionStrategy (..),
    Specified (..),
    ValidatedOptions (..),
    applyConfigPatch,
    colorModeName,
    defaultLogLevelForFormat,
    defaultRawOptions,
    getSpecified,
    isExplicit,
    logFormatName,
    parseColorMode,
    parseColumnMode,
    parseConfigPatch,
    parseLogFormat,
    parseLogLevel,
    parseStatementOrder,
    parseStrategy,
    resolveColor,
    strategyName,
    validateCliOptions,
) where

import Completion (ShellType)
import Control.Applicative ((<|>))
import Control.Monad (forM_, when)
import Data.Aeson (
    FromJSON (..),
    Value,
    eitherDecodeStrict',
    withObject,
    withText,
    (.:?),
 )
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Occam.Evaluation (parseSQLDialect)
import Occam.Logger (LogFormat (..), LogLevel (..))
import Occam.Parser (SQLDialect (..))
import Occam.Reducer (
    ColumnMode (..),
    StatementSearchPolicy (..),
    convergedRankedScheduleConfig,
    statementRankedFallback,
 )
import System.Environment (lookupEnv)

-- | Tri-state distinguishing defaults from explicit CLI flags.
data Specified a
    = Default a
    | Explicit a
    deriving (Eq, Show, Functor)

-- | Return 'True' if an option value was explicitly overridden by CLI flags or config.
isExplicit :: Specified a -> Bool
isExplicit (Explicit _) = True
isExplicit (Default _) = False

-- | Unwrap the underlying value from 'Specified' regardless of source.
getSpecified :: Specified a -> a
getSpecified (Default x) = x
getSpecified (Explicit x) = x

-- ----------------------------------------------------------------------------
-- Color Mode
-- ----------------------------------------------------------------------------

-- | Terminal ANSI color policy selected via @--color@.
data ColorMode = ColorAuto | ColorAlways | ColorNever
    deriving (Eq, Show)

-- | Parse a CLI @--color@ flag string (@auto@, @always@, or @never@).
parseColorMode :: String -> Either String ColorMode
parseColorMode s = case map toLower s of
    "auto" -> Right ColorAuto
    "always" -> Right ColorAlways
    "never" -> Right ColorNever
    _ -> Left "Expected 'auto', 'always', or 'never'"

-- | Resolve whether ANSI color escapes should be emitted given 'ColorMode', TTY status, and @NO_COLOR@.
resolveColor :: ColorMode -> Bool -> IO Bool
resolveColor ColorAlways _ = pure True
resolveColor ColorNever _ = pure False
resolveColor ColorAuto isTTY = do
    noColor <- isJust <$> lookupEnv "NO_COLOR"
    pure (isTTY && not noColor)

-- | Render a 'ColorMode' as its CLI\/JSON flag string.
colorModeName :: ColorMode -> Text
colorModeName ColorAuto = "auto"
colorModeName ColorAlways = "always"
colorModeName ColorNever = "never"

-- | Render a 'LogFormat' as @"text"@ or @"json"@.
logFormatName :: LogFormat -> Text
logFormatName FormatText = "text"
logFormatName FormatJSON = "json"

-- ----------------------------------------------------------------------------
-- Actions and Options ADTs
-- ----------------------------------------------------------------------------

-- | Dispatched CLI sub-command derived from parsed arguments.
data Action
    = ActionCompletion !ShellType
    | ActionListDialects
    | ActionListColumnModes
    | ActionListStrategies
    | ActionPrintDefaults
    | ActionNormal !RawOptions

-- | Top-level reduction workflow strategy selected by @--strategy@.
data ReductionStrategy = StrategyFull | StrategyDdminOnly | StrategyAuditOnly
    deriving (Eq, Show)

-- | Render a 'ReductionStrategy' as its CLI\/stats keyword.
strategyName :: ReductionStrategy -> T.Text
strategyName StrategyFull = "full"
strategyName StrategyDdminOnly = "ddmin-only"
strategyName StrategyAuditOnly = "audit-only"

-- | Parse a CLI @--log-level@ flag string.
parseLogLevel :: String -> Either String LogLevel
parseLogLevel s = case map toLower s of
    "debug" -> Right LevelDebug
    "info" -> Right LevelInfo
    "warn" -> Right LevelWarn
    "warning" -> Right LevelWarn
    "error" -> Right LevelError
    _ -> Left "Expected debug, info, warn, or error"

-- | Parse a CLI @--log-format@ flag string.
parseLogFormat :: String -> Either String LogFormat
parseLogFormat s = case map toLower s of
    "text" -> Right FormatText
    "json" -> Right FormatJSON
    _ -> Left "Expected text or json"

-- | Parse a CLI @--statement-order@ search policy string.
parseStatementOrder :: String -> Either String StatementSearchPolicy
parseStatementOrder s = case map toLower s of
    "ranked-fallback" -> Right statementRankedFallback
    "ranked" -> Right (StatementRankedWith convergedRankedScheduleConfig)
    "subset-first" -> Right StatementSubsetFirst
    "complement-first" -> Right StatementComplementFirst
    "byte-weighted" -> Right StatementByteWeighted
    _ -> Left "Expected ranked-fallback, ranked, byte-weighted, complement-first, or subset-first"

-- | Parse a CLI @--column-mode@ string (@off@, @single@, or @atomic@).
parseColumnMode :: String -> Either String ColumnMode
parseColumnMode s = case map toLower s of
    "off" -> Right ColumnOff
    "single" -> Right ColumnSingle
    "atomic" -> Right ColumnAtomic
    _ -> Left "Expected off, single, or atomic"

-- | Parse a CLI @--strategy@ string (@full@, @ddmin-only@, or @audit-only@).
parseStrategy :: String -> Either String ReductionStrategy
parseStrategy s = case map toLower s of
    "full" -> Right StrategyFull
    "ddmin-only" -> Right StrategyDdminOnly
    "audit-only" -> Right StrategyAuditOnly
    _ -> Left "Expected full, ddmin-only, or audit-only"

-- | Unvalidated CLI option record before applying JSON config defaults and conflict checks.
data RawOptions = RawOptions
    { rawInputFile :: !FilePath
    , rawOracleCmd :: !(Maybe String)
    , rawOut :: !(Maybe FilePath)
    , rawResultOracle :: !(Specified Bool)
    , rawInPlace :: !(Specified Bool)
    , rawStatsFile :: !(Maybe FilePath)
    , rawResetCmd :: !(Maybe String)
    , rawConfigFile :: !(Maybe FilePath)
    , rawTimeoutSecs :: !(Specified Double)
    , rawLogLevel :: !(Specified LogLevel)
    , rawLogFormat :: !(Specified LogFormat)
    , rawVerbose :: !(Specified Bool)
    , rawQuiet :: !(Specified Bool)
    , rawColor :: !(Specified ColorMode)
    , rawBudgetSecs :: !(Maybe Double)
    , rawMaxOracleExecutions :: !(Maybe Int)
    , rawJobs :: !(Specified Int)
    , rawSlicing :: !(Specified Bool)
    , rawClosure :: !(Specified Bool)
    , rawIntraStatement :: !(Specified Bool)
    , rawStatementOrder :: !(Specified StatementSearchPolicy)
    , rawLiteralBatches :: !(Specified Bool)
    , rawAstBatching :: !(Specified Bool)
    , rawAdaptiveValues :: !(Specified Bool)
    , rawColumnMode :: !(Specified ColumnMode)
    , rawUnrestricted :: !(Specified Bool)
    , rawCache :: !(Specified Bool)
    , rawDialect :: !(Maybe SQLDialect)
    , rawStrategy :: !(Specified ReductionStrategy)
    , rawDumpFacts :: !Bool
    , rawDumpFactsFkOn :: !Bool
    }
    deriving (Eq, Show)

-- | Default CLI options.
defaultRawOptions :: RawOptions
defaultRawOptions =
    RawOptions
        { rawConfigFile = Nothing
        , rawInputFile = "INPUT.sql"
        , rawOracleCmd = Nothing
        , rawResultOracle = Default False
        , rawOut = Nothing
        , rawInPlace = Default False
        , rawStatsFile = Nothing
        , rawResetCmd = Nothing
        , rawTimeoutSecs = Default 10.0
        , rawLogLevel = Default LevelWarn
        , rawLogFormat = Default FormatText
        , rawVerbose = Default False
        , rawQuiet = Default False
        , rawColor = Default ColorAuto
        , rawBudgetSecs = Nothing
        , rawMaxOracleExecutions = Nothing
        , rawJobs = Default 1
        , rawSlicing = Default True
        , rawClosure = Default True
        , rawIntraStatement = Default False
        , rawStatementOrder = Default statementRankedFallback
        , rawLiteralBatches = Default False
        , rawAstBatching = Default False
        , rawAdaptiveValues = Default False
        , rawColumnMode = Default ColumnOff
        , rawUnrestricted = Default True
        , rawCache = Default True
        , rawDialect = Nothing
        , rawStrategy = Default StrategyFull
        , rawDumpFacts = False
        , rawDumpFactsFkOn = False
        }

-- | Partial JSON configuration decoded from @--config@ (e.g. compiled from Dhall).
data ConfigPatch = ConfigPatch
    { cfgDialect :: !(Maybe SQLDialect)
    , cfgStrategy :: !(Maybe ReductionStrategy)
    , cfgSlicing :: !(Maybe Bool)
    , cfgClosure :: !(Maybe Bool)
    , cfgIntraStatement :: !(Maybe Bool)
    , cfgStatementOrder :: !(Maybe StatementSearchPolicy)
    , cfgLiteralBatches :: !(Maybe Bool)
    , cfgAstBatching :: !(Maybe Bool)
    , cfgAdaptiveValues :: !(Maybe Bool)
    , cfgColumnMode :: !(Maybe ColumnMode)
    , cfgUnrestricted :: !(Maybe Bool)
    , cfgCache :: !(Maybe Bool)
    , cfgTimeout :: !(Maybe Double)
    , cfgJobs :: !(Maybe Int)
    , cfgBudget :: !(Maybe Double)
    , cfgMaxOracleExecutions :: !(Maybe Int)
    , cfgResultOracle :: !(Maybe Bool)
    , cfgOracle :: !(Maybe String)
    , cfgReset :: !(Maybe String)
    , cfgOut :: !(Maybe FilePath)
    , cfgInPlace :: !(Maybe Bool)
    , cfgStats :: !(Maybe FilePath)
    , cfgLogLevel :: !(Maybe LogLevel)
    , cfgLogLevelJson :: !(Maybe LogLevel)
    , cfgLogFormat :: !(Maybe LogFormat)
    , cfgVerbose :: !(Maybe Bool)
    , cfgQuiet :: !(Maybe Bool)
    , cfgColor :: !(Maybe ColorMode)
    }
    deriving (Eq, Show)

knownConfigKeys :: Set.Set Text
knownConfigKeys =
    Set.fromList
        [ "dialect"
        , "strategy"
        , "slicing"
        , "closure"
        , "intra_statement"
        , "statement_order"
        , "literal_batches"
        , "ast_batching"
        , "adaptive_values"
        , "column_mode"
        , "unrestricted"
        , "cache"
        , "timeout"
        , "jobs"
        , "budget"
        , "max_oracle_executions"
        , "result_oracle"
        , "oracle"
        , "reset"
        , "out"
        , "in_place"
        , "stats"
        , "log_level"
        , "log_level_json"
        , "log_format"
        , "verbose"
        , "quiet"
        , "color"
        ]

parseEnumField :: (String -> Either String a) -> String -> Value -> Parser a
parseEnumField reader label =
    withText label $ either fail pure . reader . T.unpack

optEnum ::
    (String -> Either String a) -> String -> KeyMap.KeyMap Value -> AesonKey.Key -> Parser (Maybe a)
optEnum reader label obj key = do
    mVal <- obj .:? key
    traverse (parseEnumField reader label) mVal

instance FromJSON ConfigPatch where
    parseJSON = withObject "OccamConfig" $ \obj -> do
        forM_ (KeyMap.keys obj) $ \k ->
            let txt = AesonKey.toText k
             in when (Set.notMember txt knownConfigKeys) $
                    fail ("Unknown --config key: " ++ T.unpack txt)
        ConfigPatch
            <$> optEnum parseSQLDialect "dialect" obj "dialect"
            <*> optEnum parseStrategy "strategy" obj "strategy"
            <*> obj .:? "slicing"
            <*> obj .:? "closure"
            <*> obj .:? "intra_statement"
            <*> optEnum parseStatementOrder "statement_order" obj "statement_order"
            <*> obj .:? "literal_batches"
            <*> obj .:? "ast_batching"
            <*> obj .:? "adaptive_values"
            <*> optEnum parseColumnMode "column_mode" obj "column_mode"
            <*> obj .:? "unrestricted"
            <*> obj .:? "cache"
            <*> obj .:? "timeout"
            <*> obj .:? "jobs"
            <*> obj .:? "budget"
            <*> obj .:? "max_oracle_executions"
            <*> obj .:? "result_oracle"
            <*> obj .:? "oracle"
            <*> obj .:? "reset"
            <*> obj .:? "out"
            <*> obj .:? "in_place"
            <*> obj .:? "stats"
            <*> optEnum parseLogLevel "log_level" obj "log_level"
            <*> optEnum parseLogLevel "log_level_json" obj "log_level_json"
            <*> optEnum parseLogFormat "log_format" obj "log_format"
            <*> obj .:? "verbose"
            <*> obj .:? "quiet"
            <*> optEnum parseColorMode "color" obj "color"

-- | Decode a JSON config file into a partial 'ConfigPatch'.
parseConfigPatch :: ByteString -> Either String ConfigPatch
parseConfigPatch = eitherDecodeStrict'

-- | Merge JSON config defaults behind explicit CLI flags after validating the standalone config patch.
applyConfigPatch :: ConfigPatch -> RawOptions -> Either String RawOptions
applyConfigPatch patch cliOpts = do
    -- Validate internal consistency of options originating solely from the config file.
    let cfgAlone = mergeWith True patch defaultRawOptions
    validateStrategyConflicts cfgAlone
    pure (mergeWith (not (isExplicit (rawStrategy cliOpts))) patch cliOpts)
    where
        mergeWith markExplicit p opts =
            let pickSpec :: Maybe a -> Specified a -> Specified a
                pickSpec Nothing cur = cur
                pickSpec (Just cfgVal) (Default _)
                    | markExplicit = Explicit cfgVal
                    | otherwise = Default cfgVal
                pickSpec _ cur@(Explicit _) = cur
                pickSimple :: Maybe a -> Specified a -> Specified a
                pickSimple Nothing cur = cur
                pickSimple (Just cfgVal) (Default _) = Explicit cfgVal
                pickSimple _ cur@(Explicit _) = cur
                pickMaybe :: Maybe a -> Maybe a -> Maybe a
                pickMaybe mCfg mCli = mCli <|> mCfg
             in opts
                    { rawDialect = pickMaybe (cfgDialect p) (rawDialect opts)
                    , rawStrategy = pickSimple (cfgStrategy p) (rawStrategy opts)
                    , rawSlicing = pickSpec (cfgSlicing p) (rawSlicing opts)
                    , rawClosure = pickSpec (cfgClosure p) (rawClosure opts)
                    , rawIntraStatement = pickSpec (cfgIntraStatement p) (rawIntraStatement opts)
                    , rawStatementOrder = pickSpec (cfgStatementOrder p) (rawStatementOrder opts)
                    , rawLiteralBatches = pickSpec (cfgLiteralBatches p) (rawLiteralBatches opts)
                    , rawAstBatching = pickSpec (cfgAstBatching p) (rawAstBatching opts)
                    , rawAdaptiveValues = pickSpec (cfgAdaptiveValues p) (rawAdaptiveValues opts)
                    , rawColumnMode = pickSpec (cfgColumnMode p) (rawColumnMode opts)
                    , rawUnrestricted = pickSpec (cfgUnrestricted p) (rawUnrestricted opts)
                    , rawCache = pickSpec (cfgCache p) (rawCache opts)
                    , rawTimeoutSecs = pickSimple (cfgTimeout p) (rawTimeoutSecs opts)
                    , rawJobs = pickSimple (cfgJobs p) (rawJobs opts)
                    , rawBudgetSecs = pickMaybe (cfgBudget p) (rawBudgetSecs opts)
                    , rawMaxOracleExecutions = pickMaybe (cfgMaxOracleExecutions p) (rawMaxOracleExecutions opts)
                    , rawResultOracle = pickSimple (cfgResultOracle p) (rawResultOracle opts)
                    , rawOracleCmd = pickMaybe (cfgOracle p) (rawOracleCmd opts)
                    , rawResetCmd = pickMaybe (cfgReset p) (rawResetCmd opts)
                    , rawOut = pickMaybe (cfgOut p) (rawOut opts)
                    , rawInPlace = pickSimple (cfgInPlace p) (rawInPlace opts)
                    , rawStatsFile = pickMaybe (cfgStats p) (rawStatsFile opts)
                    , rawLogLevel = pickSimple (cfgLogLevel p) (rawLogLevel opts)
                    , rawLogFormat = pickSimple (cfgLogFormat p) (rawLogFormat opts)
                    , rawVerbose = pickSimple (cfgVerbose p) (rawVerbose opts)
                    , rawQuiet = pickSimple (cfgQuiet p) (rawQuiet opts)
                    , rawColor = pickSimple (cfgColor p) (rawColor opts)
                    }

-- | Log level used when the format is not explicitly configured as JSON.
defaultLogLevelForFormat :: LogFormat -> LogLevel
defaultLogLevelForFormat FormatJSON = LevelInfo
defaultLogLevelForFormat FormatText = getSpecified (rawLogLevel defaultRawOptions)

-- | Validated, strategy-resolved CLI options.
data ValidatedOptions = ValidatedOptions
    { valInputFile :: !FilePath
    , valOracleCmd :: !(Maybe String)
    , valResultOracle :: !Bool
    , valOut :: !(Maybe FilePath)
    , valInPlace :: !Bool
    , valStatsFile :: !(Maybe FilePath)
    , valResetCmd :: !(Maybe String)
    , valTimeoutSecs :: !Double
    , valLogLevel :: !(Specified LogLevel)
    , valLogFormat :: !LogFormat
    , valVerbose :: !Bool
    , valQuiet :: !Bool
    , valColor :: !ColorMode
    , valBudgetSecs :: !(Maybe Double)
    , valMaxOracleExecutions :: !(Maybe Int)
    , valJobs :: !Int
    , valSlicing :: !Bool
    , valClosure :: !Bool
    , valIntraStatement :: !Bool
    , valStatementOrder :: !StatementSearchPolicy
    , valLiteralBatches :: !Bool
    , valAstBatching :: !Bool
    , valAdaptiveValues :: !Bool
    , valColumnMode :: !ColumnMode
    , valUnrestricted :: !Bool
    , valCache :: !Bool
    , valDialect :: !SQLDialect
    , valStrategy :: !ReductionStrategy
    , valDumpFacts :: !Bool
    , valDumpFactsFkOn :: !Bool
    }
    deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- Two-Phase Validation
-- ----------------------------------------------------------------------------

-- | Validate merged CLI and config-file options into a total 'ValidatedOptions' record.
validateCliOptions :: RawOptions -> Either String ValidatedOptions
validateCliOptions opts@RawOptions {..} = do
    dialect <- maybe (Left "Missing required --dialect (see --list-dialects)") Right rawDialect
    let verbose = getSpecified rawVerbose
        quiet = getSpecified rawQuiet
        inPlace = getSpecified rawInPlace
        resultOracle = getSpecified rawResultOracle
        timeoutSecs = getSpecified rawTimeoutSecs
        jobs = getSpecified rawJobs
        logFormat = getSpecified rawLogFormat
        color = getSpecified rawColor

    -- 1. Mutually exclusive flags
    when (verbose && quiet) $
        Left "Cannot specify both --verbose and --quiet"
    when (inPlace && isJust rawOut) $
        Left "Cannot specify both --in-place and --out"
    when (inPlace && rawInputFile == "-") $
        Left "Cannot modify stdin in place (--in-place with '-' input)"
    when resultOracle $ do
        when (isJust rawOracleCmd || isJust rawResetCmd) $
            Left "--result-oracle cannot be combined with --oracle or --reset"
        when (dialect /= SQLite && dialect /= DuckDB) $
            Left "--result-oracle requires --dialect sqlite or duckdb"

    -- 2. Strategy conflicts
    validateStrategyConflicts opts

    -- 3. Intra-statement refinement prerequisites
    let colMode = getSpecified rawColumnMode
        intra = getSpecified rawIntraStatement
        litBatches = getSpecified rawLiteralBatches
        astBatches = getSpecified rawAstBatching
        adaptValues = case rawAdaptiveValues of
            Default _ -> resultOracle
            Explicit enabled -> enabled
    when (colMode /= ColumnOff && not intra) $
        Left
            "--column-mode single|atomic requires --intra-statement to be enabled (add '--intra-statement')"
    mapM_
        (uncurry (requireIntraFlag intra))
        [ ("--literal-batches", rawLiteralBatches)
        , ("--ast-batching", rawAstBatching)
        , ("--adaptive-values", rawAdaptiveValues)
        ]

    -- 4. Numeric bounds
    validateNumericBounds timeoutSecs jobs rawBudgetSecs rawMaxOracleExecutions

    -- 5. Effective flags according to strategy
    let strategy = getSpecified rawStrategy
        effSlicing = enabledForFull strategy rawSlicing
        effClosure = enabledForFull strategy rawClosure
        effIntra = enabledForFull strategy rawIntraStatement
        effUnrestricted = enabledForFull strategy rawUnrestricted

    pure
        ValidatedOptions
            { valInputFile = rawInputFile
            , valOracleCmd = rawOracleCmd
            , valResultOracle = resultOracle
            , valOut = rawOut
            , valInPlace = inPlace
            , valStatsFile = rawStatsFile
            , valResetCmd = rawResetCmd
            , valTimeoutSecs = timeoutSecs
            , valLogLevel = rawLogLevel
            , valLogFormat = logFormat
            , valVerbose = verbose
            , valQuiet = quiet
            , valColor = color
            , valBudgetSecs = rawBudgetSecs
            , valMaxOracleExecutions = rawMaxOracleExecutions
            , valJobs = jobs
            , valSlicing = effSlicing
            , valClosure = effClosure
            , valIntraStatement = effIntra
            , valStatementOrder = getSpecified rawStatementOrder
            , valLiteralBatches = effIntra && litBatches
            , valAstBatching = effIntra && astBatches
            , valAdaptiveValues = effIntra && adaptValues
            , valColumnMode = colMode
            , valUnrestricted = effUnrestricted
            , valCache = getSpecified rawCache
            , valDialect = dialect
            , valStrategy = strategy
            , valDumpFacts = rawDumpFacts
            , valDumpFactsFkOn = rawDumpFactsFkOn
            }

validateStrategyConflicts :: RawOptions -> Either String ()
validateStrategyConflicts RawOptions {..} = case rawStrategy of
    Explicit StrategyDdminOnly ->
        mapM_
            (\(cond, msg) -> when cond (Left msg))
            [
                ( enabled rawSlicing
                , "--slicing conflicts with --strategy ddmin-only (slicing is disabled in ddmin-only mode)"
                )
            ,
                ( enabled rawClosure
                , "--closure conflicts with --strategy ddmin-only (closure search is disabled in ddmin-only mode)"
                )
            ,
                ( enabled rawIntraStatement
                , "--intra-statement conflicts with --strategy ddmin-only (intra-statement rewrites are disabled in ddmin-only mode)"
                )
            ,
                ( enabled rawLiteralBatches
                , "--literal-batches conflicts with --strategy ddmin-only (intra-statement rewrites are disabled in ddmin-only mode)"
                )
            ,
                ( enabled rawAstBatching
                , "--ast-batching conflicts with --strategy ddmin-only (intra-statement rewrites are disabled in ddmin-only mode)"
                )
            ,
                ( enabled rawAdaptiveValues
                , "--adaptive-values conflicts with --strategy ddmin-only (intra-statement rewrites are disabled in ddmin-only mode)"
                )
            ,
                ( enabled rawUnrestricted
                , "--unrestricted conflicts with --strategy ddmin-only (unrestricted fallback is disabled in ddmin-only mode)"
                )
            ]
    Explicit StrategyAuditOnly -> do
        mapM_
            (\(flag, spec) -> when (enabled spec) $ Left (flag ++ " conflicts with --strategy audit-only"))
            [ ("--slicing", rawSlicing)
            , ("--closure", rawClosure)
            , ("--intra-statement", rawIntraStatement)
            , ("--literal-batches", rawLiteralBatches)
            , ("--ast-batching", rawAstBatching)
            , ("--adaptive-values", rawAdaptiveValues)
            , ("--unrestricted", rawUnrestricted)
            ]
        when (isExplicit rawStatementOrder) $
            Left "--statement-order conflicts with --strategy audit-only"
    _ -> pure ()
    where
        enabled spec = isExplicit spec && getSpecified spec

requireIntraFlag :: Bool -> String -> Specified Bool -> Either String ()
requireIntraFlag intra flag spec =
    when (isExplicit spec && getSpecified spec && not intra) $
        Left (flag ++ " requires --intra-statement to be enabled (add '--intra-statement')")

validateNumericBounds :: Double -> Int -> Maybe Double -> Maybe Int -> Either String ()
validateNumericBounds timeoutSecs jobs budgetSecs maxOracleExecutions = do
    let maxSeconds = fromIntegral (maxBound :: Int) / 1000000 :: Double
        invalidDuration d = d <= 0 || d > maxSeconds || isNaN d || isInfinite d
    when (invalidDuration timeoutSecs) $
        Left "--timeout must be a positive finite number of seconds"
    when (jobs <= 0) $
        Left "--jobs must be positive"
    case budgetSecs of
        Just b | invalidDuration b -> Left "--budget must be a positive finite number of seconds"
        _ -> pure ()
    case maxOracleExecutions of
        Just m | m <= 0 -> Left "--max-oracle-executions must be positive"
        _ -> pure ()

enabledForFull :: ReductionStrategy -> Specified Bool -> Bool
enabledForFull StrategyFull spec = getSpecified spec
enabledForFull _ _ = False
