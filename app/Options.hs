module Options (
    Action (..),
    ColorMode (..),
    RawOptions (..),
    ReductionStrategy (..),
    Specified (..),
    ValidatedOptions (..),
    colorModeName,
    defaultLogLevelForFormat,
    defaultRawOptions,
    getSpecified,
    isExplicit,
    logFormatName,
    parseColorMode,
    resolveColor,
    strategyName,
    validateCliOptions,
) where

import Completion (ShellType)
import Control.Monad (when)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Occam.Logger (LogFormat (..), LogLevel (..))
import Occam.Parser (SQLDialect (..))
import Occam.Reducer (
    ColumnMode (..),
    StatementSearchPolicy,
    statementRankedFallback,
 )
import System.Environment (lookupEnv)

-- | Tri-state distinguishing defaults from explicit CLI flags.
data Specified a
    = Default a
    | Explicit a
    deriving (Eq, Show, Functor)

isExplicit :: Specified a -> Bool
isExplicit (Explicit _) = True
isExplicit (Default _) = False

getSpecified :: Specified a -> a
getSpecified (Default x) = x
getSpecified (Explicit x) = x

-- ----------------------------------------------------------------------------
-- Color Mode
-- ----------------------------------------------------------------------------

data ColorMode = ColorAuto | ColorAlways | ColorNever
    deriving (Eq, Show)

parseColorMode :: String -> Either String ColorMode
parseColorMode s = case map toLower s of
    "auto" -> Right ColorAuto
    "always" -> Right ColorAlways
    "never" -> Right ColorNever
    _ -> Left "Expected 'auto', 'always', or 'never'"

resolveColor :: ColorMode -> Bool -> IO Bool
resolveColor ColorAlways _ = pure True
resolveColor ColorNever _ = pure False
resolveColor ColorAuto isTTY = do
    noColor <- isJust <$> lookupEnv "NO_COLOR"
    pure (isTTY && not noColor)

colorModeName :: ColorMode -> Text
colorModeName ColorAuto = "auto"
colorModeName ColorAlways = "always"
colorModeName ColorNever = "never"

logFormatName :: LogFormat -> Text
logFormatName FormatText = "text"
logFormatName FormatJSON = "json"

-- ----------------------------------------------------------------------------
-- Actions and Options ADTs
-- ----------------------------------------------------------------------------

data Action
    = ActionCompletion !ShellType
    | ActionListDialects
    | ActionListColumnModes
    | ActionListStrategies
    | ActionPrintDefaults
    | ActionNormal !RawOptions

data ReductionStrategy = StrategyFull | StrategyDdminOnly | StrategyAuditOnly
    deriving (Eq, Show)

strategyName :: ReductionStrategy -> T.Text
strategyName StrategyFull = "full"
strategyName StrategyDdminOnly = "ddmin-only"
strategyName StrategyAuditOnly = "audit-only"

data RawOptions = RawOptions
    { rawInputFile :: !FilePath
    , rawOracleCmd :: !(Maybe String)
    , rawOut :: !(Maybe FilePath)
    , rawResultOracle :: !Bool
    , rawInPlace :: !Bool
    , rawStatsFile :: !(Maybe FilePath)
    , rawResetCmd :: !(Maybe String)
    , rawTimeoutSecs :: !Double
    , rawLogLevel :: !(Specified LogLevel)
    , rawLogFormat :: !LogFormat
    , rawVerbose :: !Bool
    , rawQuiet :: !Bool
    , rawColor :: !ColorMode
    , rawBudgetSecs :: !(Maybe Double)
    , rawMaxOracleExecutions :: !(Maybe Int)
    , rawJobs :: !Int
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
        { rawInputFile = "INPUT.sql"
        , rawOracleCmd = Nothing
        , rawResultOracle = False
        , rawOut = Nothing
        , rawInPlace = False
        , rawStatsFile = Nothing
        , rawResetCmd = Nothing
        , rawTimeoutSecs = 10.0
        , rawLogLevel = Default LevelWarn
        , rawLogFormat = FormatText
        , rawVerbose = False
        , rawQuiet = False
        , rawColor = ColorAuto
        , rawBudgetSecs = Nothing
        , rawMaxOracleExecutions = Nothing
        , rawJobs = 1
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

validateCliOptions :: RawOptions -> Either String ValidatedOptions
validateCliOptions opts@RawOptions {..} = do
    dialect <- maybe (Left "Missing required --dialect (see --list-dialects)") Right rawDialect

    -- 1. Mutually exclusive flags
    when (rawVerbose && rawQuiet) $
        Left "Cannot specify both --verbose and --quiet"
    when (rawInPlace && isJust rawOut) $
        Left "Cannot specify both --in-place and --out"
    when (rawInPlace && rawInputFile == "-") $
        Left "Cannot modify stdin in place (--in-place with '-' input)"
    when rawResultOracle $ do
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
            Default _ -> rawResultOracle
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
    validateNumericBounds rawTimeoutSecs rawJobs rawBudgetSecs rawMaxOracleExecutions

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
            , valResultOracle = rawResultOracle
            , valOut = rawOut
            , valInPlace = rawInPlace
            , valStatsFile = rawStatsFile
            , valResetCmd = rawResetCmd
            , valTimeoutSecs = rawTimeoutSecs
            , valLogLevel = rawLogLevel
            , valLogFormat = rawLogFormat
            , valVerbose = rawVerbose
            , valQuiet = rawQuiet
            , valColor = rawColor
            , valBudgetSecs = rawBudgetSecs
            , valMaxOracleExecutions = rawMaxOracleExecutions
            , valJobs = rawJobs
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
