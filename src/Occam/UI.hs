{-# LANGUAGE OverloadedRecordDot #-}

-- | Terminal progress rendering and summary formatting for CLI reduction runs.
module Occam.UI (
    UIConfig (..),
    UI (..),
    FinishSummary (..),
    newUI,
    formatSize,
    formatRatio,
    formatInt,
) where

import Control.Monad (when)
import Data.Char (toUpper)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time.Clock (diffUTCTime)
import Numeric (showFFloat)
import Occam.Evaluation (
    EvaluationMetrics (..),
    EvaluationProgress (..),
    EvaluationStatus (..),
 )
import System.Console.ANSI (
    Color (..),
    ColorIntensity (..),
    ConsoleIntensity (..),
    ConsoleLayer (..),
    SGR (..),
    hSupportsANSI,
    setSGRCode,
 )
import System.IO (hFlush, hPutStrLn, stderr)
import System.ProgressBar (
    Label (..),
    OnComplete (..),
    Progress (..),
    ProgressBarWidth (..),
    Style (..),
    Timing (..),
    defStyle,
    hNewProgressBar,
    updateProgress,
 )

-- | Visual and verbosity settings controlling terminal UI progress output.
data UIConfig = UIConfig
    { uiTargetFile :: !FilePath
    , uiInitialBytes :: !Int
    , uiInitialStmts :: !Int
    , uiDialect :: !Text
    , uiQuiet :: !Bool
    , uiVerbose :: !Bool
    , uiUseColor :: !Bool
    }
    deriving (Eq, Show)

data ProgressInfo = ProgressInfo
    { piPhase :: !Text
    , piCurrentStmts :: !Int
    , piInitialStmts :: !Int
    , piCurrentBytes :: !Int
    , piTotalRuns :: !Int
    , piCacheHits :: !Int
    }

data NonTTYState = NonTTYState
    { ntPhase :: !Text
    , ntStmts :: !Int
    , ntBytes :: !Int
    }

-- | Parameters required to render the final reduction summary.
data FinishSummary = FinishSummary
    { finishMetrics :: !EvaluationMetrics
    , finishInitialBytes :: !Int
    , finishReducedBytes :: !Int
    , finishInitialStmts :: !Int
    , finishReducedStmts :: !Int
    , finishMinimalPath :: !FilePath
    , finishStatsPath :: !FilePath
    }

-- | Callbacks consuming reduction progress updates and final run summaries.
data UI = UI
    { uiOnProgress :: !(EvaluationProgress -> IO ())
    , uiFinish :: !(FinishSummary -> IO ())
    }

-- | Format byte counts into human-readable units (e.g. "271 B", "1.3 KB", "4.2 MB").
formatSize :: Int -> String
formatSize bytes
    | bytes < 1024 = show bytes ++ " B"
    | bytes < 1024 * 1024 =
        let kb = fromIntegral bytes / 1024.0 :: Double
         in showFFloat (Just 1) kb " KB"
    | otherwise =
        let mb = fromIntegral bytes / (1024.0 * 1024.0) :: Double
         in showFFloat (Just 1) mb " MB"

-- | Format percentage with one decimal place.
formatRatio :: Double -> String
formatRatio r = showFFloat (Just 1) r "%"

-- | Format an integer with thousands commas (e.g. 1420 -> "1,420").
formatInt :: Int -> String
formatInt n
    | n < 0 = '-' : formatInt (negate n)
    | n < 1000 = show n
    | otherwise =
        let (rest, last3) = n `quotRem` 1000
            last3Str = case show last3 of
                [d] -> "00" ++ [d]
                [d1, d2] -> '0' : [d1, d2]
                s -> s
         in formatInt rest ++ "," ++ last3Str

-- | Right-aligned, fixed-width bold action verb in Cargo / uv CLI style.
renderAction :: Bool -> Color -> String -> String
renderAction useColor color verb =
    let padded = replicate (max 0 (10 - length verb)) ' ' ++ verb
     in if useColor
            then
                setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid color]
                    ++ padded
                    ++ setSGRCode [Reset]
                    ++ " "
            else padded ++ " "

-- | Human-readable action verbs for known stage identifiers.
stageVerbTable :: [(Text, String)]
stageVerbTable =
    [ ("slicing", "Slicing")
    , ("closure-search", "Closure")
    , ("graph-free-search", "Ddmin")
    , ("unrestricted-fallback", "Fallback")
    , ("statement-audit", "Audit")
    , ("intra-statement-rewrites", "Rewriting")
    ]

-- | Map internal stage identifiers to human-readable action verbs.
stageVerb :: Text -> String
stageVerb stage = fromMaybe fallback (lookup stage stageVerbTable)
    where
        fallback = case T.unpack stage of
            [] -> "Working"
            (c : cs) -> toUpper c : cs

-- | Initialize the UI, selecting a quiet, TTY, or non-TTY renderer.
newUI :: UIConfig -> IO UI
newUI config
    | uiQuiet config || uiVerbose config = pure quietUI
    | otherwise = do
        tty <- hSupportsANSI stderr
        if tty then newTTYUI config else newNonTTYUI config

-- | Renderer that stays silent unless it is the only UI.
quietUI :: UI
quietUI =
    UI
        { uiOnProgress = const (pure ())
        , uiFinish = const (pure ())
        }

targetDescription :: UIConfig -> String
targetDescription UIConfig {uiTargetFile}
    | uiTargetFile == "-" = "<stdin>"
    | otherwise = uiTargetFile

printReductionHeader :: Bool -> UIConfig -> IO ()
printReductionHeader useColor config@UIConfig {..} = do
    hPutStrLn
        stderr
        ( renderAction useColor Cyan "Reducing"
            ++ targetDescription config
            ++ " ("
            ++ formatInt uiInitialStmts
            ++ " stmts · "
            ++ formatSize uiInitialBytes
            ++ " · "
            ++ T.unpack uiDialect
            ++ ")"
        )
    hFlush stderr

initialProgressInfo :: UIConfig -> ProgressInfo
initialProgressInfo UIConfig {..} =
    ProgressInfo
        { piPhase = "slicing"
        , piCurrentStmts = uiInitialStmts
        , piInitialStmts = uiInitialStmts
        , piCurrentBytes = uiInitialBytes
        , piTotalRuns = 0
        , piCacheHits = 0
        }

ttyProgressStyle :: Bool -> Style ProgressInfo
ttyProgressStyle useColor =
    defStyle
        { styleWidth = TerminalWidth 80
        , styleOnComplete = Clear
        , styleOpen = ""
        , styleClose = ""
        , styleDone = '━'
        , styleCurrent = '╺'
        , styleTodo = '─'
        , stylePrefix = Label $ \p _ ->
            let verb = stageVerb (piPhase (progressCustom p))
             in TL.pack (renderAction useColor Cyan verb)
        , stylePostfix = Label $ \p timing ->
            let inf = progressCustom p
                stmts = piCurrentStmts inf
                initS = piInitialStmts inf
                bytes = piCurrentBytes inf
                runs = piTotalRuns inf
                hits = piCacheHits inf
                elapsed = realToFrac (diffUTCTime (timingLastUpdate timing) (timingStart timing)) :: Double
                hitPct =
                    if runs > 0
                        then (fromIntegral hits / fromIntegral runs :: Double) * 100.0
                        else 0.0
                elapsedStr = showFFloat (Just 1) elapsed "s"
                hitStr = showFFloat (Just 0) hitPct "% hit"
                metaColor = if useColor then setSGRCode [SetColor Foreground Dull White] else ""
                resetCode = if useColor then setSGRCode [Reset] else ""
             in TL.pack
                    ( "  "
                        ++ formatInt stmts
                        ++ "/"
                        ++ formatInt initS
                        ++ " stmts ("
                        ++ formatSize bytes
                        ++ ")"
                        ++ metaColor
                        ++ " · "
                        ++ formatInt runs
                        ++ " runs · "
                        ++ hitStr
                        ++ " · "
                        ++ elapsedStr
                        ++ resetCode
                    )
        , styleEscapePrefix = const TL.empty
        , styleEscapeOpen = const TL.empty
        , styleEscapeClose = const TL.empty
        , styleEscapePostfix = const TL.empty
        , styleEscapeDone =
            if useColor
                then const (TL.pack (setSGRCode [SetColor Foreground Vivid Green]))
                else const TL.empty
        , styleEscapeCurrent =
            if useColor
                then const (TL.pack (setSGRCode [SetColor Foreground Vivid Yellow]))
                else const TL.empty
        , styleEscapeTodo =
            if useColor
                then const (TL.pack (setSGRCode [SetColor Foreground Dull Black]))
                else const TL.empty
        }

-- | TTY renderer with the library progress bar on stderr.
newTTYUI :: UIConfig -> IO UI
newTTYUI config@UIConfig {..} = do
    printReductionHeader useColor config
    let pbTodo = max 1 (uiInitialStmts + 1)
        pbStyle = ttyProgressStyle useColor
    pb <- hNewProgressBar stderr pbStyle 10.0 (Progress 0 pbTodo (initialProgressInfo config))
    let finish summary = do
            updateProgress pb (\p -> p {progressDone = progressTodo p})
            printSummary useColor summary
    pure UI {uiOnProgress = updateProgress pb . applyTTYProgress uiInitialStmts, uiFinish = finish}
    where
        useColor = uiUseColor

applyTTYProgress :: Int -> EvaluationProgress -> Progress ProgressInfo -> Progress ProgressInfo
applyTTYProgress _ (ProgressStage newStage) p =
    p {progressCustom = (progressCustom p) {piPhase = newStage}}
applyTTYProgress _ (ProgressCandidateTested callIndex cacheHit _accepted) p =
    p {progressCustom = inf {piTotalRuns = callIndex, piCacheHits = hits'}}
    where
        inf = progressCustom p
        hits' = if cacheHit then piCacheHits inf + 1 else piCacheHits inf
applyTTYProgress initialStmts (ProgressCandidateAccepted newBytes mNewStmts) p =
    p
        { progressDone = max (progressDone p) (initialStmts - newStmts)
        , progressCustom =
            inf
                { piCurrentStmts = min oldStmts newStmts
                , piCurrentBytes = min (piCurrentBytes inf) newBytes
                }
        }
    where
        inf = progressCustom p
        oldStmts = piCurrentStmts inf
        newStmts = fromMaybe oldStmts mNewStmts

-- | Non-TTY renderer that prints one line per accepted shrink.
newNonTTYUI :: UIConfig -> IO UI
newNonTTYUI config@UIConfig {..} = do
    printReductionHeader False config

    stateRef <-
        newIORef
            NonTTYState
                { ntPhase = "slicing"
                , ntStmts = uiInitialStmts
                , ntBytes = uiInitialBytes
                }

    let handleProgress = \case
            ProgressStage newStage ->
                modifyIORef' stateRef (\s -> s {ntPhase = newStage})
            ProgressCandidateTested {} ->
                pure ()
            ProgressCandidateAccepted newBytes mNewStmts -> do
                oldBytes <- ntBytes <$> readIORef stateRef
                when (newBytes < oldBytes) do
                    oldStmts <- ntStmts <$> readIORef stateRef
                    let newStmts = fromMaybe oldStmts mNewStmts
                    modifyIORef' stateRef (\s -> s {ntStmts = newStmts, ntBytes = newBytes})
                    phase <- ntPhase <$> readIORef stateRef
                    hPutStrLn
                        stderr
                        ( renderAction False Cyan (stageVerb phase)
                            ++ formatInt newStmts
                            ++ "/"
                            ++ formatInt uiInitialStmts
                            ++ " stmts ("
                            ++ formatSize newBytes
                            ++ ")"
                        )
                    hFlush stderr

        finish = printSummary False

    pure UI {uiOnProgress = handleProgress, uiFinish = finish}

printSummary :: Bool -> FinishSummary -> IO ()
printSummary useColor summary = do
    let metrics = finishMetrics summary
        initBytes = finishInitialBytes summary
        redBytes = finishReducedBytes summary
        initStmts = finishInitialStmts summary
        redStmts = finishReducedStmts summary
        minimalPath = finishMinimalPath summary
        statsPath = finishStatsPath summary
        stmtPct =
            if initStmts > 0
                then (1.0 - (fromIntegral redStmts / fromIntegral initStmts :: Double)) * 100.0
                else 0.0
        bytePct =
            if initBytes > 0
                then (1.0 - (fromIntegral redBytes / fromIntegral initBytes :: Double)) * 100.0
                else 0.0
        totHits = metrics.cacheHits
        totMisses = metrics.cacheMisses
        totLookups = totHits + totMisses
        hitRate =
            if totLookups > 0
                then (fromIntegral totHits / fromIntegral totLookups :: Double) * 100.0
                else 0.0
        totalRuns = metrics.oracleAttempts
        elapsed = metrics.totalSeconds
        isSuccess = metrics.runStatus == EvaluationCompleted && metrics.validationPassed
        statusTag = case metrics.runStatus of
            EvaluationCompleted
                | metrics.validationPassed -> ""
                | otherwise -> " (validation failed)"
            EvaluationTimeout -> " (timeout)"
            EvaluationExecutionBudget -> " (budget exceeded)"
            EvaluationError -> " (runtime error)"
            EvaluationValidationFailed -> " (validation failed)"
            EvaluationValidationIncomplete -> " (validation incomplete)"

    let greenBold =
            if useColor
                then setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green]
                else ""
        dullWhite = if useColor then setSGRCode [SetColor Foreground Dull White] else ""
        resetCode = if useColor then setSGRCode [Reset] else ""

    if isSuccess
        then do
            -- Line 1: Results
            hPutStrLn
                stderr
                ( renderAction useColor Green "Reduced"
                    ++ formatInt initStmts
                    ++ " → "
                    ++ formatInt redStmts
                    ++ " stmts ("
                    ++ greenBold
                    ++ "-"
                    ++ formatRatio stmtPct
                    ++ resetCode
                    ++ ") · "
                    ++ formatSize initBytes
                    ++ " → "
                    ++ formatSize redBytes
                    ++ " ("
                    ++ greenBold
                    ++ "-"
                    ++ formatRatio bytePct
                    ++ resetCode
                    ++ ")"
                )
            -- Line 2: Details and output target
            let statsInfo = if null statsPath then "" else dullWhite ++ " (stats: " ++ statsPath ++ ")" ++ resetCode
                destInfo = if null minimalPath then "" else " → " ++ minimalPath ++ statsInfo
            hPutStrLn
                stderr
                ( renderAction useColor Green "Finished"
                    ++ "in "
                    ++ showFFloat (Just 1) elapsed "s ("
                    ++ formatInt totalRuns
                    ++ " runs · "
                    ++ formatRatio hitRate
                    ++ " cache hit)"
                    ++ destInfo
                )
        else do
            hPutStrLn
                stderr
                ( renderAction useColor Red "Failed"
                    ++ formatInt initStmts
                    ++ " stmts ("
                    ++ formatSize initBytes
                    ++ ")"
                    ++ statusTag
                )
    hFlush stderr
