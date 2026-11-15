{- |
Module      : Occam.UI
Description : Pragmatic, Unix-native terminal progress and summary rendering
License     : MIT
Maintainer  : leij49456@gmail.com

Provides single-line dynamic progress rendering on interactive terminals (TTY)
using standard 'terminal-progress-bar' and 'ansi-terminal' libraries, and discrete
milestone reporting when redirected or in non-TTY environments.
All diagnostics strictly write to stderr, keeping stdout clean for pipeline composability.
-}
module Occam.UI (
    UIConfig (..),
    UI (..),
    newUI,
    formatSize,
    formatRatio,
) where

import Control.Monad (unless, when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe, isNothing)
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
    hSetSGR,
    hSupportsANSI,
    setSGRCode,
 )
import System.Environment (lookupEnv)
import System.IO (hFlush, hPutStr, hPutStrLn, stderr)
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
    , _ntBytes :: !Int
    }

data UI = UI
    { uiOnProgress :: !(EvaluationProgress -> IO ())
    , uiFinish :: !(EvaluationMetrics -> Int -> Int -> Int -> Int -> FilePath -> FilePath -> IO ())
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

-- | Initialize the UI, detecting TTY capabilities and using library progress rendering.
newUI :: UIConfig -> IO UI
newUI UIConfig {..} = do
    isTTY <- hSupportsANSI stderr
    noColorEnv <- lookupEnv "NO_COLOR"
    let useColor = isTTY && isNothing noColorEnv && uiUseColor
        targetDesc = if uiTargetFile == "-" then "<stdin>" else uiTargetFile

    if uiQuiet || uiVerbose
        then do
            pure
                UI
                    { uiOnProgress = const (pure ())
                    , uiFinish = \_ _ _ _ _ _ _ -> pure ()
                    }
        else
            if isTTY
                then do
                    when useColor (hSetSGR stderr [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Cyan])
                    hPutStr stderr "occam: "
                    when useColor (hSetSGR stderr [Reset])
                    hPutStrLn
                        stderr
                        ( "reducing "
                            ++ targetDesc
                            ++ " ("
                            ++ show uiInitialStmts
                            ++ " stmts, "
                            ++ formatSize uiInitialBytes
                            ++ ", "
                            ++ T.unpack uiDialect
                            ++ ")"
                        )
                    hFlush stderr

                    let pbTodo = max 1 (uiInitialStmts + 1)
                        initInfo =
                            ProgressInfo
                                { piPhase = "slicing"
                                , piCurrentStmts = uiInitialStmts
                                , piInitialStmts = uiInitialStmts
                                , piCurrentBytes = uiInitialBytes
                                , piTotalRuns = 0
                                , piCacheHits = 0
                                }
                        pbStyle =
                            defStyle
                                { styleWidth = TerminalWidth 80
                                , styleOnComplete = Clear
                                , styleTodo = ' '
                                , stylePrefix = Label $ \p _ ->
                                    TL.fromStrict ("occam: [" <> piPhase (progressCustom p) <> "]")
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
                                        hitStr = showFFloat (Just 0) hitPct "% hits"
                                     in TL.pack
                                            ( show stmts
                                                ++ "/"
                                                ++ show initS
                                                ++ " stmts ("
                                                ++ formatSize bytes
                                                ++ ") | #"
                                                ++ show runs
                                                ++ " ("
                                                ++ elapsedStr
                                                ++ ", "
                                                ++ hitStr
                                                ++ ")"
                                            )
                                , styleEscapePrefix =
                                    if useColor
                                        then
                                            const (TL.pack (setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Cyan]))
                                        else const TL.empty
                                , styleEscapeOpen =
                                    if useColor
                                        then const (TL.pack (setSGRCode [Reset, SetColor Foreground Dull White]))
                                        else const TL.empty
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
                                        then const (TL.pack (setSGRCode [Reset]))
                                        else const TL.empty
                                , styleEscapeClose =
                                    if useColor
                                        then const (TL.pack (setSGRCode [Reset, SetColor Foreground Dull White]))
                                        else const TL.empty
                                , styleEscapePostfix =
                                    if useColor
                                        then const (TL.pack (setSGRCode [Reset]))
                                        else const TL.empty
                                }
                    pb <- hNewProgressBar stderr pbStyle 20.0 (Progress 0 pbTodo initInfo)

                    let handleProgress = \case
                            ProgressStage newStage ->
                                updateProgress pb (\p -> p {progressCustom = (progressCustom p) {piPhase = newStage}})
                            ProgressCandidateTested callIndex cacheHit _accepted ->
                                updateProgress
                                    pb
                                    ( \p ->
                                        let inf = progressCustom p
                                            hits' = if cacheHit then piCacheHits inf + 1 else piCacheHits inf
                                         in p {progressCustom = inf {piTotalRuns = callIndex, piCacheHits = hits'}}
                                    )
                            ProgressCandidateAccepted newBytes mNewStmts ->
                                updateProgress
                                    pb
                                    ( \p ->
                                        let inf = progressCustom p
                                            oldStmts = piCurrentStmts inf
                                            newStmts = fromMaybe oldStmts mNewStmts
                                            doneCount = max (progressDone p) (uiInitialStmts - newStmts)
                                         in p
                                                { progressDone = doneCount
                                                , progressCustom =
                                                    inf
                                                        { piCurrentStmts = min oldStmts newStmts
                                                        , piCurrentBytes = min (piCurrentBytes inf) newBytes
                                                        }
                                                }
                                    )

                        finish metrics initBytes redBytes initStmts redStmts minPath stPath = do
                            updateProgress pb (\p -> p {progressDone = progressTodo p})
                            printSummary useColor metrics initBytes redBytes initStmts redStmts minPath stPath

                    pure UI {uiOnProgress = handleProgress, uiFinish = finish}
                else do
                    -- Non-TTY fallback
                    hPutStrLn
                        stderr
                        ( "occam: reducing "
                            ++ targetDesc
                            ++ " ("
                            ++ show uiInitialStmts
                            ++ " stmts, "
                            ++ formatSize uiInitialBytes
                            ++ ", "
                            ++ T.unpack uiDialect
                            ++ ")"
                        )
                    hFlush stderr

                    stateRef <-
                        newIORef
                            NonTTYState
                                { ntPhase = "slicing"
                                , ntStmts = uiInitialStmts
                                , _ntBytes = uiInitialBytes
                                }

                    let handleProgress = \case
                            ProgressStage newStage ->
                                modifyIORef' stateRef (\s -> s {ntPhase = newStage})
                            ProgressCandidateTested {} ->
                                pure ()
                            ProgressCandidateAccepted newBytes mNewStmts -> do
                                oldBytes <- _ntBytes <$> readIORef stateRef
                                when (newBytes < oldBytes) do
                                    oldStmts <- ntStmts <$> readIORef stateRef
                                    let newStmts = fromMaybe oldStmts mNewStmts
                                    modifyIORef' stateRef (\s -> s {ntStmts = newStmts, _ntBytes = newBytes})
                                    phase <- ntPhase <$> readIORef stateRef
                                    hPutStrLn
                                        stderr
                                        ( "occam: "
                                            ++ T.unpack phase
                                            ++ ": -> "
                                            ++ show newStmts
                                            ++ " stmts ("
                                            ++ formatSize newBytes
                                            ++ ")"
                                        )
                                    hFlush stderr

                        finish = printSummary False

                    pure UI {uiOnProgress = handleProgress, uiFinish = finish}

printSummary ::
    Bool -> EvaluationMetrics -> Int -> Int -> Int -> Int -> FilePath -> FilePath -> IO ()
printSummary useColor metrics initBytes redBytes initStmts redStmts minimalPath statsPath = do
    let stmtPct =
            if initStmts > 0
                then (1.0 - (fromIntegral redStmts / fromIntegral initStmts :: Double)) * 100.0
                else 0.0
        bytePct =
            if initBytes > 0
                then (1.0 - (fromIntegral redBytes / fromIntegral initBytes :: Double)) * 100.0
                else 0.0
        totHits = evaluationCacheHits metrics
        totMisses = evaluationCacheMisses metrics
        totLookups = totHits + totMisses
        hitRate =
            if totLookups > 0
                then (fromIntegral totHits / fromIntegral totLookups :: Double) * 100.0
                else 0.0
        totalRuns = evaluationOracleAttempts metrics
        elapsed = evaluationTotalSeconds metrics
        isSuccess = evaluationStatus metrics == EvaluationCompleted && evaluationValidationPassed metrics
        statusTag = case evaluationStatus metrics of
            EvaluationCompleted
                | evaluationValidationPassed metrics -> ""
                | otherwise -> "[ValidationFailed] "
            EvaluationTimeout -> "[Timeout] "
            EvaluationExecutionBudget -> "[BudgetExceeded] "
            EvaluationError -> "[Error] "
            EvaluationValidationFailed -> "[ValidationFailed] "
            EvaluationValidationIncomplete -> "[ValidationIncomplete] "

    when useColor (hSetSGR stderr [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Cyan])
    hPutStr stderr "occam: "
    if isSuccess
        then do
            when
                useColor
                (hSetSGR stderr [Reset, SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green])
            hPutStr stderr "✓ "
        else do
            when
                useColor
                (hSetSGR stderr [Reset, SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red])
            hPutStr stderr ("✗ " ++ statusTag)
    when useColor (hSetSGR stderr [Reset])
    hPutStrLn
        stderr
        ( "reduced "
            ++ show initStmts
            ++ " -> "
            ++ show redStmts
            ++ " stmts (-"
            ++ formatRatio stmtPct
            ++ "), "
            ++ formatSize initBytes
            ++ " -> "
            ++ formatSize redBytes
            ++ " (-"
            ++ formatRatio bytePct
            ++ ") in "
            ++ showFFloat (Just 1) elapsed "s ["
            ++ show totalRuns
            ++ " runs, "
            ++ formatRatio hitRate
            ++ " cache hits]"
        )
    unless (null minimalPath) do
        when useColor (hSetSGR stderr [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Cyan])
        hPutStr stderr "occam: "
        when useColor (hSetSGR stderr [Reset])
        hPutStrLn
            stderr
            ( "output: "
                ++ minimalPath
                ++ (if null statsPath then "" else " (stats: " ++ statsPath ++ ")")
            )
    hFlush stderr
