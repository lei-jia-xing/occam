module Occam.Logger (
    LogLevel (..),
    LogFormat (..),
    LogConfig (..),
    Logger (..),
    defaultLogConfig,
    resolveLogConfig,
    logDebug,
    logInfo,
    logWarn,
    logError,
    logDebug',
    logInfo',
    logWarn',
    logError',
    nullLogger,
    withLogger,
    newFastLogger,
) where

import Control.Exception (bracket, finally)
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON (..), Value (String), encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import System.Console.ANSI (
    Color (..),
    ColorIntensity (..),
    ConsoleIntensity (..),
    ConsoleLayer (..),
    SGR (..),
    hSupportsANSI,
    setSGRCode,
 )
import System.Environment (lookupEnv)
import System.IO (stderr)
import System.Log.FastLogger (
    FormattedTime,
    LogStr,
    LoggerSet,
    defaultBufSize,
    flushLogStr,
    newStderrLoggerSet,
    newTimeCache,
    pushLogStrLn,
    rmLoggerSet,
    toLogStr,
 )

data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError
    deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON LogLevel where
    toJSON LevelDebug = "debug"
    toJSON LevelInfo = "info"
    toJSON LevelWarn = "warn"
    toJSON LevelError = "error"

instance FromJSON LogLevel

data LogFormat = FormatText | FormatJSON
    deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

data LogConfig = LogConfig
    { logMinLevel :: !LogLevel
    , logFormat :: !LogFormat
    , logUseColor :: !Bool
    }
    deriving (Eq, Show)

defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig LevelInfo FormatText True

data Logger = Logger
    { loggerConfig :: !LogConfig
    , loggerAction :: !(LogLevel -> Text -> Text -> [(Text, Value)] -> IO ())
    , loggerFlush :: !(IO ())
    }

nullLogger :: Logger
nullLogger = Logger defaultLogConfig (\_ _ _ _ -> pure ()) (pure ())

data LogRecord = LogRecord
    { timestamp :: !Text
    , level :: !LogLevel
    , component :: !Text
    , message :: !Text
    , metadata :: !Value
    }
    deriving (Generic, ToJSON)

logDebug, logInfo, logWarn, logError :: Logger -> Text -> Text -> IO ()
logDebug l c m = logDebug' l c m []
logInfo l c m = logInfo' l c m []
logWarn l c m = logWarn' l c m []
logError l c m = logError' l c m []

logDebug', logInfo', logWarn', logError' :: Logger -> Text -> Text -> [(Text, Value)] -> IO ()
logDebug' l = logAt l LevelDebug
logInfo' l = logAt l LevelInfo
logWarn' l = logAt l LevelWarn
logError' l = logAt l LevelError

logAt :: Logger -> LogLevel -> Text -> Text -> [(Text, Value)] -> IO ()
logAt Logger {..} lvl comp msg meta =
    when (logMinLevel loggerConfig <= lvl) $ loggerAction lvl comp msg meta

resolveLogConfig :: LogConfig -> IO LogConfig
resolveLogConfig cfg = do
    supportsColor <- (&&) <$> hSupportsANSI stderr <*> (isNothing <$> lookupEnv "NO_COLOR")
    pure cfg {logUseColor = logUseColor cfg && supportsColor}

withLogger :: LogConfig -> (Logger -> IO a) -> IO a
withLogger rawCfg action = do
    cfg <- resolveLogConfig rawCfg
    timeCache <- newTimeCache "%Y-%m-%d %H:%M:%S"
    bracket (newStderrLoggerSet defaultBufSize) rmLoggerSet \lset -> do
        let logger = newFastLogger cfg timeCache lset
        action logger `finally` loggerFlush logger

newFastLogger :: LogConfig -> IO FormattedTime -> LoggerSet -> Logger
newFastLogger cfg@LogConfig {..} getTime lset =
    Logger
        { loggerConfig = cfg
        , loggerAction = \lvl comp msg meta -> do
            now <- getTime
            pushLogStrLn lset $ case logFormat of
                FormatJSON -> formatJson now lvl comp msg meta
                FormatText -> formatText logUseColor now lvl comp msg meta
        , loggerFlush = flushLogStr lset
        }

formatJson :: FormattedTime -> LogLevel -> Text -> Text -> [(Text, Value)] -> LogStr
formatJson now lvl comp msg meta =
    toLogStr $
        encode $
            LogRecord (TE.decodeUtf8Lenient now) lvl comp msg (object [Key.fromText k .= v | (k, v) <- meta])

formatText :: Bool -> FormattedTime -> LogLevel -> Text -> Text -> [(Text, Value)] -> LogStr
formatText useColor now lvl comp msg meta =
    toLogStr now
        <> " "
        <> formatBadge useColor lvl
        <> " "
        <> toLogStr (if T.null comp then "" else "[" <> comp <> "] ")
        <> toLogStr msg
        <> toLogStr (renderMeta meta)

formatBadge :: Bool -> LogLevel -> LogStr
formatBadge False lvl = plainBadge lvl
formatBadge True lvl = toLogStr (setSGRCode (levelSGR lvl)) <> plainBadge lvl <> toLogStr (setSGRCode [Reset])

plainBadge :: LogLevel -> LogStr
plainBadge LevelDebug = "DEBUG"
plainBadge LevelInfo = "INFO"
plainBadge LevelWarn = "WARN"
plainBadge LevelError = "ERROR"

levelSGR :: LogLevel -> [SGR]
levelSGR LevelDebug = [SetColor Foreground Vivid Cyan]
levelSGR LevelInfo = [SetColor Foreground Vivid Green]
levelSGR LevelWarn = [SetColor Foreground Vivid Yellow]
levelSGR LevelError = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]

renderMeta :: [(Text, Value)] -> Text
renderMeta [] = ""
renderMeta ms = " (" <> T.intercalate ", " [k <> "=" <> renderValue v | (k, v) <- ms] <> ")"

renderValue :: Value -> Text
renderValue (String s) = s
renderValue v = TE.decodeUtf8Lenient (BSL.toStrict (encode v))
