module Occam.Logger (
    LogLevel (..),
    LogFormat (..),
    LogConfig (..),
    Logger (..),
    defaultLogConfig,
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

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON (..), Value (..), encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import System.IO (hIsTerminalDevice, stdout)
import System.Log.FastLogger (
    LoggerSet,
    defaultBufSize,
    flushLogStr,
    newStdoutLoggerSet,
    pushLogStr,
    rmLoggerSet,
    toLogStr,
 )

data LogLevel
    = LevelDebug
    | LevelInfo
    | LevelWarn
    | LevelError
    deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON LogLevel where
    toJSON :: LogLevel -> Value
    toJSON LevelDebug = "debug"
    toJSON LevelInfo = "info"
    toJSON LevelWarn = "warn"
    toJSON LevelError = "error"

instance FromJSON LogLevel

data LogFormat
    = FormatText
    | FormatJSON
    deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

data LogConfig = LogConfig
    { logMinLevel :: !LogLevel
    , logFormat :: !LogFormat
    , logUseColor :: !Bool
    }
    deriving (Eq, Show)

defaultLogConfig :: LogConfig
defaultLogConfig =
    LogConfig
        { logMinLevel = LevelInfo
        , logFormat = FormatText
        , logUseColor = True
        }

data Logger = Logger
    { loggerConfig :: !LogConfig
    , loggerAction :: !(LogLevel -> Text -> Text -> [(Text, Value)] -> IO ())
    , loggerFlush :: !(IO ())
    }

nullLogger :: Logger
nullLogger =
    Logger
        { loggerConfig = defaultLogConfig
        , loggerAction = \_ _ _ _ -> pure ()
        , loggerFlush = pure ()
        }

logDebug :: Logger -> Text -> Text -> IO ()
logDebug l comp msg = logDebug' l comp msg []

logInfo :: Logger -> Text -> Text -> IO ()
logInfo l comp msg = logInfo' l comp msg []

logWarn :: Logger -> Text -> Text -> IO ()
logWarn l comp msg = logWarn' l comp msg []

logError :: Logger -> Text -> Text -> IO ()
logError l comp msg = logError' l comp msg []

logDebug' :: Logger -> Text -> Text -> [(Text, Value)] -> IO ()
logDebug' Logger {..} comp msg meta =
    when (logMinLevel loggerConfig <= LevelDebug) $
        loggerAction LevelDebug comp msg meta

logInfo' :: Logger -> Text -> Text -> [(Text, Value)] -> IO ()
logInfo' Logger {..} comp msg meta =
    when (logMinLevel loggerConfig <= LevelInfo) $
        loggerAction LevelInfo comp msg meta

logWarn' :: Logger -> Text -> Text -> [(Text, Value)] -> IO ()
logWarn' Logger {..} comp msg meta =
    when (logMinLevel loggerConfig <= LevelWarn) $
        loggerAction LevelWarn comp msg meta

logError' :: Logger -> Text -> Text -> [(Text, Value)] -> IO ()
logError' Logger {..} comp msg meta =
    when (logMinLevel loggerConfig <= LevelError) $
        loggerAction LevelError comp msg meta

-- | Bracket-managed high-performance logger using fast-logger.
withLogger :: LogConfig -> (Logger -> IO a) -> IO a
withLogger cfg action = do
    isTTY <- hIsTerminalDevice stdout
    noColor <- lookupEnv "NO_COLOR"
    let colorSupported = isTTY && isNothing noColor && logUseColor cfg
        resolvedCfg = cfg {logUseColor = colorSupported}
    bracket (newStdoutLoggerSet defaultBufSize) rmLoggerSet \lset -> do
        logger <- newFastLogger resolvedCfg lset
        res <- action logger
        loggerFlush logger
        pure res

newFastLogger :: LogConfig -> LoggerSet -> IO Logger
newFastLogger cfg@LogConfig {..} lset = do
    pure
        Logger
            { loggerConfig = cfg
            , loggerAction = \lvl comp msg meta -> do
                now <- getCurrentTime
                let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
                case logFormat of
                    FormatJSON -> do
                        let jsonPayload =
                                object
                                    [ "timestamp" .= timeStr
                                    , "level" .= lvl
                                    , "component" .= comp
                                    , "message" .= msg
                                    , "metadata" .= object [Key.fromText k .= v | (k, v) <- meta]
                                    ]
                            bs = BSL.toStrict (encode jsonPayload) <> "\n"
                        pushLogStr lset (toLogStr bs)
                    FormatText -> do
                        let badge = formatBadge logUseColor lvl
                            compBadge =
                                if T.null comp
                                    then ""
                                    else "[" <> comp <> "] "
                            metaStr =
                                if null meta
                                    then ""
                                    else
                                        " ("
                                            <> T.intercalate ", " [k <> "=" <> renderValue v | (k, v) <- meta]
                                            <> ")"
                            line =
                                BSC.pack timeStr
                                    <> " "
                                    <> badge
                                    <> " "
                                    <> TE.encodeUtf8 compBadge
                                    <> TE.encodeUtf8 msg
                                    <> TE.encodeUtf8 metaStr
                                    <> "\n"
                        pushLogStr lset (toLogStr line)
            , loggerFlush = flushLogStr lset
            }

renderValue :: Value -> Text
renderValue (String s) = s
renderValue (Number n) = T.pack (show n)
renderValue (Bool b) = if b then "true" else "false"
renderValue Null = "null"
renderValue other = TE.decodeUtf8Lenient (BSL.toStrict (encode other))

formatBadge :: Bool -> LogLevel -> BSC.ByteString
formatBadge True LevelDebug = "\ESC[36m[DEBUG]\ESC[0m"
formatBadge True LevelInfo = "\ESC[32m[INFO ]\ESC[0m"
formatBadge True LevelWarn = "\ESC[33m[WARN ]\ESC[0m"
formatBadge True LevelError = "\ESC[1;31m[ERROR]\ESC[0m"
formatBadge False LevelDebug = "[DEBUG]"
formatBadge False LevelInfo = "[INFO ]"
formatBadge False LevelWarn = "[WARN ]"
formatBadge False LevelError = "[ERROR]"
