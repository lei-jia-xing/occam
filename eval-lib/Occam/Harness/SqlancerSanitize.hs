-- | Deterministic sanitizer turning SQLancer crash logs into replayable SQL + metadata.
module Occam.Harness.SqlancerSanitize (
    SqlancerMeta (..),
    SqlancerSanitizeResult (..),
    emptySqlancerMeta,
    metaToObject,
    sanitizeSqlancerLog,
) where

import Control.Applicative ((<|>))
import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace, toLower)
import Data.Foldable qualified as Foldable
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE

-- | Extracted header comments preserved from a SQLancer log.
data SqlancerMeta = SqlancerMeta
    { metaTime :: !(Maybe Text)
    , metaDatabase :: !(Maybe Text)
    , metaDatabaseVersion :: !(Maybe Text)
    , metaSeedValue :: !(Maybe Text)
    }
    deriving (Eq, Show)

instance ToJSON SqlancerMeta where
    toJSON = Object . metaToObject

-- | Convert metadata into a compact JSON object omitting unset keys.
metaToObject :: SqlancerMeta -> KM.KeyMap Value
metaToObject meta =
    KM.fromList . catMaybes $
        [ ("time",) . String <$> metaTime meta
        , ("database",) . String <$> metaDatabase meta
        , ("database_version",) . String <$> metaDatabaseVersion meta
        , ("seed_value",) . String <$> metaSeedValue meta
        ]

-- | Result of sanitizing one SQLancer @.log@ or @-cur.log@ byte payload.
data SqlancerSanitizeResult = SqlancerSanitizeResult
    { sanitizedSql :: !BS.ByteString
    , sanitizedMeta :: !SqlancerMeta
    }
    deriving (Eq, Show)

instance ToJSON SqlancerSanitizeResult where
    toJSON :: SqlancerSanitizeResult -> Value
    toJSON res =
        object
            [ "sql" .= TE.decodeUtf8With TEE.lenientDecode (sanitizedSql res)
            , "metadata" .= sanitizedMeta res
            ]

-- | Empty metadata record.
emptySqlancerMeta :: SqlancerMeta
emptySqlancerMeta = SqlancerMeta Nothing Nothing Nothing Nothing

-- | Strip Java stacktraces, @\\c@ commands, and ephemeral DB lifecycle statements while extracting header metadata.
sanitizeSqlancerLog :: BS.ByteString -> SqlancerSanitizeResult
sanitizeSqlancerLog raw =
    let linesList = splitLogLines (TE.decodeUtf8With TEE.lenientDecode raw)
        meta = Foldable.foldl' collectMeta emptySqlancerMeta linesList
        loggedDb = metaDatabase meta
        cleaned = filter (not . shouldDropLine loggedDb . T.strip) linesList
        outText = T.strip (T.intercalate "\n" cleaned)
        finalText
            | T.null outText = ""
            | otherwise = outText <> "\n"
     in SqlancerSanitizeResult
            { sanitizedSql = TE.encodeUtf8 finalText
            , sanitizedMeta = meta
            }

splitLogLines :: Text -> [Text]
splitLogLines raw
    | T.null raw = []
    | otherwise =
        let normalized = T.replace "\r\n" "\n" raw
            crNormalized = T.replace "\r" "\n" normalized
            withoutTrailing = fromMaybe crNormalized (T.stripSuffix "\n" crNormalized)
         in T.splitOn "\n" withoutTrailing

collectMeta :: SqlancerMeta -> Text -> SqlancerMeta
collectMeta meta rawLine =
    case matchMetaLine (T.strip rawLine) of
        Just ("time", v) -> meta {metaTime = Just v}
        Just ("database", v) -> meta {metaDatabase = Just v}
        Just ("database_version", v) -> meta {metaDatabaseVersion = Just v}
        Just ("seed_value", v) -> meta {metaSeedValue = Just v}
        _ -> meta

matchMetaLine :: Text -> Maybe (Text, Text)
matchMetaLine stripped = do
    rest0 <- T.stripPrefix "--" stripped
    let rest1 = T.dropWhile (== '-') rest0
        afterSpace = T.dropWhile isSpace rest1
    (key, rawVal) <- matchMetaKey afterSpace
    let val = T.strip rawVal
    if T.null val then Nothing else Just (key, val)
    where
        matchMetaKey txt =
            tryKey "Time:" "time" txt
                <|> tryKey "Database version:" "database_version" txt
                <|> tryKey "Database:" "database" txt
                <|> tryKey "seed value:" "seed_value" txt

        tryKey prefix canonical txt = do
            afterColon <- stripPrefixCI prefix txt
            let afterWs = T.dropWhile isSpace afterColon
            pure (canonical, afterWs)

shouldDropLine :: Maybe Text -> Text -> Bool
shouldDropLine loggedDb stripped =
    isJavaStackLine stripped
        || isPsqlConnect stripped
        || isDbLifecycle loggedDb stripped

isPsqlConnect :: Text -> Bool
isPsqlConnect stripped = case stripPrefixCI "\\c" stripped of
    Nothing -> False
    Just rest -> case T.uncons rest of
        Just (c, rest') -> isSpace c && T.any (not . isSpace) rest'
        Nothing -> False

isJavaStackLine :: Text -> Bool
isJavaStackLine stripped =
    let body = case T.stripPrefix "--" stripped of
            Just rest -> T.dropWhile isSpace rest
            Nothing -> stripped
     in matchesPackageException body
            || matchesExceptionInThread body
            || matchesAtFrame body
            || matchesCausedOrSuppressed body
            || matchesEllipsisMore body

matchesPackageException :: Text -> Bool
matchesPackageException body = case findPrefix pkgPrefixes body of
    Nothing -> False
    Just rest ->
        let ident = T.takeWhile isPkgIdentChar rest
         in not (T.null ident) && T.any isWordChar ident
    where
        pkgPrefixes =
            [ "java."
            , "javax."
            , "jdk."
            , "com."
            , "org."
            , "ru."
            , "io."
            , "net."
            , "ch."
            , "sqlancer."
            ]
        findPrefix [] _ = Nothing
        findPrefix (p : ps) t = T.stripPrefix p t <|> findPrefix ps t
        isPkgIdentChar c = isWordChar c || c == '.' || c == '$'

matchesExceptionInThread :: Text -> Bool
matchesExceptionInThread body = fromBool do
    afterKw <- T.stripPrefix "Exception in thread" body
    afterWs1 <- skipWs1 afterKw
    afterOpenQ <- T.stripPrefix "\"" afterWs1
    let (threadName, restQ) = T.break (== '"') afterOpenQ
    if T.null threadName
        then Nothing
        else do
            afterCloseQ <- T.stripPrefix "\"" restQ
            skipWs1 afterCloseQ

matchesAtFrame :: Text -> Bool
matchesAtFrame body = fromBool do
    afterAt <- T.stripPrefix "at" body
    afterWs <- skipWs1 afterAt
    let (beforeParen, parenPart) = T.break (== '(') afterWs
    afterOpenP <- T.stripPrefix "(" parenPart
    let (_inside, restP) = T.break (== ')') afterOpenP
    if restP /= ")" || not (validTarget beforeParen)
        then Nothing
        else Just ()
    where
        validTarget t
            | T.null t = False
            | T.any (== '/') t =
                let (modPart, slashAndMethod) = T.break (== '/') t
                    methodPart = T.dropWhile (== '/') slashAndMethod
                 in not (T.null modPart)
                        && T.all isModChar modPart
                        && not (T.null slashAndMethod)
                        && not (T.null methodPart)
                        && T.all isMethodChar methodPart
            | otherwise = T.all isMethodChar t

        isModChar c = isWordChar c || c == '.' || c == '-'
        isMethodChar c = isWordChar c || c `elem` ['.', '$', '<', '>', '-']

matchesCausedOrSuppressed :: Text -> Bool
matchesCausedOrSuppressed body = fromBool do
    afterPrefix <- T.stripPrefix "Caused by:" body <|> T.stripPrefix "Suppressed:" body
    skipWs1 afterPrefix

matchesEllipsisMore :: Text -> Bool
matchesEllipsisMore body = fromBool do
    afterDots <- T.stripPrefix "..." body
    afterWs1 <- skipWs1 afterDots
    let digits = T.takeWhile isDigit afterWs1
        afterDigits = T.drop (T.length digits) afterWs1
    if T.null digits
        then Nothing
        else do
            afterWs2 <- skipWs1 afterDigits
            if afterWs2 == "more" then Just () else Nothing

isDbLifecycle :: Maybe Text -> Text -> Bool
isDbLifecycle loggedDb stripped =
    matchesDropDb loggedDb stripped
        || matchesCreateDb loggedDb stripped
        || matchesUseDb loggedDb stripped

matchesDropDb :: Maybe Text -> Text -> Bool
matchesDropDb loggedDb txt = fromBool do
    rest1 <- eatKeywordCI "drop" txt
    rest2 <- eatKeywordCI "database" rest1
    let rest3 = fromMaybe rest2 (eatKeywordCI "force" rest2)
        rest4 = fromMaybe rest3 (eatKeywordCI "if" rest3 >>= eatKeywordCI "exists")
    afterDb <- eatDbPattern loggedDb rest4
    consumeOptionalSemicolonEnd afterDb

matchesCreateDb :: Maybe Text -> Text -> Bool
matchesCreateDb loggedDb txt = fromBool do
    rest1 <- eatKeywordCI "create" txt
    rest2 <- eatKeywordCI "database" rest1
    let rest3 = fromMaybe rest2 (eatKeywordCI "if" rest2 >>= eatKeywordCI "not" >>= eatKeywordCI "exists")
    afterDb <- eatDbPattern loggedDb rest3
    consumeCreateTail afterDb
    where
        consumeCreateTail tailTxt
            | T.null tailTxt = Just ()
            | tailTxt == ";" = Just ()
            | otherwise = case T.uncons tailTxt of
                Just (c, rest) | isSpace c -> checkNoInnerSemicolons rest
                _ -> Nothing

        checkNoInnerSemicolons s =
            let body = fromMaybe s (T.stripSuffix ";" s)
             in if T.any (== ';') body then Nothing else Just ()

matchesUseDb :: Maybe Text -> Text -> Bool
matchesUseDb loggedDb txt = fromBool do
    rest1 <- eatKeywordCI "use" txt
    afterDb <- eatDbPattern loggedDb rest1
    consumeOptionalSemicolonEnd afterDb

eatKeywordCI :: Text -> Text -> Maybe Text
eatKeywordCI kw txt = do
    afterKw <- stripPrefixCI kw txt
    skipWs1 afterKw

eatDbPattern :: Maybe Text -> Text -> Maybe Text
eatDbPattern loggedDb txt =
    let txt1 = dropOptionalChar '`' txt
        txt2 = dropOptionalChar '"' txt1
     in do
            afterName <- matchLoggedDb txt2 <|> matchGeneratedDb txt2
            let afterQ1 = dropOptionalChar '`' afterName
                afterQ2 = dropOptionalChar '"' afterQ1
            pure afterQ2
    where
        dropOptionalChar ch s = fromMaybe s (T.stripPrefix (T.singleton ch) s)

        matchGeneratedDb s = do
            rest <- stripPrefixCI "database" s
            let digits = T.takeWhile isDigit rest
            if T.null digits then Nothing else Just (T.drop (T.length digits) rest)

        matchLoggedDb s = case loggedDb of
            Just dbName | not (T.null dbName) -> stripPrefixCI dbName s
            _ -> Nothing

consumeOptionalSemicolonEnd :: Text -> Maybe ()
consumeOptionalSemicolonEnd txt =
    let t = T.dropWhile isSpace txt
     in if T.null t || t == ";" then Just () else Nothing

stripPrefixCI :: Text -> Text -> Maybe Text
stripPrefixCI prefix txt
    | T.length txt < T.length prefix = Nothing
    | otherwise =
        let (hd, tl) = T.splitAt (T.length prefix) txt
         in if T.map toLower hd == T.map toLower prefix then Just tl else Nothing

skipWs1 :: Text -> Maybe Text
skipWs1 txt = case T.uncons txt of
    Just (c, _) | isSpace c -> Just (T.dropWhile isSpace txt)
    _ -> Nothing

isWordChar :: Char -> Bool
isWordChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_'

fromBool :: Maybe a -> Bool
fromBool (Just _) = True
fromBool Nothing = False
