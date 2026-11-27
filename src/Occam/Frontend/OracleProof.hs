-- | Oracle cache keys and DuckDB replay classification over the frontend tokens.
module Occam.Frontend.OracleProof (
    DuckDBReplayUnit (..),
    oracleCacheKey,
    duckdbReplayUnits,
) where

import Control.Applicative (optional, (<|>))
import Control.Monad (guard, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT (..), evalStateT, get, put)
import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Builder.Extra qualified as BSBExtra
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word8)
import Occam.Frontend.Lexer (
    Keyword (..),
    LexResult (..),
    LiteralKind (..),
    SpannedToken,
    Token (..),
    WordToken (..),
    asciiEqCI,
    isKeywordToken,
    isWhitespaceToken,
    lexSQL,
 )
import Occam.Frontend.Span (ByteSpan (..), Located (..), unsafeSliceSpan)
import Occam.Parser.Dialect (DialectProfile (..), LexicalProfile (..), duckdbProfile)
import Occam.Parser.Types (SQLDialect (..))

-- | Statement unit classified for DuckDB prefix checkpoint replay.
data DuckDBReplayUnit = DuckDBReplayUnit
    { replayUnitEnd :: !(Maybe Int)
    , replayUnitKind :: !Text
    , replayUnitTable :: !(Maybe Text)
    }
    deriving (Eq, Show)

instance FromJSON DuckDBReplayUnit where
    parseJSON = withObject "DuckDBReplayUnit" \o ->
        DuckDBReplayUnit <$> o .:? "end" <*> o .: "kind" <*> o .:? "table"

{- | Compute a whitespace-normalized, comment- and literal-preserving cache key
for single-statement literal scalar @SELECT ... AS ...@ probes on SQLite or DuckDB.
-}
oracleCacheKey :: DialectProfile -> ByteString -> Maybe ByteString
oracleCacheKey profile sql = do
    let !dialect = dialectId profile
    guard (dialect == SQLite || dialect == DuckDB)
    guard (startsWithSelect sql)
    guard (not (hasInteriorSemicolon (lexBackslashEscapesInSingleQuote (dialectLexical profile)) sql))
    guard (not (BS.elem 0 sql))
    toks <- lexNonWhitespace profile sql
    let !n = V.length toks
        !bare =
            if n > 0 && locValue (toks V.! (n - 1)) == TokSemicolon
                then V.slice 0 (n - 1) toks
                else toks
    matchScalarSelect bare
    buildCanonicalKey sql toks

-- | Classify DuckDB script statements for deterministic prefix checkpoint reuse.
duckdbReplayUnits :: ByteString -> [DuckDBReplayUnit]
duckdbReplayUnits sql
    | BS.elem 0 sql = []
    | otherwise = case lexNonWhitespace duckdbProfile sql of
        Nothing -> []
        Just toks -> splitStatements 0 0
            where
                !n = V.length toks

                splitStatements !start !end
                    | end > n = []
                    | end < n && locValue (toks V.! end) /= TokSemicolon =
                        splitStatements start (end + 1)
                    | start < end =
                        let !stmt = V.slice start (end - start) toks
                            (!kindText, !mTable) = classifyStatement stmt
                            !mEnd =
                                if end < n
                                    then Just (spanEnd (locSpan (toks V.! end)))
                                    else Nothing
                            !unit =
                                DuckDBReplayUnit
                                    { replayUnitEnd = mEnd
                                    , replayUnitKind = kindText
                                    , replayUnitTable = fmap asciiLowerText mTable
                                    }
                         in unit : splitStatements (end + 1) (end + 1)
                    | otherwise = splitStatements (end + 1) (end + 1)

lexNonWhitespace :: DialectProfile -> ByteString -> Maybe (Vector SpannedToken)
lexNonWhitespace profile sql =
    let LexResult vec mErr = lexSQL profile sql
     in case mErr of
            Just _ -> Nothing
            Nothing -> Just (V.filter (not . isWhitespaceToken . locValue) vec)

buildCanonicalKey :: ByteString -> Vector SpannedToken -> Maybe ByteString
buildCanonicalKey sql toks = go 0 0 mempty
    where
        !n = V.length toks
        !len = BS.length sql

        go !i !prevEnd !builder
            | i >= n = do
                guard (allAsciiWhitespace prevEnd len sql)
                pure
                    ( BL.toStrict
                        (BSBExtra.toLazyByteStringWith (BSBExtra.safeStrategy 64 BSBExtra.smallChunkSize) BL.empty builder)
                    )
            | otherwise = do
                let !(L sp@(ByteSpan s e) _) = toks V.! i
                guard (s /= e && allAsciiWhitespace prevEnd s sql)
                let !tokBytes = unsafeSliceSpan sp sql
                    !builder' = builder <> BSB.byteString tokBytes <> BSB.word8 0
                go (i + 1) e builder'

startsWithSelect :: ByteString -> Bool
startsWithSelect bs = go 0
    where
        !len = BS.length bs
        go !i
            | i < len && isAsciiWhitespace (BSU.unsafeIndex bs i) = go (i + 1)
            | i + 6 > len = False
            | otherwise =
                asciiUpperEqAt bs i "SELECT"

-- | Any interior semicolon in a cacheable SELECT must be inside a plain string literal.
hasInteriorSemicolon :: Bool -> ByteString -> Bool
hasInteriorSemicolon escapes bs =
    let !trimmedEnd = trimAsciiEndLen bs
        !bodyLen =
            if trimmedEnd > 0 && BSU.unsafeIndex bs (trimmedEnd - 1) == 59
                then trimmedEnd - 1
                else trimmedEnd
     in outsideString 0 bodyLen
    where
        outsideString !i !limit
            | i >= limit = False
            | b == 59 = True
            | b == 39 = insideString (i + 1) limit
            | otherwise = outsideString (i + 1) limit
            where
                b = BSU.unsafeIndex bs i

        insideString !i !limit
            | i >= limit = False
            | escapes && b == 92 && i + 1 < limit = insideString (i + 2) limit
            | b /= 39 = insideString (i + 1) limit
            | i + 1 < limit && BSU.unsafeIndex bs (i + 1) == 39 = insideString (i + 2) limit
            | otherwise = outsideString (i + 1) limit
            where
                b = BSU.unsafeIndex bs i

trimAsciiEndLen :: ByteString -> Int
trimAsciiEndLen bs = go (BS.length bs)
    where
        go !k
            | k > 0 && isAsciiWhitespace (BSU.unsafeIndex bs (k - 1)) = go (k - 1)
            | otherwise = k

allAsciiWhitespace :: Int -> Int -> ByteString -> Bool
allAsciiWhitespace !from !to bs
    | from > to || to > BS.length bs = False
    | otherwise = go from
    where
        go !i
            | i >= to = True
            | isAsciiWhitespace (BSU.unsafeIndex bs i) = go (i + 1)
            | otherwise = False

{-# INLINE isAsciiWhitespace #-}
isAsciiWhitespace :: Word8 -> Bool
isAsciiWhitespace b = b == 32 || b == 9 || b == 10 || b == 13 || b == 12

asciiUpperEqAt :: ByteString -> Int -> ByteString -> Bool
asciiUpperEqAt bs !off target = go 0
    where
        !tLen = BS.length target
        go !k
            | k >= tLen = True
            | toUpperByte (BSU.unsafeIndex bs (off + k)) == BSU.unsafeIndex target k = go (k + 1)
            | otherwise = False

{-# INLINE toUpperByte #-}
toUpperByte :: Word8 -> Word8
toUpperByte w
    | w >= 97 && w <= 122 = w - 32
    | otherwise = w

asciiLowerText :: ByteString -> Text
asciiLowerText = T.toLower . TE.decodeUtf8

classifyStatement :: Vector SpannedToken -> (Text, Maybe ByteString)
classifyStatement stmt
    | Just tbl <- matchCreateTable stmt = ("create", Just tbl)
    | Just tbl <- matchInsertValues stmt = ("insert", Just tbl)
    | Just tbl <- matchAggregateSelect stmt = ("query", Just tbl)
    | Just () <- matchScalarSelect stmt = ("query", Nothing)
    | otherwise = ("other", Nothing)

-- Token-stream matcher using StateT Int Maybe
type Match = StateT Int Maybe

runMatch :: Vector SpannedToken -> Match a -> Maybe a
runMatch v m = evalStateT (m <* eof v) 0

nextTok :: Vector SpannedToken -> Match Token
nextTok v = do
    i <- get
    guard (i < V.length v)
    put (i + 1)
    pure (locValue (v V.! i))

eof :: Vector SpannedToken -> Match ()
eof v = do
    i <- get
    guard (i == V.length v)

satisfy :: Vector SpannedToken -> (Token -> Maybe a) -> Match a
satisfy v f = nextTok v >>= lift . f

token :: Vector SpannedToken -> Token -> Match ()
token v expected = satisfy v (\tok -> guard (tok == expected))

keyword :: Vector SpannedToken -> Keyword -> Match ()
keyword v expected = satisfy v (guard . isKeywordToken expected)

ident :: Vector SpannedToken -> Match ByteString
ident v = satisfy v \case
    TokWord WordToken {wordRaw = raw, wordQuote = Nothing, wordKeyword = Nothing}
        | BS.all (< 0x80) raw -> Just raw
    _ -> Nothing

plainWordCI :: Vector SpannedToken -> ByteString -> Match ()
plainWordCI v expected = do
    raw <- ident v
    guard (asciiEqCI expected raw)

digitsOnly :: Vector SpannedToken -> Match ()
digitsOnly v = satisfy v \case
    TokLiteral LitNumber raw
        | not (BS.null raw) && BS.all isAsciiDigit raw -> Just ()
    _ -> Nothing

literal :: Vector SpannedToken -> Match ()
literal v = signedDigits <|> simpleLit
    where
        signedDigits = (token v TokPlus <|> token v TokMinus) *> digitsOnly v
        simpleLit = satisfy v \case
            TokLiteral LitNumber raw
                | not (BS.null raw) && BS.all isAsciiDigit raw -> Just ()
            TokLiteral LitString _ -> Just ()
            tok
                | isKeywordToken KwNull tok
                    || isKeywordToken KwTrue tok
                    || isKeywordToken KwFalse tok ->
                    Just ()
            _ -> Nothing

{-# INLINE isAsciiDigit #-}
isAsciiDigit :: Word8 -> Bool
isAsciiDigit b = b >= 48 && b <= 57

sepBy1 :: Match a -> Match sep -> Match ()
sepBy1 item sep = item *> go
    where
        go = optional sep >>= maybe (pure ()) (const (item *> go))

matchScalarSelect :: Vector SpannedToken -> Maybe ()
matchScalarSelect v = runMatch v do
    keyword v KwSelect
    sepBy1 selectItem (token v TokComma)
    where
        selectItem = literal v *> keyword v KwAs *> void (ident v)

matchCreateTable :: Vector SpannedToken -> Maybe ByteString
matchCreateTable v = runMatch v do
    keyword v KwCreate
    keyword v KwTable
    name <- ident v
    token v TokLParen
    sepBy1 colDef (token v TokComma)
    token v TokRParen
    pure name
    where
        colDef =
            ident v *> satisfy v \case
                TokWord WordToken {wordQuote = Nothing, wordKeyword = Just kw}
                    | isSupportedReplayType kw -> Just ()
                _ -> Nothing

isSupportedReplayType :: Keyword -> Bool
isSupportedReplayType = \case
    KwTinyint -> True
    KwSmallint -> True
    KwInt -> True
    KwInteger -> True
    KwBigint -> True
    KwHugeint -> True
    KwVarchar -> True
    KwBoolean -> True
    _ -> False

matchInsertValues :: Vector SpannedToken -> Maybe ByteString
matchInsertValues v = runMatch v do
    keyword v KwInsert
    keyword v KwInto
    name <- ident v
    keyword v KwValues
    sepBy1 rowTuple (token v TokComma)
    pure name
    where
        rowTuple = token v TokLParen *> sepBy1 (literal v) (token v TokComma) <* token v TokRParen

matchAggregateSelect :: Vector SpannedToken -> Maybe ByteString
matchAggregateSelect v = runMatch v do
    keyword v KwSelect
    sepBy1 aggItem (token v TokComma)
    keyword v KwFrom
    ident v
    where
        aggItem = do
            isCount <- (True <$ keyword v KwCount) <|> (False <$ plainWordCI v "SUM")
            token v TokLParen
            if isCount
                then token v TokStar <|> void (ident v)
                else void (ident v)
            token v TokRParen
            void $ optional (keyword v KwAs *> ident v)
