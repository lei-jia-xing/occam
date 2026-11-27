-- | Parser monad, trivia-free token primitives, and dialect gates.
module Occam.Frontend.Parser.Core (
    -- * Public error and result types
    ParseErrorKind (..),
    ParseError (..),
    ParsedScript (..),

    -- * Parser monad and hooks
    PState (..),
    P,
    runParser,
    parseQueryHook,
    parseStatementHook,
    throwParse,
    unexpectedHere,
    unexpectedUnsupported,
    expectStatementEnd,

    -- * Control-flow and list combinators
    earlyExit,
    exitIfJust,
    onWord,
    onWordTok,
    onWords,
    onTok,
    whenWord,
    whenWordTok,
    whenTok,
    expectWordsCI,
    inParens,
    parseParenList,
    parseOptionalParenList,
    parseOptParenList,
    parseOptParenNonEmptyList,
    rewrap,
    manyPrefix,
    manyWhile,
    wordIn,
    lastMaybe,

    -- * Spans and token primitives
    currentSpan,
    lastConsumedEnd,
    locatedSpanning,
    withLocated,
    peekTok,
    peekTokAt,
    peekToken,
    advanceTok,
    matchTok,
    expectTok,
    tokenRaw,
    decodeBS,
    describeToken,

    -- * Word, name, and alias helpers
    unquotedWord,
    headWordOf,
    currentHeadWord,
    currentWord,
    wordAt,
    wordIsAt,
    wordIsCurrent,
    matchWordCI,
    matchAnyWordCI,
    expectWordCI,
    expectWordPart,
    parseNamePart,
    parseName,
    parseOptionalColumnNames,
    parseOptionalAlias,
    parseAlias,
    tokenStartsBareAlias,
    parseCommaSeparated,
    parseOptionalCommaList,
    queryStartsAt,
    isStatementStartWord,

    -- * Dialect gating
    hasFeatureP,
    requireFeatureP,
    requireDialectP,
    isDialect,

    -- * Opaque body scanners
    parseOpaqueToEnd,
    parseOpaqueUntil,
) where

import Control.Monad (guard, join, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT (..), evalContT)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Trans.State.Strict (StateT (..), evalStateT, get, gets, modify')
import Data.ByteString (ByteString)
import Data.Foldable (find, traverse_)
import Data.Functor (($>))
import Data.HashSet qualified as HashSet
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Occam.Frontend.AST (
    Alias (..),
    Name (..),
    NamePart (..),
    OpaqueBody (..),
    OpaqueKind,
    Query,
    Statement,
 )
import Occam.Frontend.Lexer (
    SpannedToken,
    Token (..),
    WordToken (..),
    asciiEqCI,
    asciiUpperBS,
 )
import Occam.Frontend.Span (
    ByteSpan (..),
    Located (..),
    mergeSpan,
    spanEnd,
    spanStart,
 )
import Occam.Parser.Dialect (
    DialectFeature,
    DialectProfile (..),
 )
import Occam.Parser.Types (SQLDialect (ClickHouse, DuckDB))

-- ---------------------------------------------------------------------------
-- Public error and result types
-- ---------------------------------------------------------------------------

-- | Why parsing stopped.
data ParseErrorKind
    = ParseUnexpectedToken
    | ParseUnexpectedEnd
    | ParseUnsupported
    | ParseLexicalError
    deriving (Eq, Ord, Show)

-- | First parser failure, with the exact token or lexical-error span.
data ParseError = ParseError
    { parseErrorKind :: !ParseErrorKind
    , parseErrorSpan :: !ByteSpan
    , parseErrorMessage :: !Text
    }
    deriving (Eq, Ord, Show)

-- | Statements parsed from a script, in source order.
newtype ParsedScript = ParsedScript
    { parsedScriptStatements :: [Located Statement]
    }
    deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Parser monad and hooks
-- ---------------------------------------------------------------------------

-- | Dialect, remaining tokens, end offset, and recursion hooks.
data PState = PState
    { psProfile :: !DialectProfile
    , psTokens :: !(Vector SpannedToken)
    , psPos :: !Int
    , psEndOffset :: !Int
    , psParseQuery :: P (Located Query)
    , psParseStatement :: P (Located Statement)
    }

-- | Stateful parser over 'PState' with first-error short-circuiting.
type P = StateT PState (Either ParseError)

-- | Run @action@ over a trivia-free token vector ending at @endOffset@.
runParser ::
    DialectProfile ->
    Vector SpannedToken ->
    Int ->
    P (Located Query) ->
    P (Located Statement) ->
    P a ->
    Either ParseError a
runParser profile tokens endOffset parseQ parseS action =
    evalStateT action (PState profile tokens 0 endOffset parseQ parseS)

-- | Re-enter the query parser (used for subqueries and CTE hooks).
parseQueryHook :: P (Located Query)
parseQueryHook = join (gets psParseQuery)

-- | Re-enter the statement parser (used for CTE bodies and nested statements).
parseStatementHook :: P (Located Statement)
parseStatementHook = join (gets psParseStatement)

-- | Abort with a classified, spanned error.
throwParse :: ParseErrorKind -> ByteSpan -> Text -> P a
throwParse kind sp msg = lift (Left (ParseError kind sp msg))

-- ---------------------------------------------------------------------------
-- Control-flow and list combinators
-- ---------------------------------------------------------------------------

-- | Abort the current 'ContT' block with a result.
earlyExit :: (Monad m) => r -> ContT r m a
earlyExit r = ContT \_ -> pure r

-- | Run a continuation on a match, otherwise fall through.
exitIfJust :: P (Maybe a) -> (a -> P r) -> ContT r P ()
exitIfJust p k = lift p >>= traverse_ (\x -> lift (k x) >>= earlyExit)

-- | Run @action@ when the next keyword is @w@, or fall through.
onWord :: ByteString -> P r -> ContT r P ()
onWord w action = exitIfJust (matchWordCI w) (const action)

-- | Run @k@ with the matched keyword token, or fall through.
onWordTok :: ByteString -> (SpannedToken -> P r) -> ContT r P ()
onWordTok w = exitIfJust (matchWordCI w)

-- | Match one of @ws@ and pass its token and spelling, or fall through.
onWords :: [ByteString] -> (SpannedToken -> ByteString -> P r) -> ContT r P ()
onWords ws action = exitIfJust (matchAnyWordCI ws) (uncurry action)

-- | Run @action@ when the next punctuation token matches, or fall through.
onTok :: Token -> P r -> ContT r P ()
onTok t action = exitIfJust (matchTok t) (const action)

-- | Parse @action@ when the next keyword is @w@.
whenWord :: ByteString -> P a -> P (Maybe a)
whenWord w action = matchWordCI w >>= traverse (const action)

-- | Parse @action@ with the matched keyword token when the next word is @w@.
whenWordTok :: ByteString -> (SpannedToken -> P a) -> P (Maybe a)
whenWordTok w action = matchWordCI w >>= traverse action

-- | Parse @action@ when the next token is @t@.
whenTok :: Token -> P a -> P (Maybe a)
whenTok t action = matchTok t >>= traverse (const action)

-- | Consume a case-insensitive keyword sequence.
expectWordsCI :: [ByteString] -> P ()
expectWordsCI = traverse_ expectWordCI

-- | Parse @p@ surrounded by parentheses.
inParens :: P a -> P a
inParens p = expectTok TokLParen *> p <* expectTok TokRParen

-- | Parse a non-empty comma-separated list inside parentheses.
parseParenList :: P a -> P [a]
parseParenList = inParens . parseCommaSeparated

-- | Parse a parenthesized list that may be empty.
parseOptionalParenList :: P a -> P [a]
parseOptionalParenList = inParens . parseOptionalCommaList

-- | Parse an optional parenthesized list; absent parentheses yield @[]@.
parseOptParenList :: P a -> P [a]
parseOptParenList item =
    fromMaybe [] <$> whenTok TokLParen (parseOptionalCommaList item <* expectTok TokRParen)

-- | Parse an optional parenthesized non-empty list.
parseOptParenNonEmptyList :: P a -> P [a]
parseOptParenNonEmptyList item =
    fromMaybe [] <$> whenTok TokLParen (parseCommaSeparated item <* expectTok TokRParen)

-- | Re-wrap a located value, preserving its span.
rewrap :: (Located a -> b) -> Located a -> Located b
rewrap con inner = L (locSpan inner) (con inner)

-- | Repeat @item@ while the optional prefix parser keeps matching.
manyPrefix :: P (Maybe sep) -> P a -> P [a]
manyPrefix sep item = go
    where
        go = sep >>= maybe (pure []) (const ((:) <$> item <*> go))

-- | Repeat @item@ while @cond@ returns 'True'.
manyWhile :: P Bool -> P a -> P [a]
manyWhile cond item = go
    where
        go = cond >>= \b -> if b then (:) <$> item <*> go else pure []

-- | Case-insensitive membership in a keyword list.
wordIn :: [ByteString] -> ByteString -> Bool
wordIn ws raw = any (`asciiEqCI` raw) ws

-- | Last element of a list, if any.
lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just (last xs)

-- ---------------------------------------------------------------------------
-- Spans and token primitives
-- ---------------------------------------------------------------------------

-- | Span of the next token, or a zero-width span at the unit end.
currentSpan :: P ByteSpan
currentSpan = do
    st <- get
    pure $ maybe (ByteSpan (psEndOffset st) (psEndOffset st)) locSpan (tokenAt st 0)

-- | End offset of the last consumed token, or the unit end.
lastConsumedEnd :: P Int
lastConsumedEnd = do
    st <- get
    pure $
        if psPos st <= 0
            then psEndOffset st
            else spanEnd (locSpan (psTokens st V.! (psPos st - 1)))

-- | Wrap a value in a span from @start@ through the last consumed token.
locatedSpanning :: ByteSpan -> a -> P (Located a)
locatedSpanning start value = do
    end <- lastConsumedEnd
    pure (L (ByteSpan (spanStart start) (max (spanStart start) end)) value)

-- | Span the value parsed by @action@ from its first token through its last.
withLocated :: P a -> P (Located a)
withLocated action = do
    start <- currentSpan
    action >>= locatedSpanning start

tokenAt :: PState -> Int -> Maybe SpannedToken
tokenAt st n =
    let i = psPos st + n
        toks = psTokens st
     in toks V.! i <$ guard (i >= 0 && i < V.length toks)

-- | Peek the next token without consuming it.
peekTok :: P (Maybe SpannedToken)
peekTok = peekTokAt 0

-- | Peek the token @n@ positions ahead.
peekTokAt :: Int -> P (Maybe SpannedToken)
peekTokAt n = gets (`tokenAt` n)

-- | Peek the next token's payload without its span.
peekToken :: P (Maybe Token)
peekToken = fmap (fmap locValue) peekTok

-- | Consume and return the next token, or fail at the unit end.
advanceTok :: P SpannedToken
advanceTok = do
    mt <- peekTok
    case mt of
        Nothing -> do
            off <- gets psEndOffset
            throwParse ParseUnexpectedEnd (ByteSpan off off) "unexpected end of input"
        Just tok -> do
            modify' (\st -> st {psPos = psPos st + 1})
            pure tok

-- | Consume the next token when it equals @expected@.
matchTok :: Token -> P (Maybe SpannedToken)
matchTok expected = matchNextToken (\tok -> tok <$ guard (locValue tok == expected))

{-# INLINE matchNextToken #-}
matchNextToken :: (SpannedToken -> Maybe a) -> P (Maybe a)
matchNextToken select = StateT \st ->
    case tokenAt st 0 >>= select of
        Nothing -> Right (Nothing, st)
        hit -> Right (hit, st {psPos = psPos st + 1})

-- | Consume the next token or fail with an expected-token message.
expectTok :: Token -> P SpannedToken
expectTok expected =
    matchTok expected >>= maybe (unexpectedHere ("expected " <> describeToken expected)) pure

-- | Fail at the next token with a caller-supplied expectation.
unexpectedHere :: Text -> P a
unexpectedHere what = do
    mt <- peekTok
    case mt of
        Nothing -> do
            off <- gets psEndOffset
            throwParse ParseUnexpectedEnd (ByteSpan off off) (what <> " at end of input")
        Just (L sp tok) ->
            throwParse ParseUnexpectedToken sp (what <> ", found " <> describeToken tok)

-- | Fail as unsupported at the given token, including it in the message.
unexpectedUnsupported :: SpannedToken -> Text -> P a
unexpectedUnsupported (L sp tok) msg =
    throwParse ParseUnsupported sp (msg <> " (found " <> describeToken tok <> ")")

-- | Require that no substantive token remains in the unit.
expectStatementEnd :: P ()
expectStatementEnd =
    peekTok >>= traverse_ \(L sp tok) ->
        throwParse
            ParseUnexpectedToken
            sp
            ("unexpected trailing token " <> describeToken tok <> " after statement")

-- ---------------------------------------------------------------------------
-- Word, name, and alias helpers
-- ---------------------------------------------------------------------------

-- | Drop the quote marker from a bare word.
unquotedWord :: WordToken -> Maybe WordToken
unquotedWord wt@WordToken {wordQuote = Nothing} = Just wt
unquotedWord _ = Nothing

-- | Raw spelling of an unquoted word token.
headWordOf :: Token -> Maybe ByteString
headWordOf (TokWord wt) = wordRaw <$> unquotedWord wt
headWordOf _ = Nothing

-- | Raw spelling of the next token when it is an unquoted word.
currentHeadWord :: P (Maybe ByteString)
currentHeadWord = fmap (>>= headWordOf . locValue) peekTok

-- | The next token and its raw spelling when it is an unquoted word.
currentWord :: P (Maybe (SpannedToken, ByteString))
currentWord = wordAt 0

-- | Look ahead by @n@ tokens for an unquoted word.
wordAt :: Int -> P (Maybe (SpannedToken, ByteString))
wordAt n = runMaybeT do
    tok@(L _ (TokWord wt)) <- MaybeT (peekTokAt n)
    uw <- MaybeT . pure $ unquotedWord wt
    pure (tok, wordRaw uw)

-- | Case-insensitive keyword test at a fixed lookahead.
wordIsAt :: Int -> ByteString -> P Bool
wordIsAt n expected = gets \st ->
    case tokenAt st n of
        Just (L _ (TokWord WordToken {wordRaw = raw, wordQuote = Nothing})) -> asciiEqCI expected raw
        _ -> False

-- | Case-insensitive keyword test on the next token.
wordIsCurrent :: ByteString -> P Bool
wordIsCurrent = wordIsAt 0

-- | Consume the next unquoted word when it equals @expected@ (ASCII CI).
matchWordCI :: ByteString -> P (Maybe SpannedToken)
matchWordCI expected = matchNextToken \tok -> do
    raw <- headWordOf (locValue tok)
    guard (asciiEqCI expected raw)
    pure tok

-- | Consume the next word when it matches one of @expected@.
matchAnyWordCI :: [ByteString] -> P (Maybe (SpannedToken, ByteString))
matchAnyWordCI expected = matchNextToken \tok -> do
    raw <- headWordOf (locValue tok)
    hit <- find (`asciiEqCI` raw) expected
    pure (tok, hit)

-- | Consume a required keyword (ASCII case-insensitive).
expectWordCI :: ByteString -> P SpannedToken
expectWordCI expected =
    matchWordCI expected >>= maybe (unexpectedHere ("expected keyword " <> decodeBS expected)) pure

-- | Consume a required keyword and return it as an unquoted name part.
expectWordPart :: ByteString -> P (Located NamePart)
expectWordPart expected = do
    tok <- expectWordCI expected
    let raw = case locValue tok of
            TokWord wt -> wordRaw wt
            _ -> expected
    pure (L (locSpan tok) (NamePart raw False))

-- | Consume one identifier component, preserving quotes, or fail.
parseNamePart :: P (Located NamePart)
parseNamePart = do
    mt <- peekTok
    case mt of
        Just (L sp (TokWord wt)) ->
            advanceTok $> L sp (NamePart (wordRaw wt) (isJust (wordQuote wt)))
        _ -> unexpectedHere "expected identifier"

-- | Parse a possibly schema-qualified name with exact span.
parseName :: P (Located Name)
parseName = do
    first <- parseNamePart
    rest <- manyPrefix dotWordAhead parseNamePart
    let parts = first : rest
        sp = foldl' (\acc p -> mergeSpan acc (locSpan p)) (locSpan first) rest
    pure (L sp (Name parts))
    where
        dotWordAhead = runMaybeT do
            L _ TokDot <- MaybeT peekTok
            L _ (TokWord _) <- MaybeT (peekTokAt 1)
            lift (Just <$> advanceTok)

-- | Optional parenthesized column-name list.
parseOptionalColumnNames :: P [Located NamePart]
parseOptionalColumnNames = parseOptParenNonEmptyList parseNamePart

-- | Parse an optional @AS alias@ or bare alias, unless the next word starts a clause.
parseOptionalAlias :: Bool -> P (Maybe (Located Alias))
parseOptionalAlias allowColumns = evalContT do
    onWord "AS" (Just <$> parseAlias allowColumns)
    lift do
        mt <- peekTok
        isBare <- tokenStartsBareAliasP mt
        if isBare
            then Just <$> parseAlias allowColumns
            else pure Nothing

-- | Dialect-aware check for whether a token can start an implicit alias.
tokenStartsBareAliasP :: Maybe SpannedToken -> P Bool
tokenStartsBareAliasP mt = case mt of
    Just (L _ (TokWord wt))
        | Nothing <- wordQuote wt -> do
            let upper = asciiUpperBS (wordRaw wt)
            if upper == "ASOF"
                then not <$> gets ((`elem` [DuckDB, ClickHouse]) . dialectId . psProfile)
                else pure (tokenStartsBareAlias mt)
    _ -> pure (tokenStartsBareAlias mt)

-- | Parse one alias, optionally with a parenthesized column list.
parseAlias :: Bool -> P (Located Alias)
parseAlias allowColumns = withLocated do
    namePart <- parseNamePart
    cols <- if allowColumns then parseOptionalColumnNames else pure []
    pure (Alias namePart cols)

-- | Whether a token can start an implicit alias without stealing a clause keyword.
tokenStartsBareAlias :: Maybe SpannedToken -> Bool
tokenStartsBareAlias (Just (L _ (TokWord wt))) =
    isJust (wordQuote wt) || not (HashSet.member (asciiUpperBS (wordRaw wt)) reservedAliasWords)
tokenStartsBareAlias _ = False

-- | Keywords that must not be consumed as a bare table or column alias.
reservedAliasWords :: HashSet.HashSet ByteString
reservedAliasWords =
    HashSet.fromList
        [ "FROM"
        , "WHERE"
        , "GROUP"
        , "HAVING"
        , "WINDOW"
        , "QUALIFY"
        , "PREWHERE"
        , "SETTINGS"
        , "ORDER"
        , "LIMIT"
        , "OFFSET"
        , "FETCH"
        , "UNION"
        , "INTERSECT"
        , "EXCEPT"
        , "JOIN"
        , "INNER"
        , "LEFT"
        , "RIGHT"
        , "FULL"
        , "CROSS"
        , "NATURAL"
        , "ON"
        , "USING"
        , "AND"
        , "OR"
        , "XOR"
        , "NOT"
        , "IS"
        , "IN"
        , "LIKE"
        , "ILIKE"
        , "GLOB"
        , "BETWEEN"
        , "WHEN"
        , "THEN"
        , "ELSE"
        , "END"
        , "SET"
        , "VALUES"
        , "RETURNING"
        , "INTO"
        , "DO"
        , "ASC"
        , "DESC"
        , "NULLS"
        , "ROWS"
        , "RANGE"
        , "GROUPS"
        , "FOR"
        , "TO"
        , "WITH"
        , "RECURSIVE"
        , "OVER"
        , "FILTER"
        , "DISTINCT"
        , "ALL"
        , "CASE"
        , "CAST"
        , "EXISTS"
        , "ARRAY"
        , "TABLE"
        , "SELECT"
        , "INSERT"
        , "UPDATE"
        , "DELETE"
        , "MERGE"
        , "CREATE"
        , "DROP"
        , "ALTER"
        , "PRAGMA"
        , "VACUUM"
        , "ANALYZE"
        , "ATTACH"
        , "DETACH"
        , "PREPARE"
        , "EXECUTE"
        , "DEALLOCATE"
        , "TRUNCATE"
        , "EXPLAIN"
        , "COPY"
        , "BEGIN"
        , "COMMIT"
        , "ROLLBACK"
        , "SAVEPOINT"
        , "RELEASE"
        , "START"
        , "BY"
        , "PARTITION"
        , "MATERIALIZED"
        , "ENGINE"
        , "TTL"
        , "PRIMARY"
        , "KEY"
        , "UNIQUE"
        , "CHECK"
        , "FOREIGN"
        , "REFERENCES"
        , "CONSTRAINT"
        , "DEFAULT"
        , "NULL"
        , "TRUE"
        , "FALSE"
        , "GENERATED"
        , "COLLATE"
        , "BINARY"
        , "AUTO_INCREMENT"
        , "AUTOINCREMENT"
        , "COMMENT"
        , "CHARACTER"
        , "IF"
        , "ANTI"
        , "SEMI"
        , "LATERAL"
        , "PIVOT"
        , "UNPIVOT"
        , "FINAL"
        , "GLOBAL"
        , "SOME"
        , "ANY"
        ]

-- | Parse one or more @item@s separated by commas.
parseCommaSeparated :: P a -> P [a]
parseCommaSeparated item = (:) <$> item <*> manyPrefix (matchTok TokComma) item

-- | Comma-separated list that permits an immediately closing parenthesis.
parseOptionalCommaList :: P a -> P [a]
parseOptionalCommaList item = do
    mt <- peekToken
    case mt of
        Just TokRParen -> pure []
        _ -> parseCommaSeparated item

{- | Look ahead for a query without mistaking a parenthesized scalar operand
(possibly a subquery itself) for the start of a larger query.
-}
queryStartsAt :: Int -> P Bool
queryStartsAt offset = do
    PState {psProfile, psTokens, psPos} <- get
    let token !i = locValue <$> (psTokens V.!? i)
        fromFirst = dialectId psProfile == DuckDB
        startsQuery =
            maybe
                False
                ( \raw ->
                    wordIn ["SELECT", "WITH", "VALUES", "TABLE"] raw || (fromFirst && asciiEqCI "FROM" raw)
                )
                . (>>= headWordOf)
        start !i !open = case token i of
            Just TokLParen -> start (i + 1) (open + 1)
            next -> startsQuery next && (open == 0 || scanBranch i open open)
        scanBranch !i !depth !branchDepth = case token i of
            Just TokLParen -> scanBranch (i + 1) (depth + 1) branchDepth
            Just TokRParen
                | depth == branchDepth -> afterBranch (i + 1) (depth - 1)
                | otherwise -> scanBranch (i + 1) (depth - 1) branchDepth
            Just _ -> scanBranch (i + 1) depth branchDepth
            Nothing -> False
        afterBranch !i !remaining = case token i of
            Just TokRParen -> remaining == 0 || afterBranch (i + 1) (remaining - 1)
            Just next
                | Just raw <- headWordOf next
                , wordIn
                    ["UNION", "INTERSECT", "EXCEPT", "ORDER", "LIMIT", "OFFSET", "FETCH", "FOR", "SETTINGS", "FORMAT"]
                    raw ->
                    remaining == 0 || scanBranch (i + 1) remaining remaining
            Nothing -> remaining == 0
            _ -> False
    pure (start (psPos + offset) (0 :: Int))

-- | Words that begin a top-level statement.
isStatementStartWord :: ByteString -> Bool
isStatementStartWord = (`HashSet.member` statementStartWords) . asciiUpperBS

statementStartWords :: HashSet.HashSet ByteString
statementStartWords =
    HashSet.fromList
        [ "SELECT"
        , "WITH"
        , "VALUES"
        , "TABLE"
        , "INSERT"
        , "UPDATE"
        , "DELETE"
        , "MERGE"
        , "REPLACE"
        , "RENAME"
        , "NOTIFY"
        , "OPTIMIZE"
        , "SHOW"
        , "CREATE"
        , "DROP"
        , "ALTER"
        , "PRAGMA"
        , "SET"
        , "RESET"
        , "LOCK"
        , "UNLOCK"
        , "GRANT"
        , "REVOKE"
        , "USE"
        , "BEGIN"
        , "START"
        , "COMMIT"
        , "ROLLBACK"
        , "SAVEPOINT"
        , "RELEASE"
        , "END"
        , "EXPLAIN"
        , "COPY"
        , "ANALYZE"
        , "VACUUM"
        , "ATTACH"
        , "DETACH"
        , "PREPARE"
        , "EXECUTE"
        , "DEALLOCATE"
        , "TRUNCATE"
        , "PIVOT"
        , "UNPIVOT"
        ]

-- ---------------------------------------------------------------------------
-- Dialect gating
-- ---------------------------------------------------------------------------

-- | Check the active profile, including capability overrides.
hasFeatureP :: DialectFeature -> P Bool
hasFeatureP feature = gets (Set.member feature . dialectFeatures . psProfile)

-- | Require a dialect capability, reporting the caller's construct at @sp@.
requireFeatureP :: DialectFeature -> ByteSpan -> Text -> P ()
requireFeatureP feature sp what = do
    profile <- gets psProfile
    unless (Set.member feature (dialectFeatures profile)) $
        throwParse ParseUnsupported sp (what <> " is not supported by " <> dialectCanonicalName profile)

-- | Require one of the listed concrete dialects.
requireDialectP :: [SQLDialect] -> ByteSpan -> Text -> P ()
requireDialectP allowed sp what = do
    profile <- gets psProfile
    unless (dialectId profile `elem` allowed) $
        throwParse ParseUnsupported sp (what <> " is not supported by " <> dialectCanonicalName profile)

-- | True when the active dialect is @d@.
isDialect :: SQLDialect -> P Bool
isDialect d = gets ((== d) . dialectId . psProfile)

-- ---------------------------------------------------------------------------
-- Opaque body scanners
-- ---------------------------------------------------------------------------

-- | Consume the rest of the unit as an opaque body.
parseOpaqueToEnd :: OpaqueKind -> P OpaqueBody
parseOpaqueToEnd kind = do
    start <- currentSpan
    let loop = peekTok >>= traverse_ (const (advanceTok *> loop))
    loop
    end <- lastConsumedEnd
    pure (OpaqueBody (ByteSpan (spanStart start) (max (spanStart start) end)) kind)

-- | Consume until a stop word at parenthesis depth zero, leaving it unconsumed.
parseOpaqueUntil :: OpaqueKind -> [ByteString] -> P OpaqueBody
parseOpaqueUntil kind stops = do
    start <- currentSpan
    go (0 :: Int)
    end <- lastConsumedEnd
    pure (OpaqueBody (ByteSpan (spanStart start) (max (spanStart start) end)) kind)
    where
        go !depth =
            peekTok >>= traverse_ \(L _ tok) -> case tok of
                TokWord wt
                    | depth == 0
                    , Just uw <- unquotedWord wt
                    , wordIn stops (wordRaw uw) ->
                        pure ()
                TokLParen -> advanceTok *> go (depth + 1)
                TokRParen -> advanceTok *> go (max 0 (depth - 1))
                TokLBracket -> advanceTok *> go (depth + 1)
                TokRBracket -> advanceTok *> go (max 0 (depth - 1))
                TokLBrace -> advanceTok *> go (depth + 1)
                TokRBrace -> advanceTok *> go (max 0 (depth - 1))
                _ -> advanceTok *> go depth

-- ---------------------------------------------------------------------------
-- Token formatting helpers
-- ---------------------------------------------------------------------------

-- | Raw source bytes for a token, or @""@ for punctuation.
tokenRaw :: SpannedToken -> ByteString
tokenRaw (L _ tok) = case tok of
    TokWord wt -> wordRaw wt
    TokLiteral _ raw -> raw
    TokParam raw -> raw
    TokUserVar raw -> raw
    TokSysVar raw -> raw
    TokOperator raw -> raw
    TokUnknown raw -> raw
    _ -> ""

-- | Lenient UTF-8 decoding for diagnostics.
decodeBS :: ByteString -> Text
decodeBS = TE.decodeUtf8With lenientDecode

-- | Human-readable token description used in parse errors.
describeToken :: Token -> Text
describeToken = \case
    TokTrivia _ -> "trivia"
    TokWord wt -> "'" <> decodeBS (wordRaw wt) <> "'"
    TokLiteral _ raw -> "literal '" <> decodeBS raw <> "'"
    TokParam raw -> "parameter '" <> decodeBS raw <> "'"
    TokUserVar raw -> "user variable '" <> decodeBS raw <> "'"
    TokSysVar raw -> "system variable '" <> decodeBS raw <> "'"
    TokLParen -> "'('"
    TokRParen -> "')'"
    TokLBracket -> "'['"
    TokRBracket -> "']'"
    TokLBrace -> "'{'"
    TokRBrace -> "'}'"
    TokComma -> "','"
    TokSemicolon -> "';'"
    TokDot -> "'.'"
    TokDoubleColon -> "'::'"
    TokDotColon -> "'. :'"
    TokColonEq -> "':='"
    TokColon -> "':'"
    TokArrow -> "'->'"
    TokDoubleArrow -> "'->>'"
    TokPlus -> "'+'"
    TokMinus -> "'-'"
    TokStar -> "'*'"
    TokSlash -> "'/'"
    TokEq -> "'='"
    TokQuestion -> "'?'"
    TokOperator raw -> "operator '" <> decodeBS raw <> "'"
    TokUnknown raw -> "unknown token '" <> decodeBS raw <> "'"
