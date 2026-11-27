{-# LANGUAGE MagicHash #-}

-- | Single-pass UTF-8 lexer that retains trivia and exact byte spans.
module Occam.Frontend.Lexer (
    Keyword (..),
    QuoteKind (..),
    WordToken (..),
    LiteralKind (..),
    TriviaKind (..),
    Token (..),
    SpannedToken,
    LexErrorKind (..),
    LexError (..),
    LexResult (..),
    lexSQL,
    lexSQLStrict,
    isTriviaToken,
    isWhitespaceToken,
    isCommentToken,
    isKeywordToken,
    keywordText,
    asciiUpperBS,
    asciiEqCI,
) where

import Control.Monad.ST (ST, runST)
import Data.Bits (unsafeShiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Char (chr, isAlpha, isAlphaNum)
import Data.Data (Data)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Word (Word8)
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)
import Occam.Frontend.Span (ByteSpan (..), Located (..), unsafeSliceSpan, validateUtf8)
import Occam.Parser.Dialect (
    DialectProfile (..),
    LexicalProfile (..),
    clickhouseProfile,
    dialectProfile,
    duckdbProfile,
    mariadbProfile,
    mysqlProfile,
    postgresqlProfile,
    sqliteProfile,
 )
import Occam.Parser.Types (SQLDialect (..))

-- | SQL keywords recognized by the lexer (core + dialect-specific).
data Keyword
    = KwAdd
    | KwAlgorithm
    | KwAll
    | KwAlter
    | KwAnalyze
    | KwAnd
    | KwAny
    | KwArray
    | KwAs
    | KwAsc
    | KwAsof
    | KwAtomic
    | KwAttach
    | KwAutoincrement
    | KwBegin
    | KwBetween
    | KwBigint
    | KwBoolean
    | KwBy
    | KwCascade
    | KwCase
    | KwCast
    | KwChange
    | KwCheck
    | KwColumn
    | KwCommit
    | KwConflict
    | KwConstraint
    | KwConversion
    | KwCopy
    | KwCount
    | KwCreate
    | KwCross
    | KwDatabase
    | KwDefault
    | KwDeferrable
    | KwDeferred
    | KwDelete
    | KwDesc
    | KwDetach
    | KwDistinct
    | KwDo
    | KwDrop
    | KwDuplicate
    | KwEach
    | KwElse
    | KwElseif
    | KwEnd
    | KwEngine
    | KwExcept
    | KwExclude
    | KwExclusive
    | KwExists
    | KwExplain
    | KwFalse
    | KwFetch
    | KwFilter
    | KwFinal
    | KwFlush
    | KwFor
    | KwForeign
    | KwFrom
    | KwFull
    | KwFulltext
    | KwFunction
    | KwGlob
    | KwGroup
    | KwHaving
    | KwHugeint
    | KwIf
    | KwIlike
    | KwImmediate
    | KwIn
    | KwIndex
    | KwIndexed
    | KwInherits
    | KwInner
    | KwInsert
    | KwInt
    | KwInteger
    | KwIntersect
    | KwInto
    | KwIs
    | KwIsolation
    | KwJoin
    | KwKey
    | KwLateral
    | KwLeft
    | KwLike
    | KwLimit
    | KwLock
    | KwLoop
    | KwMacro
    | KwMaterialized
    | KwMerge
    | KwModify
    | KwNatural
    | KwNo
    | KwNot
    | KwNull
    | KwOffset
    | KwOn
    | KwOnly
    | KwOptimize
    | KwOr
    | KwOrder
    | KwOuter
    | KwOver
    | KwPartition
    | KwPivot
    | KwPragma
    | KwPrepare
    | KwPrewhere
    | KwPrimary
    | KwProcedure
    | KwQualify
    | KwRead
    | KwRecursive
    | KwReferences
    | KwReindex
    | KwRelease
    | KwRename
    | KwRepeat
    | KwReplace
    | KwRestrict
    | KwReturn
    | KwReturning
    | KwRight
    | KwRollback
    | KwRow
    | KwRule
    | KwSavepoint
    | KwSchema
    | KwSelect
    | KwSequence
    | KwSet
    | KwSettings
    | KwShare
    | KwSmallint
    | KwStart
    | KwStatistics
    | KwSystemTime
    | KwTable
    | KwTemp
    | KwTemporary
    | KwThen
    | KwTinyint
    | KwTo
    | KwTransaction
    | KwTrigger
    | KwTrue
    | KwTruncate
    | KwTtl
    | KwUnion
    | KwUnique
    | KwUnlock
    | KwUnpivot
    | KwUntil
    | KwUpdate
    | KwUsing
    | KwVacuum
    | KwValues
    | KwVarchar
    | KwVariadic
    | KwView
    | KwVirtual
    | KwWhen
    | KwWhere
    | KwWhile
    | KwWindow
    | KwWith
    | KwWork
    | KwXor
    | KwDialect !ByteString
    deriving (Eq, Ord, Show)

-- | Uppercase ASCII spelling of a 'Keyword'.
keywordText :: Keyword -> ByteString
keywordText = \case
    KwAdd -> "ADD"
    KwAlgorithm -> "ALGORITHM"
    KwAll -> "ALL"
    KwAlter -> "ALTER"
    KwAnalyze -> "ANALYZE"
    KwAnd -> "AND"
    KwAny -> "ANY"
    KwArray -> "ARRAY"
    KwAs -> "AS"
    KwAsc -> "ASC"
    KwAsof -> "ASOF"
    KwAtomic -> "ATOMIC"
    KwAttach -> "ATTACH"
    KwAutoincrement -> "AUTOINCREMENT"
    KwBegin -> "BEGIN"
    KwBetween -> "BETWEEN"
    KwBigint -> "BIGINT"
    KwBoolean -> "BOOLEAN"
    KwBy -> "BY"
    KwCascade -> "CASCADE"
    KwCase -> "CASE"
    KwCast -> "CAST"
    KwChange -> "CHANGE"
    KwCheck -> "CHECK"
    KwColumn -> "COLUMN"
    KwCommit -> "COMMIT"
    KwConflict -> "CONFLICT"
    KwConstraint -> "CONSTRAINT"
    KwConversion -> "CONVERSION"
    KwCopy -> "COPY"
    KwCount -> "COUNT"
    KwCreate -> "CREATE"
    KwCross -> "CROSS"
    KwDatabase -> "DATABASE"
    KwDefault -> "DEFAULT"
    KwDeferrable -> "DEFERRABLE"
    KwDeferred -> "DEFERRED"
    KwDelete -> "DELETE"
    KwDesc -> "DESC"
    KwDetach -> "DETACH"
    KwDistinct -> "DISTINCT"
    KwDo -> "DO"
    KwDrop -> "DROP"
    KwDuplicate -> "DUPLICATE"
    KwEach -> "EACH"
    KwElse -> "ELSE"
    KwElseif -> "ELSEIF"
    KwEnd -> "END"
    KwEngine -> "ENGINE"
    KwExcept -> "EXCEPT"
    KwExclude -> "EXCLUDE"
    KwExclusive -> "EXCLUSIVE"
    KwExists -> "EXISTS"
    KwExplain -> "EXPLAIN"
    KwFalse -> "FALSE"
    KwFetch -> "FETCH"
    KwFilter -> "FILTER"
    KwFinal -> "FINAL"
    KwFlush -> "FLUSH"
    KwFor -> "FOR"
    KwForeign -> "FOREIGN"
    KwFrom -> "FROM"
    KwFull -> "FULL"
    KwFulltext -> "FULLTEXT"
    KwFunction -> "FUNCTION"
    KwGlob -> "GLOB"
    KwGroup -> "GROUP"
    KwHaving -> "HAVING"
    KwHugeint -> "HUGEINT"
    KwIf -> "IF"
    KwIlike -> "ILIKE"
    KwImmediate -> "IMMEDIATE"
    KwIn -> "IN"
    KwIndex -> "INDEX"
    KwIndexed -> "INDEXED"
    KwInherits -> "INHERITS"
    KwInner -> "INNER"
    KwInsert -> "INSERT"
    KwInt -> "INT"
    KwInteger -> "INTEGER"
    KwIntersect -> "INTERSECT"
    KwInto -> "INTO"
    KwIs -> "IS"
    KwIsolation -> "ISOLATION"
    KwJoin -> "JOIN"
    KwKey -> "KEY"
    KwLateral -> "LATERAL"
    KwLeft -> "LEFT"
    KwLike -> "LIKE"
    KwLimit -> "LIMIT"
    KwLock -> "LOCK"
    KwLoop -> "LOOP"
    KwMacro -> "MACRO"
    KwMaterialized -> "MATERIALIZED"
    KwMerge -> "MERGE"
    KwModify -> "MODIFY"
    KwNatural -> "NATURAL"
    KwNo -> "NO"
    KwNot -> "NOT"
    KwNull -> "NULL"
    KwOffset -> "OFFSET"
    KwOn -> "ON"
    KwOnly -> "ONLY"
    KwOptimize -> "OPTIMIZE"
    KwOr -> "OR"
    KwOrder -> "ORDER"
    KwOuter -> "OUTER"
    KwOver -> "OVER"
    KwPartition -> "PARTITION"
    KwPivot -> "PIVOT"
    KwPragma -> "PRAGMA"
    KwPrepare -> "PREPARE"
    KwPrewhere -> "PREWHERE"
    KwPrimary -> "PRIMARY"
    KwProcedure -> "PROCEDURE"
    KwQualify -> "QUALIFY"
    KwRead -> "READ"
    KwRecursive -> "RECURSIVE"
    KwReferences -> "REFERENCES"
    KwReindex -> "REINDEX"
    KwRelease -> "RELEASE"
    KwRename -> "RENAME"
    KwRepeat -> "REPEAT"
    KwReplace -> "REPLACE"
    KwRestrict -> "RESTRICT"
    KwReturn -> "RETURN"
    KwReturning -> "RETURNING"
    KwRight -> "RIGHT"
    KwRollback -> "ROLLBACK"
    KwRow -> "ROW"
    KwRule -> "RULE"
    KwSavepoint -> "SAVEPOINT"
    KwSchema -> "SCHEMA"
    KwSelect -> "SELECT"
    KwSequence -> "SEQUENCE"
    KwSet -> "SET"
    KwSettings -> "SETTINGS"
    KwShare -> "SHARE"
    KwSmallint -> "SMALLINT"
    KwStart -> "START"
    KwStatistics -> "STATISTICS"
    KwSystemTime -> "SYSTEM_TIME"
    KwTable -> "TABLE"
    KwTemp -> "TEMP"
    KwTemporary -> "TEMPORARY"
    KwThen -> "THEN"
    KwTinyint -> "TINYINT"
    KwTo -> "TO"
    KwTransaction -> "TRANSACTION"
    KwTrigger -> "TRIGGER"
    KwTrue -> "TRUE"
    KwTruncate -> "TRUNCATE"
    KwTtl -> "TTL"
    KwUnion -> "UNION"
    KwUnique -> "UNIQUE"
    KwUnlock -> "UNLOCK"
    KwUnpivot -> "UNPIVOT"
    KwUntil -> "UNTIL"
    KwUpdate -> "UPDATE"
    KwUsing -> "USING"
    KwVacuum -> "VACUUM"
    KwValues -> "VALUES"
    KwVarchar -> "VARCHAR"
    KwVariadic -> "VARIADIC"
    KwView -> "VIEW"
    KwVirtual -> "VIRTUAL"
    KwWhen -> "WHEN"
    KwWhere -> "WHERE"
    KwWhile -> "WHILE"
    KwWindow -> "WINDOW"
    KwWith -> "WITH"
    KwWork -> "WORK"
    KwXor -> "XOR"
    KwDialect raw -> raw

coreKeywords :: [Keyword]
coreKeywords =
    [ KwAdd
    , KwAll
    , KwAlter
    , KwAnd
    , KwAny
    , KwAs
    , KwAsc
    , KwAtomic
    , KwBegin
    , KwBetween
    , KwBigint
    , KwBoolean
    , KwBy
    , KwCascade
    , KwCase
    , KwCast
    , KwCheck
    , KwColumn
    , KwCommit
    , KwConstraint
    , KwCount
    , KwCreate
    , KwCross
    , KwDatabase
    , KwDefault
    , KwDeferrable
    , KwDeferred
    , KwDelete
    , KwDesc
    , KwDistinct
    , KwDo
    , KwDrop
    , KwEach
    , KwElse
    , KwElseif
    , KwEnd
    , KwExcept
    , KwExclusive
    , KwExists
    , KwExplain
    , KwFalse
    , KwFetch
    , KwFilter
    , KwFor
    , KwForeign
    , KwFrom
    , KwFull
    , KwFunction
    , KwGroup
    , KwHaving
    , KwHugeint
    , KwIf
    , KwImmediate
    , KwIn
    , KwIndex
    , KwInner
    , KwInsert
    , KwInt
    , KwInteger
    , KwIntersect
    , KwInto
    , KwIs
    , KwIsolation
    , KwJoin
    , KwKey
    , KwLeft
    , KwLike
    , KwLimit
    , KwLoop
    , KwNatural
    , KwNo
    , KwNot
    , KwNull
    , KwOffset
    , KwOn
    , KwOr
    , KwOrder
    , KwOuter
    , KwOver
    , KwPartition
    , KwPrimary
    , KwProcedure
    , KwRead
    , KwRecursive
    , KwReferences
    , KwRelease
    , KwRename
    , KwRepeat
    , KwReplace
    , KwRestrict
    , KwReturn
    , KwRight
    , KwRollback
    , KwRow
    , KwSavepoint
    , KwSchema
    , KwSelect
    , KwSet
    , KwShare
    , KwSmallint
    , KwStart
    , KwSystemTime
    , KwTable
    , KwTemp
    , KwTemporary
    , KwThen
    , KwTinyint
    , KwTo
    , KwTransaction
    , KwTrigger
    , KwTrue
    , KwTruncate
    , KwUnion
    , KwUnique
    , KwUntil
    , KwUpdate
    , KwUsing
    , KwValues
    , KwVarchar
    , KwView
    , KwWhen
    , KwWhere
    , KwWhile
    , KwWindow
    , KwWith
    , KwWork
    ]

dialectKeywordConstructors :: [Keyword]
dialectKeywordConstructors =
    [ KwAlgorithm
    , KwAnalyze
    , KwArray
    , KwAsof
    , KwAttach
    , KwAutoincrement
    , KwChange
    , KwConflict
    , KwConversion
    , KwCopy
    , KwDetach
    , KwDuplicate
    , KwEngine
    , KwExclude
    , KwFinal
    , KwFlush
    , KwFulltext
    , KwGlob
    , KwIlike
    , KwIndexed
    , KwInherits
    , KwLateral
    , KwLock
    , KwMacro
    , KwMaterialized
    , KwMerge
    , KwModify
    , KwOnly
    , KwOptimize
    , KwPivot
    , KwPragma
    , KwPrepare
    , KwPrewhere
    , KwQualify
    , KwReindex
    , KwReturning
    , KwRule
    , KwSequence
    , KwSettings
    , KwStatistics
    , KwTtl
    , KwUnlock
    , KwUnpivot
    , KwVacuum
    , KwVariadic
    , KwVirtual
    , KwXor
    ]

coreKeywordMap :: HashMap ByteString Keyword
coreKeywordMap = HashMap.fromList [(keywordText kw, kw) | kw <- coreKeywords]

knownDialectKeywordMap :: HashMap ByteString Keyword
knownDialectKeywordMap = HashMap.fromList [(keywordText kw, kw) | kw <- dialectKeywordConstructors]

buildKeywordTable :: DialectProfile -> HashMap ByteString Keyword
buildKeywordTable profile =
    Set.foldl' insertDialectKw coreKeywordMap (dialectKeywords profile)
    where
        insertDialectKw acc kwText =
            let !bs = asciiUpperBS (TE.encodeUtf8 (T.toUpper kwText))
                !kw = HashMap.findWithDefault (KwDialect bs) bs knownDialectKeywordMap
             in HashMap.insert bs kw acc

pgKeywordTable
    , mysqlKeywordTable
    , mariadbKeywordTable
    , sqliteKeywordTable
    , duckdbKeywordTable
    , clickhouseKeywordTable ::
        HashMap ByteString Keyword
pgKeywordTable = buildKeywordTable postgresqlProfile
mysqlKeywordTable = buildKeywordTable mysqlProfile
mariadbKeywordTable = buildKeywordTable mariadbProfile
sqliteKeywordTable = buildKeywordTable sqliteProfile
duckdbKeywordTable = buildKeywordTable duckdbProfile
clickhouseKeywordTable = buildKeywordTable clickhouseProfile

-- A pointer miss also covers a caller-supplied keyword set, which must be
-- compiled from that profile. Equality is only a fast path for shared sets.
{-# INLINE keywordTableFor #-}
keywordTableFor :: DialectProfile -> HashMap ByteString Keyword
keywordTableFor profile
    | isTrue# (reallyUnsafePtrEquality# keywords canonicalKeywords) = case d of
        PostgreSQL -> pgKeywordTable
        MySQL -> mysqlKeywordTable
        MariaDB -> mariadbKeywordTable
        SQLite -> sqliteKeywordTable
        DuckDB -> duckdbKeywordTable
        ClickHouse -> clickhouseKeywordTable
    | otherwise = buildKeywordTable profile
    where
        !d = dialectId profile
        !keywords = dialectKeywords profile
        !canonicalKeywords = dialectKeywords (dialectProfile d)

-- | Identifier quote style.
data QuoteKind
    = QuoteDouble
    | QuoteBacktick
    | QuoteBracket
    deriving (Eq, Ord, Show)

-- | An identifier or keyword token, retaining its original raw slice and quote style.
data WordToken = WordToken
    { wordRaw :: !ByteString
    , wordQuote :: !(Maybe QuoteKind)
    , wordKeyword :: !(Maybe Keyword)
    }
    deriving (Eq, Ord, Show)

-- | Literal token classification.
data LiteralKind
    = LitNumber
    | LitString
    | LitEscapeString
    | LitDollarString
    | LitHexString
    | LitBitString
    | LitNationalString
    deriving (Eq, Ord, Show, Data)

-- | Trivia classification preserved in the token stream.
data TriviaKind
    = TriviaWhitespace
    | TriviaLineComment
    | TriviaBlockComment
    | TriviaConditionalComment
    deriving (Eq, Ord, Show)

-- | Lexical token kind.
data Token
    = TokTrivia !TriviaKind
    | TokWord !WordToken
    | TokLiteral !LiteralKind !ByteString
    | TokParam !ByteString
    | TokUserVar !ByteString
    | TokSysVar !ByteString
    | TokLParen
    | TokRParen
    | TokLBracket
    | TokRBracket
    | TokLBrace
    | TokRBrace
    | TokComma
    | TokSemicolon
    | TokDot
    | TokDoubleColon
    | TokDotColon
    | TokColonEq
    | TokColon
    | TokArrow
    | TokDoubleArrow
    | TokPlus
    | TokMinus
    | TokStar
    | TokSlash
    | TokEq
    | TokQuestion
    | TokOperator !ByteString
    | TokUnknown !ByteString
    deriving (Eq, Ord, Show)

-- | Token plus its exact source span.
type SpannedToken = Located Token

-- | Lexical failure class.
data LexErrorKind
    = LexInvalidUtf8
    | LexUnterminatedString
    | LexUnterminatedDollarString
    | LexUnterminatedIdent
    | LexUnterminatedComment
    deriving (Eq, Ord, Show)

-- | Lexical failure at the byte offset where the construct starts.
data LexError = LexError
    { lexErrorKind :: !LexErrorKind
    , lexErrorOffset :: !Int
    , lexErrorMessage :: !Text
    }
    deriving (Eq, Ord, Show)

-- | Tokens scanned before an error, plus the first error if any.
data LexResult = LexResult
    { lexTokens :: !(Vector SpannedToken)
    , lexError :: !(Maybe LexError)
    }
    deriving (Eq, Show)

-- | True for whitespace, comments, and conditional comments.
{-# INLINE isTriviaToken #-}
isTriviaToken :: Token -> Bool
isTriviaToken (TokTrivia _) = True
isTriviaToken _ = False

-- | True only for whitespace trivia.
{-# INLINE isWhitespaceToken #-}
isWhitespaceToken :: Token -> Bool
isWhitespaceToken (TokTrivia TriviaWhitespace) = True
isWhitespaceToken _ = False

-- | True for line, block, and conditional comments.
{-# INLINE isCommentToken #-}
isCommentToken :: Token -> Bool
isCommentToken (TokTrivia TriviaWhitespace) = False
isCommentToken (TokTrivia _) = True
isCommentToken _ = False

-- | True for an unquoted keyword token equal to @expected@.
{-# INLINE isKeywordToken #-}
isKeywordToken :: Keyword -> Token -> Bool
isKeywordToken expected (TokWord WordToken {wordQuote = Nothing, wordKeyword = Just found}) =
    found == expected
isKeywordToken _ _ = False

-- | Strict lexer returning 'Left' on any lexical error (including invalid UTF-8).
lexSQLStrict :: DialectProfile -> ByteString -> Either LexError (Vector SpannedToken)
lexSQLStrict profile bs =
    let res = lexSQL profile bs
     in case lexError res of
            Just err -> Left err
            Nothing -> Right (lexTokens res)

{- | Single-pass UTF-8 'ByteString' lexer driven by 'DialectProfile'.
On invalid UTF-8, returns empty tokens and 'LexInvalidUtf8' at the offending start byte.
On an unterminated literal/comment/identifier, returns all valid preceding tokens
alongside 'Just LexError' pointing to the opening byte offset of the unterminated construct.
-}
lexSQL :: DialectProfile -> ByteString -> LexResult
lexSQL profile bs = case validateUtf8 bs of
    Left badOffset ->
        LexResult
            V.empty
            (Just (LexError LexInvalidUtf8 badOffset "SQL input is not valid UTF-8"))
    Right () -> runST (scanAll profile kwTable bs)
    where
        !kwTable = keywordTableFor profile

scanAll :: forall s. DialectProfile -> HashMap ByteString Keyword -> ByteString -> ST s LexResult
scanAll profile kwTable bs = do
    let !len = BS.length bs
        !lp = dialectLexical profile
        !initCap = max 16 (min 16384 (len `div` 3))
    bufRef <- MV.new initCap >>= newSTRef
    sizeRef <- newSTRef (0 :: Int)

    let pushTok !tok = do
            buf <- readSTRef bufRef
            idx <- readSTRef sizeRef
            let !cap = MV.length buf
            buf' <-
                if idx < cap
                    then pure buf
                    else do
                        grown <- MV.grow buf cap
                        writeSTRef bufRef grown
                        pure grown
            MV.write buf' idx tok
            writeSTRef sizeRef (idx + 1)

        finishWith err = do
            buf <- readSTRef bufRef
            sz <- readSTRef sizeRef
            vec <- V.unsafeFreeze (MV.slice 0 sz buf)
            pure (LexResult vec err)

        previousToken = do
            sz <- readSTRef sizeRef
            if sz == 0
                then pure (TokTrivia TriviaWhitespace)
                else do
                    buf <- readSTRef bufRef
                    locValue <$> MV.unsafeRead buf (sz - 1)

        {-# INLINE emit #-}
        emit !s !e !tok = pushTok (L (ByteSpan s e) tok)

        {-# INLINE emitSpan #-}
        emitSpan !s !e !tok = emit s e tok *> loop e

        {-# INLINE emitOp #-}
        emitOp !s !k = emitSpan s (s + k) (TokOperator (slice s (s + k)))

        failLex kind off msg = finishWith (Just (LexError kind off msg))

        scanEmit ::
            Int ->
            LexErrorKind ->
            Text ->
            Maybe Int ->
            (Int -> Token) ->
            ST s LexResult
        scanEmit startOff errKind errMsg mEnd mkTok = case mEnd of
            Just endPos -> emitSpan startOff endPos (mkTok endPos)
            Nothing -> failLex errKind startOff errMsg

        {-# INLINE slice #-}
        slice !s !e = unsafeSliceSpan (ByteSpan s e) bs

        {-# INLINE at #-}
        at !idx = BSU.unsafeIndex bs idx

        {-# INLINE hasByteAt #-}
        hasByteAt !idx !b = idx < len && at idx == b

        {-# INLINE byteAt #-}
        byteAt !idx
            | idx < len = Just (at idx)
            | otherwise = Nothing

        loop :: Int -> ST s LexResult
        loop !i
            | i >= len = finishWith Nothing
            | isAsciiWhitespace b0 =
                let !endPos = scanWhile isAsciiWhitespace (i + 1)
                 in emitSpan i endPos (TokTrivia TriviaWhitespace)
            -- Line comments: --
            | b0 == 45 && hasByteAt (i + 1) 45 =
                emitSpan i (scanLineComment (i + 2)) (TokTrivia TriviaLineComment)
            -- Hash line comments: # (MySQL / MariaDB)
            | b0 == 35 && lexHashComments lp =
                emitSpan i (scanLineComment (i + 1)) (TokTrivia TriviaLineComment)
            -- Conditional & block comments: /*! ... */ and /* ... */
            | b0 == 47 && hasByteAt (i + 1) 42 =
                if lexConditionalComments lp && hasByteAt (i + 2) 33
                    then
                        scanEmit
                            i
                            LexUnterminatedComment
                            "unterminated conditional block comment"
                            (scanSimpleBlockComment (i + 3))
                            (const (TokTrivia TriviaConditionalComment))
                    else
                        let mEnd =
                                if lexNestedBlockComments lp
                                    then scanNestedBlockComment (1 :: Int) (i + 2)
                                    else scanSimpleBlockComment (i + 2)
                         in scanEmit
                                i
                                LexUnterminatedComment
                                "unterminated block comment"
                                mEnd
                                (const (TokTrivia TriviaBlockComment))
            -- Prefix strings: E'...', X'...', B'...', N'...'
            | (b0 == 69 || b0 == 101) && lexEscapeStrings lp && hasByteAt (i + 1) 39 =
                scanEmit
                    i
                    LexUnterminatedString
                    "unterminated escape string literal"
                    (scanSingleQuoted True (i + 2))
                    (TokLiteral LitEscapeString . slice i)
            | (b0 == 88 || b0 == 120) && hasByteAt (i + 1) 39 =
                scanEmit
                    i
                    LexUnterminatedString
                    "unterminated hex string literal"
                    (scanSingleQuoted False (i + 2))
                    (TokLiteral LitHexString . slice i)
            | (b0 == 66 || b0 == 98) && hasByteAt (i + 1) 39 =
                scanEmit
                    i
                    LexUnterminatedString
                    "unterminated bit string literal"
                    (scanSingleQuoted False (i + 2))
                    (TokLiteral LitBitString . slice i)
            | (b0 == 78 || b0 == 110) && hasByteAt (i + 1) 39 =
                scanEmit
                    i
                    LexUnterminatedString
                    "unterminated national string literal"
                    (scanSingleQuoted (lexBackslashEscapesInSingleQuote lp) (i + 2))
                    (TokLiteral LitNationalString . slice i)
            -- Single-quoted string literal
            | b0 == 39 =
                scanEmit
                    i
                    LexUnterminatedString
                    "unterminated single-quoted string literal"
                    (scanSingleQuoted (lexBackslashEscapesInSingleQuote lp) (i + 1))
                    (TokLiteral LitString . slice i)
            -- Double-quoted identifier
            | b0 == 34 =
                scanEmit
                    i
                    LexUnterminatedIdent
                    "unterminated double-quoted identifier"
                    (scanDoubleQuoted (lexBackslashEscapesInSingleQuote lp) (i + 1))
                    (\e -> TokWord (WordToken (slice i e) (Just QuoteDouble) Nothing))
            -- Backtick-quoted identifier (when enabled)
            | b0 == 96 && lexBacktickQuotes lp =
                scanEmit
                    i
                    LexUnterminatedIdent
                    "unterminated backtick-quoted identifier"
                    (scanBacktickQuoted (i + 1))
                    (\e -> TokWord (WordToken (slice i e) (Just QuoteBacktick) Nothing))
            -- Bracket-quoted identifier (SQLite)
            | b0 == 91 && lexBracketQuotes lp =
                scanEmit
                    i
                    LexUnterminatedIdent
                    "unterminated bracket-quoted identifier"
                    (scanBracketQuoted (i + 1))
                    (\e -> TokWord (WordToken (slice i e) (Just QuoteBracket) Nothing))
            -- Dollar-quoting $$tag$$ or $1 / $name parameters
            | b0 == 36 = scanDollar i
            -- User / system variables (@user / @@sys) or SQLite @param or operators
            | b0 == 64 = scanAt i
            -- Numbers (starting with digit or '.' followed by digit)
            | isAsciiDigit b0 = do
                fieldIndex <-
                    if i > 0 && hasByteAt (i - 1) 46
                        then (== TokDot) <$> previousToken
                        else pure False
                let !endPos = if fieldIndex then scanWhile isAsciiDigit (i + 1) else scanNumber i
                emitSpan i endPos (TokLiteral LitNumber (slice i endPos))
            | b0 == 46
            , Just b1 <- byteAt (i + 1)
            , isAsciiDigit b1 = do
                prev <- previousToken
                if blocksLeadingDot prev
                    then emitSpan i (i + 1) TokDot
                    else
                        let !endPos = scanNumberFraction (i + 1)
                         in emitSpan i endPos (TokLiteral LitNumber (slice i endPos))
            -- Unquoted identifier or keyword (ASCII start)
            | isAsciiIdentStart b0 =
                let !endPos = scanWordCont (i + 1)
                    !raw = slice i endPos
                    !mKw = lookupKeyword kwTable raw
                 in emitSpan i endPos (TokWord (WordToken raw Nothing mKw))
            -- Unquoted Unicode identifier (non-ASCII alphabetic lead byte)
            | b0 >= 0x80 && isUtf8IdentStart i =
                let !charLen = utf8CharLen b0
                    !endPos = scanWordCont (i + charLen)
                    !raw = slice i endPos
                 in emitSpan i endPos (TokWord (WordToken raw Nothing Nothing))
            -- Operators and punctuation
            | otherwise = scanPunctuation i b0
            where
                b0 = at i

        scanWhile p !idx
            | idx < len && p (at idx) = scanWhile p (idx + 1)
            | otherwise = idx

        scanLineComment !idx
            | idx >= len = len
            | at idx == 10 = idx
            | otherwise = scanLineComment (idx + 1)

        scanSimpleBlockComment !idx =
            let !rest = BSU.unsafeDrop idx bs
                !(before, match) = BS.breakSubstring "*/" rest
             in if BS.null match
                    then Nothing
                    else Just (idx + BS.length before + 2)

        scanNestedBlockComment !depth !idx
            | idx >= len = Nothing
            | at idx == 47 && hasByteAt (idx + 1) 42 =
                scanNestedBlockComment (depth + 1) (idx + 2)
            | at idx == 42 && hasByteAt (idx + 1) 47 =
                if depth == 1
                    then Just (idx + 2)
                    else scanNestedBlockComment (depth - 1) (idx + 2)
            | otherwise = scanNestedBlockComment depth (idx + 1)

        scanDelimited !closeByte !allowDoubled !allowBackslash = go
            where
                go !idx
                    | idx >= len = Nothing
                    | b == closeByte =
                        if allowDoubled && hasByteAt (idx + 1) closeByte
                            then go (idx + 2)
                            else Just (idx + 1)
                    | allowBackslash && b == 92 =
                        if idx + 1 >= len
                            then Nothing
                            else go (idx + 1 + utf8CharLen (at (idx + 1)))
                    | otherwise = go (idx + utf8CharLen b)
                    where
                        b = at idx

        scanSingleQuoted !allowBackslash = scanDelimited 39 True allowBackslash
        scanDoubleQuoted !allowBackslash = scanDelimited 34 True allowBackslash
        scanBacktickQuoted = scanDelimited 96 True False
        scanBracketQuoted = scanDelimited 93 False False

        scanDollar !i
            | lexDollarQuoting lp && hasByteAt (i + 1) 36 =
                scanDollarBody i (i + 2) (slice i (i + 2))
            | lexDollarQuoting lp
            , Just b1 <- byteAt (i + 1)
            , isAsciiDigit b1 =
                let !endPos = scanWhile isAsciiDigit (i + 1)
                 in if lexDollarParameters lp
                        then emitSpan i endPos (TokParam (slice i endPos))
                        else emitOp i 1
            | lexDollarQuoting lp && i + 1 < len && isDollarTagStart (i + 1) =
                let !tagStartLen = utf8CharLen (at (i + 1))
                    !tagEnd = scanDollarTagCont (i + 1 + tagStartLen)
                 in if hasByteAt tagEnd 36
                        then scanDollarBody i (tagEnd + 1) (slice i (tagEnd + 1))
                        else
                            if lexDollarParameters lp && (lexNamedParameters lp || dialectId profile == DuckDB)
                                then emitSpan i tagEnd (TokParam (slice i tagEnd))
                                else emitOp i 1
            | lexDollarParameters lp
            , Just b1 <- byteAt (i + 1)
            , isParamIdentCont b1 =
                let !endPos = scanWhile isParamIdentCont (i + 1)
                 in emitSpan i endPos (TokParam (slice i endPos))
            | otherwise = emitOp i 1

        scanDollarBody !openOffset !bodyStart !delim =
            let !rest = BSU.unsafeDrop bodyStart bs
                !(before, match) = BS.breakSubstring delim rest
             in if BS.null match
                    then failLex LexUnterminatedDollarString openOffset "unterminated dollar-quoted string literal"
                    else
                        let !endPos = bodyStart + BS.length before + BS.length delim
                         in emitSpan openOffset endPos (TokLiteral LitDollarString (slice openOffset endPos))

        isDollarTagStart !idx =
            let !b = at idx
             in isAsciiIdentStart b || (b >= 0x80 && isUtf8IdentStart idx)

        scanDollarTagCont !idx
            | idx >= len = idx
            | isAsciiIdentContNoDollar (at idx) = scanDollarTagCont (idx + 1)
            | at idx >= 0x80 && isUtf8IdentCont idx = scanDollarTagCont (idx + utf8CharLen (at idx))
            | otherwise = idx

        scanAt !i
            | lexUserVariables lp && hasByteAt (i + 1) 64 =
                let !nameStart = i + 2
                 in case scanSysVar nameStart of
                        Just endPos
                            | endPos == nameStart -> emitOp i 2
                            | otherwise -> emitSpan i endPos (TokSysVar (slice i endPos))
                        Nothing -> failLex LexUnterminatedIdent i "unterminated quoted system variable"
            | lexUserVariables lp && hasByteAt (i + 1) 39 =
                scanEmit
                    i
                    LexUnterminatedString
                    "unterminated quoted user variable"
                    (scanSingleQuoted (lexBackslashEscapesInSingleQuote lp) (i + 2))
                    (TokUserVar . slice i)
            | lexUserVariables lp && hasByteAt (i + 1) 34 =
                scanEmit
                    i
                    LexUnterminatedIdent
                    "unterminated quoted user variable"
                    (scanDoubleQuoted (lexBackslashEscapesInSingleQuote lp) (i + 2))
                    (TokUserVar . slice i)
            | lexUserVariables lp && hasByteAt (i + 1) 96 =
                scanEmit
                    i
                    LexUnterminatedIdent
                    "unterminated quoted user variable"
                    (scanBacktickQuoted (i + 2))
                    (TokUserVar . slice i)
            | lexUserVariables lp
            , Just b1 <- byteAt (i + 1)
            , isUserVarStart b1 =
                let !endPos = scanWhile isUserVarCont (i + 2)
                 in emitSpan i endPos (TokUserVar (slice i endPos))
            | lexUserVariables lp = emitOp i 1
            | lexNamedParameters lp
            , Just b1 <- byteAt (i + 1)
            , isAsciiIdentStart b1 =
                let !endPos = scanWhile isParamIdentCont (i + 1)
                 in emitSpan i endPos (TokParam (slice i endPos))
            | hasByteAt (i + 1) 45 && hasByteAt (i + 2) 64 = emitOp i 3
            | hasByteAt (i + 1) 64 || hasByteAt (i + 1) 62 = emitOp i 2
            | otherwise = emitOp i 1

        scanSysVar !idx
            | lexBacktickQuotes lp && hasByteAt idx 96 = do
                end <- scanBacktickQuoted (idx + 1)
                if hasByteAt end 46
                    then scanSysVar (end + 1)
                    else Just end
            | otherwise =
                let !end = scanWhile isSysVarCont idx
                 in if end > idx && hasByteAt (end - 1) 46 && lexBacktickQuotes lp && hasByteAt end 96
                        then scanSysVar end
                        else Just end

        scanNumber !i
            | at i == 48
            , hasByteAt (i + 1) 120 || hasByteAt (i + 1) 88
            , Just b2 <- byteAt (i + 2)
            , isAsciiHexDigit b2 =
                scanWhile isAsciiHexDigit (i + 2)
            | at i == 48
            , hasByteAt (i + 1) 98 || hasByteAt (i + 1) 66
            , Just b2 <- byteAt (i + 2)
            , isAsciiBinDigit b2 =
                scanWhile isAsciiBinDigit (i + 2)
            | otherwise =
                let !intEnd = scanWhile isAsciiDigit (i + 1)
                 in if hasByteAt intEnd 46 && not (hasByteAt (intEnd + 1) 46)
                        then scanNumberExponent (scanWhile isAsciiDigit (intEnd + 1))
                        else scanNumberExponent intEnd

        scanNumberFraction !digitStart =
            scanNumberExponent (scanWhile isAsciiDigit digitStart)

        scanNumberExponent !idx
            | hasByteAt idx 101 || hasByteAt idx 69 =
                let !afterExp =
                        if hasByteAt (idx + 1) 43 || hasByteAt (idx + 1) 45
                            then idx + 2
                            else idx + 1
                 in case byteAt afterExp of
                        Just b | isAsciiDigit b -> scanWhile isAsciiDigit afterExp
                        _ -> idx
            | otherwise = idx

        scanWordCont !idx
            | idx >= len = idx
            | isAsciiIdentCont (at idx) = scanWordCont (idx + 1)
            | at idx >= 0x80 && isUtf8IdentCont idx = scanWordCont (idx + utf8CharLen (at idx))
            | otherwise = idx

        isUtf8IdentStart !idx =
            let !(ch, _) = decodeUtf8Char bs idx
             in isAlpha ch

        isUtf8IdentCont !idx =
            let !(ch, _) = decodeUtf8Char bs idx
             in isAlphaNum ch || ch == '_'

        scanPunctuation !i !b0 = case b0 of
            40 -> emitSpan i (i + 1) TokLParen
            41 -> emitSpan i (i + 1) TokRParen
            91 -> emitSpan i (i + 1) TokLBracket
            93 -> emitSpan i (i + 1) TokRBracket
            123 -> emitSpan i (i + 1) TokLBrace
            125 -> emitSpan i (i + 1) TokRBrace
            44 -> emitSpan i (i + 1) TokComma
            59 -> emitSpan i (i + 1) TokSemicolon
            43 -> emitSpan i (i + 1) TokPlus
            47 -> emitSpan i (i + 1) TokSlash
            46
                | hasByteAt (i + 1) 58 -> emitSpan i (i + 2) TokDotColon
                | hasByteAt (i + 1) 46 -> emitOp i 2
                | otherwise -> emitSpan i (i + 1) TokDot
            58
                | hasByteAt (i + 1) 58 -> emitSpan i (i + 2) TokDoubleColon
                | hasByteAt (i + 1) 61 -> emitSpan i (i + 2) TokColonEq
                | lexNamedParameters lp
                , Just b1 <- byteAt (i + 1)
                , isAsciiIdentStart b1 ->
                    let !endPos = scanWhile isParamIdentCont (i + 1)
                     in emitSpan i endPos (TokParam (slice i endPos))
                | otherwise -> emitSpan i (i + 1) TokColon
            45
                | hasByteAt (i + 1) 62 && hasByteAt (i + 2) 62 -> emitSpan i (i + 3) TokDoubleArrow
                | hasByteAt (i + 1) 62 -> emitSpan i (i + 2) TokArrow
                | otherwise -> emitSpan i (i + 1) TokMinus
            42
                | hasByteAt (i + 1) 42 -> emitOp i 2
                | otherwise -> emitSpan i (i + 1) TokStar
            61
                | hasByteAt (i + 1) 61 || hasByteAt (i + 1) 62 -> emitOp i 2
                | otherwise -> emitSpan i (i + 1) TokEq
            60
                -- <=>, <<=, <<, <=, <>, <@, <->, <
                | (hasByteAt (i + 1) 61 && hasByteAt (i + 2) 62)
                    || (hasByteAt (i + 1) 60 && hasByteAt (i + 2) 61)
                    || (hasByteAt (i + 1) 45 && hasByteAt (i + 2) 62) ->
                    emitOp i 3
                | hasByteAt (i + 1) 61
                    || hasByteAt (i + 1) 62
                    || hasByteAt (i + 1) 60
                    || hasByteAt (i + 1) 64 ->
                    emitOp i 2
                | otherwise -> emitOp i 1
            62
                -- >>=, >>, >=, >
                | hasByteAt (i + 1) 62 && hasByteAt (i + 2) 61 -> emitOp i 3
                | hasByteAt (i + 1) 61 || hasByteAt (i + 1) 62 -> emitOp i 2
                | otherwise -> emitOp i 1
            33
                -- !~~*, !~*, !=, !~, !~~, !!, !
                | hasByteAt (i + 1) 126 && hasByteAt (i + 2) 126 && hasByteAt (i + 3) 42 -> emitOp i 4
                | (hasByteAt (i + 1) 126 && hasByteAt (i + 2) 42)
                    || (hasByteAt (i + 1) 126 && hasByteAt (i + 2) 126) ->
                    emitOp i 3
                | hasByteAt (i + 1) 61 || hasByteAt (i + 1) 126 || hasByteAt (i + 1) 33 -> emitOp i 2
                | otherwise -> emitOp i 1
            126
                -- ~~*, ~*, ~~, ~
                | hasByteAt (i + 1) 126 && hasByteAt (i + 2) 42 -> emitOp i 3
                | hasByteAt (i + 1) 42 || hasByteAt (i + 1) 126 -> emitOp i 2
                | otherwise -> emitOp i 1
            124
                -- \||/, ||, |/, |
                | hasByteAt (i + 1) 124 && hasByteAt (i + 2) 47 -> emitOp i 3
                | hasByteAt (i + 1) 124 || hasByteAt (i + 1) 47 -> emitOp i 2
                | otherwise -> emitOp i 1
            38
                | hasByteAt (i + 1) 38 -> emitOp i 2
                | otherwise -> emitOp i 1
            35
                -- #>>, #>, #-, # (only reachable when lexHashComments is False)
                | hasByteAt (i + 1) 62 && hasByteAt (i + 2) 62 -> emitOp i 3
                | hasByteAt (i + 1) 62 || hasByteAt (i + 1) 45 -> emitOp i 2
                | otherwise -> emitOp i 1
            63
                -- ?|, ?& (PostgreSQL JSONB) or ?123 (SQLite param) or ?
                | lexQuestionOperators lp && (hasByteAt (i + 1) 124 || hasByteAt (i + 1) 38) ->
                    emitOp i 2
                | lexNumberedQuestionParameters lp
                , Just b1 <- byteAt (i + 1)
                , isAsciiDigit b1 ->
                    let !endPos = scanWhile isAsciiDigit (i + 1)
                     in emitSpan i endPos (TokParam (slice i endPos))
                | otherwise -> emitSpan i (i + 1) TokQuestion
            37 -> emitOp i 1
            94 -> emitOp i 1
            _ ->
                let !endPos = min len (i + utf8CharLen b0)
                 in emitSpan i endPos (TokUnknown (slice i endPos))

        blocksLeadingDot = \case
            TokWord WordToken {wordKeyword = Just kw} ->
                kw
                    `notElem` [ KwSelect
                              , KwDefault
                              , KwDistinct
                              , KwAll
                              , KwWhere
                              , KwPrewhere
                              , KwHaving
                              , KwQualify
                              , KwWhen
                              , KwThen
                              , KwElse
                              , KwBy
                              , KwLimit
                              , KwOffset
                              , KwReturning
                              , KwOn
                              , KwAnd
                              , KwOr
                              , KwXor
                              , KwNot
                              , KwBetween
                              , KwLike
                              ]
            TokWord _ -> True
            TokLiteral LitNumber _ -> True
            TokRParen -> True
            TokRBracket -> True
            TokRBrace -> True
            TokDot -> True
            _ -> False
    loop 0

{-# INLINE isAsciiWhitespace #-}
isAsciiWhitespace :: Word8 -> Bool
isAsciiWhitespace b = b == 32 || b == 9 || b == 10 || b == 13 || b == 12

{-# INLINE isAsciiDigit #-}
isAsciiDigit :: Word8 -> Bool
isAsciiDigit b = b >= 48 && b <= 57

{-# INLINE isAsciiHexDigit #-}
isAsciiHexDigit :: Word8 -> Bool
isAsciiHexDigit b =
    (b >= 48 && b <= 57) || (b >= 65 && b <= 70) || (b >= 97 && b <= 102)

{-# INLINE isAsciiBinDigit #-}
isAsciiBinDigit :: Word8 -> Bool
isAsciiBinDigit b = b == 48 || b == 49

{-# INLINE isAsciiAlpha #-}
isAsciiAlpha :: Word8 -> Bool
isAsciiAlpha b = (b >= 65 && b <= 90) || (b >= 97 && b <= 122)

{-# INLINE isAsciiIdentStart #-}
isAsciiIdentStart :: Word8 -> Bool
isAsciiIdentStart b = isAsciiAlpha b || b == 95

{-# INLINE isAsciiIdentContNoDollar #-}
isAsciiIdentContNoDollar :: Word8 -> Bool
isAsciiIdentContNoDollar b = isAsciiIdentStart b || isAsciiDigit b

{-# INLINE isAsciiIdentCont #-}
isAsciiIdentCont :: Word8 -> Bool
isAsciiIdentCont b = isAsciiIdentContNoDollar b || b == 36

{-# INLINE isParamIdentCont #-}
isParamIdentCont :: Word8 -> Bool
isParamIdentCont = isAsciiIdentContNoDollar

{-# INLINE isUserVarStart #-}
isUserVarStart :: Word8 -> Bool
isUserVarStart b = isAsciiAlpha b || isAsciiDigit b || b == 95 || b == 36

{-# INLINE isUserVarCont #-}
isUserVarCont :: Word8 -> Bool
isUserVarCont b = isUserVarStart b || b == 46

{-# INLINE isSysVarCont #-}
isSysVarCont :: Word8 -> Bool
isSysVarCont b = isAsciiAlpha b || isAsciiDigit b || b == 95 || b == 46

{-# INLINE utf8CharLen #-}
utf8CharLen :: Word8 -> Int
utf8CharLen b
    | b < 0x80 = 1
    | b < 0xe0 = 2
    | b < 0xf0 = 3
    | otherwise = 4

-- | Decode a single UTF-8 codepoint at a validated UTF-8 boundary.
{-# INLINE decodeUtf8Char #-}
decodeUtf8Char :: ByteString -> Int -> (Char, Int)
decodeUtf8Char bs idx
    | b0 < 0x80 = (chr (fromIntegral b0), 1)
    | b0 < 0xe0 =
        let !cp = (fromIntegral (b0 .&. 0x1f) `unsafeShiftL` 6) .|. cont 1
         in (chr cp, 2)
    | b0 < 0xf0 =
        let !cp =
                (fromIntegral (b0 .&. 0x0f) `unsafeShiftL` 12)
                    .|. (cont 1 `unsafeShiftL` 6)
                    .|. cont 2
         in (chr cp, 3)
    | otherwise =
        let !cp =
                (fromIntegral (b0 .&. 0x07) `unsafeShiftL` 18)
                    .|. (cont 1 `unsafeShiftL` 12)
                    .|. (cont 2 `unsafeShiftL` 6)
                    .|. cont 3
         in (chr cp, 4)
    where
        !b0 = BSU.unsafeIndex bs idx
        {-# INLINE cont #-}
        cont !off = fromIntegral (BSU.unsafeIndex bs (idx + off) .&. 0x3f)

-- | ASCII-only uppercase, leaving non-ASCII bytes unchanged.
{-# INLINE asciiUpperBS #-}
asciiUpperBS :: ByteString -> ByteString
asciiUpperBS bs
    | BS.any (\b -> b >= 97 && b <= 122) bs = BS.map toUpperByte bs
    | otherwise = bs
    where
        toUpperByte b
            | b >= 97 && b <= 122 = b - 32
            | otherwise = b

-- | ASCII case-insensitive equality.
{-# INLINE asciiEqCI #-}
asciiEqCI :: ByteString -> ByteString -> Bool
asciiEqCI a b =
    BS.length a == BS.length b && go 0
    where
        !len = BS.length a
        go !i
            | i >= len = True
            | lower (BSU.unsafeIndex a i) == lower (BSU.unsafeIndex b i) = go (i + 1)
            | otherwise = False
        lower w
            | w >= 65 && w <= 90 = w + 32
            | otherwise = w

{-# INLINE lookupKeyword #-}
lookupKeyword :: HashMap ByteString Keyword -> ByteString -> Maybe Keyword
lookupKeyword table raw
    | BS.length raw > 20 = Nothing
    | BS.any (>= 0x80) raw = Nothing
    | otherwise = HashMap.lookup (asciiUpperBS raw) table
