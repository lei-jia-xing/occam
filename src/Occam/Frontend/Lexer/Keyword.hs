{-# LANGUAGE MagicHash #-}

-- | SQL keyword classifications, dialect tables, and ASCII helpers.
module Occam.Frontend.Lexer.Keyword (
    Keyword (..),
    keywordText,
    keywordTableFor,
    asciiUpperBS,
    asciiEqCI,
    lookupKeyword,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)
import Occam.Parser.Dialect (
    DialectProfile (..),
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
