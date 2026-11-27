-- | Public SQL parsing facade backed by the pure-Haskell strict frontend.
module Occam.Parser (
    SQLDialect (..),
    dialectName,
    CaseFolding (..),
    applyCaseFolding,
    LexicalProfile (..),
    IdentifierProfile (..),
    DialectFeature (..),
    NamespaceRule (..),
    IndexScopeRule (..),
    TriggerScopeRule (..),
    SemanticsProfile (..),
    DialectProfile (..),
    allDialectProfiles,
    dialectProfile,
    lookupDialectByName,
    dialectHasFeature,
    ServerConfig (..),
    defaultServerConfig,
    ParseOptions (..),
    defaultParseOptions,
    ParserError (..),
    ParserErrorKind (..),
    ParsedSQL (..),
    StatementUnit (..),
    StatementFacts (..),
    ParserEdit (..),
    ParserEditCategory (..),
    ClauseTier (..),
    ExprTier (..),
    EditTier (..),
    ColumnSite (..),
    ColumnSiteKind (..),
    ColumnTarget (..),
    parseSQL,
    parseSQLTimed,
    countStatements,
    spanOnlyOptions,
    nativeOracleCacheKey,
    nativeDuckDBReplayUnits,
    DuckDBReplayUnit (..),
    factDumpJson,
) where

import Control.Exception (evaluate)
import Data.ByteString (ByteString)
import Data.Maybe (isNothing)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Core (SemanticFacts (..))
import Occam.Facts (
    ClauseTier (..),
    ColumnSite (..),
    ColumnSiteKind (..),
    ColumnTarget (..),
    EditTier (..),
    ExprTier (..),
    ParserEdit (..),
    ParserEditCategory (..),
    StatementFacts (..),
    StatementUnit (..),
 )
import Occam.Frontend.Lexer (LexError (..))
import Occam.Frontend.OracleProof (
    DuckDBReplayUnit (..),
    duckdbReplayUnits,
    oracleCacheKey,
 )
import Occam.Frontend.Pipeline (parseFrontend)
import Occam.Frontend.Splitter (SplitUnit (..), splitSQL)
import Occam.Metrics (MetricsStore, ParseKind (..), recordParseTiming)
import Occam.Parser.Dialect (
    CaseFolding (..),
    DialectFeature (..),
    DialectProfile (..),
    IdentifierProfile (..),
    IndexScopeRule (..),
    LexicalProfile (..),
    NamespaceRule (..),
    SemanticsProfile (..),
    TriggerScopeRule (..),
    allDialectProfiles,
    applyCaseFolding,
    dialectHasFeature,
    dialectProfile,
    lookupDialectByName,
 )
import Occam.Parser.Facts (factDumpJson)
import Occam.Parser.Types (
    ParseOptions (..),
    ParsedSQL (..),
    ParserError (..),
    ParserErrorKind (..),
    SQLDialect (..),
    ServerConfig (..),
    defaultParseOptions,
    defaultServerConfig,
    dialectName,
 )

-- | Available only for native predicates and for source-independent result shapes.
nativeOracleCacheKey :: SQLDialect -> ByteString -> IO (Maybe ByteString)
nativeOracleCacheKey dialect sql = pure $! oracleCacheKey (dialectProfile dialect) sql

nativeDuckDBReplayUnits :: ByteString -> IO [DuckDBReplayUnit]
nativeDuckDBReplayUnits sql = pure $! duckdbReplayUnits sql

-- | Untimed parse; delegates to the single timed implementation.
parseSQL :: ParseOptions -> ByteString -> IO (Either ParserError ParsedSQL)
parseSQL = parseSQLTimed Nothing ParseStatementOnly

-- | Parse SQL via the pure-Haskell frontend with optional metrics recording.
parseSQLTimed ::
    Maybe MetricsStore -> ParseKind -> ParseOptions -> ByteString -> IO (Either ParserError ParsedSQL)
parseSQLTimed store kind options sql = do
    start <- clockIf store
    result <- evaluate (forceParsed (parseFrontend options sql))
    end <- clockIf store
    recordParseTiming store kind (microsBetween start end) 0
    pure result

clockIf :: Maybe MetricsStore -> IO Word64
clockIf Nothing = pure 0
clockIf (Just _) = getMonotonicTimeNSec

-- | Parse options for span-and-error-only parsing (no semantic facts or edits).
spanOnlyOptions :: SQLDialect -> ParseOptions
spanOnlyOptions dialect = (defaultParseOptions dialect) {parseSpanOnly = True}

-- | Count SQL statements in the input according to the chosen dialect.
countStatements :: SQLDialect -> ByteString -> IO (Either ParserError Int)
countStatements dialect sql =
    pure $!
        case splitSQL (dialectProfile dialect) sql of
            Left err -> Left (ParserError ParserInvalidUtf8 (lexErrorMessage err))
            Right units -> Right (foldl' countUnit 0 units)
    where
        countUnit total unit
            | isNothing (splitUnitDelimiterDirective unit) = total + 1
            | otherwise = total

microsBetween :: Word64 -> Word64 -> Int
microsBetween start end = fromIntegral ((end - start) `div` 1000)

-- | Force the spine of the parsed result for accurate timing.
forceParsed :: Either ParserError ParsedSQL -> Either ParserError ParsedSQL
forceParsed (Left err) = err `seq` Left err
forceParsed (Right parsed) =
    foldl' (\() unit -> unit `seq` forceUnitFacts (unitFacts unit)) () (parsedUnits parsed) `seq`
        length (parsedColumns parsed) `seq`
            Right parsed
    where
        forceUnitFacts Nothing = ()
        forceUnitFacts (Just facts) =
            length (factEdits facts) `seq`
                length (factOps (factSemantics facts)) `seq`
                    length (factBarriers (factSemantics facts)) `seq`
                        ()
