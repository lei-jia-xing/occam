module Occam.Parser.Types (
    SQLDialect (..),
    defaultSQLDialect,
    dialectName,
    ServerConfig (..),
    defaultServerConfig,
    ParseOptions (..),
    defaultParseOptions,
    ParserErrorKind (..),
    ParserError (..),
    ParsedSQL (..),
) where

import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Word (Word8)
import Occam.Core (SessionInit)
import Occam.Facts (ColumnTarget, StatementUnit)

-- Keep dialect selection at the frontend boundary: parsing and every derived
-- fact use the same selected sqlparser dialect.
data SQLDialect
    = GenericSQL
    | PostgreSQL
    | MySQL
    | SQLite
    | DuckDB
    | ClickHouse
    | Oracle
    | Ansi
    | BigQuery
    | Databricks
    | Hive
    | MsSql
    | Redshift
    | Snowflake
    | Spark
    | Teradata
    deriving (Eq, Ord, Show, Enum, Bounded)

defaultSQLDialect :: SQLDialect
defaultSQLDialect = GenericSQL

{- | Server/session configuration that is independent of a particular SQL text.
The graph builder starts from this state and applies session effects as it
scans statements.
-}
data ServerConfig = ServerConfig
    { serverNamespacePrefix :: ![Text]
    {- ^ Each element is one namespace part. A part may be double-quoted to
    preserve case.
    -}
    , serverSearchPath :: ![Text]
    {- ^ Each element is one schema entry. An element may be double-quoted to
    preserve case.
    -}
    , serverCurrentUser :: !(Maybe Text)
    {- ^ An unquoted name follows the dialect's relation folding; a
    double-quoted name preserves case.
    -}
    , serverAutocommit :: !Bool
    , serverForeignKeyChecks :: !(Maybe Bool)
    , serverVariables :: !(Map Text Text)
    , serverLowerCaseTableNames :: !Word8
    , serverTableNamesCaseSensitive :: !(Maybe Bool)
    , serverColumnNamesCaseSensitive :: !(Maybe Bool)
    , serverQuotedIdentifiersCaseSensitive :: !(Maybe Bool)
    }
    deriving (Eq, Show)

defaultServerConfig :: ServerConfig
defaultServerConfig =
    ServerConfig
        { serverNamespacePrefix = []
        , serverSearchPath = []
        , serverCurrentUser = Nothing
        , serverAutocommit = True
        , serverForeignKeyChecks = Nothing
        , serverVariables = Map.empty
        , serverLowerCaseTableNames = 0
        , serverTableNamesCaseSensitive = Nothing
        , serverColumnNamesCaseSensitive = Nothing
        , serverQuotedIdentifiersCaseSensitive = Nothing
        }
data ParseOptions = ParseOptions
    { parseDialect :: !SQLDialect
    , parseServerConfig :: !ServerConfig
    , parseChunkEdits :: !Bool
    , parseColumnIR :: !Bool
    , parseColumnBundles :: !Bool
    , parseSyntaxEdits :: !Bool
    }
    deriving (Eq, Show)

{- | Facts-only defaults: statements, boundaries, and dependency/semantic
facts, but no per-edit syntax IR. Passes that consume 'factEdits' must
request 'parseSyntaxEdits' explicitly (see 'Occam.Reducer' syntax options).
-}
defaultParseOptions :: SQLDialect -> ParseOptions
defaultParseOptions dialect =
    ParseOptions
        { parseDialect = dialect
        , parseServerConfig = defaultServerConfig
        , parseChunkEdits = False
        , parseColumnIR = False
        , parseColumnBundles = False
        , parseSyntaxEdits = False
        }

instance ToJSON ServerConfig where
    toJSON :: ServerConfig -> Value
    toJSON config =
        object
            [ "namespace_prefix" .= serverNamespacePrefix config
            , "search_path" .= serverSearchPath config
            , "current_user" .= serverCurrentUser config
            , "autocommit" .= serverAutocommit config
            , "foreign_key_checks" .= serverForeignKeyChecks config
            , "variables" .= serverVariables config
            , "lower_case_table_names" .= serverLowerCaseTableNames config
            , "table_names_case_sensitive" .= serverTableNamesCaseSensitive config
            , "column_names_case_sensitive" .= serverColumnNamesCaseSensitive config
            , "quoted_identifiers_case_sensitive" .= serverQuotedIdentifiersCaseSensitive config
            ]

instance ToJSON ParseOptions where
    toJSON :: ParseOptions -> Value
    toJSON options =
        object
            [ "dialect" .= dialectName (parseDialect options)
            , "server" .= parseServerConfig options
            , "chunk_edits" .= parseChunkEdits options
            , "column_ir" .= parseColumnIR options
            , "column_bundles" .= parseColumnBundles options
            , "syntax_edits" .= parseSyntaxEdits options
            ]

data ParserErrorKind
    = ParserInvalidArguments
    | ParserInvalidConfig
    | ParserInvalidUtf8
    | ParserNativeFailure
    | ParserNativePanic
    | ParserNullResult
    | ParserDecodeFailure
    | ParserInvalidSourceRange
    | ParserInvalidEditRange
    | ParserUnknownFailure !Int
    deriving (Eq, Show)

data ParserError = ParserError
    { parserErrorKind :: !ParserErrorKind
    , parserErrorMessage :: !Text
    }
    deriving (Eq, Show)

data ParsedSQL = ParsedSQL
    { parsedInitialSession :: !SessionInit
    , parsedUnits :: ![StatementUnit]
    , parsedColumns :: ![ColumnTarget]
    }
    deriving (Eq, Show)

dialectName :: SQLDialect -> Text
dialectName GenericSQL = "generic"
dialectName PostgreSQL = "postgresql"
dialectName MySQL = "mysql"
dialectName SQLite = "sqlite"
dialectName DuckDB = "duckdb"
dialectName ClickHouse = "clickhouse"
dialectName Oracle = "oracle"
dialectName Ansi = "ansi"
dialectName BigQuery = "bigquery"
dialectName Databricks = "databricks"
dialectName Hive = "hive"
dialectName MsSql = "mssql"
dialectName Redshift = "redshift"
dialectName Snowflake = "snowflake"
dialectName Spark = "spark"
dialectName Teradata = "teradata"
