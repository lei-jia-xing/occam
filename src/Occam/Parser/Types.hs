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

-- | Supported SQL dialects.
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

-- | Initial server and session configuration.
data ServerConfig = ServerConfig
    { serverNamespacePrefix :: ![Text]
    -- ^ Namespace parts; double-quoted to preserve case.
    , serverSearchPath :: ![Text]
    -- ^ Schema search path entries; double-quoted to preserve case.
    , serverCurrentUser :: !(Maybe Text)
    -- ^ Current session user; double-quoted to preserve case.
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
    , parseSyntaxOnly :: !Bool
    , parseSpanOnly :: !Bool
    }
    deriving (Eq, Show)

-- | Default parse options enabling only core statement and semantic facts.
defaultParseOptions :: SQLDialect -> ParseOptions
defaultParseOptions dialect =
    ParseOptions
        { parseDialect = dialect
        , parseServerConfig = defaultServerConfig
        , parseChunkEdits = False
        , parseColumnIR = False
        , parseColumnBundles = False
        , parseSyntaxEdits = False
        , parseSyntaxOnly = False
        , parseSpanOnly = False
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
            , "syntax_only" .= parseSyntaxOnly options
            , "span_only" .= parseSpanOnly options
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
