-- | Configuration, diagnostic, and output types for SQL parsing and analysis.
module Occam.Parser.Types (
    SQLDialect (..),
    dialectName,
    ServerConfig (..),
    defaultServerConfig,
    ParseOptions (..),
    defaultParseOptions,
    ParserErrorKind (..),
    ParserError (..),
    ParsedSQL (..),
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Word (Word8)
import Occam.Core.Semantics (SessionInit)
import Occam.Facts (ColumnTarget, StatementUnit)

-- | Supported SQL dialects matching the 6 target DBMS harnesses.
data SQLDialect
    = PostgreSQL
    | MySQL
    | MariaDB
    | SQLite
    | DuckDB
    | ClickHouse
    deriving (Eq, Ord, Show, Enum, Bounded)

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

-- | Default session assumptions (e.g. autocommit, FK checks) when dialect overrides are absent.
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

-- | Options controlling SQL parsing and optional analysis/edit extraction passes.
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

-- | Machine-readable failure category reported by the frontend parser.
data ParserErrorKind
    = ParserInvalidArguments
    | ParserInvalidConfig
    | ParserInvalidUtf8
    | ParserInvalidSourceRange
    | ParserInvalidEditRange
    deriving (Eq, Show)

-- | Structured frontend parse error with error kind and diagnostic text.
data ParserError = ParserError
    { parserErrorKind :: !ParserErrorKind
    , parserErrorMessage :: !Text
    }
    deriving (Eq, Show)

-- | Complete parsed seed result containing session init, statement units, and column bundles.
data ParsedSQL = ParsedSQL
    { parsedInitialSession :: !SessionInit
    , parsedUnits :: ![StatementUnit]
    , parsedColumns :: ![ColumnTarget]
    }
    deriving (Eq, Show)

-- | Render a 'SQLDialect' as its lowercase canonical identifier string.
dialectName :: SQLDialect -> Text
dialectName PostgreSQL = "postgresql"
dialectName MySQL = "mysql"
dialectName MariaDB = "mariadb"
dialectName SQLite = "sqlite"
dialectName DuckDB = "duckdb"
dialectName ClickHouse = "clickhouse"
