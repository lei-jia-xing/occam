{-# OPTIONS_GHC -Wno-orphans #-}

module Occam.Parser (
    SQLDialect (..),
    defaultSQLDialect,
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
    parseSQL,
    countStatements,
) where

import Control.Exception (finally)
import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value (..),
    eitherDecodeStrict',
    encode,
    object,
    withArray,
    withBool,
    withObject,
    withText,
    (.:),
    (.:?),
    (.=),
 )
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import Foreign (Ptr, alloca, castPtr, nullPtr, peek, poke)
import Foreign.C.Types (CInt (..), CSize (..), CUInt (..))
import Occam.Core (
    AlterChange (..),
    AlterTable (..),
    BarrierReason (..),
    BarrierScope (..),
    BeginPolicy (..),
    CatalogOp (..),
    ColumnDef (..),
    ConflictAction (..),
    Constraint (..),
    ConstraintRef (..),
    ConstraintValidity (..),
    DataWrite (..),
    DeleteScope (..),
    DmlEvent (..),
    DropMode (..),
    DropObject (..),
    DropOp (..),
    ForeignKey (..),
    Ident (..),
    IndexDef (..),
    IndexRef (..),
    IndexRole (..),
    IndexSchemaPlacement (..),
    IndexUniqueness (..),
    Name (..),
    NameRef (..),
    ObjectRef (..),
    OnCommitAction (..),
    ReadFact (..),
    ReadMode (..),
    ReferentialAction (..),
    RefreshOp (..),
    RelationKind (..),
    RenameOp (..),
    SchemaDef (..),
    ScopedBarrier (..),
    SearchPathEntry (..),
    SearchPathValue (..),
    SemanticFacts (..),
    SequenceDef (..),
    SequenceWrite (..),
    SessionEffect (..),
    SessionInit (..),
    SessionScope (..),
    SessionValue (..),
    Span (..),
    Statement (..),
    StatementId (..),
    TableDef (..),
    TriggerBody (..),
    TriggerDef (..),
    TriggerEffect (..),
    TriggerEvent (..),
    TriggerRef (..),
    TriggerTiming (..),
    TruncateBehavior (..),
    TruncateIdentity (..),
    TxEffect (..),
    TypeDef (..),
    ViewDef (..),
    WriteKind (..),
 )

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
    }
    deriving (Eq, Show)

defaultParseOptions :: SQLDialect -> ParseOptions
defaultParseOptions dialect =
    ParseOptions
        { parseDialect = dialect
        , parseServerConfig = defaultServerConfig
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
    }
    deriving (Eq, Show)

data StatementUnit = StatementUnit
    { unitStatement :: !Statement
    , unitFacts :: !(Maybe StatementFacts)
    , unitParseError :: !(Maybe Text)
    }
    deriving (Eq, Show)

data StatementFacts = StatementFacts
    { factSemantics :: !SemanticFacts
    , factEdits :: ![ParserEdit]
    }
    deriving (Eq, Show)

data ParserEditCategory
    = EditExpression
    | EditClause
    | EditConstraint
    | EditList
    | EditType
    | EditLiteral
    deriving (Eq, Ord, Show)

instance ToJSON ParserEditCategory where
    toJSON category = toJSON (editCategoryName category)

instance FromJSON ParserEditCategory where
    parseJSON = withText "ParserEditCategory" \name ->
        maybe (fail ("unknown parser edit category: " <> T.unpack name)) pure (parseEditCategoryName name)

editCategoryName :: ParserEditCategory -> Text
editCategoryName EditExpression = "expression"
editCategoryName EditClause = "clause"
editCategoryName EditConstraint = "constraint"
editCategoryName EditList = "list"
editCategoryName EditType = "type"
editCategoryName EditLiteral = "literal"

parseEditCategoryName :: Text -> Maybe ParserEditCategory
parseEditCategoryName "expression" = Just EditExpression
parseEditCategoryName "clause" = Just EditClause
parseEditCategoryName "constraint" = Just EditConstraint
parseEditCategoryName "list" = Just EditList
parseEditCategoryName "type" = Just EditType
parseEditCategoryName "literal" = Just EditLiteral
parseEditCategoryName _ = Nothing

data ParserEdit = ParserEdit
    { parserEditStart :: !Int
    , parserEditEnd :: !Int
    , parserEditReplacement :: !ByteString
    , parserEditCategory :: !ParserEditCategory
    , parserEditLabel :: !Text
    }
    deriving (Eq, Show)

data NativeResponse = NativeResponse
    { responseInitialSession :: !SessionInit
    , responseUnits :: ![NativeUnit]
    }

data NativeUnit = NativeUnit
    { nativeStart :: !Int
    , nativeEnd :: !Int
    , nativeParseError :: !(Maybe Text)
    , nativeFacts :: !(Maybe NativeFacts)
    }

data NativeFacts = NativeFacts
    { nativeSemantics :: !SemanticFacts
    , nativeEdits :: ![ParserEdit]
    }

instance FromJSON NativeResponse where
    parseJSON = withObject "NativeResponse" \objectValue ->
        NativeResponse
            <$> objectValue .: "initial_session"
            <*> objectValue .: "units"

instance FromJSON NativeUnit where
    parseJSON = withObject "NativeUnit" \objectValue ->
        NativeUnit
            <$> objectValue .: "start"
            <*> objectValue .: "end"
            <*> objectValue .:? "parse_error"
            <*> objectValue .:? "facts"

instance FromJSON NativeFacts where
    parseJSON = withObject "NativeFacts" \objectValue ->
        NativeFacts
            <$> parseJSON (Object objectValue)
            <*> objectValue .: "edits"

instance FromJSON SemanticFacts where
    parseJSON = withObject "SemanticFacts" \objectValue ->
        SemanticFacts
            <$> objectValue .: "ops"
            <*> objectValue .: "session"
            <*> objectValue .: "pre_tx"
            <*> objectValue .: "post_tx"
            <*> objectValue .: "barriers"

instance FromJSON SearchPathEntry where
    parseJSON = withObject "SearchPathEntry" \objectValue -> do
        tag <- objectValue .: "entry"
        case tag of
            "name" -> SearchPathName <$> objectValue .: "name"
            "current_user" -> pure SearchPathCurrentUser
            _ -> fail ("unknown search path entry: " <> T.unpack tag)

instance FromJSON SearchPathValue where
    parseJSON = withObject "SearchPathValue" \objectValue -> do
        tag <- objectValue .: "path"
        case tag of
            "explicit" -> SearchPathExplicit <$> objectValue .: "entries"
            "default" -> pure SearchPathDefault
            _ -> fail ("unknown search path value: " <> T.unpack tag)

instance FromJSON SessionInit where
    parseJSON = withObject "SessionInit" \objectValue ->
        SessionInit
            <$> objectValue .: "namespace_prefix"
            <*> objectValue .: "search_path"
            <*> objectValue .: "current_user"
            <*> objectValue .: "autocommit"
            <*> objectValue .: "foreign_key_checks"
            <*> objectValue .: "variables"

instance FromJSON Ident where
    parseJSON = withText "Ident" (pure . Ident)

instance ToJSON Ident where
    toJSON (Ident value) = String value

instance FromJSON Name where
    parseJSON = withArray "Name" (fmap Name . traverse parseJSON . toList)

instance ToJSON Name where
    toJSON (Name parts) = toJSON parts

instance FromJSON NameRef where
    parseJSON = withObject "NameRef" \objectValue ->
        NameRef
            <$> objectValue .: "name"
            <*> objectValue .: "temp"

instance FromJSON ReadMode where
    parseJSON = withText "ReadMode" \name ->
        case name of
            "data" -> pure ReadData
            "schema" -> pure ReadSchema
            _ -> fail ("unknown read mode: " <> T.unpack name)

instance FromJSON ReadFact where
    parseJSON = withObject "ReadFact" \objectValue ->
        ReadFact
            <$> objectValue .: "source"
            <*> objectValue .: "mode"
            <*> objectValue .: "columns"

instance FromJSON RelationKind where
    parseJSON = withText "RelationKind" \name ->
        case name of
            "table" -> pure TableRelation
            "view" -> pure ViewRelation
            "materialized_view" -> pure MaterializedViewRelation
            _ -> fail ("unknown relation kind: " <> T.unpack name)

instance FromJSON ReferentialAction where
    parseJSON = withText "ReferentialAction" \name ->
        case name of
            "no_action" -> pure NoAction
            "restrict" -> pure Restrict
            "cascade" -> pure Cascade
            "set_null" -> pure SetNull
            "set_default" -> pure SetDefault
            _ -> fail ("unknown referential action: " <> T.unpack name)

instance FromJSON OnCommitAction where
    parseJSON = withText "OnCommitAction" \name ->
        case name of
            "delete_rows" -> pure OnCommitDeleteRows
            "drop" -> pure OnCommitDrop
            _ -> fail ("unknown ON COMMIT action: " <> T.unpack name)

instance FromJSON ConstraintValidity where
    parseJSON = withBool "ConstraintValidity" \valid ->
        pure (if valid then ConstraintValid else ConstraintNotValid)

instance FromJSON IndexUniqueness where
    parseJSON = withBool "IndexUniqueness" \unique ->
        pure (if unique then IndexUnique else IndexNonUnique)

instance FromJSON IndexRole where
    parseJSON = withBool "IndexRole" \primary ->
        pure (if primary then PrimaryIndex else SecondaryIndex)

instance FromJSON DeleteScope where
    parseJSON = withBool "DeleteScope" \allRows ->
        pure (if allRows then DeleteAllRows else DeleteFilteredRows)

instance FromJSON TruncateBehavior where
    parseJSON = withBool "TruncateBehavior" \cascade ->
        pure (if cascade then TruncateCascade else TruncateRestrict)

instance FromJSON TruncateIdentity where
    parseJSON = withBool "TruncateIdentity" \restart ->
        pure (if restart then RestartIdentity else ContinueIdentity)

instance FromJSON ColumnDef where
    parseJSON = withObject "ColumnDef" \objectValue ->
        ColumnDef
            <$> objectValue .: "name"
            <*> objectValue .: "not_null"
            <*> objectValue .: "has_default"
            <*> objectValue .: "generated"
            <*> objectValue .: "references"

instance FromJSON ForeignKey where
    parseJSON = withObject "ForeignKey" \objectValue ->
        ForeignKey
            <$> objectValue .: "name"
            <*> objectValue .: "child_columns"
            <*> objectValue .: "parent"
            <*> objectValue .: "parent_columns"
            <*> objectValue .: "on_delete"
            <*> objectValue .: "on_update"
            <*> objectValue .: "not_valid"
            <*> objectValue .: "deferrable"

instance FromJSON Constraint where
    parseJSON = withObject "Constraint" \objectValue -> do
        tag <- objectValue .: "constraint"
        case tag of
            "primary_key" ->
                PrimaryKeyConstraint
                    <$> objectValue .: "name"
                    <*> objectValue .: "columns"
            "unique" ->
                UniqueConstraint
                    <$> objectValue .: "name"
                    <*> objectValue .: "columns"
            "foreign_key" -> ForeignKeyConstraint <$> parseJSON (Object objectValue)
            "check" ->
                CheckConstraint
                    <$> objectValue .: "name"
                    <*> objectValue .: "columns"
                    <*> objectValue .: "not_valid"
            "index" ->
                IndexConstraint
                    <$> objectValue .: "name"
                    <*> objectValue .: "columns"
                    <*> objectValue .: "unique"
            "using_index" ->
                UsingIndexConstraint
                    <$> objectValue .: "name"
                    <*> objectValue .: "index"
                    <*> objectValue .: "primary"
            "other" -> OtherConstraint <$> objectValue .: "name"
            _ -> fail ("unknown constraint kind: " <> T.unpack tag)

instance FromJSON TableDef where
    parseJSON = withObject "TableDef" \objectValue ->
        TableDef
            <$> objectValue .: "name"
            <*> objectValue .: "temporary"
            <*> objectValue .: "transactional"
            <*> objectValue .: "columns"
            <*> objectValue .: "constraints"
            <*> objectValue .: "sources"
            <*> objectValue .: "like"
            <*> objectValue .: "on_commit"
            <*> objectValue .: "if_not_exists"
            <*> objectValue .: "or_replace"

instance FromJSON ViewDef where
    parseJSON = withObject "ViewDef" \objectValue ->
        ViewDef
            <$> objectValue .: "name"
            <*> objectValue .: "temporary"
            <*> objectValue .: "materialized"
            <*> objectValue .: "columns"
            <*> objectValue .: "sources"
            <*> objectValue .: "if_not_exists"
            <*> objectValue .: "or_replace"

instance FromJSON IndexRef where
    parseJSON = withObject "IndexRef" \objectValue -> do
        tag <- objectValue .: "scope"
        case tag of
            "named" -> NamedIndex <$> objectValue .: "name"
            "table" -> TableIndex <$> objectValue .: "table" <*> objectValue .: "name"
            _ -> fail ("unknown index scope: " <> T.unpack tag)

instance FromJSON IndexDef where
    parseJSON = withObject "IndexDef" \objectValue ->
        IndexDef
            <$> objectValue .: "index"
            <*> objectValue .: "table"
            <*> objectValue .: "columns"
            <*> objectValue .: "unique"
            <*> objectValue .: "predicate"
            <*> objectValue .: "expression"
            <*> objectValue .: "if_not_exists"
            <*> objectValue .: "schema_placement"

instance FromJSON IndexSchemaPlacement where
    parseJSON = withText "IndexSchemaPlacement" \name ->
        case name of
            "table_schema" -> pure IndexTableSchema
            "current_schema" -> pure IndexCurrentSchema
            _ -> fail ("unknown index schema placement: " <> T.unpack name)

instance FromJSON TriggerRef where
    parseJSON = withObject "TriggerRef" \objectValue -> do
        tag <- objectValue .: "scope"
        case tag of
            "named" -> NamedTrigger <$> objectValue .: "name"
            "table" -> TableTrigger <$> objectValue .: "table" <*> objectValue .: "name"
            _ -> fail ("unknown trigger scope: " <> T.unpack tag)

instance FromJSON ConstraintRef where
    parseJSON = withObject "ConstraintRef" \objectValue ->
        ConstraintRef
            <$> objectValue .: "table"
            <*> objectValue .: "name"

instance FromJSON TriggerTiming where
    parseJSON = withText "TriggerTiming" \name ->
        case name of
            "before" -> pure TriggerBefore
            "after" -> pure TriggerAfter
            "instead_of" -> pure TriggerInsteadOf
            _ -> fail ("unknown trigger timing: " <> T.unpack name)

instance FromJSON TriggerEvent where
    parseJSON = withObject "TriggerEvent" \objectValue -> do
        tag <- objectValue .: "event"
        case tag of
            "insert" -> pure TriggerInsert
            "update" -> TriggerUpdate <$> objectValue .: "columns"
            "delete" -> pure TriggerDelete
            "truncate" -> pure TriggerTruncate
            _ -> fail ("unknown trigger event: " <> T.unpack tag)

instance FromJSON TriggerBody where
    parseJSON = withObject "TriggerBody" \objectValue -> do
        tag <- objectValue .: "body"
        case tag of
            "inline" -> TriggerInline <$> objectValue .: "effects"
            "function" -> TriggerFunction <$> objectValue .: "name" <*> objectValue .: "args"
            "opaque" -> pure TriggerOpaque
            _ -> fail ("unknown trigger body: " <> T.unpack tag)

instance FromJSON TriggerEffect where
    parseJSON = withObject "TriggerEffect" \objectValue -> do
        tag <- objectValue .: "kind"
        case tag of
            "dml" -> TriggerDml <$> objectValue .: "event"
            "read" -> TriggerRead <$> objectValue .: "read"
            "write_sequence" -> TriggerWriteSequence <$> objectValue .: "write"
            "session" -> TriggerSession <$> objectValue .: "effect"
            "barrier" -> TriggerBarrier <$> objectValue .: "barrier"
            _ -> fail ("unknown trigger effect: " <> T.unpack tag)

instance FromJSON DmlEvent where
    parseJSON = withObject "DmlEvent" \objectValue -> do
        tag <- objectValue .: "event"
        case tag of
            "insert" -> DmlInsert <$> objectValue .: "target" <*> objectValue .: "columns"
            "update" -> DmlUpdate <$> objectValue .: "target" <*> objectValue .: "columns"
            "merge" -> DmlMerge <$> objectValue .: "target" <*> objectValue .: "columns"
            "delete" -> DmlDelete <$> objectValue .: "target" <*> objectValue .: "all"
            "truncate" -> DmlTruncate <$> objectValue .: "target" <*> objectValue .: "cascade"
            _ -> fail ("unknown DML event: " <> T.unpack tag)

instance FromJSON TriggerDef where
    parseJSON = withObject "TriggerDef" \objectValue ->
        TriggerDef
            <$> objectValue .: "trigger"
            <*> objectValue .: "table"
            <*> objectValue .: "timing"
            <*> objectValue .: "events"
            <*> objectValue .: "for_each_row"
            <*> objectValue .: "when_reads"
            <*> objectValue .: "body"
            <*> objectValue .: "referenced_table"
            <*> objectValue .: "temporary"
            <*> objectValue .: "if_not_exists"
            <*> objectValue .: "or_replace"

instance FromJSON SequenceDef where
    parseJSON = withObject "SequenceDef" \objectValue ->
        SequenceDef
            <$> objectValue .: "name"
            <*> objectValue .: "temporary"
            <*> objectValue .: "transactional"

instance FromJSON TypeDef where
    parseJSON = withObject "TypeDef" \objectValue -> TypeDef <$> objectValue .: "name"

instance FromJSON SchemaDef where
    parseJSON = withObject "SchemaDef" \objectValue -> SchemaDef <$> objectValue .: "name"

instance FromJSON ConflictAction where
    parseJSON = withText "ConflictAction" \name ->
        case name of
            "error" -> pure ConflictError
            "ignore" -> pure ConflictIgnore
            "replace" -> pure ConflictReplace
            "update" -> pure ConflictUpdate
            _ -> fail ("unknown conflict action: " <> T.unpack name)

instance FromJSON WriteKind where
    parseJSON = withObject "WriteKind" \objectValue -> do
        tag <- objectValue .: "kind"
        case tag of
            "insert" -> InsertWrite <$> objectValue .: "conflict"
            "update" -> pure UpdateWrite
            "delete" -> DeleteWrite <$> objectValue .: "all"
            "merge" -> pure MergeWrite
            "truncate" ->
                TruncateWrite
                    <$> objectValue .: "cascade"
                    <*> objectValue .: "restart_identity"
            _ -> fail ("unknown write kind: " <> T.unpack tag)

instance FromJSON DataWrite where
    parseJSON = withObject "DataWrite" \objectValue ->
        DataWrite
            <$> objectValue .: "target"
            <*> objectValue .: "kind"
            <*> objectValue .: "columns"
            <*> objectValue .: "unqualified_columns"
            <*> objectValue .: "reads"

instance FromJSON SequenceWrite where
    parseJSON = withObject "SequenceWrite" \objectValue ->
        SequenceWrite
            <$> objectValue .: "sequence"
            <*> objectValue .: "transactional"

instance FromJSON AlterChange where
    parseJSON = withObject "AlterChange" \objectValue -> do
        tag <- objectValue .: "change"
        case tag of
            "add_column" ->
                AddColumn
                    <$> objectValue .: "column"
                    <*> objectValue .: "constraints"
            "drop_column" -> DropColumn <$> objectValue .: "column" <*> objectValue .: "cascade"
            "add_constraint" -> AddConstraint <$> objectValue .: "constraint"
            "drop_constraint" -> DropConstraint <$> objectValue .: "name" <*> objectValue .: "cascade"
            "rename_constraint" -> RenameConstraint <$> objectValue .: "from" <*> objectValue .: "to"
            "validate_constraint" -> ValidateConstraint <$> objectValue .: "name"
            "drop_index" -> DropIndexChange <$> objectValue .: "name"
            "alter_column" -> AlterColumn <$> objectValue .: "column" <*> objectValue .: "detail"
            "other" -> OtherChange <$> objectValue .: "detail"
            _ -> fail ("unknown ALTER TABLE change: " <> T.unpack tag)

instance FromJSON AlterTable where
    parseJSON = withObject "AlterTable" \objectValue ->
        AlterTable
            <$> objectValue .: "table"
            <*> objectValue .: "changes"

instance FromJSON DropMode where
    parseJSON = withText "DropMode" \name ->
        case name of
            "restrict" -> pure DropRestrict
            "cascade" -> pure DropCascade
            _ -> fail ("unknown drop mode: " <> T.unpack name)

instance FromJSON DropObject where
    parseJSON = withObject "DropObject" \objectValue -> do
        tag <- objectValue .: "object"
        case tag of
            "relation" -> DropRelation <$> objectValue .: "name" <*> objectValue .: "kind"
            "index" -> DropIndex <$> objectValue .: "index"
            "trigger" -> DropTrigger <$> objectValue .: "trigger"
            "sequence" -> DropSequence <$> objectValue .: "name"
            "type" -> DropType <$> objectValue .: "name"
            "schema" -> DropSchema <$> objectValue .: "name"
            _ -> fail ("unknown drop object: " <> T.unpack tag)

instance FromJSON DropOp where
    parseJSON = withObject "DropOp" \objectValue ->
        DropOp
            <$> objectValue .: "objects"
            <*> objectValue .: "mode"

instance FromJSON RenameOp where
    parseJSON = withObject "RenameOp" \objectValue -> do
        tag <- objectValue .: "rename"
        case tag of
            "relation" -> RenameRelation <$> objectValue .: "from" <*> objectValue .: "to"
            "column" ->
                RenameColumnOp
                    <$> objectValue .: "table"
                    <*> objectValue .: "from"
                    <*> objectValue .: "to"
            _ -> fail ("unknown rename kind: " <> T.unpack tag)

instance FromJSON RefreshOp where
    parseJSON = withObject "RefreshOp" \objectValue -> RefreshOp <$> objectValue .: "target"

instance FromJSON CatalogOp where
    parseJSON = withObject "CatalogOp" \objectValue -> do
        tag <- objectValue .: "op"
        case tag of
            "define_table" -> DefineTableOp <$> parseJSON (Object objectValue)
            "define_view" -> DefineViewOp <$> parseJSON (Object objectValue)
            "define_index" -> DefineIndexOp <$> parseJSON (Object objectValue)
            "define_trigger" -> DefineTriggerOp <$> parseJSON (Object objectValue)
            "define_sequence" -> DefineSequenceOp <$> parseJSON (Object objectValue)
            "define_type" -> DefineTypeOp <$> parseJSON (Object objectValue)
            "define_schema" -> DefineSchemaOp <$> parseJSON (Object objectValue)
            "read" -> ReadOp <$> parseJSON (Object objectValue)
            "write_data" -> WriteDataOp <$> parseJSON (Object objectValue)
            "write_sequence" -> WriteSequenceOp <$> parseJSON (Object objectValue)
            "alter_table" -> AlterTableOp <$> parseJSON (Object objectValue)
            "drop" -> DropCatalogOp <$> parseJSON (Object objectValue)
            "rename" -> RenameOp <$> parseJSON (Object objectValue)
            "refresh" -> RefreshCatalogOp <$> parseJSON (Object objectValue)
            _ -> fail ("unknown catalog operation: " <> T.unpack tag)

instance FromJSON SessionScope where
    parseJSON = withText "SessionScope" \name ->
        case name of
            "session" -> pure ScopeSession
            "transaction" -> pure ScopeTransaction
            "local" -> pure ScopeLocal
            "global" -> pure ScopeGlobal
            _ -> fail ("unknown session scope: " <> T.unpack name)

instance FromJSON SessionValue where
    parseJSON = withObject "SessionValue" \objectValue -> do
        tag <- objectValue .: "kind"
        case tag of
            "text" -> SessionText <$> objectValue .: "value"
            "bool" -> SessionBool <$> objectValue .: "value"
            "integer" -> SessionInteger <$> objectValue .: "value"
            "names" -> SessionNames <$> objectValue .: "value"
            "default" -> pure SessionDefault
            _ -> fail ("unknown session value: " <> T.unpack tag)

instance FromJSON SessionEffect where
    parseJSON = withObject "SessionEffect" \objectValue -> do
        tag <- objectValue .: "effect"
        case tag of
            "set_variable" ->
                SetVariable
                    <$> objectValue .: "scope"
                    <*> objectValue .: "name"
                    <*> objectValue .: "value"
            "set_search_path" ->
                SetSearchPath
                    <$> objectValue .: "scope"
                    <*> objectValue .: "path"
            "set_namespace" ->
                SetNamespace
                    <$> objectValue .: "scope"
                    <*> objectValue .: "namespace"
            "set_session_user" ->
                SetSessionUser
                    <$> objectValue .: "scope"
                    <*> objectValue .: "user"
            "reset_session_user" -> ResetSessionUser <$> objectValue .: "scope"
            "set_current_user" ->
                SetCurrentUser
                    <$> objectValue .: "scope"
                    <*> objectValue .: "user"
            "reset_current_user" -> ResetCurrentUser <$> objectValue .: "scope"
            "set_autocommit" ->
                SetAutocommit
                    <$> objectValue .: "scope"
                    <*> objectValue .: "value"
            "set_foreign_key_checks" ->
                SetForeignKeyChecks
                    <$> objectValue .: "scope"
                    <*> objectValue .: "value"
            "reset_foreign_key_checks" ->
                ResetForeignKeyChecks <$> objectValue .: "scope"
            "reset_variable" ->
                ResetVariable
                    <$> objectValue .: "scope"
                    <*> objectValue .: "name"
            "reset_all" -> ResetAll <$> objectValue .: "scope"
            _ -> fail ("unknown session effect: " <> T.unpack tag)

instance FromJSON BeginPolicy where
    parseJSON = withText "BeginPolicy" \name ->
        case name of
            "error" -> pure BeginError
            "warn" -> pure BeginWarn
            "commit_active" -> pure BeginCommitActive
            "ignore" -> pure BeginIgnore
            _ -> fail ("unknown BEGIN policy: " <> T.unpack name)

instance FromJSON TxEffect where
    parseJSON = withObject "TxEffect" \objectValue -> do
        tag <- objectValue .: "effect"
        case tag of
            "begin" -> TxBegin <$> objectValue .: "if_active"
            "commit" -> TxCommit <$> objectValue .: "chain"
            "rollback_all" -> pure TxRollbackAll
            "rollback_to" -> TxRollbackTo <$> objectValue .: "savepoint"
            "savepoint" -> TxSavepoint <$> objectValue .: "name"
            "release_savepoint" -> TxReleaseSavepoint <$> objectValue .: "name"
            "set_read_only" -> TxSetReadOnly <$> objectValue .: "value"
            _ -> fail ("unknown transaction effect: " <> T.unpack tag)

instance FromJSON BarrierReason where
    parseJSON = withText "BarrierReason" \name ->
        case name of
            "syntax_error" -> pure SyntaxErrorBarrier
            "unknown_statement" -> pure UnknownStatementBarrier
            "unknown_relation" -> pure UnknownRelationBarrier
            "unknown_ddl" -> pure UnknownDDLBarrier
            "unknown_dml" -> pure UnknownDMLBarrier
            "unknown_drop" -> pure UnknownDropBarrier
            "unknown_alter" -> pure UnknownAlterBarrier
            "unknown_view" -> pure UnknownViewBarrier
            "unknown_index" -> pure UnknownIndexBarrier
            "unknown_trigger" -> pure UnknownTriggerBarrier
            "unknown_truncate" -> pure UnknownTruncateBarrier
            "unknown_session_effect" -> pure UnknownSessionEffectBarrier
            "unsupported_expression" -> pure UnsupportedExpressionBarrier
            "unsupported_conflict_clause" -> pure UnsupportedConflictClauseBarrier
            "unsupported_trigger_body" -> pure UnsupportedTriggerBodyBarrier
            "unsupported_transaction" -> pure UnsupportedTransactionBarrier
            "unresolved_view_source" -> pure UnresolvedViewSourceBarrier
            "write_through_view" -> pure WriteThroughViewBarrier
            "opaque_trigger_body" -> pure OpaqueTriggerBodyBarrier
            "unmodeled_relation_source" -> pure UnmodeledRelationSourceBarrier
            _ -> fail ("unknown barrier reason: " <> T.unpack name)

instance FromJSON ObjectRef where
    parseJSON = withObject "ObjectRef" \objectValue -> do
        tag <- objectValue .: "object"
        case tag of
            "relation" -> ObjectRelation <$> objectValue .: "relation"
            "index" -> ObjectIndex <$> objectValue .: "index"
            "trigger" -> ObjectTrigger <$> objectValue .: "trigger"
            "sequence" -> ObjectSequence <$> objectValue .: "sequence"
            "type" -> ObjectType <$> objectValue .: "name"
            "schema" -> ObjectSchema <$> objectValue .: "name"
            "constraint" -> ObjectConstraint <$> objectValue .: "constraint"
            _ -> fail ("unknown object reference: " <> T.unpack tag)

instance FromJSON BarrierScope where
    parseJSON = withObject "BarrierScope" \objectValue -> do
        tag <- objectValue .: "scope"
        case tag of
            "global" -> pure GlobalScope
            "relation" -> RelationScope <$> objectValue .: "relation"
            "schema" -> SchemaScope <$> objectValue .: "schema"
            "object" -> ObjectScope <$> objectValue .: "object"
            "session_state" -> pure SessionStateScope
            "transaction_state" -> pure TransactionStateScope
            _ -> fail ("unknown barrier scope: " <> T.unpack tag)

instance FromJSON ScopedBarrier where
    parseJSON = withObject "ScopedBarrier" \objectValue ->
        ScopedBarrier
            <$> objectValue .: "scope"
            <*> objectValue .: "reason"
            <*> objectValue .: "detail"

instance FromJSON ParserEdit where
    parseJSON = withObject "ParserEdit" \objectValue ->
        ParserEdit
            <$> objectValue .: "start"
            <*> objectValue .: "end"
            <*> (TE.encodeUtf8 <$> objectValue .: "replacement")
            <*> objectValue .: "category"
            <*> objectValue .: "label"

foreign import ccall safe "occam_sql_parse"
    c_parseSQL :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

foreign import ccall unsafe "occam_sql_parse_error"
    c_parseError :: CInt -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

foreign import ccall unsafe "occam_sql_count"
    c_countSQL :: Ptr Word8 -> CSize -> CUInt -> Ptr CSize -> IO CInt

foreign import ccall unsafe "occam_sql_free"
    c_freeSQL :: Ptr Word8 -> CSize -> IO ()

parseSQL :: ParseOptions -> ByteString -> IO (Either ParserError ParsedSQL)
parseSQL options sql =
    BSU.unsafeUseAsCStringLen sql \(input, inputLength) ->
        BSU.unsafeUseAsCStringLen configBytes \(configPointer, configLength) ->
            alloca \outputPointer -> alloca \outputLength -> do
                poke outputPointer nullPtr
                poke outputLength 0
                status <-
                    c_parseSQL
                        (castPtr configPointer)
                        (fromIntegral configLength)
                        (castPtr input)
                        (fromIntegral inputLength)
                        outputPointer
                        outputLength
                if status /= 0
                    then Left <$> nativeError status
                    else do
                        pointer <- peek outputPointer
                        length' <- peek outputLength
                        if pointer == nullPtr
                            then pure (Left (ParserError ParserNullResult "sqlparser returned a null result buffer"))
                            else do
                                let free = c_freeSQL pointer length'
                                    decode = do
                                        bytes <- BS.packCStringLen (castPtr pointer, fromIntegral length')
                                        case eitherDecodeStrict' bytes of
                                            Left message -> pure (Left (ParserError ParserDecodeFailure (T.pack message)))
                                            Right response ->
                                                pure $ do
                                                    units <- makeUnits sql (responseUnits response)
                                                    pure (ParsedSQL (responseInitialSession response) units)
                                finally decode free
    where
        configBytes = BL.toStrict (encode options)
        nativeError status =
            alloca \messagePointer -> alloca \messageLength -> do
                poke messagePointer nullPtr
                poke messageLength 0
                errorStatus <- c_parseError status messagePointer messageLength
                if errorStatus /= 0
                    then pure (ParserError (parserErrorKindFromStatus status) "sqlparser native parser failed")
                    else do
                        pointer <- peek messagePointer
                        length' <- peek messageLength
                        if pointer == nullPtr
                            then pure (ParserError (parserErrorKindFromStatus status) "sqlparser native parser failed")
                            else do
                                let free = c_freeSQL pointer length'
                                    decode = do
                                        bytes <- BS.packCStringLen (castPtr pointer, fromIntegral length')
                                        pure (ParserError (parserErrorKindFromStatus status) (TE.decodeUtf8Lenient bytes))
                                finally decode free

countStatements :: SQLDialect -> ByteString -> IO (Either ParserError Int)
countStatements dialect sql =
    BSU.unsafeUseAsCStringLen sql \(input, inputLength) ->
        alloca \countPointer -> do
            poke countPointer 0
            status <- c_countSQL (castPtr input) (fromIntegral inputLength) (dialectCode dialect) countPointer
            if status /= 0
                then Left <$> simpleNativeError status
                else Right . fromIntegral <$> peek countPointer

simpleNativeError :: CInt -> IO ParserError
simpleNativeError status =
    let kind = parserErrorKindFromStatus status
     in pure (ParserError kind (parserErrorMessageFor kind))

parserErrorKindFromStatus :: CInt -> ParserErrorKind
parserErrorKindFromStatus status = case status of
    1 -> ParserInvalidArguments
    2 -> ParserInvalidConfig
    3 -> ParserInvalidUtf8
    4 -> ParserNativeFailure
    5 -> ParserNativePanic
    _ -> ParserUnknownFailure (fromIntegral status)

parserErrorMessageFor :: ParserErrorKind -> Text
parserErrorMessageFor ParserInvalidArguments = "invalid native parser arguments"
parserErrorMessageFor ParserInvalidConfig = "invalid SQL parser configuration or dialect"
parserErrorMessageFor ParserInvalidUtf8 = "SQL input is not valid UTF-8"
parserErrorMessageFor ParserNativeFailure = "sqlparser native parser failed"
parserErrorMessageFor ParserNativePanic = "sqlparser native parser panicked"
parserErrorMessageFor ParserNullResult = "sqlparser returned a null result buffer"
parserErrorMessageFor ParserDecodeFailure = "sqlparser returned malformed JSON"
parserErrorMessageFor ParserInvalidSourceRange = "sqlparser returned an invalid source byte range"
parserErrorMessageFor ParserInvalidEditRange = "sqlparser returned an invalid edit range"
parserErrorMessageFor (ParserUnknownFailure code) =
    "unknown sqlparser native error " <> T.pack (show code)

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

dialectCode :: SQLDialect -> CUInt
dialectCode GenericSQL = 0
dialectCode PostgreSQL = 1
dialectCode MySQL = 2
dialectCode SQLite = 3
dialectCode DuckDB = 4
dialectCode ClickHouse = 5
dialectCode Oracle = 6
dialectCode Ansi = 7
dialectCode BigQuery = 8
dialectCode Databricks = 9
dialectCode Hive = 10
dialectCode MsSql = 11
dialectCode Redshift = 12
dialectCode Snowflake = 13
dialectCode Spark = 14
dialectCode Teradata = 15

makeUnits :: ByteString -> [NativeUnit] -> Either ParserError [StatementUnit]
makeUnits source = go 0 1
    where
        sourceLength = BS.length source
        isBoundary offset =
            offset == 0 || offset == sourceLength || BS.index source offset .&. 0xc0 /= 0x80

        go _ _ [] = Right []
        go previousEnd index (native : rest)
            | start < 0 || start >= end || end > sourceLength || start < previousEnd =
                Left (ParserError ParserInvalidSourceRange "sqlparser returned an invalid source byte range")
            | not (isBoundary start && isBoundary end) =
                Left (ParserError ParserInvalidSourceRange "sqlparser returned a source range splitting UTF-8")
            | maybe False (not . all (validEdit start end) . nativeEdits) (nativeFacts native) =
                Left (ParserError ParserInvalidEditRange "sqlparser returned an invalid edit range")
            | otherwise = do
                let statement = Statement (StatementId index) (Span start end) (BS.take (end - start) (BS.drop start source))
                    facts = fmap makeFacts (nativeFacts native)
                units <- go end (index + 1) rest
                pure (StatementUnit statement facts (nativeParseError native) : units)
            where
                start = nativeStart native
                end = nativeEnd native
                validEdit unitStart unitEnd edit =
                    parserEditStart edit >= unitStart
                        && parserEditStart edit < parserEditEnd edit
                        && parserEditEnd edit <= unitEnd
                        && isBoundary (parserEditStart edit)
                        && isBoundary (parserEditEnd edit)

        makeFacts native = StatementFacts (nativeSemantics native) (nativeEdits native)
