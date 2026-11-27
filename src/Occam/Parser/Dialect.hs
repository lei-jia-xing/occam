-- | Dialect profiles: lexical rules, keywords, folding, features, and semantics.
module Occam.Parser.Dialect (
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
    postgresqlProfile,
    mysqlProfile,
    mariadbProfile,
    sqliteProfile,
    duckdbProfile,
    clickhouseProfile,
) where

import Data.Char (isAsciiUpper)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Occam.Core (BeginPolicy (..))
import Occam.Parser.Types (SQLDialect (..), dialectName)

-- | Identifier case-folding transformation applied per dialect and quote status.
data CaseFolding
    = FoldLower
    | FoldAsciiLower
    | FoldPreserve
    deriving (Eq, Ord, Show)

-- | Apply a dialect's identifier folding.
applyCaseFolding :: CaseFolding -> Text -> Text
applyCaseFolding FoldLower = T.toLower
applyCaseFolding FoldAsciiLower = T.map toAsciiLower
    where
        toAsciiLower c
            | isAsciiUpper c = toEnum (fromEnum c + 32)
            | otherwise = c
applyCaseFolding FoldPreserve = id

-- | Dialect-specific lexical rules used by the pure-Haskell lexer and splitter.
data LexicalProfile = LexicalProfile
    { lexBacktickQuotes :: !Bool
    , lexBracketQuotes :: !Bool
    , lexDollarQuoting :: !Bool
    , lexNestedBlockComments :: !Bool
    , lexHashComments :: !Bool
    , lexConditionalComments :: !Bool
    , lexEscapeStrings :: !Bool
    , lexBackslashEscapesInSingleQuote :: !Bool
    , lexDollarParameters :: !Bool
    , lexNamedParameters :: !Bool
    , lexNumberedQuestionParameters :: !Bool
    , lexQuestionOperators :: !Bool
    , lexUserVariables :: !Bool
    , lexCompoundBlocks :: !Bool
    }
    deriving (Eq, Show)

-- | Identifier normalization rules for unquoted and quoted spellings.
data IdentifierProfile = IdentifierProfile
    { identUnquotedFolding :: !CaseFolding
    , identQuotedFolding :: !CaseFolding
    , identSupportsLowerCaseTableNames :: !Bool
    }
    deriving (Eq, Show)

-- | Modular grammar & semantic capability flags gated per dialect.
data DialectFeature
    = FeatureSelectIntoCreatesTable
    | FeatureSelectIntoUserVar
    | FeatureReturningClause
    | FeatureIndexLengthPrefix
    | FeaturePragmas
    | FeatureCreateMacro
    | FeaturePivotUnpivot
    | FeatureQualifyClause
    | FeaturePrewhereSettings
    | FeatureEngineClause
    | FeatureAttachDetach
    | FeatureOnDuplicateKeyUpdate
    | FeatureOnConflict
    | FeatureMergeStatement
    | FeatureCopyStatement
    | FeatureCreateStatistics
    | FeatureAlterPartitionInherit
    | FeatureFulltextIndex
    | FeatureVirtualTables
    | FeatureSequenceDDL
    | FeatureMaterializedViews
    | FeatureDropMaterializedViews
    | FeaturePreparedStatements
    | FeatureColonCast
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | How @USE@ / @search_path@ modifies object resolution namespaces.
data NamespaceRule
    = NamespaceSearchPath
    | NamespaceOneLevel
    | NamespaceTwoPartDuckDB
    | NamespaceNone
    deriving (Eq, Ord, Show)

-- | How unqualified index names are scoped in the catalog.
data IndexScopeRule
    = IndexSchemaScoped
    | IndexTableOwned
    deriving (Eq, Ord, Show)

-- | How unqualified trigger names are scoped in the catalog.
data TriggerScopeRule
    = TriggerSchemaScoped
    | TriggerTableScoped
    deriving (Eq, Ord, Show)

-- | Session and catalog semantic rules for a dialect.
data SemanticsProfile = SemanticsProfile
    { semDefaultForeignKeyChecks :: !Bool
    , semTableTransactional :: !(Bool -> Bool)
    , semSequenceTransactional :: !Bool
    , semImplicitDdlCommit :: !Bool
    , semBeginPolicy :: !BeginPolicy
    , semNamespaceRule :: !NamespaceRule
    , semIndexScope :: !IndexScopeRule
    , semTriggerScope :: !TriggerScopeRule
    , semDefaultTriggerForEachRow :: !Bool
    , semTempSchemas :: ![Text]
    }

-- | Complete capability specification for a concrete SQL dialect.
data DialectProfile = DialectProfile
    { dialectId :: !SQLDialect
    , dialectCanonicalName :: !Text
    , dialectCliAliases :: ![Text]
    , dialectLexical :: !LexicalProfile
    , dialectIdentifiers :: !IdentifierProfile
    , dialectKeywords :: !(Set Text)
    , dialectFeatures :: !(Set DialectFeature)
    , dialectSemantics :: !SemanticsProfile
    }

-- | Profiles in `--list-dialects` order.
allDialectProfiles :: [DialectProfile]
allDialectProfiles =
    [ postgresqlProfile
    , mysqlProfile
    , mariadbProfile
    , sqliteProfile
    , duckdbProfile
    , clickhouseProfile
    ]

-- | Total lookup for the six supported dialects.
dialectProfile :: SQLDialect -> DialectProfile
dialectProfile PostgreSQL = postgresqlProfile
dialectProfile MySQL = mysqlProfile
dialectProfile MariaDB = mariadbProfile
dialectProfile SQLite = sqliteProfile
dialectProfile DuckDB = duckdbProfile
dialectProfile ClickHouse = clickhouseProfile

dialectAliasMap :: Map Text SQLDialect
dialectAliasMap =
    Map.fromList
        [ (T.toLower alias, dialectId p)
        | p <- allDialectProfiles
        , alias <- dialectCanonicalName p : dialectCliAliases p
        ]

-- | Resolve a canonical name or CLI alias (case-insensitive).
lookupDialectByName :: Text -> Maybe SQLDialect
lookupDialectByName raw = Map.lookup (T.toLower raw) dialectAliasMap

-- | Whether a dialect declares a grammar feature.
dialectHasFeature :: SQLDialect -> DialectFeature -> Bool
dialectHasFeature dialect feature =
    Set.member feature (dialectFeatures (dialectProfile dialect))

postgresqlProfile :: DialectProfile
postgresqlProfile =
    DialectProfile
        { dialectId = PostgreSQL
        , dialectCanonicalName = dialectName PostgreSQL
        , dialectCliAliases = ["postgres"]
        , dialectLexical =
            LexicalProfile
                { lexBacktickQuotes = False
                , lexBracketQuotes = False
                , lexDollarQuoting = True
                , lexNestedBlockComments = True
                , lexHashComments = False
                , lexConditionalComments = False
                , lexEscapeStrings = True
                , lexBackslashEscapesInSingleQuote = False
                , lexDollarParameters = True
                , lexNamedParameters = False
                , lexNumberedQuestionParameters = False
                , lexQuestionOperators = True
                , lexUserVariables = False
                , lexCompoundBlocks = True
                }
        , dialectIdentifiers =
            IdentifierProfile
                { identUnquotedFolding = FoldLower
                , identQuotedFolding = FoldPreserve
                , identSupportsLowerCaseTableNames = False
                }
        , dialectKeywords =
            Set.fromList
                [ "ANALYZE"
                , "CONVERSION"
                , "COPY"
                , "ILIKE"
                , "INHERITS"
                , "LATERAL"
                , "MATERIALIZED"
                , "MERGE"
                , "ONLY"
                , "RETURNING"
                , "RULE"
                , "STATISTICS"
                , "VARIADIC"
                ]
        , dialectFeatures =
            Set.fromList
                [ FeatureSelectIntoCreatesTable
                , FeatureReturningClause
                , FeatureOnConflict
                , FeatureMergeStatement
                , FeatureCopyStatement
                , FeatureCreateStatistics
                , FeatureAlterPartitionInherit
                , FeatureSequenceDDL
                , FeatureMaterializedViews
                , FeatureDropMaterializedViews
                , FeaturePreparedStatements
                , FeatureColonCast
                ]
        , dialectSemantics =
            SemanticsProfile
                { semDefaultForeignKeyChecks = True
                , semTableTransactional = const True
                , semSequenceTransactional = False
                , semImplicitDdlCommit = False
                , semBeginPolicy = BeginWarn
                , semNamespaceRule = NamespaceSearchPath
                , semIndexScope = IndexSchemaScoped
                , semTriggerScope = TriggerTableScoped
                , semDefaultTriggerForEachRow = False
                , semTempSchemas = ["pg_temp"]
                }
        }

mysqlSharedLexical :: LexicalProfile
mysqlSharedLexical =
    LexicalProfile
        { lexBacktickQuotes = True
        , lexBracketQuotes = False
        , lexDollarQuoting = False
        , lexNestedBlockComments = False
        , lexHashComments = True
        , lexConditionalComments = True
        , lexEscapeStrings = False
        , lexBackslashEscapesInSingleQuote = True
        , lexDollarParameters = False
        , lexNamedParameters = False
        , lexNumberedQuestionParameters = False
        , lexQuestionOperators = False
        , lexUserVariables = True
        , lexCompoundBlocks = True
        }

mysqlSharedSemantics :: SemanticsProfile
mysqlSharedSemantics =
    SemanticsProfile
        { semDefaultForeignKeyChecks = True
        , semTableTransactional = not
        , semSequenceTransactional = False
        , semImplicitDdlCommit = True
        , semBeginPolicy = BeginCommitActive
        , semNamespaceRule = NamespaceOneLevel
        , semIndexScope = IndexTableOwned
        , semTriggerScope = TriggerSchemaScoped
        , semDefaultTriggerForEachRow = True
        , semTempSchemas = []
        }

mysqlProfile :: DialectProfile
mysqlProfile =
    DialectProfile
        { dialectId = MySQL
        , dialectCanonicalName = dialectName MySQL
        , dialectCliAliases = []
        , dialectLexical = mysqlSharedLexical
        , dialectIdentifiers =
            IdentifierProfile
                { identUnquotedFolding = FoldLower
                , identQuotedFolding = FoldLower
                , identSupportsLowerCaseTableNames = True
                }
        , dialectKeywords =
            Set.fromList
                [ "ALGORITHM"
                , "CHANGE"
                , "DUPLICATE"
                , "ENGINE"
                , "FLUSH"
                , "FULLTEXT"
                , "LOCK"
                , "MODIFY"
                , "OPTIMIZE"
                , "PREPARE"
                , "UNLOCK"
                , "XOR"
                ]
        , dialectFeatures =
            Set.fromList
                [ FeatureSelectIntoUserVar
                , FeatureIndexLengthPrefix
                , FeatureEngineClause
                , FeatureOnDuplicateKeyUpdate
                , FeatureFulltextIndex
                , FeaturePreparedStatements
                ]
        , dialectSemantics = mysqlSharedSemantics
        }

mariadbProfile :: DialectProfile
mariadbProfile =
    DialectProfile
        { dialectId = MariaDB
        , dialectCanonicalName = dialectName MariaDB
        , dialectCliAliases = []
        , dialectLexical = mysqlSharedLexical
        , dialectIdentifiers =
            IdentifierProfile
                { identUnquotedFolding = FoldLower
                , identQuotedFolding = FoldLower
                , identSupportsLowerCaseTableNames = True
                }
        , dialectKeywords =
            Set.union
                (dialectKeywords mysqlProfile)
                (Set.fromList ["RETURNING", "SEQUENCE"])
        , dialectFeatures =
            Set.union
                (dialectFeatures mysqlProfile)
                (Set.fromList [FeatureReturningClause, FeatureSequenceDDL])
        , dialectSemantics = mysqlSharedSemantics
        }

sqliteProfile :: DialectProfile
sqliteProfile =
    DialectProfile
        { dialectId = SQLite
        , dialectCanonicalName = dialectName SQLite
        , dialectCliAliases = []
        , dialectLexical =
            LexicalProfile
                { lexBacktickQuotes = True
                , lexBracketQuotes = True
                , lexDollarQuoting = False
                , lexNestedBlockComments = False
                , lexHashComments = False
                , lexConditionalComments = False
                , lexEscapeStrings = False
                , lexBackslashEscapesInSingleQuote = False
                , lexDollarParameters = True
                , lexNamedParameters = True
                , lexNumberedQuestionParameters = True
                , lexQuestionOperators = False
                , lexUserVariables = False
                , lexCompoundBlocks = True
                }
        , dialectIdentifiers =
            IdentifierProfile
                { identUnquotedFolding = FoldAsciiLower
                , identQuotedFolding = FoldAsciiLower
                , identSupportsLowerCaseTableNames = False
                }
        , dialectKeywords =
            Set.fromList
                [ "ATTACH"
                , "AUTOINCREMENT"
                , "CONFLICT"
                , "DETACH"
                , "GLOB"
                , "INDEXED"
                , "PRAGMA"
                , "REINDEX"
                , "RETURNING"
                , "VACUUM"
                , "VIRTUAL"
                ]
        , dialectFeatures =
            Set.fromList
                [ FeatureReturningClause
                , FeaturePragmas
                , FeatureAttachDetach
                , FeatureOnConflict
                , FeatureVirtualTables
                ]
        , dialectSemantics =
            SemanticsProfile
                { semDefaultForeignKeyChecks = False
                , semTableTransactional = const True
                , semSequenceTransactional = True
                , semImplicitDdlCommit = False
                , semBeginPolicy = BeginError
                , semNamespaceRule = NamespaceNone
                , semIndexScope = IndexSchemaScoped
                , semTriggerScope = TriggerSchemaScoped
                , semDefaultTriggerForEachRow = True
                , semTempSchemas = ["temp"]
                }
        }

duckdbProfile :: DialectProfile
duckdbProfile =
    DialectProfile
        { dialectId = DuckDB
        , dialectCanonicalName = dialectName DuckDB
        , dialectCliAliases = []
        , dialectLexical =
            LexicalProfile
                { lexBacktickQuotes = False
                , lexBracketQuotes = False
                , lexDollarQuoting = True
                , lexNestedBlockComments = False
                , lexHashComments = False
                , lexConditionalComments = False
                , lexEscapeStrings = True
                , lexBackslashEscapesInSingleQuote = False
                , lexDollarParameters = True
                , lexNamedParameters = False
                , lexNumberedQuestionParameters = False
                , lexQuestionOperators = False
                , lexUserVariables = False
                , lexCompoundBlocks = False
                }
        , dialectIdentifiers =
            IdentifierProfile
                { identUnquotedFolding = FoldAsciiLower
                , identQuotedFolding = FoldAsciiLower
                , identSupportsLowerCaseTableNames = False
                }
        , dialectKeywords =
            Set.fromList
                [ "ASOF"
                , "ATTACH"
                , "COPY"
                , "DETACH"
                , "EXCLUDE"
                , "MACRO"
                , "MERGE"
                , "PIVOT"
                , "PRAGMA"
                , "QUALIFY"
                , "RETURNING"
                , "UNPIVOT"
                ]
        , dialectFeatures =
            Set.fromList
                [ FeatureReturningClause
                , FeaturePragmas
                , FeatureCreateMacro
                , FeaturePivotUnpivot
                , FeatureQualifyClause
                , FeatureAttachDetach
                , FeatureOnConflict
                , FeatureMergeStatement
                , FeatureCopyStatement
                , FeatureSequenceDDL
                , FeaturePreparedStatements
                , FeatureColonCast
                ]
        , dialectSemantics =
            SemanticsProfile
                { semDefaultForeignKeyChecks = True
                , semTableTransactional = const True
                , semSequenceTransactional = True
                , semImplicitDdlCommit = False
                , semBeginPolicy = BeginWarn
                , semNamespaceRule = NamespaceTwoPartDuckDB
                , semIndexScope = IndexSchemaScoped
                , semTriggerScope = TriggerSchemaScoped
                , semDefaultTriggerForEachRow = False
                , semTempSchemas = ["temp"]
                }
        }

clickhouseProfile :: DialectProfile
clickhouseProfile =
    DialectProfile
        { dialectId = ClickHouse
        , dialectCanonicalName = dialectName ClickHouse
        , dialectCliAliases = []
        , dialectLexical =
            LexicalProfile
                { lexBacktickQuotes = True
                , lexBracketQuotes = False
                , lexDollarQuoting = False
                , lexNestedBlockComments = True
                , lexHashComments = False
                , lexConditionalComments = False
                , lexEscapeStrings = False
                , lexBackslashEscapesInSingleQuote = True
                , lexDollarParameters = False
                , lexNamedParameters = False
                , lexNumberedQuestionParameters = False
                , lexQuestionOperators = False
                , lexUserVariables = False
                , lexCompoundBlocks = False
                }
        , dialectIdentifiers =
            IdentifierProfile
                { identUnquotedFolding = FoldPreserve
                , identQuotedFolding = FoldPreserve
                , identSupportsLowerCaseTableNames = False
                }
        , dialectKeywords =
            Set.fromList
                [ "ARRAY"
                , "ATTACH"
                , "DETACH"
                , "ENGINE"
                , "FINAL"
                , "MATERIALIZED"
                , "OPTIMIZE"
                , "PREWHERE"
                , "SETTINGS"
                , "TTL"
                ]
        , dialectFeatures =
            Set.fromList
                [ FeaturePrewhereSettings
                , FeatureEngineClause
                , FeatureAttachDetach
                , FeatureMaterializedViews
                , FeatureQualifyClause
                , FeatureColonCast
                ]
        , dialectSemantics =
            SemanticsProfile
                { semDefaultForeignKeyChecks = True
                , semTableTransactional = const False
                , semSequenceTransactional = False
                , semImplicitDdlCommit = False
                , semBeginPolicy = BeginWarn
                , semNamespaceRule = NamespaceOneLevel
                , semIndexScope = IndexSchemaScoped
                , semTriggerScope = TriggerSchemaScoped
                , semDefaultTriggerForEachRow = False
                , semTempSchemas = []
                }
        }
