-- | Statement sum type and session/utility statement syntax nodes.
module Occam.Frontend.AST.Statement (
    Statement (..),
    NotifyStmt (..),
    PragmaStmt (..),
    PragmaValue (..),
    SetStmt (..),
    SetScope (..),
    SetItem (..),
    SetTarget (..),
    ResetStmt (..),
    LockTablesStmt (..),
    LockTableItem (..),
    GrantStmt (..),
    TransactionStmt (..),
    TransactionCommand (..),
    TransactionMode (..),
    ExplainStmt (..),
    ExplainOption (..),
    CopyStmt (..),
    CopySource (..),
    CopyDirection (..),
    CopyTarget (..),
    CopyOption (..),
    AnalyzeStmt (..),
    AnalyzeTable (..),
    VacuumStmt (..),
    AttachStmt (..),
    DetachStmt (..),
    PrepareStmt (..),
    PrepareSource (..),
    ExecuteStmt (..),
    DeallocateStmt (..),
    TruncateStmt (..),
    PivotStmt (..),
    OptimizeStmt (..),
    ShowStmt (..),
    RefreshMaterializedViewStmt (..),
) where

import Data.ByteString (ByteString)
import Occam.Frontend.AST.DDL
import Occam.Frontend.AST.DML
import Occam.Frontend.AST.Expr
import Occam.Frontend.AST.Query
import Occam.Frontend.Span (Located)

-- ---------------------------------------------------------------------------
-- Statements
-- ---------------------------------------------------------------------------

-- | Parsed top-level statement, including client delimiter directives.
data Statement
    = StatementDelimiter !(Located ByteString)
    | StatementQuery !(Located Query)
    | StatementInsert !(Located InsertStmt)
    | StatementUpdate !(Located UpdateStmt)
    | StatementDelete !(Located DeleteStmt)
    | StatementMerge !(Located MergeStmt)
    | StatementCreate !(Located CreateStmt)
    | StatementDrop !(Located DropStmt)
    | StatementAlter !(Located AlterStmt)
    | StatementPragma !(Located PragmaStmt)
    | StatementSet !(Located SetStmt)
    | StatementReset !(Located ResetStmt)
    | StatementLockTables !(Located LockTablesStmt)
    | StatementUnlockTables
    | StatementGrant !(Located GrantStmt)
    | StatementUse !(Located Name)
    | StatementTransaction !(Located TransactionStmt)
    | StatementExplain !(Located ExplainStmt)
    | StatementCopy !(Located CopyStmt)
    | StatementAnalyze !(Located AnalyzeStmt)
    | StatementVacuum !(Located VacuumStmt)
    | StatementAttach !(Located AttachStmt)
    | StatementDetach !(Located DetachStmt)
    | StatementPrepare !(Located PrepareStmt)
    | StatementExecute !(Located ExecuteStmt)
    | StatementDeallocate !(Located DeallocateStmt)
    | StatementTruncate !(Located TruncateStmt)
    | StatementPivot !(Located PivotStmt)
    | StatementNotify !(Located NotifyStmt)
    | StatementRenameTable !(Located [(Located Name, Located Name)])
    | StatementOptimize !(Located OptimizeStmt)
    | StatementShow !(Located ShowStmt)
    | StatementRefreshMaterializedView !(Located RefreshMaterializedViewStmt)
    deriving (Eq, Show)

-- | PostgreSQL notification channel and optional payload expression.
data NotifyStmt = NotifyStmt
    { notifyChannel :: !(Located Name)
    , notifyPayload :: !(Maybe (Located Expr))
    }
    deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Session and utility statements
-- ---------------------------------------------------------------------------

-- | PRAGMA with an optional bare or expression value.
data PragmaStmt = PragmaStmt
    { pragmaName :: !(Located Name)
    , pragmaValue :: !(Maybe PragmaValue)
    }
    deriving (Eq, Show)

-- | PRAGMA value kept as a bare spelling or parsed expression.
data PragmaValue
    = PragmaBare !(Located ByteString)
    | PragmaValueExpr !(Located Expr)
    deriving (Eq, Show)

-- | SET with scope, targets, and transaction modes.
data SetStmt = SetStmt
    { setScope :: !SetScope
    , setItems :: ![Located SetItem]
    , setTransactionModes :: ![Located ByteString]
    }
    deriving (Eq, Show)

-- | Explicit session/local/global scope, or no scope keyword.
data SetScope
    = SetScopeSession
    | SetScopeLocal
    | SetScopeGlobal
    | SetScopeNone
    deriving (Eq, Ord, Show)

-- | One SET target and its value list.
data SetItem = SetItem
    { setItemTarget :: !SetTarget
    , setItemValue :: ![Located Expr]
    , setItemToSyntax :: !Bool
    }
    deriving (Eq, Show)

-- | SET/RESET target namespace: ordinary name, user/system variable, or special target.
data SetTarget
    = SetTargetName !(Located Name)
    | SetTargetUserVar !(Located ByteString)
    | SetTargetSysVar !(Located ByteString)
    | SetTargetSpecial !(Located ByteString)
    deriving (Eq, Ord, Show)

-- | RESET configuration parameter, ROLE, SESSION AUTHORIZATION, or ALL.
data ResetStmt
    = ResetAll
    | ResetRole
    | ResetSessionAuthorization
    | ResetTarget !SetTarget
    deriving (Eq, Ord, Show)

-- | MySQL/MariaDB @LOCK TABLES@ statement.
newtype LockTablesStmt = LockTablesStmt
    { lockTablesItems :: [Located LockTableItem]
    }
    deriving (Eq, Ord, Show)

-- | One table entry in @LOCK TABLES@.
data LockTableItem = LockTableItem
    { lockTableName :: !(Located Name)
    , lockTableAlias :: !(Maybe (Located Alias))
    , lockTableType :: !(Located ByteString)
    }
    deriving (Eq, Ord, Show)

-- | @GRANT@ or @REVOKE@ privilege statement.
data GrantStmt = GrantStmt
    { grantIsRevoke :: !Bool
    , grantPrivileges :: ![Located ByteString]
    , grantObject :: !(Maybe (Located Name))
    , grantGrantees :: ![Located Name]
    }
    deriving (Eq, Ord, Show)

-- | Transaction control command with optional savepoint, modes, and chain.
data TransactionStmt = TransactionStmt
    { transactionCommand :: !TransactionCommand
    , transactionName :: !(Maybe (Located NamePart))
    , transactionModes :: ![TransactionMode]
    , transactionChain :: !(Maybe Bool)
    }
    deriving (Eq, Ord, Show)

-- | Transaction command normalized across BEGIN/START and COMMIT/END spellings.
data TransactionCommand
    = TransactionBegin
    | TransactionStart
    | TransactionCommit
    | TransactionRollback
    | TransactionRollbackTo
    | TransactionSavepoint
    | TransactionReleaseSavepoint
    | TransactionEnd
    deriving (Eq, Ord, Show)

-- | Transaction setting; unmodeled mode spelling is retained for conservative analysis.
data TransactionMode
    = TransactionReadOnly !Bool
    | TransactionBeginLock !ByteString
    | TransactionUnmodeledMode !ByteString
    deriving (Eq, Ord, Show)

-- | EXPLAIN options and the explained statement.
data ExplainStmt = ExplainStmt
    { explainOptions :: ![Located ExplainOption]
    , explainStatement :: !(Located Statement)
    }
    deriving (Eq, Show)

-- | EXPLAIN option name and optional source spelling of its value.
data ExplainOption = ExplainOption
    { explainOptionName :: !(Located ByteString)
    , explainOptionValue :: !(Maybe (Located ByteString))
    }
    deriving (Eq, Ord, Show)

-- | COPY source, direction, target, and options.
data CopyStmt = CopyStmt
    { copySource :: !CopySource
    , copyDirection :: !CopyDirection
    , copyTarget :: !CopyTarget
    , copyOptions :: ![Located CopyOption]
    }
    deriving (Eq, Show)

-- | COPY input relation with optional columns, or a query supplying output rows.
data CopySource
    = CopySourceTable !(Located Name) ![Located NamePart]
    | CopySourceQuery !(Located Query)
    deriving (Eq, Show)

-- | Direction of data flow relative to the COPY statement.
data CopyDirection
    = CopyFrom
    | CopyTo
    deriving (Eq, Ord, Show)

-- | COPY file, standard stream, or external program endpoint.
data CopyTarget
    = CopyFile !(Located Literal)
    | CopyStdin
    | CopyStdout
    | CopyProgram !(Located Literal)
    deriving (Eq, Ord, Show)

-- | COPY option name and optional raw value.
data CopyOption = CopyOption
    { copyOptionName :: !(Located ByteString)
    , copyOptionValue :: !(Maybe (Located ByteString))
    }
    deriving (Eq, Ord, Show)

-- | ANALYZE flags and table list.
data AnalyzeStmt = AnalyzeStmt
    { analyzeVerbose :: !Bool
    , analyzeTables :: ![Located AnalyzeTable]
    }
    deriving (Eq, Ord, Show)

-- | ANALYZE target relation and optional selected columns.
data AnalyzeTable = AnalyzeTable
    { analyzeTableName :: !(Located Name)
    , analyzeTableColumns :: ![Located NamePart]
    }
    deriving (Eq, Ord, Show)

-- | VACUUM flags, target, and SQLite @INTO@ target.
data VacuumStmt = VacuumStmt
    { vacuumFull :: !Bool
    , vacuumFreeze :: !Bool
    , vacuumVerbose :: !Bool
    , vacuumAnalyze :: !Bool
    , vacuumSchema :: !(Maybe (Located Name))
    , vacuumTable :: !(Maybe (Located Name))
    , vacuumColumns :: ![Located NamePart]
    , vacuumInto :: !(Maybe (Located Expr))
    }
    deriving (Eq, Show)

-- | SQLite/DuckDB database attachment or ClickHouse object attachment.
data AttachStmt
    = AttachDatabase
        !(Located Expr)
        !(Maybe (Located NamePart))
        !Bool
    | AttachObject
        !(Located ByteString)
        !(Located Name)
        !Bool
    deriving (Eq, Show)

-- | SQLite/DuckDB database detachment or ClickHouse object detachment.
data DetachStmt
    = DetachDatabase !(Located Name) !Bool
    | DetachObject !(Located ByteString) !(Located Name) !Bool !Bool
    deriving (Eq, Ord, Show)

-- | PREPARE name, parameter types, and source.
data PrepareStmt = PrepareStmt
    { prepareName :: !(Located Name)
    , prepareParams :: ![Located TypeName]
    , prepareSource :: !PrepareSource
    }
    deriving (Eq, Show)

-- | Prepared statement body represented as a statement or a dialect expression.
data PrepareSource
    = PrepareStatement !(Located Statement)
    | PrepareExpression !(Located Expr)
    deriving (Eq, Show)

-- | EXECUTE prepared name and arguments.
data ExecuteStmt = ExecuteStmt
    { executeName :: !(Located Name)
    , executeArgs :: ![Located Expr]
    }
    deriving (Eq, Show)

-- | DEALLOCATE ALL or a named prepared statement.
data DeallocateStmt = DeallocateStmt
    { deallocateAll :: !Bool
    , deallocateName :: !(Maybe (Located Name))
    }
    deriving (Eq, Ord, Show)

-- | TRUNCATE targets and identity/behavior flags.
data TruncateStmt = TruncateStmt
    { truncateOnly :: !Bool
    , truncateTables :: ![Located Name]
    , truncateRestartIdentity :: !(Maybe Bool)
    , truncateBehavior :: !(Maybe DropBehavior)
    }
    deriving (Eq, Ord, Show)

-- | DuckDB PIVOT/UNPIVOT statement.
data PivotStmt = PivotStmt
    { pivotIsUnpivot :: !Bool
    , pivotSource :: !(Located TableRef)
    , pivotOn :: ![Located Expr]
    , pivotIn :: ![Located Expr]
    , pivotUsing :: ![Located SelectItem]
    , pivotGroupBy :: ![Located Expr]
    , pivotIntoName :: !(Maybe (Located NamePart))
    , pivotIntoValue :: !(Maybe (Located NamePart))
    }
    deriving (Eq, Show)

-- | ClickHouse/MySQL OPTIMIZE TABLE statement.
data OptimizeStmt = OptimizeStmt
    { optimizeTable :: !(Located Name)
    , optimizePartition :: !(Maybe (Located Expr))
    , optimizeFinal :: !Bool
    , optimizeDeduplicate :: !Bool
    }
    deriving (Eq, Show)

-- | SHOW subject and optional target, LIKE, and WHERE.
data ShowStmt = ShowStmt
    { showSubject :: ![Located NamePart]
    , showTarget :: !(Maybe (Located Name))
    , showLike :: !(Maybe (Located Expr))
    , showWhere :: !(Maybe (Located Expr))
    }
    deriving (Eq, Show)

-- | PostgreSQL @REFRESH MATERIALIZED VIEW [CONCURRENTLY] name [WITH [NO] DATA]@.
data RefreshMaterializedViewStmt = RefreshMaterializedViewStmt
    { refreshConcurrently :: !Bool
    , refreshName :: !(Located Name)
    , refreshWithData :: !Bool
    }
    deriving (Eq, Ord, Show)
