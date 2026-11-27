-- | Span-annotated SQL tree with exact UTF-8 spans, raw names, and opaque bodies.
module Occam.Frontend.AST (
    NamePart (..),
    Name (..),
    Alias (..),
    LiteralKind (..),
    Literal (..),
    ParamKind (..),

    -- * Expressions
    Expr (..),
    UnaryOp (..),
    BinaryOp (..),
    LikeOp (..),
    Quantifier (..),
    CastStyle (..),
    FunctionCall (..),
    FunctionArg (..),
    NullTreatment (..),
    CaseExpr (..),
    WindowRef (..),
    WindowSpec (..),
    WindowFrame (..),
    WindowFrameUnit (..),
    WindowBound (..),

    -- * Queries
    Query (..),
    WithClause (..),
    Cte (..),
    MaterializedHint (..),
    QueryBody (..),
    SetOperator (..),
    SetQuantifier (..),
    SelectCore (..),
    SelectInto (..),
    SelectIntoScope (..),
    SelectQuantifier (..),
    SelectItem (..),
    GroupByItem (..),
    CteBody (..),
    FromClause (..),
    TableRef (..),
    TableSample (..),
    TableSource (..),
    JoinClause (..),
    JoinKind (..),
    JoinTarget (..),
    ArrayJoinItem (..),
    JoinConstraint (..),
    OrderItem (..),
    SortDirection (..),
    NullsOrder (..),
    LimitClause (..),
    FetchClause (..),
    ForClause (..),
    ForStrength (..),
    WindowDef (..),
    SettingItem (..),

    -- * Statements
    Statement (..),
    NotifyStmt (..),
    InsertStmt (..),
    InsertOrAction (..),
    InsertSource (..),
    OnConflictClause (..),
    OnConflictAction (..),
    Assignment (..),
    UpdateStmt (..),
    DeleteStmt (..),
    DeleteTarget (..),
    MergeStmt (..),
    MergeClause (..),
    MergeAction (..),

    -- * DDL
    CreateStmt (..),
    CreateObject (..),
    CreateTable (..),
    TableElement (..),
    SkippingIndex (..),
    ProjectionDef (..),
    ColumnDef (..),
    ColumnConstraint (..),
    ColumnConflictAction (..),
    TableConstraint (..),
    ConstraintIndexOptions (..),
    TableLikeOption (..),
    ForeignKeyRef (..),
    ForeignKeyAction (..),
    ForeignKeyActionKind (..),
    ForeignKeyTiming (..),
    TypeName (..),
    TypeArg (..),
    IndexKeyPart (..),
    CreateIndex (..),
    CreateView (..),
    ViewBody (..),
    OpaqueBody (..),
    OpaqueKind (..),
    CreateSequence (..),
    CreateSchema (..),
    CreateDatabase (..),
    CreateStatistics (..),
    CreateMacro (..),
    MacroParam (..),
    CreatePolicy (..),
    CreateRule (..),
    CreateTrigger (..),
    TriggerGranularity (..),
    CreateRoutine (..),
    RoutineParam (..),
    CreateAggregate (..),
    CreateVirtualTable (..),
    VirtualTableArg (..),
    CreateExtension (..),
    TableOption (..),
    DropStmt (..),
    DropObject (..),
    DropBehavior (..),
    AlterStmt (..),
    AlterObject (..),
    AlterAction (..),
    AlterColumnAction (..),
    ColumnPosition (..),
    GenericOption (..),
    RlsAction (..),
    PartitionBound (..),
    PartitionBy (..),
    TablePartition (..),

    -- * Session and utility statements
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
import Data.Data (Data)
import Occam.Frontend.Lexer (LiteralKind (..))
import Occam.Frontend.Span (ByteSpan, Located)

-- ---------------------------------------------------------------------------
-- Names, literals, and parameters
-- ---------------------------------------------------------------------------

{- | One identifier component.  'namePartRaw' is the exact source spelling
(quotes included when quoted); 'namePartQuoted' distinguishes quoted
identifiers from unquoted keywords/identifiers for case folding.
-}
data NamePart = NamePart
    { namePartRaw :: !ByteString
    , namePartQuoted :: !Bool
    }
    deriving (Eq, Ord, Show, Data)

-- | A possibly schema-qualified object or column name.
newtype Name = Name
    { nameParts :: [Located NamePart]
    }
    deriving (Eq, Ord, Show, Data)

-- | Table / subquery alias, optionally with a column-name list.
data Alias = Alias
    { aliasName :: !(Located NamePart)
    , aliasColumns :: ![Located NamePart]
    }
    deriving (Eq, Ord, Show, Data)

-- | Literal token with its original source spelling.
data Literal = Literal
    { literalKind :: !LiteralKind
    , literalRaw :: !ByteString
    }
    deriving (Eq, Ord, Show, Data)

-- | Parameter or variable class.
data ParamKind
    = ParamPositional
    | ParamUser
    | ParamSystem
    deriving (Eq, Ord, Show, Data)

-- ---------------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------------

-- | Expression tree; spans cover the full expression text.
data Expr
    = ExprLiteral !Literal
    | ExprNull
    | ExprBoolean !Bool
    | ExprColumn !(Located Name)
    | ExprStar
    | ExprQualifiedStar !(Located Name)
    | ExprParam !ParamKind !ByteString
    | ExprDefault
    | ExprUnary !UnaryOp !(Located Expr)
    | ExprBinary !BinaryOp !(Located Expr) !(Located Expr)
    | ExprCast !CastStyle !(Located Expr) !(Located TypeName)
    | ExprCastExpr !(Located Expr) !(Located Expr)
    | ExprLambda ![Located NamePart] !(Located Expr)
    | ExprCharsetString !ByteString !(Located Literal)
    | ExprTypedString !ByteString !(Located Literal)
    | ExprSubscript
        !(Located Expr)
        !(Maybe (Located Expr))
        !(Maybe (Located Expr))
    | ExprField !(Located Expr) !(Located NamePart)
    | ExprFieldIndex !(Located Expr) !(Located ByteString)
    | ExprFieldStar !(Located Expr)
    | ExprCall !(Located FunctionCall)
    | ExprCase !CaseExpr
    | ExprExists !(Located Query)
    | ExprIsNull !Bool !(Located Expr)
    | ExprIsTrue !Bool !(Located Expr)
    | ExprIsFalse !Bool !(Located Expr)
    | ExprIsUnknown !Bool !(Located Expr)
    | ExprIsDistinct !Bool !(Located Expr) !(Located Expr)
    | ExprBetween !Bool !(Located Expr) !(Located Expr) !(Located Expr)
    | ExprInList !Bool !(Located Expr) ![Located Expr]
    | ExprInSubquery !Bool !(Located Expr) !(Located Query)
    | ExprLike !Bool !LikeOp !(Located Expr) !(Located Expr) !(Maybe (Located Expr))
    | ExprQuantified
        !Quantifier
        !BinaryOp
        !(Located Expr)
        !(Located Expr)
    | ExprArray ![Located Expr]
    | ExprStruct ![(Located Expr, Located Expr)]
    | ExprBareKeyword !(Located ByteString)
    | ExprTuple ![Located Expr]
    | ExprSubquery !(Located Query)
    | ExprParen !(Located Expr)
    | ExprCollate !(Located Expr) !(Located Name)
    deriving (Eq, Ord, Show, Data)

data UnaryOp
    = UnaryNot
    | UnaryNegate
    | UnaryPositive
    | UnaryBitNot
    deriving (Eq, Ord, Show, Data)

data BinaryOp
    = OpOr
    | OpAnd
    | OpXor
    | OpEq
    | OpNotEq
    | OpLt
    | OpLtEq
    | OpGt
    | OpGtEq
    | OpAdd
    | OpSub
    | OpMul
    | OpDiv
    | OpMod
    | OpPow
    | OpConcat
    | OpBitAnd
    | OpBitOr
    | OpBitXor
    | OpShiftLeft
    | OpShiftRight
    | OpJsonGet
    | OpJsonGetText
    | OpJsonPathGet
    | OpJsonPathGetText
    | OpContains
    | OpContainedBy
    | OpOverlap
    | OpRegexMatch
    | OpRegexIMatch
    | OpRegexNotMatch
    | OpRegexNotIMatch
    | OpCustom !ByteString
    deriving (Eq, Ord, Show, Data)

data LikeOp
    = LikeLike
    | LikeILike
    | LikeGlob
    | LikeRegexp
    deriving (Eq, Ord, Show, Data)

data Quantifier
    = QuantAny
    | QuantAll
    | QuantSome
    deriving (Eq, Ord, Show, Data)

data CastStyle
    = CastDoubleColon
    | CastDotColon
    | CastFunction
    | CastTryFunction
    deriving (Eq, Ord, Show, Data)

-- | Function call with aggregate/window decorations.
data FunctionCall = FunctionCall
    { functionName :: !(Located Name)
    , functionDistinct :: !Bool
    , functionArgs :: ![Located FunctionArg]
    , functionOrderBy :: ![Located OrderItem]
    , functionWithinGroup :: ![Located OrderItem]
    , functionFilter :: !(Maybe (Located Expr))
    , functionNullTreatment :: !(Maybe NullTreatment)
    , functionOver :: !(Maybe WindowRef)
    }
    deriving (Eq, Ord, Show, Data)

data NullTreatment
    = NullTreatmentRespect
    | NullTreatmentIgnore
    deriving (Eq, Ord, Show, Data)

data FunctionArg
    = FunctionArgExpr !(Located Expr)
    | FunctionArgStar
    | FunctionArgNamed !(Located NamePart) !(Located Expr)
    deriving (Eq, Ord, Show, Data)

-- | Simple or searched CASE expression.
data CaseExpr = CaseExpr
    { caseOperand :: !(Maybe (Located Expr))
    , caseWhens :: ![(Located Expr, Located Expr)]
    , caseElse :: !(Maybe (Located Expr))
    }
    deriving (Eq, Ord, Show, Data)

-- | Window reference: a named window or an inline specification.
data WindowRef
    = WindowRefName !(Located NamePart)
    | WindowRefSpec !WindowSpec
    deriving (Eq, Ord, Show, Data)

-- | Window partitioning, ordering, and frame.
data WindowSpec = WindowSpec
    { windowBaseName :: !(Maybe (Located NamePart))
    , windowPartitionBy :: ![Located Expr]
    , windowOrderBy :: ![Located OrderItem]
    , windowFrame :: !(Maybe WindowFrame)
    }
    deriving (Eq, Ord, Show, Data)

data WindowFrame = WindowFrame
    { windowFrameUnit :: !WindowFrameUnit
    , windowFrameStart :: !WindowBound
    , windowFrameEnd :: !(Maybe WindowBound)
    }
    deriving (Eq, Ord, Show, Data)

data WindowFrameUnit
    = FrameRows
    | FrameRange
    | FrameGroups
    deriving (Eq, Ord, Show, Data)

data WindowBound
    = BoundUnboundedPreceding
    | BoundPreceding !(Located Expr)
    | BoundCurrentRow
    | BoundFollowing !(Located Expr)
    | BoundUnboundedFollowing
    deriving (Eq, Ord, Show, Data)

-- ---------------------------------------------------------------------------
-- Queries
-- ---------------------------------------------------------------------------

-- | Query with optional WITH, query body, and trailing clauses.
data Query = Query
    { queryWith :: !(Maybe (Located WithClause))
    , queryBody :: !(Located QueryBody)
    , queryOrderBy :: ![Located OrderItem]
    , queryLimit :: !(Maybe (Located LimitClause))
    , queryOffset :: !(Maybe (Located Expr))
    , queryFetch :: !(Maybe (Located FetchClause))
    , queryFor :: !(Maybe (Located ForClause))
    , querySettings :: !(Maybe (Located [Located SettingItem]))
    , queryFormat :: !(Maybe (Located NamePart))
    }
    deriving (Eq, Ord, Show, Data)

-- | WITH clause and its CTE list.
data WithClause = WithClause
    { withRecursive :: !Bool
    , withCtes :: ![Located Cte]
    }
    deriving (Eq, Ord, Show, Data)

-- | One common table expression.
data Cte = Cte
    { cteName :: !(Located NamePart)
    , cteColumns :: ![Located NamePart]
    , cteMaterialized :: !(Maybe MaterializedHint)
    , cteBody :: !(Located CteBody)
    }
    deriving (Eq, Ord, Show, Data)

data CteBody
    = CteQuery !(Located Query)
    | CteStatement !(Located Statement)
    deriving (Eq, Ord, Show, Data)

data MaterializedHint
    = Materialized
    | NotMaterialized
    deriving (Eq, Ord, Show, Data)

-- | Query body form: SELECT, set operation, VALUES, TABLE, FROM-first, or parens.
data QueryBody
    = QueryBodySelect !SelectCore
    | QueryBodySet
        !SetOperator
        !SetQuantifier
        !(Located QueryBody)
        !(Located QueryBody)
    | QueryBodyValues ![[Located Expr]]
    | QueryBodyTable !(Located Name)
    | QueryBodyFrom !SelectCore
    | QueryBodyParen !(Located Query)
    deriving (Eq, Ord, Show, Data)

data SetOperator
    = SetUnion
    | SetIntersect
    | SetExcept
    deriving (Eq, Ord, Show, Data)

data SetQuantifier
    = SetAll
    | SetDistinct
    | SetDefault
    deriving (Eq, Ord, Show, Data)

-- | SELECT target list and clauses before set operations and query tail.
data SelectCore = SelectCore
    { selectQuantifier :: !SelectQuantifier
    , selectItems :: ![Located SelectItem]
    , selectInto :: !(Maybe (Located SelectInto))
    , selectFrom :: !(Maybe FromClause)
    , selectWhere :: !(Maybe (Located Expr))
    , selectPrewhere :: !(Maybe (Located (Located Expr)))
    , selectGroupBy :: ![Located GroupByItem]
    , selectHaving :: !(Maybe (Located Expr))
    , selectWindows :: ![Located WindowDef]
    , selectQualify :: !(Maybe (Located (Located Expr)))
    }
    deriving (Eq, Ord, Show, Data)

data SelectInto
    = SelectInto !(Located Name) !SelectIntoScope !Bool
    | SelectIntoUserVars ![Located ByteString]
    deriving (Eq, Ord, Show, Data)

data SelectIntoScope
    = SelectIntoTemporary
    | SelectIntoUnlogged
    | SelectIntoPersistent
    deriving (Eq, Ord, Show, Data)

data SelectQuantifier
    = SelectAll
    | SelectDistinct
    | SelectDistinctOn ![Located Expr]
    | SelectDefault
    deriving (Eq, Ord, Show, Data)

-- | One projection item, its optional alias, and whether @AS@ was written.
data SelectItem = SelectItem
    { selectItemExpr :: !(Located Expr)
    , selectItemAlias :: !(Maybe (Located Alias))
    , selectItemAsWritten :: !Bool
    }
    deriving (Eq, Ord, Show, Data)

{- | One element of a GROUP BY list.  Parentheses and the PostgreSQL-style
grouping constructs are kept structural instead of being folded into 'Expr':
an empty grouping set @()@ is 'GroupByParens' around an empty list, and
constructs may nest as allowed by PostgreSQL, DuckDB, and ClickHouse.
-}
data GroupByItem
    = GroupByExpr !(Located Expr)
    | GroupByParens ![Located GroupByItem]
    | GroupByGroupingSets ![Located GroupByItem]
    | GroupByRollup ![Located GroupByItem]
    | GroupByCube ![Located GroupByItem]
    deriving (Eq, Ord, Show, Data)

newtype FromClause = FromClause
    { fromItems :: [Located TableRef]
    }
    deriving (Eq, Ord, Show, Data)

-- | One FROM item: ONLY/descendant markers, source, alias, sample, FINAL, and join chain.
data TableRef = TableRef
    { tableRefOnly :: !Bool
    , tableRefSource :: !(Located TableSource)
    , tableRefIncludeDescendants :: !Bool
    , tableRefAlias :: !(Maybe (Located Alias))
    , tableRefFinal :: !Bool
    , tableRefSample :: !(Maybe (Located TableSample))
    , tableRefJoins :: ![Located JoinClause]
    }
    deriving (Eq, Ord, Show, Data)

{- | A TABLESAMPLE clause.  'tableSamplePercent' records a trailing @%@ on
the amount so @50%@ and @50@ remain distinguishable; the amount span covers
only the expression itself.
-}
data TableSample = TableSample
    { tableSampleMethod :: !(Located ByteString)
    , tableSampleAmount :: !(Located Expr)
    , tableSamplePercent :: !Bool
    , tableSampleRepeatable :: !(Maybe (Located Expr))
    , tableSampleOffset :: !(Maybe (Located Expr))
    }
    deriving (Eq, Ord, Show, Data)

-- | Table name, subquery, VALUES, function call, parenthesized FROM, or LATERAL.
data TableSource
    = TableSourceName !(Located Name)
    | TableSourceSubquery !(Located Query)
    | TableSourceValues ![[Located Expr]]
    | TableSourceFunction !(Located FunctionCall)
    | TableSourceParens !FromClause
    | TableSourceLateral !(Located TableSource)
    deriving (Eq, Ord, Show, Data)

-- | One JOIN clause with modifiers, kind, target, and constraint.
data JoinClause = JoinClause
    { joinNatural :: !Bool
    , joinGlobal :: !Bool
    , joinStrictness :: !(Maybe (Located ByteString))
    , joinKind :: !JoinKind
    , joinTarget :: !JoinTarget
    , joinConstraint :: !(Maybe JoinConstraint)
    }
    deriving (Eq, Ord, Show, Data)

data JoinKind
    = JoinInner
    | JoinLeft
    | JoinRight
    | JoinFull
    | JoinCross
    | JoinAsof !Bool
    | JoinArray !Bool
    deriving (Eq, Ord, Show, Data)

data JoinTarget
    = JoinTargetTable !(Located TableRef)
    | JoinTargetArray ![Located ArrayJoinItem]
    deriving (Eq, Ord, Show, Data)

data ArrayJoinItem = ArrayJoinItem
    { arrayJoinExpr :: !(Located Expr)
    , arrayJoinAlias :: !(Maybe (Located NamePart))
    }
    deriving (Eq, Ord, Show, Data)

data JoinConstraint
    = JoinOn !(Located Expr)
    | JoinUsing ![Located NamePart]
    deriving (Eq, Ord, Show, Data)

-- | One ORDER BY item.
data OrderItem = OrderItem
    { orderExpr :: !(Located Expr)
    , orderDirection :: !(Maybe SortDirection)
    , orderNulls :: !(Maybe NullsOrder)
    }
    deriving (Eq, Ord, Show, Data)

data SortDirection
    = SortAscending
    | SortDescending
    deriving (Eq, Ord, Show, Data)

data NullsOrder
    = NullsFirst
    | NullsLast
    deriving (Eq, Ord, Show, Data)

-- | LIMIT with @offset, count@ or @count OFFSET@ forms.
data LimitClause = LimitClause
    { limitFirst :: !(Located Expr)
    , limitSecond :: !(Maybe (Located Expr))
    , limitCommaForm :: !Bool
    }
    deriving (Eq, Ord, Show, Data)

-- | FETCH FIRST/NEXT clause.
data FetchClause = FetchClause
    { fetchCount :: !(Maybe (Located Expr))
    , fetchWithTies :: !Bool
    }
    deriving (Eq, Ord, Show, Data)

-- | FOR UPDATE/SHARE locking clause.
data ForClause = ForClause
    { forStrength :: !ForStrength
    , forTables :: ![Located Name]
    , forSkipLocked :: !Bool
    , forNoWait :: !Bool
    }
    deriving (Eq, Ord, Show, Data)

data ForStrength
    = ForUpdate
    | ForShare
    | ForNoKeyUpdate
    | ForKeyShare
    deriving (Eq, Ord, Show, Data)

data WindowDef = WindowDef
    { windowDefName :: !(Located NamePart)
    , windowDefSpec :: !WindowSpec
    }
    deriving (Eq, Ord, Show, Data)

data SettingItem = SettingItem
    { settingName :: !(Located Name)
    , settingValue :: !(Located Expr)
    }
    deriving (Eq, Ord, Show, Data)

-- ---------------------------------------------------------------------------
-- Statements
-- ---------------------------------------------------------------------------

-- | Any parsed top-level statement.
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
    deriving (Eq, Ord, Show, Data)

data NotifyStmt = NotifyStmt
    { notifyChannel :: !(Located Name)
    , notifyPayload :: !(Maybe (Located Expr))
    }
    deriving (Eq, Ord, Show, Data)

-- | INSERT/REPLACE with optional conflict, row alias, and RETURNING clauses.
data InsertStmt = InsertStmt
    { insertWith :: !(Maybe (Located WithClause))
    , insertOrAction :: !(Maybe (Located InsertOrAction))
    , insertTable :: !(Located Name)
    , insertPartitions :: ![Located NamePart]
    , insertAlias :: !(Maybe (Located Alias))
    , insertColumns :: ![Located NamePart]
    , insertSource :: !InsertSource
    , insertValuesAlias :: !(Maybe (Located Alias))
    , insertOnConflict :: !(Maybe (Located OnConflictClause))
    , insertOnDuplicate :: !(Maybe [Located Assignment])
    , insertReturning :: ![Located SelectItem]
    , insertReplace :: !Bool
    }
    deriving (Eq, Ord, Show, Data)

-- | SQLite @INSERT OR <action>@ conflict resolution modifier.
data InsertOrAction
    = InsertOrRollback
    | InsertOrAbort
    | InsertOrFail
    | InsertOrIgnore
    | InsertOrReplace
    deriving (Eq, Ord, Show, Data)

data InsertSource
    = InsertValues ![[Located Expr]]
    | InsertQuery !(Located Query)
    | InsertDefaultValues
    deriving (Eq, Ord, Show, Data)

data OnConflictClause = OnConflictClause
    { onConflictConstraint :: !(Maybe (Located NamePart))
    , onConflictKeys :: ![Located IndexKeyPart]
    , onConflictWhere :: !(Maybe (Located Expr))
    , onConflictAction :: !OnConflictAction
    }
    deriving (Eq, Ord, Show, Data)

data OnConflictAction
    = OnConflictDoNothing
    | OnConflictDoUpdate
        ![Located Assignment]
        !(Maybe (Located Expr))
    deriving (Eq, Ord, Show, Data)

data Assignment = Assignment
    { assignTarget :: !(Located Name)
    , assignValue :: !(Located Expr)
    }
    deriving (Eq, Ord, Show, Data)

-- | UPDATE target, assignments, FROM, WHERE, ORDER BY, LIMIT, and RETURNING.
data UpdateStmt = UpdateStmt
    { updateWith :: !(Maybe (Located WithClause))
    , updateOnly :: !Bool
    , updateTargets :: !FromClause
    , updateAssignments :: ![Located Assignment]
    , updateFrom :: !(Maybe FromClause)
    , updateWhere :: !(Maybe (Located Expr))
    , updateOrderBy :: ![Located OrderItem]
    , updateLimit :: !(Maybe (Located LimitClause))
    , updateReturning :: ![Located SelectItem]
    }
    deriving (Eq, Ord, Show, Data)

-- | DELETE target, USING, WHERE, ORDER BY, LIMIT, and RETURNING, including multi-table forms.
data DeleteStmt = DeleteStmt
    { deleteWith :: !(Maybe (Located WithClause))
    , deleteOnly :: !Bool
    , deleteIncludeDescendants :: !Bool
    , deleteTargets :: ![Located DeleteTarget]
    , deleteTable :: !(Located Name)
    , deleteAlias :: !(Maybe (Located Alias))
    , deleteFromTables :: !(Maybe FromClause)
    , deleteUsing :: !(Maybe FromClause)
    , deleteWhere :: !(Maybe (Located Expr))
    , deleteOrderBy :: ![Located OrderItem]
    , deleteLimit :: !(Maybe (Located LimitClause))
    , deleteReturning :: ![Located SelectItem]
    }
    deriving (Eq, Ord, Show, Data)

-- | One target in a MySQL/MariaDB multi-table DELETE.
data DeleteTarget = DeleteTarget
    { deleteTargetName :: !(Located Name)
    , deleteTargetHasStar :: !Bool
    }
    deriving (Eq, Ord, Show, Data)

-- | MERGE source, ON condition, WHEN clauses, and RETURNING.
data MergeStmt = MergeStmt
    { mergeWith :: !(Maybe (Located WithClause))
    , mergeTarget :: !(Located Name)
    , mergeTargetAlias :: !(Maybe (Located Alias))
    , mergeSource :: !(Located TableRef)
    , mergeOn :: !(Located Expr)
    , mergeClauses :: ![Located MergeClause]
    , mergeReturning :: ![Located SelectItem]
    }
    deriving (Eq, Ord, Show, Data)

data MergeClause = MergeClause
    { mergeClauseMatched :: !Bool
    , mergeClauseBySource :: !Bool
    , mergeClauseCondition :: !(Maybe (Located Expr))
    , mergeClauseAction :: !MergeAction
    }
    deriving (Eq, Ord, Show, Data)

data MergeAction
    = MergeUpdate ![Located Assignment]
    | MergeInsert ![Located NamePart] ![Located Expr]
    | MergeInsertDefaultValues
    | MergeDelete
    | MergeDoNothing
    deriving (Eq, Ord, Show, Data)

-- ---------------------------------------------------------------------------
-- DDL
-- ---------------------------------------------------------------------------

-- | CREATE flags plus the concrete object definition.
data CreateStmt = CreateStmt
    { createOrReplace :: !Bool
    , createTemp :: !Bool
    , createUnique :: !Bool
    , createFulltext :: !Bool
    , createAlgorithm :: !(Maybe (Located ByteString))
    , createObject :: !(Located CreateObject)
    }
    deriving (Eq, Ord, Show, Data)

-- | CREATE object variant.
data CreateObject
    = CreateTableObject !CreateTable
    | CreateIndexObject !CreateIndex
    | CreateViewObject !CreateView
    | CreateSequenceObject !CreateSequence
    | CreateSchemaObject !CreateSchema
    | CreateDatabaseObject !CreateDatabase
    | CreateStatisticsObject !CreateStatistics
    | CreateMacroObject !CreateMacro
    | CreatePolicyObject !CreatePolicy
    | CreateRuleObject !CreateRule
    | CreateTriggerObject !CreateTrigger
    | CreateFunctionObject !CreateRoutine
    | CreateProcedureObject !CreateRoutine
    | CreateAggregateObject !CreateAggregate
    | CreateVirtualTableObject !CreateVirtualTable
    | CreateExtensionObject !CreateExtension
    deriving (Eq, Ord, Show, Data)

-- | PostgreSQL @CREATE EXTENSION [IF NOT EXISTS] name ...@.
data CreateExtension = CreateExtension
    { createExtensionName :: !(Located NamePart)
    , createExtensionIfNotExists :: !Bool
    }
    deriving (Eq, Ord, Show, Data)

-- | CREATE TABLE elements, options, engine, partition, and CTAS body.
data CreateTable = CreateTable
    { createTableName :: !(Located Name)
    , createTableIfNotExists :: !Bool
    , createTableElements :: ![Located TableElement]
    , createTableOptions :: ![Located TableOption]
    , createTableEngine :: !(Maybe OpaqueBody)
    , createTableSettings :: ![Located SettingItem]
    , createTablePartitionOf :: !(Maybe (Located TablePartition))
    , createTablePartitionBy :: !(Maybe (Located PartitionBy))
    , createTableAs :: !(Maybe (Located Query))
    , createTableWithData :: !(Maybe Bool)
    }
    deriving (Eq, Ord, Show, Data)

data PartitionBy = PartitionBy
    { partitionByStrategy :: !(Located ByteString)
    , partitionByExprs :: ![Located Expr]
    }
    deriving (Eq, Ord, Show, Data)

data PartitionBound
    = PartitionBoundDefault
    | PartitionBoundIn ![Located Expr]
    | PartitionBoundFromTo ![Located Expr] ![Located Expr]
    | PartitionBoundWith ![(Located NamePart, Located Expr)]
    deriving (Eq, Ord, Show, Data)

data TablePartition = TablePartition
    { tablePartitionParent :: !(Located Name)
    , tablePartitionBound :: !PartitionBound
    }
    deriving (Eq, Ord, Show, Data)

data TableElement
    = TableElementColumn !(Located ColumnDef)
    | TableElementConstraint !(Located TableConstraint)
    | TableElementProjection !(Located ProjectionDef)
    | TableElementSkippingIndex !(Located SkippingIndex)
    deriving (Eq, Ord, Show, Data)

data SkippingIndex = SkippingIndex
    { skippingIndexName :: !(Located NamePart)
    , skippingIndexExpr :: !(Located Expr)
    , skippingIndexType :: !(Located TypeName)
    , skippingIndexGranularity :: !(Maybe (Located Expr))
    }
    deriving (Eq, Ord, Show, Data)

data ProjectionDef = ProjectionDef
    { projectionName :: !(Located NamePart)
    , projectionQuery :: !(Located Query)
    }
    deriving (Eq, Ord, Show, Data)

-- | One column definition.
data ColumnDef = ColumnDef
    { columnName :: !(Located NamePart)
    , columnType :: !(Maybe (Located TypeName))
    , columnConstraints :: ![Located ColumnConstraint]
    , columnSettings :: ![Located SettingItem]
    }
    deriving (Eq, Ord, Show, Data)

-- | Column constraint, optionally named.
data ColumnConstraint
    = ColumnConstraintNamed !(Located NamePart) !ColumnConstraint
    | ColumnConstraintNotNull
    | ColumnConstraintNull
    | ColumnConstraintDefault !(Located Expr)
    | ColumnConstraintPrimaryKey !(Maybe SortDirection)
    | ColumnConstraintUnique
    | ColumnConstraintCheck !(Located Expr)
    | ColumnConstraintReferences !(Located ForeignKeyRef)
    | ColumnConstraintCollate !(Located ByteString)
    | ColumnConstraintKey
    | ColumnConstraintBinary
    | ColumnConstraintAutoIncrement
    | ColumnConstraintGenerated !Bool !(Located Expr)
    | ColumnConstraintIdentity !Bool ![Located TableOption]
    | ColumnConstraintOnConflict !ColumnConstraint !(Located ColumnConflictAction)
    | ColumnConstraintOnUpdate !(Located Expr)
    | ColumnConstraintComment !(Located Expr)
    | ColumnConstraintCharacterSet !(Located ByteString)
    | ColumnConstraintEphemeral !(Maybe (Located Expr))
    deriving (Eq, Ord, Show, Data)

data ColumnConflictAction
    = ColumnConflictRollback
    | ColumnConflictAbort
    | ColumnConflictFail
    | ColumnConflictIgnore
    | ColumnConflictReplace
    deriving (Eq, Ord, Show, Data)

-- | Table-level constraint or index definition.
data TableConstraint
    = TableConstraintPrimaryKey !ConstraintIndexOptions ![Located IndexKeyPart]
    | TableConstraintUnique !ConstraintIndexOptions ![Located IndexKeyPart]
    | TableConstraintForeignKey
        !ConstraintIndexOptions
        ![Located NamePart]
        !(Located ForeignKeyRef)
    | TableConstraintCheck !(Maybe (Located NamePart)) !(Located Expr)
    | TableConstraintIndex !ConstraintIndexOptions ![Located IndexKeyPart]
    | TableConstraintUsingIndex !(Maybe (Located NamePart)) !Bool !(Located NamePart)
    | TableConstraintLike !(Located Name) ![Located TableLikeOption]
    deriving (Eq, Ord, Show, Data)

data ConstraintIndexOptions = ConstraintIndexOptions
    { constraintName :: !(Maybe (Located NamePart))
    , constraintIndexName :: !(Maybe (Located NamePart))
    , constraintIndexMethod :: !(Maybe (Located NamePart))
    }
    deriving (Eq, Ord, Show, Data)

data TableLikeOption = TableLikeOption
    { tableLikeInclude :: !Bool
    , tableLikeOptionName :: !(Located ByteString)
    }
    deriving (Eq, Ord, Show, Data)

data ForeignKeyRef = ForeignKeyRef
    { foreignKeyTable :: !(Located Name)
    , foreignKeyColumns :: ![Located NamePart]
    , foreignKeyOnDelete :: !(Maybe (Located ForeignKeyAction))
    , foreignKeyOnUpdate :: !(Maybe (Located ForeignKeyAction))
    , foreignKeyMatch :: !(Maybe (Located NamePart))
    , foreignKeyDeferrable :: !(Maybe Bool)
    , foreignKeyInitially :: !(Maybe ForeignKeyTiming)
    }
    deriving (Eq, Ord, Show, Data)

data ForeignKeyTiming = ForeignKeyInitiallyDeferred | ForeignKeyInitiallyImmediate
    deriving (Eq, Ord, Show, Data)

data ForeignKeyAction = ForeignKeyAction
    { foreignKeyActionKind :: !ForeignKeyActionKind
    }
    deriving (Eq, Ord, Show, Data)

data ForeignKeyActionKind
    = ForeignKeyCascade
    | ForeignKeyRestrict
    | ForeignKeyNoAction
    | ForeignKeySetNull
    | ForeignKeySetDefault
    deriving (Eq, Ord, Show, Data)

-- | Type name with arguments, suffix words, and array dimensions.
data TypeName = TypeName
    { typeNameParts :: ![Located NamePart]
    , typeNameArgs :: ![Located TypeArg]
    , typeNameSuffix :: ![Located NamePart]
    , typeNameArrayDims :: !Int
    }
    deriving (Eq, Ord, Show, Data)

data TypeArg
    = TypeArgType !(Located TypeName)
    | TypeArgExpr !(Located Expr)
    deriving (Eq, Ord, Show, Data)

data IndexKeyPart = IndexKeyPart
    { indexKeyExpr :: !(Located Expr)
    , indexKeyPrefixLength :: !(Maybe (Located Expr))
    , indexKeyDirection :: !(Maybe SortDirection)
    , indexKeyNulls :: !(Maybe NullsOrder)
    }
    deriving (Eq, Ord, Show, Data)

-- | CREATE INDEX definition.
data CreateIndex = CreateIndex
    { createIndexName :: !(Maybe (Located Name))
    , createIndexTable :: !(Located Name)
    , createIndexMethod :: !(Maybe (Located Name))
    , createIndexKeys :: ![Located IndexKeyPart]
    , createIndexIncludes :: ![Located NamePart]
    , createIndexWhere :: !(Maybe (Located Expr))
    }
    deriving (Eq, Ord, Show, Data)

-- | CREATE [MATERIALIZED] VIEW definition.
data CreateView = CreateView
    { createViewName :: !(Located Name)
    , createViewIfNotExists :: !Bool
    , createViewColumns :: ![Located NamePart]
    , createViewMaterialized :: !Bool
    , createViewTo :: !(Maybe (Located Name))
    , createViewEngine :: !(Maybe OpaqueBody)
    , createViewSettings :: ![Located SettingItem]
    , createViewPopulate :: !Bool
    , createViewBody :: !ViewBody
    , createViewWithData :: !(Maybe Bool)
    }
    deriving (Eq, Ord, Show, Data)

data ViewBody
    = ViewBodyQuery !(Located Query)
    | ViewBodyOpaque !OpaqueBody
    deriving (Eq, Ord, Show, Data)

-- | Byte range of a body that is intentionally not parsed.
data OpaqueBody = OpaqueBody
    { opaqueBodySpan :: !ByteSpan
    , opaqueBodyKind :: !OpaqueKind
    }
    deriving (Eq, Ord, Show, Data)

data OpaqueKind
    = OpaqueFunctionBody
    | OpaqueProcedureBody
    | OpaqueTriggerBody
    | OpaqueEngineExpression
    | OpaqueViewBody
    deriving (Eq, Ord, Show, Data)

data CreateSequence = CreateSequence
    { createSequenceName :: !(Located Name)
    , createSequenceIfNotExists :: !Bool
    , createSequenceOptions :: ![Located TableOption]
    }
    deriving (Eq, Ord, Show, Data)

data CreateSchema = CreateSchema
    { createSchemaName :: !(Maybe (Located Name))
    , createSchemaIfNotExists :: !Bool
    , createSchemaAuthorization :: !(Maybe (Located Name))
    }
    deriving (Eq, Ord, Show, Data)

data CreateDatabase = CreateDatabase
    { createDatabaseName :: !(Located Name)
    , createDatabaseIfNotExists :: !Bool
    , createDatabaseOptions :: ![Located TableOption]
    }
    deriving (Eq, Ord, Show, Data)

data CreateStatistics = CreateStatistics
    { createStatisticsName :: !(Located Name)
    , createStatisticsIfNotExists :: !Bool
    , createStatisticsKinds :: ![Located NamePart]
    , createStatisticsColumns :: ![Located Expr]
    , createStatisticsFrom :: !(Maybe (Located Name))
    }
    deriving (Eq, Ord, Show, Data)

data CreateMacro = CreateMacro
    { createMacroName :: !(Located Name)
    , createMacroParams :: ![Located MacroParam]
    , createMacroTable :: !Bool
    , createMacroBody :: !(Either (Located Query) (Located Expr))
    }
    deriving (Eq, Ord, Show, Data)

data MacroParam = MacroParam
    { macroParamName :: !(Located NamePart)
    , macroParamDefault :: !(Maybe (Located Expr))
    }
    deriving (Eq, Ord, Show, Data)

data CreatePolicy = CreatePolicy
    { createPolicyName :: !(Located Name)
    , createPolicyTable :: !(Located Name)
    , createPolicyPermissive :: !(Maybe Bool)
    , createPolicyCommand :: !(Maybe (Located ByteString))
    , createPolicyRoles :: ![Located Name]
    , createPolicyUsing :: !(Maybe (Located Expr))
    , createPolicyWithCheck :: !(Maybe (Located Expr))
    }
    deriving (Eq, Ord, Show, Data)

data CreateRule = CreateRule
    { createRuleName :: !(Located Name)
    , createRuleEvent :: !(Located ByteString)
    , createRuleTable :: !(Located Name)
    , createRuleWhere :: !(Maybe (Located Expr))
    , createRuleInstead :: !Bool
    , createRuleAction :: !(Maybe (Located Statement))
    }
    deriving (Eq, Ord, Show, Data)

data CreateTrigger = CreateTrigger
    { createTriggerName :: !(Located Name)
    , createTriggerTiming :: !(Maybe (Located ByteString))
    , createTriggerEvents :: ![Located ByteString]
    , createTriggerTable :: !(Located Name)
    , createTriggerGranularity :: !(Maybe (Located TriggerGranularity))
    , createTriggerWhen :: !(Maybe (Located Expr))
    , createTriggerExecute :: !(Maybe (Located Name))
    , createTriggerExecuteArgs :: ![Located Expr]
    , createTriggerBody :: !OpaqueBody
    }
    deriving (Eq, Ord, Show, Data)

data TriggerGranularity = TriggerForEachRow | TriggerForEachStatement
    deriving (Eq, Ord, Show, Data)

data CreateRoutine = CreateRoutine
    { createRoutineName :: !(Located Name)
    , createRoutineParams :: ![Located RoutineParam]
    , createRoutineReturns :: !(Maybe (Located TypeName))
    , createRoutineLanguage :: !(Maybe (Located NamePart))
    , createRoutineBody :: !OpaqueBody
    }
    deriving (Eq, Ord, Show, Data)

data RoutineParam = RoutineParam
    { routineParamMode :: !(Maybe ByteString)
    , routineParamName :: !(Maybe (Located NamePart))
    , routineParamType :: !(Located TypeName)
    , routineParamDefault :: !(Maybe (Located Expr))
    }
    deriving (Eq, Ord, Show, Data)

-- | PostgreSQL @CREATE [OR REPLACE] AGGREGATE@ definition.
data CreateAggregate = CreateAggregate
    { createAggregateName :: !(Located Name)
    , createAggregateParams :: ![Located RoutineParam]
    , createAggregateStar :: !Bool
    , createAggregateOptions :: ![Located TableOption]
    }
    deriving (Eq, Ord, Show, Data)

data CreateVirtualTable = CreateVirtualTable
    { createVirtualTableName :: !(Located Name)
    , createVirtualTableIfNotExists :: !Bool
    , createVirtualTableModule :: !(Located Name)
    , createVirtualTableArgs :: ![Located VirtualTableArg]
    }
    deriving (Eq, Ord, Show, Data)

data VirtualTableArg
    = VirtualTableColumn !(Located ColumnDef)
    | VirtualTableExpression !(Located Expr)
    deriving (Eq, Ord, Show, Data)

data TableOption = TableOption
    { tableOptionName :: ![Located NamePart]
    , tableOptionValue :: !(Maybe (Located Expr))
    }
    deriving (Eq, Ord, Show, Data)

-- | DROP object kind, names, and modifiers.
data DropStmt = DropStmt
    { dropObject :: !(Located DropObject)
    , dropIfExists :: !Bool
    , dropNames :: ![Located Name]
    , dropOnTable :: !(Maybe (Located Name))
    , dropBehavior :: !(Maybe DropBehavior)
    , dropSync :: !Bool
    }
    deriving (Eq, Ord, Show, Data)

data DropObject
    = DropTable
    | DropIndex
    | DropView
    | DropMaterializedView
    | DropSequence
    | DropSchema
    | DropDatabase
    | DropTrigger
    | DropFunction
    | DropProcedure
    | DropAggregate
    | DropStatistics
    | DropConversion
    | DropPolicy
    | DropRule
    | DropMacro
    deriving (Eq, Ord, Show, Data)

data DropBehavior
    = DropCascade
    | DropRestrict
    deriving (Eq, Ord, Show, Data)

-- | ALTER object kind, target, and action list.
data AlterStmt = AlterStmt
    { alterObject :: !AlterObject
    , alterIfExists :: !Bool
    , alterOnly :: !Bool
    , alterTable :: !(Located Name)
    , alterActions :: ![Located AlterAction]
    }
    deriving (Eq, Ord, Show, Data)

data AlterObject
    = AlterObjectTable
    | AlterObjectView
    deriving (Eq, Ord, Show, Data)

-- | One ALTER TABLE or ALTER VIEW action.
data AlterAction
    = AlterAddColumn !Bool ![Located ColumnDef] !(Maybe (Located ColumnPosition))
    | AlterAddConstraint !(Located TableConstraint)
    | AlterAddIndex !(Maybe (Located NamePart)) ![Located IndexKeyPart]
    | AlterDropColumn !Bool !(Located NamePart) !(Maybe DropBehavior)
    | AlterRenameColumn !(Located NamePart) !(Located NamePart)
    | AlterRenameTable !(Located Name)
    | AlterModifyColumn ![Located ColumnDef] !(Maybe (Located ColumnPosition))
    | AlterChangeColumn !(Located NamePart) !(Located ColumnDef) !(Maybe (Located ColumnPosition))
    | AlterAlterColumn !(Located NamePart) !AlterColumnAction
    | AlterDropConstraint !Bool !(Located NamePart) !(Maybe DropBehavior)
    | AlterDropIndex !Bool !(Located NamePart) !(Maybe DropBehavior)
    | AlterDropPrimaryKey
    | AlterTableOption !(Located TableOption)
    | AlterMaterializeTtl
    | AlterModifySetting ![Located SettingItem]
    | AlterAttachPartition !(Located Name) !PartitionBound
    | AlterDetachPartition !(Located Name) !Bool !Bool
    | AlterInherit !(Located Name)
    | AlterNoInherit !(Located Name)
    | AlterSetLogged !Bool
    | AlterUpdate ![Located Assignment] ![Located Expr] !(Located Expr)
    | AlterDelete ![Located Expr] !(Located Expr)
    | AlterOwnerTo !(Located Name)
    | AlterViewSetOptions ![Located SettingItem]
    | AlterViewResetOptions ![Located NamePart]
    | AlterViewAsQuery !(Located Query)
    | AlterRowLevelSecurity !RlsAction
    deriving (Eq, Ord, Show, Data)

-- | MySQL/MariaDB column placement in ALTER TABLE ADD/MODIFY/CHANGE COLUMN.
data ColumnPosition
    = ColumnPositionFirst
    | ColumnPositionAfter !(Located NamePart)
    deriving (Eq, Ord, Show, Data)

data RlsAction
    = RlsEnable
    | RlsDisable
    | RlsForce
    | RlsNoForce
    deriving (Eq, Ord, Show, Data)

data AlterColumnAction
    = AlterSetDefault !(Located Expr)
    | AlterDropDefault
    | AlterSetNotNull
    | AlterDropNotNull
    | AlterType !(Located TypeName) !(Maybe (Located Expr))
    | AlterAddIdentity !Bool ![Located TableOption]
    | AlterColumnOptions ![Located GenericOption]
    deriving (Eq, Ord, Show, Data)

-- | One entry in PostgreSQL @ALTER COLUMN ... OPTIONS (...)@.
data GenericOption = GenericOption
    { genericOptionAction :: !(Maybe (Located ByteString))
    , genericOptionName :: !(Located NamePart)
    , genericOptionValue :: !(Maybe (Located Expr))
    }
    deriving (Eq, Ord, Show, Data)

-- ---------------------------------------------------------------------------
-- Session and utility statements
-- ---------------------------------------------------------------------------

-- | PRAGMA with an optional bare or expression value.
data PragmaStmt = PragmaStmt
    { pragmaName :: !(Located Name)
    , pragmaValue :: !(Maybe PragmaValue)
    }
    deriving (Eq, Ord, Show, Data)

data PragmaValue
    = PragmaBare !(Located ByteString)
    | PragmaValueExpr !(Located Expr)
    deriving (Eq, Ord, Show, Data)

-- | SET with scope, targets, and transaction modes.
data SetStmt = SetStmt
    { setScope :: !SetScope
    , setItems :: ![Located SetItem]
    , setTransactionModes :: ![Located ByteString]
    }
    deriving (Eq, Ord, Show, Data)

data SetScope
    = SetScopeSession
    | SetScopeLocal
    | SetScopeGlobal
    | SetScopeNone
    deriving (Eq, Ord, Show, Data)

-- | One SET target and its value list.
data SetItem = SetItem
    { setItemTarget :: !SetTarget
    , setItemValue :: ![Located Expr]
    , setItemToSyntax :: !Bool
    }
    deriving (Eq, Ord, Show, Data)

data SetTarget
    = SetTargetName !(Located Name)
    | SetTargetUserVar !(Located ByteString)
    | SetTargetSysVar !(Located ByteString)
    | SetTargetSpecial !(Located ByteString)
    deriving (Eq, Ord, Show, Data)

-- | RESET configuration parameter, ROLE, SESSION AUTHORIZATION, or ALL.
data ResetStmt
    = ResetAll
    | ResetRole
    | ResetSessionAuthorization
    | ResetTarget !SetTarget
    deriving (Eq, Ord, Show, Data)

-- | MySQL/MariaDB @LOCK TABLES@ statement.
newtype LockTablesStmt = LockTablesStmt
    { lockTablesItems :: [Located LockTableItem]
    }
    deriving (Eq, Ord, Show, Data)

-- | One table entry in @LOCK TABLES@.
data LockTableItem = LockTableItem
    { lockTableName :: !(Located Name)
    , lockTableAlias :: !(Maybe (Located Alias))
    , lockTableType :: !(Located ByteString)
    }
    deriving (Eq, Ord, Show, Data)

-- | @GRANT@ or @REVOKE@ privilege statement.
data GrantStmt = GrantStmt
    { grantIsRevoke :: !Bool
    , grantPrivileges :: ![Located ByteString]
    , grantObject :: !(Maybe (Located Name))
    , grantGrantees :: ![Located Name]
    }
    deriving (Eq, Ord, Show, Data)

-- | Transaction control command with optional savepoint, modes, and chain.
data TransactionStmt = TransactionStmt
    { transactionCommand :: !TransactionCommand
    , transactionName :: !(Maybe (Located NamePart))
    , transactionModes :: ![TransactionMode]
    , transactionChain :: !(Maybe Bool)
    }
    deriving (Eq, Ord, Show, Data)

data TransactionCommand
    = TransactionBegin
    | TransactionStart
    | TransactionCommit
    | TransactionRollback
    | TransactionRollbackTo
    | TransactionSavepoint
    | TransactionReleaseSavepoint
    | TransactionEnd
    deriving (Eq, Ord, Show, Data)

data TransactionMode
    = TransactionReadOnly !Bool
    | TransactionBeginLock !ByteString
    | TransactionUnmodeledMode !ByteString
    deriving (Eq, Ord, Show, Data)

-- | EXPLAIN options and the explained statement.
data ExplainStmt = ExplainStmt
    { explainOptions :: ![Located ExplainOption]
    , explainStatement :: !(Located Statement)
    }
    deriving (Eq, Ord, Show, Data)

data ExplainOption = ExplainOption
    { explainOptionName :: !(Located ByteString)
    , explainOptionValue :: !(Maybe (Located ByteString))
    }
    deriving (Eq, Ord, Show, Data)

-- | COPY source, direction, target, and options.
data CopyStmt = CopyStmt
    { copySource :: !CopySource
    , copyDirection :: !CopyDirection
    , copyTarget :: !CopyTarget
    , copyOptions :: ![Located CopyOption]
    }
    deriving (Eq, Ord, Show, Data)

data CopySource
    = CopySourceTable !(Located Name) ![Located NamePart]
    | CopySourceQuery !(Located Query)
    deriving (Eq, Ord, Show, Data)

data CopyDirection
    = CopyFrom
    | CopyTo
    deriving (Eq, Ord, Show, Data)

data CopyTarget
    = CopyFile !(Located Literal)
    | CopyStdin
    | CopyStdout
    | CopyProgram !(Located Literal)
    deriving (Eq, Ord, Show, Data)

data CopyOption = CopyOption
    { copyOptionName :: !(Located ByteString)
    , copyOptionValue :: !(Maybe (Located ByteString))
    }
    deriving (Eq, Ord, Show, Data)

-- | ANALYZE flags and table list.
data AnalyzeStmt = AnalyzeStmt
    { analyzeVerbose :: !Bool
    , analyzeTables :: ![Located AnalyzeTable]
    }
    deriving (Eq, Ord, Show, Data)

data AnalyzeTable = AnalyzeTable
    { analyzeTableName :: !(Located Name)
    , analyzeTableColumns :: ![Located NamePart]
    }
    deriving (Eq, Ord, Show, Data)

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
    deriving (Eq, Ord, Show, Data)

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
    deriving (Eq, Ord, Show, Data)

-- | SQLite/DuckDB database detachment or ClickHouse object detachment.
data DetachStmt
    = DetachDatabase !(Located Name) !Bool
    | DetachObject !(Located ByteString) !(Located Name) !Bool !Bool
    deriving (Eq, Ord, Show, Data)

-- | PREPARE name, parameter types, and source.
data PrepareStmt = PrepareStmt
    { prepareName :: !(Located Name)
    , prepareParams :: ![Located TypeName]
    , prepareSource :: !PrepareSource
    }
    deriving (Eq, Ord, Show, Data)

data PrepareSource
    = PrepareStatement !(Located Statement)
    | PrepareExpression !(Located Expr)
    deriving (Eq, Ord, Show, Data)

-- | EXECUTE prepared name and arguments.
data ExecuteStmt = ExecuteStmt
    { executeName :: !(Located Name)
    , executeArgs :: ![Located Expr]
    }
    deriving (Eq, Ord, Show, Data)

-- | DEALLOCATE ALL or a named prepared statement.
data DeallocateStmt = DeallocateStmt
    { deallocateAll :: !Bool
    , deallocateName :: !(Maybe (Located Name))
    }
    deriving (Eq, Ord, Show, Data)

-- | TRUNCATE targets and identity/behavior flags.
data TruncateStmt = TruncateStmt
    { truncateOnly :: !Bool
    , truncateTables :: ![Located Name]
    , truncateRestartIdentity :: !(Maybe Bool)
    , truncateBehavior :: !(Maybe DropBehavior)
    }
    deriving (Eq, Ord, Show, Data)

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
    deriving (Eq, Ord, Show, Data)

-- | ClickHouse/MySQL OPTIMIZE TABLE statement.
data OptimizeStmt = OptimizeStmt
    { optimizeTable :: !(Located Name)
    , optimizePartition :: !(Maybe (Located Expr))
    , optimizeFinal :: !Bool
    , optimizeDeduplicate :: !Bool
    }
    deriving (Eq, Ord, Show, Data)

-- | SHOW subject and optional target, LIKE, and WHERE.
data ShowStmt = ShowStmt
    { showSubject :: ![Located NamePart]
    , showTarget :: !(Maybe (Located Name))
    , showLike :: !(Maybe (Located Expr))
    , showWhere :: !(Maybe (Located Expr))
    }
    deriving (Eq, Ord, Show, Data)

-- | PostgreSQL @REFRESH MATERIALIZED VIEW [CONCURRENTLY] name [WITH [NO] DATA]@.
data RefreshMaterializedViewStmt = RefreshMaterializedViewStmt
    { refreshConcurrently :: !Bool
    , refreshName :: !(Located Name)
    , refreshWithData :: !Bool
    }
    deriving (Eq, Ord, Show, Data)
