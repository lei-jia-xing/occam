-- | Query, SELECT, and FROM syntax nodes.
module Occam.Frontend.AST.Query (
    Query (..),
    WithClause (..),
    Cte (..),
    CteBody (..),
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
    FromClause (..),
    TableRef (..),
    TableSample (..),
    TableSource (..),
    JoinClause (..),
    JoinKind (..),
    JoinTarget (..),
    ArrayJoinItem (..),
    JoinConstraint (..),
    LimitClause (..),
    FetchClause (..),
    ForClause (..),
    ForStrength (..),
    WindowDef (..),
    SettingItem (..),
) where

import Data.ByteString (ByteString)
import Occam.Frontend.AST.Expr
import {-# SOURCE #-} Occam.Frontend.AST.Statement (Statement)
import Occam.Frontend.Span (Located)

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
    deriving (Eq, Show)

-- | WITH clause and its CTE list.
data WithClause = WithClause
    { withRecursive :: !Bool
    , withCtes :: ![Located Cte]
    }
    deriving (Eq, Show)

-- | One common table expression.
data Cte = Cte
    { cteName :: !(Located NamePart)
    , cteColumns :: ![Located NamePart]
    , cteMaterialized :: !(Maybe MaterializedHint)
    , cteBody :: !(Located CteBody)
    }
    deriving (Eq, Show)

-- | CTE body that may be a query or a data-modifying statement.
data CteBody
    = CteQuery !(Located Query)
    | CteStatement !(Located Statement)
    deriving (Eq, Show)

-- | Optional request to materialize or avoid materializing a CTE.
data MaterializedHint
    = Materialized
    | NotMaterialized
    deriving (Eq, Ord, Show)

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
    deriving (Eq, Show)

-- | Set-combination operator joining two query bodies.
data SetOperator
    = SetUnion
    | SetIntersect
    | SetExcept
    deriving (Eq, Ord, Show)

-- | Set-operation duplicate policy, preserving whether it was omitted.
data SetQuantifier
    = SetAll
    | SetDistinct
    | SetDefault
    deriving (Eq, Ord, Show)

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
    deriving (Eq, Show)

-- | SELECT INTO destination: a relation with scope metadata or user variables.
data SelectInto
    = SelectInto !(Located Name) !SelectIntoScope !Bool
    | SelectIntoUserVars ![Located ByteString]
    deriving (Eq, Ord, Show)

-- | Persistence scope for a SELECT INTO relation target.
data SelectIntoScope
    = SelectIntoTemporary
    | SelectIntoUnlogged
    | SelectIntoPersistent
    deriving (Eq, Ord, Show)

-- | SELECT duplicate policy, including PostgreSQL DISTINCT ON expressions.
data SelectQuantifier
    = SelectAll
    | SelectDistinct
    | SelectDistinctOn ![Located Expr]
    | SelectDefault
    deriving (Eq, Show)

-- | One projection item, its optional alias, and whether @AS@ was written.
data SelectItem = SelectItem
    { selectItemExpr :: !(Located Expr)
    , selectItemAlias :: !(Maybe (Located Alias))
    , selectItemAsWritten :: !Bool
    }
    deriving (Eq, Show)

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
    deriving (Eq, Show)

-- | Comma-separated top-level FROM references.
newtype FromClause = FromClause
    { fromItems :: [Located TableRef]
    }
    deriving (Eq, Show)

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
    deriving (Eq, Show)

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
    deriving (Eq, Show)

-- | Table name, subquery, VALUES, function call, parenthesized FROM, or LATERAL.
data TableSource
    = TableSourceName !(Located Name)
    | TableSourceSubquery !(Located Query)
    | TableSourceValues ![[Located Expr]]
    | TableSourceFunction !(Located FunctionCall)
    | TableSourceParens !FromClause
    | TableSourceLateral !(Located TableSource)
    deriving (Eq, Show)

-- | One JOIN clause with modifiers, kind, target, and constraint.
data JoinClause = JoinClause
    { joinNatural :: !Bool
    , joinGlobal :: !Bool
    , joinStrictness :: !(Maybe (Located ByteString))
    , joinKind :: !JoinKind
    , joinTarget :: !JoinTarget
    , joinConstraint :: !(Maybe JoinConstraint)
    }
    deriving (Eq, Show)

-- | Join family; parameterized ASOF/ARRAY forms retain their directional modifier.
data JoinKind
    = JoinInner
    | JoinLeft
    | JoinRight
    | JoinFull
    | JoinCross
    | JoinAsof !Bool
    | JoinArray !Bool
    deriving (Eq, Ord, Show)

-- | Table-reference join target or ClickHouse ARRAY JOIN item list.
data JoinTarget
    = JoinTargetTable !(Located TableRef)
    | JoinTargetArray ![Located ArrayJoinItem]
    deriving (Eq, Show)

-- | ARRAY JOIN expression and optional output name.
data ArrayJoinItem = ArrayJoinItem
    { arrayJoinExpr :: !(Located Expr)
    , arrayJoinAlias :: !(Maybe (Located NamePart))
    }
    deriving (Eq, Show)

-- | Join predicate expressed as @ON@ or a @USING@ column list.
data JoinConstraint
    = JoinOn !(Located Expr)
    | JoinUsing ![Located NamePart]
    deriving (Eq, Show)

-- | LIMIT with @offset, count@ or @count OFFSET@ forms.
data LimitClause = LimitClause
    { limitFirst :: !(Located Expr)
    , limitSecond :: !(Maybe (Located Expr))
    , limitCommaForm :: !Bool
    }
    deriving (Eq, Show)

-- | FETCH FIRST/NEXT clause.
data FetchClause = FetchClause
    { fetchCount :: !(Maybe (Located Expr))
    , fetchWithTies :: !Bool
    }
    deriving (Eq, Show)

-- | FOR UPDATE/SHARE locking clause.
data ForClause = ForClause
    { forStrength :: !ForStrength
    , forTables :: ![Located Name]
    , forSkipLocked :: !Bool
    , forNoWait :: !Bool
    }
    deriving (Eq, Show)

-- | Row-locking strength requested by a query's @FOR@ clause.
data ForStrength
    = ForUpdate
    | ForShare
    | ForNoKeyUpdate
    | ForKeyShare
    deriving (Eq, Ord, Show)

-- | Named window and its inline window specification.
data WindowDef = WindowDef
    { windowDefName :: !(Located NamePart)
    , windowDefSpec :: !WindowSpec
    }
    deriving (Eq, Show)

-- | Query setting assignment retained as a name and expression pair.
data SettingItem = SettingItem
    { settingName :: !(Located Name)
    , settingValue :: !(Located Expr)
    }
    deriving (Eq, Show)
