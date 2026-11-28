-- | Data manipulation statement syntax nodes.
module Occam.Frontend.AST.DML (
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
) where

import Occam.Frontend.AST.Expr
import Occam.Frontend.AST.Query
import Occam.Frontend.Span (Located)

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
    deriving (Eq, Show)

-- | SQLite @INSERT OR <action>@ conflict resolution modifier.
data InsertOrAction
    = InsertOrRollback
    | InsertOrAbort
    | InsertOrFail
    | InsertOrIgnore
    | InsertOrReplace
    deriving (Eq, Ord, Show)

data InsertSource
    = InsertValues ![[Located Expr]]
    | InsertQuery !(Located Query)
    | InsertDefaultValues
    deriving (Eq, Show)

data OnConflictClause = OnConflictClause
    { onConflictConstraint :: !(Maybe (Located NamePart))
    , onConflictKeys :: ![Located IndexKeyPart]
    , onConflictWhere :: !(Maybe (Located Expr))
    , onConflictAction :: !OnConflictAction
    }
    deriving (Eq, Show)

data OnConflictAction
    = OnConflictDoNothing
    | OnConflictDoUpdate
        ![Located Assignment]
        !(Maybe (Located Expr))
    deriving (Eq, Show)

data Assignment = Assignment
    { assignTarget :: !(Located Name)
    , assignValue :: !(Located Expr)
    }
    deriving (Eq, Show)

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
    deriving (Eq, Show)

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
    deriving (Eq, Show)

-- | One target in a MySQL/MariaDB multi-table DELETE.
data DeleteTarget = DeleteTarget
    { deleteTargetName :: !(Located Name)
    , deleteTargetHasStar :: !Bool
    }
    deriving (Eq, Ord, Show)

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
    deriving (Eq, Show)

data MergeClause = MergeClause
    { mergeClauseMatched :: !Bool
    , mergeClauseBySource :: !Bool
    , mergeClauseCondition :: !(Maybe (Located Expr))
    , mergeClauseAction :: !MergeAction
    }
    deriving (Eq, Show)

data MergeAction
    = MergeUpdate ![Located Assignment]
    | MergeInsert ![Located NamePart] ![Located Expr]
    | MergeInsertDefaultValues
    | MergeDelete
    | MergeDoNothing
    deriving (Eq, Show)
