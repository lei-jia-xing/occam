-- | Data definition statement and object syntax nodes.
module Occam.Frontend.AST.DDL (
    CreateStmt (..),
    CreateObject (..),
    CreateExtension (..),
    CreateTable (..),
    PartitionBy (..),
    PartitionBound (..),
    TablePartition (..),
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
    ForeignKeyTiming (..),
    ForeignKeyAction (..),
    ForeignKeyActionKind (..),
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
    TableOption (..),
    DropStmt (..),
    DropObject (..),
    DropBehavior (..),
    AlterStmt (..),
    AlterObject (..),
    AlterAction (..),
    ColumnPosition (..),
    RlsAction (..),
    AlterColumnAction (..),
    GenericOption (..),
) where

import Data.ByteString (ByteString)
import Occam.Frontend.AST.DML (Assignment (..))
import Occam.Frontend.AST.Expr
import Occam.Frontend.AST.Query
import {-# SOURCE #-} Occam.Frontend.AST.Statement (Statement)
import Occam.Frontend.Span (ByteSpan, Located)

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
    deriving (Eq, Show)

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
    deriving (Eq, Show)

-- | PostgreSQL @CREATE EXTENSION [IF NOT EXISTS] name ...@.
data CreateExtension = CreateExtension
    { createExtensionName :: !(Located NamePart)
    , createExtensionIfNotExists :: !Bool
    }
    deriving (Eq, Ord, Show)

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
    deriving (Eq, Show)

data PartitionBy = PartitionBy
    { partitionByStrategy :: !(Located ByteString)
    , partitionByExprs :: ![Located Expr]
    }
    deriving (Eq, Show)

data PartitionBound
    = PartitionBoundDefault
    | PartitionBoundIn ![Located Expr]
    | PartitionBoundFromTo ![Located Expr] ![Located Expr]
    | PartitionBoundWith ![(Located NamePart, Located Expr)]
    deriving (Eq, Show)

data TablePartition = TablePartition
    { tablePartitionParent :: !(Located Name)
    , tablePartitionBound :: !PartitionBound
    }
    deriving (Eq, Show)

data TableElement
    = TableElementColumn !(Located ColumnDef)
    | TableElementConstraint !(Located TableConstraint)
    | TableElementProjection !(Located ProjectionDef)
    | TableElementSkippingIndex !(Located SkippingIndex)
    deriving (Eq, Show)

data SkippingIndex = SkippingIndex
    { skippingIndexName :: !(Located NamePart)
    , skippingIndexExpr :: !(Located Expr)
    , skippingIndexType :: !(Located TypeName)
    , skippingIndexGranularity :: !(Maybe (Located Expr))
    }
    deriving (Eq, Show)

data ProjectionDef = ProjectionDef
    { projectionName :: !(Located NamePart)
    , projectionQuery :: !(Located Query)
    }
    deriving (Eq, Show)

-- | One column definition.
data ColumnDef = ColumnDef
    { columnName :: !(Located NamePart)
    , columnType :: !(Maybe (Located TypeName))
    , columnConstraints :: ![Located ColumnConstraint]
    , columnSettings :: ![Located SettingItem]
    }
    deriving (Eq, Show)

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
    deriving (Eq, Show)

data ColumnConflictAction
    = ColumnConflictRollback
    | ColumnConflictAbort
    | ColumnConflictFail
    | ColumnConflictIgnore
    | ColumnConflictReplace
    deriving (Eq, Ord, Show)

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
    deriving (Eq, Show)

data ConstraintIndexOptions = ConstraintIndexOptions
    { constraintName :: !(Maybe (Located NamePart))
    , constraintIndexName :: !(Maybe (Located NamePart))
    , constraintIndexMethod :: !(Maybe (Located NamePart))
    }
    deriving (Eq, Ord, Show)

data TableLikeOption = TableLikeOption
    { tableLikeInclude :: !Bool
    , tableLikeOptionName :: !(Located ByteString)
    }
    deriving (Eq, Ord, Show)

data ForeignKeyRef = ForeignKeyRef
    { foreignKeyTable :: !(Located Name)
    , foreignKeyColumns :: ![Located NamePart]
    , foreignKeyOnDelete :: !(Maybe (Located ForeignKeyAction))
    , foreignKeyOnUpdate :: !(Maybe (Located ForeignKeyAction))
    , foreignKeyMatch :: !(Maybe (Located NamePart))
    , foreignKeyDeferrable :: !(Maybe Bool)
    , foreignKeyInitially :: !(Maybe ForeignKeyTiming)
    }
    deriving (Eq, Ord, Show)

data ForeignKeyTiming = ForeignKeyInitiallyDeferred | ForeignKeyInitiallyImmediate
    deriving (Eq, Ord, Show)

data ForeignKeyAction = ForeignKeyAction
    { foreignKeyActionKind :: !ForeignKeyActionKind
    }
    deriving (Eq, Ord, Show)

data ForeignKeyActionKind
    = ForeignKeyCascade
    | ForeignKeyRestrict
    | ForeignKeyNoAction
    | ForeignKeySetNull
    | ForeignKeySetDefault
    deriving (Eq, Ord, Show)

-- | CREATE INDEX definition.
data CreateIndex = CreateIndex
    { createIndexName :: !(Maybe (Located Name))
    , createIndexTable :: !(Located Name)
    , createIndexMethod :: !(Maybe (Located Name))
    , createIndexKeys :: ![Located IndexKeyPart]
    , createIndexIncludes :: ![Located NamePart]
    , createIndexWhere :: !(Maybe (Located Expr))
    }
    deriving (Eq, Show)

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
    deriving (Eq, Show)

data ViewBody
    = ViewBodyQuery !(Located Query)
    | ViewBodyOpaque !OpaqueBody
    deriving (Eq, Show)

-- | Byte range of a body that is intentionally not parsed.
data OpaqueBody = OpaqueBody
    { opaqueBodySpan :: !ByteSpan
    , opaqueBodyKind :: !OpaqueKind
    }
    deriving (Eq, Ord, Show)

data OpaqueKind
    = OpaqueFunctionBody
    | OpaqueProcedureBody
    | OpaqueTriggerBody
    | OpaqueEngineExpression
    | OpaqueViewBody
    deriving (Eq, Ord, Show)

data CreateSequence = CreateSequence
    { createSequenceName :: !(Located Name)
    , createSequenceIfNotExists :: !Bool
    , createSequenceOptions :: ![Located TableOption]
    }
    deriving (Eq, Show)

data CreateSchema = CreateSchema
    { createSchemaName :: !(Maybe (Located Name))
    , createSchemaIfNotExists :: !Bool
    , createSchemaAuthorization :: !(Maybe (Located Name))
    }
    deriving (Eq, Ord, Show)

data CreateDatabase = CreateDatabase
    { createDatabaseName :: !(Located Name)
    , createDatabaseIfNotExists :: !Bool
    , createDatabaseOptions :: ![Located TableOption]
    }
    deriving (Eq, Show)

data CreateStatistics = CreateStatistics
    { createStatisticsName :: !(Located Name)
    , createStatisticsIfNotExists :: !Bool
    , createStatisticsKinds :: ![Located NamePart]
    , createStatisticsColumns :: ![Located Expr]
    , createStatisticsFrom :: !(Maybe (Located Name))
    }
    deriving (Eq, Show)

data CreateMacro = CreateMacro
    { createMacroName :: !(Located Name)
    , createMacroParams :: ![Located MacroParam]
    , createMacroTable :: !Bool
    , createMacroBody :: !(Either (Located Query) (Located Expr))
    }
    deriving (Eq, Show)

data MacroParam = MacroParam
    { macroParamName :: !(Located NamePart)
    , macroParamDefault :: !(Maybe (Located Expr))
    }
    deriving (Eq, Show)

data CreatePolicy = CreatePolicy
    { createPolicyName :: !(Located Name)
    , createPolicyTable :: !(Located Name)
    , createPolicyPermissive :: !(Maybe Bool)
    , createPolicyCommand :: !(Maybe (Located ByteString))
    , createPolicyRoles :: ![Located Name]
    , createPolicyUsing :: !(Maybe (Located Expr))
    , createPolicyWithCheck :: !(Maybe (Located Expr))
    }
    deriving (Eq, Show)

data CreateRule = CreateRule
    { createRuleName :: !(Located Name)
    , createRuleEvent :: !(Located ByteString)
    , createRuleTable :: !(Located Name)
    , createRuleWhere :: !(Maybe (Located Expr))
    , createRuleInstead :: !Bool
    , createRuleAction :: !(Maybe (Located Statement))
    }
    deriving (Eq, Show)

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
    deriving (Eq, Show)

data TriggerGranularity = TriggerForEachRow | TriggerForEachStatement
    deriving (Eq, Ord, Show)

data CreateRoutine = CreateRoutine
    { createRoutineName :: !(Located Name)
    , createRoutineParams :: ![Located RoutineParam]
    , createRoutineReturns :: !(Maybe (Located TypeName))
    , createRoutineLanguage :: !(Maybe (Located NamePart))
    , createRoutineBody :: !OpaqueBody
    }
    deriving (Eq, Show)

data RoutineParam = RoutineParam
    { routineParamMode :: !(Maybe ByteString)
    , routineParamName :: !(Maybe (Located NamePart))
    , routineParamType :: !(Located TypeName)
    , routineParamDefault :: !(Maybe (Located Expr))
    }
    deriving (Eq, Show)

-- | PostgreSQL @CREATE [OR REPLACE] AGGREGATE@ definition.
data CreateAggregate = CreateAggregate
    { createAggregateName :: !(Located Name)
    , createAggregateParams :: ![Located RoutineParam]
    , createAggregateStar :: !Bool
    , createAggregateOptions :: ![Located TableOption]
    }
    deriving (Eq, Show)

data CreateVirtualTable = CreateVirtualTable
    { createVirtualTableName :: !(Located Name)
    , createVirtualTableIfNotExists :: !Bool
    , createVirtualTableModule :: !(Located Name)
    , createVirtualTableArgs :: ![Located VirtualTableArg]
    }
    deriving (Eq, Show)

data VirtualTableArg
    = VirtualTableColumn !(Located ColumnDef)
    | VirtualTableExpression !(Located Expr)
    deriving (Eq, Show)

data TableOption = TableOption
    { tableOptionName :: ![Located NamePart]
    , tableOptionValue :: !(Maybe (Located Expr))
    }
    deriving (Eq, Show)

-- | DROP object kind, names, and modifiers.
data DropStmt = DropStmt
    { dropTemp :: !Bool
    , dropObject :: !(Located DropObject)
    , dropIfExists :: !Bool
    , dropNames :: ![Located Name]
    , dropOnTable :: !(Maybe (Located Name))
    , dropBehavior :: !(Maybe DropBehavior)
    , dropSync :: !Bool
    }
    deriving (Eq, Ord, Show)

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
    deriving (Eq, Ord, Show)

data DropBehavior
    = DropCascade
    | DropRestrict
    deriving (Eq, Ord, Show)

-- | ALTER object kind, target, and action list.
data AlterStmt = AlterStmt
    { alterObject :: !AlterObject
    , alterIfExists :: !Bool
    , alterOnly :: !Bool
    , alterTable :: !(Located Name)
    , alterActions :: ![Located AlterAction]
    , alterSettings :: ![Located SettingItem]
    }
    deriving (Eq, Show)

data AlterObject
    = AlterObjectTable
    | AlterObjectView
    deriving (Eq, Ord, Show)

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
    deriving (Eq, Show)

-- | MySQL/MariaDB column placement in ALTER TABLE ADD/MODIFY/CHANGE COLUMN.
data ColumnPosition
    = ColumnPositionFirst
    | ColumnPositionAfter !(Located NamePart)
    deriving (Eq, Ord, Show)

data RlsAction
    = RlsEnable
    | RlsDisable
    | RlsForce
    | RlsNoForce
    deriving (Eq, Ord, Show)

data AlterColumnAction
    = AlterSetDefault !(Located Expr)
    | AlterDropDefault
    | AlterSetNotNull
    | AlterDropNotNull
    | AlterType !(Located TypeName) !(Maybe (Located Expr))
    | AlterAddIdentity !Bool ![Located TableOption]
    | AlterColumnOptions ![Located GenericOption]
    deriving (Eq, Show)

-- | One entry in PostgreSQL @ALTER COLUMN ... OPTIONS (...)@.
data GenericOption = GenericOption
    { genericOptionAction :: !(Maybe (Located ByteString))
    , genericOptionName :: !(Located NamePart)
    , genericOptionValue :: !(Maybe (Located Expr))
    }
    deriving (Eq, Show)
