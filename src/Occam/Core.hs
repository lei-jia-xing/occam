module Occam.Core (
    StatementId (..),
    Statement (..),
    Span (..),
    ObjectName (..),
    BarrierReason (..),
    ActionTarget (..),
    targetObjectName,
    ObjectKind (..),
    DependencyType (..),
    ObjectAction (..),
) where

import Data.ByteString (ByteString)
import Data.Text (Text)

newtype StatementId = StatementId Int
    deriving (Eq, Ord, Show)

data Span = Span
    { spanStartByte :: !Int
    , spanEndByte :: !Int
    }
    deriving (Eq, Show)

data Statement = Statement
    { stmtId :: !StatementId
    , stmtSpan :: !Span
    , stmtRaw :: !ByteString
    }
    deriving (Eq, Show)

newtype ObjectName = ObjectName {objectNameParts :: [Text]}
    deriving (Eq, Ord, Show)

data BarrierReason
    = UnknownStatementBarrier
    | SyntaxErrorBarrier
    | UnknownRelationBarrier
    | UnknownDDLBarrier
    | UnknownDMLBarrier
    | UnknownDropBarrier
    | UnknownAlterBarrier
    | UnknownViewBarrier
    | UnknownIndexBarrier
    | UnknownTriggerBarrier
    | UnknownTruncateBarrier
    deriving (Eq, Ord, Show)

data ActionTarget
    = ObjectTarget !ObjectName
    | ColumnTarget !ObjectName !Text
    | BarrierTarget !BarrierReason
    | NoTarget
    deriving (Eq, Ord, Show)

targetObjectName :: ActionTarget -> Maybe ObjectName
targetObjectName (ObjectTarget name) = Just name
targetObjectName _ = Nothing

data ObjectKind
    = TableObject
    | TemporaryTableObject
    | ColumnObject
    | IndexObject
    | ViewObject
    | TriggerObject
    | BarrierObject
    | SessionVariableObject
    | TransactionObject
    deriving (Eq, Ord, Show)

data DependencyType
    = DefDep
    | UseDep
    | ModDep
    | DropDep
    | RenameFromDep
    | RenameToDep
    | BarrierDep
    | TxBeginDep
    | TxCommitDep
    | TxRollbackDep
    | TxSavepointDep
    | TxRollbackToDep
    | TxReleaseDep
    | CascadeDep
    | OnCommitDropDep
    | OnCommitDeleteRowsDep
    deriving (Eq, Ord, Show)

data ObjectAction = ObjectAction
    { actionKind :: !ObjectKind
    , actionType :: !DependencyType
    , actionTarget :: !ActionTarget
    }
    deriving (Eq, Ord, Show)
