module Occam.Core (
    StatementId (..),
    Statement (..),
    Span (..),
    ObjectName,
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

type ObjectName = Text

data ObjectKind
    = TableObject
    | TemporaryTableObject
    | ColumnObject
    | IndexObject
    | ViewObject
    | TriggerObject
    | BarrierObject
    deriving (Eq, Ord, Show)

data DependencyType
    = DefDep
    | UseDep
    | ModDep
    | DropDep
    | RenameFromDep
    | RenameToDep
    | BarrierDep
    deriving (Eq, Ord, Show)

data ObjectAction = ObjectAction
    { actionKind :: !ObjectKind
    , actionType :: !DependencyType
    , actionName :: !ObjectName
    }
    deriving (Eq, Ord, Show)
