module Occam.Core (
    StatementId (..),
    Statement (..),
    ResultStatement (..),
    ResultTrace (..),
    traceResultStatement,
    EarlyRejectReason (..),
    Span (..),
    Ident (..),
    Name (..),
    NameRef (..),
    nameText,
    namePartsText,
    SearchPathEntry (..),
    SearchPathValue (..),
    SessionInit (..),
    ReadMode (..),
    ReadFact (..),
    RelationKind (..),
    ConstraintValidity (..),
    IndexUniqueness (..),
    IndexRole (..),
    Constraint (..),
    ForeignKey (..),
    ReferentialAction (..),
    OnCommitAction (..),
    ColumnDef (..),
    TableDef (..),
    SourceColumn (..),
    ViewColumnLineage (..),
    ViewDef (..),
    IndexRef (..),
    IndexDef (..),
    IndexSchemaPlacement (..),
    TriggerRef (..),
    ConstraintRef (..),
    TriggerTiming (..),
    TriggerEvent (..),
    TriggerBody (..),
    TriggerDef (..),
    TriggerEffect (..),
    SequenceDef (..),
    TypeDef (..),
    SchemaDef (..),
    ConflictAction (..),
    UpdateScope (..),
    DeleteScope (..),
    TruncateBehavior (..),
    TruncateIdentity (..),
    WriteKind (..),
    DataWrite (..),
    SequenceWrite (..),
    AlterChange (..),
    AlterTable (..),
    PartitionLink (..),
    DropMode (..),
    DropObject (..),
    RenameOp (..),
    RefreshOp (..),
    CatalogOp (..),
    SessionScope (..),
    SessionValue (..),
    SessionEffect (..),
    BeginPolicy (..),
    TxEffect (..),
    BarrierReason (..),
    ObjectRef (..),
    BarrierScope (..),
    ScopedBarrier (..),
    SemanticFacts (..),
    DmlEvent (..),
    DropOp (..),
    OracleOutcome (..),
    OracleInconclusiveError (..),
    Oracle,
    oracleAccepts,
) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.IntSet (IntSet)
import GHC.Generics (Generic)
import Occam.Core.Schema
import Occam.Core.Semantics

newtype StatementId = StatementId {unStatementId :: Int}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Hashable)

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

-- | Engine statement count and zero-based ordinal of its last result set.
data ResultStatement = ResultStatement
    { resultStatementCount :: !Int
    , resultStatementIndex :: !Int
    }
    deriving (Eq, Show)

{- | Per-statement engine boundary and result-set evidence recorded during one
deterministic execution of an accepted SQL script against a native oracle.
-}
data ResultTrace = ResultTrace
    { traceStatementCount :: !Int
    , traceFirstResultIndex :: !Int
    , traceLastResultIndex :: !Int
    , traceResultIndices :: !IntSet
    , tracePrefixMatchIndices :: !IntSet
    , traceDeterministic :: !Bool
    , traceEndByteOffsets :: !(Maybe [Int])
    }
    deriving (Eq, Show)

traceResultStatement :: ResultTrace -> ResultStatement
traceResultStatement trace =
    ResultStatement (traceStatementCount trace) (traceLastResultIndex trace)

-- | Provable certificate class justifying early rejection without running the oracle.
data EarlyRejectReason
    = EarlyRejectExactPrefix
    | EarlyRejectNoResultStatement
    | EarlyRejectFailingPrefix
    | EarlyRejectMissingDefinition
    | EarlyRejectColumnArityMismatch
    deriving (Eq, Ord, Show)

-- | Verdict of an oracle execution on a candidate.
data OracleOutcome
    = OracleAccepted
    | OracleRejected
    | OracleInconclusive
    deriving (Eq, Ord, Show)

-- | Primitive per-candidate oracle callback.
type Oracle = ByteString -> IO OracleOutcome

data OracleInconclusiveError = OracleInconclusiveError
    deriving (Show)

instance Exception OracleInconclusiveError

-- | Check if the oracle accepts a candidate, throwing on inconclusive.
oracleAccepts :: Oracle -> ByteString -> IO Bool
oracleAccepts oracle candidate = do
    outcome <- oracle candidate
    case outcome of
        OracleAccepted -> pure True
        OracleRejected -> pure False
        OracleInconclusive -> throwIO OracleInconclusiveError
