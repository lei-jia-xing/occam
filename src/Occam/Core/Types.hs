-- | Runtime statement metadata and early-rejection evidence.
module Occam.Core.Types (
    StatementId (..),
    Span (..),
    Statement (..),
    ResultStatement (..),
    ResultTrace (..),
    traceResultStatement,
    EarlyRejectReason (..),
) where

import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.IntSet (IntSet)
import GHC.Generics (Generic)

-- | Zero-based statement identifier assigned after splitting a SQL seed.
newtype StatementId = StatementId {unStatementId :: Int}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Hashable)

-- | Half-open UTF-8 byte interval @[spanStartByte, spanEndByte)@ within the seed buffer.
data Span = Span
    { spanStartByte :: !Int
    , spanEndByte :: !Int
    }
    deriving (Eq, Show)

-- | Top-level statement slice paired with its assigned 'StatementId' and original raw bytes.
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

-- | Summarize a 'ResultTrace' by extracting its statement count and final result index.
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
