-- | Oracle outcomes, callbacks, and inconclusive-execution handling.
module Occam.Core.Oracle (
    OracleOutcome (..),
    Oracle,
    OracleInconclusiveError (..),
    oracleAccepts,
) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)

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
