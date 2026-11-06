module Baseline (
    runSequentialDrop,
    runNaiveDdmin,
) where

import Data.ByteString (ByteString)
import Occam.Reducer (Oracle, SearchStrategy (CountDD), statementAudit, statementDdmin)

{- | Baseline 1 (Sequential Statement Deletion):
Loops through statements one by one, attempts removing statement i.
If Oracle is True, keeps it removed. Repeats until fixed-point.
-}
runSequentialDrop :: Oracle -> ByteString -> IO ByteString
runSequentialDrop = statementAudit

{- | Baseline 2 (Standard statement ddmin):
Delta-debug the same top-level statement units used by the shared reducer,
without dependency awareness or syntax-level rewrites.
-}
runNaiveDdmin :: Oracle -> ByteString -> IO ByteString
runNaiveDdmin = statementDdmin CountDD
