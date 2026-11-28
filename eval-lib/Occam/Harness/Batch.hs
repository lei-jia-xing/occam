-- | Native Haskell batch orchestrator (@occam-harness batch@): containers, qualification, reduction, and reporting.
module Occam.Harness.Batch (
    BatchOptions (..),
    BatchSummary (..),
    runBatch,
) where

import Occam.Harness.Batch.Runner (runBatch)
import Occam.Harness.Batch.Types (BatchOptions (..), BatchSummary (..))
