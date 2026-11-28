{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Public batch options and internal worker state shared by the batch runner.
module Occam.Harness.Batch.Types (
    BatchOptions (..),
    BatchSummary (..),
    ManagedContainer (..),
    WorkerBackend (..),
    WorkerEndpointSpec (..),
    WorkerState (..),
    WorkerHandle (..),
) where

import Control.Concurrent (MVar)
import Data.Aeson (Value)
import Data.IORef (IORef)
import Data.Set (Set)
import Data.Text (Text)
import Occam.Harness.BatchPlan (BatchPlanOptions, ExternalContainer)
import Occam.Harness.Config (HarnessConfig)
import Occam.Harness.Signature (DbmsKind)

-- | Options controlling native batch evaluation.
data BatchOptions = BatchOptions
    { configPath :: !FilePath
    -- ^ Harness configuration file or compiled JSON path.
    , plan :: !BatchPlanOptions
    -- ^ Batch planning options, including corpus, output, and worker settings.
    , dryRun :: !Bool
    -- ^ Only prepare and print the batch plan preview.
    , aggregateOnly :: !Bool
    -- ^ Aggregate an existing output directory without scheduling work.
    , occamBin :: !(Maybe FilePath)
    -- ^ Optional path to the @occam@ executable.
    , engineBin :: !(Maybe FilePath)
    -- ^ Optional embedded database engine executable.
    , harnessBin :: !(Maybe FilePath)
    -- ^ Optional path to the @occam-harness@ executable.
    , runTag :: !(Maybe Text)
    -- ^ Explicit suffix for worker container names; omitted values are generated.
    }
    deriving (Eq, Show)

-- | Top-level JSON summary printed by @occam-harness batch@.
data BatchSummary = BatchSummary
    { exitCode :: !Int
    -- ^ Process exit status represented by the batch summary.
    , payload :: !Value
    -- ^ JSON response payload printed by the batch command.
    }
    deriving (Eq, Show)

-- | Container coordinates tracked for dynamically spawned batch worker containers.
data ManagedContainer = ManagedContainer
    { name :: !Text
    , hostLog :: !(Maybe FilePath)
    }

-- | Execution backend assigned to an individual batch worker slot.
data WorkerBackend
    = BackendLocalEmbedded !(Maybe FilePath)
    | BackendExternal !ExternalContainer
    | BackendManaged !(Maybe ManagedContainer)

-- | Resolved endpoint attributes bound to a worker's active DBMS instance.
data WorkerEndpointSpec = WorkerEndpointSpec
    { container :: !(Maybe Text)
    , hostLog :: !(Maybe FilePath)
    , host :: !(Maybe Text)
    , port :: !(Maybe Int)
    , dbName :: !(Maybe Text)
    }

-- | Mutable per-worker state protected by an 'MVar' inside 'WorkerHandle'.
data WorkerState = WorkerState
    { workerId :: !Int
    , runTag :: !Text
    , outDir :: !FilePath
    , containerWait :: !Int
    , currentDbms :: !(Maybe DbmsKind)
    , currentImage :: !(Maybe Text)
    , backend :: !WorkerBackend
    , endpoint :: !(Maybe WorkerEndpointSpec)
    }

-- | Concurrent worker handle passed across qualification and reduction tasks.
data WorkerHandle = WorkerHandle
    { state :: !(MVar WorkerState)
    , spawned :: !(MVar (Set Text))
    , harnessConfig :: !HarnessConfig
    , stopRef :: !(IORef Bool)
    }
