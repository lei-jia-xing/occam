{-# LANGUAGE DuplicateRecordFields #-}

-- | Haskell-native DBMS crash oracle (@occam-harness oracle@ and direct in-process probe API).
module Occam.Harness.Oracle (
    OracleClassification (..),
    OracleDecision (..),
    OracleOptions (..),
    OracleOverrides (..),
    OracleResult (..),
    OracleVerdict (..),
    defaultOracleOptions,
    emptyOracleOverrides,
    probeCrashOracle,
    probeCrashOracleCancellable,
    resolveOracleOptions,
) where

import Occam.Harness.Oracle.Config (resolveOracleOptions)
import Occam.Harness.Oracle.Probe (probeCrashOracle, probeCrashOracleCancellable)
import Occam.Harness.Oracle.Types (
    OracleClassification (..),
    OracleDecision (..),
    OracleOptions (..),
    OracleOverrides (..),
    OracleResult (..),
    OracleVerdict (..),
    defaultOracleOptions,
    emptyOracleOverrides,
 )
