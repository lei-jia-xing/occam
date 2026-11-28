{- | Observation shared by the native result oracles. A missing result set is
represented separately from an empty result set with named columns.
-}
module Occam.Oracle.Result (ResultSet (..)) where

import Data.Aeson (Value)
import Data.Text (Text)

-- | Query result schema (column names, engine column types, and decoded row values).
data ResultSet = ResultSet
    { resultColumns :: ![Text]
    , resultColumnTypes :: ![Int]
    , resultRows :: ![[Value]]
    }
    deriving (Eq, Show)
