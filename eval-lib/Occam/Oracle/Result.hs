{- | Observation shared by the native result oracles. A missing result set is
represented separately from an empty result set with named columns.
-}
module Occam.Oracle.Result (ResultSet (..)) where

import Data.Aeson (Value)
import Data.Text (Text)

data ResultSet = ResultSet
    { resultColumns :: ![Text]
    , resultColumnTypes :: ![Int]
    , resultRows :: ![[Value]]
    }
    deriving (Eq, Show)
