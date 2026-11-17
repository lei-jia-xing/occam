module Occam (
    version,
) where

import Data.Version (showVersion)
import Paths_occam qualified as Paths

-- | Package version, single-sourced from the @.cabal@ file.
version :: String
version = showVersion Paths.version
