-- | Thin entry point for @occam-harness@.
module Main (main) where

import Occam.Harness.Cli (runHarnessMain)

main :: IO ()
main = runHarnessMain
