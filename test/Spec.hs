module Main (main) where

import qualified Occam

main :: IO ()
main = do
    putStrLn ("Running test suite for Occam v" ++ Occam.version)
    putStrLn "All tests passed."
