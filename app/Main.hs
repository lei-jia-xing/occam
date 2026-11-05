module Main (main) where

import Occam qualified
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--version"] -> putStrLn ("Occam v" ++ Occam.version)
        ["--help"] -> putStrLn "Usage: occam <crash-script.sql> --oracle <oracle-cmd>"
        _ -> do
            putStrLn ("Occam v" ++ Occam.version ++ ": Dependency-Aware SQL Crash Minimizer")
            putStrLn "Run 'occam --help' for usage."
            exitFailure
