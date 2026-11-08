{-# LANGUAGE CPP #-}

import Distribution.Simple (defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (
    ComponentLocalBuildInfo,
    LocalBuildInfo,
    componentBuildDir,
    withLibLBI,
 )
import Distribution.Simple.UserHooks (buildHook)
#if MIN_VERSION_Cabal(3, 14, 0)
import Distribution.Utils.Path (getSymbolicPath)
#endif
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Process (callProcess)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {buildHook = buildRust}

#if MIN_VERSION_Cabal(3, 14, 0)
buildDirectory :: LocalBuildInfo -> ComponentLocalBuildInfo -> FilePath
buildDirectory localBuildInfo component =
    getSymbolicPath (componentBuildDir localBuildInfo component)
#else
buildDirectory :: LocalBuildInfo -> ComponentLocalBuildInfo -> FilePath
buildDirectory = componentBuildDir
#endif

buildRust package localBuildInfo hooks flags = do
    callProcess
        "cargo"
        [ "build"
        , "--locked"
        , "--release"
        , "--manifest-path"
        , "rust/sqlparser-ffi/Cargo.toml"
        , "--target-dir"
        , "rust/sqlparser-ffi/target"
        ]
    withLibLBI package localBuildInfo $ \_ component -> do
        let dir = buildDirectory localBuildInfo component
        createDirectoryIfMissing True dir
        copyFile
            "rust/sqlparser-ffi/target/release/liboccam_sqlparser.a"
            (dir </> "libCoccam_sqlparser.a")
    buildHook simpleUserHooks package localBuildInfo hooks flags
