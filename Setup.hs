{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

import Control.Monad (when)
import Distribution.PackageDescription (
    Benchmark (benchmarkBuildInfo),
    BuildInfo (extraBundledLibs, extraLibDirs, ldOptions),
    Executable (buildInfo),
    GenericPackageDescription (..),
    Library (libBuildInfo),
    PackageDescription (library),
    TestSuite (testBuildInfo),
 )
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (
    ComponentLocalBuildInfo,
    LocalBuildInfo,
    componentBuildDir,
    withLibLBI,
    withProfLib,
 )
import Distribution.Simple.Setup (ConfigFlags (configConfigurationsFlags))
import Distribution.Simple.UserHooks (buildHook, confHook)
import Distribution.Types.CondTree (CondTree (..))
import Distribution.Types.Flag (lookupFlagAssignment, mkFlagName)
import Distribution.Types.HookedBuildInfo (HookedBuildInfo)
#if MIN_VERSION_Cabal(3, 14, 0)
import Distribution.Utils.Path (FileOrDir (Dir), Lib, Pkg, SymbolicPath, getSymbolicPath, makeSymbolicPath)
#endif
import System.Directory (copyFile, createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>))
import System.Info (os)
import System.Process (callProcess)

main :: IO ()
main =
    defaultMainWithHooks
        simpleUserHooks
            { confHook = confRust
            , buildHook = buildRust
            }

#if MIN_VERSION_Cabal(3, 14, 0)
buildDirectory :: LocalBuildInfo -> ComponentLocalBuildInfo -> FilePath
buildDirectory localBuildInfo component =
    getSymbolicPath (componentBuildDir localBuildInfo component)
#else
buildDirectory :: LocalBuildInfo -> ComponentLocalBuildInfo -> FilePath
buildDirectory = componentBuildDir
#endif

-- | Static when explicitly requested, or on Windows (static-only in v1).
useStatic :: ConfigFlags -> Bool
useStatic flags =
    os == "mingw32"
        || lookupFlagAssignment (mkFlagName "static-rust") (configConfigurationsFlags flags) == Just True

-- Cabal 3.14 changed BuildInfo.extraLibDirs to a SymbolicPath.
#if MIN_VERSION_Cabal(3, 14, 0)
rustLibDir :: FilePath -> SymbolicPath Pkg (Dir Lib)
rustLibDir = makeSymbolicPath
#else
rustLibDir :: FilePath -> FilePath
rustLibDir = id
#endif

{- | In dynamic (dev) mode, point every component at the absolute cargo
release dir and bake it as an rpath, so the loader finds the cdylib with
no env vars. Static mode skips this untouched (byte-identical output).
-}
confRust :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
confRust (package, hooked) flags = do
    package' <-
        if useStatic flags
            then pure package
            else do
                root <- getCurrentDirectory
                pure (addRustRpath (root </> "rust/sqlparser-ffi/target/release") package)
    confHook simpleUserHooks (package', hooked) flags

addRustRpath :: FilePath -> GenericPackageDescription -> GenericPackageDescription
addRustRpath dir package =
    package
        { condLibrary = fmap addLib (condLibrary package)
        , condExecutables = [(name, addExe tree) | (name, tree) <- condExecutables package]
        , condTestSuites = [(name, addTest tree) | (name, tree) <- condTestSuites package]
        , condBenchmarks = [(name, addBench tree) | (name, tree) <- condBenchmarks package]
        }
    where
        rpath = "-Wl,-rpath," ++ dir
        start info = info {extraLibDirs = rustLibDir dir : extraLibDirs info, ldOptions = rpath : ldOptions info}
        addLib tree = tree {condTreeData = lib {libBuildInfo = start (libBuildInfo lib)}}
            where
                lib = condTreeData tree
        addExe tree = tree {condTreeData = exe {buildInfo = start (buildInfo exe)}}
            where
                exe = condTreeData tree
        addTest tree = tree {condTreeData = suite {testBuildInfo = start (testBuildInfo suite)}}
            where
                suite = condTreeData tree
        addBench tree = tree {condTreeData = bench {benchmarkBuildInfo = start (benchmarkBuildInfo bench)}}
            where
                bench = condTreeData tree

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
    when (isStatic package) $
        withLibLBI package localBuildInfo $ \_ component -> do
            let dir = buildDirectory localBuildInfo component
                archive = "rust/sqlparser-ffi/target/release/liboccam_sqlparser.a"
            createDirectoryIfMissing True dir
            copyFile archive (dir </> "libCoccam_sqlparser.a")
            when (withProfLib localBuildInfo) $
                copyFile archive (dir </> "libCoccam_sqlparser_p.a")
    buildHook simpleUserHooks package localBuildInfo hooks flags
    where
        isStatic p = case library p of
            Just lib -> "Coccam_sqlparser" `elem` extraBundledLibs (libBuildInfo lib)
            Nothing -> False
