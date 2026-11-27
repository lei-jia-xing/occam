{ mkDerivation
, aeson
, ansi-terminal
, base
, base16-bytestring
, bytestring
, containers
, direct-sqlite
, directory
, duckdb
, fast-logger
, filepath
, hashable
, installShellFiles
, lib
, libpg_query
, makeBinaryWrapper
, microlens
, optparse-applicative
, process
, python3
, QuickCheck
, safe-exceptions
, sqlite
, stdenv
, tasty
, tasty-bench
, tasty-hunit
, tasty-quickcheck
, temporary
, terminal-progress-bar
, text
, time
, transformers
, unix
, unordered-containers
, vector
, enableDuckDB ? true
}:
let
  sharedExt = stdenv.hostPlatform.extensions.sharedLibrary;
  duckdbLibDir = "${lib.getLib duckdb}/lib";
  pgQueryLibDir = "${lib.getLib libpg_query}/lib";
in
mkDerivation {
  pname = "occam";
  version = "0.1.0.0";
  src = lib.fileset.toSource {
    root = ./..;
    fileset = lib.fileset.unions [
      ../occam.cabal
      ../cabal.project
      ../LICENSE
      ../README.md
      ../src
      ../app
      ../eval-lib
      ../test
      ../bench
      ../benchmarks
      ../completions
      ../config
    ];
  };
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = false;

  libraryHaskellDepends = [
    aeson
    ansi-terminal
    base
    base16-bytestring
    bytestring
    containers
    directory
    fast-logger
    filepath
    hashable
    microlens
    process
    safe-exceptions
    temporary
    terminal-progress-bar
    text
    time
    transformers
    unix
    unordered-containers
    vector
  ];
  executableHaskellDepends = [
    aeson
    base
    bytestring
    containers
    direct-sqlite
    directory
    filepath
    optparse-applicative
    safe-exceptions
    text
    time
    transformers
    unix
  ];
  executableToolDepends = [
    installShellFiles
    makeBinaryWrapper
  ];
  testHaskellDepends = [
    aeson
    base
    bytestring
    containers
    direct-sqlite
    directory
    filepath
    optparse-applicative
    process
    QuickCheck
    safe-exceptions
    tasty
    tasty-hunit
    tasty-quickcheck
    text
    time
    transformers
    unix
    vector
  ];
  testSystemDepends = [
    (lib.getLib duckdb)
    (lib.getLib libpg_query)
  ];
  testToolDepends = [
    python3
    sqlite
  ];
  benchmarkHaskellDepends = [
    aeson
    base
    bytestring
    containers
    direct-sqlite
    directory
    filepath
    safe-exceptions
    tasty-bench
    text
    transformers
    unix
  ];

  preCheck = ''
    export OCCAM_BIN="$PWD/dist/build/occam/occam"
    export OCCAM_DUCKDB_LIB="${duckdbLibDir}/libduckdb${sharedExt}"
    export OCCAM_PG_QUERY_LIB="${pgQueryLibDir}/libpg_query${sharedExt}"
  '';

  postInstall = ''
    installShellCompletion --cmd occam \
      --bash completions/occam.bash \
      --zsh completions/_occam \
      --fish completions/occam.fish
  '' + lib.optionalString enableDuckDB ''
    wrapProgram "$out/bin/occam" \
      --set-default OCCAM_DUCKDB_LIB "${duckdbLibDir}/libduckdb${sharedExt}"
  '';

  homepage = "https://github.com/lei-jia-xing/occam";
  description = "A Dependency-Aware Minimizer for Multi-Statement SQL";
  license = lib.licenses.mit;
  mainProgram = "occam";
}
