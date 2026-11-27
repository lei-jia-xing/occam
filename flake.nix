{
  description = "Occam: Cross-Platform Dependency-Aware SQL Test Case Minimizer";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    in
    {
      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          baseHook = ''
            export OCCAM_PG_QUERY_LIB="${pkgs.libpg_query}/lib/libpg_query${pkgs.stdenv.hostPlatform.extensions.sharedLibrary}"
            if [ -d .githooks ]; then
              git config core.hooksPath .githooks
            fi
            echo "Occam dev environment ready (GHC $(ghc --numeric-version), Cabal $(cabal --numeric-version))"
          '';
          baseInputs = with pkgs; [
            ghc
            cabal-install
            haskell-language-server
            fourmolu
            hlint
            elan
            python3
            duckdb
            libpg_query
            bashInteractive
          ];
        in
        {
          default = pkgs.mkShell {
            name = "occam-dev";
            nativeBuildInputs = baseInputs;
            shellHook = baseHook;
          };

          # Full multi-engine syntax conformance shell (zero Dockerfiles; uses
          # prebuilt Nix binaries for ClickHouse 26.8, MySQL 8.4, and MariaDB 11.4).
          syntax = pkgs.mkShell {
            name = "occam-syntax";
            nativeBuildInputs =
              baseInputs
              ++ [
                pkgs.mysql84
                pkgs.mariadb
              ]
              ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [ pkgs.clickhouse ];
            shellHook = ''
              ${baseHook}
              export OCCAM_MYSQLD_BIN="${pkgs.mysql84}/bin/mysqld"
              export OCCAM_MYSQL_CLIENT_BIN="${pkgs.mysql84}/bin/mysql"
              export OCCAM_MARIADBD_BIN="${pkgs.mariadb}/bin/mariadbd"
              export OCCAM_MARIADB_CLIENT_BIN="${pkgs.mariadb}/bin/mariadb"
              export OCCAM_MARIADB_INSTALL_DB_BIN="${pkgs.mariadb}/bin/mariadb-install-db"
              ${pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isLinux ''
                export OCCAM_CLICKHOUSE_BIN="${pkgs.clickhouse}/bin/clickhouse"
              ''}
              echo "Syntax conformance tools ready (MySQL ${pkgs.mysql84.version}, MariaDB ${pkgs.mariadb.version}${pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isLinux ", ClickHouse ${pkgs.clickhouse.version}"})"
            '';
          };
        }
      );
    };
}
