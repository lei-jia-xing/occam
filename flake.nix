{
  description = "Occam: Cross-Platform Dependency-Aware SQL Crash Minimizer";

  inputs = {
    nixpkgs.url = "nixpkgs";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system:
          f (import nixpkgs {
            inherit system;
          }));
    in
    {
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          name = "occam-dev";

          packages = with pkgs; [
            # Haskell toolchain
            haskell.compiler.ghc910
            cabal-install
            haskell-language-server
            fourmolu
            hlint

            # C toolchain & Tree-sitter
            pkg-config
            tree-sitter
            gcc
            gdb
          ];

          shellHook = ''
            if [ -d .githooks ]; then
              git config core.hooksPath .githooks
            fi
            echo "Occam dev environment ready (GHC $(ghc --numeric-version), Cabal $(cabal --numeric-version))"
          '';
        };
      });
    };
}
