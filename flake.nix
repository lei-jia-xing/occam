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
        in
        {
          default = pkgs.mkShell {
            name = "occam-dev";

            # Build and development tools
            nativeBuildInputs = with pkgs; [
              ghc
              cabal-install
              haskell-language-server
              fourmolu
              hlint
              pkg-config
            ];

            # Shared C libraries
            buildInputs = with pkgs; [
              tree-sitter
            ];

            # Ensure dynamic loader finds C FFI shared libraries at runtime
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.tree-sitter ];

            shellHook = ''
              if [ -d .githooks ]; then
                git config core.hooksPath .githooks
              fi
              echo "Occam dev environment ready (GHC $(ghc --numeric-version), Cabal $(cabal --numeric-version))"
            '';
          };
        }
      );
    };
}
