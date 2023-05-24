{
  description = "Linear streaming for bytestrings";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    mission-control.url = "github:Platonic-Systems/mission-control";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    flake-root.url = "github:srid/flake-root";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
        inputs.mission-control.flakeModule
      ];

      perSystem = { self', system, lib, config, inputs', pkgs, ... }: {

        haskellProjects.default =
          {
            devShell = {
              tools = hp: {
                inherit (hp) implicit-hie threadscope;
                treefmt = config.treefmt.build.wrapper;
              } // config.treefmt.build.programs;
              hlsCheck.enable = true;
              mkShellArgs = { };
            };
            overrides = self: super: with pkgs.haskell.lib; {
              threadscope = unmarkBroken (doJailbreak super.threadscope);
            };
            autoWire = [ "packages" "apps" "checks" ];
          };

        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;
          flakeFormatter = false; # For https://github.com/numtide/treefmt-nix/issues/55

          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;

          # We use fourmolu
          programs.ormolu.package = pkgs.haskellPackages.fourmolu;
          settings.formatter.ormolu = {
            options = [
              "--ghc-opt"
              "-XImportQualifiedPost"
            ];
          };
        };

        # Dev shell scripts.
        mission-control.scripts = {
          docs = {
            description = "Start Hoogle server for project dependencies";
            exec = ''
              echo http://127.0.0.1:8888
              hoogle serve -p 8888 --local
            '';
            category = "Dev Tools";
          };
          repl = {
            description = "Start the cabal repl";
            exec = ''
              cabal repl "$@"
            '';
            category = "Dev Tools";
          };
          fmt = {
            description = "Format the source tree";
            exec = config.treefmt.build.wrapper;
            category = "Dev Tools";
          };
          mem-prof = {
            description = "Profile memory usage using threadscope";
            exec = ''
              threadscope "$@"
            '';
            category = "Dev Tools";
          };
          test = {
            description = "Run all tests";
            exec = ''
              ghcid -c "cabal repl test:tests" -T :main
            '';
            category = "Primary";
          };
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.flake-root.devShell
            config.mission-control.devShell
          ];
        };

        packages.default = self'.packages.streaming-bytestring-linear;
      };
    };
}
