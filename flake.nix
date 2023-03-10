{
  description = "Flake to build the haskell-src package 'sayable' and dependencies";

  nixConfig.bash-prompt-suffix = "sayable.env} ";

  inputs = {
    nixpkgs = { url = "github:nixos/nixpkgs/22.11"; };
    levers = {
      url = "github:kquick/nix-levers";
      inputs.nixpkgs.follows = "nixpkgs";
      };
    prettyprinter-src = {
      url = "https://hackage.haskell.org/package/prettyprinter-1.7.1/prettyprinter-1.7.1.tar.gz";
      flake = false;
      };
    };

  outputs = { self, levers, nixpkgs, prettyprinter-src }:
    let
      shellWith = pkgs: adds: drv: drv.overrideAttrs(old:
        { buildInputs = old.buildInputs ++ adds pkgs; });
      # Add additional packages useful for a development shell, generally
      # representing test packages or non-propagated build dependencies of
      # various sub-packages.
      shellPkgs = pkgs: [
        # pkgs.haskell.compiler.integer-simple.ghc8107
        # pkgs.haskell.packages.ghc8107.profiteur
        pkgs.cabal-install
      ];
    in rec {
      defaultPackage = levers.eachSystem (s:
        self.packages.${s}.sayable.default);
      devShell = levers.eachSystem (s:
        let pkgs = import nixpkgs { system=s; };
        in shellWith pkgs shellPkgs
          (defaultPackage.${s}.env.overrideAttrs (a:
            {
              # Set envvars here
            }
          )));

      devShells =
        let oneshell = s: n:
              let pkgs = import nixpkgs { system=s; };
              in levers.variedTargets
                { ghcver = levers.validGHCVersions pkgs.haskell.compiler; }
                ( { ghcver, ... } @ vargs:
                  shellWith pkgs shellPkgs
                    (self.packages.${s}.${n}.${ghcver}.env.overrideAttrs (a:
                      {
                        # Set envvars here
                      }
                    )));
        in levers.eachSystem
          (s:
            let pkgs = import nixpkgs { system=s; };
                names = builtins.attrNames (self.packages.${s});
            in pkgs.lib.genAttrs names (oneshell s)
          ) ;

      packages = levers.eachSystem (system:
        let
          mkHaskell = levers.mkHaskellPkg {
            inherit nixpkgs system;
            ghcver = levers.validGHCVersions pkgs.haskell.compiler;
            };
          pkgs = import nixpkgs { inherit system; };
          haskellAdj = drv:
            with (pkgs.haskell).lib;
            dontHaddock (dontCheck (dontBenchmark drv));
        in rec {
          ghc = pkgs.haskell.compiler.ghc8107;
          prettyprinter = mkHaskell "prettyprinter" prettyprinter-src {
            adjustDrv = args:
              drv:
                haskellAdj drv;
            };
          prettyprinter_tests = mkHaskell "prettyprinter_tests" prettyprinter-src {
            adjustDrv = args:
              drv:
                pkgs.haskell.lib.doBenchmark (pkgs.haskell.lib.doCheck (haskellAdj drv));
            };
          sayable = mkHaskell "sayable" self {
            inherit prettyprinter;
            adjustDrv = args:
              drv:
                haskellAdj drv;
            };
          sayable_tests = mkHaskell "sayable_tests" self {
            inherit prettyprinter;
            adjustDrv = args:
              drv:
                pkgs.haskell.lib.doBenchmark (pkgs.haskell.lib.doCheck (haskellAdj drv));
            };
        });
    };
}
